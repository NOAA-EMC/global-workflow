#!/usr/bin/env bash

set -e

#########################################################################
# launch_gitlab_runner.sh - Script to manage GitLab runners for CI/CD
#
# This script handles four main operations for GitLab runners:
# 1. register   - Registers a new GitLab runner with the GitLab server
# 2. run        - Starts or health-checks the GitLab runner (idempotent)
# 3. unregister - Removes a GitLab runner from the GitLab server
# 4. status     - Reports runner health across 3 tiers
#
# Usage: launch_gitlab_runner.sh <command> [options] [token]
#
# Options (for 'run' command):
#   -f    Force launch regardless of current runner status
#   -n    Skip the wait period if runner is found offline
#   -h    Print help message
#
# Phase 35: Hardened for robust GitLab runner lifecycle management
# Phase 35b: Cross-node health checks for multi-head-node RDHPCS clusters
#########################################################################

print_usage() {
    cat << 'EOF'
Usage: launch_gitlab_runner.sh <command> [options] [token]

Commands:
  register   Register a new GitLab runner with the GitLab server
  run        Start or health-check the GitLab runner
  unregister Remove the GitLab runner from the GitLab server
  status     Report runner health (process, metrics, server verify)

Options (apply to 'run' command):
  -f    Force launch regardless of current runner status
  -n    Skip the wait period if runner is found offline
  -h    Print this help message

Token:
  Runner authentication token. Can also be set via:
    GITLAB_RUNNER_TOKEN environment variable, or
    gitlab_token file in the runner directory
EOF
}

log_msg() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "${GITLAB_LOG}"
}

#########################################################################
# Parse subcommand and options
#########################################################################

if [[ $# -lt 1 ]]; then
    print_usage
    exit 1
fi

SUBCOMMAND="${1}"
shift

# Handle -h at any position
if [[ "${SUBCOMMAND}" == "-h" ]]; then
    print_usage
    exit 0
fi

# Validate subcommand
case "${SUBCOMMAND}" in
    register | run | unregister | status) ;;
    *)
        echo "ERROR: Unknown command '${SUBCOMMAND}'"
        print_usage
        exit 1
        ;;
esac

# Parse flags (apply to 'run' command)
force_launch="False"
skip_wait="False"
while getopts ":fnh" flag; do
    case "${flag}" in
        f) force_launch="True" ;;
        n) skip_wait="True" ;;
        h)
            print_usage
            exit 0
            ;;
        *)
            echo "ERROR: Unknown flag: -${OPTARG}"
            print_usage
            exit 1
            ;;
    esac
done
shift $((OPTIND - 1))

# Remaining positional arg is the optional token
TOKEN_ARG="${1:-}"

# Set the HOMEglobal_ variable to the root directory of the global workflow
HOMEglobal_="$(cd "$(dirname "${BASH_SOURCE[0]}")" && git rev-parse --show-toplevel)"
host="$(hostname)"

# Defaults for cross-node state (set properly by check_runner_status)
RUNNER_ON_REMOTE="False"
RUNNER_HOST_NODE="${host}"

#########################################################################
#  Set up runtime environment variables for accounts on supported machines
#########################################################################

# Source the detect_machine.sh script to determine the MACHINE_ID
HOMEglobal="${HOMEglobal_}" source "${HOMEglobal_}/ush/detect_machine.sh"
# Check the MACHINE_ID and set up the environment accordingly
case "${MACHINE_ID}" in
    ursa | hera | orion | hercules | wcoss2 | gaeac6)
        echo "Running GitLab Runner script on ${MACHINE_ID}"
        ;;
    noaacloud)
        echo "Running GitLab Runner script on ${PW_CSP}"
        ;;
    *)
        echo "Unsupported platform. Exiting with error."
        exit 1
        ;;
esac

# Load module environment
HOMEglobal="${HOMEglobal_}" source "${HOMEglobal_}/ush/module-setup.sh"
module use "${HOMEglobal_}/modulefiles"
module load "gw_setup.${MACHINE_ID}"

# Source the platform-specific configuration file
# This file contains platform-specific variables such as GITLAB_URL, GITLAB_CI_BUILDS_DIR,
# and GITLAB_RUNNER_DIR which are required for runner registration and execution
source "${HOMEglobal_}/dev/ci/platforms/config.${MACHINE_ID}"

# Change to the GitLab runner directory defined in the platform config
mkdir -p "${GITLAB_RUNNER_DIR}"
cd "${GITLAB_RUNNER_DIR}" || exit 1

# Set the log file name with the current date and time
DATE=$(date +%Y%m%d%M) || true
GITLAB_LOG="${PWD}/launched_gitlab_runner-${DATE}.log"
rm -f "${GITLAB_LOG}"

#########################################################################
# GitLab Token Handling
#########################################################################

GITLAB_RUNNER_TOKEN="${TOKEN_ARG:-${GITLAB_RUNNER_TOKEN:-}}"
if [[ -z "${GITLAB_RUNNER_TOKEN}" ]]; then
    if [[ -f gitlab_token ]]; then
        source gitlab_token
    fi
fi
# Token is required for register/unregister but not for run/status
if [[ -z "${GITLAB_RUNNER_TOKEN}" && "${SUBCOMMAND}" == "register" ]]; then
    log_msg "ERROR: GITLAB_RUNNER_TOKEN not set (required for register)"
    exit 1
fi

# Download the GitLab runner binary if it does not exist
if [[ ! -f gitlab-runner ]]; then
    log_msg "Downloading gitlab-runner binary..."
    case "$(uname -m)" in
        x86_64) RUNNER_ARCH="amd64" ;;
        aarch64) RUNNER_ARCH="arm64" ;;
        *)
            log_msg "ERROR: Unsupported architecture: $(uname -m)"
            exit 1
            ;;
    esac
    curl -L --output "${PWD}/gitlab-runner" "https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-linux-${RUNNER_ARCH}"
    chmod +x ./gitlab-runner
    log_msg "gitlab-runner binary downloaded (${RUNNER_ARCH})"
fi

#########################################################################
# Health check functions
#
# Cross-node awareness (Phase 35b):
# On multi-head-node RDHPCS clusters (Hera, Ursa, Gaea, etc.), cron
# jobs can execute on ANY login node. The runner process and its metrics
# port are only visible on the node where it was launched. When this
# script runs on a different node, we SSH to the runner's host (recorded
# in runner.state) for Tier 1 (pgrep) and Tier 2 (curl metrics) checks.
# SSH between head nodes is passwordless for the same service account.
#########################################################################

# Run a command on the runner's host node. If the runner is on this node,
# execute locally; otherwise SSH to the recorded RUNNER_HOST.
run_on_runner_host() {
    if [[ "${RUNNER_ON_REMOTE}" == "True" ]]; then
        ssh -o BatchMode=yes -o ConnectTimeout=5 -o StrictHostKeyChecking=no \
            "${RUNNER_HOST_NODE}" "$*" 2> /dev/null
    else
        eval "$*"
    fi
}

check_port_available() {
    local port="${1}"
    if ss -tlnp 2> /dev/null | grep -q ":${port} "; then
        # Port is in use — check if it's OUR runner
        if curl -s --max-time 2 "http://localhost:${port}/metrics" 2> /dev/null | grep -q "gitlab_runner"; then
            log_msg "Port ${port} already in use by a GitLab Runner (may be our existing process)"
            return 1 # port used by a runner — caller should check if it's ours
        else
            log_msg "ERROR: Port ${port} is occupied by another service. Set GITLAB_RUNNER_METRICS_PORT to an available port in config.${MACHINE_ID}"
            return 2 # port used by something else — fatal
        fi
    fi
    return 0 # port available
}

check_runner_status() {
    # Load state file if it exists (written by launch_runner)
    RUNNER_STATE_FILE="${GITLAB_RUNNER_DIR}/runner.state"
    if [[ -f "${RUNNER_STATE_FILE}" ]]; then
        source "${RUNNER_STATE_FILE}"
        METRICS_PORT="${RUNNER_METRICS_PORT:-${RUNNER_METRICS:-${GITLAB_RUNNER_METRICS_PORT:-9252}}}"
    else
        METRICS_PORT="${GITLAB_RUNNER_METRICS_PORT:-9252}"
    fi

    # Determine if runner is on a different head node (cross-node awareness)
    RUNNER_HOST_NODE="${RUNNER_HOST:-${host}}"
    if [[ -n "${RUNNER_HOST_NODE}" && "${RUNNER_HOST_NODE}" != "${host}" ]]; then
        RUNNER_ON_REMOTE="True"
        log_msg "Runner was launched on ${RUNNER_HOST_NODE} (current node: ${host}) — using SSH for health checks"
    else
        RUNNER_ON_REMOTE="False"
    fi

    # Tier 1: Is the process running? (node-local — SSH if remote)
    if run_on_runner_host pgrep -f \"gitlab-runner run --working-directory "${GITLAB_RUNNER_DIR}"\" > /dev/null 2>&1; then
        # shellcheck disable=SC2311
        RUNNER_PID=$(run_on_runner_host pgrep -f \"gitlab-runner run --working-directory "${GITLAB_RUNNER_DIR}"\" 2> /dev/null | head -1)
        RUNNER_PROCESS_ALIVE="True"
    else
        RUNNER_PID=""
        RUNNER_PROCESS_ALIVE="False"
    fi

    # Tier 2: Is the metrics endpoint responding? (node-local — SSH if remote)
    if run_on_runner_host curl -s --max-time 5 "http://localhost:${METRICS_PORT}/metrics" > /dev/null 2>&1; then
        RUNNER_METRICS_ALIVE="True"
    else
        RUNNER_METRICS_ALIVE="False"
    fi

    # Tier 3: Can it reach the GitLab server? (registration validity — works from any node)
    if ./gitlab-runner verify --name "${GITLAB_RUNNER_NAME}" > /dev/null 2>&1; then
        RUNNER_VERIFIED="True"
    else
        RUNNER_VERIFIED="False"
    fi
}

launch_runner() {
    # Kill any orphaned process — on the runner's host if it's a different node
    local stale_pids
    # shellcheck disable=SC2311
    stale_pids=$(run_on_runner_host pgrep -f \"gitlab-runner run --working-directory "${GITLAB_RUNNER_DIR}"\" 2> /dev/null || true)
    if [[ -n "${stale_pids}" ]]; then
        if [[ "${RUNNER_ON_REMOTE}" == "True" ]]; then
            log_msg "Killing stale gitlab-runner on remote host ${RUNNER_HOST_NODE}: PIDs ${stale_pids}"
            run_on_runner_host "kill ${stale_pids}" || true
        else
            log_msg "Killing stale gitlab-runner process(es): ${stale_pids}"
            echo "${stale_pids}" | while read -r pid; do
                kill "${pid}" 2> /dev/null || true
            done
        fi
        sleep 2
    fi

    METRICS_PORT="${GITLAB_RUNNER_METRICS_PORT:-9252}"

    # Check port availability before launching
    check_port_available "${METRICS_PORT}" || {
        local port_status=$?
        if [[ ${port_status} -eq 1 ]]; then
            log_msg "FATAL: Cannot launch — metrics port ${METRICS_PORT} already used by another GitLab Runner"
            exit 1
        elif [[ ${port_status} -eq 2 ]]; then
            log_msg "FATAL: Cannot launch — metrics port ${METRICS_PORT} occupied by non-runner service"
            exit 1
        else
            log_msg "FATAL: Cannot launch — metrics port ${METRICS_PORT} unavailable (status=${port_status})"
            exit 1
        fi
    }

    COMMAND="nohup ./gitlab-runner run --working-directory ${GITLAB_RUNNER_DIR} --listen-address localhost:${METRICS_PORT}"
    log_msg "Launching GitLab Runner on ${host}"
    log_msg "Command: ${COMMAND}"
    ${COMMAND} >> "${GITLAB_LOG}" 2>&1 &
    RUNNER_PID=$!

    # Write state file for cron health checks
    cat > "${GITLAB_RUNNER_DIR}/runner.state" << EOF
RUNNER_PID=${RUNNER_PID}
RUNNER_METRICS_PORT=${METRICS_PORT}
RUNNER_STARTED="$(date '+%Y-%m-%d %H:%M:%S')"
RUNNER_HOST=$(hostname)
GITLAB_RUNNER_DIR=${GITLAB_RUNNER_DIR}
EOF

    log_msg "GitLab Runner launched with PID: ${RUNNER_PID} (metrics on localhost:${METRICS_PORT})"
    log_msg "State written to ${GITLAB_RUNNER_DIR}/runner.state"
}

#########################################################################
# REGISTER: Registers a new GitLab runner with the GitLab server
#########################################################################

if [[ "${SUBCOMMAND}" == "register" ]]; then

    # Validate dependencies
    if [[ -z "${GITLAB_URL:-}" ]]; then
        log_msg "ERROR: GITLAB_URL not set in platform config"
        exit 1
    fi
    if ! curl --silent --head --max-time 10 "${GITLAB_URL}" > /dev/null 2>&1; then
        log_msg "WARN: GITLAB_URL (${GITLAB_URL}) is not reachable — registration may fail"
    fi
    mkdir -p "${GITLAB_BUILDS_DIR}"

    log_msg "Registering GitLab Runner ${MACHINE_ID} on host ${host}"
    log_msg "Runner name: ${GITLAB_RUNNER_NAME}"
    ./gitlab-runner register -n -t "${GITLAB_RUNNER_TOKEN}" --name "${GITLAB_RUNNER_NAME}" --url "${GITLAB_URL}" --executor shell --shell bash --builds-dir "${GITLAB_BUILDS_DIR}" --custom_build_dir-enabled=true --request-concurrency 24

    # Set the concurrent job limit in the GitLab runner config file
    sed -i 's/concurrent.*/concurrent = 24/' ~/.gitlab-runner/config.toml
    log_msg "Registration complete — concurrent set to 24"
    exit 0
fi

#########################################################################
# STATUS: Reports runner health across 3 tiers
#########################################################################

if [[ "${SUBCOMMAND}" == "status" ]]; then
    check_runner_status
    log_msg "Runner host: ${RUNNER_HOST_NODE} (current node: ${host}, remote: ${RUNNER_ON_REMOTE})"
    log_msg "Process alive: ${RUNNER_PROCESS_ALIVE} (PID: ${RUNNER_PID:-none})"
    log_msg "Metrics endpoint: ${RUNNER_METRICS_ALIVE} (port: ${METRICS_PORT})"
    log_msg "Server verified: ${RUNNER_VERIFIED}"
    if [[ "${RUNNER_PROCESS_ALIVE}" == "True" && "${RUNNER_METRICS_ALIVE}" == "True" && "${RUNNER_VERIFIED}" == "True" ]]; then
        log_msg "GitLab Runner is healthy (all 3 tiers passed)"
        exit 0
    elif [[ "${RUNNER_PROCESS_ALIVE}" == "True" && "${RUNNER_METRICS_ALIVE}" == "False" ]]; then
        log_msg "GitLab Runner process alive but metrics unresponsive — possible hung process"
        exit 1
    else
        log_msg "GitLab Runner needs attention"
        exit 1
    fi
fi

#########################################################################
# RUN: Starts or health-checks the GitLab runner (idempotent)
#########################################################################

if [[ "${SUBCOMMAND}" == "run" ]]; then

    if [[ "${force_launch}" == "True" ]]; then
        log_msg "Force launch requested — skipping health checks"
        launch_runner
        exit 0
    fi

    check_runner_status

    if [[ "${RUNNER_PROCESS_ALIVE}" == "True" && "${RUNNER_METRICS_ALIVE}" == "True" && "${RUNNER_VERIFIED}" == "True" ]]; then
        log_msg "GitLab Runner is online (nothing done)"
        exit 0
    fi

    # Runner is not fully healthy
    if [[ "${RUNNER_PROCESS_ALIVE}" == "True" && "${RUNNER_METRICS_ALIVE}" == "True" ]]; then
        # Process and metrics OK but verify failed — may be transient network issue
        log_msg "GitLab Runner process and metrics OK but server verify failed — possible transient issue"
        log_msg "GitLab Runner is online (nothing done)"
        exit 0
    fi

    # Runner is offline or unhealthy
    if [[ "${skip_wait}" != "True" ]]; then
        log_msg "GitLab Runner is offline. Waiting 5 minutes to check again in case of transient network issue"
        sleep 300
        check_runner_status
        if [[ "${RUNNER_PROCESS_ALIVE}" == "True" && "${RUNNER_METRICS_ALIVE}" == "True" ]]; then
            log_msg "GitLab Runner is online (nothing done)"
            exit 0
        fi
    fi

    log_msg "GitLab Runner confirmed offline — relaunching"
    launch_runner
    exit 0
fi

#########################################################################
# UNREGISTER: Removes a GitLab runner from the GitLab server
#########################################################################

if [[ "${SUBCOMMAND}" == "unregister" ]]; then
    log_msg "Unregistering GitLab Runner: ${GITLAB_RUNNER_NAME}"
    ./gitlab-runner unregister --name "${GITLAB_RUNNER_NAME}"
    log_msg "Unregistration complete"
fi
