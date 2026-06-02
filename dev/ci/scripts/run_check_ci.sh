#!/usr/bin/env bash

set -eu

#####################################################################################
# Script description: script to check the status of an experiment as reported
#                     by ecFlow
#####################################################################################

TEST_DIR=${1:-${TEST_DIR:-?}}            # Location of the root of the testing directory
pslot=${2:-${pslot:-?}}                  # Name of the experiment being tested by this script
SYSTEM_BUILD_DIR=${3:-"global-workflow"} # Name of the system build directory, default is "global-workflow

# TEST_DIR contains 2 directories;
# 1. HOMEglobal: clone of the global-workflow
# 2. RUNTESTS: A directory containing EXPDIR and COMROOT for experiments
# # e.g. $> tree ./TEST_DIR
# ./TEST_DIR
# ├── HOMEglobal
# └── RUNTESTS
#     ├── COMROOT
#     │   └── ${pslot}
#     └── EXPDIR
#         └── ${pslot}
# Two system build directories created at build time gfs, and gdas
# TODO: Make this configurable (for now all scripts run from gfs for CI at runtime)
HOMEglobal="${TEST_DIR}/${SYSTEM_BUILD_DIR}"
RUNTESTS="${TEST_DIR}/RUNTESTS"
run_check_logfile="${RUNTESTS}/ci-run_check.log"

# Source modules and setup logging
echo "Source modules."
source "${HOMEglobal}/dev/ush/gw_setup.sh"

# cd into the experiment directory
echo "cd ${RUNTESTS}/EXPDIR/${pslot}"
cd "${RUNTESTS}/EXPDIR/${pslot}" || (
    echo "FATAL ERROR: Unable to cd into '${RUNTESTS}/EXPDIR/${pslot}', ABORT!"
    exit 1
)

# ecFlow suite definition file
def_file="ecf/defs/${pslot}.def"

# Ensure the definition file is present for the experiment
if [[ ! -f "${def_file}" ]]; then
    echo "FATAL ERROR: ecFlow definition file ${def_file} not found in '${pslot}', experiment ${pslot} failed, ABORT!"
    exit 1
fi

# Determine ecFlow port (use ECF_PORT if set, otherwise default)
ECF_PORT="${ECF_PORT:-3141}"

# Load the suite definition into ecFlow
echo "Loading ecFlow suite definition."
ecflow_client --port "${ECF_PORT}" --load "${def_file}" 2>/dev/null || true
ecflow_client --port "${ECF_PORT}" --begin "${pslot}" 2>/dev/null || true

# Monitor experiment via ecFlow
rc=99
set +e
while true; do

    # Wait before checking status
    sleep 60

    # Get suite status from ecFlow
    echo "Gather ecFlow statistics for ${pslot}"
    suite_status=$(ecflow_client --port "${ECF_PORT}" --stats 2>/dev/null) || true

    # Count task states
    TASKS_COMPLETE=$(ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null | grep -c "state:complete" || echo "0")
    TASKS_ABORTED=$(ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null | grep -c "state:aborted" || echo "0")
    TASKS_ACTIVE=$(ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null | grep -c "state:active" || echo "0")
    TASKS_QUEUED=$(ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null | grep -c "state:queued" || echo "0")
    TASKS_SUBMITTED=$(ecflow_client --port "${ECF_PORT}" --get_state "/${pslot}" 2>/dev/null | grep -c "state:submitted" || echo "0")
    TASKS_TOTAL=$((TASKS_COMPLETE + TASKS_ABORTED + TASKS_ACTIVE + TASKS_QUEUED + TASKS_SUBMITTED))

    echo -e "(${pslot} on ${MACHINE_ID^})\n\tTotal Tasks: ${TASKS_TOTAL}\n\tComplete: ${TASKS_COMPLETE}\n\tAborted: ${TASKS_ABORTED}\n\tActive: ${TASKS_ACTIVE}\n\tQueued: ${TASKS_QUEUED}"

    # Check for aborted tasks (failure)
    if [[ "${TASKS_ABORTED}" -gt 0 && "${TASKS_ACTIVE}" -eq 0 && "${TASKS_SUBMITTED}" -eq 0 ]]; then
        {
            echo "Experiment ${pslot} Terminated with ${TASKS_ABORTED} tasks aborted at $(date)" || true
        } | tee -a "${run_check_logfile}"
        rc=1
        break
    fi

    # Check if all tasks are complete (success)
    if [[ "${TASKS_TOTAL}" -gt 0 && "${TASKS_QUEUED}" -eq 0 && "${TASKS_ACTIVE}" -eq 0 && "${TASKS_SUBMITTED}" -eq 0 && "${TASKS_ABORTED}" -eq 0 ]]; then
        {
            echo "Experiment ${pslot} Completed ${TASKS_COMPLETE} Tasks: *SUCCESS* at $(date)" || true
        } | tee -a "${run_check_logfile}"
        rc=0
        break
    fi

    # Wait before checking again
    sleep 300

done

# Cleanup: delete the suite from ecFlow
ecflow_client --port "${ECF_PORT}" --delete "/${pslot}" yes 2>/dev/null || true

exit "${rc}"
