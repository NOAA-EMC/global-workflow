#!/bin/bash
###############################################################################
# Script: rocoto_monitor_notify.sh
# Description: Wrapper for rocotorun with email notifications for failures
#              and stalled workflows. Prevents email spam using lock files.
#
# Usage: rocoto_monitor_notify.sh -d DATABASE -w WORKFLOW [-m EMAIL]
#
# Features:
# - Runs rocotorun and monitors workflow state
# - Sends email notifications for failed jobs
# - Detects stalled workflows
# - Prevents duplicate notifications using lock files
# - Integrates with scrontab --mail-type=FAIL
###############################################################################

set -u

# Default values
DATABASE=""
WORKFLOW=""
EMAIL="${USER}@noaa.gov"
ROCOTORUN="${ROCOTORUN:-rocotorun}"
ROCOTOSTAT="${ROCOTOSTAT:-rocotostat}"
LOCK_DIR=""
VERBOSE=0

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    -d|--database)
      DATABASE="$2"
      shift 2
      ;;
    -w|--workflow)
      WORKFLOW="$2"
      shift 2
      ;;
    -m|--mail)
      EMAIL="$2"
      shift 2
      ;;
    -v|--verbose)
      VERBOSE=1
      shift
      ;;
    -h|--help)
      echo "Usage: $0 -d DATABASE -w WORKFLOW [-m EMAIL] [-v]"
      echo ""
      echo "Options:"
      echo "  -d, --database    Path to Rocoto database file"
      echo "  -w, --workflow    Path to Rocoto workflow XML file"
      echo "  -m, --mail        Email address for notifications (default: ${USER}@noaa.gov)"
      echo "  -v, --verbose     Verbose output"
      echo "  -h, --help        Display this help message"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Validate required arguments
if [[ -z "${DATABASE}" ]] || [[ -z "${WORKFLOW}" ]]; then
  echo "ERROR: Both database (-d) and workflow (-w) are required"
  exit 1
fi

# Set lock directory based on workflow location
WORKFLOW_DIR="$(dirname "$(readlink -f "${WORKFLOW}")")"
LOCK_DIR="${WORKFLOW_DIR}/.rocoto_notify_locks"
mkdir -p "${LOCK_DIR}"

# Extract experiment name from workflow file
PSLOT=$(basename "${WORKFLOW}" .xml)

[[ ${VERBOSE} -eq 1 ]] && echo "Monitoring workflow: ${PSLOT}"
[[ ${VERBOSE} -eq 1 ]] && echo "Database: ${DATABASE}"
[[ ${VERBOSE} -eq 1 ]] && echo "Workflow: ${WORKFLOW}"

###############################################################################
# Function: send_email_notification
# Description: Send email notification about failed or stalled jobs
# Arguments:
#   $1 - Notification type (FAILED or STALLED)
#   $2 - Job details (multiline string)
###############################################################################
send_email_notification() {
  local notification_type="${1}"
  local job_details="${2}"
  local subject="${notification_type}: Rocoto workflow ${PSLOT}"
  local timestamp=$(date '+%Y-%m-%d %H:%M:%S')

  local email_body
  read -r -d '' email_body <<EOF || true
Rocoto Workflow Notification
============================

Experiment: ${PSLOT}
Status: ${notification_type}
Time: ${timestamp}
Database: ${DATABASE}
Workflow: ${WORKFLOW}

${notification_type} Jobs:
-----------------------------
${job_details}

-----------------------------
To view the workflow status, run:
  rocotostat -w ${WORKFLOW} -d ${DATABASE}

To check job details, run:
  rocotocheck -w ${WORKFLOW} -d ${DATABASE} -c <CYCLE> -t <TASK>

This is an automated message from rocoto_monitor_notify.sh
EOF

  if command -v mail &> /dev/null; then
    echo "${email_body}" | mail -s "${subject}" "${EMAIL}"
  elif command -v sendmail &> /dev/null; then
    echo -e "Subject: ${subject}\n\n${email_body}" | sendmail "${EMAIL}"
  else
    # Fallback: write to a notification file
    local notify_file="${WORKFLOW_DIR}/${PSLOT}_${notification_type}_$(date +%Y%m%d_%H%M%S).txt"
    echo "${email_body}" > "${notify_file}"
    echo "WARNING: No mail command found. Notification written to: ${notify_file}"
  fi

  [[ ${VERBOSE} -eq 1 ]] && echo "Email notification sent to ${EMAIL}"
}

###############################################################################
# Function: get_failed_jobs
# Description: Get list of failed jobs from rocotostat
# Returns: Multiline string of failed jobs with cycle and task info
###############################################################################
get_failed_jobs() {
  local failed_jobs=""

  # Run rocotostat and capture output
  local rocotostat_output
  rocotostat_output=$("${ROCOTOSTAT}" -w "${WORKFLOW}" -d "${DATABASE}" 2>/dev/null || true)

  # Parse for DEAD or FAILED states
  failed_jobs=$(echo "${rocotostat_output}" | grep -E "DEAD|FAIL" || true)

  echo "${failed_jobs}"
}

###############################################################################
# Function: get_stalled_status
# Description: Detect if workflow is stalled (no progress for extended period)
# Returns: 0 if stalled, 1 if active
###############################################################################
get_stalled_status() {
  # Get workflow state summary
  local state_output
  state_output=$("${ROCOTOSTAT}" -w "${WORKFLOW}" -d "${DATABASE}" -s 2>/dev/null || true)

  # Check if there are QUEUED or RUNNING jobs
  local active_jobs=$(echo "${state_output}" | grep -E "QUEUED|RUNNING" | wc -l)

  # Check if there are only SUCCEEDED and ready-to-run jobs stuck
  local succeeded_jobs=$(echo "${state_output}" | grep "SUCCEEDED" | wc -l)
  local total_jobs=$(echo "${state_output}" | wc -l)

  # Consider stalled if no active jobs but not all succeeded
  if [[ ${active_jobs} -eq 0 ]] && [[ ${succeeded_jobs} -lt ${total_jobs} ]]; then
    # Check stall lock timestamp
    local stall_lock="${LOCK_DIR}/stalled.lock"
    if [[ -f "${stall_lock}" ]]; then
      # Check if stalled for more than 1 hour
      local stall_age=$(($(date +%s) - $(stat -c %Y "${stall_lock}")))
      if [[ ${stall_age} -gt 3600 ]]; then
        return 0  # Stalled
      fi
    else
      # Create stall lock
      touch "${stall_lock}"
    fi
  else
    # Remove stall lock if exists
    rm -f "${LOCK_DIR}/stalled.lock"
  fi

  return 1  # Not stalled
}

###############################################################################
# Function: check_and_notify_failures
# Description: Check for failures and send notifications if not already sent
###############################################################################
check_and_notify_failures() {
  local failed_jobs
  failed_jobs=$(get_failed_jobs)

  if [[ -n "${failed_jobs}" ]]; then
    # Create hash of failed jobs to detect changes
    local failed_hash
    failed_hash=$(echo "${failed_jobs}" | md5sum | cut -d' ' -f1)
    local failed_lock="${LOCK_DIR}/failed_${failed_hash}.lock"

    # Check if we already notified about these failures
    if [[ ! -f "${failed_lock}" ]]; then
      send_email_notification "FAILED" "${failed_jobs}"
      touch "${failed_lock}"

      # Clean up old failure locks (keep only last 10)
      find "${LOCK_DIR}" -name "failed_*.lock" -type f | sort -r | tail -n +11 | xargs rm -f 2>/dev/null || true
    else
      [[ ${VERBOSE} -eq 1 ]] && echo "Failed jobs already notified (lock exists)"
    fi
  else
    # No failures - clean up failed locks
    rm -f "${LOCK_DIR}"/failed_*.lock
  fi
}

###############################################################################
# Function: check_and_notify_stalled
# Description: Check for stalled workflow and send notification
###############################################################################
check_and_notify_stalled() {
  if get_stalled_status; then
    local stall_notify_lock="${LOCK_DIR}/stalled_notified.lock"

    # Only notify once per stall event
    if [[ ! -f "${stall_notify_lock}" ]]; then
      local stall_info
      stall_info=$("${ROCOTOSTAT}" -w "${WORKFLOW}" -d "${DATABASE}" 2>/dev/null || echo "Unable to get workflow status")
      send_email_notification "STALLED" "${stall_info}"
      touch "${stall_notify_lock}"
    fi
  else
    # Not stalled - remove notification lock
    rm -f "${LOCK_DIR}/stalled_notified.lock"
  fi
}

###############################################################################
# Main execution
###############################################################################
main() {
  # Run rocotorun
  [[ ${VERBOSE} -eq 1 ]] && echo "Running: ${ROCOTORUN} -w ${WORKFLOW} -d ${DATABASE}"

  local rocotorun_output
  rocotorun_output=$("${ROCOTORUN}" -w "${WORKFLOW}" -d "${DATABASE}" 2>&1)
  local rocotorun_status=$?

  [[ ${VERBOSE} -eq 1 ]] && [[ -n "${rocotorun_output}" ]] && echo "${rocotorun_output}"

  # Check for failures
  check_and_notify_failures

  # Check for stalled workflows
  check_and_notify_stalled

  # Return rocotorun status
  return ${rocotorun_status}
}

# Execute main function
main "$@"
