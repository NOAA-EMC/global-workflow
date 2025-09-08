#!/usr/bin/env bash

set -eu

#####################################################################################
# Script description: script to check the status of an experiment as reported
#                     by Rocoto and report failures to GitHub PR if applicable
#####################################################################################


TEST_DIR=${1:-${TEST_DIR:-?}}  # Location of the root of the testing directory
pslot=${2:-${pslot:-?}}        # Name of the experiment being tested by this script
SYSTEM_BUILD_DIR=${3:-"global-workflow"}  # Name of the system build directory, default is "global-workflow

# TEST_DIR contains 2 directories;
# 1. HOMEgfs: clone of the global-workflow
# 2. RUNTESTS: A directory containing EXPDIR and COMROOT for experiments
# # e.g. $> tree ./TEST_DIR
# ./TEST_DIR
# ├── HOMEgfs
# └── RUNTESTS
#     ├── COMROOT
#     │   └── ${pslot}
#     └── EXPDIR
#         └── ${pslot}
# Two system build directories created at build time gfs, and gdas
# TODO: Make this configurable (for now all scripts run from gfs for CI at runtime)

# -----------------------------------------------------------------------------------
# GitHub PR Failure Reporting
# -----------------------------------------------------------------------------------
# This script supports reporting failed experiment cases to GitHub PR feeds via the
# report_failure_to_github() routine. For this to work, the following environment
# variables must be set:
#   - GH: Path to the GitHub CLI executable (e.g., 'gh')
#   - PR_NUMBER: The pull request number to comment on
#   - GW_REPO_URL: The GitHub repository URL (e.g., 'NOAA-EMC/global-workflow')
#   - CI_PIPELINE_ID: The GitLab pipeline ID (used for context in the comment)
#   - MACHINE_ID: The machine identifier (used for labeling)
#
# These variables are required for the script to post comments and update labels on the
# relevant GitHub PR. If any are missing, PR reporting will be skipped for failed cases.
# -----------------------------------------------------------------------------------


HOMEgfs="${TEST_DIR}/${SYSTEM_BUILD_DIR}"
RUNTESTS="${TEST_DIR}/RUNTESTS"
run_check_logfile="${RUNTESTS}/ci-run_check.log"

# Function to report experiment failure to GitHub
report_failure_to_github() {
  local pslot="${1}"
  local Machine="${MACHINE_ID^}"
  local caseName="${caseName:-${pslot%_*-*}}"
  local error_log_file="${RUNTESTS}/EXPDIR/${pslot}/${pslot}_fullpath_error.logs"
  local gist_message_section=""

  echo "================================================================================"
  echo "FAILURE DETECTED: Found error log files in ${RUNTESTS}/EXPDIR/${pslot}"
  echo "Error log file: ${error_log_file}"
  echo "================================================================================"

  # Create processed logs directory to prevent reprocessing
  DATE=$(date +%Y%m%d_%H%M%S)
  local processed_dir="${RUNTESTS}/EXPDIR/${pslot}/error_logs/${DATE}" || true
  mkdir -p "${processed_dir}"

  if [[ -f "${error_log_file}" && -s "${error_log_file}" ]]; then
    echo "Processing log reports to GitHub for failure with case: ${caseName}, pslot: ${pslot}"
    local error_logs_for_gist=""
    local error_logs_markdown=""

    while IFS= read -r full_log_path; do
      [[ -n "${full_log_path}" ]] || continue

      if [[ -f "${full_log_path}" && -s "${full_log_path}" ]]; then
        error_logs_for_gist="${error_logs_for_gist} ${full_log_path}"
        error_logs_markdown=$(echo -e "${error_logs_markdown}\n${full_log_path}")
      fi
    done < "${error_log_file}"

    if [[ -n "${error_logs_for_gist}" ]]; then
      # Generate gist URLs with formatted markdown links
      source "${HOMEgfs}/dev/ush/gw_setup.sh"
      # shellcheck disable=SC2027,SC2086,SC2155
      local gist_links=$("${HOMEgfs}/dev/ci/scripts/utils/publish_logs.py" \
      --file ${error_logs_for_gist} --multiple --format=github \
      --gist "PR_${PR_NUMBER}_${caseName}" | tail -n 1) || true

      # Upload to repo as well for backup
      # shellcheck disable=SC2027,SC2086
      "${HOMEgfs}/dev/ci/scripts/utils/publish_logs.py" \
      --file ${error_logs_for_gist} --repo "PR_${PR_NUMBER}_${caseName}" || true

      # Prepare markdown section for files links to gist for GitHub comment
      gist_message_section=$(cat <<EOF
Error Log Files:
\`\`\`
${error_logs_markdown}
\`\`\`
View Error Logs: ${gist_links}
EOF
      )
    else
      echo "No valid error log files found for case: ${caseName}, pslot: ${pslot}"
      gist_message_section="No valid error log files found for this case."
    fi
  fi

  # Create formatted GitHub comment
  comment_body=$(cat << EOF
_${caseName}_ **FAILED** on ${Machine}  (pipeline ID: ${CI_PIPELINE_ID})

In directory: \`${GW_RUN_PATH}/RUNTESTS/EXPDIR/${pslot}\`

${gist_message_section}

_This failure was detected automatically by global-workflow's CI/CD Pipeline_
EOF
  )

  # Post GitHub comment
  cd "${HOMEgfs}"
  "${GH}" pr comment "${PR_NUMBER}" --repo "${GW_REPO_URL}" --body "${comment_body}" || true

  # Move processed error log to prevent reprocessing
  if [[ -f "${error_log_file}" ]]; then
    mv "${error_log_file}" "${processed_dir}/"
  fi

  # Update GitHub labels
  "${GH}" pr edit "${PR_NUMBER}" --repo "${GW_REPO_URL}" --add-label "CI-${Machine}-Failed" --remove-label "CI-${Machine}-Running" || true
}

# Source modules and setup logging
echo "Source modules."
source "${HOMEgfs}/dev/ush/gw_setup.sh"
# TODO We need to add local python env to support PyGitHub
PYTHONPATH="${PYTHONPATH}:$(python3 -m site --user-site)" || true
echo "Updated PYTHONPATH: ${PYTHONPATH}"

# cd into the experiment directory
echo "cd ${RUNTESTS}/EXPDIR/${pslot}"
cd "${RUNTESTS}/EXPDIR/${pslot}" || (echo "FATAL ERROR: Unable to cd into '${RUNTESTS}/EXPDIR/${pslot}', ABORT!"; exit 1)

# Name of the Rocoto XML and database files
xml="${pslot}.xml"
db="${pslot}.db"

# Ensure the XML is present for the experiment
if [[ ! -f "${xml}" ]]; then
  echo "FATAL ERROR: XML file ${xml} not found in '${pslot}', experiment ${pslot} failed, ABORT!"
  exit 1
fi

# Launch experiment
echo "Launch experiment with Rocoto."
rocotorun -v "${ROCOTO_VERBOSE:-0}" -w "${xml}" -d "${db}"
sleep 10
if [[ ! -f "${db}" ]]; then
  echo "FATAL ERROR: Rocoto database file ${db} not found, experiment ${pslot} failed, ABORT!"
  exit 1
fi

# Experiment launched
rc=99
set +e
while true; do

  echo "running rocotorun for (${pslot} on ${MACHINE_ID^})"
  rocotorun -v "${ROCOTO_VERBOSE:-0}" -w "${xml}" -d "${db}"

  # Wait a minute before running rocotostat
  sleep 60

  caseName="${pslot%_*-*}"  # caseName recovered from pslot: (caseName_<hash>-<pipeline ID> (eg. C48_ATM_90f10fc1-3517)
  echo "Gather Rocoto statistics for (${caseName} on ${MACHINE_ID^})"
  export ROCOTOSTAT_LOG_FILE="${RUNTESTS}/EXPDIR/${pslot}/logs/${caseName}_rocotostat.log"
  source <("${HOMEgfs}/dev/ci/scripts/utils/rocotostat.py" -w "${xml}" -d "${db}" --declare --thread-logging) || true

  echo -e "\tCompleted Cycles: ${CYCLES_DONE}/${CYCLES_TOTAL}
  \tCompleted Jobs  : ${JOBS_DONE}/${JOBS_TOTAL}
  \tState           : ${ROCOTO_STATE}"

  failure_states="FAIL UNAVAILABLE UNKNOWN STALLED"
  # shellcheck disable=SC2076  # We want literal string matching, not regex
  if [[ " ${failure_states} " =~ " ${ROCOTO_STATE} " ]]; then
    {
      echo "Experiment ${pslot} Terminated with state ${ROCOTO_STATE}: ${FAIL} tasks failed, ${DEAD} dead at $(date)" || true
    } | tee -a "${run_check_logfile}"
    error_logs=$(rocotostat -d "${db}" -w "${xml}" | grep -E 'FAIL|DEAD' | awk '{print "-c", $1, "-t", $2}' | xargs rocotocheck -d "${db}" -w "${xml}" | grep join | awk '{print $2}') || true
    {
      echo "Error logs:"
      echo "${error_logs}"
    } | tee -a  "${run_check_logfile}"
    rm -f "${RUNTESTS}/EXPDIR/${pslot}/${pslot}_error.logs"
    for log in ${error_logs}; do
      echo "RUNTESTS${log#*RUNTESTS}" >> "${RUNTESTS}/EXPDIR/${pslot}/${pslot}_error.logs"
      echo "${log}" >> "${RUNTESTS}/EXPDIR/${pslot}/${pslot}_fullpath_error.logs"
    done
   
   # Report failure to GitHub if running in CI environment
   if [[ -n "${CI_PIPELINE_ID:-}" && -n "${PR_NUMBER:-}" && "${PR_NUMBER}" != "0" ]]; then
     report_failure_to_github "${pslot}"
   fi
   
   rc=1
   break
  fi

  if [[ "${ROCOTO_STATE}" == "DONE" ]]; then
    {
      echo "Experiment ${pslot} Completed ${CYCLES_DONE} Cycles: *SUCCESS* at $(date)" || true
    } | tee -a "${run_check_logfile}"
    rc=0
    break
  fi

  # Wait before running rocotorun again
  sleep 300

done

exit "${rc}"
