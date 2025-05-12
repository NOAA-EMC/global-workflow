#!/usr/bin/env bash

# Determine HOMEgfs_ and source machine detection early
if [[ -z "${HOMEgfs_}" ]]; then
  HOMEgfs_="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"
fi
source "${HOMEgfs_}/ush/detect_machine.sh"

# --- Existing functions ---

function determine_scheduler() {
  if command -v sbatch &> /dev/null; then
    echo "slurm";
  elif command -v qsub &> /dev/null; then
    echo "torque";
  else
    echo "unknown"
  fi
}

function cancel_batch_jobs() {
  # Usage: cancel_batch_jobs <substring>
  # Example: cancel_batch_jobs "C48_ATM_3c4e7f74"
  #
  # Cancel all batch jobs that have the given substring in their name
  # So like in the example all jobs with "C48_ATM_3c4e7f74"
  # in their name will be canceled

  local substring=$1
  local job_ids

  scheduler=$(determine_scheduler)

  if [[ "${scheduler}" == "torque" ]]; then
    job_ids=$(qstat -u "${USER}" | awk '{print $1}') || true

    for job_id in ${job_ids}; do
      job_name=$(qstat -f "${job_id}" | grep Job_Name | awk '{print $3}') || true
      if [[ "${job_name}" =~ ${substring} ]]; then
        echo "Canceling PBS Job ${job_name} with: qdel ${job_id}"
        qdel "${job_id}"
        continue
      fi
    done

  elif [[ "${scheduler}" == "slurm" ]]; then

    job_ids=$(squeue -u "${USER}" -h -o "%i")

    for job_id in ${job_ids}; do
      job_name=$(sacct -j "${job_id}" --format=JobName%100 | head -3 | tail -1 | sed -r 's/\s+//g') || true
      if [[ "${job_name}" =~ ${substring} ]]; then
        echo "Canceling Slurm Job ${job_name} with: scancel ${job_id}"
        scancel "${job_id}"
        continue
      fi
    done

  else
      echo "FATAL: Unknown/unsupported job scheduler"
      exit 1
  fi
}


function get_pr_case_list () {

    #############################################################
    # loop over every yaml file in the PR's ci/cases
    # and create an run directory for each one for this PR loop
    #############################################################
    for yaml_config in "${HOMEgfs_}/dev/ci/cases/pr/"*.yaml; do
      case=$(basename "${yaml_config}" .yaml) || true
      echo "${case}"
    done
}

function get_pslot_list () {

    local RUNTESTS="${1}"

    #############################################################
    # loop over expdir directories in RUNTESTS
    # and create list of the directory names (pslot) with the hash tag
    #############################################################
    for pslot_dir in "${RUNTESTS}/EXPDIR/"*; do
      pslot=$(basename "${pslot_dir}") || true
      echo "${pslot}"
    done

}

function get_pslot () {

    local RUNTESTS="${1}"
    local case="${2}"

    #############################################################
    # loop over expdir directories in RUNTESTS
    # and return the name of the pslot with its tag that matches the case
    #############################################################
    # shellcheck disable=SC2045
    for pslot_dir in $(ls -td "${RUNTESTS}/EXPDIR/"*); do
      pslot=$(basename "${pslot_dir}")
      check_case=$(echo "${pslot}" | rev | cut -d"_" -f2- | rev) || true
      if [[ "${check_case}" == "${case}" ]]; then
        echo "${pslot}"
        break
      fi
    done

}

function cancel_all_batch_jobs () {
  local RUNTESTS="${1}"
  pslot_list=$(get_pslot_list "${RUNTESTS}")
  for pslot in ${pslot_list}; do
    cancel_batch_jobs "${pslot}"
  done
}

function create_experiment () {

  local yaml_config="${1}"
  cd "${HOMEgfs_}" || exit 1
  pr_sha=$(git rev-parse --short HEAD)
  case=$(basename "${yaml_config}" .yaml) || true
  export pslot=${case}_${pr_sha}

  if [[ ${MACHINE_ID} == "noaacloud" ]]; then
      source "${HOMEgfs_}/dev/ci/platforms/config.${PW_CSP}"
  else
      source "${HOMEgfs_}/dev/ci/platforms/config.${MACHINE_ID}"
  fi

  source "${HOMEgfs_}/dev/ush/gw_setup.sh"

  # Remove RUNDIRS dir incase this is a retry (STMP now in host file)
  if [[ ${MACHINE_ID} == "noaacloud" ]]; then
      STMP=$("${HOMEgfs_}/dev/ci/scripts/utils/parse_yaml.py" -y "${HOMEgfs_}/dev/workflow/hosts/${PW_CSP}pw.yaml" -k STMP -s)
  else
      STMP=$("${HOMEgfs_}/dev/ci/scripts/utils/parse_yaml.py" -y "${HOMEgfs_}/dev/workflow/hosts/${MACHINE_ID}.yaml" -k STMP -s)
  fi
  echo "Removing ${STMP}/RUNDIRS/${pslot} directory incase this is a retry"
  rm -Rf "${STMP}/RUNDIRS/${pslot}"

  "${HOMEgfs_}/${system}/dev/workflow/create_experiment.py" --overwrite --yaml "${yaml_config}"

}

function publish_logs() {
    # publish_logs function
    # This function takes a directory path and a list of files as arguments.
    # It calls the publish_logs.py script to publish the logs and returns its gist URL.
    # Usage: publish_logs <ID> <dir_path> <file1> <file2> ... <fileN>
    local PR_header="$1"
    local dir_path="$2"
    local file="$3"
    local full_paths=""
    while IFS= read -r line; do
        full_path="${dir_path}/${line}"
        if [[ -f "${full_path}" ]]; then
            full_paths+="${full_path} "
        else
            echo "File ${full_path} does not exist"
        fi
    done < "${file}"

    if [[ -n "${full_paths}" ]]; then
        # shellcheck disable=SC2027,SC2086
        ${HOMEgfs_}/dev/ci/scripts/utils/publish_logs.py --file ${full_paths} --repo ${PR_header} > /dev/null
        URL="$("${HOMEgfs_}/dev/ci/scripts/utils/publish_logs.py" --file "${full_paths}" --gist "${PR_header}")"
    fi
    echo "${URL}"
}

function cleanup_experiment() {

    local EXPDIR="$1"
    local pslot
    local ARCDIR
    local ATARDIR
    local COMROOT

    EXPDIR="$1"
    pslot=$(basename "${EXPDIR}")

    # Use the Python utility to get the required variables
    read -r ARCDIR ATARDIR STMP COMROOT < <("${HOMEgfs_}/dev/ci/scripts/utils/get_config_var.py" ARCDIR ATARDIR STMP COMROOT "${EXPDIR}") || true

    rm -Rf "${ARCDIR:?}"
    rm -Rf "${ATARDIR:?}"
    rm -Rf "${COMROOT}/${pslot:?}"
    rm -Rf "${EXPDIR}/${pslot:?}"
    rm -Rf "${STMP}/RUNDIRS/${pslot:?}"
}

function build () {

  source "${HOMEgfs_}/dev/ci/platforms/config.${MACHINE_ID}"
  # TODO: when it's safe to build on C6 compute nodes again, do so
  if [[ "${MACHINE_ID}" == "gaeac6" ]]; then
    "${HOMEgfs_}/sorc/build_all.sh" -v -k all
  else
    "${HOMEgfs_}/sorc/build_compute.sh" -A "${HPC_ACCOUNT}" -v all
  fi

}

# --- Dispatch logic ---

# Check if the script is being executed directly (not sourced)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # Script is being executed directly
    utility_function="${1}"
    shift # Remove the function name from the arguments list

    # Check if the first argument corresponds to a defined function
    type_t="$(type -t "${utility_function}")" || true
    if [[  "${type_t}" == "function" ]]; then
        # Call the function with the remaining arguments
        "${utility_function}" "$@"
    else
        echo "ERROR: Utility function ${utility_function} not found or not a function in ${BASH_SOURCE[0]}" >&2
        exit 1
    fi
fi
