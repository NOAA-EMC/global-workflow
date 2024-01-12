#!/bin/env bash

function cancel_batch_jobs() {
  # Usage: cancel_batch_jobs <substring>
  # Example: cancel_batch_jobs "C48_ATM_3c4e7f74"
  #
  # Cancel all batch jobs that have the given substring in their name
  # So like in the example all jobs with "C48_ATM_3c4e7f74"
  # in their name will be canceled

  local substring=$1
  local job_ids

  # cancel pbs jobs <substring>
  if [[ ${MACHINE_ID} == "wcoss2" ]]; then
    job_ids=$(qstat -u "${USER}" | awk '{print $1}')

    for job_id in ${job_ids}; do
      job_name=$(qstat -f "${job_id}" | grep Job_Name | awk '{print $3}') || true
      if [[ "${job_name}" =~ ${substring} ]]; then
        echo "Canceling PBS Job ${job_name} with: qdel ${job_id}"
        qdel "${job_id}"
        continue
      fi
    done
  # cancel slurm jobs <substring>
  else 
    job_ids=$(squeue -u "${USER}" -h -o "%i")

    for job_id in ${job_ids}; do
      job_name=$(sacct -j "${job_id}" --format=JobName%100 | head -3 | tail -1 | sed -r 's/\s+//g') || true
      if [[ "${job_name}" =~ ${substring} ]]; then
        echo "Canceling Slurm Job ${job_name} with: scancel ${job_id}"
        scancel "${job_id}"
        continue
      fi
    done
  fi
}


function get_pr_case_list () {

    #############################################################
    # loop over every yaml file in the PR's ci/cases
    # and create an run directory for each one for this PR loop
    #############################################################
    for yaml_config in "${HOMEgfs}/ci/cases/pr/"*.yaml; do
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
    for pslot_dir in "${RUNTESTS}/EXPDIR/"*; do
      pslot=$(basename "${pslot_dir}")
      check_case=$(echo "${pslot}" | rev | cut -d"_" -f2- | rev) || true
      if [[ "${check_case}" == "${case}" ]]; then
        echo "${pslot}"
        break
      fi
    done

}

function create_experiment () {

  local yaml_config="${1}"
  cd "${HOMEgfs}" || exit 1
  pr_sha=$(git rev-parse --short HEAD)
  case=$(basename "${yaml_config}" .yaml) || true
  
  source "${HOMEgfs}/ci/platforms/config.${MACHINE_ID}"
  source "${HOMEgfs}/workflow/gw_setup.sh"

  export pslot=${case}_${pr_sha}
  "${HOMEgfs}/workflow/create_experiment.py" --yaml "${yaml_config}"

}