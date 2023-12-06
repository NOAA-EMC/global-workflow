#!/bin/env bash

function cancel_slurm_jobs() {

  # Usage: cancel_slurm_jobs <substring>
  # Example: cancel_slurm_jobs "C48_ATM_3c4e7f74"
  #
  # Cancel all Slurm jobs that have the given substring in their name
  # So like in the example all jobs with "C48_ATM_3c4e7f74"
  # in their name will be canceled

  local substring=$1
  local job_ids
  job_ids=$(squeue -u "${USER}" -h -o "%i")

  for job_id in ${job_ids}; do
    job_name=$(sacct -j "${job_id}" --format=JobName%100 | head -3 | tail -1 | sed -r 's/\s+//g') || true
    if [[ "${job_name}" =~ ${substring} ]]; then
      echo "Canceling Slurm Job ${job_name} with: scancel ${job_id}"
      scancel "${job_id}"
      continue
    fi
  done
}
