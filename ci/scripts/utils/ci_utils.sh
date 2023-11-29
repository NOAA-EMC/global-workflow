#!/bin/env bash

function cancel_slurm_jobs() {

  local substring=$1
  local job_ids
  job_ids=$(squeue -u "${USER}" -h -o "%i")

  for job_id in ${job_ids}; do
    job_name=$(sacct -j "${job_id}" --format=JobName%100 | head -3 | tail -1 | sed -r 's/\s+//g') || true
    if [[ "${job_name}" =~ ${substring} ]]; then
      echo "Canceling Slurm Job ${job_name} with: scancel ${job_id}"
      scancel "${job_id}"
    fi
  done
}
