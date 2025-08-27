#!/usr/bin/env bash

# slurm_job_info.sh
# Query and display all available information about a Slurm job by JOB ID
# Usage: ./slurm_job_info.sh <JOB_ID>

set -euo pipefail

JOB_ID="${1:-}"

if [[ -z "${JOB_ID}" ]]; then
  echo "Usage: $0 <JOB_ID>"
  exit 1
fi

# Display basic job info
sacct -j "${JOB_ID}" --format=JobID,JobName,Partition,Account,AllocCPUS,State,ExitCode,Elapsed,Start,End,NodeList,User,MaxRSS,MaxVMSize,ReqMem,Timelimit,Submit,WorkDir,Command

# Display job details
scontrol show job "${JOB_ID}"

# Display job logs if available
if [[ -n $(find . -type f -name "slurm-${JOB_ID}*.out" 2>/dev/null) ]]; then
  echo "\n--- Slurm Output Logs ---"
  find . -type f -name "slurm-${JOB_ID}*.out" -exec cat {} +
fi
