#!/bin/bash
# Mock job script for testing Rocoto workflow
echo "Starting mock job: $0"
echo "Job Name: ${LSB_JOBNAME:-${SLURM_JOB_NAME:-unknown}}"
echo "Job ID: ${LSB_JOBID:-${SLURM_JOB_ID:-unknown}}"
echo "Sleeping for 20 seconds..."
sleep 20
echo "Mock job completed successfully"
exit 0
