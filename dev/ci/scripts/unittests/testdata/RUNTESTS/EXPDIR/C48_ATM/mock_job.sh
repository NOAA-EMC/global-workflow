#!/bin/bash
# Mock job script for testing Rocoto workflow
echo "Starting mock job: $0"
echo "Job Name: ${SLURM_JOB_NAME:-unknown}"
echo "Job ID: ${SLURM_JOB_ID:-unknown}"
echo "Partition: ${SLURM_JOB_PARTITION:-unknown}"
echo "Account: ${SLURM_JOB_ACCOUNT:-unknown}"
echo "Sleeping for 20 seconds..."
sleep 20
echo "Mock job completed successfully"
exit 0
