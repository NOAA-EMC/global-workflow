#!/usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         run_mpmd.sh
# Script description:  Run multiple commands in MPMD mode or serially
#
# Author:   Rahul Mahajan
#
# Org:      NCEP/EMC
#
# Abstract: This script runs multiple commands in MPMD mode. It is used to run
#           multiple serial commands in parallel using the CFP (Coupled Framework
#           Parallelism) feature of the workflow.
#
# Environment variables:
#           USE_CFP: If set to YES, run in MPMD mode, else run in serial mode. Default is 'NO'.
#           launcher: Command to launch the MPMD job. Default is empty.
#                     Supported launchers are 'srun' and 'mpiexec'.
#           mpmd_opt: Additional options to pass to the launcher. Default is empty.
#
# Input:
#           cmdfile: File containing commands to execute in MPMD/serial mode
#
# Command line:
#           run_mpmd.sh cmdfile
#
################################################################################

source "${USHgfs}/preamble.sh"

source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"

if [[ "$MACHINE_ID" == "container" ]]; then
  source /usr/lmod/lmod/init/bash
  module use "${HOMEgfs}/sorc/gfs_utils.fd/modulefiles"
  module load gfsutils_container.intel
  module load prod_util
  module load wgrib2
fi

cmdfile=${1:?"run_mpmd requires an input file containing commands to execute in MPMD/serial mode"}

# If USE_CFP is not set, run in serial mode
if [[ "${USE_CFP:-}" != "YES" ]]; then
  echo "INFO: Using serial mode for MPMD job"
  chmod 755 "${cmdfile}"
  bash +x "${cmdfile}" > mpmd.out 2>&1
  rc=$?
  cat mpmd.out
  exit "${rc}"
fi

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1

# Redirect output from each process to its own stdout
# Read the incoming cmdfile and create mpiexec usable cmdfile
nm=0
# shellcheck disable=SC2312
while IFS= read -r line; do
  echo "Line ${nm}: ${line}"
  ${line} > "mpmd.${nm}.out" &
  ((nm=nm+1))
done < "${cmdfile}"
wait

err=$?
set_strict

# On success concatenate processor specific output into a single mpmd.out
if [[ ${err} -eq 0 ]]; then
  out_files=$(find . -name 'mpmd.*.out')
  for file in ${out_files}; do
    cat "${file}" >> mpmd.out
    rm -f "${file}"
  done
  cat mpmd.out
fi

exit "${err}"
