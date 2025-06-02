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

# Determine the number of MPMD processes from incoming ${cmdfile}
nprocs=$(wc -l < "${cmdfile}")

# Local MPMD file containing instructions to run in CFP
mpmd_cmdfile="${DATA:-}/mpmd_cmdfile"
if [[ -s "${mpmd_cmdfile}" ]]; then rm -f "${mpmd_cmdfile}"; fi

cat << EOF
  INFO: Executing MPMD job, STDOUT redirected for each process separately
  INFO: On failure, logs for each job will be available in ${DATA}/mpmd.proc_num.out
  INFO: The proc_num corresponds to the line in '${mpmd_cmdfile}'
EOF

if [[ "${launcher:-}" =~ ^srun.* ]]; then  #  srun-based system e.g. Hera, Orion, etc.

  # Slurm requires a counter in front of each line in the script
  # Read the incoming cmdfile and create srun usable cmdfile
  nm=0
  # shellcheck disable=SC2312
  while IFS= read -r line; do
    echo "${nm} ${line}" >> "${mpmd_cmdfile}"
    ((nm=nm+1))
  done < "${cmdfile}"

  set +e
  # shellcheck disable=SC2086
  ${launcher:-} ${mpmd_opt:-} -n ${nprocs} "${mpmd_cmdfile}"
  err=$?
  set_strict

elif [[ "${launcher:-}" =~ ^mpiexec.* ]]; then  # mpiexec

  # Redirect output from each process to its own stdout
  # Read the incoming cmdfile and create mpiexec usable cmdfile
  nm=0
  echo "#!/bin/bash" >> "${mpmd_cmdfile}"
  # shellcheck disable=SC2312
  while IFS= read -r line; do
    echo "${line} > mpmd.${nm}.out" >> "${mpmd_cmdfile}"
    ((nm=nm+1))
  done < "${cmdfile}"
  chmod 755 "${mpmd_cmdfile}"

  # shellcheck disable=SC2086
  ${launcher:-} -np ${nprocs} ${mpmd_opt:-} "${mpmd_cmdfile}"
  err=$?

else

  echo "FATAL ERROR: CFP is not usable with launcher: '${launcher:-}'"
  err=1

fi

# On success concatenate processor specific output into a single mpmd.out
if [[ ${err} -eq 0 ]]; then
  rm -f "${mpmd_cmdfile}"
  out_files=$(find . -name 'mpmd.*.out')
  for file in ${out_files}; do
    cat "${file}" >> mpmd.out
    rm -f "${file}"
  done
  cat mpmd.out
fi

exit "${err}"
