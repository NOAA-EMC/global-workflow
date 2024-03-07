#!/usr/bin/env bash

source "${USHgfs}/preamble.sh"

cmdfile=${1:?"run_mpmd requires an input file containing commands to execute in MPMD mode"}

# Determine the number of MPMD processes from incoming ${cmdfile}
nprocs=$(wc -l < "${cmdfile}")

# Local MPMD file containing instructions to run in CFP
mpmd_cmdfile="${DATA:-}/mpmd_cmdfile"
if [[ -s "${mpmd_cmdfile}" ]]; then rm -f "${mpmd_cmdfile}"; fi

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
  rc=$?
  set_strict
  if (( rc == 0 )); then
    out_files=$(find . -name 'mpmd.*.*.out')
  fi

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
  rc=$?
  if (( rc == 0 )); then
    out_files=$(find . -name 'mpmd.*.out')
  fi

else

  echo "FATAL ERROR: CFP is not usable with launcher: '${launcher:-}'"
  rc=1

fi

# On success concatenate processor specific output into a single mpmd.out
if (( rc == 0 )); then
  rm -f "${mpmd_cmdfile}"
  for file in ${out_files}; do
    cat "${file}" >> mpmd.out
    rm -f "${file}"
  done
fi

exit "${rc}"
