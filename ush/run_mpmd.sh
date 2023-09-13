#!/usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

cmdfile=${1:?"run_mpmd requires an input file containing commands to execute in MPMD mode"}

# This is a partial implementation for Slurm-only
# TODO: extend to PBS, etc.

if [[ "${launcher:-}" =~ ^srun.* ]]; then  #  srun-based system e.g. Hera, Orion, etc.

  # Slurm requires a counter in front of each line in the script
  # Read the incoming cmdfile and create srun usable cmdfile
  if [[ -s "${DATA:-}/cmdfile_srun" ]]; then rm -f "${DATA:-}/poescript_srun"; fi
  nm=0
  # shellcheck disable=SC2312
  while IFS= read -r line; do
    echo "${nm} ${line}" >> "${DATA:-}/cmdfile_srun"
    ((nm=nm+1))
  done < "${cmdfile}"

  nprocs=$(wc -l < "${DATA:-}/cmdfile_srun")
  set +e
  ${launcher:-} ${mpmd_opt:-} -n ${nprocs} "${DATA:-}/cmdfile_srun"
  rc=$?
  set_strict
  if (( rc == 0 )); then
    out_files=$(find . -name 'mpmd.*.*.out')
    rm -f "${DATA:-}/cmdfile_srun"
  fi

elif [[ "${launcher:-}" =~ ^mpiexec.* ]]; then  # mpiexec

  # Redirect output from each process to its own stdout
  # Read the incoming cmdfile and create mpiexec usable cmdfile
  if [[ -s "${DATA:-}/cmdfile_mpiexec" ]]; then rm -f "${DATA:-}/poescript_mpiexec"; fi
  nm=0
  # shellcheck disable=SC2312
  while IFS= read -r line; do
    echo "${line} > mpmd.${nm}.out" >> "${DATA:-}/cmdfile_mpiexec"
    ((nm=nm+1))
  done < "${cmdfile}"

  export MP_PGMMODEL="mpmd"
  export MP_CMDFILE="${DATA:-}/cmdfile_mpiexec"
  ${launcher:-} "${MP_CMDFILE}"  # TODO: Is MP_CMDFILE needed?  Can it just be cmdfile?
  rc=$?
  if (( rc == 0 )); then
    out_files=$(find . -name 'mpmd.*.out')
    rm -f "${DATA:-}/cmdfile_mpiexec"
  fi

else

  echo "FATAL ERROR: CFP is not usable with launcher: '${launcher:-}'"
  rc=1

fi

# On success concatenate processor specific output into a single mpmd.out
if (( rc == 0 )); then
  for file in ${out_files}; do
    cat "${file}" >> mpmd.out
    rm -f "${file}"
  done
fi

exit "${rc}"