#!/usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

cmdfile=${1:?"run_mpmd requires an input file containing commands to execute in MPMD mode"}

# This is a partial implementation for Slurm-only
# TODO: extend to PBS, etc.

# Slurm requires a counter in front of each line in the script
# Read the incoming cmdfile and create srun usable cmdfile
if [[ -s "${DATA:-}/cmdfile_srun" ]]; then rm -f "${DATA:-}/poescript_srun"; fi
nm=0
# shellcheck disable=SC2312
cat "${cmdfile}" | while read line; do
  echo "${nm} ${line}" >> "${DATA:-}/cmdfile_srun"
  ((nm=nm+1))
done

nprocs=$(wc -l < "${DATA:-}/cmdfile_srun")
set +e
${launcher:-} "${mpmd_opt:-}" -n "${nprocs}" "${DATA:-}/cmdfile_srun"
rc=$?
set_strict
if (( rc == 0 )); then
  rm -f "${DATA:-}/cmdfile_srun"
fi
# On success concatenate processor specific output into a single mpmd.out
if (( rc == 0 )); then
  out_files=$(ls | grep -P "[0-9]*?.out")
  for file in ${out_files}; do
    cat "${file}" >> mpmd.out
    rm -f "${file}"
  done
fi

exit "${rc}"
