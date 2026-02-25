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
#                     Example:
#                            srun: "--multi-prog --output=mpmd.%j.%t.out"
#                         mpiexec: "--cpu-bind verbose,core cfp"
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

# If USE_CFP is not set or is not YES, run in serial mode
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
if [[ -s "${mpmd_cmdfile}" ]]; then
    rm -f "${mpmd_cmdfile}"
fi

cat << EOF
INFO: Executing MPMD job, STDOUT and STDERR redirected for each process separately
INFO: On failure, logs for each job will be available in ${DATA}/mpmd.proc_num.out
INFO: The proc_num corresponds to the line in '${mpmd_cmdfile}'
EOF

chunk_mpmd() {
    # Usage chunk_mpmd mpmd_cmdfile chunk_size chunk_file
    # This takes a chunk of the full mpmd command file and creates a new chunk
    # file with the specified number of lines, while removing those lines from the
    # original file to prevent running the same lines multiple times.
    # Use this function when the number of MPMD tasks is greater than the maximum tasks per node.
    local mpmd_file="${1}"
    local chunk_sz="${2}"
    local chunk_file="${3}"
    if [[ ! -s "${mpmd_file}" ]]; then
        echo "ERROR: MPMD command file '${mpmd_file}' is empty or does not exist."
        return 1
    fi
    head -n ${chunk_sz} "${mpmd_file}" > "${chunk_file}" && true
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: Failed to create chunk file '${chunk_file}' from '${mpmd_file}'"
        return "${err}"
    fi
    sed -i "1,${chunk_size}d" "${mpmd_file}"
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: Failed to remove lines from '${mpmd_file}' after creating chunk file '${chunk_file}'"
        return "${err}"
    fi
    return 0
}

if [[ "${launcher:-}" =~ ^srun.* ]]; then #  srun-based system e.g. Hera, Orion, etc.

    # Slurm requires a counter in front of each line in the script
    # Read the incoming cmdfile and create srun usable cmdfile
    nm=0
    while IFS= read -r line; do
        echo "${nm} ${line}" >> "${mpmd_cmdfile}"
        ((nm = nm + 1))
    done < "${cmdfile}"

    # For now, keep all MPMD tasks on one node.
    # Test if the number of lines in the cmdfile is greater than the number of tasks per node ($max_tasks_per_node).
    # If needed, split the mpmd_cmdfile and run it in chunks.
    # TODO: consider running the MPMD job across multiple nodes.

    if [[ ${nm} -gt ${max_tasks_per_node:-1} ]]; then
        echo "WARNING: Number of MPMD tasks (${nm}) is greater than the maximum tasks per node (${max_tasks_per_node:-1})."
        echo "         Running MPMD job in chunks of ${max_tasks_per_node:-1} tasks per node."
        chunk_size=${max_tasks_per_node:-1}
        # Create a temporary copy of the mpmd_cmdfile
        tmp_file="${mpmd_cmdfile}.tmp"
        cp "${mpmd_cmdfile}" "${tmp_file}"
        for ((i = 0; i < nm; i += chunk_size)); do
            chunk_file="${mpmd_cmdfile}.chunk${i}"
            chunk_mpmd "${tmp_file}" "${chunk_size}" "${chunk_file}"
            err=$?
            if [[ ${err} -ne 0 ]]; then
                echo "FATAL ERROR: Failed to create chunk file '${chunk_file}' from '${tmp_file}'"
                break
            fi
            unset_strict
            # shellcheck disable=SC2086
            ${launcher:-} ${mpmd_opt:-} -n ${n_mpmd_tasks} "${chunk_file}"
            err=$?
            if [[ ${err} -ne 0 ]]; then
                echo "ERROR: MPMD job failed for ${chunk_file}"
                break
            fi
            set_strict
        done
    else

        unset_strict
        # shellcheck disable=SC2086
        ${launcher:-} ${mpmd_opt:-} -n ${nprocs} "${mpmd_cmdfile}"
        err=$?
        set_strict

    fi

elif [[ "${launcher:-}" =~ ^mpiexec.* ]]; then # mpiexec

    # Redirect output from each process to its own stdout
    # Read the incoming cmdfile and create mpiexec usable cmdfile
    nm=0
    echo "#!/bin/bash" >> "${mpmd_cmdfile}"
    while IFS= read -r line; do
        echo "${line} > mpmd.${nm}.out 2>&1" >> "${mpmd_cmdfile}"
        ((nm = nm + 1))
    done < "${cmdfile}"
    chmod 755 "${mpmd_cmdfile}"

    # Similar to srun, split the cmdfile into chunks if needed
    if [[ ${nm} -gt ${max_tasks_per_node:-1} ]]; then
        echo "WARNING: Number of MPMD tasks (${nm}) is greater than the maximum tasks per node (${max_tasks_per_node:-1})."
        echo "         Running MPMD job in chunks of ${max_tasks_per_node:-1} tasks per node."
        chunk_size=${max_tasks_per_node:-1}
        for ((i = 0; i < nm; i += chunk_size)); do
            chunk_file="${mpmd_cmdfile}.chunk${i}"
            chunk_mpmd "${tmp_file}" "${chunk_size}" "${chunk_file}"
            unset_strict
            # shellcheck disable=SC2086
            ${launcher:-} ${mpmd_opt:-} -np ${n_mpmd_tasks} "${chunk_file}"
            err=$?
            if [[ ${err} -ne 0 ]]; then
                echo "ERROR: MPMD job failed for ${chunk_file}"
                break
            fi
            set_strict
        done
    else

        # shellcheck disable=SC2086
        ${launcher:-} -np ${nprocs} ${mpmd_opt:-} "${mpmd_cmdfile}"
        err=$?

    fi

else # Unsupported or empty launcher, run in serial mode

    echo "WARNING: CFP is not usable with launcher: '${launcher:-}', using serial mode instead"
    chmod 755 "${cmdfile}"
    bash +x "${cmdfile}" > mpmd.out 2>&1
    err=$?

fi

# On success remove the command file and any chunk files.
if [[ ${err} -eq 0 ]]; then
    rm -f "${mpmd_cmdfile}"
    rm -f "${mpmd_cmdfile}.chunk"*
fi

# Either way, concatenate the output files if they exist
out_files=$(find . -name 'mpmd.*.out')
for file in ${out_files}; do
    {
        echo "BEGIN OUTPUT FROM ${file}"
        cat "${file}"
        echo "END OUTPUT FROM ${file}"
    } >> mpmd.out
    rm -f "${file}"
done
cat mpmd.out

exit "${err}"
