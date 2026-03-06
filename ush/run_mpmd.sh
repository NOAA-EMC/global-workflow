#!/usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         run_mpmd.sh
# Script description:  Run multiple commands in MPMD mode or serially
#
# Author:   Rahul Mahajan and David Huber
#
# Org:      NCEP/EMC
#
# Abstract: This script runs multiple commands in MPMD mode. It is used to run
#           multiple serial commands in parallel using the CFP (Coupled Framework
#           Parallelism) feature of the workflow. The script handles chunking of the
#           commands to avoid oversubscription of resources.
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

cmdfile=${1:?"run_mpmd requires an input file containing commands to execute in MPMD/serial mode"}

# Determine launcher type
if [[ "${launcher:-}" =~ ^srun.* ]]; then #  srun-based system e.g. Hera, Orion, etc.
    _mpmd_launcher=srun
elif [[ "${launcher:-}" =~ ^mpiexec.* ]]; then # mpiexec-based system e.g. WCOSS2
    _mpmd_launcher=mpiexec
else
    echo "WARNING: Unsupported or empty launcher: '${launcher:-}', using serial mode instead"
    echo "         Supported launchers are 'srun' and 'mpiexec'"
    _mpmd_launcher=unsupported
fi

# Check if we are running a supported launcher
if [[ "${_mpmd_launcher}" == "srun" || "${_mpmd_launcher}" == "mpiexec" ]]; then
    echo "INFO: Detected launcher '${_mpmd_launcher}', will attempt to run in MPMD mode if USE_CFP is set to YES"
    if [[ -z "${max_tasks_per_node:-}" || -z "${ntasks:-}" ]]; then
        echo "WARNING: max_tasks_per_node and/or ntasks is not set, disabling MPMD mode."
        USE_CFP=NO
    else
        USE_CFP=${USE_CFP:-"NO"}
        max_tasks_per_node=$((ntasks < max_tasks_per_node ? ntasks : max_tasks_per_node))
    fi
else
    USE_CFP="NO"
fi

# If USE_CFP is not set or is not YES, run in serial mode
if [[ "${USE_CFP}" != "YES" ]]; then
    echo "INFO: Using serial mode for MPMD job"
    chmod 755 "${cmdfile}"
    bash +x "${cmdfile}" > mpmd.out 2>&1 && true
    rc=$?
    cat mpmd.out
    exit "${rc}"
fi

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1

# Establish the MPMD chunk file pattern.
mpmd_cmdfile="${DATA:-}/mpmd_cmdfile"
rm -f "${mpmd_cmdfile}"*

# Functions to support MPMD execution
chunk_mpmd() {
    # Usage chunk_mpmd cmdfile chunk_size chunk_num chunk_file
    # This takes a chunk of the full mpmd command file and creates a new chunk
    # file with the specified number of lines
    # Inputs:
    #   cmdfile: the full mpmd command file to read from and modify
    #   chunk_size: the number of lines to include in the chunk file
    #   chunk_num: the chunk number (used to determine which lines from the cmdfile to include in the chunk file)
    #   chunk_file: the name of the chunk file to create
    # Use this function when the number of MPMD tasks is greater than the maximum tasks per node.
    local mpmd_file="${1}"
    local chunk_sz="${2}"
    local chunk_num="${3}"
    local chunk_file="${4}"
    if [[ ! -s "${mpmd_file}" ]]; then
        echo "ERROR: MPMD command file '${mpmd_file}' is empty or does not exist."
        return 1
    fi

    if [[ -f "${chunk_file}" ]]; then
        echo "ERROR: chunk file '${chunk_file}' already exists!"
        return 1
    fi

    # Determine which line to start reading from
    local _start_line=$(((chunk_num - 1) * chunk_sz + 1))
    local _end_line=$((chunk_num * chunk_sz))

    # mpiexec needs to know the interpreter
    if [[ "${_mpmd_launcher}" == "mpiexec" ]]; then
        echo "#!/usr/bin/bash" > "${chunk_file}"
    fi

    local _counter=1
    while IFS= read -r line; do
        if [[ ${_counter} -ge ${_start_line} && ${_counter} -le ${_end_line} ]]; then
            local i=$((_counter - _start_line))
            # Slurm requires a counter in front of each line in the script
            if [[ "${_mpmd_launcher}" == "srun" ]]; then
                echo "${i} ${line}" >> "${chunk_file}"
            elif [[ "${_mpmd_launcher}" == "mpiexec" ]]; then
                echo "${line} > mpmd.${i}.out 2>&1" >> "${chunk_file}"
            fi
            err=$?
            if [[ ${err} -ne 0 ]]; then
                echo "ERROR: Failed to write line '${line}' to chunk file '${chunk_file}'"
                return "${err}"
            fi
        fi
        ((_counter = _counter + 1))
    done < "${mpmd_file}"

    return 0
}

cat_outputs() {
    # This function concatenates the output files from the MPMD job and prints them to stdout.
    # It also removes the individual output files after concatenation.

    # Optional argument to issue error if no output files are found.
    _err_on_empty="${1:-false}"
    out_files=$(find . -name 'mpmd.*.out')
    if [[ -z "${out_files}" ]]; then
        if [[ "${_err_on_empty}" == "true" ]]; then
            echo "ERROR: No output files found for MPMD job"
            return 1
        else
            # Nothing to do, return success.
            return 0
        fi
    fi
    for file in ${out_files}; do
        {
            echo "BEGIN OUTPUT FROM ${file}"
            cat "${file}"
            echo "END OUTPUT FROM ${file}"
        } >> mpmd.out
        rm -f "${file}"
    done
}

cat << EOF
INFO: Executing MPMD job, STDOUT and STDERR redirected for each process separately
INFO: On failure, logs for each job will be available in ${DATA}/mpmd.proc_num.out
INFO: The proc_num corresponds to the line in '${cmdfile}'
EOF

# Determine the number of MPMD processes from incoming ${cmdfile}
nm=$(wc -l < "${cmdfile}")

# Test if the number of lines in the cmdfile is greater than the number of tasks per node ($max_tasks_per_node).

if [[ ${nm} -gt ${max_tasks_per_node:-1} ]]; then
    # If needed, split the cmdfile and run it in chunks.
    # For now, keep all MPMD tasks on one node.
    # TODO: consider running the MPMD job across multiple nodes.
    echo "INFO: Number of MPMD tasks (${nm}) is greater than the maximum tasks per node (${max_tasks_per_node:-1})."
    echo "      Running MPMD job in chunks of ${max_tasks_per_node:-1} tasks per node."
    chunk_size=${max_tasks_per_node:-1}
else
    # Otherwise, we can run all MPMD tasks in one chunk.
    chunk_size=${nm}
fi

# Start chunking through the MPMD command file.
chunk_num=1
err=0
for ((i = 0; i < nm; i += chunk_size)); do
    chunk_file="${mpmd_cmdfile}.chunk${chunk_num}"
    chunk_mpmd "${cmdfile}" "${chunk_size}" "${chunk_num}" "${chunk_file}"
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: Failed to create chunk file '${chunk_file}' from '${cmdfile}'"
        break
    fi
    chmod 755 "${chunk_file}"
    # Count the number of lines not including commented lines (i.e. shebangs)
    n_mpmd_tasks=$(grep -v -c "^ *#" < "${chunk_file}")
    if [[ "${_mpmd_launcher}" == "srun" ]]; then
        unset_strict
        # shellcheck disable=SC2086
        ${launcher:-} ${mpmd_opt:-} -n "${n_mpmd_tasks}" "${chunk_file}"
        set_strict
    elif [[ "${_mpmd_launcher}" == "mpiexec" ]]; then
        # shellcheck disable=SC2086
        ${launcher:-} -np "${n_mpmd_tasks}" ${mpmd_opt:-} "${chunk_file}"
    fi
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: MPMD job failed for ${chunk_file}"
        break
    fi
    # Call cat_outputs and error if no outputs are found.
    cat_outputs "true"
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: No output files found for MPMD job for chunk file '${chunk_file}'"
        break
    fi
    ((chunk_num = chunk_num + 1))
done

# On success remove the command file and any chunk files.
if [[ ${err} -eq 0 ]]; then
    rm -f "${mpmd_cmdfile}.chunk"*
fi

# Concatenate any remaining output files if they exist
cat_outputs
if [[ -s mpmd.out ]]; then
    cat mpmd.out
else
    echo "WARNING: No output files found for MPMD job"
fi

exit "${err}"
