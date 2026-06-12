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
if [[ "${_mpmd_launcher}" == "unsupported" ]]; then
    USE_CFP="NO"
else
    echo "INFO: Detected launcher '${_mpmd_launcher}', will attempt to run in MPMD mode if USE_CFP is set to YES"
    if [[ -z "${max_tasks_per_node:-}" || -z "${ntasks:-}" ]]; then
        echo "WARNING: max_tasks_per_node and/or ntasks is not set, disabling MPMD mode."
        USE_CFP=NO
    else
        USE_CFP=${USE_CFP:-"NO"}
        max_tasks_per_node=$((ntasks < max_tasks_per_node ? ntasks : max_tasks_per_node))
    fi
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

# the Derecho mpiexec implementations does not respect stdout redirection,
# so make a wrapper script.
if [[ "${machine}" == "DERECHO" ]]; then
    wrapper_script="stdout_wrapper.sh"
    rm -f "${wrapper_script}"
    cat << 'EOF' > "${wrapper_script}"
#! /usr/bin/bash
# Run the rest of the args and redirect stdout and stderr to the
# file named by the last argument
${@: 1:$#-1} > ${@: -1} 2>&1
exit $?
EOF
    chmod 755 "${wrapper_script}"
fi

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1

# Establish the MPMD chunk file pattern.
mpmd_cmdfile="${DATA:-}/mpmd_cmdfile"
rm -f "${mpmd_cmdfile}"*

# Get the starting timestamp for the log/command output directory.
timestamp=$(date +%Y%m%d_%H%M%S)

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
                # The MPMD implemtation is different between WCOSS and Derecho, but both
                # use mpiexec
                if [[ "${machine}" == "DERECHO" ]]; then
                    echo "-n 1 ${wrapper_script} ${line} mpmd.${i}.out" >> "${chunk_file}"
                else
                    echo "${line} > mpmd.${i}.out 2>&1" >> "${chunk_file}"
                fi
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

move_outputs() {
    # This function makes an after-run directory (mpmd_<timestamp>) and moves the run scripts
    # and outputs to this directory.
    # Usage: move_outputs chunk_num

    if [[ $# -ne 1 ]]; then
        echo "ERROR: move_outputs function requires 1 argument: the chunk number."
        return 1
    fi

    local chunk_num="${1}"

    # Only find the output files for this chunk, which should be named mpmd.*.out
    out_files=$(find "${DATA:-}" -maxdepth 1 -type f -name "mpmd.*.out" -print)
    if [[ -z "${out_files}" ]]; then
        # Nothing to do, raise a warning and exit successfully
        echo "WARNING: No output files found from MPMD jobs."
        return 0
    fi

    echo "INFO: Moving MPMD output files for chunk ${chunk_num} to after-run directory."

    after_run_dir="mpmd_${timestamp}_chunk${chunk_num}"
    mkdir -p "${after_run_dir}"

    # shellcheck disable=SC2086
    mv -f ${out_files} "${after_run_dir}/"
    mv -f "${mpmd_cmdfile}.chunk${chunk_num}" "${after_run_dir}/"

    # Always copy the cmdfile to the after_run_dir for reference.
    cp "${cmdfile}" "${after_run_dir}/"
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
    # Calculate the number of chunks needed (ceil (nm / chunk_size))
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
        source "${USHglobal}/unset_strict.sh"
        # shellcheck disable=SC2086
        ${launcher:-} ${mpmd_opt:-} -n "${n_mpmd_tasks}" "${chunk_file}"
        source "${USHglobal}/set_strict.sh"
    elif [[ "${_mpmd_launcher}" == "mpiexec" ]]; then
        # The MPMD implemtation is different between WCOSS and Derecho, but both
        # use mpiexec
        if [[ "${machine}" == "DERECHO" ]]; then
            # shellcheck disable=SC2086
            ${launcher:-} ${mpmd_opt:-} "${chunk_file}"
        else
            # shellcheck disable=SC2086
            ${launcher:-} -np "${n_mpmd_tasks}" ${mpmd_opt:-} "${chunk_file}"
        fi
    fi
    err=$?
    if [[ ${err} -ne 0 ]]; then
        echo "ERROR: MPMD job failed for ${chunk_file}"
        break
    fi
    # Move just the log files for this chunk.
    move_outputs "${chunk_num}"
    ((chunk_num = chunk_num + 1))
done

exit "${err}"
