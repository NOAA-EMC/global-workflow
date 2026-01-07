#!/usr/bin/env bash

function _usage() {
    cat << EOF
Builds all of the global-workflow components on compute nodes.

Usage: ${BASH_SOURCE[0]} [-h][-v] -A HPC_ACCOUNT -c [gfs gefs sfs gcafs gsi gdas all]
  -h:
    Print this help message and exit
  -v:
    Verbose mode
  -A:
    HPC account to use for the compute-node builds [REQUIRED when building on compute nodes]
  -c Build on compute nodes (DEFAULT: NO)

  Input arguments are the system(s) to build.
  Valid options are
    "gfs", "gefs", "sfs", "gcafs", "gsi", "gdas", or "all".
    (default is "gfs")
EOF
    exit 1
}
# This script launches compute-node builds of selected submodules
# Two positional arguments are accepted:

set -eu

rocoto_verbose_opt=""
verbose="NO"
build_xml="build.xml"
build_db="build.db"
build_lock_db="build_lock.db"
HPC_ACCOUNT="UNDEFINED"
compute_build="NO"

OPTIND=1
while getopts ":hA:vc" option; do
    case "${option}" in
        h) _usage ;;
        A) HPC_ACCOUNT="${OPTARG}" ;;
        c) compute_build="YES" ;;
        v) verbose="YES" && rocoto_verbose_opt="-v10" ;;
        :)
            echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
            _usage
            ;;
        *)
            echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
            _usage
            ;;
    esac
done
shift $((OPTIND - 1))

# Set build system to gfs if not specified
if [[ $# -eq 0 ]]; then
    systems="gfs"
else
    systems=$*
fi

if [[ "${compute_build}" == "YES" && "${HPC_ACCOUNT}" == "UNDEFINED" ]]; then
    echo "FATAL ERROR: -A <HPC_ACCOUNT> is required when building on compute nodes, ABORT!"
    _usage
fi

if [[ "${verbose}" == "YES" ]]; then
    set -x
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
HOMEgfs=$(cd "${script_dir}" && git rev-parse --show-toplevel)
# Needs to be exported for gw_setup.sh
export HOMEgfs

echo "Sourcing global-workflow modules ..."
source "${HOMEgfs}/dev/ush/gw_setup.sh"

# Un-export after gw_setup.sh
export -n HOMEgfs

cd "${HOMEgfs}/sorc" || exit 1
mkdir -p "${HOMEgfs}/sorc/logs" || exit 1

# Delete the rocoto XML and database if they exist
rm -f "${build_xml}" "${build_db}" "${build_lock_db}"

echo "Generating build.xml for building global-workflow programs ..."
yaml="${HOMEgfs}/sorc/build_opts.yaml"
"${HOMEgfs}/dev/workflow/setup_buildxml.py" --account "${HPC_ACCOUNT}" --yaml "${yaml}" --systems "${systems}"
rc=$?
if [[ "${rc}" -ne 0 ]]; then
    echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to create 'build.xml' with error code ${rc}"
    exit 1
fi

# Catch errors manually from here out
set +e

if [[ "${compute_build}" != "YES" ]]; then

    echo "Building on head node as requested ..."

    # Maximum number of cores to use for builds on head node
    declare -r max_cores=20

    # grep for <command> tags in the build.xml and collect the commands in an array
    mapfile -t commands < <(grep -oP '(?<=<command>).*(?=</command>)' "${build_xml}")
    # get the corresponding log file names from the build.xml in an array
    mapfile -t logs < <(grep -oP '(?<=<join><cyclestr>).*(?=</cyclestr></join>)' "${build_xml}")

    # Initialize associative arrays to track build status
    declare -A build_names build_status build_dirs build_commands build_logs build_cores build_pids
    for i in "${!logs[@]}"; do

        cmd="${commands[i]}"
        log="${logs[i]}"
        name=$(echo "${log}" | xargs -n1 basename | sed 's/\.log$//')

        # Get the number of cores from the command (-j N).
        # If N is greater than max_cores, set it to max_cores and update the command accordingly.
        cores=$(echo "${cmd}" | grep -oP '(?<=-j )\d+')
        if [[ ${cores} -gt ${max_cores} ]]; then
            cores=${max_cores}
            cmd="$(echo "${cmd}" | sed -E "s/-j [0-9]+/-j ${cores}/")"
        fi

        build_names["${name}"]="${name}"
        build_dirs["${name}"]="$(echo "${cmd}" | awk -F';' '{ print $1 }' | sed 's/cd //')"
        build_commands["${name}"]="$(echo "${cmd}" | awk -F';' '{ $1=""; print $0 }' | sed 's/^[[:space:]]*//')"
        build_logs["${name}"]="${log}"
        build_cores["${name}"]="${cores}"
        build_status["${name}"]="pending"
        build_pids["${name}"]=""

    done
    unset commands logs

    # copy build_names into a new array to iterate over
    builds_to_process=("${!build_names[@]}")

    current_cores=0
    builds_in_progress=true
    while [[ ${builds_in_progress} == true ]]; do

        for name in "${builds_to_process[@]}"; do

            # If the build is already completed, skip it
            if [[ ${build_status[${name}]} == "completed" ]]; then
                continue
            fi

            # Check if the build is still running
            pid="${build_pids[${name}]}"
            if [[ -z "${pid}" ]]; then # No pid means build not started yet
                cores_needed="${build_cores[${name}]}"
                if ((current_cores + cores_needed <= max_cores)); then
                    # Launch the build command in the background and redirect output to log file
                    dir="${build_dirs[${name}]}"
                    command="${build_commands[${name}]}"
                    log_file="${build_logs[${name}]}"
                    cd "${dir}" || exit 1
                    ${command} > "${log_file}" 2>&1 &
                    pid=$!
                    echo "Build for ${name} started with PID ${pid}, using ${cores_needed} cores."
                    build_pids["${name}"]="${pid}"
                    build_status["${name}"]="building"
                    # Update the current cores in use
                    current_cores=$((current_cores + cores_needed))
                else
                    # Not enough cores available, skip to next build
                    continue
                fi

            else

                #echo "Checking status of build for ${name} with PID ${pid} ..."
                if ! ps -p "${pid}" > /dev/null 2>&1; then
                    # Build has finished
                    wait "${pid}"
                    rc=$?
                    if [[ "${rc}" -ne 0 ]]; then
                        echo "BUILD ERROR: Build for ${name} failed with exit code ${rc}."
                        echo "See log file: ${build_logs[${name}]}"
                        build_status["${name}"]="failed"
                    else
                        echo "BUILD SUCCESS: Build for ${name} completed successfully."
                        build_status["${name}"]="completed"
                    fi
                    # Free up the cores used by this build (regardless of success or failure)
                    current_cores=$((current_cores - build_cores[${name}]))
                fi

            fi

            # If the build failed, do not submit any more builds
            if [[ ${build_status[${name}]} == "failed" ]]; then
                break
            fi

        done

        # Check for any failed builds, and abort all if any found
        abort_all_builds=false
        for name in "${build_names[@]}"; do
            if [[ ${build_status[${name}]} == "failed" ]]; then
                echo "Detected failed build: ${name}"
                abort_all_builds=true
            fi
        done
        if [[ ${abort_all_builds} == true ]]; then
            echo "FATAL ERROR: One or more builds failed. Aborting all builds."
            # Terminate all running build processes
            for i in "${!build_pids[@]}"; do
                pid="${build_pids[${i}]}"
                name="${build_names[${i}]}"
                if kill -0 "${pid}" 2> /dev/null; then # Check if process still exists
                    echo "Terminating build for ${name} with PID ${pid} ..."
                    pkill -P "${pid}" # Kill any child processes
                fi
            done
            exit 1
        fi

        # Remove completed builds from the list to process during the next iteration
        builds_to_process=()
        builds_in_progress=false
        for name in "${!build_names[@]}"; do
            if [[ ${build_status[${name}]} != "completed" ]]; then
                builds_to_process+=("${name}")
                builds_in_progress=true
            fi
        done

        echo "Waiting for builds to complete. Current cores in use: ${current_cores}/${max_cores}"
        sleep 1m

    done

else

    echo "Building on compute nodes as requested ..."
    runcmd="rocotorun -w ${build_xml} -d ${build_db} ${rocoto_verbose_opt}"

    finished=false
    ${runcmd}
    rc=$?
    if [[ "${rc}" -ne 0 ]]; then
        echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to run rocoto on the first attempt!"
        exit 1
    fi

    echo "Monitoring builds on compute nodes"
    while [[ "${finished}" == "false" ]]; do
        sleep 1m
        ${runcmd}

        state="$("${HOMEgfs}/dev/ci/scripts/utils/rocotostat.py" -w "${build_xml}" -d "${build_db}")" || true
        if [[ "${verbose_opt}" == "true" ]]; then
            echo "Rocoto is in state ${state}"
        else
            echo -n "."
        fi

        if [[ "${state}" == "DONE" ]]; then
            finished=true
        elif [[ "${state}" == "RUNNING" ]]; then
            finished=false
        else
            msg="FATAL ERROR: ${BASH_SOURCE[0]} rocoto failed with state '${state}'"
            echo "${msg}"
            err_file="${PWD}/logs/error.logs"
            rm -f "${err_file}"
            # Determine which build(s) failed
            stat_out="$(rocotostat -w "${build_xml}" -d "${build_db}")"
            echo "${stat_out}" > rocotostat.out
            line_number=0
            while read -r line; do
                ((line_number += 1))
                # Skip the first two lines (header)
                if [[ ${line_number} -lt 3 ]]; then
                    continue
                fi

                if [[ "${line}" =~ "DEAD" || "${line}" =~ "UNKNOWN" ||
                    "${line}" =~ "UNAVAILABLE" || "${line}" =~ "FAIL" ]]; then
                    job=$(echo "${line}" | awk '{ print $2 }')
                    log_file="${PWD}/logs/${job}.log"
                    echo "${log_file}" >> "${err_file}"
                    echo "Rocoto reported that the build failed for ${job}"
                fi
            done < rocotostat.out
            exit 1
        fi
    done

fi

echo "All builds completed successfully!"

exit 0
