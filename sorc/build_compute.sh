#!/usr/bin/env bash

function _usage() {
    cat << EOF
Builds all of the global-workflow components on compute nodes.

Usage: ${BASH_SOURCE[0]} [-h][-v] -A HPC_ACCOUNT [gfs gefs sfs gcafs gsi gdas all]
  -h:
    Print this help message and exit
  -v:
    Verbose mode
  -A:
    HPC account to use for the compute-node builds [REQUIRED]
  -c Build on compute nodes (default is NO)

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
build_on_compute="NO"

OPTIND=1
while getopts ":hA:vc" option; do
    case "${option}" in
        h) _usage ;;
        A) HPC_ACCOUNT="${OPTARG}" ;;
        c) build_on_compute="YES" ;;
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

if [[ "${HPC_ACCOUNT}" == "UNDEFINED" ]]; then
    echo "FATAL ERROR: -A <HPC_ACCOUNT> is required, ABORT!"
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

yaml="${HOMEgfs}/dev/workflow/build_opts.yaml"
echo "Generating build.xml for building global-workflow programs on compute nodes ..."
# Catch errors manually from here out
set +e

"${HOMEgfs}/dev/workflow/build_compute.py" --account "${HPC_ACCOUNT}" --yaml "${yaml}" --systems "${systems}"
rc=$?
if [[ "${rc}" -ne 0 ]]; then
    echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to create 'build.xml' with error code ${rc}"
    exit 1
fi

if [[ "${build_on_compute}" != "YES" ]]; then

    echo "Building on head node as requested ..."

    # grep for <command> tags in the build.xml and collect the commands in an array
    mapfile -t commands < <(grep -oP '(?<=<command>).*(?=</command>)' "${build_xml}")
    # get the corresponding log file names from the build.xml in an array
    mapfile -t logs < <(grep -oP '(?<=<join><cyclestr>).*(?=</cyclestr></join>)' "${build_xml}")
    # get the number of build jobs each command corresponds to in an array.  The build jobs are the strings -j N in each command.
    mapfile -t cores < <(echo "${commands[@]}" | grep -oP '(?<=-j )\d+')
    # create an array of build names from the log file names (by obtaining the basename and stripping the .log extension)
    mapfile -t names < <(printf "%s\n" "${logs[@]}" | xargs -n1 basename | sed 's/\.log$//')

    # Initialize associative arrays to track build status
    declare -A build_names build_status build_commands build_logs build_cores build_pids
    for i in "${!names[@]}"; do

        name="${names[i]}"

        build_names["${name}"]="${name}"
        build_commands["${name}"]="${commands[i]}"
        build_logs["${name}"]="${logs[i]}"
        build_cores["${name}"]="${cores[i]}"
        build_status["${name}"]="pending"
        build_pids["${name}"]=""

    done
    unset commands logs cores names

    # copy build_names into a new array to iterate over
    builds_to_process=("${!build_names[@]}")

    declare -r max_cores=40
    current_cores=0
    builds_in_progress=true
    while [[ ${builds_in_progress} == true ]]; do

        for name in "${!builds_to_process[@]}"; do

            # If the build is already completed, skip it
            if [[ ${build_status[${name}]} == "completed" ]]; then
                continue
            fi

            # Check if the build is still running
            pid="${build_pids[${name}]}"
            if [[ -z "${pid}" ]]; then # No pid means build not started yet
                cores_needed="${build_cores[${name}]}"
                if (( current_cores + cores_needed <= max_cores )); then
                    # Launch the build command in the background and redirect output to log file
                    command="${build_commands[${name}]}"
                    log_file="${build_logs[${name}]}"
                    echo "Launching build command: ${command} > ${log_file} 2>&1"
                    bash -c "${command} > ${log_file} 2>&1 &"
                    pid=$!
                    build_pids["${name}"]="${pid}"
                    build_status["${name}"]="building"
                    current_cores=$((current_cores + cores_needed))
                else
                    # Not enough cores available, skip to next build
                    continue
                fi

            else

                if ! ps -p "${pid}" > /dev/null 2>&1; then
                    # Build has finished
                    wait "${pid}"
                    rc=$?
                    if [[ "${rc}" -ne 0 ]]; then
                        echo "BUILD ERROR: Build command '${build_commands[${name}]}' failed with exit code ${rc}."
                        echo "See log file: ${build_logs[${name}]}.log"
                        build_status["${name}"]="failed"
                    else
                        echo "BUILD SUCCESS: Build command '${build_commands[${name}]}' completed successfully."
                        build_status["${name}"]="completed"
                    fi
                    # Free up the cores used by this build (regardless of success or failure)
                    current_cores=$((current_cores - build_cores[${name}]))
                fi

            fi

            # If the build failed, exit immediately
            if [[ ${build_status[${name}]} == "failed" ]]; then
                exit 1
            fi

        done

        # Check for any failed builds, and abort all if any found
        abort_all_builds=false
        for name in "${!build_names[@]}"; do
            if [[ ${build_status[${name}]} == "failed" ]]; then
                echo "Detected failed build: ${name}"
                abort_all_builds=true
            fi
        done
        if [[ ${abort_all_builds} == true ]]; then
            echo "FATAL ERROR: One or more builds failed. Aborting remaining builds."
            # Terminate all running build processes
            for pid in "${build_pids[@]}"; do
                if kill -0 "${pid}" 2> /dev/null; then # Check if process still exists
                    kill "${pid}"
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

        sleep 30s

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
