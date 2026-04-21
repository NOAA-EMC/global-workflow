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
  -d Build in debug mode (DEFAULT: NO)

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
debug_opt=""
max_cores=20 # Maximum number of cores to use for builds on head node

OPTIND=1
while getopts ":hA:vcd" option; do
    case "${option}" in
        h) _usage ;;
        A) HPC_ACCOUNT="${OPTARG}" ;;
        c) compute_build="YES" ;;
        d) debug_opt="--debug" ;;
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
HOMEglobal=$(cd "${script_dir}" && git rev-parse --show-toplevel)
# Needs to be exported for gw_setup.sh
export HOMEglobal

echo "Sourcing global-workflow modules ..."
source "${HOMEglobal}/dev/ush/gw_setup.sh"
if [[ ${MACHINE_ID} == "derecho" ]]; then
    # Derecho has stricter limits on head node usage
    cat << 'EOF'
WARNING: Interactive build on Derecho is limited to four cores to comply
         with login node policies. Consider killing (CTRL+C) and retrying
         using compute build:

         ./build_all.sh -A <HPC_ACCOUNT> -c [gfs] [gefs] [sfs] [gcafs] [gsi] [gdas] [all]

EOF
    max_cores=4
fi

# Un-export after gw_setup.sh
export -n HOMEglobal

cd "${HOMEglobal}/sorc" || exit 1
mkdir -p "${HOMEglobal}/sorc/logs" || exit 1

# Delete the rocoto XML and database if they exist
rm -f "${build_xml}" "${build_db}" "${build_lock_db}"

echo "Generating build.xml for building global-workflow programs ..."
yaml="${HOMEglobal}/sorc/build_opts.yaml"
# shellcheck disable=SC2086,SC2248
"${HOMEglobal}/dev/workflow/setup_buildxml.py" --account "${HPC_ACCOUNT}" --yaml "${yaml}" --systems "${systems}" ${debug_opt:-}
rc=$?
if [[ "${rc}" -ne 0 ]]; then
    echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to create 'build.xml' with error code ${rc}"
    exit 1
fi

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

    # If building on head node, limit the number of cores used per build
    if [[ ${compute_build} != "YES" ]]; then
        # Get the number of cores from the command (-j N).
        # If N is greater than max_cores, set it to max_cores and update the command accordingly.
        cores=$(echo "${cmd}" | grep -oP '(?<=-j )\d+')
        if [[ ${cores} -gt ${max_cores} ]]; then
            cores=${max_cores}
            cmd="$(echo "${cmd}" | sed -E "s/-j [0-9]+/-j ${cores}/")"
        fi
    fi

    build_names["${name}"]="${name}"
    build_dirs["${name}"]="$(echo "${cmd}" | awk -F';' '{ print $1 }' | sed 's/cd //')"
    build_commands["${name}"]="$(echo "${cmd}" | awk -F';' '{ $1=""; print $0 }' | sed 's/^[[:space:]]*//')"
    build_logs["${name}"]="${log}"
    build_cores["${name}"]="${cores}"
    build_status["${name}"]="PENDING"
    build_pids["${name}"]=""

done
unset commands logs

nbuilds=${#build_names[@]}
nback=$((nbuilds + 4))

print_build_status() {
    local name
    echo "-----------------------------------"
    printf "| %-18s | %-10s |\n" "System" "Status"
    echo "-----------------------------------"
    for name in "${build_names[@]}"; do
        printf "| %-18s | %-10s |\n" "${name}" "${build_status[${name}]}"
    done
    echo "-----------------------------------"
}

if [[ "${compute_build}" == "YES" ]]; then
    echo "Building on compute nodes using account: ${HPC_ACCOUNT}"
else
    echo "Building on head node using up to ${max_cores} cores ..."
fi

print_build_status

# Catch errors manually from here out
set +e

if [[ "${compute_build}" != "YES" ]]; then

    cleanup() {
        local pid name
        for name in "${!build_pids[@]}"; do
            pid=${build_pids[${name}]}
            if kill -0 "${pid}" 2> /dev/null; then # Check if process still exists
                pkill -P "${pid}"                  # Kill any child processes
            fi
        done
    }

    # Call the cleanup function when exiting (normally, on error, or by interruption)
    trap cleanup EXIT
    trap cleanup SIGINT
    trap cleanup SIGTERM
    trap cleanup SIGHUP
    trap cleanup ERR

    current_cores=0
    builds_in_progress=true

    while [[ ${builds_in_progress} == true ]]; do

        abort_all_builds=false

        for name in "${build_names[@]}"; do

            if [[ ${abort_all_builds} == true ]]; then
                continue
            fi

            # If the build is already SUCCEEDED, skip it
            if [[ ${build_status[${name}]} == "SUCCEEDED" ]]; then
                continue
            fi

            # Check if the build is still running
            pid=${build_pids[${name}]}
            if [[ -z "${pid}" ]]; then # No pid means build not started yet
                cores_needed="${build_cores[${name}]}"
                if ((current_cores + cores_needed <= max_cores)); then
                    # Launch the build command in the background and redirect output to log file
                    dir="${build_dirs[${name}]}"
                    command="${build_commands[${name}]}"
                    log_file="${build_logs[${name}]}"
                    cd "${dir}" || exit 1
                    ${command} > "${log_file}" 2>&1 &
                    _pid=$!
                    build_pids["${name}"]="${_pid}"
                    build_status["${name}"]="RUNNING"
                    # Update the current cores in use
                    current_cores=$((current_cores + cores_needed))
                else
                    # Not enough cores available, skip to next build
                    continue
                fi

            else

                if ! ps -p "${pid}" > /dev/null 2>&1; then
                    # Build has finished, check its exit status
                    wait "${pid}"
                    rc=$?
                    if [[ ${rc} -ne 0 ]]; then
                        build_status["${name}"]="FAILED"
                    else
                        build_status["${name}"]="SUCCEEDED"
                    fi
                    # Free up the cores used by this build (regardless of success or failure)
                    current_cores=$((current_cores - build_cores[${name}]))
                fi

            fi

            # If the build failed, do not submit any more builds
            if [[ ${build_status[${name}]} == "FAILED" ]]; then
                abort_all_builds=true
            fi

        done

        if [[ ${abort_all_builds} == true ]]; then
            # Terminate all running build processes
            cleanup
            # Mark all running builds as aborted and free up their cores
            for name in "${build_names[@]}"; do
                if [[ ${build_status[${name}]} == "RUNNING" ]]; then
                    build_status["${name}"]="ABORTED"
                    current_cores=$((current_cores - build_cores[${name}]))
                fi
            done
            builds_in_progress=false
        else
            # Check if any builds are still in progress or all have succeeded
            all_succeeded=true
            for name in "${build_names[@]}"; do
                if [[ ${build_status[${name}]} != "SUCCEEDED" ]]; then
                    all_succeeded=false
                fi
            done
            if [[ ${all_succeeded} == true ]]; then
                builds_in_progress=false
            else
                builds_in_progress=true
            fi
        fi

        # Move the cursor up nback lines before printing the build status again
        echo -ne "\033[${nback}A"
        print_build_status

        sleep 1m

    done

    if [[ ${abort_all_builds} == true ]]; then
        echo "FATAL ERROR: The following builds failed, see log files for details:"
        for name in "${build_names[@]}"; do
            if [[ ${build_status[${name}]} == "FAILED" ]]; then
                echo -e "\t${name}: ${build_logs[${name}]}"
            fi
        done
        exit 1
    fi

else

    runcmd="rocotorun -w ${build_xml} -d ${build_db} ${rocoto_verbose_opt}"
    ${runcmd}
    rc=$?
    if [[ "${rc}" -ne 0 ]]; then
        echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to run rocoto on the first attempt!"
        exit 1
    fi

    builds_in_progress=true
    consecutive_unknown=0
    max_unknown=2
    while [[ ${builds_in_progress} == true ]]; do

        sleep 1m

        ${runcmd}

        sleep 15s

        stat_out="$(rocotostat -w "${build_xml}" -d "${build_db}")"
        echo "${stat_out}" > rocotostat.out
        # Ignore 1st 2 lines and store each row of rocotostat output in an array
        mapfile -t stat_lines < <(tail -n +3 rocotostat.out)
        rm -f rocotostat.out

        # Loop through each line of the rocotostat output and update build_pids and build_status arrays
        for line in "${stat_lines[@]}"; do
            # Read each line into an array using read
            IFS=' ' read -r -a columns <<< "${line}"

            # Get the name, jobid and jobstatus of the build in this row
            name=${columns[1]}
            jobid=${columns[2]}
            jobstatus=${columns[3]}

            # Update build_pids and build_status arrays for the build_name
            build_pids["${name}"]="${jobid}"
            build_status["${name}"]="${jobstatus}"
        done

        echo -ne "\033[${nback}A"
        print_build_status

        # Count number of builds still in progress and check for failures
        nsuccess=0
        nfailed=0
        nunknown=0
        for name in "${build_names[@]}"; do
            job_state="${build_status[${name}]}"
            if [[ "${job_state}" =~ "DEAD" || "${job_state}" =~ "FAIL" ]]; then
                nfailed=$((nfailed + 1))
            elif [[ "${job_state}" =~ "UNKNOWN" || "${job_state}" =~ "UNAVAILABLE" ]]; then
                nunknown=$((nunknown + 1))
            elif [[ "${job_state}" == "SUCCEEDED" ]]; then
                nsuccess=$((nsuccess + 1))
            fi
        done

        # Some schedulers are volatile, so don't fail until there are a few consecutive
        # queries that return unknown status.
        if [[ ${nunknown} -gt 0 ]]; then
            consecutive_unknown=$((consecutive_unknown + 1))
            if [[ ${consecutive_unknown} -gt ${max_unknown} ]]; then
                nfailed=$((nfailed + nunknown))
            fi
        else
            consecutive_unknown=0
        fi

        # If any builds failed, exit with error
        if [[ ${nfailed} -gt 0 ]]; then
            echo "FATAL ERROR: The following builds failed, see log files for details:"
            for name in "${build_names[@]}"; do
                job_state="${build_status[${name}]}"
                if [[ "${job_state}" =~ "DEAD" || "${job_state}" =~ "UNKNOWN" ||
                    "${job_state}" =~ "UNAVAILABLE" || "${job_state}" =~ "FAIL" ]]; then
                    echo -e "\t${name}: ${build_logs[${name}]}"
                fi
            done
            exit 1
        fi

        # If all builds succeeded, exit the loop
        if [[ ${nsuccess} -eq ${nbuilds} ]]; then
            builds_in_progress=false
        fi

    done

fi

echo "All builds completed successfully!"

exit 0
