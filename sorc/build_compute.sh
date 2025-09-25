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

OPTIND=1
while getopts ":hA:v" option; do
  case "${option}" in
    h) _usage;;
    A) HPC_ACCOUNT="${OPTARG}" ;;
    v) verbose="YES" && rocoto_verbose_opt="-v10";;
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
shift $((OPTIND-1))

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

script_dir="$(cd "$(dirname  "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"
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

# Temporarily build the GDASApp on the head node
# Cleanup function to kill the GDASApp build on ctrl-c or non-clean exit
build_ids=()
function cleanup() {
  echo "Exiting build script. Terminating subprocesses..."
  for pid in "${build_ids[@]}"; do
    if kill -0 "${pid}" 2>/dev/null; then # Check if process still exists
       kill "${pid}"
    fi
  done
  exit 1
}

trap cleanup TERM
trap cleanup INT
trap cleanup ERR

# TODO remove this when all builds move to the head nodes and/or the GDASApp is able to build on all compute nodes again
#      See GW issue 3933
if [[ ${systems} == "all" || ${systems} =~ "gdas" ]]; then
  echo "Building the GDASApp locally (on this node)"
  gdas_build_log="${HOMEgfs}/sorc/logs/build_gdas.log"
  "${HOMEgfs}/sorc/build_gdas.sh" -j 12 >& "${gdas_build_log}" &
  build_gdas_id=$!
  build_ids+=("${build_gdas_id}")
fi

"${HOMEgfs}/dev/workflow/build_compute.py" --account "${HPC_ACCOUNT}" --yaml "${yaml}" --systems "${systems}"
rc=$?
if [[ "${rc}" -ne 0 ]]; then
  echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to create 'build.xml' with error code ${rc}"
  exit 1
fi

echo "Launching builds in parallel on compute nodes ..."
runcmd="rocotorun -w ${build_xml} -d ${build_db} ${rocoto_verbose_opt}"

finished=false
${runcmd}
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
         (( line_number += 1 ))
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

      # Kill the GDASApp build if it is still running
      cleanup
   fi
done

# Wait for the GDASApp to finish building
if [[ -n "${build_gdas_id+0}" ]]; then
  echo "Compute builds have completed successfully, but the GDASApp is still building locally.  Waiting for it to complete."
  wait "${build_gdas_id}"
  gdas_stat=$?
  if [[ ${gdas_stat} -ne 0 ]]; then
    echo "FATAL ERROR The GDASApp failed to build!  Check log in ${gdas_build_log}"
    # Capture the error log in logs/error.logs
    echo "${gdas_build_log}" >> "${err_file}"
    exit 3
  fi
fi
echo "All builds completed successfully!"

exit 0
