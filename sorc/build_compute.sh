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
"${HOMEgfs}/dev/workflow/build_compute.py" --account "${HPC_ACCOUNT}" --yaml "${yaml}" --systems "${systems}"
rc=$?
if [[ "${rc}" -ne 0 ]]; then
  msg="FATAL ERROR: ${BASH_SOURCE[0]} failed to create 'build.xml' with error code ${rc}"
  echo "${msg}"
  echo "${msg}" > logs/error.logs
  exit 1
fi

echo "Launching builds in parallel on compute nodes ..."
runcmd="rocotorun -w ${build_xml} -d ${build_db} ${rocoto_verbose_opt}"

finished=false
${runcmd}
echo "Running builds on compute nodes"
while [[ "${finished}" == "false" ]]; do
   sleep 1m
   ${runcmd}
   state="$("${HOMEgfs}/dev/ci/scripts/utils/rocotostat.py" -w "${build_xml}" -d "${build_db}")"
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
      rm -f logs/error.logs
      echo "${msg}" > logs/error.logs
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
            log_file="logs/${job}.log"
            echo "${log_file}" >> logs/error.logs
            echo "Rocoto reported that the build failed for ${job}"
         fi
      done < rocotostat.out
      exit 2
   fi
done

echo "All builds completed successfully!"

exit 0
