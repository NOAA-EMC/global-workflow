#!/usr/bin/env bash

function _usage() {
  cat << EOF
Builds all of the global-workflow components on compute nodes.

Usage: ${BASH_SOURCE[0]} [-h][-v][-A <hpc-account>] [ gfs gefs sfs gsi gdas all]
  -h:
    Print this help message and exit
  -v:
    Verbose mode
  -A:
    HPC account to use for the compute-node builds
    (default is \$HOMEgfs/ci/platforms/config.\$machine:\$HPC_ACCOUNT)

  Input arguments are the system(s) to build.
  Valid options are
    "gfs", "gefs", "sfs", "gsi", "gdas", or "all".
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

OPTIND=1
while getopts ":hA:v" option; do
  case "${option}" in
    h) _usage;;
    A) export HPC_ACCOUNT="${OPTARG}" ;;
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

if [[ "${verbose}" == "YES" ]]; then
   set -x
fi

# shellcheck disable=SC2155,SC2312
HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
cd "${HOMEgfs}/sorc" || exit 1

# Delete the rocoto XML and database if they exist
rm -f "${build_xml}" "${build_db}" "${build_lock_db}"

echo "Sourcing global-workflow modules ..."
source "${HOMEgfs}/workflow/gw_setup.sh"

echo "Generating build.xml for building global-workflow programs on compute nodes ..."
# Catch errors manually from here out
set +e
"${HOMEgfs}/workflow/build_compute.py" --yaml "${HOMEgfs}/workflow/build_opts.yaml" --systems "${systems}"
rc=$?
if (( rc != 0 )); then
  echo "FATAL ERROR: ${BASH_SOURCE[0]} failed to create 'build.xml' with error code ${rc}"
  exit 1
fi

echo "Launching builds in parallel on compute nodes ..."
runcmd="rocotorun -w ${build_xml} -d ${build_db} ${rocoto_verbose_opt}"

finished=false
${runcmd}
echo "Running builds on compute nodes"
while [[ "${finished}" == "false" ]]; do
   sleep 3m
   ${runcmd}
   state="$("${HOMEgfs}/ci/scripts/utils/rocotostat.py" -w "${build_xml}" -d "${build_db}")"
   if [[ "${verbose_opt}" == "true" ]]; then
      echo "Rocoto is in state ${state}"
   else
      echo -n "."
   fi

   if [[ "${state}" == "DONE" ]]; then
      finished=true
   elif [[ "${state}" == "RUNNING" ]]; then
      finished=false
   elif [[ "${state}" == "DEAD" ]]; then
      echo "FATAL ERROR: ${BASH_SOURCE[0]} one or more builds failed!"
      # TODO add capability to determine which build(s) failed
      exit 2
   else
      echo "FATAL ERROR: ${BASH_SOURCE[0]} rocoto failed with state '${state}'"
      exit 3
   fi
done

echo "All builds completed successfully!"

exit 0
