#! /usr/bin/env bash

set +x
#------------------------------------
# Exception handling is now included.
#
# USER DEFINED STUFF:
#
#------------------------------------

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------
function _usage() {
  cat << EOF
Builds all of the global-workflow components by calling the individual build
  scripts in sequence.

Usage: ${BASH_SOURCE[0]} [-a UFS_app][-c build_config][-h][-j n][-v][-w]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -g:
    Build GSI
  -h:
    Print this help message and exit
  -j:
    Specify maximum number of build jobs (n)
  -u:
    Build UFS-DA
  -v:
    Execute all build scripts with -v option to turn on verbose where supported
  -w: 
    Use unstructured wave grid 
EOF
  exit 1
}

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd "${script_dir}" || exit 1

_build_ufs_opt=""
_build_ufsda="NO"
_build_gsi="NO"
_verbose_opt=""
_wave_unst=""
_build_job_max=20
# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":a:ghj:uvw" option; do
  case "${option}" in
    a) _build_ufs_opt+="-a ${OPTARG} ";;
    g) _build_gsi="YES" ;;
    h) _usage;;
    j) _build_job_max="${OPTARG} ";;
    u) _build_ufsda="YES" ;;
    v) _verbose_opt="-v";;
    w) _wave_unst="-w";;
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

logs_dir="${script_dir}/logs"
if [[ ! -d "${logs_dir}" ]]; then
  echo "Creating logs folder"
  mkdir "${logs_dir}" || exit 1
fi

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# GET MACHINE
#------------------------------------
export COMPILER="intel"
source gfs_utils.fd/ush/detect_machine.sh
source gfs_utils.fd/ush/module-setup.sh
if [[ -z "${MACHINE_ID}" ]]; then
  echo "FATAL: Unable to determine target machine"
  exit 1
fi

#------------------------------------
# SOURCE BUILD VERSION FILES
#------------------------------------
# TODO: Commented out until components aligned for build
#source ../versions/build.ver

#------------------------------------
# Exception Handling Init
#------------------------------------
# Disable shellcheck warning about single quotes not being substituted.
# shellcheck disable=SC2016
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $errs = 0 ]]'}
# shellcheck disable=
errs=0

declare -A build_jobs
declare -A build_opts

#------------------------------------
# Check which builds to do and assign # of build jobs
#------------------------------------

# Mandatory builds, unless otherwise specified, for the UFS
big_jobs=0
build_jobs["ufs"]=8
big_jobs=$((big_jobs+1))
build_opts["ufs"]="${_wave_unst} ${_verbose_opt} ${_build_ufs_opt}"

build_jobs["upp"]=2
build_opts["upp"]=""

build_jobs["ufs_utils"]=2
build_opts["ufs_utils"]="${_verbose_opt}"

build_jobs["gfs_utils"]=1
build_opts["gfs_utils"]="${_verbose_opt}"

build_jobs["ww3prepost"]=2
build_opts["ww3prepost"]="${_wave_unst} ${_verbose_opt} ${_build_ufs_opt}"

# Optional DA builds
if [[ "${_build_ufsda}" == "YES" ]]; then
   if [[ "${MACHINE_ID}" != "orion" && "${MACHINE_ID}" != "hera" && "${MACHINE_ID}" != "hercules" ]]; then
      echo "NOTE: The GDAS App is not supported on ${MACHINE_ID}.  Disabling build."
   else
      build_jobs["gdas"]=8
      big_jobs=$((big_jobs+1))
      build_opts["gdas"]="${_verbose_opt}"
   fi
fi
if [[ "${_build_gsi}" == "YES" ]]; then
   build_jobs["gsi_enkf"]=8
   build_opts["gsi_enkf"]="${_verbose_opt}"
fi
if [[ "${_build_gsi}" == "YES" || "${_build_ufsda}" == "YES" ]] ; then
   build_jobs["gsi_utils"]=1
   build_opts["gsi_utils"]="${_verbose_opt}"
   if [[ "${MACHINE_ID}" == "hercules" ]]; then
      echo "NOTE: The GSI Monitor is not supported on Hercules.  Disabling build."
   else
      build_jobs["gsi_monitor"]=1
      build_opts["gsi_monitor"]="${_verbose_opt}"
   fi
fi

# Go through all builds and adjust CPU counts down if necessary
requested_cpus=0
build_list=""
for build in "${!build_jobs[@]}"; do
   if [[ -z "${build_list}" ]]; then
      build_list="${build}"
   else
      build_list="${build_list}, ${build}"
   fi
   if [[ ${build_jobs[${build}]} -gt ${_build_job_max} ]]; then
      build_jobs[${build}]=${_build_job_max}
   fi
   requested_cpus=$(( requested_cpus + build_jobs[${build}] ))
done

echo "Building ${build_list}"

# Go through all builds and adjust CPU counts up if possible
if [[ ${requested_cpus} -lt ${_build_job_max} && ${big_jobs} -gt 0 ]]; then
   # Add cores to the gdas, ufs, and gsi build jobs
   extra_cores=$(( _build_job_max - requested_cpus ))
   extra_cores=$(( extra_cores / big_jobs ))
   for build in "${!build_jobs[@]}"; do
      if [[ "${build}" == "gdas" || "${build}" == "ufs" ]]; then
         build_jobs[${build}]=$(( build_jobs[${build}] + extra_cores ))
      fi
   done
fi

procs_in_use=0
declare -A build_ids

builds_started=0
# Now start looping through all of the jobs until everything is done
while [[ ${builds_started} -lt ${#build_jobs[@]} ]]; do
   for build in "${!build_jobs[@]}"; do
      # Has the job started?
      if [[ -n "${build_jobs[${build}]+0}" && -z "${build_ids[${build}]+0}" ]]; then
         # Do we have enough processors to run it?
         if [[ ${_build_job_max} -ge $(( build_jobs[build] + procs_in_use )) ]]; then
            if [[ "${build}" != "upp" ]]; then
               "./build_${build}.sh" -j "${build_jobs[${build}]}" "${build_opts[${build}]:-}" > \
                  "${logs_dir}/build_${build}.log" 2>&1 &
            else
               "./build_${build}.sh" "${build_opts[${build}]}" > \
                  "${logs_dir}/build_${build}.log" 2>&1 &
            fi
            build_ids["${build}"]=$!
            echo "Starting build_${build}.sh"
            procs_in_use=$(( procs_in_use + build_jobs[${build}] ))
         fi
      fi
   done

   # Check if all builds have completed
   # Also recalculate how many processors are in use to account for completed builds
   builds_started=0
   procs_in_use=0
   for build in "${!build_jobs[@]}"; do
      # Has the build started?
      if [[ -n "${build_ids[${build}]+0}" ]]; then
         builds_started=$(( builds_started + 1))
         # Calculate how many processors are in use
         # Is the build still running?
         if ps -p "${build_ids[${build}]}" > /dev/null; then
            procs_in_use=$(( procs_in_use + build_jobs["${build}"] ))
         fi
      fi
   done

   sleep 5s
done

# Wait for all jobs to complete and check return statuses
while [[ ${#build_jobs[@]} -gt 0 ]]; do
   for build in "${!build_jobs[@]}"; do
      # Test if each job is complete and if so, notify and remove from the array
      if [[ -n "${build_ids[${build}]+0}" ]]; then
         if ! ps -p "${build_ids[${build}]}" > /dev/null; then
            wait "${build_ids[${build}]}"
            build_stat=$?
            errs=$((errs+build_stat))
            if [[ ${build_stat} == 0 ]]; then
               echo "build_${build}.sh completed successfully!"
            else
               echo "build_${build}.sh failed with status ${build_stat}!"
            fi

            # Remove the completed build from the list of PIDs
            unset 'build_ids[${build}]'
            unset 'build_jobs[${build}]'
         fi
      fi
   done

   sleep 5s
done

#------------------------------------
# Exception Handling
#------------------------------------
if (( errs != 0 )); then
  cat << EOF
BUILD ERROR: One or more components failed to build
  Check the associated build log(s) for details.
EOF
  ${ERRSCRIPT} || exit "${errs}"
fi

echo;echo " .... Build system finished .... "

exit 0
