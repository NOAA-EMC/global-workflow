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

Usage: ${BASH_SOURCE[0]} [-a UFS_app][-c build_config][-h][-j n][-v]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -c build_config:
    Selectively build based on the provided config instead of the default config
  -h:
    print this help message and exit
  -j:
    Specify maximum number of build jobs (n)
  -v:
    Execute all build scripts with -v option to turn on verbose where supported
EOF
  exit 1
}

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd "${script_dir}" || exit 1

_build_ufs_opt=""
_verbose_opt=""
_partial_opt=""
_build_job_max=20
# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":a:c:j:hv" option; do
  case "${option}" in
    a) _build_ufs_opt+="-a ${OPTARG} ";;
    c) _partial_opt+="-c ${OPTARG} ";;
    h) _usage;;
    j) _build_job_max="${OPTARG} ";;
    v) _verbose_opt="-v";;
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
# INCLUDE PARTIAL BUILD
#------------------------------------
# Turn off some shellcheck warnings because we want to have
#   variables with multiple arguments.
# shellcheck disable=SC2086,SC2248
source ./partial_build.sh ${_verbose_opt} ${_partial_opt}
# shellcheck disable=

#------------------------------------
# Exception Handling Init
#------------------------------------
# Disable shellcheck warning about single quotes not being substituted.
# shellcheck disable=SC2016
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
# shellcheck disable=
err=0

declare -A build_jobs

#------------------------------------
# Check which builds to do and assign # of build jobs
#------------------------------------

# Mandatory builds, unless otherwise specified, for the UFS
big_jobs=0
[[ ${Build_ufs_model} == 'true' ]] && build_jobs["ufs_model"]=8 && big_jobs=$((big_jobs+1))
# The UPP is hardcoded to use 6 cores
[[ ${Build_upp} == 'true' ]] && build_jobs["upp"]=6
[[ ${Build_ufs_utils} == 'true' ]] && build_jobs["ufs_utils"]=3
[[ ${Build_gfs_utils} == 'true' ]] && build_jobs["gfs_utils"]=1
[[ ${Build_ww3prepost} == "true" ]] && build_jobs["ww3prepost"]=2

# Optional DA builds
[[ -d gdas.cd ]] && build_jobs["gdas"]=16 && big_jobs=$((big_jobs+1))
[[ -d gsi_enkf.fd ]] && build_jobs["gsi_enkf"]=8 && big_jobs=$((big_jobs+1))
[[ -d gsi_utils.fd ]] && build_jobs["gsi_utils"]=2
[[ -d gsi_monitor.fd ]] && build_jobs["gsi_monitor"]=1

# Go through all builds and adjust CPU counts down if necessary
requested_cpus=0
build_list=""
for build in "${!build_jobs[@]}"; do
   [[ -z "${build_list}" ]] && build_list="${build}" || build_list="${build_list}, ${build}"
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
      if [[ "${build}" == "gdas" || "${build}" == "ufs_model" || "${build}" == "gsi_enkf" ]]; then
         build_jobs[${build}]=$(( build_jobs[${build}] + extra_cores ))
      fi
   done
fi

procs_in_use=0
declare -A build_ids

builds_started=0
# Now start looping through all of the remaining jobs until everything is done
while [[ ${builds_started} -lt ${#build_jobs[@]} ]]; do
   if [[ ${procs_in_use} -lt ${_build_job_max} ]]; then
      if [[ -n "${build_jobs[gdas]+0}" && ! -n "${build_ids[gdas]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['gdas'] + procs_in_use )) ]]; then
            ./build_gdas.sh -j "${build_jobs[gdas]}" "${_verbose_opt}" > \
               "${logs_dir}/build_gdas.log" 2>&1 &
            build_ids["gdas"]=$!
            echo "Starting build_gdas.sh"
            procs_in_use=$(( procs_in_use + build_jobs['gdas'] ))
         fi
      fi
      if [[ -n "${build_jobs[ufs_model]+0}" && ! -n "${build_ids[ufs_model]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['ufs_model'] + procs_in_use )) ]]; then
            ./build_ufs.sh -j "${build_jobs[ufs_model]}" "${_verbose_opt}" "${_build_ufs_opt}" > \
               "${logs_dir}/build_ufs.log" 2>&1 &
            build_ids["ufs_model"]=$!
            echo "Starting build_ufs.sh"
            procs_in_use=$(( procs_in_use + build_jobs['ufs_model'] ))
         fi
      fi
      if [[ -n "${build_jobs[gsi_enkf]+0}" && ! -n "${build_ids[gsi_enkf]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['gsi_enkf'] + procs_in_use )) ]]; then
            ./build_gsi_enkf.sh -j "${build_jobs[gsi_enkf]}" "${_verbose_opt}" > \
               "${logs_dir}/build_gsi_enkf.log" 2>&1 &
            build_ids["gsi_enkf"]=$!
            echo "Starting build_gsi_enkf.sh"
            procs_in_use=$(( procs_in_use + build_jobs['gsi_enkf'] ))
         fi
      fi
      if [[ -n "${build_jobs[ufs_utils]+0}" && ! -n "${build_ids[ufs_utils]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['ufs_utils'] + procs_in_use )) ]]; then
            ./build_ufs_utils.sh -j "${build_jobs[ufs_utils]}" "${_verbose_opt}" > \
               "${logs_dir}/build_ufs_utils.log" 2>&1 &
            build_ids["ufs_utils"]=$!
            echo "Starting build_ufs_utils.sh"
            procs_in_use=$(( procs_in_use + build_jobs['ufs_utils'] ))
         fi
      fi
      if [[ -n "${build_jobs[gsi_utils]+0}" && ! -n "${build_ids[gsi_utils]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['gsi_utils'] + procs_in_use )) ]]; then
            ./build_gsi_utils.sh -j "${build_jobs[gsi_utils]}" "${_verbose_opt}" > \
               "${logs_dir}/build_gsi_utils.log" 2>&1 &
            build_ids["gsi_utils"]=$!
            echo "Starting build_gsi_utils.sh"
            procs_in_use=$(( procs_in_use + build_jobs['gsi_utils'] ))
         fi
      fi
      if [[ -n "${build_jobs[upp]+0}" && ! -n "${build_ids[upp]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['upp'] + procs_in_use )) ]]; then
            ./build_upp.sh "${_verbose_opt}" > \
               "${logs_dir}/build_upp.log" 2>&1 &
            build_ids["upp"]=$!
            echo "Starting build_upp.sh"
            procs_in_use=$(( procs_in_use + build_jobs['upp'] ))
         fi
      fi
      if [[ -n "${build_jobs[ww3prepost]+0}" && ! -n "${build_ids[ww3prepost]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['ww3prepost'] + procs_in_use )) ]]; then
            ./build_ww3prepost.sh -j "${build_jobs[ww3prepost]}" "${_verbose_opt}" "${_build_ufs_opt}" > \
               "${logs_dir}/build_ww3prepost.log" 2>&1 &
            build_ids["ww3prepost"]=$!
            echo "Starting build_ww3prepost.sh"
            procs_in_use=$(( procs_in_use + build_jobs['ww3prepost'] ))
         fi
      fi
      if [[ -n "${build_jobs[gsi_monitor]+0}" && ! -n "${build_ids[gsi_monitor]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['gsi_monitor'] + procs_in_use )) ]]; then
            ./build_gsi_monitor.sh -j "${build_jobs[gsi_monitor]}" "${_verbose_opt}" > \
               "${logs_dir}/build_gsi_monitor.log" 2>&1 &
            build_ids["gsi_monitor"]=$!
            echo "Starting build_gsi_monitor.sh"
            procs_in_use=$(( procs_in_use + build_jobs['gsi_monitor'] ))
         fi
      fi
      if [[ -n "${build_jobs[gfs_utils]+0}" && ! -n "${build_ids[gfs_utils]+0}" ]]; then
         if [[ ${_build_job_max} -ge $(( build_jobs['gfs_utils'] + procs_in_use )) ]]; then
            ./build_gfs_utils.sh -j "${build_jobs[gfs_utils]}" "${_verbose_opt}" > \
               "${logs_dir}/build_gfs_utils.log" 2>&1 &
            build_ids["gfs_utils"]=$!
            echo "Starting build_gfs_utils.sh"
            procs_in_use=$(( procs_in_use + build_jobs['gfs_utils'] ))
         fi
      fi
   fi

   # Check if all builds have completed
   # Also recalculate how many processors are in use to account for completed builds
   builds_started=0
   procs_in_use=0
   for build in "${!build_jobs[@]}"; do
      if [[ -n "${build_ids[${build}]+0}" ]]; then
         builds_started=$(( builds_started + 1))
         # Calculate how many processors are in use
         if ps -p "${build_ids[${build}]}" > /dev/null; then
            procs_in_use=$(( procs_in_use + build_jobs["${build}"] ))
         fi
      fi
   done

   sleep 5s
done

# Wait for all jobs to complete and check return statuses
errs=0
while [[ ${#build_jobs[@]} -gt 0 ]]; do
   for build in "${!build_jobs[@]}"; do
      # Test if each job is complete and if so, notify and remove from the array
      if [[ -n "${build_ids[${build}]+0}" ]]; then
         if ! ps -p "${build_ids[${build}]}" > /dev/null; then
            wait "${build_ids[${build}]}" && build_stat=$?
            errs=$((errs+build_stat))
            if [[ ${build_stat} == 0 ]]; then
               echo "${build} completed successfully!"
            else
               echo "${build} failed with status ${build_stat}!"
            fi

            # Remove the completed build from the list of PIDs
            unset 'build_ids[${build}]'
            unset 'build_jobs[${build}]'
            break
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
  ${ERRSCRIPT} || exit "${err}"
fi

echo;echo " .... Build system finished .... "

exit 0
