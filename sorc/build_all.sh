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

Usage: ${BASH_SOURCE[0]} [-a UFS_app][-c build_config][-d][-f][-h][-j n][-v][-w][-y]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -d:
    Build in debug mode
  -f:
    Build the UFS model using the -DFASTER=ON option
  -g:
    Build GSI
  -h:
    Print this help message and exit
  -j:
    Specify maximum number of build jobs (n)
  -k:
    Kill all builds if any build fails
  -u:
    Build UFS-DA
  -v:
    Execute all build scripts with -v option to turn on verbose where supported
  -w:
    Use structured wave grid
  -y:
    Use hydrostatic version of FV3
EOF
  exit 1
}

# shellcheck disable=SC2155
readonly HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
cd "${HOMEgfs}/sorc" || exit 1

_build_ufs_opt=""
_build_ufsda="NO"
_build_gsi="NO"
_build_debug=""
_verbose_opt=""
_wave_opt=""
_hydro_opt=""
_build_job_max=20
_quick_kill="NO"
# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":a:dfghj:kuvwy" option; do
  case "${option}" in
    a) _build_ufs_opt+="-a ${OPTARG} ";;
    f) _build_ufs_opt+="-f ";;
    d) _build_debug="-d" ;;
    g) _build_gsi="YES" ;;
    h) _usage;;
    j) _build_job_max="${OPTARG} ";;
    k) _quick_kill="YES" ;;
    u) _build_ufsda="YES" ;;
    v) _verbose_opt="-v";;
    w) _wave_opt="-w";;
    y) _hydro_opt="-y";;
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

logs_dir="${HOMEgfs}/sorc/logs"
if [[ ! -d "${logs_dir}" ]]; then
  echo "Creating logs folder"
  mkdir -p "${logs_dir}" || exit 1
fi

# Check final exec folder exists
if [[ ! -d "${HOMEgfs}/exec" ]]; then
  echo "Creating ${HOMEgfs}/exec folder"
  mkdir -p "${HOMEgfs}/exec"
fi

#------------------------------------
# GET MACHINE
#------------------------------------
export COMPILER="intel"
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"
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
build_opts["ufs"]="${_wave_opt} ${_hydro_opt} ${_verbose_opt} ${_build_ufs_opt} ${_build_debug}"

build_jobs["upp"]=1
build_opts["upp"]="${_build_debug}"

build_jobs["ufs_utils"]=1
build_opts["ufs_utils"]="${_verbose_opt} ${_build_debug}"

build_jobs["gfs_utils"]=1
build_opts["gfs_utils"]="${_verbose_opt} ${_build_debug}"

build_jobs["ww3prepost"]=1
build_opts["ww3prepost"]="${_wave_opt} ${_verbose_opt} ${_build_ufs_opt} ${_build_debug}"

# Optional DA builds
if [[ "${_build_ufsda}" == "YES" ]]; then
   if [[ "${MACHINE_ID}" != "orion" && "${MACHINE_ID}" != "hera" && "${MACHINE_ID}" != "hercules" && "${MACHINE_ID}" != "wcoss2" && "${MACHINE_ID}" != "noaacloud" && "${MACHINE_ID}" != "gaea" ]]; then
      echo "NOTE: The GDAS App is not supported on ${MACHINE_ID}.  Disabling build."
   else
      build_jobs["gdas"]=8
      big_jobs=$((big_jobs+1))
      build_opts["gdas"]="${_verbose_opt} ${_build_debug}"
   fi
fi
if [[ "${_build_gsi}" == "YES" ]]; then
   build_jobs["gsi_enkf"]=2
   build_opts["gsi_enkf"]="${_verbose_opt} ${_build_debug}"
fi
if [[ "${_build_gsi}" == "YES" || "${_build_ufsda}" == "YES" ]] ; then
   build_jobs["gsi_utils"]=1
   build_opts["gsi_utils"]="${_verbose_opt} ${_build_debug}"
   build_jobs["gsi_monitor"]=1
   build_opts["gsi_monitor"]="${_verbose_opt} ${_build_debug}"
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
   # Add cores to the gdas and ufs build jobs
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

check_builds()
{
   for chk_build in "${!build_jobs[@]}"; do
      # Check if the build is complete and if so what the status was
      if [[ -n "${build_ids[${chk_build}]+0}" ]]; then
         if ! ps -p "${build_ids[${chk_build}]}" > /dev/null; then
            wait "${build_ids[${chk_build}]}"
            build_stat=$?
            if [[ ${build_stat} != 0 ]]; then
               echo "build_${chk_build}.sh failed!  Exiting!"
               echo "Check logs/build_${chk_build}.log for details."
               echo "logs/build_${chk_build}.log" > "${HOMEgfs}/sorc/logs/error.logs"
               for kill_build in "${!build_jobs[@]}"; do
                  if [[ -n "${build_ids[${kill_build}]+0}" ]]; then
                     pkill -P "${build_ids[${kill_build}]}"
                  fi
               done
               return "${build_stat}"
            fi
         fi
      fi
   done
   return 0
}

builds_started=0
# Now start looping through all of the jobs until everything is done
while [[ ${builds_started} -lt ${#build_jobs[@]} ]]; do
   for build in "${!build_jobs[@]}"; do
      # Has the job started?
      if [[ -n "${build_jobs[${build}]+0}" && -z "${build_ids[${build}]+0}" ]]; then
         # Do we have enough processors to run it?
         if [[ ${_build_job_max} -ge $(( build_jobs[build] + procs_in_use )) ]]; then
            # double-quoting build_opts here will not work since it is a string of options
            #shellcheck disable=SC2086
            "./build_${build}.sh" ${build_opts[${build}]:-} -j "${build_jobs[${build}]}" > \
               "${logs_dir}/build_${build}.log" 2>&1 &
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

   # If requested, check if any build has failed and exit if so
   if [[ "${_quick_kill}" == "YES" ]]; then
      check_builds
      build_stat=$?
      if (( build_stat != 0 )); then
         exit "${build_stat}"
      fi
   fi

   sleep 5s
done


# Wait for all jobs to complete and check return statuses
while [[ "${#build_jobs[@]}" -gt 0 ]]; do

   # If requested, check if any build has failed and exit if so
   if [[ "${_quick_kill}" == "YES" ]]; then
      check_builds
      build_stat=$?
      if [[ ${build_stat} != 0 ]]; then
         exit "${build_stat}"
      fi
   fi

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
