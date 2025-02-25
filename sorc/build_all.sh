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
Builds all of the global-workflow components by calling the individual build scripts in parallel.

Usage: ${BASH_SOURCE[0]} [-a UFS_app][-c build_config][-d][-f][-h][-v] [gfs] [gefs] [sfs] [gsi] [gdas] [all]
  -a UFS_app:
    Build a specific UFS app instead of the default.  This will be applied to all UFS (GFS, GEFS, SFS) builds.
  -d:
    Build in debug mode
  -f:
    Build the UFS model(s) using the -DFASTER=ON option.
  -h:
    Print this help message and exit
  -k:
    Kill all builds if any build fails
  -v:
    Execute all build scripts with -v option to turn on verbose where supported

  Specified systems (gfs, gefs, sfs, gsi, gdas) are non-exclusive, so they can be built together.
EOF
  exit 1
}

# shellcheck disable=SC2155
readonly HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
cd "${HOMEgfs}/sorc" || exit 1

_build_ufs_opt=""
_build_debug=""
_verbose_opt=""
_build_job_max=20
_quick_kill="NO"
_ufs_exec="-e gfs_model.x"
# Reset option counter in case this script is sourced
OPTIND=1
while getopts ":a:dfhkv" option; do
  case "${option}" in
    a) _build_ufs_opt+="-a ${OPTARG} ";;
    f) _build_ufs_opt+="-f ";;
    d) _build_debug="-d" ;;
    h) _usage;;
    k) _quick_kill="YES" ;;
    v) _verbose_opt="-v" ;;
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

# If no build system was specified, build for gfs forecast-only
if [[ $# -eq 0 ]]; then
   selected_systems="gfs"
else
   selected_systems="$*"
fi

supported_systems=("gfs" "gefs" "sfs" "gsi" "gdas" "all")

declare -A system_builds
system_builds=(
   ["gfs"]="ufs_gfs gfs_utils ufs_utils upp ww3_gfs"
   ["gefs"]="ufs_gefs gfs_utils ufs_utils upp ww3_gefs"
   ["sfs"]="ufs_sfs gfs_utils ufs_utils upp ww3_gefs"
   ["gsi"]="gsi_enkf gsi_monitor gsi_utils"
   ["gdas"]="gdas gsi_monitor gsi_utils"
   ["all"]="ufs_gfs gfs_utils ufs_utils upp ww3_gfs ufs_gefs ufs_sfs ww3_gefs gdas gsi_enkf gsi_monitor gsi_utils"
)

logs_dir="${HOMEgfs}/sorc/logs"
if [[ ! -d "${logs_dir}" ]]; then
  echo "Creating logs folder"
  mkdir -p "${logs_dir}" || exit 1
fi

# Jobs per build ("min max")
declare -A build_jobs build_opts build_scripts
build_jobs=(
    ["ufs_gfs"]=8 ["ufs_gefs"]=8 ["ufs_sfs"]=8 ["gdas"]=8 ["gsi_enkf"]=2 ["gfs_utils"]=1 ["ufs_utils"]=1
    ["ww3_gfs"]=1 ["ww3_gefs"]=1 ["gsi_utils"]=1 ["gsi_monitor"]=1 ["gfs_utils"]=1 ["upp"]=1
)

# Establish build options for each job
_gfs_exec="gfs_model.x"
_gefs_exec="gefs_model.x"
_sfs_exec="sfs_model.x"
build_opts=(
    ["ufs_gfs"]="${wave_opt} ${_build_ufs_opt} ${_verbose_opt} ${_build_debug} -e ${_gfs_exec}"
    ["ufs_gefs"]="${wave_opt} ${_build_ufs_opt} ${_verbose_opt} ${_build_debug} -w -e ${_gefs_exec}"
    ["ufs_sfs"]="${wave_opt} ${_build_ufs_opt} ${_verbose_opt} ${_build_debug} -y -e ${_sfs_exec}"
    ["upp"]="${_build_debug}"
    ["ww3_gfs"]="${_verbose_opt} ${_build_debug}"
    ["ww3_gefs"]="-w ${_verbose_opt} ${_build_debug}"
    ["gdas"]="${_verbose_opt} ${_build_debug}"
    ["ufs_utils"]="${_verbose_opt} ${_build_debug}"
    ["gfs_utils"]="${_verbose_opt} ${_build_debug}"
    ["gsi_utils"]="${_verbose_opt} ${_build_debug}"
    ["gsi_enkf"]="${_verbose_opt} ${_build_debug}"
    ["gsi_monitor"]="${_verbose_opt} ${_build_debug}"
)

# Set the build script name for each build
build_scripts=(
    ["ufs_gfs"]="build_ufs.sh"
    ["ufs_gefs"]="build_ufs.sh"
    ["ufs_sfs"]="build_ufs.sh"
    ["gdas"]="build_gdas.sh"
    ["gsi_enkf"]="build_gsi_enkf.sh"
    ["gfs_utils"]="build_gfs_utils.sh"
    ["ufs_utils"]="build_ufs_utils.sh"
    ["ww3_gfs"]="build_ww3prepost.sh"
    ["ww3_gefs"]="build_ww3prepost.sh"
    ["gsi_utils"]="build_gsi_utils.sh"
    ["gsi_monitor"]="build_gsi_monitor.sh"
    ["gfs_utils"]="build_gfs_utils.sh"
    ["upp"]="build_upp.sh"
)

# Check the requested systems to make sure we can build them
declare -A builds
system_count=0
for system in ${selected_systems}; do
   # shellcheck disable=SC2076
   if [[ " ${supported_systems[*]} " =~ " ${system} " ]]; then
      (( system_count += 1 ))
      for build in ${system_builds["${system}"]}; do
         builds["${build}"]="yes"
      done
   else
      echo "Unsupported build system: ${system}"
      _usage
   fi
done

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

# Create the log directory
mkdir -p "${HOMEgfs}/sorc/logs"

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

#------------------------------------
# Check which builds to do and assign # of build jobs
#------------------------------------

echo "Building ${build_list}"

procs_in_use=0
declare -A build_ids

check_builds()
{
   for chk_build in "${!builds[@]}"; do
      # Check if the build is complete and if so what the status was
      if [[ -n "${build_ids[${chk_build}]+0}" ]]; then
         if ! ps -p "${build_ids[${chk_build}]}" > /dev/null; then
            wait "${build_ids[${chk_build}]}"
            build_stat=$?
            if [[ ${build_stat} != 0 ]]; then
               echo "build_${chk_build}.sh failed!  Exiting!"
               echo "Check logs/build_${chk_build}.log for details."
               echo "logs/build_${chk_build}.log" > "${HOMEgfs}/sorc/logs/error.logs"
               for kill_build in "${!builds[@]}"; do
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
while [[ ${builds_started} -lt ${#builds[@]} ]]; do
   for build in "${!builds[@]}"; do
      # Has the job started?
      if [[ -n "${build_jobs[${build}]+0}" && -z "${build_ids[${build}]+0}" ]]; then
         # Do we have enough processors to run it?
         if [[ ${_build_job_max} -ge $(( build_jobs[build] + procs_in_use )) ]]; then
            # double-quoting build_opts here will not work since it is a string of options
            #shellcheck disable=SC2086
            "./${build_scripts[${build}]}" ${build_opts[${build}]:-} -j "${build_jobs[${build}]}" > \
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
   for build in "${!builds[@]}"; do
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
while [[ "${#builds[@]}" -gt 0 ]]; do

   # If requested, check if any build has failed and exit if so
   if [[ "${_quick_kill}" == "YES" ]]; then
      check_builds
      build_stat=$?
      if [[ ${build_stat} != 0 ]]; then
         exit "${build_stat}"
      fi
   fi

   for build in "${!builds[@]}"; do
      # Test if each job is complete and if so, notify and remove from the array
      if [[ -n "${build_ids[${build}]+0}" ]]; then
         if ! ps -p "${build_ids[${build}]}" > /dev/null; then
            wait "${build_ids[${build}]}"
            build_stat=$?
            errs=$((errs+build_stat))
            if [[ ${build_stat} == 0 ]]; then
               echo "${build_scripts[${build}]} completed successfully!"
            else
               echo "${build_scripts[${build}]} failed with status ${build_stat}!"
            fi

            # Remove the completed build from the list of PIDs
            unset 'build_ids[${build}]'
            unset 'builds[${build}]'
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
