#!/usr/bin/env bash

###
function _usage() {
   cat <<-EOF
   This script automates the experiment setup process for the global workflow.
   Options are also available to update submodules, build the workflow (with
   specific build flags), specicy which YAMLs and YAML directory to run, and
   whether to automatically update your crontab.

   Usage: generage_experiments.sh [OPTIONS] /path/to/RUNTESTS

    -H Root directory of the global workflow.
       If not specified, then the directory is assumed to be one parent
       directory up from this script's residing directory.

    -b Run build_all.sh with default flags
       (build the UFS, UPP, UFS_Utils, and GFS-utils only

    -B "build flags"
       Run build_all.sh with the build specified flags.  Refer to
       build_all.sh -h for a list of valid flags.
       NOTE: the list of build flags MUST be in quotes.

    -u Update submodules before building and/or generating experiments.

    -y "list of YAMLs to run"
       If this option is not specified, the default case (C48_ATM) will be
       run.  This option is overidden by -G or -E (see below).
       Example: -y "C48_ATM C48_S2SW C96C48_hybatmDA"

    -Y /path/to/directory/with/YAMLs
       If this option is not specified, then the \${HOMEgfs}/ci/cases/pr
       directory is used.

    -G Run all valid GFS cases in the specified YAML directory.
       If -b is specified, then "-g -u" (build the GSI and GDASApp)
       will be passed to build_all.sh unless -B is also specified.
       Note that these builds are disabled on some systems, which
       will result in a warning from build_all.sh.

    -E Run all valid GEFS cases in the specified YAML directory.
       If -b is specified, then "-w" will be passed to build_all.sh
          unless -B is also specified.

    -S (Not yet supported!)
       Run all valid SFS cases in the specified YAML directory.

    NOTES:
         - Only one of -G -E or -S may be specified
         - Valid cases are determined by the experiment:system key as
           well as the skip_ci_on_hosts list in each YAML.

    -A "HPC account name"  Set the HPC account name.
       If this is not set, the default in
       \$HOMEgfs/ci/platform/config.\$machine
       will be used.

    -c Append the chosen set of tests to your existing crontab
       If this option is not chosen, the new entries that would have been
       written to your crontab will be printed to stdout.
       NOTES:
          - This option is not supported on Gaea.  Instead, the output will
            need to be written to scrontab manually.
          - For Orion/Hercules, this option will not work unless run on
            the [orion|hercules]-login-1 head node.

    -e "your@email.com" Email address to place in the crontab.
       If this option is not specified, then the existing email address in
       the crontab will be preserved.

    -v Verbose mode.  Prints output of all commands to stdout.

    -V Very verbose mode.  Passes -v to all commands and prints to stdout.

    -d Debug mode.  Same as -V but also enables logging (set -x).

    -h Display this message.
EOF
}

set -eu

if [[ $# -ne 1 ]]; then
   _usage
   exit 1
fi

# Set default options
HOMEgfs=""
_specified_home=false
_build=false
_build_flags=""
_explicit_build_flags=false
_update_submods=false
_yaml_list="C48_ATM"
_yaml_dir=""
_specified_yaml_dir=false
_run_all_gfs=false
_run_all_gefs=false
_run_all_sfs=false
_hpc_account=""
_set_account=false
_update_cron=false
_email=""
_set_email=false
_verbose=false
_very_verbose=false
_verbose_flag=""
_debug="false"
_cwd=$(pwd)
_redirect='>/dev/null'  # Redirect stdout to /dev/null; stderr to terminal

while getopts ":H:bB:uy:Y:GESA:ce:vVdh" option; do
  case "${option}" in
    H)
       HOMEgfs="${OPTARG}"
       _specified_home=true
       if [[ ! -d "${HOMEGFS}" ]]; then
          echo "Specified HOMEgfs directory (${HOMEGFS}) does not exist"
          exit 2
       fi
       ;;
    b) _build=true ;;
    B) _build_flags="${OPTARG}" && _explicit_build_flags=true ;;
    u) _update_submods=true ;;
    y) _yaml_list="${OPTARG}"
       # strip .yaml from the end of each
       _yaml_list=${_yaml_list//".yaml"/}
       ;;
    Y) _yaml_dir="${OPTARG}" && _specified_yaml_dir=true ;;
    G) _run_all_gfs=true ;;
    E) _run_all_gefs=true ;;
    S) _run_all_sfs=true ;;
    c) _update_cron=true ;;
    e) _email="${OPTARG}" && _set_email=true ;;
    v) _verbose=true ;;
    V) _very_verbose=true && _verbose=true && _verbose_flag="-v" ;;
    d) _debug=true && _very_verbose=true && _verbose=true && _verbose_flag="-v";;
    h) _usage ;;
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

# Nullify the redirect if we are in verbose mode
[[ "${_verbose}" == "true" ]] && _redirect=""

# Turn on logging if running in debug mode
if [[ "${_debug}" == "true" ]]; then
   set -x
fi

# Create the RUNTESTS directory
export RUNTESTS=$1
[[ "${_verbose}" == "true" ]] && echo "Creating RUNTESTS in ${RUNTESTS}"
if [[ ! -d "${RUNTESTS}" ]]; then
   set +e
   if ! mkdir -p "${RUNTESTS}" "${_verbose_flag}"; then
      echo "Unable to create RUNTESTS directory: ${RUNTESTS}"
      echo "Rerun with -h for usage examples."
      exit 3
   fi
   set -e
fi

# Test if multiple "run_all" options were set
_count_run_alls=0
[[ "${_run_all_gfs}" == "true" ]] && ((_count_run_alls+=1))
[[ "${_run_all_gefs}" == "true" ]] && ((_count_run_alls+=1))
[[ "${_run_all_sfs}" == "true" ]] && ((_count_run_alls+=1))

if (( _count_run_alls > 1 )) ; then
   echo "Only one run all option (-G -E -S) may be specified"
   echo "Rerun with just one option and/or with -h for usage examples"
   exit 4
fi

# Append -w to build_all.sh flags if -E was specified
if [[ "${_run_all_gefs}" == "true" && "${_explicit_build_flags}" == "false" && \
      "${_build}" == "true" ]]; then
   _build_flags="-w"
fi

# Append -g -u to build_all.sh flags if -G was specified
if [[ "${_run_all_gfs}" == "true" && "${_explicit_build_flags}" == "false" && \
      "${_build}" == "true" ]]; then
   _build_flags="-g -u"
fi

# If -S is specified, exit (for now).
# TODO when SFS tests come online, enable this option.
if [[ "${_run_all_sfs}" == "true" ]]; then
   echo "There are no known SFS tests at this time.  Aborting."
   echo "If you have prepared YAMLs for SFS cases, specify their"
   echo "location and names without '-S', e.g."
   echo "generate_experiments.sh -y \"C48_S2S_SFS\" -Y \"/path/to/yaml/directory\""
   exit 0
fi

# Set HOMEgfs if it wasn't set by the user
if [[ "${_specified_home}" == "false" ]]; then
   script_relpath="$(dirname "${BASH_SOURCE[0]}")"
   HOMEgfs="$(cd "${script_relpath}/.." && pwd)"
   [[ "${_verbose}" == "true" ]] && echo "Setting HOMEgfs to ${HOMEgfs}"
fi

# Loading modules sometimes raises unassigned errors, so disable checks
set +eu
[[ "${_verbose}" == "true" ]] && echo "Loading modules"
[[ "${_debug}" == "true" ]] && set +x
source "${HOMEgfs}/workflow/gw_setup.sh"
[[ "${_debug}" == "true" ]] && set -x
set -eu
machine=${MACHINE_ID}
. "${HOMEgfs}/ci/platforms/config.$machine"

# If _yaml_dir is not set, set it to $HOMEgfs/ci/cases/pr
if [[ -z ${_yaml_dir} ]]; then
   _yaml_dir="${HOMEgfs}/ci/cases/pr"
fi

# Update submodules if requested
if [[ "${_update_submods}" == "true" ]]; then
   echo "Updating submodules..."
   #shellcheck disable=SC2086
   git submodule update --init --recursive -j 10 ${_redirect}
fi

if [[ ${BUILD} =~ "Y" ]]; then
    echo "Building via build_all.sh ${_build_flags}..."
    # Let the output of build_all.sh go to stdout regardless of verbose options
    #shellcheck disable=SC2086
    ${HOMEgfs}/sorc/build_all.sh ${_verbose_flag} ${_build_flags}
fi

# Configure the environment for running create_experiment.py

for _yaml in ${_yaml_list}; do
   _yaml_file="${_yaml_dir}/${_yaml}.yaml"
   # Verify that the YAMLs are where we are pointed
   if [[ ! -s "${_yaml_file}" ]]; then
      echo "The YAML file ${_yaml_file} does not exist!"
      echo "Please check the input yaml list and directory."
      exit 4
   fi

   # Strip any unsupported tests
   _unsupported_systems=$(sed '1,/skip_ci_on_hosts/ d' "${_yaml_file}")

   for _system in ${_unsupported_systems}; do
      if [[ "${_system}" =~ ${machine} ]]; then
         echo "WARNING ${_yaml} is unsupported on ${machine}, removing from test list"
         # Sleep so the user has a moment to notice
         sleep 2s
         yaml_list=$(echo "${_yaml_list}" | sed "s/ ${_yaml}//")
         break
      fi
   done
done

# Update the account if specified
[[ "${_set_account}" == "true" ]] && export HPC_ACCOUNT=${_hpc_account}

# Create the experiments
rm -f "tests.cron" "${_verbose_flag}"
for _case in ${_yaml_list}; do
   #shellcheck disable=SC2086
   pslot=${_case} ./create_experiment.py -y "../ci/cases/pr/${_case}.yaml" --overwrite ${_redirect}
   crontab_entry=$(grep "${_case}" "${RUNTESTS}/EXPDIR/${_case}/${_case}.crontab")
   echo "${crontab_entry}" >> tests.cron
done

# Update the cron
if [[ "${_update_cron}" == "true" ]]; then
   echo "Updating the existing crontab"
   rm -f existing.cron final.cron "${_verbose_flag}"
   touch existing.cron final.cron

   # disable -e in case crontab is empty
   set +e
   crontab -l > existing.cron
   set -e

   if [[ "${_debug}" == "true" ]]; then
      echo "Existing crontab: "
      echo "#######################"
      cat existing.cron
      echo "#######################"
   fi

   if [[ "${_set_email}" == "true" ]]; then
      # Replace the existing email in the crontab
      [[ "${_verbose}" ]] && echo "Updating crontab email to ${_email}"
      sed -i "/^MAILTO/d" existing.cron
      echo "MAILTO=\"${_email}\"" >> final.cron
   fi

   cat existing.cron tests.cron >> final.cron

   if [[ "${_verbose}" == "true" ]]; then
      echo "Setting crontab to:"
      echo "#######################"
      cat final.cron
      echo "#######################"
   fi

   cat final.cron | crontab -
else
   echo "Experiment setup complete!"
   echo "Add the following to your crontab or scrontab to start running:"
   cat tests.cron
fi

# Cleanup
[[ "${_debug}" == "false" ]] && rm -f final.cron existing.cron tests.cron "${_verbose_flag}"

echo "Success!!"
