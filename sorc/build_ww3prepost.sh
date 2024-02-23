#! /usr/bin/env bash
set -x

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "${script_dir}" || exit 1

# Default settings
APP="S2SWA"
PDLIB="OFF" 

while getopts ":j:a:dvw" option; do
  case "${option}" in
    a) APP="${OPTARG}";;
    d) BUILD_TYPE="DEBUG";;
    j) BUILD_JOBS="${OPTARG}";;
    v) export BUILD_VERBOSE="YES";;
    w) PDLIB="ON";;
    :)
      echo "[${BASH_SOURCE[0]}]: ${option} requires an argument"
      usage
      ;;
    *)
      echo "[${BASH_SOURCE[0]}]: Unrecognized option: ${option}"
      usage
      ;;
  esac
done

# Determine which switch to use
if [[ "${APP}" == "ATMW" ]]; then 
  ww3switch="model/esmf/switch"
else 
  if [[ "${PDLIB}" == "ON" ]]; then 
    ww3switch="model/bin/switch_meshcap_pdlib"
  else 
    ww3switch="model/bin/switch_meshcap"
  fi 
fi 

# Check final exec folder exists
if [[ ! -d "../exec" ]]; then
  mkdir ../exec
fi

finalexecdir="$( pwd -P )/../exec"

#Determine machine and load modules
set +x
source "${script_dir}/ufs_model.fd/tests/detect_machine.sh"
source "${script_dir}/ufs_model.fd/tests/module-setup.sh"
module use "${script_dir}/ufs_model.fd/modulefiles"
module load "ufs_${MACHINE_ID}.intel"
set -x

#Set WW3 directory, switch, prep and post exes
cd ufs_model.fd/WW3 || exit 1
WW3_DIR=$( pwd -P )
export WW3_DIR
export SWITCHFILE="${WW3_DIR}/${ww3switch}"

# Build exes for prep jobs and post jobs:
prep_exes="ww3_grid ww3_prep ww3_prnc ww3_grid"
post_exes="ww3_outp ww3_outf ww3_outp ww3_gint ww3_ounf ww3_ounp ww3_grib"

#create build directory:
path_build="${WW3_DIR}/build_SHRD"
[[ -d "${path_build}" ]] && rm -rf "${path_build}"
mkdir -p "${path_build}" || exit 1
cd "${path_build}" || exit 1
echo "Forcing a SHRD build"

buildswitch="${path_build}/switch"

cat "${SWITCHFILE}" > "${path_build}/tempswitch"

sed -e "s/DIST/SHRD/g"\
    -e "s/OMPG / /g"\
    -e "s/OMPH / /g"\
    -e "s/MPIT / /g"\
    -e "s/MPI / /g"\
    -e "s/B4B / /g"\
    -e "s/PDLIB / /g"\
    -e "s/SCOTCH / /g"\
    -e "s/METIS / /g"\
    -e "s/NOGRB/NCEP2/g"\
       "${path_build}/tempswitch" > "${path_build}/switch"
rm "${path_build}/tempswitch"

echo "Switch file is ${buildswitch} with switches:" 
cat "${buildswitch}"

#define cmake build options
MAKE_OPT="-DCMAKE_INSTALL_PREFIX=install"
[[ ${BUILD_TYPE:-"Release"} = "DEBUG" ]] && MAKE_OPT+=" -DDEBUG=ON"

#Build executables:
cmake "${WW3_DIR}" -DSWITCH="${buildswitch}" "${MAKE_OPT}"
rc=$?
if (( rc != 0 )); then
  echo "Fatal error in cmake."
  exit "${rc}"
fi
make -j "${BUILD_JOBS:-8}"
rc=$?
if (( rc != 0 )); then
  echo "Fatal error in make."
  exit "${rc}"
fi
make install
if (( rc != 0 )); then
  echo "Fatal error in make install."
  exit "${rc}"
fi

# Copy to top-level exe directory
for prog in ${prep_exes} ${post_exes}; do
  cp "${path_build}/install/bin/${prog}" "${finalexecdir}/"
  rc=$?
  if (( rc != 0 )); then
    echo "FATAL: Unable to copy ${path_build}/${prog} to ${finalexecdir} (Error code ${rc})"
    exit "${rc}"
  fi
done

#clean-up build directory:
echo "executables are in ${finalexecdir}"
echo "cleaning up ${path_build}"
rm -rf "${path_build}"

exit 0
