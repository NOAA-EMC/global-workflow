#!/bin/bash
set -ex

#####################################################################
# Setup the reletive paths to scripts and source preamble for logging 
#####################################################################
pwd="$( cd "$( dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd )"

#####################################################################
#  Usage and arguments for specfifying cloned directgory
#####################################################################
usage() {
  set +x
  echo
  echo "Usage: $0 -d <directory> -o <output> -h"
  echo
  echo "  -d  Run build and ctest for clone in <directory>"
  echo "  -o  Path to output message detailing results of CI tests"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

################################################################
while getopts "d:o:h" opt; do
  case ${opt} in
    d)
      repodir=${OPTARG}
      ;;
    o)
      outfile=${OPTARG}
      ;;
    h|\?|:)
      usage
      ;;
    *)
      exit
     ;; 
  esac
done

####################################################################
# start output file
current_date=$(date)
the_hostname=$(hostname)
echo "Automated global-workflow Testing Results:" > "${outfile}"
echo "Machine: ${TARGET}" >> "${outfile}"
echo '```' >> "${outfile}"
echo "Start: ${current_date} on ${the_hostname}" >> "${outfile}"
echo "---------------------------------------------------" >> "${outfile}"
######################################################################
# run build script

# run build script
cd "${repodir}/sorc"
export BUILD_JOBS=8
rm -rf log.build
./checkout.sh -g -c
# build full cycle
./build_all.sh -g &>> log.build

# Validations

check_status=true

build_status=$?
if [[ ${build_status} -eq 0 ]]; then
  echo "Build:                                 *SUCCESS*" >> "${outfile}"
  echo "Build: Completed at ${the_date}" >> "${outfile}"
else
  echo "Build:                                  *FAILED*" >> "${outfile}"
  echo "Build: Failed at ${the_date}" >> "${outfile}"
  echo "Build: see output at ${repodir}/log.build" >> "${outfile}"
  echo '```' >> "${outfile}"
  check_status=false
fi

./link_workflow.sh

# Creat a a test case for C90C49 Atom
export pslot=test_C96C48
export icdir=/scratch1/NCEPDEV/global/glopara/data/ICSDIR/C96C48
export NOSCRUB="${repodir}"
export PTMP="${repodir}"
export SAVE="${NOSCRUB}"

mkdir -p "${repodir}/RUNTEST/expdir"
mkdir -p "${repodir}/RUNTEST/DATAROOT"

source "${repodir}/ci/expt_functions.sh"
setup_cold_96_00z "{pslot}"

xmlfile="${repodir}/RUNTEST/expdir/${pslot}/${pslot}.xml"
if [[ -f ${xmlfile} ]]; then
  echo "CREATE EXP:  created Rocoto XML file in experment directory *SUCCESS*" >> "${outfile}"
else
  echo "CREATE EXP:  created Rocoto XML file in experment directory *FAILED*" >> "${outfile}"
  check_status=false
fi

check_ROTDIR=$(check_xml_ROTDIR "${xmlfile}")
if [[ "${check_ROTDIR}" ]]; then
 echo "ROTDIR path in XML is valid *SUCCESS*" >> "${outfile}"
else
 echo "ROTDIR path in XML is valid *FAILED*" >> "${outfile}"
 check_status=false
fi

cd ${pwd}
echo "check/build/link and create a single test completed"
exit "${check_status}"

