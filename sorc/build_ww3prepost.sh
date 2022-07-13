#!/bin/sh
set -x

# Default settings
APP="S2SWA"

while getopts "a:v" option; do
  case "${option}" in
    a) APP="${OPTARG}" ;;
    v) BUILD_VERBOSE="YES";;
    *)
      echo "Unrecognized option: ${1}"
      exit 1
      ;;
  esac
done


# Determine which switch to use 
if [ APP == ATMW ]; then 
  ww3switch=model/esmf/switch 
else 
  ww3switch=model/bin/switch_meshcap
fi 


# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

finalexecdir=$( pwd -P )/../exec

#Determine machine and load modules
set +x
source ./machine-setup.sh > /dev/null 2>&1

module use ../modulefiles
module load modulefile.ww3.$target
set -x

#Set WW3 directory, switch, prep and post exes 
cd ufs_model.fd/WW3
export WW3_DIR=$( pwd -P )
export SWITCHFILE="${WW3_DIR}/${ww3switch}"

# Build exes for prep jobs and post jobs:
prep_exes="ww3_grid ww3_prep ww3_prnc ww3_grid"
post_exes="ww3_outp ww3_outf ww3_outp ww3_gint ww3_ounf ww3_ounp ww3_grib"

#create build directory: 
path_build=$WW3_DIR/build_SHRD
mkdir -p $path_build
cd $path_build
echo "Forcing a SHRD build" 

echo $(cat ${SWITCHFILE}) > ${path_build}/tempswitch

sed -e "s/DIST/SHRD/g"\
    -e "s/OMPG / /g"\
    -e "s/OMPH / /g"\
    -e "s/MPIT / /g"\
    -e "s/MPI / /g"\
    -e "s/B4B / /g"\
    -e "s/PDLIB / /g"\
    -e "s/NOGRB/NCEP2/g"\
       ${path_build}/tempswitch > ${path_build}/switch
rm ${path_build}/tempswitch

echo "Switch file is $path_build/switch with switches:" 
cat $path_build/switch 

#Build executables: 
cmake $WW3_DIR -DSWITCH=$path_build/switch -DCMAKE_INSTALL_PREFIX=install 
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "Fatal error in cmake."
  exit $rc
fi
make -j 8 
rc=$?
if [[ $rc -ne 0 ]] ; then
  echo "Fatal error in make."
  exit $rc
fi
make install 
if [[ $rc -ne 0 ]] ; then
  echo "Fatal error in make install."
  exit $rc  
fi

# Copy to top-level exe directory
for prog in $prep_exes $post_exes; do
  cp $path_build/install/bin/$prog $finalexecdir/
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "FATAL: Unable to copy $path_build/$prog to $finalexecdir (Error code $rc)"
    exit $rc
  fi
done

#clean-up build directory:
echo "executables are in $finalexecdir" 
echo "cleaning up $path_build" 
rm -rf $path_build

exit 0
