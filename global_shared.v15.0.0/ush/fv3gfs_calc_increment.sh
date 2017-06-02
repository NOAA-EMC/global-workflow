#!/bin/sh

################################################################################
# UNIX Script Documentation Block
# Script name:         fv3gfs_calc_increment.sh
# Script description:  Calculate increment from NEMSIO anl-bkg files and write out
#                      FV3 netCDF increment file for model to read during warm start
#
# $Id$
#
# Author:   Rahul Mahajan      Org: NCEP/EMC       Date: 2017-03-02
# Abstract: calc_increment.fd provided by Jeffrey.S.Whitaker OAR/ESRL
#
# Script history log:
# 2017-03-02  Rahul Mahajan
#
# Attributes:
#   Language: Portable Operating System Interface (POSIX) Shell
#   Machine: WCOSS-CRAY, Theia
#
################################################################################

#  Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = YES ] ; then
    echo $(date) EXECUTING $0 $* >&2
    set -x
fi

#-------------------------------------------------------
# Directories and paths
pwd=$(pwd)
NWPROD=${NWPROD:-$pwd}
DATA=${DATA:-$pwd}
BASE_GSM=${BASE_GSM:-$NWPROD}
CALC_INCREMENT_EXEC=${CALC_INCREMENT_EXEC:-$BASE_GSM/exec/calc_increment.x}

ATMGES=${1:-${ATMGES:-"atmf006"}}
ATMANL=${2:-${ATMANL:-"atmanl"}}
ATMINC=${3:-${ATMINC:-"atminc"}}
DEBUG=${CALC_INCREMENT_DEBUG:-".true."}

APRUN=${APRUN_CALC_INCREMENT:-${APRUN:-""}}
NTHREADS=${NTHREADS_CALC_INCREMENT:-${NTHREADS:-1}}

#-------------------------------------------------------
# IO specific parameters and error traps
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}

#-------------------------------------------------------
# Go to the working directory
cd $DATA

#-------------------------------------------------------
# Create namelist
rm -f calc-increment.input
cat > calc-increment.input << EOF
&share
  debug=$DEBUG,
  analysis_filename="$ATMANL",
  firstguess_filename="$ATMGES",
  increment_filename="$ATMINC"
/
EOF

#------------------------------------------------------------------
export OMP_NUM_THREADS=$NTHREADS
$APRUN $CALC_INCREMENT_EXEC

export ERR=$?
export err=$ERR
$ERRSCRIPT || exit $err

rm -f calc-increment.input

#------------------------------------------------------------------
set +x
if [ $VERBOSE = "YES" ] ; then
    echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
