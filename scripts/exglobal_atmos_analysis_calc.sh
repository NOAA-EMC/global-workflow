#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_analysis_calc.sh
# Script description:  Runs non-diagnostic file tasks after GSI analysis is performed
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2020-03-03
#
# Abstract: This script wraps up analysis-related tasks after GSI exits successfully
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

#  Set environment.

source "${USHgfs}/preamble.sh"

#  Directories.
pwd=$(pwd)

# Base variables
rCDUMP=${rCDUMP:-"gdas"}
GDUMP=${GDUMP:-"gdas"}

# Utilities
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-${USHgfs}/getncdimlen}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUNCFP=${APRUNCFP:-""}

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

# Dependent Scripts and Executables
export NTHREADS_CALCINC=${NTHREADS_CALCINC:-1}
export APRUN_CALCINC=${APRUN_CALCINC:-${APRUN:-""}}
export APRUN_CALCANL=${APRUN_CALCANL:-${APRUN:-""}}
export APRUN_CHGRES=${APRUN_CALCANL:-${APRUN:-""}}

export CALCANLEXEC=${CALCANLEXEC:-${EXECgfs}/calc_analysis.x}
export CHGRESNCEXEC=${CHGRESNCEXEC:-${EXECgfs}/enkf_chgres_recenter_nc.x}
export CHGRESINCEXEC=${CHGRESINCEXEC:-${EXECgfs}/interp_inc.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
CALCINCPY=${CALCINCPY:-${USHgfs}/calcinc_gfs.py}
CALCANLPY=${CALCANLPY:-${USHgfs}/calcanl_gfs.py}

DOGAUSFCANL=${DOGAUSFCANL-"NO"}
GAUSFCANLSH=${GAUSFCANLSH:-${USHgfs}/gaussian_sfcanl.sh}
export GAUSFCANLEXE=${GAUSFCANLEXE:-${EXECgfs}/gaussian_sfcanl.x}
NTHREADS_GAUSFCANL=${NTHREADS_GAUSFCANL:-1}
APRUN_GAUSFCANL=${APRUN_GAUSFCANL:-${APRUN:-""}}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}

# Guess files
GPREFIX=${GPREFIX:-""}
ATMG03=${ATMG03:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf003.nc}
ATMG04=${ATMG04:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf004.nc}
ATMG05=${ATMG05:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf005.nc}
ATMGES=${ATMGES:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf006.nc}
ATMG07=${ATMG07:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf007.nc}
ATMG08=${ATMG08:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf008.nc}
ATMG09=${ATMG09:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf009.nc}

# Analysis files
export APREFIX=${APREFIX:-""}
SFCANL=${SFCANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}sfcanl.nc}
DTFANL=${DTFANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}dtfanl.nc}
ATMANL=${ATMANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmanl.nc}

# Increment files
ATMINC=${ATMINC:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atminc.nc}

# Set script / GSI control parameters
DOHYBVAR=${DOHYBVAR:-"NO"}
lrun_subdirs=${lrun_subdirs:-".true."}
if [ $DOHYBVAR = "YES" ]; then
   l_hyb_ens=.true.
   export l4densvar=${l4densvar:-".false."}
   export lwrite4danl=${lwrite4danl:-".false."}
else
   l_hyb_ens=.false.
   export l4densvar=.false.
   export lwrite4danl=.false.
fi

# Set 4D-EnVar specific variables
if [ $DOHYBVAR = "YES" -a $l4densvar = ".true." -a $lwrite4danl = ".true." ]; then
   ATMA03=${ATMA03:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma003.nc}
   ATMI03=${ATMI03:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi003.nc}
   ATMA04=${ATMA04:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma004.nc}
   ATMI04=${ATMI04:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi004.nc}
   ATMA05=${ATMA05:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma005.nc}
   ATMI05=${ATMI05:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi005.nc}
   ATMA07=${ATMA07:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma007.nc}
   ATMI07=${ATMI07:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi007.nc}
   ATMA08=${ATMA08:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma008.nc}
   ATMI08=${ATMI08:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi008.nc}
   ATMA09=${ATMA09:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma009.nc}
   ATMI09=${ATMI09:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi009.nc}
fi

################################################################################
################################################################################
#  Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi

cd $DATA || exit 99

################################################################################
# Clean the run-directory
rm -rf dir.*

##############################################################
# If analysis increment is written by GSI, produce an analysis file here
if [ $DO_CALC_ANALYSIS == "YES" ]; then
   # link analysis and increment files
   $NLN $ATMANL siganl
   $NLN $ATMINC siginc.nc
   if [ $DOHYBVAR = "YES" -a $l4densvar = ".true." -a $lwrite4danl = ".true." ]; then
      $NLN $ATMA03   siga03
      $NLN $ATMI03   sigi03.nc
      $NLN $ATMA04   siga04
      $NLN $ATMI04   sigi04.nc
      $NLN $ATMA05   siga05
      $NLN $ATMI05   sigi05.nc
      $NLN $ATMA07   siga07
      $NLN $ATMI07   sigi07.nc
      $NLN $ATMA08   siga08
      $NLN $ATMI08   sigi08.nc
      $NLN $ATMA09   siga09
      $NLN $ATMI09   sigi09.nc
   fi
   # link guess files
   $NLN $ATMG03 sigf03
   $NLN $ATMGES sigf06
   $NLN $ATMG09 sigf09

   [[ -f $ATMG04 ]] && $NLN $ATMG04 sigf04
   [[ -f $ATMG05 ]] && $NLN $ATMG05 sigf05
   [[ -f $ATMG07 ]] && $NLN $ATMG07 sigf07
   [[ -f $ATMG08 ]] && $NLN $ATMG08 sigf08

   # Link hourly backgrounds (if present)
   if [ -f $ATMG04 -a -f $ATMG05 -a -f $ATMG07 -a -f $ATMG08 ]; then
      nhr_obsbin=1
   fi

   $CALCANLPY
   export err=$?; err_chk
else
   echo "Neither increment nor analysis are generated by external utils"
fi

##############################################################
# Create gaussian grid surface analysis file at middle of window
if [ $DOGAUSFCANL = "YES" ]; then
    export APRUNSFC=$APRUN_GAUSFCANL
    export OMP_NUM_THREADS_SFC=$NTHREADS_GAUSFCANL

    $GAUSFCANLSH
    export err=$?; err_chk
fi

echo "${rCDUMP} ${PDY}${cyc} atmanl and sfcanl done at $(date)" > "${COMOUT_ATMOS_ANALYSIS}/${APREFIX}loganl.txt"

################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA


exit $err

