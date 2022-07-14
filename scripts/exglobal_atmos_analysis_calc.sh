#!/bin/bash
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
#   Machine: WCOSS-Dell / Hera
#
################################################################################

#  Set environment.
export VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

#  Directories.
pwd=$(pwd)
export FIXgsm=${FIXgsm:-$HOMEgfs/fix/fix_am}

# Base variables
CDATE=${CDATE:-"2001010100"}
CDUMP=${CDUMP:-"gdas"}
GDUMP=${GDUMP:-"gdas"}

# Derived base variables
GDATE=$($NDATE -$assim_freq $CDATE)
BDATE=$($NDATE -3 $CDATE)
PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)
bPDY=$(echo $BDATE | cut -c1-8)
bcyc=$(echo $BDATE | cut -c9-10)

# Utilities
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NEMSIOGET=${NEMSIOGET:-${NWPROD}/exec/nemsio_get}
export NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}
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

export CALCANLEXEC=${CALCANLEXEC:-$HOMEgfs/exec/calc_analysis.x}
export CHGRESNCEXEC=${CHGRESNCEXEC:-$HOMEgfs/exec/enkf_chgres_recenter_nc.x}
export CHGRESINCEXEC=${CHGRESINCEXEC:-$HOMEgfs/exec/interp_inc.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
CALCINCPY=${CALCINCPY:-$HOMEgfs/ush/calcinc_gfs.py}
CALCANLPY=${CALCANLPY:-$HOMEgfs/ush/calcanl_gfs.py}

DOGAUSFCANL=${DOGAUSFCANL-"NO"}
GAUSFCANLSH=${GAUSFCANLSH:-$HOMEgfs/ush/gaussian_sfcanl.sh}
export GAUSFCANLEXE=${GAUSFCANLEXE:-$HOMEgfs/exec/gaussian_sfcanl.exe}
NTHREADS_GAUSFCANL=${NTHREADS_GAUSFCANL:-1}
APRUN_GAUSFCANL=${APRUN_GAUSFCANL:-${APRUN:-""}}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}

# Guess files
GPREFIX=${GPREFIX:-""}
GSUFFIX=${GSUFFIX:-$SUFFIX}
ATMG03=${ATMG03:-${COMIN_GES}/${GPREFIX}atmf003${GSUFFIX}}
ATMG04=${ATMG04:-${COMIN_GES}/${GPREFIX}atmf004${GSUFFIX}}
ATMG05=${ATMG05:-${COMIN_GES}/${GPREFIX}atmf005${GSUFFIX}}
ATMGES=${ATMGES:-${COMIN_GES}/${GPREFIX}atmf006${GSUFFIX}}
ATMG07=${ATMG07:-${COMIN_GES}/${GPREFIX}atmf007${GSUFFIX}}
ATMG08=${ATMG08:-${COMIN_GES}/${GPREFIX}atmf008${GSUFFIX}}
ATMG09=${ATMG09:-${COMIN_GES}/${GPREFIX}atmf009${GSUFFIX}}

# Analysis files
export APREFIX=${APREFIX:-""}
export ASUFFIX=${ASUFFIX:-$SUFFIX}
SFCANL=${SFCANL:-${COMOUT}/${APREFIX}sfcanl${ASUFFIX}}
DTFANL=${DTFANL:-${COMOUT}/${APREFIX}dtfanl.nc}
ATMANL=${ATMANL:-${COMOUT}/${APREFIX}atmanl${ASUFFIX}}

# Increment files
ATMINC=${ATMINC:-${COMOUT}/${APREFIX}atminc.nc}

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
   ATMA03=${ATMA03:-${COMOUT}/${APREFIX}atma003${ASUFFIX}}
   ATMI03=${ATMI03:-${COMOUT}/${APREFIX}atmi003.nc}
   ATMA04=${ATMA04:-${COMOUT}/${APREFIX}atma004${ASUFFIX}}
   ATMI04=${ATMI04:-${COMOUT}/${APREFIX}atmi004.nc}
   ATMA05=${ATMA05:-${COMOUT}/${APREFIX}atma005${ASUFFIX}}
   ATMI05=${ATMI05:-${COMOUT}/${APREFIX}atmi005.nc}
   ATMA07=${ATMA07:-${COMOUT}/${APREFIX}atma007${ASUFFIX}}
   ATMI07=${ATMI07:-${COMOUT}/${APREFIX}atmi007.nc}
   ATMA08=${ATMA08:-${COMOUT}/${APREFIX}atma008${ASUFFIX}}
   ATMI08=${ATMI08:-${COMOUT}/${APREFIX}atmi008.nc}
   ATMA09=${ATMA09:-${COMOUT}/${APREFIX}atma009${ASUFFIX}}
   ATMI09=${ATMI09:-${COMOUT}/${APREFIX}atmi009.nc}
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

echo "$CDUMP $CDATE atmanl and sfcanl done at `date`" > $COMOUT/${APREFIX}loganl.txt

################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA

set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err

