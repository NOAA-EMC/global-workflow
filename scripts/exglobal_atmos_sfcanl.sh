#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_sfcanl.sh
# Script description:  Makes global model surface analysis files
#
# Author: Russ Treadon      Org: NCEP/EMC     Date: 2021-12-13
#
# Abstract: This script makes global model surface analysis filesk
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: WCOSS-Dell
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

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

# Surface cycle related parameters
DOGCYCLE=${DOGCYCLE:-"NO"}
CYCLESH=${CYCLESH:-$HOMEgfs/ush/global_cycle.sh}
export CYCLEXEC=${CYCLEXEC:-$HOMEgfs/exec/global_cycle}
NTHREADS_CYCLE=${NTHREADS_CYCLE:-24}
APRUN_CYCLE=${APRUN_CYCLE:-${APRUN:-""}}
export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-'-2.'}
export CYCLVARS=${CYCLVARS:-""}
export FHOUR=${FHOUR:-0}
export DELTSFC=${DELTSFC:-6}
export FIXgsm=${FIXgsm:-$HOMEgfs/fix/fix_am}
export FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}

# FV3 specific info (required for global_cycle)
export CASE=${CASE:-"C384"}
ntiles=${ntiles:-6}

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

export APRUN_CHGRES=${APRUN_CALCANL:-${APRUN:-""}}
CHGRESEXEC=${CHGRESEXEC:-$HOMEgfs/exec/enkf_chgres_recenter.x}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}
RUN_GETGES=${RUN_GETGES:-"NO"}
GETGESSH=${GETGESSH:-"getges.sh"}
export gesenvir=${gesenvir:-$envir}

# Observations
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}

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

SFCG03=${SFCG03:-${COMIN_GES}/${GPREFIX}sfcf003${GSUFFIX}}
SFCG04=${SFCG04:-${COMIN_GES}/${GPREFIX}sfcf004${GSUFFIX}}
SFCG05=${SFCG05:-${COMIN_GES}/${GPREFIX}sfcf005${GSUFFIX}}
SFCGES=${SFCGES:-${COMIN_GES}/${GPREFIX}sfcf006${GSUFFIX}}
SFCG07=${SFCG07:-${COMIN_GES}/${GPREFIX}sfcf007${GSUFFIX}}
SFCG08=${SFCG08:-${COMIN_GES}/${GPREFIX}sfcf008${GSUFFIX}}
SFCG09=${SFCG09:-${COMIN_GES}/${GPREFIX}sfcf009${GSUFFIX}}

# Analysis files
export APREFIX=${APREFIX:-""}
export ASUFFIX=${ASUFFIX:-$SUFFIX}
SFCANL=${SFCANL:-${COMOUT}/${APREFIX}sfcanl${ASUFFIX}}
DTFANL=${DTFANL:-${COMOUT}/${APREFIX}dtfanl.nc}
ATMANL=${ATMANL:-${COMOUT}/${APREFIX}atmanl${ASUFFIX}}

# Increment files
ATMINC=${ATMINC:-${COMOUT}/${APREFIX}atminc.nc}

# Set script parameters
export DOIAU=${DOIAU:-"NO"}
DO_CALC_INCREMENT=${DO_CALC_INCREMENT:-"NO"}
DO_CALC_ANALYSIS=${DO_CALC_ANALYSIS:-"NO"}

# Get header information from Guess files
if [ ${SUFFIX} = ".nc" ]; then
   LONB=${LONB:-$($NCLEN $ATMGES grid_xt)} # get LONB
   LATB=${LATB:-$($NCLEN $ATMGES grid_yt)} # get LATB
   LEVS=${LEVS:-$($NCLEN $ATMGES pfull)} # get LEVS
   JCAP=${JCAP:--9999} # there is no jcap in these files
else
   LONB=${LONB:-$($NEMSIOGET $ATMGES dimx | grep -i "dimx" | awk -F"= " '{print $2}' | awk -F" " '{print $1}')}  # 'get LONB
   LATB=${LATB:-$($NEMSIOGET $ATMGES dimy | grep -i "dimy" | awk -F"= " '{print $2}' | awk -F" " '{print $1}')}  # 'get LATB
   LEVS=${LEVS:-$($NEMSIOGET $ATMGES dimz | grep -i "dimz" | awk -F"= " '{print $2}' | awk -F" " '{print $1}')}  # 'get LEVS
   JCAP=${JCAP:-$($NEMSIOGET $ATMGES jcap | grep -i "jcap" | awk -F"= " '{print $2}' | awk -F" " '{print $1}')}  # 'get JCAP
fi
[ $JCAP -eq -9999 -a $LATB -ne -9999 ] && JCAP=$((LATB-2))
[ $LONB -eq -9999 -o $LATB -eq -9999 -o $LEVS -eq -9999 -o $JCAP -eq -9999 ] && exit -9999


# Get dimension information based on CASE
res=$(echo $CASE | cut -c2-)
JCAP_CASE=$((res*2-2))
LATB_CASE=$((res*2))
LONB_CASE=$((res*4))

# logic for netCDF I/O
if [ ${SUFFIX} = ".nc" ]; then
  # GSI namelist options to use netCDF background
  use_gfs_nemsio=".false."
  use_gfs_ncio=".true."
else
  # GSI namelist options to use NEMSIO background
  use_gfs_nemsio=".true."
  use_gfs_ncio=".false."
fi

# determine if writing or calculating increment
if [ $DO_CALC_INCREMENT = "YES" ]; then
  write_fv3_increment=".false."
else
  write_fv3_increment=".true."
  WRITE_INCR_ZERO="incvars_to_zero= $INCREMENTS_TO_ZERO,"
  WRITE_ZERO_STRAT="incvars_zero_strat= $INCVARS_ZERO_STRAT,"
  WRITE_STRAT_EFOLD="incvars_efold= $INCVARS_EFOLD,"
fi


################################################################################
#  Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi

cd $DATA || exit 99

[[ $DONST = "YES" ]] && $NLN $NSSTBF nsstbufr


##############################################################
# Required model guess files
$NLN $SFCG03 sfcf03
$NLN $SFCGES sfcf06
$NLN $SFCG09 sfcf09

[[ -f $SFCG04 ]] && $NLN $SFCG04 sfcf04
[[ -f $SFCG05 ]] && $NLN $SFCG05 sfcf05
[[ -f $SFCG07 ]] && $NLN $SFCG07 sfcf07
[[ -f $SFCG08 ]] && $NLN $SFCG08 sfcf08


##############################################################
# Output files
if [ $DONST = "YES" ]; then
   $NLN $DTFANL dtfanl
fi


##############################################################
# Update surface fields in the FV3 restart's using global_cycle
if [ $DOGCYCLE = "YES" ]; then

    mkdir -p $COMOUT/RESTART

    # Global cycle requires these files
    export FNTSFA=${FNTSFA:-$COMIN_OBS/${OPREFIX}rtgssthr.grb}
    export FNACNA=${FNACNA:-$COMIN_OBS/${OPREFIX}seaice.5min.blend.grb}
    export FNSNOA=${FNSNOA:-$COMIN_OBS/${OPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
    [[ ! -f $FNSNOA ]] && export FNSNOA="$COMIN_OBS/${OPREFIX}snogrb_t1534.3072.1536"
    FNSNOG=${FNSNOG:-$COMIN_GES_OBS/${GPREFIX}snogrb_t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}}
    [[ ! -f $FNSNOG ]] && FNSNOG="$COMIN_GES_OBS/${GPREFIX}snogrb_t1534.3072.1536"

    # Set CYCLVARS by checking grib date of current snogrb vs that of prev cycle
    if [ $RUN_GETGES = "YES" ]; then
        snoprv=$($GETGESSH -q -t snogrb_$JCAP_CASE -e $gesenvir -n $GDUMP -v $GDATE)
    else
        snoprv=${snoprv:-$FNSNOG}
    fi

    if [ $($WGRIB -4yr $FNSNOA 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') -le \
         $($WGRIB -4yr $snoprv 2>/dev/null | grep -i snowc | awk -F: '{print $3}' | awk -F= '{print $2}') ] ; then
        export FNSNOA=" "
        export CYCLVARS="FSNOL=99999.,FSNOS=99999.,"
    else
        export SNOW_NUDGE_COEFF=${SNOW_NUDGE_COEFF:-0.}
        export CYCLVARS="FSNOL=${SNOW_NUDGE_COEFF},$CYCLVARS"
    fi

    if [ $DONST = "YES" ]; then
        export NST_ANL=".true."
        export GSI_FILE=${GSI_FILE:-$COMOUT/${APREFIX}dtfanl.nc}
    else
        export NST_ANL=".false."
        export GSI_FILE="NULL"
    fi

    if [ $DOIAU = "YES" ]; then
        # update surface restarts at the beginning of the window, if IAU
        # For now assume/hold dtfanl.nc valid at beginning of window
        for n in $(seq 1 $ntiles); do
            $NLN $COMIN_GES/RESTART/$bPDY.${bcyc}0000.sfc_data.tile${n}.nc $DATA/fnbgsi.00$n
            $NLN $COMOUT/RESTART/$bPDY.${bcyc}0000.sfcanl_data.tile${n}.nc $DATA/fnbgso.00$n
            $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc                    $DATA/fngrid.00$n
            $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc                $DATA/fnorog.00$n
        done

        export APRUNCY=$APRUN_CYCLE
        export OMP_NUM_THREADS_CY=$NTHREADS_CYCLE
        export MAX_TASKS_CY=$ntiles

        $CYCLESH
        export err=$?; err_chk
    fi
    # update surface restarts at middle of window
    for n in $(seq 1 $ntiles); do
        $NLN $COMIN_GES/RESTART/$PDY.${cyc}0000.sfc_data.tile${n}.nc $DATA/fnbgsi.00$n
        $NLN $COMOUT/RESTART/$PDY.${cyc}0000.sfcanl_data.tile${n}.nc $DATA/fnbgso.00$n
        $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc                  $DATA/fngrid.00$n
        $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc              $DATA/fnorog.00$n
    done

    export APRUNCY=$APRUN_CYCLE
    export OMP_NUM_THREADS_CY=$NTHREADS_CYCLE
    export MAX_TASKS_CY=$ntiles

    $CYCLESH
    export err=$?; err_chk
fi


################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA


################################################################################
set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err

################################################################################
