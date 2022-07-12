#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_atmos_chgres_forenkf.sh
# Script description:  Runs chgres on full-resolution forecast for EnKF recentering
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2020-06-08
#
# Abstract: This script runs chgres on full-resolution forecast for later
#           use in the EnKF recentering step
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
export NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

# Dependent Scripts and Executables
export APRUN_CHGRES=${APRUN_CHGRES:-${APRUN:-""}}
export CHGRESNCEXEC=${CHGRESNCEXEC:-$HOMEgfs/exec/enkf_chgres_recenter_nc.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
APRUNCFP=${APRUNCFP:-""}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}

# level info file
SIGLEVEL=${SIGLEVEL:-${FIXgsm}/global_hyblev.l${LEVS}.txt}

# forecast files
APREFIX=${APREFIX:-""}
ASUFFIX=${ASUFFIX:-$SUFFIX}
# at full resolution
ATMF03=${ATMF03:-${COMOUT}/${APREFIX}atmf003${ASUFFIX}}
ATMF04=${ATMF04:-${COMOUT}/${APREFIX}atmf004${ASUFFIX}}
ATMF05=${ATMF05:-${COMOUT}/${APREFIX}atmf005${ASUFFIX}}
ATMF06=${ATMF06:-${COMOUT}/${APREFIX}atmf006${ASUFFIX}}
ATMF07=${ATMF07:-${COMOUT}/${APREFIX}atmf007${ASUFFIX}}
ATMF08=${ATMF08:-${COMOUT}/${APREFIX}atmf008${ASUFFIX}}
ATMF09=${ATMF09:-${COMOUT}/${APREFIX}atmf009${ASUFFIX}}
# at ensemble resolution
ATMF03ENS=${ATMF03ENS:-${COMOUT}/${APREFIX}atmf003.ensres${ASUFFIX}}
ATMF04ENS=${ATMF04ENS:-${COMOUT}/${APREFIX}atmf004.ensres${ASUFFIX}}
ATMF05ENS=${ATMF05ENS:-${COMOUT}/${APREFIX}atmf005.ensres${ASUFFIX}}
ATMF06ENS=${ATMF06ENS:-${COMOUT}/${APREFIX}atmf006.ensres${ASUFFIX}}
ATMF07ENS=${ATMF07ENS:-${COMOUT}/${APREFIX}atmf007.ensres${ASUFFIX}}
ATMF08ENS=${ATMF08ENS:-${COMOUT}/${APREFIX}atmf008.ensres${ASUFFIX}}
ATMF09ENS=${ATMF09ENS:-${COMOUT}/${APREFIX}atmf009.ensres${ASUFFIX}}
ATMFCST_ENSRES=${ATMFCST_ENSRES:-${COMOUT_ENS}/mem001/${APREFIX}atmf006${ASUFFIX}}

# Set script / GSI control parameters
DOHYBVAR=${DOHYBVAR:-"NO"}
lrun_subdirs=${lrun_subdirs:-".true."}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""
if [ $CFP_MP = "YES" ]; then
    nm=0
fi
if [ $DOHYBVAR = "YES" ]; then
   l_hyb_ens=.true.
   export l4densvar=${l4densvar:-".false."}
   export lwrite4danl=${lwrite4danl:-".false."}
else
   echo "DOHYBVAR != YES, this script will exit without regridding deterministic forecast"
   exit 0
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

##############################################################
# get resolution information
LONB_ENKF=${LONB_ENKF:-$($NCLEN $ATMFCST_ENSRES grid_xt)} # get LONB_ENKF
LATB_ENKF=${LATB_ENKF:-$($NCLEN $ATMFCST_ENSRES grid_yt)} # get LATB_ENFK
LEVS_ENKF=${LEVS_ENKF:-$($NCLEN $ATMFCST_ENSRES pfull)} # get LATB_ENFK

##############################################################
# If analysis increment is written by GSI, regrid forecasts to increment resolution
if [ $DO_CALC_ANALYSIS == "YES" ]; then
   $NLN $ATMF06 fcst.06
   $NLN $ATMF06ENS fcst.ensres.06
   $NLN $ATMFCST_ENSRES atmens_fcst
   if [ $DOHYBVAR = "YES" -a $l4densvar = ".true." -a $lwrite4danl = ".true." ]; then
      $NLN $ATMF03     fcst.03
      $NLN $ATMF03ENS  fcst.ensres.03
      $NLN $ATMF04     fcst.04
      $NLN $ATMF04ENS  fcst.ensres.04
      $NLN $ATMF05     fcst.05
      $NLN $ATMF05ENS  fcst.ensres.05
      $NLN $ATMF07     fcst.07
      $NLN $ATMF07ENS  fcst.ensres.07
      $NLN $ATMF08     fcst.08
      $NLN $ATMF08ENS  fcst.ensres.08
      $NLN $ATMF09     fcst.09
      $NLN $ATMF09ENS  fcst.ensres.09
   fi
   export OMP_NUM_THREADS=$NTHREADS_CHGRES
   SIGLEVEL=${SIGLEVEL:-${FIXgsm}/global_hyblev.l${LEVS_ENKF}.txt}

   if [ $USE_CFP = "YES" ]; then
      [[ -f $DATA/mp_chgres.sh ]] && rm $DATA/mp_chgres.sh
   fi

   nfhrs=`echo $IAUFHRS_ENKF | sed 's/,/ /g'`
   for FHR in $nfhrs; do
     echo "Regridding deterministic forecast for forecast hour $FHR"
     rm -f chgres_nc_gauss0$FHR.nml
cat > chgres_nc_gauss0$FHR.nml << EOF
&chgres_setup
i_output=$LONB_ENKF
j_output=$LATB_ENKF
input_file="fcst.0$FHR"
output_file="fcst.ensres.0$FHR"
terrain_file="atmens_fcst"
ref_file="atmens_fcst"
/
EOF
     if [ $USE_CFP = "YES" ]; then
          echo "$nm $APRUN_CHGRES $CHGRESNCEXEC chgres_nc_gauss0$FHR.nml" | tee -a $DATA/mp_chgres.sh
          if [ ${CFP_MP:-"NO"} = "YES" ]; then
              nm=$((nm+1))
          fi
     else

         export pgm=$CHGRESNCEXEC
         . prep_step

	 $APRUN_CHGRES $CHGRESNCEXEC chgres_nc_gauss0$FHR.nml
         export err=$?; err_chk
     fi
   done

   if [ $USE_CFP = "YES" ]; then
      chmod 755 $DATA/mp_chgres.sh
      ncmd=$(cat $DATA/mp_chgres.sh | wc -l)
      if [ $ncmd -gt 0 ]; then
         ncmd_max=$((ncmd < npe_node_max ? ncmd : npe_node_max))
         APRUNCFP_CHGRES=$(eval echo $APRUNCFP)

         export pgm=$CHGRESNCEXEC
         . prep_step

         $APRUNCFP_CHGRES $DATA/mp_chgres.sh
         export err=$?; err_chk
      fi
   fi

else
   echo "DO_CALC_ANALYSIS != YES, doing nothing"
fi


################################################################################
# Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA

set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
