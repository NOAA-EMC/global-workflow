#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_fcst.sh
# Script description:  Run ensemble forecasts
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script runs ensemble forecasts serially one-after-another
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: WCOSS-Cray/Theia
#
####
################################################################################

# Set environment.
export VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

# Directories.
pwd=$(pwd)
export FIX_DIR=${FIX_DIR:-$HOMEgfs/fix}
export FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}

# Utilities
export NCP=${NCP:-"/bin/cp -p"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}

# Scripts.
FORECASTSH=${FORECASTSH:-$HOMEgfs/scripts/exglobal_forecast.sh}

# Enemble group, begin and end
ENSGRP=${ENSGRP:-1}
ENSBEG=${ENSBEG:-1}
ENSEND=${ENSEND:-1}

# Model builds
export FCSTEXECDIR=${FCSTEXECDIR:-$HOMEgfs/sorc/fv3gfs.fd/BUILD/bin}
export FCSTEXEC=${FCSTEXEC:-fv3gfs.x}

# Get DA specific diag table.
export PARM_FV3DIAG=${PARM_FV3DIAG:-$HOMEgfs/parm/parm_fv3diag}
export DIAG_TABLE=${DIAG_TABLE_ENKF:-${DIAG_TABLE:-$PARM_FV3DIAG/diag_table_da}}

# Cycling and forecast hour specific parameters
export CDATE=${CDATE:-"2001010100"}
export CDUMP=${CDUMP:-"gdas"}

# Re-run failed members, or entire group
RERUN_EFCSGRP=${RERUN_EFCSGRP:-"YES"}

# Recenter flag and increment file prefix
RECENTER_ENKF=${RECENTER_ENKF:-"YES"}
export PREFIX_ATMINC=${PREFIX_ATMINC:-""}

# Ops related stuff
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}
GSUFFIX=${GSUFFIX:-$SUFFIX}

################################################################################
# Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 99
DATATOP=$DATA

################################################################################
# Set output data
cymd=$(echo $CDATE | cut -c1-8)
chh=$(echo  $CDATE | cut -c9-10)
EFCSGRP=$COMOUT/efcs.grp${ENSGRP}
if [ -f $EFCSGRP ]; then
   if [ $RERUN_EFCSGRP = "YES" ]; then
      rm -f $EFCSGRP
   else
      echo "RERUN_EFCSGRP = $RERUN_EFCSGRP, will re-run FAILED members only!"
      $NMV $EFCSGRP ${EFCSGRP}.fail
   fi
fi

################################################################################
# Set namelist/model config options common to all members once

# There are many many model namelist options
# Some are resolution (CASE) dependent, some depend on the model configuration
# and will need to be added here before $FORECASTSH is called
# For now assume that
# 1. the ensemble and the deterministic are same resolution
# 2. the ensemble runs with the same configuration as the deterministic

# Model config option for Ensemble
export TYPE=${TYPE_ENKF:-${TYPE:-nh}}                  # choices:  nh, hydro
export MONO=${MONO_ENKF:-${MONO:-non-mono}}            # choices:  mono, non-mono

# fv_core_nml
export CASE=${CASE_ENKF:-${CASE:-C768}}
export layout_x=${layout_x_ENKF:-${layout_x:-8}}
export layout_y=${layout_y_ENKF:-${layout_y:-16}}
export LEVS=${LEVS_ENKF:-${LEVS:-64}}

# nggps_diag_nml
export FHOUT=${FHOUT_ENKF:-3}

# model_configure
export DELTIM=${DELTIM_ENKF:-${DELTIM:-225}}
export FHMAX=${FHMAX_ENKF:-9}
export restart_interval=${restart_interval_ENKF:-${restart_interval:-6}}

# gfs_physics_nml
export FHSWR=${FHSWR_ENKF:-${FHSWR:-3600.}}
export FHLWR=${FHLWR_ENKF:-${FHLWR:-3600.}}
export IEMS=${IEMS_ENKF:-${IEMS:-1}}
export ISOL=${ISOL_ENKF:-${ISOL:-2}}
export IAER=${IAER_ENKF:-${IAER:-111}}
export ICO2=${ICO2_ENKF:-${ICO2:-2}}
export cdmbgwd=${cdmbgwd_ENKF:-${cdmbgwd:-"3.5,0.25"}}
export dspheat=${dspheat_ENKF:-${dspheat:-".true."}}
export shal_cnv=${shal_cnv_ENKF:-${shal_cnv:-".true."}}
export FHZER=${FHZER_ENKF:-${FHZER:-6}}
export FHCYC=${FHCYC_ENKF:-${FHCYC:-6}}

# Set PREFIX_ATMINC to r when recentering on
if [ $RECENTER_ENKF = "YES" ]; then
   export PREFIX_ATMINC="r"
fi

# APRUN for different executables
export APRUN_FV3=${APRUN_FV3:-${APRUN:-""}}
export NTHREADS_FV3=${NTHREADS_FV3:-${NTHREADS:-1}}

################################################################################
# Run forecast for ensemble member
rc=0
for imem in $(seq $ENSBEG $ENSEND); do

   cd $DATATOP

   cmem=$(printf %03i $imem)
   memchar="mem$cmem"

   echo "Processing MEMBER: $cmem"

   ra=0

   skip_mem="NO"
   if [ -f ${EFCSGRP}.fail ]; then
      memstat=$(cat ${EFCSGRP}.fail | grep "MEMBER $cmem" | grep "PASS" | wc -l)
      [[ $memstat -eq 1 ]] && skip_mem="YES"
   fi

   if [ $skip_mem = "NO" ]; then

      ra=0

      export MEMBER=$imem
      export DATA=$DATATOP/$memchar
      if [ -d $DATA ]; then rm -rf $DATA; fi
      mkdir -p $DATA
      $FORECASTSH
      ra=$?

      # Notify a member forecast failed and abort
      if [ $ra -ne 0 ]; then
         err_exit "FATAL ERROR:  forecast of member $cmem FAILED.  Aborting job"
      fi

      ((rc+=ra))

   fi

   if [ $SENDDBN = YES ]; then
     fhr=$FHOUT
     while [ $fhr -le $FHMAX ]; do
       FH3=$(printf %03i $fhr)
       if [ $(expr $fhr % 3) -eq 0 ]; then
         $DBNROOT/bin/dbn_alert MODEL GFS_ENKF $job $COMOUT/$memchar/${CDUMP}.t${cyc}z.sfcf${FH3}${GSUFFIX}
       fi
       fhr=$((fhr+FHOUT))
     done
   fi

   cd $DATATOP

   if [ -s $EFCSGRP ]; then
       $NCP $EFCSGRP log_old
   fi
   [[ -f log ]] && rm log
   [[ -f log_new ]] && rm log_new
   if [ $ra -ne 0 ]; then
      echo "MEMBER $cmem : FAIL" > log
   else
      echo "MEMBER $cmem : PASS" > log
   fi
   if [ -s log_old ] ; then
       cat log_old log > log_new
   else
       cat log > log_new
   fi
   $NCP log_new $EFCSGRP

done

################################################################################
# Echo status of ensemble group
cd $DATATOP
echo "Status of ensemble members in group $ENSGRP:"
cat $EFCSGRP
[[ -f ${EFCSGRP}.fail ]] && rm ${EFCSGRP}.fail

################################################################################
# If any members failed, error out
export err=$rc; err_chk

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATATOP
set +x
if [ $VERBOSE = "YES" ] ; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
