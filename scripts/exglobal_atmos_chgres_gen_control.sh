#! /usr/bin/env bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_chgres_gen_control.sh
# Script description:  Runs chgres on changing resolution of GEFS stage ic control member
################################################################################

#  Directories.
pwd=$(pwd)

# Base variables
GDUMP=${GDUMP:-"gdas"}

# Derived base variables
# shellcheck disable=SC2153
GDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${assim_freq} hours")
BDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - 3 hours")
bPDY=${BDATE:0:8}
bcyc=${BDATE:8:2}

# Utilities
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-${USHgfs}/getncdimlen}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6,"}

# Dependent Scripts and Executables
export APRUN_CHGRES=${APRUN_CHGRES:-${APRUN:-""}}
export CHGRESNCEXEC=${CHGRESNCEXEC:-${EXECgfs}/enkf_chgres_recenter_nc.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-1}
APRUNCFP=${APRUNCFP:-""}

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}

# level info file
SIGLEVEL=${SIGLEVEL:-${FIXgfs}/am/global_hyblev.l${LEVS}.txt}

# forecast files
APREFIX=${APREFIX:-""}
APREFIX_ENS=${APREFIX_ENS:-""}
# at full resolution
ATMF03=${ATMF03:-${COMIN_ATMOS_HISTORY}/${APREFIX}atmf003.nc}
# at ensemble resolution
ATMF03ENS=${ATMF03ENS:-${COMOUT_ATMOS_HISTORY}/${APREFIX}atmf003.ensres.nc}

# Set script / GSI control parameters
DOHYBVAR=${DOHYBVAR:-"NO"}
lrun_subdirs=${lrun_subdirs:-".true."}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""

################################################################################
# get resolution information
LONB_ENKF=${LONB_ENKF:-$($NCLEN $ATMFCST_ENSRES grid_xt)} # get LONB_ENKF
LATB_ENKF=${LATB_ENKF:-$($NCLEN $ATMFCST_ENSRES grid_yt)} # get LATB_ENFK
LEVS_ENKF=${LEVS_ENKF:-$($NCLEN $ATMFCST_ENSRES pfull)} # get LATB_ENFK
##############################################################
# If analysis increment is written by GSI, regrid forecasts to increment resolution
if [ $DO_CALC_ANALYSIS == "YES" ]; then
   $NLN $ATMFCST_ENSRES atmens_fcst
   if [ $DOHYBVAR = "YES" -a $l4densvar = ".true." -a $lwrite4danl = ".true." ]; then
      $NLN $ATMF03     fcst.03
      $NLN $ATMF03ENS  fcst.ensres.03
   fi
   export OMP_NUM_THREADS=$NTHREADS_CHGRES
   SIGLEVEL=${SIGLEVEL:-${FIXgfs}/am/global_hyblev.l${LEVS_ENKF}.txt}

   if [[ "${USE_CFP}" == "YES" ]]; then
       rm -f "${DATA}/mp_chgres.sh"
   fi

   nfhrs=$(echo "${IAUFHRS_ENKF}" | sed 's/,/ /g')
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
     if [[ $USE_CFP == "YES" ]]; then
          echo "$nm $APRUN_CHGRES $CHGRESNCEXEC chgres_nc_gauss0$FHR.nml" | tee -a $DATA/mp_chgres.sh
          if [[ ${CFP_MP:-"NO"} = "YES" ]]; then
              nm=$((nm+1))
          fi
     else

         export pgm=$CHGRESNCEXEC
         . prep_step

         ${APRUN_CHGRES} "${CHGRESNCEXEC}" "chgres_nc_gauss0${FHR}.nml" && true
         export err=$?
         if [[ ${err} -ne 0 ]]; then
            err_exit
         fi
     fi
   done

   if [[ ${USE_CFP} == "YES" ]]; then
      chmod 755 ${DATA}/mp_chgres.sh
      ncmd=$(wc -l < "${DATA}/mp_chgres.sh")
      if [[ ${ncmd} -gt 0 ]]; then
         ncmd_max=$((ncmd < max_tasks_per_node ? ncmd : max_tasks_per_node))
         APRUNCFP_CHGRES=$(eval echo "${APRUNCFP}")

         export pgm=${CHGRESNCEXEC}
         source prep_step

         ${APRUNCFP_CHGRES} "${DATA}/mp_chgres.sh" && true
         export err=$?
         if [[ ${err} -ne 0 ]]; then
           err_exit
         fi
      fi
   fi

else
   echo "DO_CALC_ANALYSIS != YES, doing nothing"
fi


################################################################################
# Postprocessing
cd $pwd

exit $err

