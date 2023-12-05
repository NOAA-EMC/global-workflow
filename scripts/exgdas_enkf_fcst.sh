#! /usr/bin/env bash

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
#
####
################################################################################

source "${HOMEgfs}/ush/preamble.sh"

# Enemble group, begin and end
ENSGRP=${ENSGRP:-1}
ENSBEG=${ENSBEG:-1}
ENSEND=${ENSEND:-1}

# Re-run failed members, or entire group
RERUN_EFCSGRP=${RERUN_EFCSGRP:-"YES"}

# Recenter flag and increment file prefix
RECENTER_ENKF=${RECENTER_ENKF:-"YES"}
export PREFIX_ATMINC=${PREFIX_ATMINC:-""}

################################################################################
# Preprocessing
cd "${DATA}" || exit 99
DATATOP=${DATA}

################################################################################
# Set output data
EFCSGRP="${COM_TOP}/efcs.grp${ENSGRP}"
if [[ -f ${EFCSGRP} ]]; then
   if [[ ${RERUN_EFCSGRP} = "YES" ]]; then
      rm -f "${EFCSGRP}"
   else
      echo "RERUN_EFCSGRP = ${RERUN_EFCSGRP}, will re-run FAILED members only!"
      ${NMV} "${EFCSGRP}" "${EFCSGRP}.fail"
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
export CASE=${CASE_ENS:-${CASE:-C768}}
export layout_x=${layout_x_ENKF:-${layout_x:-8}}
export layout_y=${layout_y_ENKF:-${layout_y:-16}}
export LEVS=${LEVS_ENKF:-${LEVS:-64}}

# nggps_diag_nml
export FHOUT=${FHOUT_ENKF:-3}
if [[ ${RUN} == "enkfgfs" ]]; then
    export FHOUT=${FHOUT_ENKF_GFS:-${FHOUT_ENKF:${FHOUT:-3}}}
fi
# model_configure
export DELTIM=${DELTIM_ENKF:-${DELTIM:-225}}
export FHMAX=${FHMAX_ENKF:-9}
if [[ ${RUN} == "enkfgfs" ]]; then
   export FHMAX=${FHMAX_ENKF_GFS:-${FHMAX_ENKF:-${FHMAX}}}
fi

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
if [[ ${RECENTER_ENKF} = "YES" ]]; then
   export PREFIX_ATMINC="r"
fi

# Ignore possible spelling error (nothing is misspelled)
# shellcheck disable=SC2153
GDATE=$(${NDATE} -"${assim_freq}" "${PDY}${cyc}")
declare -x gPDY="${GDATE:0:8}"
declare -x gcyc="${GDATE:8:2}"

################################################################################
# Run forecast for ensemble member
rc=0
for imem in $(seq "${ENSBEG}" "${ENSEND}"); do

   cd "${DATATOP}"

   ENSMEM=$(printf %03i "${imem}")
   export ENSMEM
   memchar="mem${ENSMEM}"

   echo "Processing MEMBER: ${ENSMEM}"

   ra=0

   skip_mem="NO"
   if [[ -f ${EFCSGRP}.fail ]]; then
      set +e
      memstat=$(grep "MEMBER ${ENSMEM}" "${EFCSGRP}.fail" | grep -c "PASS")
      set_strict
      [[ ${memstat} -eq 1 ]] && skip_mem="YES"
   fi

   # Construct COM variables from templates (see config.com)
   # Can't make these read-only because we are looping over members
   MEMDIR="${memchar}" YMD=${PDY} HH=${cyc} generate_com -x COM_ATMOS_RESTART COM_ATMOS_INPUT COM_ATMOS_ANALYSIS \
     COM_ATMOS_HISTORY COM_ATMOS_MASTER COM_CONF

   MEMDIR="${memchar}" YMD="${gPDY}" HH="${gcyc}" generate_com -x COM_ATMOS_RESTART_PREV:COM_ATMOS_RESTART_TMPL

   if [[ ${DO_WAVE} == "YES" ]]; then
     MEMDIR="${memchar}" YMD=${PDY} HH=${cyc} generate_com -x COM_WAVE_RESTART COM_WAVE_PREP COM_WAVE_HISTORY
     MEMDIR="${memchar}" YMD="${gPDY}" HH="${gcyc}" generate_com -x COM_WAVE_RESTART_PREV:COM_WAVE_RESTART_TMPL
   fi

   if [[ ${DO_OCN} == "YES" ]]; then
     MEMDIR="${memchar}" YMD=${PDY} HH=${cyc} generate_com -x COM_MED_RESTART COM_OCEAN_RESTART \
       COM_OCEAN_INPUT COM_OCEAN_HISTORY COM_OCEAN_ANALYSIS
     MEMDIR="${memchar}" YMD="${gPDY}" HH="${gcyc}" generate_com -x COM_OCEAN_RESTART_PREV:COM_OCEAN_RESTART_TMPL
   fi

   if [[ ${DO_ICE} == "YES" ]]; then
     MEMDIR="${memchar}" YMD=${PDY} HH=${cyc} generate_com -x COM_ICE_HISTORY COM_ICE_INPUT COM_ICE_RESTART
     MEMDIR="${memchar}" YMD="${gPDY}" HH="${gcyc}" generate_com -x COM_ICE_RESTART_PREV:COM_ICE_RESTART_TMPL
   fi

   if [[ ${DO_AERO} == "YES" ]]; then
     MEMDIR="${memchar}" YMD=${PDY} HH=${cyc} generate_com -x COM_CHEM_HISTORY
   fi


   if [[ ${skip_mem} = "NO" ]]; then

      ra=0

      export MEMBER=${imem}
      export DATA="${DATATOP}/${memchar}"
      if [[ -d ${DATA} ]]; then rm -rf "${DATA}"; fi
      mkdir -p "${DATA}"
      ${FORECASTSH}
      ra=$?

      # Notify a member forecast failed and abort
      if [[ ${ra} -ne 0 ]]; then
         err_exit "FATAL ERROR:  forecast of member ${ENSMEM} FAILED.  Aborting job"
      fi

      rc=$((rc+ra))

   fi

   if [[ ${SENDDBN} = YES ]]; then
     fhr=${FHOUT}
     while [[ ${fhr} -le ${FHMAX} ]]; do
       FH3=$(printf %03i "${fhr}")
       if (( fhr % 3 == 0 )); then
         "${DBNROOT}/bin/dbn_alert" MODEL GFS_ENKF "${job}" "${COM_ATMOS_HISTORY}/${RUN}.t${cyc}z.sfcf${FH3}.nc"
       fi
       fhr=$((fhr+FHOUT))
     done
   fi

   cd "${DATATOP}"

   if [[ -s ${EFCSGRP} ]]; then
       ${NCP} "${EFCSGRP}" log_old
   fi
   [[ -f log ]] && rm log
   [[ -f log_new ]] && rm log_new
   if [[ ${ra} -ne 0 ]]; then
      echo "MEMBER ${ENSMEM} : FAIL" > log
   else
      echo "MEMBER ${ENSMEM} : PASS" > log
   fi
   if [[ -s log_old ]] ; then
       cat log_old log > log_new
   else
       cat log > log_new
   fi
   ${NCP} log_new "${EFCSGRP}"

done

################################################################################
# Echo status of ensemble group
cd "${DATATOP}"
echo "Status of ensemble members in group ${ENSGRP}:"
cat "${EFCSGRP}"
[[ -f ${EFCSGRP}.fail ]] && rm "${EFCSGRP}".fail

################################################################################
# If any members failed, error out
export err=${rc}; err_chk

################################################################################
#  Postprocessing

exit "${err}"
