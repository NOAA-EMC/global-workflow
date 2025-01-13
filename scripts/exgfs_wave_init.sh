#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exwave_init.sh
# Script description:  Creates model definition files for WW3
#
# Author:   Jose-Henrique Alves Org: NCEP/EMC      Date: 2019-04-20
# Abstract: This script is the init config for the global multi_grid wave model.
#           It creates model definition files with all configurations of spatial
#           and spectral grids, as well as physics parameters and time steps.
#
# Script history log:
# 2019-05-06  J-Henrique Alves First Version.
# 2019-11-02  J-Henrique Alves Ported to global-workflow.
# 2020-06-10  J-Henrique Alves Ported to R&D machine Hera
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (BASH) shell
#
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  err=0

  cd $DATA

  set +x
  echo ' '
  echo '                      ********************************'
  echo '                      *** MWW3 INIT CONFIG  SCRIPT ***'
  echo '                      ********************************'
  echo '                          Initial configuration script'
  echo "                       Model identifier : ${RUN}wave"
  echo ' '
  echo "Starting at : $(date)"
  echo ' '
  set_trace

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
  if [ -z ${NTASKS} ]
  then
    echo "FATAL ERROR: requires NTASKS to be set "
    err=1; export err;${errchk}
  fi

  set +x
  echo ' '
  echo " Script set to run with $NTASKS tasks "
  echo ' '
  set_trace


# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  set +x
  echo 'Preparing input files :'
  echo '-----------------------'
  echo ' '
  set_trace

# 1.a Model definition files

  nmoddef=0

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# Eliminate duplicate grids
  array=($WAVECUR_FID $WAVEICE_FID $WAVEWND_FID $waveuoutpGRD $waveGRD $wavepostGRD $waveinterpGRD)
  grdALL=$(printf "%s\n" "${array[@]}" | sort -u | tr '\n' ' ')

  for grdID in ${grdALL}; do
    if [[ -f "${COMOUT_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" ]]; then
      set +x
      echo " Mod def file for ${grdID} found in ${COMOUT_WAVE_PREP}. copying ...."
      set_trace
      cp "${COMOUT_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" "mod_def.${grdID}"

    else
      set +x
      echo " Mod def file for ${grdID} not found in ${COMOUT_WAVE_PREP}. Setting up to generate ..."
      echo ' '
      set_trace
      if [ -f ${FIXgfs}/wave/ww3_grid.inp.$grdID ]
      then
        cp ${FIXgfs}/wave/ww3_grid.inp.$grdID ww3_grid.inp.$grdID
      fi

      if [ -f ww3_grid.inp.$grdID ]
      then
        set +x
        echo ' '
        echo "   ww3_grid.inp.$grdID copied (${FIXgfs}/wave/ww3_grid.inp.$grdID)."
        echo ' '
        set_trace
      else
        set +x
        echo ' '
        echo '*********************************************************** '
        echo '*** FATAL ERROR : NO INP FILE FOR MODEL DEFINITION FILE *** '
        echo '*********************************************************** '
        echo "                                grdID = $grdID"
        echo ' '
        set_trace
        err=2;export err;${errchk}
      fi


      if [ -f ${FIXgfs}/wave/${grdID}.msh ]
      then
        cp "${FIXgfs}/wave/${grdID}.msh" "${grdID}.msh"
      fi
      #TO DO: how do we say "it's unstructured, and therefore need to have error check here" 

      if [ ${CFP_MP:-"NO"} = "YES" ]; then
        echo "$nmoddef ${USHgfs}/wave_grid_moddef.sh $grdID > $grdID.out 2>&1" >> cmdfile
      else
        echo "${USHgfs}/wave_grid_moddef.sh $grdID > $grdID.out 2>&1" >> cmdfile
      fi

      nmoddef=$(expr $nmoddef + 1)

    fi
  done

# 1.a.1 Execute parallel or serialpoe

  if [ "$nmoddef" -gt '0' ]
  then

    set +x
    echo ' '
    echo " Generating $nmoddef mod def files"
    echo ' '
    set_trace

# Set number of processes for mpmd
    wavenproc=$(wc -l cmdfile | awk '{print $1}')
    wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

# 1.a.3 Execute the serial or parallel cmdfile

    set +x
    echo ' '
    echo "   Executing the mod_def command file at : $(date)"
    echo '   ------------------------------------'
    echo ' '
    set_trace
    if [ "$NTASKS" -gt '1' ]
    then
      if [ ${CFP_MP:-"NO"} = "YES" ]; then
        ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdfile
      else
        ${wavempexec} ${wavenproc} ${wave_mpmd} cmdfile
      fi
      exit=$?
    else
      ./cmdfile
      exit=$?
    fi

    if [[ "$exit" != '0' ]]
    then
      set +x
      echo ' '
      echo '********************************************************'
      echo '*** FATAL ERROR: POE FAILURE DURING RAW DATA COPYING ***'
      echo '********************************************************'
      echo '     See Details Below '
      echo ' '
      set_trace
    fi

  fi

# 1.a.3 File check

  for grdID in ${grdALL}; do
    if [[ -f "${COMOUT_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" ]]; then
      set +x
      echo ' '
      echo " mod_def.$grdID succesfully created/copied "
      echo ' '
      set_trace
    else
      set +x
      echo ' '
      echo '********************************************** '
      echo '*** FATAL ERROR : NO MODEL DEFINITION FILE *** '
      echo '********************************************** '
      echo "                                grdID = ${grdID}"
      echo ' '
      sed "s/^/${grdID}.out : /g"  "${grdID}.out"
      set_trace
      err=3;export err;${errchk}
    fi
  done

# Copy to other members if needed
if (( NMEM_ENS > 0 )); then
  for mem in $(seq -f "%03g" 1 "${NMEM_ENS}"); do
    MEMDIR="mem${mem}" YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_PREP_MEM:COM_WAVE_PREP_TMPL
    mkdir -p "${COMOUT_WAVE_PREP_MEM}"
    for grdID in ${grdALL}; do
      ${NLN} "${COMOUT_WAVE_PREP}/${RUN}wave.mod_def.${grdID}" "${COMOUT_WAVE_PREP_MEM}/${RUN}wave.mod_def.${grdID}"
    done
  done
fi

# --------------------------------------------------------------------------- #
# 2.  Ending


# End of MWW3 init config script ------------------------------------------- #
