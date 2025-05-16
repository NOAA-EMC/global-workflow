#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exgfs_wave_post_pnt.sh
# Script description:  Creates point output products from NetCDF WW3 point data
#
# Abstract: This script is the point postprocessor for the wave component in GFS.
#           It executes several scripts forpreparing and creating output data
#           as follows:
#
#  wave_tar.sh               : tars the spectral and bulletin multiple files
#
# COM inputs:
#  - ${COMIN_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin
#  - ${COMIN_WAVE_HISTORY}/${RUN}.wave.t${cyc}z.points.f${FH3}.nc
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (Bash) Shell
#
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

# 0.a Basic modes of operation

  export WAV_MOD_TAG="${RUN}.wave.t${cyc}z"

  echo "HAS BEGUN on $(hostname)"
  echo "Starting WAVE PNT POSTPROCESSOR SCRIPT for ${WAV_MOD_TAG}"

  cat << EOF

                     *************************************
                     *** WAVE PNT POSTPROCESSOR SCRIPT ***
                     *************************************

Starting at : $(date)
-------------

EOF

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
  if [[ -z "${NTASKS}" ]]
  then
    export err=1
    err_exit "Requires NTASKS to be set"
  fi

# 0.c Defining model grids

  waveuoutpGRD=${waveuoutpGRD:?buoyNotSet}

# 0.c.1 Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=${DATA}/station_ascii_files
  rm -rf "${STA_DIR}"
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull

  printf "\n   Grid information  :\n   ------------------\n     Output points : %s\n" "${waveuoutpGRD}"

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  printf "\nPreparing input files :\n-------------------------\n"

# 1.a Model definition files and output files (set up using poe)

# Copy model definition files
  iloop=0
  for grdID in ${waveuoutpGRD}; do
    if [[ -f "${COMIN_WAVE_PREP}/${WAV_MOD_TAG}.mod_def.${grdID}.bin" ]]; then
      echo " Mod def file for ${grdID} found in ${COMIN_WAVE_PREP}. copying ...."

      cpreq -f "${COMIN_WAVE_PREP}/${WAV_MOD_TAG}.mod_def.${grdID}.bin" "mod_def.${grdID}"
      iloop=$((iloop + 1))
    fi
  done

  for grdID in ${waveuoutpGRD}
  do
    if [[ ! -f "mod_def.${grdID}" ]]
    then
      export err=2
      err_exit "NO MOD_DEF FILE mod_def.${grdID}"
    else
      echo "File mod_def.${grdID} found. Syncing to all nodes ..."
    fi
  done

# 1.b Output locations file

  rm -f buoy.loc

  if [[ -f "${PARMgfs}/wave/wave_${NET}.buoys" ]]
  then
    cpreq -f "${PARMgfs}/wave/wave_${NET}.buoys" buoy.loc.temp
    if [[ "${DOBNDPNT_WAV}" == "YES" ]]; then
      #only do boundary points
      sed -n '/^\$.*/!p' buoy.loc.temp | grep IBP > buoy.loc || {
          echo "WARNING: No boundary points found in buoy file ${PARMgfs}/wave/wave_${NET}.buoys"
          echo "         Ending job without doing anything."
          exit 0
        }
    else
      #exclude boundary points
      sed -n '/^\$.*/!p' buoy.loc.temp | grep -v IBP > buoy.loc
    fi
  fi

  if [[ -s buoy.loc ]]
  then
    echo "   buoy.loc and buoy.ibp copied and processed (${PARMgfs}/wave/wave_${NET}.buoys)."
  else
    export err=3
    err_exit 'NO BUOY LOCATION FILE'
  fi

# 1.c Input template files

  if [[ -f "${PARMgfs}/wave/ww3_outp_spec.inp.tmpl" ]]
  then
    cpreq -f "${PARMgfs}/wave/ww3_outp_spec.inp.tmpl" ww3_outp_spec.inp.tmpl
  fi

  if [[ -f ww3_outp_spec.inp.tmpl ]]
  then
    echo "   ww3_outp_spec.inp.tmpl copied. Syncing to all grids ..."
  else
    export err=3
    err_exit "NO TEMPLATE FOR SPEC INPUT FILE"
  fi

  if [[ -f "${PARMgfs}/wave/ww3_outp_bull.inp.tmpl" ]]
  then
    cpreq "${PARMgfs}/wave/ww3_outp_bull.inp.tmpl" ww3_outp_bull.inp.tmpl
  fi

  if [[ -f ww3_outp_bull.inp.tmpl ]]
  then
    echo "   ww3_outp_bull.inp.tmpl copied. Syncing to all nodes ..."
  else
    export err=4
    err_exit "NO TEMPLATE FOR BULLETIN INPUT FILE"
  fi

# 1.d Linking the output files

  ymdh=$(${NDATE} -"${WAVHINDH}" "${PDY}${cyc}")
  tstart="${ymdh:0:8} ${ymdh:8:2}0000"
  N=$(( (${FHMAX_WAV_PNT} - ${FHMIN_WAV}) * 3600 / ${DTPNT_WAV} + 1 ))
  truntime="${PDY} ${cyc}0000"

  # Loop through forecast hours to link output file
  fhr=${FHMIN_WAV}
  while [[ ${fhr} -le ${FHMAX_WAV_PNT} ]]; do
    ymdh=$(${NDATE} ${fhr} "${PDY}${cyc}")
    YMD=${ymdh:0:8}
    HMS="${ymdh:8:2}0000"
    FH3=$(printf %03i ${fhr})
    pfile="${COMIN_WAVE_HISTORY}/${WAV_MOD_TAG}.points.f${FH3}.nc"
    if [[ -f "${pfile}" ]]; then
      ${NLN} "${pfile}" "./${YMD}.${HMS}.out_pnt.ww3.nc"
    else
      export err=7
      err_exit "NO RAW POINT OUTPUT FILE ${YMD}.${HMS}.out_pnt.ww3.nc"
    fi

    FHINCP=$(( DTPNT_WAV / 3600 ))
    fhrp=$((fhr+FHINCP))
    fhr=${fhrp}  # no gridded output, loop with out_pnt stride
  done

# 1.e Getting buoy information for points

  if [[ "${DOSPC_WAV}" == 'YES' || "${DOBLL_WAV}" == 'YES' ]]
  then
    ymdh=$(${NDATE} -"${WAVHINDH}" "${PDY}${cyc}")
    YMD=${ymdh:0:8}
    tstart="${ymdh:0:8} ${ymdh:8:2}0000"
    N=$(( (FHMAX_WAV_PNT - FHMIN_WAV) * 3600 / DTPNT_WAV + 1 ))
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${DTPNT_WAV}/g" \
        -e "s/999/${N}/g" \
        -e "s/PREFIX/${RUN}.wave/g" \
        -e "s/^.*POINT.*/\$ &/g" \
        -e "s/ITYPE/0/g" \
        -e "s/FORMAT/F/g" \
                               ww3_outp_spec.inp.tmpl > ww3_outp.inp
  fi

    rm -f buoy_tmp.loc buoy_log.ww3 ww3_oup.inp
    ${NLN} ./mod_def.${waveuoutpGRD} ./mod_def.ww3

    export pgm="${NET,,}_ww3_outp.x"
    source prep_step

    "${EXECgfs}/${pgm}" > buoy_lst.loc 2>&1
    export err=$?
    if [[ ${err} -ne 0 && ! -f buoy_log.ww3 ]]
    then
      cat buoy_tmp.loc || true
      export err=5
      err_exit "${WAV_MOD_TAG} post ${date} ${cycle} : buoy log file failed to be created."
    fi

# Create new buoy_log.ww3
    awk '{print $3}' buoy.loc | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.dat
    mv buoy_log.tmp buoy_log.dat

    Nb=$(wc buoy_log.dat | awk '{ print $1 }')

    if [[ -s buoy_log.dat ]]
    then
      echo 'Buoy log file created. Syncing to all nodes ...'
    else
      export err=6
      err_exit "NO BUOY LOG FILE CREATED"
    fi

# 1.f Data summary

  cat << EOF

   Input files read and processed at : $(date)

   Data summary :
   ---------------------------------------------
      Sufficient data for spectral files        : ${DOSPC_WAV} (${Nb} points)
      Sufficient data for bulletins             : ${DOBLL_WAV} (${Nb} points)
      Boundary points                           : ${DOBNDPNT_WAV}

EOF

# --------------------------------------------------------------------------- #
# 2. Make files for processing boundary points
#
# 2.a Creating ww3_outp.inp for each job and execute ww3_outp

  echo '   Making command file for wave post points '

  grep -F -f ibp_tags buoy_log.dat | awk '{ print $2 }' > buoys
  grep -F -f buoys buoy_log.ww3 | awk '{ print $1 }' > points
  points=$(awk '{print $0 "\\n"}' points | tr -d '\n')
  rm -f buoys

  # Generate the ww3_outp.inp file from the template
  if [[ "${DOSPC_WAV}" == 'YES' ]]; then
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${DTPNT_WAV}/g" \
        -e "s/999/${N}/g" \
        -e "s/PREFIX/${RUN}.wave/g" \
        -e "s|POINT|$points|g" \
        -e "s/ITYPE/1/g" \
        -e "s/FORMAT/F/g" \
                           ww3_outp_spec.inp.tmpl > ww3_outp.inp

    export pgm="${NET,,}_ww3_outp.x"
    "${EXECgfs}/${pgm}"
  fi

  if [[ "${DOBLL_WAV}" == 'YES' ]]; then
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${DTPNT_WAV}/g" \
        -e "s/999/${N}/g" \
        -e "s/PREFIX/${RUN}.wave/g" \
        -e "s|POINT|$points|g" \
        -e "s/REFT/$truntime/g" \
                           ww3_outp_bull.inp.tmpl > ww3_outp.inp
    export pgm="${NET,,}_ww3_outp.x"
    "${EXECgfs}/${pgm}"
  fi

# --------------------------------------------------------------------------- #
# 3. Compress point output data into tar files

# 3.a Set up cmdfile

  rm -f cmdtarfile
  touch cmdtarfile
  chmod 744 cmdtarfile

  printf "\n   Making command file for taring all point output files."

# 3.b Execute the taring

  if [[ "${DOBNDPNT_WAV}" == "YES" ]]; then
    if [[ "${DOSPC_WAV}" == "YES" ]]; then
      echo "${USHgfs}/wave_tar.sh ${WAV_MOD_TAG} ibp ${Nb} 2>&1 | tee ${WAV_MOD_TAG}_ibp_tar.out" >> cmdtarfile
    fi
    if [[ "${DOBLL_WAV}" == "YES" ]]; then
      echo "${USHgfs}/wave_tar.sh ${WAV_MOD_TAG} ibpbull ${Nb} 2>&1 | tee ${WAV_MOD_TAG}_ibpbull_tar.out" >> cmdtarfile
      echo "${USHgfs}/wave_tar.sh ${WAV_MOD_TAG} ibpcbull ${Nb} 2>&1 | tee ${WAV_MOD_TAG}_ibpcbull_tar.out" >> cmdtarfile
    fi
  else
    if [[ "${DOSPC_WAV}" == "YES" ]]; then
      echo "${USHgfs}/wave_tar.sh ${WAV_MOD_TAG} spec ${Nb} 2>&1 | tee ${WAV_MOD_TAG}_spec_tar.out" >> cmdtarfile
    fi
    if [[ "${DOBLL_WAV}" == "YES" ]]; then
      echo "${USHgfs}/wave_tar.sh ${WAV_MOD_TAG} bull ${Nb} 2>&1 | tee ${WAV_MOD_TAG}_bull_tar.out" >> cmdtarfile
      echo "${USHgfs}/wave_tar.sh ${WAV_MOD_TAG} cbull ${Nb} 2>&1 | tee ${WAV_MOD_TAG}_cbull_tar.out" >> cmdtarfile
    fi
  fi

  # Ensure there are enough processors for MPMD else use serial
  ncmds=$(wc -l < cmdtarfile)
  if [[ ${NTASKS} -lt ${ncmds} ]]; then
    if [[ "${USE_CFP:-}" == "YES" ]]; then
      echo "WARNING: Not enough processors for MPMD, '${NTASKS} < ${ncmd}', running in serial mode"
      export USE_CFP="NO"
    fi
  fi

  "${USHgfs}/run_mpmd.sh" "${DATA}/cmdtarfile" && true
  export err=$?
  if [[ ${err} -ne 0 ]]; then
     export pgm="run_mpmd.sh"
     err_exit "run_mpmd failed while tarring point outputs."
  fi

# End of WW3 point prostprocessor script ---------------------------------------- #
