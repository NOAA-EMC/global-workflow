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

  cd $DATA

  export WAV_MOD_TAG="${RUN}.wave.t${cyc}z"

  echo "HAS BEGUN on $(hostname)"
  echo "Starting WAVE PNT POSTPROCESSOR SCRIPT for ${WAV_MOD_TAG}"

  set +x
  echo ' '
  echo '                     *************************************'
  echo '                     *** WAVE PNT POSTPROCESSOR SCRIPT ***'
  echo '                     *************************************'
  echo ' '
  echo "Starting at : $(date)"
  echo '-------------'
  echo ' '
  set_trace

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
  if [ -z ${NTASKS} ]
  then
    echo "FATAL ERROR: requires NTASKS to be set "
    err=1; export err;${errchk}
    exit $err
  fi

# 0.c Defining model grids

  waveuoutpGRD=${waveuoutpGRD:?buoyNotSet}

# 0.c.1 Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull

  set +x
  echo ' '
  echo 'Grid information  :'
  echo '-------------------'
  echo "   Output points : $waveuoutpGRD"
  echo ' '
  set_trace

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  exit_code=0

  set +x
  echo ' '
  echo 'Preparing input files :'
  echo '-----------------------'
  set_trace

# 1.a Model definition files and output files (set up using poe)

# Copy model definition files
  iloop=0
  for grdID in ${waveuoutpGRD}; do
    if [[ -f "${COMIN_WAVE_PREP}/${WAV_MOD_TAG}.mod_def.${grdID}.bin" ]]; then
      set +x
      echo " Mod def file for ${grdID} found in ${COMIN_WAVE_PREP}. copying ...."
      set_trace

      cp -f "${COMIN_WAVE_PREP}/${WAV_MOD_TAG}.mod_def.${grdID}.bin" "mod_def.${grdID}"
      iloop=$((iloop + 1))
    fi
  done

  for grdID in $waveuoutpGRD
  do
    if [ ! -f mod_def.$grdID ]
    then
      set +x
      echo ' '
      echo '*************************************************** '
      echo " FATAL ERROR : NO MOD_DEF FILE mod_def.$grdID "
      echo '*************************************************** '
      echo ' '
      set_trace
      err=2; export err;${errchk}
      exit $err
    else
      set +x
      echo "File mod_def.$grdID found. Syncing to all nodes ..."
      set_trace
    fi
  done

# 1.b Output locations file

  rm -f buoy.loc

  if [ -f ${PARMgfs}/wave/wave_${NET}.buoys ]
  then
    cp -f ${PARMgfs}/wave/wave_${NET}.buoys buoy.loc.temp
    if [ "$DOBNDPNT_WAV" = YES ]; then
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

  if [ -s buoy.loc ]
  then
    set +x
    echo "   buoy.loc and buoy.ibp copied and processed (${PARMgfs}/wave/wave_${NET}.buoys)."
    set_trace
  else
    set +x
    echo ' '
    echo '************************************* '
    echo ' FATAL ERROR : NO BUOY LOCATION FILE  '
    echo '************************************* '
    echo ' '
    set_trace
    err=3; export err;${errchk}
    exit $err
    DOSPC_WAV='NO'
    DOBLL_WAV='NO'
  fi

# 1.c Input template files

  if [ -f ${PARMgfs}/wave/ww3_outp_spec.inp.tmpl ]
  then
    cp -f ${PARMgfs}/wave/ww3_outp_spec.inp.tmpl ww3_outp_spec.inp.tmpl
  fi

  if [ -f ww3_outp_spec.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_spec.inp.tmpl copied. Syncing to all grids ..."
    set_trace
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR SPEC INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    set_trace
    exit_code=3
    DOSPC_WAV='NO'
    DOBLL_WAV='NO'
  fi

  if [ -f ${PARMgfs}/wave/ww3_outp_bull.inp.tmpl ]
  then
    cp -f ${PARMgfs}/wave/ww3_outp_bull.inp.tmpl ww3_outp_bull.inp.tmpl
  fi

  if [ -f ww3_outp_bull.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_bull.inp.tmpl copied. Syncing to all nodes ..."
    set_trace
  else
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE *** '
    echo '*************************************************** '
    echo ' '
    set_trace
    exit_code=4
    DOBLL_WAV='NO'
  fi

# 1.d Linking the output files

  ymdh=$(${NDATE} -"${WAVHINDH}" "${PDY}${cyc}")
  tstart="${ymdh:0:8} ${ymdh:8:2}0000"
  N=$(( ($FHMAX_WAV_PNT - $FHMIN_WAV) * 3600 / $DTPNT_WAV + 1 ))
  truntime="${PDY} ${cyc}0000"

  # Loop through forecast hours to link output file
  fhr=$FHMIN_WAV
  while [ $fhr -le $FHMAX_WAV_PNT ]; do
    ymdh=$($NDATE $fhr "${PDY}${cyc}")
    YMD=${ymdh:0:8}
    HMS="${ymdh:8:2}0000"
    FH3=$(printf %03i ${fhr})
    pfile="${COMIN_WAVE_HISTORY}/${WAV_MOD_TAG}.points.f${FH3}.nc"
    if [[ -f "${pfile}" ]]; then
      ${NLN} "${pfile}" "./${YMD}.${HMS}.out_pnt.ww3.nc"
    else
      echo '*************************************************** '
      echo "  FATAL ERROR : NO RAW POINT OUTPUT FILE ${YMD}.${HMS}.out_pnt.ww3.nc "
      echo '*************************************************** '
      [[ "$LOUD" = YES ]] && set -x
      err=7; export err; ${errchk}
      exit $err
    fi

    FHINCP=$(( DTPNT_WAV / 3600 ))
    fhrp=$((fhr+FHINCP))
    fhr=$fhrp  # no gridded output, loop with out_pnt stride
  done

# 1.e Getting buoy information for points

  if [ "$DOSPC_WAV" = 'YES' ] || [ "$DOBLL_WAV" = 'YES' ]
  then
    ymdh=$(${NDATE} -"${WAVHINDH}" "${PDY}${cyc}")
    YMD=${ymdh:0:8}
    tstart="${ymdh:0:8} ${ymdh:8:2}0000"
    N=$(( (FHMAX_WAV_PNT - FHMIN_WAV) * 3600 / DTPNT_WAV + 1 ))
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${DTPNT_WAV}/g" \
	-e "s/999/$N/g" \
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
    export err=$?;err_chk
    if [ "$err" != '0' ] && [ ! -f buoy_log.ww3 ]
    then
      set +x
      echo ' '
      echo '******************************************** '
      echo "*** FATAL ERROR : ERROR IN ${pgm} *** "
      echo '******************************************** '
      echo ' '
      cat buoy_tmp.loc
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file failed to be created."
      set_trace
      err=5;export err;${errchk}
      DOSPC_WAV='NO'
      DOBLL_WAV='NO'
      exit $err
    fi

# Create new buoy_log.ww3
    awk '{print $3}' buoy.loc | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.dat
    mv buoy_log.tmp buoy_log.dat

    Nb=$(wc buoy_log.dat | awk '{ print $1 }')

    if [ -s buoy_log.dat ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      set_trace
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      set_trace
      err=6;export err;${errchk}
      DOSPC_WAV='NO'
      DOBLL_WAV='NO'
    fi

# 1.f Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : $(date)"
  echo ' '
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "      Sufficient data for spectral files        : $DOSPC_WAV ($Nb points)"
  echo "      Sufficient data for bulletins             : $DOBLL_WAV ($Nb points)"
  echo "      Boundary points                           : $DOBNDPNT_WAV"
  echo ' '
  set_trace

# --------------------------------------------------------------------------- #
# 2. Make files for processing boundary points
#
# 2.a Creating ww3_outp.inp for each job and execute ww3_outp

  set +x
  echo '   Making command file for wave post points '
  set_trace

  grep -F -f ibp_tags buoy_log.dat | awk '{ print $2 }' > buoys
  grep -F -f buoys buoy_log.ww3 | awk '{ print $1 }' > points
  points=$(awk '{print $0 "\\n"}' points | tr -d '\n')
  rm buoys

  # Generate the ww3_outp.inp file from the template
  if [ "$DOSPC_WAV" = 'YES' ]; then
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${DTPNT_WAV}/g" \
        -e "s/999/$N/g" \
	-e "s/PREFIX/${RUN}.wave/g" \
        -e "s|POINT|$points|g" \
        -e "s/ITYPE/1/g" \
        -e "s/FORMAT/F/g" \
                           ww3_outp_spec.inp.tmpl > ww3_outp.inp

    export pgm="${NET,,}_ww3_outp.x"
    "${EXECgfs}/${pgm}"
  fi

  if [ "$DOBLL_WAV" = 'YES' ]; then
    sed -e "s/TIME/${tstart}/g" \
        -e "s/DT/${DTPNT_WAV}/g" \
        -e "s/999/$N/g" \
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

  set +x
  echo ' '
  echo '   Making command file for taring all point output files.'

  set_trace

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
    if [[ "${USE_CFP:-}" = "YES" ]]; then
      echo "WARNING: Not enough processors for MPMD, '${NTASKS} < ${ncmd}', running in serial mode"
      export USE_CFP="NO"
    fi
  fi

  "${USHgfs}/run_mpmd.sh" "${DATA}/cmdtarfile"
  export err=$?; err_chk

# --------------------------------------------------------------------------- #
# 4.  Ending output


exit "${exit_code}"

# End of WW3 point prostprocessor script ---------------------------------------- #
