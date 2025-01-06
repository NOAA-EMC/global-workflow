#! /usr/bin/env bash

WW3_namelists(){

# WW3 namelists/input generation

  FHMAX_WAV=${FHMAX_WAV:-384}

  # Date and time stuff

  # Beginning time for outpupt may differ from SDATE if DOIAU=YES
  export date=$PDY
  export YMDH=${PDY}${cyc}
  # Roll back $IAU_FHROT hours of DOIAU=YES
  if [ "$DOIAU" = "YES" ]
  then
    WAVHINDH=$(( WAVHINDH + IAU_FHROT ))
  fi
  # Set time stamps for model start and output
  # For special case when IAU is on but this is an initial half cycle 
  if [ ${IAU_OFFSET:-0} = 0 ]; then
    ymdh_beg=$YMDH
  else
    ymdh_beg=$($NDATE -$WAVHINDH $YMDH)
  fi
  time_beg="$(echo $ymdh_beg | cut -c1-8) $(echo $ymdh_beg | cut -c9-10)0000"
  ymdh_end=$($NDATE $FHMAX_WAV $YMDH)
  time_end="$(echo $ymdh_end | cut -c1-8) $(echo $ymdh_end | cut -c9-10)0000"
  ymdh_beg_out=$YMDH
  time_beg_out="$(echo $ymdh_beg_out | cut -c1-8) $(echo $ymdh_beg_out | cut -c9-10)0000"

  set +x
  echo ' '
  echo 'Times in wave model format :'
  echo '----------------------------'
  echo "   date / cycle  : $date $cycle"
  echo "   starting time : $time_beg"
  echo "   ending time   : $time_end"
  echo ' '
  set_trace



# --------------------------------------------------------------------------- #
# Buoy location file

  if [ -f ${PARMgfs}/wave/wave_${NET}.buoys ]
  then
    ${NCP} "${PARMgfs}/wave/wave_${NET}.buoys" "${DATA}/ww3_points.list"
  fi

  if [ -f ${DATA}/ww3_points.list ]
  then
    set +x
    echo "   ww3_points.list copied (${PARMgfs}/wave/wave_${NET}.buoys)."
    set_trace
  else
    echo " FATAL ERROR : ww3_points.list (${PARMgfs}/wave/wave_${NET}.buoys) NOT FOUND"
    exit 12 
  fi

  #set coupling to ice/current
  WW3_ICE='F'
  WW3_IC1='F'
  WW3_IC5='F'
  WW3_CUR='F'

  case ${WW3ICEINP} in
    'YES' )
      WW3_ICE="T";;
    'CPL' )
      WW3_ICE="C";;
  esac

  case ${WW3CURINP} in
    'YES' )
      WW3_CUR="T";;
    'CPL' )
      WW3_CUR="C";;
  esac

  local WW3_OUTPARS="${OUTPARS_WAV}"
  local WW3_DTFLD="${DTFLD_WAV}"
  local WW3_DTPNT="${DTPNT_WAV}"
  # Ensure the template exists
  local template=${WW3_INPUT_TEMPLATE:-"${PARMgfs}/ufs/ww3_shel.nml.IN"}
  if [[ ! -f "${template}" ]]; then
    echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
    exit 1
  fi
  rm -f "${DATA}/ww3_shel.nml"
  atparse < "${template}" >> "${DATA}/ww3_shel.nml"
  echo "Rendered ww3_shel.nml:"
  cat "${DATA}/ww3_shel.nml"

}
