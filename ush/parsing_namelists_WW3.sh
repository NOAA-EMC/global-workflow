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

  # Restart file times (already has IAU_FHROT in WAVHINDH) 
  RSTOFFSET=$(( ${WAVHCYC} - ${WAVHINDH} ))
  # Update restart time is added offset relative to model start
  RSTOFFSET=$(( ${RSTOFFSET} + ${RSTIOFF_WAV} ))
  ymdh_rst_ini=$($NDATE ${RSTOFFSET} $YMDH)
  RST2OFFSET=$(( DT_2_RST_WAV / 3600 ))
  ymdh_rst2_ini=$($NDATE ${RST2OFFSET} $YMDH) # DT2 relative to first-first-cycle restart file
  # First restart file for cycling
  time_rst_ini="$(echo $ymdh_rst_ini | cut -c1-8) $(echo $ymdh_rst_ini | cut -c9-10)0000"
  if [ ${DT_1_RST_WAV} = 1 ]; then
    time_rst1_end=${time_rst_ini}
  else
    RST1OFFSET=$(( DT_1_RST_WAV / 3600 ))
    ymdh_rst1_end=$($NDATE $RST1OFFSET $ymdh_rst_ini)
    time_rst1_end="$(echo $ymdh_rst1_end | cut -c1-8) $(echo $ymdh_rst1_end | cut -c9-10)0000"
  fi
  # Second restart file for checkpointing
  if [ "${RSTTYPE_WAV}" = "T" ]; then
    time_rst2_ini="$(echo $ymdh_rst2_ini | cut -c1-8) $(echo $ymdh_rst2_ini | cut -c9-10)0000"
    time_rst2_end=$time_end
  # Condition for gdas run or any other run when checkpoint stamp is > ymdh_end
    if [ $ymdh_rst2_ini -ge $ymdh_end ]; then
      ymdh_rst2_ini=$($NDATE 3 $ymdh_end)
      time_rst2_ini="$(echo $ymdh_rst2_ini | cut -c1-8) $(echo $ymdh_rst2_ini | cut -c9-10)0000"
      time_rst2_end=$time_rst2_ini
    fi
  else
    time_rst2_ini="$"
    time_rst2_end=
    DT_2_RST_WAV=
  fi


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
# Create ww3_shel.inp

  if [ -f "${PARMgfs}/wave/ww3_shel.inp.tmpl" ]; then
    cp "${PARMgfs}/wave/ww3_shel.inp.tmpl" "ww3_shel.inp.tmpl"
  fi
  if [ ! -f ww3_shel.inp.tmpl ]; then
    echo "ABNORMAL EXIT: NO TEMPLATE FOR WW3 SHEL INPUT FILE" 
    exit 12
  fi

# Buoy location file

  if [ -f ${PARMgfs}/wave/wave_${NET}.buoys ]
  then
    cp ${PARMgfs}/wave/wave_${NET}.buoys buoy.loc
  fi

  if [ -f buoy.loc ]
  then
    set +x
    echo "   buoy.loc copied (${PARMgfs}/wave/wave_${NET}.buoys)."
    set_trace
  else
    echo " FATAL ERROR : buoy.loc (${PARMgfs}/wave/wave_${NET}.buoys) NOT FOUND"
    exit 12 
  fi

# Initialize inp file parameters
ICELINE='F F'
CURRLINE='F F'
WINDLINE='F F'

case ${WW3ATMINP} in
  'YES' )
    WINDLINE="T F";;
  'CPL' )
    WINDLINE="C F";;
esac

case ${WW3ICEINP} in
  'YES' )
    ICELINE="T F";;
  'CPL' )
    ICELINE="C F";;
esac

case ${WW3CURINP} in
  'YES' )
    CURRLINE="T F";;
  'CPL' )
    CURRLINE="C F";;
esac

sed -e "s/IOSRV/${IOSRV}/g" \
    -e "s/OUTPARS/${OUTPARS_WAV}/g" \
    -e "s/ICELINE/$ICELINE/g" \
    -e "s/CURRLINE/$CURRLINE/g" \
    -e "s/WINDLINE/$WINDLINE/g" \
    -e "s/RUN_BEG/$time_beg/g" \
    -e "s/RUN_END/$time_end/g" \
    -e "s/OUT_BEG/$time_beg_out/g" \
    -e "s/OUT_END/$time_end/g" \
    -e "s/DTFLD/ $DTFLD_WAV/g" \
    -e "s/GOFILETYPE/ $GOFILETYPE/g" \
    -e "s/POFILETYPE/ $POFILETYPE/g" \
    -e "s/DTPNT/ $DTPNT_WAV/g" \
    -e "s/DTPNT/ $DTPNT_WAV/g" \
    -e "/BUOY_FILE/r buoy.loc" \
    -e "s/BUOY_FILE/DUMMY/g" \
    -e "s/RST_BEG/$time_rst_ini/g" \
    -e "s/RSTTYPE/$RSTTYPE_WAV/g" \
    -e "s/RST_2_BEG/$time_rst2_ini/g" \
    -e "s/DTRST/$DT_1_RST_WAV/g" \
    -e "s/DT_2_RST/$DT_2_RST_WAV/g" \
    -e "s/RST_END/$time_rst1_end/g" \
    -e "s/RST_2_END/$time_rst2_end/g" \
                                   ww3_shel.inp.tmpl | \
sed -n "/DUMMY/!p"               > ww3_shel.inp

rm -f ww3_shel.inp.tmpl buoy.loc

cat ww3_shel.inp

}
