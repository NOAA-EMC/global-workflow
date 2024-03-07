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
  if [ $IAU_OFFSET = 0 ]; then
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
# Create ww3_multi/shel.inp

  if [ $waveMULTIGRID = ".true." ]; then
    # ww3_multi template
    if [ -f ${PARMgfs}/wave/ww3_multi.inp.tmpl ]; then
      cp ${PARMgfs}/wave/ww3_multi.inp.tmpl ww3_multi.inp.tmpl
    fi
    if [ ! -f ww3_multi.inp.tmpl ]; then
      echo "ABNORMAL EXIT: NO TEMPLATE FOR WW3 MULTI INPUT FILE" 
      exit 11 
    fi
  else 
    # ww3_multi template
    if [ -f ${PARMgfs}/wave/ww3_shel.inp.tmpl ]; then
      cp ${PARMgfs}/wave/ww3_shel.inp.tmpl ww3_shel.inp.tmpl
    fi
    if [ ! -f ww3_shel.inp.tmpl ]; then
      echo "ABNORMAL EXIT: NO TEMPLATE FOR WW3 SHEL INPUT FILE" 
      exit 12
    fi
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



if [ $waveMULTIGRID = ".true." ]; then
#multi 

# Initialize inp file parameters
  NFGRIDS=0
  NMGRIDS=0
  CPLILINE='$'
  ICELINE='$'
  ICEFLAG='no'
  CURRLINE='$'
  CURRFLAG='no'
  WINDLINE='$'
  WINDFLAG='no'
  UNIPOINTS='$'

# Check for required inputs and coupling options
  if [ $waveuoutpGRD ]
  then
    UNIPOINTS="'$waveuoutpGRD'"
  fi

# Check if waveesmfGRD is set
  if [ ${waveesmfGRD} ]
  then
    NFGRIDS=$(expr $NFGRIDS + 1)
  fi

  case ${WW3ATMINP} in
    'YES' )
      NFGRIDS=$(expr $NFGRIDS + 1)
      WINDLINE="  '$WAVEWND_FID'  F F T F F F F F F"
      WINDFLAG="$WAVEWND_FID"
    ;;
    'CPL' )
      WNDIFLAG='T'
      if [ ${waveesmfGRD} ]
      then
        WINDFLAG="CPL:${waveesmfGRD}"
        CPLILINE="  '${waveesmfGRD}' F F T F F F F F F"
      else
        WINDFLAG="CPL:native"
      fi
    ;;
  esac

  case ${WW3ICEINP} in
    'YES' )
      NFGRIDS=$(expr $NFGRIDS + 1)
      ICEIFLAG='T'
      ICELINE="  '$WAVEICE_FID'  F F F T F F F F F"
      ICEFLAG="$WAVEICE_FID"
    ;;
    'CPL' )
      ICEIFLAG='T'
      if [ ${waveesmfGRD} ]
      then
        ICEFLAG="CPL:${waveesmfGRD}"
        CPLILINE="  '${waveesmfGRD}' F F ${WNDIFLAG} T F F F F F"
      else
        ICEFLAG="CPL:native"
      fi
    ;;
  esac

  case ${WW3CURINP} in
    'YES' )
      if [ "$WAVECUR_FID" != "$WAVEICE_FID" ]; then
        NFGRIDS=$(expr $NFGRIDS + 1)
        CURRLINE="  '$WAVECUR_FID'  F T F F F F F F F"
        CURRFLAG="$WAVECUR_FID"
      else # cur fields share the same grid as ice grid
        ICELINE="  '$WAVEICE_FID'  F T F ${ICEIFLAG} F F F F F"
        CURRFLAG="$WAVEICE_FID"
      fi
    ;;
    'CPL' )
      CURIFLAG='T'
      if [ ${waveesmfGRD} ]
      then
        CURRFLAG="CPL:${waveesmfGRD}"
        CPLILINE="  '${waveesmfGRD}' F T ${WNDIFLAG} ${ICEFLAG} F F F F F"
      else
        CURRFLAG="CPL:native"
      fi
    ;;
  esac

  unset agrid
  agrid=
  gline=
  GRDN=0
#  grdGRP=1 # Single group for now
  for grid in ${waveGRD}
  do
    GRDN=$(expr ${GRDN} + 1)
    agrid=( ${agrid[*]} ${grid} )
    NMGRIDS=$(expr $NMGRIDS + 1)
    gridN=$(echo $waveGRDN | awk -v i=$GRDN '{print $i}')
    gridG=$(echo $waveGRDG | awk -v i=$GRDN '{print $i}')
    gline="${gline}'${grid}'  'no' 'CURRFLAG' 'WINDFLAG' 'ICEFLAG'  'no' 'no' 'no' 'no' 'no'  ${gridN} ${gridG}  0.00 1.00  F\n"
  done
  gline="${gline}\$"
  echo $gline

  sed -e "s/NFGRIDS/$NFGRIDS/g" \
      -e "s/NMGRIDS/${NMGRIDS}/g" \
      -e "s/FUNIPNT/${FUNIPNT}/g" \
      -e "s/IOSRV/${IOSRV}/g" \
      -e "s/FPNTPROC/${FPNTPROC}/g" \
      -e "s/FGRDPROC/${FGRDPROC}/g" \
      -e "s/OUTPARS/${OUTPARS_WAV}/g" \
      -e "s/CPLILINE/${CPLILINE}/g" \
      -e "s/UNIPOINTS/${UNIPOINTS}/g" \
      -e "s/GRIDLINE/${gline}/g" \
      -e "s/ICELINE/$ICELINE/g" \
      -e "s/CURRLINE/$CURRLINE/g" \
      -e "s/WINDLINE/$WINDLINE/g" \
      -e "s/ICEFLAG/$ICEFLAG/g" \
      -e "s/CURRFLAG/$CURRFLAG/g" \
      -e "s/WINDFLAG/$WINDFLAG/g" \
      -e "s/RUN_BEG/$time_beg/g" \
      -e "s/RUN_END/$time_end/g" \
      -e "s/OUT_BEG/$time_beg_out/g" \
      -e "s/OUT_END/$time_end/g" \
      -e "s/DTFLD/ $DTFLD_WAV/g" \
      -e "s/FLAGMASKCOMP/ $FLAGMASKCOMP/g" \
      -e "s/FLAGMASKOUT/ $FLAGMASKOUT/g" \
      -e "s/GOFILETYPE/ $GOFILETYPE/g" \
      -e "s/POFILETYPE/ $POFILETYPE/g" \
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
                                     ww3_multi.inp.tmpl | \
  sed -n "/DUMMY/!p"               > ww3_multi.inp

  rm -f ww3_multi.inp.tmpl buoy.loc

  cat ww3_multi.inp

else 
  #ww3_shel 

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

fi   

}
