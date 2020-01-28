#!/bin/bash
###############################################################################
#                                                                             #
# This script is the postprocessor for the wave component in NCEP's global    #
# coupled system (GEFS, GFS). Ift sets some shell script variables for export #
# to child scripts, copies files, set parallel streams, executes several      #
# scripts to generate output data. The actual postprocessing is performed     #
# by the following child scripts :                                            #
#                                                                             #
#  wave_grib.sh              : generates GRIB2 files.                         #
#  wave_outp_spec.sh         : generates spectral data files for output       #
#                             locations.                                      #
#  wave_outp_bull.sh         : generates bulletins for output locations.      #
#                             grids for backward compatibility                #
#  wave_tar.sh               : tars the spectral and bulletin multiple files  #
#  wave_grid_interp.sh       : interpolates data from new grids to old grids  #
#                                                                             #
# Remarks :                                                                   #
# - The above scripts are (mostly) run using mpiserial or cfp.                #
#   Each script runs in its own directory created in DATA.                    # 
# - For non-fatal errors output is witten to the wave.log file.               #
#                                                                             #
# Origination  : 03/09/2007                                                   #
#                                                                             #
# Update log                                                                  #
# Mar2007 HTolman - Added NCO note on resources on mist/dew                   #
# Apr2007 HTolman - Renaming mod_def files in $FIX_wave.                      #
# Mar2011 AChawla - Migrating to a vertical structure                         #
# Nov2012 JHAlves - Transitioning to WCOSS                                    #
# Apr2019 JHAlves - Transitioning to GEFS workflow                            #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
# Dec2019 JHAlves - Creating side-by-side version                             #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $DATA

  postmsg "$jlogfile" "HAS BEGUN on `hostname`"

  msg="Starting WAVE POSTPROCESSOR SCRIPT for $WAV_MOD_TAG"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                     *********************************'
  echo '                     *** WAVE POSTPROCESSOR SCRIPT ***'
  echo '                     *********************************'
  echo ' '
  echo "Starting at : `date`"
  echo '-------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
  if [ -z ${NTASKS} ]        
  then
    echo "FATAL ERROR: requires NTASKS to be set "
    err=1; export err;${errchk}
    exit $err
  fi

# 0.c Defining model grids

  uoutpGRD=${uoutpGRD:?buoyNotSet}

# 0.c.1 Grids

  export waveGRD=${waveGRD?Var waveGRD Not Set}
  export sbsGRD=${sbsGRD?Var sbsGRD Not Set}

# 0.c.3 extended global grid and rtma transfer grid
  export interpGRD=${interpGRD?Var postGRD Not Set}
  export postGRD=${postGRD?Var postGRD Not Set}

# 0.c.4 Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then 
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/ibp
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull

  set +x
  echo ' '
  echo 'Grid information  :'
  echo '-------------------'
  echo "   Native wave grids  : $waveGRD"
  echo "   Side-by-side grids : $sbsGRD"
  echo "   Interpolated grids : $interpGRD"
  echo "   Post-process grids : $postGRD"
  echo "   Output points : $uoutpGRD"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x


# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

  fieldOK='yes'
  pointOK='yes'
   gribOK='yes'
  grintOK='yes'
   specOK='yes'
   bullOK='yes'

  exit_code=0

  set +x
  echo ' '
  echo 'Preparing input files :'
  echo '-----------------------'
  [[ "$LOUD" = YES ]] && set -x

# 1.a Model definition files and output files (set up using poe) 

# 1.a.1 Set up the parallel command tasks

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  [[ "$LOUD" = YES ]] && set -x

# Copy model definition files
  for grdID in $waveGRD $sbsGRD $postGRD $interpGRD $uoutpGRD
  do
    if [ -f "$COMIN/rundata/${MDC}.mod_def.${grdID}" ]
    then
      set +x
      echo " Mod def file for $grdID found in ${COMIN}/rundata. copying ...."
      [[ "$LOUD" = YES ]] && set -x

      cp -f $COMIN/rundata/${MDC}.mod_def.${grdID} mod_def.$grdID
      iloop=`expr $iloop + 1`

    fi

  done

  for grdID in $waveGRD $sbsGRD $postGRD $interpGRD $uoutpGRD
  do
    if [ ! -f mod_def.$grdID ]
    then
      set +x
      echo ' '
      echo '*************************************************** '
      echo " FATAL ERROR : NO MOD_DEF FILE mod_def.$grdID "
      echo '*************************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $grdID $date $cycle : mod_def file missing." >> $wavelog
      postmsg "$jlogfile" "FATAL ERROR : NO MOD_DEF file mod_def.$grdID"
      fieldOK='no'
      err=2; export err;${errchk}
      exit $err
      gribOK='no'
    else
      set +x
      echo "File mod_def.$grdID found. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC mod_def.$grdID
    fi
  done
 
# 1.c Output locations file

  rm -f buoy.loc

  if [ -f $FIXwave/wave_${NET}.buoys ]
  then
    cp -f $FIXwave/wave_${NET}.buoys buoy.loc.temp
# Reverse grep to exclude IBP points
    sed -n '/^\$.*/!p' buoy.loc.temp | grep -v IBP > buoy.loc
# Grep to include IBP points
    sed -n '/^\$.*/!p' buoy.loc.temp | grep IBP > buoy.ibp
    rm -f buoy.loc.temp
  fi

  if [ -s buoy.loc ] && [ -s buoy.ibp ]
  then
    set +x
    echo "   buoy.loc and buoy.ibp copied and processed ($FIXwave/wave_${NET}.buoys)."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '************************************* '
    echo ' FATAL ERROR : NO BUOY LOCATION FILE  '
    echo '************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$AV_MOD_ID post $date $cycle : buoy location file missing." >> $wavelog
    postmsg "$jlogfile" "FATAL ERROR : NO BUOY LOCATION FILE"
    err=3; export err;${errchk}
    exit $err
    pointOK='no'
    specOK='no'
    bullOK='no'
  fi

# 1.d Input template files

  if [ "$grintOK" = 'yes' ]
  then
    for intGRD in $interpGRD
    do
      if [ -f $FIXwave/${intGRD}_interp.inp.tmpl ]
      then
        cp -f $FIXwave/${intGRD}_interp.inp.tmpl ${intGRD}_interp.inp.tmpl
      fi
  
      if [ -f ${intGRD}_interp.inp.tmpl ]
      then
        set +x
        echo "   ${intGRD}_interp.inp.tmpl copied. Syncing to all nodes ..."
        [[ "$LOUD" = YES ]] && set -x
        $FSYNC ${intGRD}_interp.inp.tmpl
      else
        set +x
        echo ' '
        echo '*********************************************** '
        echo '*** ERROR : NO TEMPLATE FOR GRINT INPUT FILE *** '
        echo '*********************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$WAV_MOD_TAG post $date $cycle : GRINT template file missing." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRINT INPUT FILE"
        exit_code=1
        grintOK='no'
      fi
    done
  fi

  if [ "$gribOK" = 'yes' ]
  then
    for grbGRD in $interpGRD $postGRD
    do
      if [ -f $FIXwave/ww3_grib2.${grbGRD}.inp.tmpl ]
      then
        cp -f $FIXwave/ww3_grib2.${grbGRD}.inp.tmpl ww3_grib2.${grbGRD}.inp.tmpl
      fi

      if [ -f ww3_grib2.${grbGRD}.inp.tmpl ]
      then
        set +x
        echo "   ww3_grib2.${grbGRD}.inp.tmpl copied. Syncing to all nodes ..."
        [[ "$LOUD" = YES ]] && set -x
        $FSYNC ww3_grib2.inp.tmpl
      else
        set +x
        echo ' '
        echo '*********************************************** '
        echo "*** ERROR : NO TEMPLATE FOR ${grbGRD} GRIB INPUT FILE *** "
        echo '*********************************************** '
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$WAV_MOD_TAG post $date $cycle : GRIB2 template file missing." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB2 INPUT FILE"
        exit_code=2
        gribOK='no'
      fi
    done
  fi

  if [ -f $FIXwave/ww3_outp_spec.inp.tmpl ]
  then
    cp -f $FIXwave/ww3_outp_spec.inp.tmpl ww3_outp_spec.inp.tmpl
  fi

  if [ -f ww3_outp_spec.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_spec.inp.tmpl copied. Syncing to all grids ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC ww3_outp_spec.inp.tmpl
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR SPEC INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$WAV_MOD_TAG post $date $cycle : ww3_outp_spec.inp.tmpl file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR SPEC INPUT FILE"
    exit_code=3
    specOK='no'
    bullOK='no'
  fi

  if [ -f $FIXwave/ww3_outp_bull.inp.tmpl ]
  then
    cp -f $FIXwave/ww3_outp_bull.inp.tmpl ww3_outp_bull.inp.tmpl
  fi

  if [ -f ww3_outp_bull.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_bull.inp.tmpl copied. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC ww3_outp_bull.inp.tmpl
  else
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE *** '
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$WAV_MOD_TAG post $date $cycle : bulletin template file missing." >> $wavelog
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE"
    exit_code=4
    bullOK='no'
  fi

# 1.e Getting buoy information for points

  if [ "$specOK" = 'yes' ] || [ "$bullOK" = 'yes' ]
  then
    ymdh=`$NDATE -${HINDH} $CDATE`
    tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
    dtspec=3600.            # default time step (not used here)
    sed -e "s/TIME/$tstart/g" \
        -e "s/DT/$dtspec/g" \
        -e "s/POINT/1/g" \
        -e "s/ITYPE/0/g" \
        -e "s/FORMAT/F/g" \
                               ww3_outp_spec.inp.tmpl > ww3_outp.inp
   
    ln -s mod_def.$uoutpGRD mod_def.ww3
    fhr=$FHMIN
    YMD=$(echo $CDATE | cut -c1-8)
    HMS="$(echo $CDATE | cut -c9-10)0000"
    tloop=0
    tloopmax=600
    tsleep=10
    while [ ${tloop} -le ${tloopmax} ]
    do
      if [ -f $COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${uoutpGRD}.${YMD}.${HMS} ]
      then
        cp -f $COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${uoutpGRD}.${YMD}.${HMS} ./out_pnt.${uoutpGRD}
        break
      else
        sleep ${tsleep}
        tloop=$(($tloop + $tsleep))
      fi
    done
    
    rm -f buoy_tmp.loc buoy_log.ww3 ww3_oup.inp
    ln -fs ./out_pnt.${uoutpGRD} ./out_pnt.ww3
    ln -fs ./mod_def.${uoutpGRD} ./mod_def.ww3
    $EXECcode/ww3_outp > buoy_lst.loc 2>&1 
    err=$?

    if [ "$err" != '0' ] && [ ! -f buoy_log.ww3 ]
    then
      pgm=wave_post
      msg="ABNORMAL EXIT: ERROR IN ww3_outp"
      postmsg "$jlogfile" "$msg"
      set +x
      echo ' '
      echo '******************************************** '
      echo '*** FATAL ERROR : ERROR IN ww3_outp *** '
      echo '******************************************** '
      echo ' '
      cat buoy_tmp.loc 
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file failed to be created." >> $wavelog
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      err=4;export err;${errchk}
      specOK='no'
      bullOK='no'
      exit $err
    fi

# Create new buoy_log.ww3 excluding all IBP files
    cat buoy.loc | awk '{print $3}' | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.dat
    mv buoy_log.tmp buoy_log.dat

    grep -F -f ibp_tags buoy_lst.loc >  buoy_tmp1.loc
    sed -n '11,/^$/p' buoy_tmp1.loc > buoy_tmp2.loc
    sed    '$d' buoy_tmp2.loc > buoy_tmp3.loc
    buoys=`awk '{ print $1 }' buoy_tmp3.loc`
    Nb=`wc buoy_tmp3.loc | awk '{ print $1 }'`
    rm buoy_tmp1.loc buoy_tmp2.loc buoy_tmp3.loc

    if [ -s buoy_log.dat ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      $FSYNC buoy_log.dat
      [[ "$LOUD" = YES ]] && set -x
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file missing." >> $wavelog
      postmsg "$jlogfile" "FATAL ERROR : NO BUOY LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
      err=5;export err;${errchk}
      specOK='no'
      bullOK='no'
      OspecOK='no'
      ObullOK='no'
    fi

# Create new buoy_log.ww3 including all IBP files
    ibspecOK='yes'
    cat buoy.ibp | awk '{print $3}' | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.ibp
    mv buoy_log.tmp buoy_log.ibp

    grep -F -f ibp_tags buoy_lst.loc >  buoy_tmp1.loc
    sed -n '11,/^$/p' buoy_tmp1.loc > buoy_tmp2.loc
    sed    '$d' buoy_tmp2.loc > buoy_tmp3.loc
    ibpoints=`awk '{ print $1 }' buoy_tmp3.loc`
    Nibp=`wc buoy_tmp3.loc | awk '{ print $1 }'`
    rm buoy_tmp1.loc buoy_tmp2.loc buoy_tmp3.loc

    if [ -s buoy_log.ibp ]
    then
      set +x
      echo 'IBP  log file created. Syncing to all nodes ...'
      $FSYNC buoy_log.ibp
      [[ "$LOUD" = YES ]] && set -x
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO  IBP LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : ibp  log file missing." >> $wavelog
      postmsg "$jlogfile" "FATAL ERROR : NO  IBP LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
      err=6;export err;${errchk}
      ibspecOK='no'
    fi

  fi

# 1.f Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : `date`"
  echo ' ' 
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "      Sufficient data for GRID interpolation    : $grintOK"
  echo "      Sufficient data for GRIB files            : $gribOK"
  echo "      Sufficient data for spectral files        : $specOK ($Nb points)"
  echo "      Sufficient data for bulletins             : $bullOK ($Nb points)"
  echo "      Sufficient data for Input Boundary Points : $ibspecOK ($Nibp points)"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 2.  Make consolidated grib2 file for side-by-side grids and interpolate
#     onto extended grids
#
# 2.a Command file set-up

  set +x
  echo '   Making command file for sbs grib2 and GRID Interpolation '
  [[ "$LOUD" = YES ]] && set -x

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 1.a.2 Loop over forecast time to generate post files 
# When executed side-by-side, serial mode (cfp when run after the fcst step)
  fhr=$FHMIN
  fhrp=$fhr
  fhrg=$fhr
  iwaitmax=120 # Maximum loop cycles for waiting until wave component output file is ready (fails after max)
  while [ $fhr -le $FHMAXWAV ]; do
    
    ymdh=`$NDATE $fhr $CDATE`
    YMD=$(echo $ymdh | cut -c1-8)
    HMS="$(echo $ymdh | cut -c9-10)0000"
    YMDHMS=${YMD}${HMS}
    FH3=$(printf %03i $fhr)

    fcmdnow=cmdfile.${FH3}
    fcmdigrd=icmdfile.${FH3}
    fcmdpnt=pcmdfile.${FH3}
    fcmdibp=ibpcmdfile.${FH3}
    rm -f ${fcmdnow} ${fcmdigrd} ${fcmdpnt} ${fcmdibp}
    touch ${fcmdnow} ${fcmdigrd} ${fcmdpnt} ${fcmdibp}
#    echo "mkdir output_$YMDHMS" >> ${fcmdnow}
    mkdir output_$YMDHMS
#    echo "cd output_$YMDHMS" >> ${fcmdnow}
    cd output_$YMDHMS
# Create instances of directories for spec and gridded output
    export SPECDATA=${DATA}/output_$YMDHMS
    export BULLDATA=${DATA}/output_$YMDHMS
    export GRIBDATA=${DATA}/output_$YMDHMS
    export GRDIDATA=${DATA}/output_$YMDHMS
#    echo "ln -fs $DATA/mod_def.${uoutpGRD} mod_def.ww3" >> ${fcmdnow}
    ln -fs $DATA/mod_def.${uoutpGRD} mod_def.ww3

    if [ $fhr = $fhrp ]
    then
      iwait=0
      pfile=$COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${uoutpGRD}.${YMD}.${HMS}
      while [ ! -s ${pfile} ]; do sleep 10; ((iwait++)) && ((iwait==$iwaitmax)) && break ; echo $iwait; done
      if [ $iwait -eq $iwaitmax ]; then 
        echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.$uoutpGRD
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$WAV_MOD_TAG post $uoutpGRD $date $cycle : point output missing." >> $wavelog
        postmsg "$jlogfile" "FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.$uoutpGRD
        err=6; export err;${errchk}
        exit $err
      fi
#    echo "cp -f ${pfile} ./out_pnt.${uoutpGRD} > cpoutp_$uoutpGRD.out 2>&1" >> ${fcmdnow}
      cp -f ${pfile} ./out_pnt.${uoutpGRD} > cpoutp_$uoutpGRD.out 2>&1

      if [ "$specOK" = 'yes' ]
      then
        export dtspec=3600.
        for buoy in $buoys
        do
          echo "$USHwave/wave_outp_spec.sh $buoy $ymdh spec > spec_$buoy.out 2>&1" >> ${fcmdnow}
        done
      fi
  
      if [ "$ibspecOK" = 'yes' ]
      then
        export dtspec=3600.
        for buoy in $ibpoints
        do
          echo "$USHwave/wave_outp_spec.sh $buoy $ymdh ibp > ibp_$buoy.out 2>&1" >> ${fcmdnow}
        done
      fi

      if [ "$bullOK" = 'yes' ]
      then
        export dtspec=3600.
        for buoy in $buoys
        do
          echo "$USHwave/wave_outp_spec.sh $buoy $ymdh bull > bull_$buoy.out 2>&1" >> ${fcmdnow}
        done
      fi

    fi

    if [ $fhr = $fhrg ]
    then
      for wavGRD in ${waveGRD} ; do
        gfile=$COMIN/rundata/${WAV_MOD_TAG}.out_grd.${wavGRD}.${YMD}.${HMS}
        while [ ! -s ${gfile} ]; do sleep 10; done
        if [ $iwait -eq $iwaitmax ]; then 
          echo '*************************************************** '
          echo " FATAL ERROR : NO RAW FIELD OUTPUT FILE out_grd.$grdID "
          echo '*************************************************** '
          echo ' '
          [[ "$LOUD" = YES ]] && set -x
          echo "$WAV_MOD_TAG post $grdID $date $cycle : field output missing." >> $wavelog
          postmsg "$jlogfile" "NON-FATAL ERROR : NO RAW FIELD OUTPUT FILE out_grd.$grdID"
          fieldOK='no'
          err=7; export err;${errchk}
          exit $err
        fi
#      echo "cp -f ${gfile} ./out_grd.${wavGRD} > cpoutg_$wavGRD.out 2>&1" >> ${fcmdnow}
        cp -f ${gfile} ./out_grd.${wavGRD} > cpoutg_$wavGRD.out 2>&1
      done

      if [ "$grintOK" = 'yes' ]
      then
        nigrd=1
        for grdID in $interpGRD
        do
          case $grdID in
            glo_15mxt) ymdh_int=`$NDATE -${HINDH} $ymdh`; dt_int=3600.; n_int=9999 ;;
            glo_30mxt) ymdh_int=`$NDATE -${HINDH} $ymdh`; dt_int=3600.; n_int=9999 ;;
          esac
          echo "$USHwave/wave_grid_interp_sbs.sh $grdID $ymdh_int $dt_int $n_int > grint_$grdID.out 2>&1" >> ${fcmdigrd}.${nigrd}
          if [ "$gribOK" = 'yes' ]
          then
          gribFL=\'`echo ${OUTPARS}`\'
            case $grdID in
              glo_15mxt) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=255 ;;
              glo_30mxt) GRDNAME='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=255 ;;
            esac
            echo "$USHwave/wave_grib2_sbs.sh $grdID $GRIDNR $MODNR $ymdh $fhr $GRDNAME $GRDRES $gribFL > grib_$grdID.out 2>&1" >> ${fcmdigrd}.${nigrd}
          fi
          echo "${fcmdigrd}.${nigrd}" >> ${fcmdnow}
          chmod 744 ${fcmdigrd}.${nigrd}
          nigrd=$((nigrd+1)) 
        done
      fi

      if [ "$gribOK" = 'yes' ]
      then
        for grdID in ${postGRD} # First concatenate grib files for sbs grids
        do
          gribFL=\'`echo ${OUTPARS}`\'
          case $grdID in
              aoc_9km) GRDNAME='arctic' ; GRDRES=9km ; GRIDNR=255  ; MODNR=255  ;;
              ant_9km) GRDNAME='antarc' ; GRDRES=9km ; GRIDNR=255  ; MODNR=255  ;;
              glo_10m) GRDNAME='global' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=255  ;;
              glo_15m) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=255  ;;
              ao_20m) GRDNAME='arctic' ; GRDRES=0p33 ; GRIDNR=255  ; MODNR=255  ;;
              so_20m) GRDNAME='antarc' ; GRDRES=0p33 ; GRIDNR=255  ; MODNR=255  ;;
              glo_15mxt) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=255  ;;
          esac
          echo "$USHwave/wave_grib2_sbs.sh $grdID $GRIDNR $MODNR $ymdh $fhr $GRDNAME $GRDRES $gribFL > grib_$grdID.out 2>&1" >> ${fcmdnow}
        done
      fi

    fi

    wavenproc=`wc -l ${fcmdnow} | awk '{print $1}'`
    wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

    set +x
    echo ' '
    echo "   Executing the copy command file at : `date`"
    echo '   ------------------------------------'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x

    if [ "$wavenproc" -gt '1' ]
    then
      ${wavempexec} ${wavenproc} ${wave_mpmd} ${fcmdnow}
      exit=$?
    else
      chmod 744 ${fcmdnow}
      ./${fcmdnow}
      exit=$?
    fi

    if [ "$exit" != '0' ]
    then
      set +x
      echo ' '
      echo '********************************************'
      echo '*** CMDFILE FAILED   ***'
      echo '********************************************'
      echo '     See Details Below '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      err=8; export err;${errchk}
      exit $err
    fi

    cd $DATA

    FHINCP=$(( DTPNT / 3600 ))
    FHINCG=$(( DTFLD / 3600 ))
    if [ $fhr = $fhrg ]
    then
      if [ $FHMAX_HF -gt 0 ] && [ $FHOUT_HF -gt 0 ] && [ $fhr -lt $FHMAX_HF ]; then
        FHINCG=$FHOUT_HF
      fi
      fhrg=$((fhr+FHINCG))
    fi
    if [ $fhr = $fhrp ]
    then
      fhrp=$((fhr+FHINCP))
    fi
    echo $fhrg $fhrp
    fhr=$([ $fhrg -le $fhrp ] && echo "$fhrg" || echo "$fhrp") # reference fhr is the least between grid and point stride
  done

# --------------------------------------------------------------------------- #
# 3. Compress point output data into tar files

# 3.a Set up cmdfile

  rm -f cmdtarfile
  touch cmdtarfile
  chmod 744 cmdtarfile

  set +x
  echo ' '
  echo '   Making command file for taring all point output files.'

  [[ "$LOUD" = YES ]] && set -x

# 6.b Spectral data files

  if [ "$specOK" = 'yes' ]
  then
    echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibp $Nibp > ${WAV_MOD_TAG}_ibp_tar.out 2>&1 "   >> cmdtarfile
    echo "$USHwave/wave_tar.sh $WAV_MOD_TAG spec $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
    echo "$USHwave/wave_tar.sh $WAV_MOD_TAG bull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
    echo "$USHwave/wave_tar.sh $WAV_MOD_TAG cbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
  fi

    wavenproc=`wc -l cmdtarfile | awk '{print $1}'`
    wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

    set +x
    echo ' '
    echo "   Executing the copy command file at : `date`"
    echo '   ------------------------------------'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x

    if [ "$wavenproc" -gt '1' ]
    then
      ${wavempexec} ${wavenproc} ${wave_mpmd} cmdtarfile
      exit=$?
    else
      chmod 744 cmdtarfile
      ./cmdtarfile
      exit=$?
    fi

    if [ "$exit" != '0' ]
    then
      set +x
      echo ' '
      echo '********************************************'
      echo '*** CMDFILE FAILED   ***'
      echo '********************************************'
      echo '     See Details Below '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      err=8; export err;${errchk}
      exit $err
    fi

exit

# --------------------------------------------------------------------------- #
# 4. Post-process (concatenate) point outputs: spectra, bulletins 
#

# 4.a Set up cmdfile

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 4.b Spectral data files

  set +x; [ "$LOUD" = YES -a "$specOK" = 'yes' ] 
  if [ "$specOK" = 'yes' ]
  then
    export dtspec=$DTPNT   # time step for spectra
    ymdh=`$NDATE -${HINDH} $CDATE` # start time for spectra output

    ifile=1
    ilayer=1
    for buoy in $buoys
    do
      echo "$USHwave/wave_outp_spec.sh $buoy $ymdh > spec_$buoy.out 2>&1" >> cmdfile
    done
  fi
  [[ "$LOUD" = YES ]] && set -x

# 4.c Bulletins

  set +x; [ "$LOUD" = YES -a "$bullOK" = 'yes' ] 
  if [ "$bullOK" = 'yes' ]
  then
    export dtbull=3600.    # time step for bulletins
    ymdh=`$NDATE -${HINDH} $CDATE` # start time for bulletin output
   
    for buoy in $buoys
    do
      echo "$USHwave/wave_outp_bull.sh $buoy $ymdh > bull_$buoy.out 2>&1" >> cmdfile
    done
  fi
  [[ "$LOUD" = YES ]] && set -x

# 4.d Execute point output cmd file

# Determine number of processes needed for mpmd
  cat cmdfile

  wavenproc=`wc -l cmdfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  set +x
  echo "   Executing point output command file at : `date`"
  echo '   ---------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$wavenproc" -gt '1' ]
  then
    ${wavempexec} ${wavenproc} ${wave_mpmd} cmdfile
    exit=$?
  else
    ./cmdfile
    exit=$?
  fi

# 4.e Check for errors

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** CMDFILE FAILED TO GENERATE SPEC AND BULLETIN FILES  ***'
    echo '***********************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    err=10;export err;${errchk}
    exit $err
  fi

# --------------------------------------------------------------------------- #
# 5.  General error checking

  set +x
  echo ' '
  echo '   Checking for errors (error output concatenated below).'
  [[ "$LOUD" = YES ]] && set -x

# 5.a Grid interpolation

  if [ "$grintOK" = 'yes' ]
  then
    for grdID in $interpGRD
    do
      if [ -d grint_$grdID ]
      then
        set +x
        echo "      Error in GRID interpolation for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRID interpolation for $grdID."
        exit_code=6
        mv -f grint_$grdID.out grint_$grdID.err 
      else
        set +x
        echo "      GRID interpolation successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi

      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
        exit_code=7
        mv -f grib_$grdID.out grib_$grdID.err 
      else
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 5.b GRIB file

  if [ "$gribOK" = 'yes' ]
  then
    for grdID in $sbsGRD $postGRD
    do
      if [ -d grib_$grdID ]
      then
        set +x
        echo "      Error in GRIB encoding for $grdID."
        [[ "$LOUD" = YES ]] && set -x
        postmsg "$jlogfile" "NON-FATAL ERROR in GRIB encoding for $grdID."
        exit_code=8
        mv -f grib_$grdID.out grib_$grdID.err
      else
        set +x
        echo "      GRIB encoding successful for $grdID."
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 5.c Spectral data files and bulletins

  set +x

  if  [ "$specOK" = 'yes' ]
  then
    if ls spec_* 1> /dev/null 2>&1
    then
      for buoy in $buoys
      do
        if [ -d spec_$buoy ]
        then
          specstring='Error in spectra.'
          postmsg "$jlogfile" "NON-FATAL ERROR in spectra."
          exit_code=9
          mv -f spec_$buoy.out spec_$buoy.err
        fi
      done
    else
      echo " Spectra OK "
    fi
  fi
 
  if  [ "$bullOK" = 'yes' ]
  then
    if ls bull_* 1> /dev/null 2>&1
    then
      for buoy in $buoys
      do
        if [ -d bull_$buoy ]
        then
          specstring='Error in spectra.'
          postmsg "$jlogfile" "NON-FATAL ERROR in spectra."
          exit_code=10
          mv -f bull_$buoy.out bull_$buoy.err
        fi
      done
    else
      echo " Bulletins OK "
    fi
  fi

  [[ "$LOUD" = YES ]] && set -x

  if ls *.err 1> /dev/null 2>&1
  then
    for grdID in $waveGRD $sbsGRD $postGRD
    do 
      if [ -f grib_$grdID.err ]
      then
        set +x
        echo ' '
        echo '**************************************'
        echo '*** ERROR OUTPUT wave_grib2.sh ***'
        echo '**************************************'
        echo ' '
        [[ "$LOUD" = YES ]] && set -x
        echo "$WAV_MOD_TAG post $date $cycle : error in GRIB." >> $wavelog
        postmsg "$jlogfile" "NON-FATAL ERROR in wave_grib2.sh"
        err=11;export err;${errchk}
        sed "s/^/grib_$grdID.err : /g"  grib_$grdID.err
      fi

    done

    if ls spec_*.err 1> /dev/null 2>&1
    then
      set +x
      echo ' '
      echo '*************************************'
      echo '*** ERROR OUTPUT wave_outp.sh ***'
      echo '*************************************'
      echo '            Possibly in multiple calls'
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : error in spectra." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in wave_outp.sh, possibly in multiple calls."
      err=12;export err;${errchk}
      for file in spec_*.err
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
    fi

    if ls bull_*.err 1> /dev/null 2>&1
    then
      set +x
      echo ' '
      echo '******************************************'
      echo '*** ERROR OUTPUT wave_outp_bull.sh ***'
      echo '******************************************'
      echo '            Possibly in multiple calls'
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : error in bulletins." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in wave_bull.sh, possibly in multiple calls."
      err=13;export err;${errchk}
      for file in bull_*.err
      do
        echo ' '
        sed "s/^/$file : /g" $file
      done
    fi
  fi



# --------------------------------------------------------------------------- #
# 6. Compress point output data into tar files

# 6.a Set up cmdfile

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  set +x
  echo ' '
  echo '   Making command file for taring all point output files.'

  [[ "$LOUD" = YES ]] && set -x

# 6.b Spectral data files

  if [ "$specOK" = 'yes' ]
  then
    echo "$USHwave/wave_tar.sh $WAV_MOD_TAG spec $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdfile

  fi

# 6.c Bulletins

  if [ "$bullOK" = 'yes' ]
  then
    echo "$USHwave/wave_tar.sh $WAV_MOD_TAG bull $Nb > ${WAV_MOD_TAG}_bull_tar.out 2>&1 "   >> cmdfile

  fi

# 6.d Compressed bulletins

  if [ "$bullOK" = 'yes' ]
  then
     echo "$USHwave/wave_tar.sh $WAV_MOD_TAG cbull $Nb > ${WAV_MOD_TAG}_cbull_tar.out 2>&1 " >> cmdfile
  fi

# 6.e Execute fourth command file

# Set number of processes for mpmd
  cat cmdfile

  wavenproc=`wc -l cmdfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

# 1.a.3 Execute the serial or parallel cmdfile

  set +x
  echo ' '
  echo "   Executing the tar command file at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$wavenproc" -gt '1' ]
  then
    ${wavempexec} ${wavenproc} ${wave_mpmd} cmdfile
    exit=$?
  else
    ./cmdfile
    exit=$?
  fi

# 6.f Check for errors

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '***************************************************'
    echo '*** FATAL ERROR: CMD FAILURE DURING TAR PROCESS ***'
    echo '***************************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    exit_code=$exit
    err=14; export err;${errchk}
    exit $err
  fi


# 6.g  Check further for errors in file generation`

  set +x
  echo ' '
  echo '   Checking for errors (error output concatenated below).'
  [[ "$LOUD" = YES ]] && set -x

# 6.g.1 Spectral tar file

  if [ "$specOK" = 'yes' ]
  then
    if [ -d TAR_spec_$WAV_MOD_TAG ]
    then
      set +x
      echo "      Error in $WAV_MOD_TAG spectral tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : error in spectral tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $WAV_MOD_TAG spectral tar file."
      exit_code=11
      mv ${WAV_MOD_TAG}_spec_tar.out ${WAV_MOD_TAG}_spec_tar.err
    else
      set +x
      echo "      $WAV_MOD_TAG Spectral tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi
  fi

# 6.g.2 Bulletin tar files

  if [ "$bullOK" = 'yes' ]
  then
    if [ -d TAR_bull_$WAV_MOD_TAG ]
    then
      set +x
      echo "      Error in $WAV_MOD_TAG bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : error in bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $WAV_MOD_TAG bulletin tar file."
      exit_code=12
      mv -f ${WAV_MOD_TAG}_bull_tar.out ${WAV_MOD_TAG}_bull_tar.err
    else
      set +x
      echo "      $WAV_MOD_TAG Bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

    if [ -d TAR_cbull_$WAV_MOD_TAG ]
    then
      set +x
      echo "      Error in $WAV_MOD_TAG compressed bulletin tar file."
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : error in compressed bulletin tar." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR in $WAV_MOD_TAG compressed bulletin tar file."
      exit_code=13
      mv -f ${WAV_MOD_TAG}_cbull_tar.out ${WAV_MOD_TAG}_cbull_tar.err
    else
      set +x
      echo "      $WAV_MOD_TAG compressed bulletin tar file OK."
      [[ "$LOUD" = YES ]] && set -x
    fi

  fi

# 7. Check if any left over error output

  if ls *.err 1> /dev/null 2>&1
  then
    set +x
    echo ' '
    echo '*********************'
    echo '*** ERROR OUTPUTS ***'
    echo '*********************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    for file in *.err
    do
      echo ' '
      sed "s/^/$file : /g" $file
    done
    err=15;export err;${errchk}
  fi

# --------------------------------------------------------------------------- #
# 8. Clean up and rename old grid files

  set +x
  rm -f *.tmpl
  for ID in $WAV_MOD_TAG
  do
    rm -f $ID.*.spec
    rm -f $ID.*.bull
    rm -f $ID.*.cbull
  done
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 9.  Ending output

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo '-----------'
  echo ' '
  echo '                     *** End of MWW3 postprocessor ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$exit_code" -ne '0' ]
  then
    msg="ABNORMAL EXIT: Problem in MWW3 POST"
    postmsg "$jlogfile" "$msg"
    echo $msg
    err=16; export err;${errchk}
    exit $err
  else
    echo " Wave Post Completed Normally "
    msg="$job completed normally"
    postmsg "$jlogfile" "$msg"
    exit 0
  fi

# End of MWW3 prostprocessor script ---------------------------------------- #
