#!/bin/bash
###############################################################################
#                                                                             #
# This script is the postprocessor for the multi scale  WW3 wave model. It    #
# sets some shell script variables for export to child scripts and copies     #
# some generally used files to the work directory. After this the actual      #
# postprocessing is performed by the following child scripts :                #
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
#   Each script runs in its own directory created in DATA. If all is well     #
#   this directory disappears. If this directory is still there after poe     #
#   has finished, an error has occured Only then the output of the process    #
#   is copied to the output file. Otherwise, the output is deleted.           #
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

  msg="Starting MWW3 POSTPROCESSOR SCRIPT for $WAV_MOD_TAG"
  postmsg "$jlogfile" "$msg"

  set +x
  echo ' '
  echo '                     *********************************'
  echo '                     *** MWW3 POSTPROCESSOR SCRIPT ***'
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

# 0.b Date and time stuff

  export date=$PDY
  export YMDH=${PDY}${cyc}

# 0.c Defining model grids

  uoutpGRD=${uoutpGRD:?uoutpGRDNotSet}

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
   bullOK='no '

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

# 1.a.2 Create entries in cmdfile for copying raw data files

  iloop=1
  for grdID in $waveGRD $sbsGRD
  do
  
    if [ ! -f out_grd.$grdID ]
    then
      set +x
      echo "   Copying $WAV_MOD_TAG.out_grd.$grdID.$PDY$cyc from ${COMIN}/rundata to out_grd.$grdID"
      [[ "$LOUD" = YES ]] && set -x

      echo  "cp -f $COMIN/rundata/$WAV_MOD_TAG.out_grd.$grdID.$PDY$cyc out_grd.$grdID > copycmd.out.$iloop 2>&1"  >> cmdfile
      iloop=`expr $iloop + 1`
    fi 
  done

  for grdID in $waveGRD $sbsGRD $postGRD $interpGRD $uoutpGRD
  do
    if [ -f "$COMIN/rundata/${MDC}.mod_def.${grdID}" ]
    then
      set +x
      echo " Mod def file for $grdID found in ${COMIN}/rundata. copying ...."
      [[ "$LOUD" = YES ]] && set -x

      echo "cp -f $COMIN/rundata/${MDC}.mod_def.${grdID} mod_def.$grdID > copycmd.out.$iloop 2>&1"  >> cmdfile
      iloop=`expr $iloop + 1`
      
    fi

  done

  if [ ! -f out_pnt.ww3 ]
  then
    set +x
    echo "   Copying $COMIN/rundata/$WAV_MOD_TAG.out_pnt.${uoutpGRD}.$PDY$cyc to out_pnt.ww3"
    [[ "$LOUD" = YES ]] && set -x
    echo "cp -f $COMIN/rundata/$WAV_MOD_TAG.out_pnt.${uoutpGRD}.$PDY$cyc out_pnt.ww3 > copycmd.out.$iloop 2>&1" >> cmdfile
    iloop=`expr $iloop + 1`
  fi


# Set number of processes for mpmd
    cat cmdfile

    wavenproc=`wc -l cmdfile | awk '{print $1}'`
    wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

# 1.a.3 Execute the serial or parallel cmdfile

  set +x
  echo ' '
  echo "   Executing the copy command file at : `date`"
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

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** POE FAILURE DURING RAW DATA COPYING ***'
    echo '********************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi

# 1.a.4 Error checks

  for grdID in $waveGRD
  do
    if [ ! -f out_grd.$grdID ]
    then
      set +x
      echo ' '
      echo '*************************************************** '
      echo " FATAL ERROR : NO RAW FIELD OUTPUT FILE out_grd.$grdID "
      echo '*************************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $grdID $date $cycle : field output missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO RAW FIELD OUTPUT FILE out_grd.$grdID"
      fieldOK='no'
      err=2; export err;${errchk}
      exit $err
      gribOK='no'
    else
      set +x
      echo "File out_grd.$grdID found. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC out_grd.$grdID
    fi
  done

  if [ -f out_pnt.ww3 ]
  then
    set +x
    echo "   out_pnt.ww3 exists. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
    $FSYNC out_pnt.ww3
  else
    set +x
    echo ' '
    echo '**************************************** '
    echo ' FATAL ERROR : NO RAW POINT OUTPUT FILE  '
    echo '**************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$WAV_MOD_TAG post $date $cycle : point output missing." >> $wavelog
    postmsg "$jlogfile" " FATAL ERROR NO RAW POINT OUTPUT FILE"
    err=3; export err;${errchk}
    exit $err
    pointOK='no'
    specOK='no'
    bullOK='no'
    OspecOK='no'
    Obull_ok='no'
  fi


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
    rm -f buoy.loc.temp
  fi

  if [ -f buoy.loc ]
  then
    set +x
    echo "   buoy.loc copied and processed ($FIXwave/wave_${NET}.buoys)."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '************************************* '
    echo ' FATAL ERROR : NO BUOY LOCATION FILE  '
    echo '************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$WAV_MOD_TAG post $date $cycle : buoy location file missing." >> $wavelog
    postmsg "$jlogfile" "FATAL ERROR : NO BUOY LOCATION FILE"
    err=5; export err;${errchk}
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
    if [ -f $FIXwave/ww3_grib2.inp.tmpl ]
    then
      cp -f $FIXwave/ww3_grib2.inp.tmpl ww3_grib2.inp.tmpl
    fi

    if [ -f ww3_grib2.inp.tmpl ]
    then
      set +x
      echo "   ww3_grib2.inp.tmpl copied. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC ww3_grib2.inp.tmpl
    else
      set +x
      echo ' '
      echo '*********************************************** '
      echo '*** ERROR : NO TEMPLATE FOR GRIB INPUT FILE *** '
      echo '*********************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : GRIB2 template file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR GRIB2 INPUT FILE"
      exit_code=2
      gribOK='no'
    fi
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
    ymdh=`$NDATE -${HINDH} $YMDH`
    tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
    dtspec=3600.            # default time step (not used here)
    sed -e "s/TIME/$tstart/g" \
        -e "s/DT/$dtspec/g" \
        -e "s/POINT/1/g" \
        -e "s/ITYPE/0/g" \
        -e "s/FORMAT/F/g" \
                               ww3_outp_spec.inp.tmpl > ww3_outp.inp
   
    ln -s mod_def.$uoutpGRD mod_def.ww3
    rm -f ww3_oup.inp
    $EXECcode/ww3_outp > buoy_tmp.loc 
    err=$?

    if [ "$err" != '0' ]
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
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file failed to be created." >> $wavelog
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      err=6;export err;${errchk}
      specOK='no'
      bullOK='no'
      exit $err
    fi

# Create new buoy_log.ww3 excluding all IBP files
    cat buoy.loc | awk '{print $3}' | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.ww3
    mv buoy_log.tmp buoy_log.ww3

    grep -F -f ibp_tags buoy_tmp.loc >  buoy_tmp1.loc
    sed -n '11,/^$/p' buoy_tmp1.loc > buoy_tmp2.loc
    sed    '$d' buoy_tmp2.loc > buoy_tmp3.loc
    buoys=`awk '{ print $1 }' buoy_tmp3.loc`
    Nb=`wc buoy_tmp3.loc | awk '{ print $1 }'`
    rm buoy_tmp.loc buoy_tmp1.loc buoy_tmp2.loc buoy_tmp3.loc

    if [ -f buoy_log.ww3 ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      [[ "$LOUD" = YES ]] && set -x
      $FSYNC buoy_log.ww3
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file missing." >> $wavelog
      postmsg "$jlogfile" "NON-FATAL ERROR : NO BUOY LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
      exit_code=5
      specOK='no'
      bullOK='no'
      OspecOK='no'
      ObullOK='no'
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
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 2.  Make consolidated grib2 file for side-by-side grids and interpolate
#     onto extended grids
#
# 2.a Command file set-up

  set +x
  echo '   Making command file for sbs grib2 and GRID Interpolation '

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

  [[ "$LOUD" = YES ]] && set -x

# 2.b Generate GRIB files for side-by-side grid (if any)s

# GRIB field time step -- dtgrib
# Number of GRIB fields -- ngrib
# Assigned NCEP number for grid -- GRIDNR
# Assigned NCEP number for model -- MODNR

  if [ "$gribOK" = 'yes' ]
  then
    for grdID in $sbsGRD # First concatenate grib files for sbs grids
    do

      case $grdID in
        glo_15m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        ao_30m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        so_30m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        glo_30m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        glo_15mxt) gribFL=\'${OUTPARS}\';
                  GRIDNR=11  ; MODNR=255 ; dtgrib=10800. ; ngrib=181 ;;
        glo_30mxt) gribFL=\'${OUTPARS}\';
                  GRIDNR=11  ; MODNR=11  ; dtgrib=3600. ; ngrib=181 ;;
      esac

# Recalculate ngrib based on FHMAXWAV (TODO: add new interval if changes to dtgrib after given forecast hour)
      dtgi=`echo ${dtgrib} | sed 's/\.//g'`
      dtgh=`expr ${dtgi} / 3600`
      ngrib=`expr ${FHMAXWAV} / ${dtgh} + 1`

      echo "$USHwave/wave_grib2_cat.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFL > grib_$grdID.out 2>&1"               >> cmdfile

    done

  fi

# 2.c Generate binary raw data for extended, interpolated grids (if any)

  if [ "$grintOK" = 'yes' ]
  then
    for grdID in $interpGRD
    do
      case $grdID in
      glo_15mxt) ymdh_int=`$NDATE -${HINDH} $YMDH`; dt_int=3600.; n_int=9999 ;;
      glo_30mxt) ymdh_int=`$NDATE -${HINDH} $YMDH`; dt_int=3600.; n_int=9999 ;;
      esac

      echo "$USHwave/wave_grid_interp.sh $grdID $ymdh_int $dt_int $n_int > grint_$grdID.out 2>&1" >> cmdfile
    done
  fi

# Determine number of processes needed for mpmd
  cat cmdfile

  wavenproc=`wc -l cmdfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

# 2.d Execute sbs grib2 and interpolation grids cmdfile

  set +x
  echo "   Executing sbs grib2 and interpolated grids raw cmdfile at : `date`"
  echo '   ----------------------------------------------------------'
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

# 2.e Check for errors

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** CMDFILE FAILED TO GENERATE SBS GRIB2 AND GINT FILES ***'
    echo '***********************************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    err=7;export err;${errchk}
    exit $err
  fi

# --------------------------------------------------------------------------- #
# 3. Generate grib2 filed for postprocessed grids, including exetend grids (if)
#

# 3.a Prepare cmdfile

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 3.b Generate post grib2 files

  if [ "$gribOK" = 'yes' ]
  then
    for grdID in $postGRD # First concatenate grib files for sbs grids
    do

      case $grdID in
        glo_15m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        ao_30m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        so_30m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        glo_30m) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255  ; dtgrib=10800. ;;
        glo_15mxt) gribFL=\'${OUTPARS}\';
                  GRIDNR=255  ; MODNR=255 ; dtgrib=10800. ; ngrib=181 ;;
        glo_30mxt) gribFL=\'${OUTPARS}\';
                  GRIDNR=11  ; MODNR=11  ; dtgrib=3600. ; ngrib=181 ;;
      esac

# Recalculate ngrib based on FHMAXWAV (TODO: add new interval if changes to dtgrib after given forecast hour)
      dtgi=`echo ${dtgrib} | sed 's/\.//g'`
      dtgh=`expr ${dtgi} / 3600`
      ngrib=`expr ${FHMAXWAV} / ${dtgh} + 1`

      echo "$USHwave/wave_grib2.sh $grdID $dtgrib $ngrib $GRIDNR $MODNR $gribFL > grib_$grdID.out 2>&1"  >> cmdfile

    done

  fi

# 3.c Run mpmd cmdfile

# Determine number of processes needed for mpmd
  cat cmdfile

  wavenproc=`wc -l cmdfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  set +x
  echo "   Executing gridded command file at : `date`"
  echo '   ----------------------------------'
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

# 3.d Check for errors

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '***********************************************************'
    echo '*** CMDFILE FAILED TO GENERATE POST GRIB2 FILES ***********'
    echo '***********************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    err=8;export err;${errchk}
    exit_code=$exit
    exit $err
  fi

# --------------------------------------------------------------------------- #
# 4. Point outputs: files containing spectra, bulletins 
#

# 4.a Set up cmdfile

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 4.b Spectral data files

  set +x; [ "$LOUD" = YES -a "$specOK" = 'yes' ] 
  if [ "$specOK" = 'yes' ]
  then
    export dtspec=10800.   # time step for spectra
    ymdh=`$NDATE -${HINDH} $YMDH` # start time for spectra output

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
    ymdh=`$NDATE -${HINDH} $YMDH` # start time for bulletin output
   
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
    err=9;export err;${errchk}
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
        err=10;export err;${errchk}
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
      err=11;export err;${errchk}
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
      err=12;export err;${errchk}
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
    err=13; export err;${errchk}
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
    err=14;export err;${errchk}
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
    err=15; export err;${errchk}
    exit $err
  else
    echo " Wave Post Completed Normally "
    msg="$job completed normally"
    postmsg "$jlogfile" "$msg"
    exit 0
  fi

# End of MWW3 prostprocessor script ---------------------------------------- #
