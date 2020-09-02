#!/bin/bash
#
################################################################################
#
# UNIX Script Documentation Block
# Script name:         exgfs_wave_post_bndpnt.sh
# Script description:  Creates output products from binary WW3 data
#
# Author:   Jose-Henrique Alves Org: NCEP/EMC      Date: 2019-12-06
# Abstract: This script is the postprocessor for the wave component in GFS.
#           This version runs side-by-side with the GFS fcst step. 
#           It executes several scripts forpreparing and creating output data
#           as follows:
#
#  wave_outp_spec.sh         : generates spectral data for output locations.                                      
#  wave_tar.sh               : tars the spectral and bulletin multiple files  
#
# Script history log:
# 2019-12-06  J-Henrique Alves: First Version adapted from HTolman post.sh 2007 
# 2020-06-10  J-Henrique Alves: Porting to R&D machine Hera
# 2020-07-30  Jessica Meixner: Points only - no gridded data + optimization 
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (Bash) Shell
#   Machine: WCOSS-DELL-P3
#
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  set -x
  # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  cd $DATA

  # Set wave model ID tag to include member number
  # if ensemble; waveMEMB var empty in deterministic
  export WAV_MOD_TAG=${CDUMP}wave${waveMEMB}
  FHMAX_WAV_IBP=192

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

# 0.b Defining model grids

  waveuoutpGRD=${waveuoutpGRD:?buoyNotSet}

# 0.c Define a temporary directory for storing ascii point output files
#       and flush it

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then 
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/ibp

  set +x
  echo ' '
  echo 'Grid information  :'
  echo '-------------------'
  echo "   Output points : $waveuoutpGRD"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

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

# Copy model definition file for points
  if [ -f "$COMIN/rundata/${CDUMP}wave.mod_def.${waveuoutpGRD}" ]
  then
    set +x
    echo " Mod def file for $waveuoutpGRD found in ${COMIN}/rundata. copying ...."
    [[ "$LOUD" = YES ]] && set -x

    cp -f $COMIN/rundata/${CDUMP}wave.mod_def.${waveuoutpGRD} mod_def.${waveuoutpGRD}
  fi

  if [ ! -f mod_def.${waveuoutpGRD} ]
  then
    set +x
    echo ' '
    echo '*************************************************** '
    echo " FATAL ERROR : NO MOD_DEF FILE mod_def.${waveuoutpGRD} "
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : NO MOD_DEF file mod_def.${waveuoutpGRD}"
    err=2; export err;${errchk}
    exit $err
  else
    set +x
    echo "File mod_def.${waveuoutpGRD} found. Syncing to all nodes ..."
    [[ "$LOUD" = YES ]] && set -x
  fi
 
# 1.b Output locations file

  rm -f buoy.loc

  if [ -f $FIXwave/wave_${NET}.buoys ]
  then
    cp -f $FIXwave/wave_${NET}.buoys buoy.loc
  fi

  if [ -s buoy.loc ]
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
    postmsg "$jlogfile" "FATAL ERROR : NO BUOY LOCATION FILE"
    err=3; export err;${errchk}
    exit $err
  fi

  sed -n '/^\$.*/!p' buoy.loc | grep IBP > buoy.ibp
  if [ -s buoy.ibp ]; then
    set +x
    echo "   buoy.loc and buoy.ibp copied and processed ($FIXwave/wave_${NET}.buoys)."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '***************************************** '
    echo ' FATAL ERROR : NO IBP BUOY LOCATION FILE  '
    echo '***************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "FATAL ERROR : NO IBP BUOY LOCATION FILE"
    err=4; export err;${errchk}
    exit $err
  fi

# 1.c Input template files

  if [ -f $FIXwave/ww3_outp_spec.inp.tmpl ]
  then
    cp -f $FIXwave/ww3_outp_spec.inp.tmpl ww3_outp_spec.inp.tmpl
  fi

  if [ -f ww3_outp_spec.inp.tmpl ]
  then
    set +x
    echo "   ww3_outp_spec.inp.tmpl copied. Syncing to all grids ..."
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '*********************************************** '
    echo '*** ERROR : NO TEMPLATE FOR SPEC INPUT FILE *** '
    echo '*********************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "NON-FATAL ERROR : NO TEMPLATE FOR SPEC INPUT FILE"
    exit_code=3
  fi

# 1.d Getting buoy information for points

  ymdh=`$NDATE -${WAVHINDH} $CDATE`
  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
  dtspec=3600.            # default time step (not used here)
  sed -e "s/TIME/$tstart/g" \
      -e "s/DT/$dtspec/g" \
      -e "s/POINT/1/g" \
      -e "s/ITYPE/0/g" \
      -e "s/FORMAT/F/g" \
                       ww3_outp_spec.inp.tmpl > ww3_outp.inp
   
  ln -s mod_def.$waveuoutpGRD mod_def.ww3
  YMD=$(echo $CDATE | cut -c1-8)
  HMS="$(echo $CDATE | cut -c9-10)0000"
  if [ -f $COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS} ]
  then
    ln -s $COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS} ./out_pnt.${waveuoutpGRD}   
  else
    echo '*************************************************** '
    echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.${waveuoutpGRD}.${YMD}.${HMS} "
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$WAV_MOD_TAG post $waveuoutpGRD $CDATE $cycle : field output missing." 
    postmsg "$jlogfile" "FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.${waveuoutpGRD}.${YMD}.${HMS}"
    err=5; export err;${errchk}
  fi
    
  rm -f buoy_tmp.loc buoy_log.ww3 ww3_oup.inp
  ln -fs ./out_pnt.${waveuoutpGRD} ./out_pnt.ww3
  ln -fs ./mod_def.${waveuoutpGRD} ./mod_def.ww3
  $EXECwave/ww3_outp > buoy_lst.loc 2>&1 
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
    echo "$WAV_MOD_TAG post $date $cycle : buoy log file failed to be created."
    echo $msg
    [[ "$LOUD" = YES ]] && set -x
    err=6;export err;${errchk}
    exit $err
  fi


# Create new buoy_log.ww3 including all IBP files
  cat buoy.ibp | awk '{print $3}' | sed 's/'\''//g' > ibp_tags
  grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
  rm -f buoy_log.ibp
  mv buoy_log.tmp buoy_log.ibp

  grep -F -f ibp_tags buoy_lst.loc >  buoy_tmp1.loc
  sed    '$d' buoy_tmp1.loc > buoy_tmp2.loc
  ibpoints=`awk '{ print $1 }' buoy_tmp2.loc`
  Nibp=`wc buoy_tmp2.loc | awk '{ print $1 }'`
  rm -f buoy_tmp1.loc buoy_tmp2.loc
  if [ -s buoy_log.ibp ]
  then
    set +x
    echo 'IBP  log file created. Syncing to all nodes ...'
    [[ "$LOUD" = YES ]] && set -x
  else
    set +x
    echo ' '
    echo '********************************************** '
    echo '*** FATAL ERROR : NO  IBP LOG FILE CREATED *** '
    echo '********************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$WAV_MOD_TAG post $date $cycle : ibp  log file missing."  
    postmsg "$jlogfile" "FATAL ERROR : NO  IBP LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
    err=7;export err;${errchk}
    exit $err
  fi

# 1.e Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : `date`"
  echo ' ' 
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "     Sufficient data for Input Boundary Points ($Nibp points)"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 2.  Make consolidated grib2 file for side-by-side grids and interpolate
#     onto extended grids
#
# 2.a Command file set-up

  set +x
  echo '   Making command file for wave boundary points '
  [[ "$LOUD" = YES ]] && set -x

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile

# 2.a.1 Loop over forecast time to generate post files 
# When executed side-by-side, serial mode (cfp when run after the fcst step)
  fhr=$FHMIN_WAV
  fhrp=$fhr
  while [ $fhr -le $FHMAX_WAV_IBP ]; do
    
    echo " Starting processing wave boundary points for FHR=$fhr at: `date`"

    ymdh=`$NDATE $fhr $CDATE`
    YMD=$(echo $ymdh | cut -c1-8)
    HMS="$(echo $ymdh | cut -c9-10)0000"
    YMDHMS=${YMD}${HMS}
    FH3=$(printf %03i $fhr)

    fcmdnow=cmdfile.${FH3}
    rm -f ${fcmdnow} 
    touch ${fcmdnow}
    chmod 744 ${fcmdnow}

    mkdir output_$YMDHMS
    cd output_$YMDHMS

# Create instances of directories for spec and gridded output
    export SPECDATA=${DATA}/output_$YMDHMS
    ln -fs $DATA/mod_def.${waveuoutpGRD} mod_def.ww3

    pfile=$COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS}
    if [ -f  ${pfile} ]
    then 
      ln -fs ${pfile} ./out_pnt.${waveuoutpGRD}
    else 
      echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.$waveuoutpGRD.${YMD}.${HMS}
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      postmsg "$jlogfile" "FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.$waveuoutpGRD.${YMD}.${HMS}
      err=8; export err;${errchk}
      exit $err
    fi

    cd $DATA
    export dtspec=3600.
    echo "cd ${DATA}/output_$YMDHMS" >> ${fcmdnow}
    for buoy in $ibpoints
    do
      echo "$USHwave/wave_outp_spec.sh $buoy $ymdh ibp > ibp_$buoy.out 2>&1" >> ${fcmdnow}
    done
    echo "cd $DATA"

    echo "${fcmdnow}" >> cmdfile 

    FHINCP=$(( DTPNT_WAV / 3600 ))
    if [ $fhr = $fhrp ]
    then
      fhrp=$((fhr+FHINCP))
    fi
    echo $fhrp

    fhr=$fhrp # no gridded output, loop with out_pnt stride

  done
  echo "   create first round of cmdfiles JDM : `date`"


  if [ ${CFP_MP:-"NO"} = "YES" ]; then
    nfile=0
    ifile=0
    iline=1
    ifirst='yes'
    nlines=$( wc -l cmdfile | awk '{print $1}' )
    while [ $iline -le $nlines ]; do
      line=$( sed -n ''$iline'p' cmdfile )
      if [ -z "$line" ]; then  
        break
      else
        if [ "$ifirst" = 'yes' ]; then 
          echo "#!/bin/sh" > cmdmfile.$nfile 
          echo "$nfile cmdmfile.$nfile" >> cmdmprog
          chmod 744 cmdmfile.$nfile
        fi
        echo $line >> cmdmfile.$nfile
        nfile=$(( nfile + 1 ))
        if [ $nfile -eq $NTASKS ]; then
          nfile=0 
          ifirst='no'
        fi
        iline=$(( iline + 1 ))
      fi
    done
  fi

  wavenproc=`wc -l ${fcmdnow} | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  set +x
  echo ' '
  echo "   Executing the boundary point scripts at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$wavenproc" -gt '1' ]
  then
    if [ ${CFP_MP:-"NO"} = "YES" ]; then
      ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdmprog
    else
      ${wavempexec} ${wavenproc} ${wave_mpmd} ${fcmdnow}
    fi
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
    echo '*************************************'
    echo '*** FATAL ERROR: CMDFILE FAILED   ***'
    echo '*************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    err=9; export err;${errchk}
    exit $err
  fi


# 2.b Loop over each buoy to cat the final buoy file for all fhr 

  rm -f cmdfile.bouy
  touch cmdfile.bouy
  chmod 744 cmdfile.bouy

  for buoy in $ibpoints
  do
    echo "$USHwave/wave_outp_cat.sh $buoy ibp > ibp_cat_$buoy.out 2>&1" >> cmdfile.bouy
  done

  if [ ${CFP_MP:-"NO"} = "YES" ]; then
    nfile=0
    ifile=0
    iline=1
    ifirst='yes'
    nlines=$( wc -l cmdfile | awk '{print $1}' )
    while [ $iline -le $nlines ]; do
      line=$( sed -n ''$iline'p' cmdfile )
      if [ -z "$line" ]; then
        break
      else
        if [ "$ifirst" = 'yes' ]; then
          echo "#!/bin/sh" > cmdmfile.$nfile
          echo "$nfile cmdmfile.$nfile" >> cmdmprog
          chmod 744 cmdmfile.$nfile
        fi
        echo $line >> cmdmfile.$nfile
        nfile=$(( nfile + 1 ))
        if [ $nfile -eq $NTASKS ]; then
          nfile=0
          ifirst='no'
        fi
        iline=$(( iline + 1 ))
      fi
    done
  fi

  wavenproc=`wc -l ${fcmdnow} | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  set +x
  echo ' '
  echo "   Executing the boundary point scripts at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$wavenproc" -gt '1' ]
  then
    if [ ${CFP_MP:-"NO"} = "YES" ]; then
      ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdmprog
    else
      ${wavempexec} ${wavenproc} ${wave_mpmd} ${fcmdnow}
    fi
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
    echo '*************************************'
    echo '*** FATAL ERROR: CMDFILE FAILED   ***'
    echo '*************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    err=10; export err;${errchk}
    exit $err
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

  [[ "$LOUD" = YES ]] && set -x

# 6.b Spectral data files

  echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibp $Nibp > ${WAV_MOD_TAG}_ibp_tar.out 2>&1 "   >> cmdtarfile

  wavenproc=`wc -l cmdtarfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  set +x
  echo ' '
  echo "   Executing the wave_tar scripts at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  chmod 744 cmdtarfile
  ./cmdtarfile
  exit=$?

  if [ "$exit" != '0' ]
  then
      set +x
      echo ' '
      echo '*************************************'
      echo '*** FATAL ERROR: CMDFILE FAILED   ***'
      echo '*************************************'
      echo '     See Details Below '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      err=11; export err;${errchk}
      exit $err
  fi

# --------------------------------------------------------------------------- #
# 7.  Ending output

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
    echo " FATAL ERROR: Problem in MWW3 POST"
    msg="ABNORMAL EXIT: Problem in MWW3 POST"
    postmsg "$jlogfile" "$msg"
    echo $msg
    err=12; export err;${errchk}
    exit $err
  else
    echo " Side-by-Side Wave Post Completed Normally "
    msg="$job completed normally"
    postmsg "$jlogfile" "$msg"
    exit 0
  fi

# End of MWW3 prostprocessor script ---------------------------------------- #
