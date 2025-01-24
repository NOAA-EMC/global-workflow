#!/bin/bash
#
################################################################################
#
# UNIX Script Documentation Block
# Script name:         exgfs_wave_post_pnt.sh
# Script description:  Creates point output products from binary WW3 data
#
# Author:   Jose-Henrique Alves Org: NCEP/EMC      Date: 2019-12-06
# Abstract: This script is the point postprocessor for the wave component in GFS.
#           It executes several scripts forpreparing and creating output data
#           as follows:
#
#  wave_outp_spec.sh         : generates spectral data for output locations                                      
#  wave_outp_bull.sh         : generates bulletins for output locations      
#  wave_outp_cat.sh          : cats the by hour into the single output file
#  wave_tar.sh               : tars the spectral and bulletin multiple files  
#
# Script history log:
# 2019-12-06  J-Henrique Alves: First Version adapted from HTolman post.sh 2007 
# 2020-06-10  J-Henrique Alves: Porting to R&D machine Hera
# 2020-07-30  Jessica Meixner: Points only - no gridded data
# 2020-09-29  Jessica Meixner: optimized by changing loop structures
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

  echo "HAS BEGUN on `hostname`"
  echo "Starting WAVE PNT POSTPROCESSOR SCRIPT for $WAV_MOD_TAG"

  set +x
  echo ' '
  echo '                     *************************************'
  echo '                     *** WAVE PNT POSTPROCESSOR SCRIPT ***'
  echo '                     *************************************'
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

  export STA_DIR=$DATA/station_ascii_files
  if [ -d $STA_DIR ]
  then
    rm -rf ${STA_DIR}
  fi
  mkdir -p ${STA_DIR}
  mkdir -p ${STA_DIR}/spec
  mkdir -p ${STA_DIR}/bull
  mkdir -p ${STA_DIR}/cbull

# 0.b Defining model grids

  waveuoutpGRD=${waveuoutpGRD:?buoyNotSet}

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

  [[ "$LOUD" = YES ]] && set -x

# Copy model definition files
  for grdID in $waveuoutpGRD
  do
    if [ -f "$COMIN/rundata/${CDUMP}wave.mod_def.${grdID}" ]
    then
      set +x
      echo " Mod def file for $grdID found in ${COMIN}/rundata. copying ...."
      [[ "$LOUD" = YES ]] && set -x

      cp -f $COMIN/rundata/${CDUMP}wave.mod_def.${grdID} mod_def.$grdID
      iloop=`expr $iloop + 1`
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
      [[ "$LOUD" = YES ]] && set -x
      err=2; export err;${errchk}
      exit $err
    else
      set +x
      echo "File mod_def.$grdID found. Syncing to all nodes ..."
      [[ "$LOUD" = YES ]] && set -x
    fi
  done
 
# 1.b Output locations file

  rm -f buoy.loc

  if [ -f $FIXwave/wave_${NET}.buoys ]
  then
    cp -f $FIXwave/wave_${NET}.buoys buoy.loc.temp
    if [ "$DOBNDPNT_WAV" = YES ]; then
      #only do boundary points
      sed -n '/^\$.*/!p' buoy.loc.temp | grep IBP > buoy.loc
    else
      #exclude boundary points   
      sed -n '/^\$.*/!p' buoy.loc.temp | grep -v IBP > buoy.loc
    fi 
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
    err=3; export err;${errchk}
    exit $err
    DOSPC_WAV='NO'
    DOBLL_WAV='NO'
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
    exit_code=3
    DOSPC_WAV='NO'
    DOBLL_WAV='NO'
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
  else
    set +x
    echo ' '
    echo '*************************************************** '
    echo '*** ERROR : NO TEMPLATE FOR BULLETIN INPUT FILE *** '
    echo '*************************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    exit_code=4
    DOBLL_WAV='NO'
  fi

# 1.d Getting buoy information for points

  if [ "$DOSPC_WAV" = 'YES' ] || [ "$DOBLL_WAV" = 'YES' ]
  then
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
      ln -s $COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS}
    else
      echo '*************************************************** '
      echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.${waveuoutpGRD}.${YMD}.${HMS} "
      echo '*************************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "$WAV_MOD_TAG post $waveuoutpGRD $CDATE $cycle : field output missing." 
      err=4; export err;${errchk}
    fi
    
    rm -f buoy_tmp.loc buoy_log.ww3 ww3_oup.inp
    ln -fs ${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS} ./out_pnt.ww3
    ln -fs ./mod_def.${waveuoutpGRD} ./mod_def.ww3
    export pgm=ww3_outp;. prep_step
    $EXECwave/ww3_outp ${WAV_MOD_TAG} > buoy_lst.loc 2>&1 
    export err=$?;err_chk

    if [ "$err" != '0' ] && [ ! -f buoy_log.ww3 ]
    then
      pgm=wave_post
      set +x
      echo ' '
      echo '******************************************** '
      echo '*** FATAL ERROR : ERROR IN ww3_outp *** '
      echo '******************************************** '
      echo ' '
      cat buoy_tmp.loc
      echo "$WAV_MOD_TAG post $date $cycle : buoy log file failed to be created."
      [[ "$LOUD" = YES ]] && set -x
      err=5;export err;${errchk}
      DOSPC_WAV='NO'
      DOBLL_WAV='NO'
      exit $err
    fi

# Create new buoy_log.ww3 
    cat buoy.loc | awk '{print $3}' | sed 's/'\''//g' > ibp_tags
    grep -F -f ibp_tags buoy_log.ww3 > buoy_log.tmp
    rm -f buoy_log.dat
    mv buoy_log.tmp buoy_log.dat

    grep -F -f ibp_tags buoy_lst.loc >  buoy_tmp1.loc
    #sed    '$d' buoy_tmp1.loc > buoy_tmp2.loc
    buoys=`awk '{ print $1 }' buoy_tmp1.loc`
    Nb=`wc buoy_tmp1.loc | awk '{ print $1 }'`
    rm -f buoy_tmp1.loc 

    if [ -s buoy_log.dat ]
    then
      set +x
      echo 'Buoy log file created. Syncing to all nodes ...'
      [[ "$LOUD" = YES ]] && set -x
    else
      set +x
      echo ' '
      echo '**************************************** '
      echo '*** ERROR : NO BUOY LOG FILE CREATED *** '
      echo '**************************************** '
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      echo "FATAL ERROR : NO BUOY LOG FILE GENERATED FOR SPEC AND BULLETIN FILES"
      err=6;export err;${errchk}
      DOSPC_WAV='NO'
      DOBLL_WAV='NO'
    fi
 fi

# 1.e Data summary

  set +x
  echo ' '
  echo "   Input files read and processed at : `date`"
  echo ' ' 
  echo '   Data summary : '
  echo '   ---------------------------------------------'
  echo "      Sufficient data for spectral files        : $DOSPC_WAV ($Nb points)"
  echo "      Sufficient data for bulletins             : $DOBLL_WAV ($Nb points)"
  echo "      Boundary points                           : $DOBNDPNT_WAV"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 2. Make files for processing boundary points 
#
# 2.a creating ww3_outp.inp for each jobs

  set +x
  echo '   Making input file for wave post point '
  [[ "$LOUD" = YES ]] && set -x

  grep -F -f ibp_tags buoy_lst.loc | awk '{ print $1 }' > buoys
  grep -F -f buoys buoy_log.ww3 | awk '{ print $1 }' > points
  points=$(cat points | awk '{print $0 "\\n"}' | tr -d '\n')
  rm buoys
  
  ymdh=`$NDATE -${WAVHINDH} $CDATE`
  tstart="`echo $ymdh | cut -c1-8` `echo $ymdh | cut -c9-10`0000"
  dtspec=3600.            # default time step (not used here)
  N=$(( ($FHMAX_WAV_PNT - $FHMIN_WAV) + 1 ))
  truntime="`echo $CDATE | cut -c1-8` `echo $CDATE | cut -c9-10`0000"

  fhr=$FHMIN_WAV
  while [ $fhr -le $FHMAX_WAV_PNT ]; do
    ymdh=`$NDATE $fhr $CDATE`
    YMD=$(echo $ymdh | cut -c1-8)
    HMS="$(echo $ymdh | cut -c9-10)0000"

    pfile=$COMIN/rundata/${WAV_MOD_TAG}.out_pnt.${waveuoutpGRD}.${YMD}.${HMS}
    if [ -f  ${pfile} ]
    then
      ln -fs ${pfile}
    else
      echo " FATAL ERROR : NO RAW POINT OUTPUT FILE out_pnt.$waveuoutpGRD.${YMD}.${HMS} "
      echo ' '
      [[ "$LOUD" = YES ]] && set -x
      err=7; export err;${errchk}
      exit $err
    fi
    FHINCP=$(( DTPNT_WAV / 3600 ))
    fhrp=$((fhr+FHINCP))
    fhr=$fhrp # no gridded output, loop with out_pnt stride
  done

  if [ "$DOSPC_WAV" = 'YES' ]; then
    sed -e "s/TIME/$tstart/g" \
        -e "s/DT/$dtspec/g" \
	-e "s/999/$N/g" \
        -e "s|POINT|$points|g" \
        -e "s/ITYPE/1/g" \
        -e "s/FORMAT/F/g" \
                               ww3_outp_spec.inp.tmpl > ww3_outp.inp
   
    $EXECwave/ww3_outp ${WAV_MOD_TAG} 1> ww3_outp_spec.log 2>&1

  fi

  if [ "$DOBLL_WAV" = "YES" ]; then
    sed -e "s/TIME/$tstart/g" \
        -e "s/DT/$dtspec/g" \
        -e "s/999/$N/g" \
        -e "s|POINT|$points|g" \
        -e "s/REFT/$truntime/g" \
	                       ww3_outp_bull.inp.tmpl > ww3_outp.inp
    
    $EXECwave/ww3_outp ${WAV_MOD_TAG} 1> ww3_outp_bull.log 2>&1
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

  if [ ${CFP_MP:-"NO"} = "YES" ]; then nm=0; fi

  if [ ${CFP_MP:-"NO"} = "YES" ]; then
    if [ "$DOBNDPNT_WAV" = YES ]; then
      if [ "$DOSPC_WAV" = YES ]; then 
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibp $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi 
      if [ "$DOBLL_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibpbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibpcbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi 
    else 
      if [ "$DOSPC_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG spec $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG bull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG cbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        nm=$(( nm + 1 ))
      fi 
    fi
  else
    if [ "$DOBNDPNT_WAV" = YES ]; then
      if [ "$DOSPC_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibp $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibpbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG ibpcbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
    else 
      if [ "$DOSPC_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG spec $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
      if [ "$DOBLL_WAV" = YES ]; then
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG bull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
        echo "$USHwave/wave_tar.sh $WAV_MOD_TAG cbull $Nb > ${WAV_MOD_TAG}_spec_tar.out 2>&1 "   >> cmdtarfile
      fi
    fi
  fi

  wavenproc=`wc -l cmdtarfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  set +x
  echo ' '
  echo "   Executing the wave_tar scripts at : `date`"
  echo '   ------------------------------------'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$wavenproc" -gt '1' ]
  then
    if [ ${CFP_MP:-"NO"} = "YES" ]; then
      ${wavempexec} ${wave_mpmd} ${DATA}/cmdtarfile
    else
      ${wavempexec} ${wavenproc} ${wave_mpmd} cmdtarfile
    fi
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
# 4.  Ending output

  set +x
  echo ' '
  echo "Ending at : `date`"
  echo '-----------'
  echo ' '
  echo '                     *** End of MWW3 pnt postprocessor ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  if [ "$exit_code" -ne '0' ]
  then
    echo " FATAL ERROR: Problem in MWW3 PNT POST"
    err=11; export err;${errchk}
    exit $err
  else
    echo " Point Wave Post Completed Normally "
    exit 0
  fi

# End of MWW3 point prostprocessor script ---------------------------------------- #
