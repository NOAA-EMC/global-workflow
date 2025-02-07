#!/bin/bash
#                                                                       
################################################################################
#                                                                              #
# exwave_stats.sh - Compute unified statistics for global wave ensemble        #
#                                                                              #        
# Packs ensemble mean, spread and probabilities in grib2 format.               #
#                                                                              #             
# Requirements:                                                                #    
# - WGRIB2 with IPOLATES library                                               #              
#                                                                              #              
# Origination:                                                                 #
# - Unreported Waves Group Developer, Feb 2008                                 #               
#                                                                              #          
# Changes:                                                                     #           
# - expanded parameter list including partitioned data                         #
#   (list in parameter array) (JH Alves, Jan 2014)                             #                 #                                                                              #
# - introduced wave ensemble bulletin following spectral bulletin format       #
#   (JH Alves, Jan 2014)                                                       #    
# - introduced two USH scripts for post proc                                   #
#   - wave_ens_stats.sh : generate unified stats files (mean, spread, prob)    #
#   - wave_ens_bull.sh : generates wave ensemble bulletin files                #
#   (JH Alves, Jan 2014)                                                       #
# - mpiserial for parallel processing (JH Alves, Jan 2014)                     #
# - Changes to wave_ens_stats (fortran) for paralellism: code now              #
#    computes separately stats type (mean, spread or prob) and prob            #
#    level (JH Alves, Jan 2014)                                                #
#                                                                              #
# Update log since 2014                                                        #
# Nov2019 JHAlves - Transitioning to GEFS workflow                             #
# Dec2019 JHAlves RPadilla - Merging wave scripts to global workflow           #
# Jan2025 SBanihash - Adding this script to the global workflow                #
################################################################################
#
  set -x
  #£ Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  set +x
  echo -e '                   ******************************************\n'
  echo '                   *** WAVE ENSEMBLE STATS SCRIPT ***'
  echo -e '                   ******************************************\n'
  echo "Starting at : `date`"
  [[ "$LOUD" = YES ]] && set -x
#
# 0. Preliminaries
#
  exit_code=0
# 0.a Date and system wide settings
# In coupled system used CDATE
#
# 0.b System-specific settings
#
  nens=$NMEM_ENS
  nens=${nens:?Parameter npert required for ensemble statistics}
  nmembn=`expr ${nens} + 1`
#
  export membn=""
  for i in $(seq -f "%03g" 0 $nens); do membn="$membn $i"; done
#
# 0.c Time management
#
 
  ymdh=$($NDATE)
  YMD=$(echo $ymdh | cut -c1-8)
  HMS="$(echo $ymdh | cut -c9-10)0000"
  YMDHMS=${YMD}${HMS}

  fcmdnow=cmdfile
  mkdir output_$YMDHMS
  cd output_$YMDHMS
  rm -f ${fcmdnow} 
  touch ${fcmdnow}


# 1.a Check if buoy input files exist and copy
#
  buoyfile=wave_${NET}.buoys
  if [ -s ${PARMgfs}/wave/${buoyfile} ] ; then
    cp  ${PARMgfs}/wave/${buoyfile} buoy_file.data
    echo " ${PARMgfs}/wave/${buoyfile} copied to buoy_file.data."
  else
    msg="ABNORMAL EXIT: ERR in coping ${buoyfile}."
    postmsg "$msg"
    set +x
    echo ' '
    echo '******************************************************* '
    echo "*** FATAL ERROR: No ${PARMgfs}/wave/${buoyfile} copied. *** "
    echo '******************************************************* '
    echo ' '
    echo "$PARMgfs/wave/wave_gefs_buoy.data  missing." >> $ensemb_log
    [[ "$LOUD" = YES ]] && set -x
    echo "${PARMgfs}/wave/wave_${NET}_buoy.data  missing." >> $ensemb_log
    msg="ABNORMAL EXIT: NO FILE $buoyfile"
    postmsg "$msg"
    export err=1;${errchk};
    exit ${err}
  fi

# 2. link grib files

    for stype in mean spread prob
	do
    # Correct indirect variable expansion
    	dir_var="COMOUT_WAVE_GRIB_${wavepostGRD}"
    	cpdir="${!dir_var}"

    # Ensure directory exists before proceeding
    	if [ -z "$cpdir" ] || [ ! -d "$cpdir" ]; then
        	echo "Error: Directory '$cpdir' does not exist or is empty."
        exit 2
    	fi

    # Use ls to safely check for matching files
    	cpfile=$(ls -1 "${cpdir}/${WAV_MOD_TAG}.t${cyc}z.${stype}.${GRDNAME}.${GRDRES}.f"???.grib2 2>/dev/null)

    	if [ -n "$cpfile" ]; then
        	ln -s "$cpfile" .
    	else
        	msg="ABNORMAL EXIT: Error in copying $cpfile"
        	postmsg "$msg"
        	echo ' '
        	echo '******************************************************* '
        	echo "*** FATAL ERROR: No $cpfile copied. *** "
        	echo '******************************************************* '
        	echo ' '
        	echo "$cpfile missing." >> "$ensemb_log"
        	export err=2
        	[ -n "$errchk" ] && eval "$errchk"
        	exit "$err"
    	fi
	done
#
# 3. Generate bulletin and time series files at complete set of buoy locations
#
  
# 3.a Buoy locations file massaging

  sed '/\$/d' buoy_file.data | sed '/STOPSTRING/d ' > buoy.file
  
  nbuoys=`cat buoy.file | wc -l`

# 3.b Command file set-up
  rm -f cmdfile cmdfile.$ 

  ibuoy=1

# 3.c Create bundled grib2 file with all parameters

  cat ${WAV_MOD_TAG}.t${cyc}z.mean.${GRDNAME}.${GRDRES}.f???.grib2 | $WGRIB2 - -match "(HTSGW|PERPW|WIND)" -grib gribfile > gribfile.out 2>&1 

  if [ -s gribfile ]
  then
     set +x
     echo "   Gribfile for bulletins created"
     [[ "$LOUD" = YES ]] && set -x
    else
      set +x
      echo ' '
      echo '************************************************* '
      echo "*** FATAL ERROR: No gribfile created, no bulls  *"
      echo '************************************************* '
      echo ' '
      echo "$modIE fcst $date $cycle: No gribfile created." >> $wavelog
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      export err=7;${errchk}
      exit $err
    fi

# 3.d Loop through buoys and populate cmdfiles with calls to wave_ens_bull.sh
  ifile=0
  while [ ${ibuoy} -le ${nbuoys} ]
  do

    bline=`sed ''$ibuoy'!d' buoy.file`
    blat=`echo $bline | awk '{print $2}'`
    blon=`echo $bline | awk '{print $1}'`
    bnom=`echo $bline | awk '{print $3}' | sed "s/'//g"`

    echo "$HOMEgfs/ush/wave_ens_bull.sh ${blon} ${blat} ${bnom} 1> bull_${bnom}.out 2>&1" >> cmdfile.${fhr}

    ibuoy=`expr ${ibuoy} + 1`
    ifile=`expr ${ifile} + 1`

  done


  chmod 744 cmdfile.*
  ls -1 cmdfile.* > cmdfile


# 3.e Execute poe or serial cmdfile
  set +x
  echo ' '
  echo " Generating bulletins and ts files for ${nbuoys} locations."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  wavenproc=`wc -l cmdfile | awk '{print $1}'`
  wavenproc=`echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS}))`

  if [ "$wavenproc" -gt '1' ]
  then
    ${wavempexec} ${wavenproc} ${wave_mpmd} cmdfile
    exit=$?
  else
    chmod 744 cmdfile
    ./cmdfile.${fhr}
    exit=$?
  fi

  if [ "$exit" != '0' ]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** FATAL ERROR: CMDFILE FAILED   ***'
    echo '********************************************'
    echo '     See Details Below '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    export err=8; ${errchk}
    exit $err
  fi

# echo ' Checking for errors after bulletins cfp'

# 3.f Check for errors
  while [ ${ibuoy} -le ${nbuoys} ]
  do

    bline=`sed ''$ibuoy'!d' buoy.file`
    blat=`echo $bline | awk '{print $2}'`
    blon=`echo $bline | awk '{print $1}'`
    bnom=`echo $bline | awk '{print $3}' | sed "s/'//g"`

    if [ ! -s ${modID}.${bnom}.bull ]
    then
     msg="ABNORMAL EXIT: ERR in generating bulettin file"
     postmsg "$msg"
     set +x
     echo ' '
     echo '***************************************** '
     echo "***            FATAL ERROR            *** "
     echo "--- No ${WAV_MOD_TAG}.${bnom}.bull file created --- "
     echo '***************************************** '
     echo ' '
     [[ "$LOUD" = YES ]] && set -x
     echo "No ${modIE}.${bnom}.bull " >> $wavelog
     export err=9;${errchk}
     exit $err
   else
     set +x
     echo -e "\n Bulletin file ${modID}.${bnom}.bull generated succesfully.\n"
     [[ "$LOUD" = YES ]] && set -x
     rm -f bull_${bnom}.out
   fi
 done
#
# 4. Output and closing management calls
#

# 4.a Compress bulletins into tar file and copy to COMOUT
  tar cf ${WAV_MOD_TAG}.t${cyc}z.bull_tar ${WAV_MOD_TAG}.*.bull
  rm -f ${WAV_MOD_TAG}.*.bull
  tar cf ${WAV_MOD_TAG}.t${cyc}z.station_tar ${WAV_MOD_TAG}.*.ts
  rm -f ${WAV_MOD_TAG}.*.ts

  set +x
  echo ' '
  echo 'Saving output files :'
  echo '---------------------'
  [[ "$LOUD" = YES ]] && set -x

  if [ -s ${WAV_MOD_TAG}.t${cyc}z.bull_tar ]
  then
    set +x
    echo "   Copying ${WAV_MOD_TAG}.t${cyc}z.bull_tar  to ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${WAV_MOD_TAG}.t${cyc}z.bull_tar ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/gridded/station
   else
     set +x
     echo ' '
     echo '*************************************** '
     echo '*** FATAL ERROR: No bull_tar file found *'
     echo '*************************************** '
     echo ' '
     echo "$modIE fcst $date $cycle: bull_tar not fouund." >> $wavelog
     echo $msg
     [[ "$LOUD" = YES ]] && set -x
     export err=10;${errchk}
     exit $err
   fi


# 4.b Compress time series into tar file and copy to COMOUT
  if [ -s ${WAV_MOD_TAG}.t${cyc}z.station_tar ]
  then
    set +x
    echo "   Copying ${WAV_MOD_TAG}.t${cyc}z.bull_tar  to ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${WAV_MOD_TAG}.t${cyc}z.station_tar ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station
   else
     set +x
     echo ' '
     echo '*************************************** '
     echo '*** FATAL ERROR: No station_tar file found *'
     echo '*************************************** '
     echo ' '
     echo "$modIE fcst $date $cycle: station_tar not fouund." >> $wavelog
     echo $msg
     [[ "$LOUD" = YES ]] && set -x
     export err=11;${errchk}
     exit $err
   fi

#
# 4.c Alert DBN
#
  if [ "$SENDDBN" = 'YES' ]
  then
       MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
       $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station/${WAV_MOD_TAG}.t${cyc}z.bull_tar
       $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station/${WAV_MOD_TAG}.t${cyc}z.station_tar
  fi
#
  if [ "$exit_code" -ne '0' ]
  then
     echo "FATAL ERROR: Problem in WAVE STAT"
     msg="ABNORMAL EXIT: Problem in WAVE STAT"
     postmsg "$msg"
     echo $msg
     export err=12;${errchk}
     exit $err
  fi

  msg="$job completed normally"
  postmsg "$msg"
#
  echo "Ending at : `date`"
#
# END
#
