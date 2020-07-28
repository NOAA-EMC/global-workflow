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
#                                                                              #
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
  npert=${npert:?Parameter npert required for ensemble statistics}
  nmembn=`expr ${npert} + 1`
#
  export membn=""
  for i in $(seq -f "%02g" 0 $npert); do membn="$membn $i"; done
#
# 0.c Time management
#
# 0.d Parameter selection and deployment of arrays
#
  ASWELL=(SWELL1 SWELL2 SWELL3) # Indices of HS from partitions
  ASWPER=(SWPER1 SWPER2 SWPER3) # Indices of PERIODS from partitions 
                                #  (should be same as ASWELL)
  ASWDIR=(SWDIR1 SWDIR2 SWDIR3) # Indices of PERIODS from partitions 
  export arrpar=(HTSGW PERPW ICEC IMWF MWSPER DIRPW WVHGT WVPER WVDIR WWSDIR WIND WDIR ${ASWELL[@]} ${ASWDIR[@]} ${ASWPER[@]})
  export nparam=`echo ${arrpar[@]} | wc -w`

#
# 1. Get Input files for current script 
#
# 1.a Check if buoy input files exist and copy
#
  buoyfile=wave_${NET}.buoys
  if [ -s $FIXwave/${buoyfile} ] ; then
    cp  $FIXwave/${buoyfile} buoy_file.data
    echo " $FIXwave/${buoyfile} copied to buoy_file.data."
  else
    msg="ABNORMAL EXIT: ERR in coping ${buoyfile}."
    postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '******************************************************* '
    echo "*** FATAL ERROR: No $FIXwave/${buoyfile} copied. *** "
    echo '******************************************************* '
    echo ' '
    echo "$FIXwave/wave_ens_buoy.data  missing." >> $ensemb_log
    [[ "$LOUD" = YES ]] && set -x
    echo "$FIXwave/wave_${NET}_buoy.data  missing." >> $ensemb_log
    msg="ABNORMAL EXIT: NO FILE $buoyfile"
    postmsg "$jlogfile" "$msg"
    export err=1;${errchk};
    exit ${err}
  fi

#
# 1.b Link grib2 data for all members
#
  ngrib=0
  inc=$FHOUT_HF_WAV
  ifrcst=0
  while [ $ifrcst -le $FHMAX_WAV ]
  do
    for grdID in $waveinterpGRD
    do
      ngrib=$(( $ngrib + 1 ))
      FH3=$(printf "%03d" $ifrcst)
      for me in $membn
      do
        if [ "$me" == "00" ]; then
          ftype="c"
        else
          ftype="p"
        fi
        ENSTAG=${ftype}${me}
        case $grdID in
          glo_15mxt) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11 ;;
          glo_30mxt) GRDNAME='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=11 ;;
        esac
        cpfile=$COMIN/gridded/${WAV_MOD_TAG}.${cycle}.${ENSTAG}.${GRDNAME}.${GRDRES}.f${FH3}.grib2
        if [ -s ${cpfile} ] ; then 
          ln -s  $cpfile  ./. 
        else
          msg="ABNORMAL EXIT: ERR in coping $cpfile "
          postmsg "$jlogfile" "$msg"
          echo ' '
          echo '******************************************************* '
          echo "*** FATAL ERROR: No $cpfile copied. *** "
          echo '******************************************************* '
          echo ' '
          echo "$cpfile missing." >> $ensemb_log
          export err=2;${errchk}
          exit ${err}
        fi
      done
      if [ $ifrcst -ge $FHMAX_HF_WAV ]
      then
         inc=$FHOUT_WAV
      fi
      ifrcst=$(( $ifrcst + $inc ))
    done
  done
# Prepare separate data files to reduce copy load to tmp directories
# 
# 2.a Command file set-up
  rm -f cmdfile cmdfile.$ 

  iparam=1

# Number of expected extracted files if nparam * nmembn
  nef=`expr ${nparam} \* ${nmembn}`

  while [ ${iparam} -le ${nparam} ]
  do
    nip=${arrpar[$iparam-1]}
    echo $nip
    prepar=`echo $nip | rev | cut -c2- | rev` #Part prefix (assumes 1 digit index)
    paridx=`echo $nip | rev | cut -c-1`
    npart=0
    case $prepar in
      HTSG)   nnip=${nip} ; snip=hs ;;
      PERP)   nnip=${nip} ; snip=tp ;;
      ICE)   nnip=${nip} ; snip=ice ;;
      IMW)   nnip=${nip} ; snip=tm ;;
      MWSPE)   nnip=${nip} ; snip=tz ;;
      DIRP)   nnip=${nip} ; snip=pdir ;;
      WVHG)  nnip=${nip} ; snip=wshs ;;
      WVPE)  nnip=${nip} ; snip=wstp ;;
      WVDI)  nnip=${nip} ; snip=wsdir ;;
      WWSDI)  nnip=${nip} ; snip=wwdir ;;
      WIN)    nnip=${nip} ; snip=wnd ;;
      WDI)    nnip=${nip} ; snip=wnddir ;;
      SWELL)  nnip=${nip} ; snip=hswell ; npart=1 ;;
      SWDIR)  nnip=${nip} ; snip=dswell ; npart=1 ;;
      SWPER)  nnip=${nip} ; snip=tswell ; npart=1 ;;
      *)       nnip= ;;
    esac

    inc=$FHOUT_HF_WAV
    ifrcst=0
    if [ ${iparam} -eq 3 ] ||[ ${iparam} -eq 4 ] || [ ${iparam} -eq 5 ] || \
       [ ${iparam} -eq 10 ] || [ ${iparam} -eq 15 ] || [ ${iparam} -eq 16 ] ||
       [ ${iparam} -eq 17 ] || [ ${iparam} -eq 18 ] || [ ${iparam} -eq 21 ]
    then
      echo "Parameter $snip not yet available for stats"
    else

      while [ $ifrcst -le $FHMAX_WAV ]
      do
        FH3=$(printf "%03d" $ifrcst)
        for me in $membn
          do
           if [ "$me" == "00" ]; then
             ftype="c"
           else
             ftype="p"
           fi
           ENSTAG=${ftype}${me}
           infile=${WAV_MOD_TAG}.${cycle}.${ENSTAG}.${GRDNAME}.${GRDRES}.f${FH3}.grib2
           outfile=${nnip}_${me}.t${cyc}z.f${FH3}.grib2

           wgfileout=wgrib_${nnip}_${me}.out
           if [ "${npart}" = "0" ]
           then 
             echo "$WGRIB2 -match ${nip} -match surface ${infile} -grib ${outfile} > ${wgfileout} 2>&1" >> cmdfile
           else
             echo "$WGRIB2 -match ${prepar} -match \"${paridx} in sequence\" ${infile} -grib ${outfile} > ${wgfileout} 2>&1" >> cmdfile
           fi

        done    #for members
        if [ $ifrcst -ge $FHMAX_HF_WAV ]
        then
           inc=$FHOUT_WAV
        fi
        ifrcst=$(( $ifrcst + $inc ))
      done    #for times
    fi
      iparam=`expr ${iparam} + 1`
  done    #for parameters
  # END all loops

# 2.c Execute poe or serial command files

  set +x
  echo ' '
  echo " Extracting $nmembn x $nparam wave ensembles parameter files at `date`"
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
    ./cmdfile
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
    export err=3; ${errchk} 
    exit $err
  fi

#
# 2.f Clean up larger grib2 gridded files
#
# 2. Generate ensemble mean, spread and probability files
# 
# 2.b Populate command files with stats wave_ens_stats.sh calls
#
  rm -f cmdfile cmdfile.$ 

  fhour=0
  while [ "$fhour" -le "$FHMAX_WAV" ]
  do
    iparam=1
    while [ ${iparam} -le ${nparam} ]
    do
      nip=${arrpar[$iparam-1]}

      if [ ${iparam} -eq 3 ] ||[ ${iparam} -eq 4 ] || [ ${iparam} -eq 5 ] || \
         [ ${iparam} -eq 10 ] || [ ${iparam} -eq 15 ] || [ ${iparam} -eq 16 ] ||
         [ ${iparam} -eq 17 ] || [ ${iparam} -eq 18 ] || [ ${iparam} -eq 21 ]
      then
        echo " Parameter $nip not yet available in grib2 library "
      else
# Line for doing per parameter, per time stamp
        echo "nip ngrib fhour: ${nip}, ${ngrib}, ${fhour}"
        echo "$USHwave/wave_ens_stat.sh ${nip} ${ngrib} ${fhour} 1> wave_ens_stats_${nip}_${fhour}.out 2>&1" >> cmdfile

      fi

        iparam=`expr ${iparam} + 1`

    done
    if [ $fhour -ge $FHMAX_HF_WAV ]
    then
      inc=$FHOUT_WAV
    else 
      inc=$FHOUT_HF_WAV
    fi
    fhour=`expr $fhour + $inc`
  done

# 2.c Execute poe or serial command files

  set +x
  echo ' '
  echo " Generating $nmembn hourly to ${FHMAX_WAV}h wave ensembles stats files "
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
    ./cmdfile
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
    export err=4; ${errchk}
    exit $err
  fi

# Regroup all outputs in parameter/stats files
# Regrouping has to be sequential per parameter, per hour


  fhour=0

  while [ "$fhour" -le "$FHMAX_WAV" ]
  do
    FH3=$(printf "%03d" $fhour)
    valtime=`$NDATE ${fhour} ${CDATE}`  
    iparam=1

    while [ ${iparam} -le ${nparam} ]
    do
      nip=${arrpar[$iparam-1]}
      case $nip in
        HTSGW)   stypes='mean spread probab' ; snip=hs ;;
        PERPW)   stypes='mean spread probab' ; snip=tp ;;
        ICEC)    stypes='mean spread probab' ; snip=ice ;;
        DIRPW)   stypes='mean spread probab' ; snip=pdir ;;
        IMWF)    stypes='mean spread probab' ; snip=tm ;;
        MWSP)    stypes='mean spread probab' ; snip=tz ;;
        WVHGT)   stypes='mean spread probab' ; snip=wshs ;;
        WVPER)   stypes='mean spread probab' ; snip=wstp ;;
        WVDIR)   stypes='mean spread' ; snip=wsdir ;;
        WWSDIR)  stypes='mean spread' ; snip=wwdir ;;
        WIND)    stypes='mean spread probab' ; snip=wnd ;;
        WDIR)    stypes='mean spread' ; snip=wnddir ;;
        SWELL1)  stypes='mean spread probab' ; snip=hswell1 ;;
        SWELL2)  stypes='mean spread probab' ; snip=hswell2 ;;
        SWELL3)  stypes='mean spread probab' ; snip=hswell3 ;;
        SWDIR1)  stypes='mean spread' ; snip=dswell1 ;;
        SWDIR2)  stypes='mean spread' ; snip=dswell2 ;;
        SWDIR3)  stypes='mean spread' ; snip=dswell3 ;;
        SWPER1)  stypes='mean spread probab' ; snip=tswell1 ;;
        SWPER2)  stypes='mean spread probab' ; snip=tswell2 ;;
        SWPER3)  stypes='mean spread probab' ; snip=tswell3 ;;
        *)       nnip= ;;
      esac

      par_dir=tmp_${nip}

      if [ ${iparam} -eq 3 ] ||[ ${iparam} -eq 4 ] || [ ${iparam} -eq 5 ] || \
         [ ${iparam} -eq 10 ] || [ ${iparam} -eq 15 ] || [ ${iparam} -eq 16 ] ||
         [ ${iparam} -eq 17 ] || [ ${iparam} -eq 18 ] || [ ${iparam} -eq 21 ]
      then
        echo " Parameter $nip not yet available in grib2 library "
      else
# 2.e Cleanup base parameter files per member
        rm -f ${nip}_??.t${cyc}z.grib2         
  
        for stype in $stypes
        do

          ingrib=${snip}_${stype}.${FH3}.grib2
          outgrib=${WAV_MOD_TAG}.t${cyc}z.${stype}.f${FH3}.grib2 

          echo "$WGRIB2  ./${par_dir}/${valtime}/${ingrib} -append -grib ./${outgrib} >> ${FH3}_${stype}.t${cyc}z.out 2>> ${FH3}_${stype}.t${cyc}z.err" >> cmdfile.${fhour}

        done

      fi
      iparam=$((iparam + 1))
      echo "IPARAM: $iparam"
    done

    if [ $fhour -ge $FHMAX_HF_WAV ]
    then
      inc=$FHOUT_WAV
    else 
      inc=$FHOUT_HF_WAV
    fi
    fhour=`expr $fhour + $inc`

  done
  chmod 744 cmdfile.*
  ls -1 cmdfile.* > cmdfile

  set +x
  echo ' '
  echo " Regrouping stats files for ${nparam} parameters"
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
    ./cmdfile
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
    export err=5; ${errchk}
    exit $err
  fi


# 2.f Output all grib2 parameter files to COMOUT

  fhour=0

  while [ "$fhour" -le "$FHMAX_WAV" ]
  do
    FH3=$(printf "%03d" $fhour)
    for stype in mean spread probab
    do
      fcopy=${WAV_MOD_TAG}.t${cyc}z.${stype}.f${FH3}.grib2
      if [ -s ${fcopy} ]
      then
        set +x
        echo "   Copying ${fcopy} to $COMOUT/gridded and ALERT if SENDDBN=YES"
        [[ "$LOUD" = YES ]] && set -x
        if [ $SENDCOM = "YES" ] ; then
          cp -f ${fcopy} $COMOUT/gridded
# 2.g Alert DBN
          if [ "$SENDDBN" = 'YES' ]
          then
           MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
           $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job $COMOUT/gridded/${fcopy}
          fi
        fi
      else
        set +x
        echo ' '
        echo '*************************************** '
        echo "*** FATAL ERROR: No ${fcopy} file found *"
        echo '*************************************** '
        echo ' '
        echo "$modIE fcst $date $cycle: ${fcopy} not fouund." >> $wavelog
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        export err=6;${errchk}
        exit $err
      fi
    done
    if [ $fhour -ge $FHMAX_HF_WAV ]
    then
      inc=$FHOUT_WAV
    else 
      inc=$FHOUT_HF_WAV
    fi
    fhour=`expr $fhour + $inc`
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

  cat gefswave.t${cyc}z.mean.f???.grib2 | $WGRIB2 - -match "(HTSGW|PERPW|WIND)" -grib gribfile > gribfile.out 2>&1 

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

  while [ ${ibuoy} -le ${nbuoys} ]
  do

    bline=`sed ''$ibuoy'!d' buoy.file`
    blat=`echo $bline | awk '{print $2}'`
    blon=`echo $bline | awk '{print $1}'`
    bnom=`echo $bline | awk '{print $3}' | sed "s/'//g"`

    echo "$USHwave/wave_ens_bull.sh ${blon} ${blat} ${bnom} 1> bull_${bnom}.out 2>&1" >> cmdfile

    ibuoy=`expr ${ibuoy} + 1`

  done

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
    ./cmdfile
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
     postmsg "$jlogfile" "$msg"
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
    echo "   Copying ${WAV_MOD_TAG}.t${cyc}z.bull_tar  to $COMOUT/station"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${WAV_MOD_TAG}.t${cyc}z.bull_tar $COMOUT/station
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
    echo "   Copying ${WAV_MOD_TAG}.t${cyc}z.bull_tar  to $COMOUT/station"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${WAV_MOD_TAG}.t${cyc}z.station_tar $COMOUT/station
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
       $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job $COMOUT/station/${WAV_MOD_TAG}.t${cyc}z.bull_tar
       $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job $COMOUT/station/${WAV_MOD_TAG}.t${cyc}z.station_tar
  fi
#
  if [ "$exit_code" -ne '0' ]
  then
     echo "FATAL ERROR: Problem in WAVE STAT"
     msg="ABNORMAL EXIT: Problem in WAVE STAT"
     postmsg "$jlogfile" "$msg"
     echo $msg
     export err=12;${errchk}
     exit $err
  fi

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"
#
  echo "Ending at : `date`"
#
# END
#
