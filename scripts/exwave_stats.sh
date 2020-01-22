#!/bin/bash
#                                                                       
################################################################################
#                                                                       
# exwave_stats.sh - Compute unified statistics for global wave ensemble
#                                                                             
# Packs ensemble mean, spread and probabilities in grib2 format.              
#                                                                           
# Requirements:                                                             
# - wgrib2 with IPOLATES library                                            
#                                                                           
# Origination: 
# - Unreported Waves Group Developer, Feb 2008                               
#                                                                       
# Changes:                                                              
# - expanded parameter list including partitioned data (list in parameter array)
#   (JH Alves, Jan 2014)                                                
# - introduced wave ensemble bulletin following spectral bulletin format
#   (JH Alves, Jan 2014)                                                
# - introduced two USH scripts for post proc 
#   - wave_ens_stats.sh : generate unified stats files (mean, spread, prob)
#   - wave_ens_bull.sh : generates wave ensemble bulletin files        
#   (JH Alves, Jan 2014)                                                
# - mpiserial for parallel processing (JH Alves, Jan 2014)                
# - Changes to wave_ens_stats (fortran) for paralellism: code now computes separately
#    stats type (mean, spread or prob) and prob level (JH Alves, Jan 2014)
#
# Update log since 2014                                                       #
# Nov2019 JHAlves - Transitioning to GEFS workflow                            #
# Dec2019 JHAlves - Merging wave scripts to global workflow                   #
#                                                                       
################################################################################
#
  set -x
  #£ Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  set +x
  echo -e '\n                   ******************************************\n'
  echo '                   *** WAVE ENSEMBLE STATS SCRIPT ***'
  echo -e '                   ******************************************\n'
  echo "Starting at : `date`"
  [[ "$LOUD" = YES ]] && set -x
#
# 0. Preliminaries
#
  exit_code=0

# 0.a Date and system wide settings
#
  export YMD=$PDY
  export YMDH=${PDY}${cyc}
  export tcycz=t${cyc}z
#
  export wgrib2=$utilexec/wgrib2
#
# 0.b System-specific settings
#
  export grdID='glo_30m'
  npert=${npert:?Parameter npert required for ensemble statistics}
  nmembn=`expr ${npert} + 1`
  export membn="00 `seq -w 1 ${nmembn}`"
#
# 0.c Time management
#
  export FHMAXWAV=${FHMAXWAV:-384}

  export dtgh='3' # Time interval between fields
  export hcst_hour=-24

#   Time range for data output
# This line for when hindcast codes are available
#   export trange=`expr ${FHMAXWAV} - ${hcst_hour}` 
# In the meantime, generate grib2 stats data only with forecasts
   export trange=`expr ${FHMAXWAV} - 0`
   ngrib=`expr ${trange} / ${dtgh} + 1`

#
# 0.d Parameter selection and deployment of arrays
#
    ASWELL=(SWELL1 SWELL2) # Indices of HS from partitions
    ASWPER=(SWPER1 SWPER2) # Indices of PERIODS form partitions 
                                  #  (should be same as ASWELL)
    export arrpar=(HTSGW PERPW WIND WDIR DIRPW WVHGT WVPER WVDIR ${ASWELL[@]} ${ASWPER[@]})
    export nparam=`echo ${arrpar[@]} | wc -w`

# 0.e Number of processes available for script
  ncmdfile=`echo $LSB_HOSTS | wc -w | awk '{ print $1}'`

#
# 1. Get Input files for current script 
#
# 1.a Check if buoy input files exist and copy
#
# When changing to cfp, verify for cfp hang noticed during Q4FY15 GEFS upgrade
 buoyfile='wave_${NET}.buoys.stats'
  if [ -f $FIXwave/${buoyfile} ] ; then
    cp  $FIXwave/${buoyfile} buoy_file.data
    echo " $FIXwave/${buoyfile} copied to buoy_file.data."
  else
    msg="ABNORMAL EXIT: ERR in coping ${buoyfile}."
    ./postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '******************************************************* '
    echo "*** ERROR: No $FIXwave/${buoyfile} copied. *** "
    echo '******************************************************* '
    echo ' '
    echo "$FIXwave/wave_ens_buoy.data  missing." >> $ensemb_log
    [[ "$LOUD" = YES ]] && set -x
    err=1;export err;./err_chk
    exit_code=1
  fi
#
# 1.b Copy grib2 data for all members
#
  for me in $membn
  do
    if [ -f $COMIN/${modIE}${me}.$grdID.t${cyc}z.grib2 ] ; then
#       infile=$COMIN/${modIE}${me}.$grdID.t${cyc}z.grib2
       cp  $COMIN/${modIE}${me}.$grdID.t${cyc}z.grib2  ./. 
     else
       msg="ABNORMAL EXIT: ERR in coping ${modIE}$me.$grdID.t${cyc}z.grib2."
       ./postmsg "$jlogfile" "$msg"
       echo ' '
       echo '******************************************************* '
       echo "*** ERR : No $COMIN/${modIE}$me.$grdID.t${cyc}z.grib2 copied. *** "
       echo '******************************************************* '
       echo ' '
       echo "$COMIN/${modIE}$me.$grdID.t${cyc}z.grib2 missing." >> $ensemb_log
       err=2;export err;./err_chk
       exit_code=2
     fi
  done

# Prepare separate data files to reduce copy load to tmp directories
# 
# 2.a Command file set-up
#     The command file points to $ncmdfile files named cmdfile.$ifile.
#     The actual work is distributed over these files.
  
  rm -f cmdfile*
  touch cmdfile
  chmod 744 cmdfile
  rm -f cmdfile.*
 # mpiserial lines
  ifile=1
  while [ "$ifile" -le "$ncmdfile" ]
  do
    touch cmdfile.$ifile
    chmod 700 cmdfile.$ifile
    echo "./cmdfile.$ifile" >> cmdfile
    ifile=`expr $ifile + 1`
  done

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
      DIRP)   nnip=${nip} ; snip=pdir ;;
      WVHG)  nnip=${nip} ; snip=wshs ;;
      WVPE)  nnip=${nip} ; snip=wstp ;;
      WVDI)  nnip=${nip} ; snip=wsdir ;;
      WIN)    nnip=${nip} ; snip=wnd ;;
      WDI)    nnip=${nip} ; snip=wnddir ;;
      SWELL)  nnip=${nip} ; snip=hswell ; npart=1 ;;
      SWPER)  nnip=${nip} ; snip=tswell ; npart=1 ;;
      *)       nnip= ;;
    esac

# mpiserial line
    ifile=1

    for me in $membn
    do

     infile=${modIE}${me}.$grdID.t${cyc}z.grib2

       if [ "${npart}" = "0" ]
       then
         echo "$wgrib2 -match ${nip} -match surface ${infile} -grib ${nnip}_${me}.t${cyc}z.grib2 > wgrib_${nnip}_${me}.out 2>&1" >> cmdfile.${ifile}
       else
         echo "$wgrib2 -match ${prepar} -match \"${paridx} in sequence\" ${infile} -grib ${nnip}_${me}.t${cyc}z.grib2 > wgrib_${nnip}_${me}.out 2>&1" >> cmdfile.${ifile}
       fi

# mpiserial if block
       if [ "$ncmdfile" -gt '1' ]
       then
         ifile=`expr $ifile + 1`
       fi
       if [ "$ifile" -gt "$ncmdfile" ]
       then
         ifile=1
       fi

     done
   
     iparam=`expr ${iparam} + 1`
   done

# 2.c Execute poe or serial command files

  if [ "$nparam" -gt '0' ]
  then

  set +x
  echo ' '
  echo " Extracting $nmembn x $nparam wave ensembles parameter files "
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
   if [ "$ncmdfile" -gt '1' ]
   then
#    mpirun.lsf cfp cmdfile
    mpirun.lsf mpiserial
     exit=$?
   else
#     ./cmdfile
     ./cmdfile.1
     exit=$?
   fi
 fi

# mpiserial section
# 2.d.1 Ending times on the different processors

  if [ "$nparam" -gt '0' ]
  then
    set +x
    echo ' '
    echo "   Ending times for separate processors :"
    [[ "$LOUD" = YES ]] && set -x

    for file in `cat cmdfile`
    do
      if [ "`wc $file | awk '{ print $1 }'`" = '0' ]
      then
        set +x
        echo "      $file : no commands in this file."
        [[ "$LOUD" = YES ]] && set -x
      else
        words="`tail -1 $file | wc | awk '{ print $2 }'`"
        wrdnr=`expr $words - 1`
        echo "{ print "'$'"$wrdnr }" > awkfile
        outfile="`tail -1 $file | awk -f awkfile`"
        rm -f awkfile
        set +x
        echo "      $file : `tail -1 $outfile`"
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# 2.d Check for errors

  iparam=1

  for me in $membn
  do

  while [ ${iparam} -le ${nparam} ]
  do

    nnip=${arrpar[$iparam-1]}

   if [ ! -f ${nnip}_${me}.t${cyc}z.grib2 ]
   then
     msg="ABNORMAL EXIT: ERR in generating base grib parameter file"
     ./postmsg "$jlogfile" "$msg"
     set +x
     echo ' '
     echo '***************************************** '
     echo "***            FATAL ERROR            *** "
     echo "--- No ${nnip}_${me}.t${cyc}z.grib2 file --- "
     echo '***************************************** '
     echo ' '
     [[ "$LOUD" = YES ]] && set -x
     echo "No ${nnip}_${me}.t${cyc}z.grib2 " >> $wavelog
     err=3;export err;./err_chk
     exit_code=3
   else
     set +x
     echo -e "\n Base grib parameter file created succesfully.\n"
     [[ "$LOUD" = YES ]] && set -x
     echo -e "\n ${COMOUT}/${nnip}_${me}.t${cyc}z.grib2 \n"
     rm -f wgrib_${nnip}_${me}.out
   fi

    iparam=`expr ${iparam} + 1`

  done
  done

# 2.f Clean up larger grib2 gridded files
rm -f ${modID}??.${grdID}.t${cyc}z.grib2

#
# 2. Generate ensemble mean, spread and probability files
#
# 2.a Command file set-up
#     The command file points to $ncmdfile files named cmdfile.$ifile.
#     The actual work is distributed over these files.

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile
# mpiserial block
  rm -f cmdfile.*
  ifile=1
  while [ "$ifile" -le "$ncmdfile" ]
  do
    touch cmdfile.$ifile
    chmod 744 cmdfile.$ifile
    echo "./cmdfile.$ifile" >> cmdfile
    ifile=`expr $ifile + 1`
  done

# 
# 2.b Populate command files with stats wave_ens_stats.sh calls
#
  ifile=1
  fhour=0
  end_hour=`expr ${ngrib} \* ${dtgh}` 

  while [ "$fhour" -lt "$end_hour" ]
  do

    iparam=1

    while [ ${iparam} -le ${nparam} ]
    do
      nip=${arrpar[$iparam-1]}

# Line for doing per parameter, per time stamp
#            echo "$USHwave/wave_ens_stats.sh ${nip} ${ngrib} ${fhour} 1> wave_ens_stats_${nip}_${fhour}.out 2>&1" >> cmdfile
# mpiserial block
            echo "$USHwave/wave_ens_stats.sh ${nip} ${ngrib} ${fhour} 1> wave_ens_stats_${nip}_${fhour}.out 2>&1" >> cmdfile.${ifile}

            if [ "$ncmdfile" -gt '1' ]
            then
              ifile=`expr $ifile + 1`
            fi
            if [ "$ifile" -gt "$ncmdfile" ]
            then
              ifile=1
            fi
 
      iparam=`expr ${iparam} + 1`

    done

    fhour=`expr $fhour + $dtgh`
  done

# 2.c Execute poe or serial command files

  if [ "$nparam" -gt '0' ]
  then

  set +x
  echo ' '
  echo " Generating $nmembn hourly to ${end_hour}h wave ensembles stats files "
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
   if [ "$ncmdfile" -gt '1' ]
   then
#     mpirun.lsf cfp cmdfile
     mpirun.lsf mpiserial
     exit=$?
   else
#     ./cmdfile
     ./cmdfile.1
     exit=$?
   fi
 fi

# 2.d.1 Ending times on the different processors

  if [ "$nparam" -gt '0' ]
  then
    set +x
    echo ' '
    echo "   Ending times for separate processors :"
    [[ "$LOUD" = YES ]] && set -x

    for file in `cat cmdfile`
    do
      if [ "`wc $file | awk '{ print $1 }'`" = '0' ]
      then
        set +x
        echo "      $file : no commands in this file."
        [[ "$LOUD" = YES ]] && set -x
      else
        words="`tail -1 $file | wc | awk '{ print $2 }'`"
        wrdnr=`expr $words - 1`
        echo "{ print "'$'"$wrdnr }" > awkfile
        outfile="`tail -1 $file | awk -f awkfile`"
        rm -f awkfile
        set +x
        echo "      $file : `tail -1 $outfile`"
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
  fi

# Regroup all outputs in parameter/stats files
# 2.d Command file set-up
#     The command file points to $ncmdfile files named cmdfile.$ifile.
#     The actual work is distributed over these files.

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile
# mpiserial block
  ifile=1
  rm -f cmdfile.*

  while [ "$ifile" -le "$ncmdfile" ]
  do
    touch cmdfile.$ifile
    chmod 700 cmdfile.$ifile
    echo "./cmdfile.$ifile" >> cmdfile
    ifile=`expr $ifile + 1`
  done

  ifile=1

  iparam=1

  while [ ${iparam} -le ${nparam} ]
  do
    nip=${arrpar[$iparam-1]}
    case $nip in
      HTSGW)   snip=hs ;;
      PERPW)   snip=tp ;;
      DIRPW)   snip=pdir ;;
      WVHGT)   snip=wshs ;;
      WVPER)   snip=wstp ;;
      WVDIR)   snip=wsdir ;;
      WIND)    snip=wnd ;;
      WDIR)    snip=wnddir ;;
      SWELL1)  snip=hswell1 ;;
      SWELL2)  snip=hswell2 ;;
      SWPER1)  snip=tswell1 ;;
      SWPER2)  snip=tswell2 ;;
      *)       nnip= ;;
    esac

# 2.e Cleanup base parameter files per member
    rm -f ${nip}_??.t${cyc}z.grib2

    fhour=0
    par_dir=tmp_${nip}
#    cd ${par_dir}

    while [ "$fhour" -lt "$end_hour" ]
    do

# Line for doing per parameter, per time stamp
      if [ $fhour -eq 0 ] ; then
        hhh='000'
      elif [ $fhour -lt 10 ] ; then
        hhh='00'$fhour
      elif [ $fhour -lt 100 ] ; then
        hhh='0'$fhour
      elif [ $fhour -ge 100 ] ; then
        hhh=$fhour
      fi

      valtime=`${utilexec}/ndate ${fhour} ${YMDH}`           

      stypes='mean spread probab'
      if [ "${snip}" = "pdir" ] || [ "${snip}" = "wnddir" ] || [ "${snip}" = "wsdir" ]
      then
        stypes='mean spread'
      fi

      for stype in ${stypes}
      do
  
        ingrib=${snip}_${stype}.${hhh}.grib2
        outgrib=${MDC}.${snip}_${stype}.t${cyc}z.grib2 

#        echo "$wgrib2  ./${par_dir}/${valtime}/${ingrib} -append -grib ./${outgrib} >> ${snip}_${stype}.t${cyc}z.out 2>> ${snip}_${stype}.t${cyc}z.err" >> cmdfile
        echo "$wgrib2  ./${par_dir}/${valtime}/${ingrib} -append -grib ./${outgrib} >> ${snip}_${stype}.t${cyc}z.out 2>> ${snip}_${stype}.t${cyc}z.err" >> cmdfile.${ifile}

      done

      fhour=`expr $fhour + $dtgh`
    done

# Cleanup appended data file
#    echo "rm -rf ./${par_dir}/${valtime}" >> cmdfile
# mpiserial block
    echo "rm -rf ./${par_dir}/${valtime}" >> cmdfile.${ifile}
    if [ "$ncmdfile" -gt '1' ]
    then
      ifile=`expr $ifile + 1`
    fi
    if [ "$ifile" -gt "$ncmdfile" ]
    then
      ifile=1
    fi

    iparam=`expr ${iparam} + 1`

  done

 set +x
  echo ' '
  echo " Regrouping stats files for ${nparam} parameters"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
   if [ "$ncmdfile" -gt '1' ]
   then
#     mpirun.lsf cfp cmdfile
     mpirun.lsf mpiserial
     exit=$?
   else
#     ./cmdfile
     ./cmdfile.1
     exit=$?
   fi

# 2.e Check for errors and create bundle files
  rm -f ${MDC}.mean.t${cyc}z.grib2 
  rm -f ${MDC}.spread.t${cyc}z.grib2 
  rm -f ${MDC}.probab.t${cyc}z.grib2
  iparam=1

  while [ ${iparam} -le ${nparam} ]
  do

    nip=${arrpar[$iparam-1]}
    case $nip in
      HTSGW)   snip=hs ;;
      PERPW)   snip=tp ;;
      DIRPW)   snip=pdir ;;
      WVHGT)   snip=wshs ;;
      WVPER)   snip=wstp ;;
      WVDIR)   snip=wsdir ;;
      WIND)    snip=wnd ;;
      WDIR)    snip=wnddir ;;
      SWELL1)  snip=hswell1 ;;
      SWELL2)  snip=hswell2 ;;
      SWPER1)  snip=tswell1 ;;
      SWPER2)  snip=tswell2 ;;
      *)       nnip= ;;
    esac

   if [ ! -f ${MDC}.${snip}_mean.t${cyc}z.grib2 ]
   then
     msg="ABNORMAL EXIT: ERR in generating statistics file"
     ./postmsg "$jlogfile" "$msg"
     set +x
     echo ' '
     echo '***************************************** '
     echo "***            FATAL ERROR            *** "
     echo "--- No ${MDC}.${snip}_mean.t${cyc}z.grib2 file --- "
     echo '***************************************** '
     echo ' '
     [[ "$LOUD" = YES ]] && set -x
     echo "No ${modIE}.${wndID}.$cycle.wind " >> $wavelog
     err=4;export err;./err_chk
     exit_code=4
   else
     set +x
     echo -e "\n Statistics files generated succesfully.\n"
     [[ "$LOUD" = YES ]] && set -x
     echo -e "\n ${COMOUT}/${MDC}.${snip}_mean.t${cyc}z.grib2\n"
# Large directory, cleanup is taking long time. Will leave for SPA to decide
#     rm -rf tmp_${nip}  
     rm -f wave_${modID}_stats_${nip}_*.out
     rm -f ${snip}_*.t${cyc}z.out ${snip}_*.t${cyc}z.err

# Bundle individual parameter files into a single file
     cat ${MDC}.${snip}_mean.t${cyc}z.grib2 >> ${MDC}.mean.t${cyc}z.grib2
     cat ${MDC}.${snip}_spread.t${cyc}z.grib2 >> ${MDC}.spread.t${cyc}z.grib2
     cat ${MDC}.${snip}_probab.t${cyc}z.grib2 >> ${MDC}.probab.t${cyc}z.grib2

   fi

    iparam=`expr ${iparam} + 1`

  done

#
# 3. Generate bulletin and time series files at complete set of buoy locations
#
  
# 3.a Buoy locations file massaging

  sed '/\$/d' buoy_file.data | sed '/STOPSTRING/d ' > buoy.file
  
  nbuoys=`cat buoy.file | wc -l`

# 3.b Command file set-up

  rm -f cmdfile
  touch cmdfile
  chmod 744 cmdfile
# mpiserial block
  rm -f cmdfile.*

  ifile=1

  while [ "$ifile" -le "$ncmdfile" ]
  do
    touch cmdfile.$ifile
    chmod 700 cmdfile.$ifile
    echo "./cmdfile.$ifile" >> cmdfile
    ifile=`expr $ifile + 1`
  done

  ifile=1

  ibuoy=1

# 3.c Create bundled grib2 file with all parameters
  cat ${MDC}.hs_*.t${cyc}z.grib2 ${MDC}.tp_*.t${cyc}z.grib2 ${MDC}.wnd_*.t${cyc}z.grib2 > gribfile

# 3.d Loop through buoys and populate cmdfiles with calls to wave_ens_bull.sh

  while [ ${ibuoy} -le ${nbuoys} ]
  do

    bline=`sed ''$ibuoy'!d' buoy.file`
    blat=`echo $bline | awk '{print $2}'`
    blon=`echo $bline | awk '{print $1}'`
    bnom=`echo $bline | awk '{print $3}' | sed "s/'//g"`

#    echo "$USHwave/wave_ens_bull.sh ${blon} ${blat} ${bnom} 1> bull_${bnom}.out 2>&1" >> cmdfile
# mpiserial blocki
    echo "$USHwave/wave_ens_bull.sh ${blon} ${blat} ${bnom} 1> bull_${bnom}.out 2>&1" >> cmdfile.${ifile}
    if [ "$ncmdfile" -gt '1' ]
    then
      ifile=`expr $ifile + 1`
    fi
    if [ "$ifile" -gt "$ncmdfile" ]
    then
      ifile=1
    fi

    ibuoy=`expr ${ibuoy} + 1`

  done

# 3.e Execute poe or serial cmdfile

  if [ "$nparam" -gt '0' ]
  then

  set +x
  echo ' '
  echo " Generating bulletins and ts files for ${nbuoys} locations."
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
   if [ "$ncmdfile" -gt '1' ]
   then
#     mpirun.lsf cfp cmdfile
     mpirun.lsf mpiserial
     exit=$?
   else
#     ./cmdfile
     ./cmdfile.${ifile}
     exit=$?
   fi
 fi


# mpiserial section
# 3.f.1 Ending times on the different processors

  if [ "$nparam" -gt '0' ]
  then
    set +x
    echo ' '
    echo "   Ending times for separate processors :"
    [[ "$LOUD" = YES ]] && set -x

    for file in `cat cmdfile`
    do
      if [ "`wc $file | awk '{ print $1 }'`" = '0' ]
      then
        set +x
        echo "      $file : no commands in this file."
        [[ "$LOUD" = YES ]] && set -x
      else
        words="`tail -1 $file | wc | awk '{ print $2 }'`"
        wrdnr=`expr $words - 1`
        echo "{ print "'$'"$wrdnr }" > awkfile
        outfile="`tail -1 $file | awk -f awkfile`"
        rm -f awkfile
        set +x
        echo "      $file : `tail -1 $outfile`"
        [[ "$LOUD" = YES ]] && set -x
      fi
    done
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
     ./postmsg "$jlogfile" "$msg"
     set +x
     echo ' '
     echo '***************************************** '
     echo "***            FATAL ERROR            *** "
     echo "--- No ${MDC}.${bnom}.bull file created --- "
     echo '***************************************** '
     echo ' '
     [[ "$LOUD" = YES ]] && set -x
     echo "No ${modIE}.${bnom}.bull " >> $wavelog
     err=5;export err;./err_chk
     exit_code=5
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
  tar cf ${MDC}.t${cyc}z.bull_tar ${MDC}.*.bull
  rm -f ${MDC}.*.bull
  tar cf ${MDC}.t${cyc}z.station_tar ${MDC}.*.ts
  rm -f ${MDC}.*.ts

  set +x
  echo ' '
  echo 'Saving output files :'
  echo '---------------------'
  [[ "$LOUD" = YES ]] && set -x

  if [ -f ${MDC}.t${cyc}z.bull_tar ]
  then
    set +x
    echo "   Copying ${MDC}.t${cyc}z.bull_tar  to $COMOUT"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${MDC}.t${cyc}z.bull_tar $COMOUT/.
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
     err=6;export err pgm;./err_chk
   fi


# 4.b Compress time series into tar file and copy to COMOUT
  if [ -f ${MDC}.t${cyc}z.station_tar ]
  then
    set +x
    echo "   Copying ${MDC}.t${cyc}z.bull_tar  to $COMOUT"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${MDC}.t${cyc}z.station_tar $COMOUT/.
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
     err=7;export err pgm;./err_chk
   fi

# 4.b Output all grib2 parameter files to COMOUT

  for stype in mean spread probab
  do
   if [ -f ${MDC}.${stype}.t${cyc}z.grib2 ]
   then
     set +x
     echo "   Copying ${MDC}.${stype}.t${cyc}z.grib2 to $COMOUT"
     [[ "$LOUD" = YES ]] && set -x
     cp -f ${MDC}.${stype}.t${cyc}z.grib2 $COMOUT/.
    else
      set +x
      echo ' '
      echo '*************************************** '
      echo '*** FATAL ERROR: No ${MDC}.${stype}.t${cyc}z.grib2 file found *'
      echo '*************************************** '
      echo ' '
      echo "$modIE fcst $date $cycle: ${MDC}.${stype}.t${cyc}z.grib2 not fouund." >> $wavelog
      echo $msg
      [[ "$LOUD" = YES ]] && set -x
      err=7;export err pgm;./err_chk
    fi
  done

  iparam=1
  while [ ${iparam} -le ${nparam} ]
  do
    nip=${arrpar[$iparam-1]}
    case $nip in
      HTSGW)   snip=hs ;;
      PERPW)   snip=tp ;;
      DIRPW)   snip=pdir ;;
      WVHGT)   snip=wshs ;;
      WVPER)   snip=wstp ;;
      WVDIR)   snip=wsdir ;;
      WIND)    snip=wnd ;;
      WDIR)    snip=wnddir ;;
      SWELL1)  snip=hswell1 ;;
      SWELL2)  snip=hswell2 ;;
      SWPER1)  snip=tswell1 ;;
      SWPER2)  snip=tswell2 ;;
      *)       nnip= ;;
    esac

    stypes='mean spread probab'
    if [ "${snip}" = "pdir" ] || [ "${snip}" = "wnddir" ] || [ "${snip}" = "wsdir" ]
    then
      stypes='mean spread'
    fi

    for stype in ${stypes} 
    do
      if [ -f ${MDC}.${snip}_${stype}.t${cyc}z.grib2 ]
      then
       set +x
       echo "   Copying ${MDC}.${snip}_${stype}.t${cyc}z.grib2 to $COMOUT"
       [[ "$LOUD" = YES ]] && set -x
       cp -f ${MDC}.${snip}_${stype}.t${cyc}z.grib2 $COMOUT/.
      else
        set +x
        echo ' '
        echo '*************************************** '
        echo '*** FATAL ERROR: No ${MDC}.${snip}_${stype}.t${cyc}z.grib2 file found *'
        echo '*************************************** '
        echo ' '
        echo "$modIE fcst $date $cycle: ${MDC}.${snip}_${stype}.t${cyc}z.grib2 not fouund." >> $wavelog
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        err=7;export err pgm;./err_chk
      fi
    done
    iparam=`expr ${iparam} + 1`
  done

#
# 4.b Alert DBN
#
  if [ "$SENDDBN" = 'YES' ]
  then
       $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2 $job $COMOUT/${MDC}.mean.t${cyc}z.grib2
       $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2 $job $COMOUT/${MDC}.spread.t${cyc}z.grib2
       $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2 $job $COMOUT/${MDC}.probab.t${cyc}z.grib2
       $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2 $job $COMOUT/${MDC}.t${cyc}z.bull_tar
       $DBNROOT/bin/dbn_alert MODEL WAVE_GRIB_GB2 $job $COMOUT/${MDC}.t${cyc}z.station_tar
  fi
#
  if [ "$exit_code" -ne '0' ]
  then
     msg="ABNORMAL EXIT: Problem in GWES STATS"
     ./postmsg "$jlogfile" "$msg"
     echo $msg
     err=$exit_code ; export err ; ./err_chk
  else
     touch $COMOUT/${MDC}.$cycle.statsdone
  fi

  msg="$job completed normally"
  ./postmsg "$jlogfile" "$msg"
#
  echo "Ending at : `date`"
#
# END
#
