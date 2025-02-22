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
 source "${USHgfs}/wave_domain_grid.sh"
 process_grdID "${wavepostGRD}"

#
# 0.c Time management
#
 
  fhr=$(( 10#${FHR3} ))
  fhrg=$fhr
  ymdh=$($NDATE $fhr ${PDY}${cyc})
  YMD=$(echo $ymdh | cut -c1-8)
  HMS="$(echo $ymdh | cut -c9-10)0000"
  YMDHMS=${YMD}${HMS}
  FH3=$(printf %03i $fhr)

  fcmdnow=cmdfile.${FH3}
  mkdir output_$YMDHMS
  cd output_$YMDHMS
  rm -f ${fcmdnow} 
  touch ${fcmdnow}


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

#
# 1.b Link grib2 data for all members
#
  ngrib=0
  inc=$FHOUT_HF_WAV
  ftype="mem"
      
      ngrib=$(( $ngrib + 1 ))
      for me in $membn
      do
        ENSTAG=${ftype}${me}
        cpfile=${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/gridded/${grdNAME}/${WAV_MOD_TAG}.${cycle}.${grdNAME}.f${FH3}.grib2
        if [ -s "${cpfile}" ] ; then 
          ln -s  "$cpfile"  ./${WAV_MOD_TAG}.${cycle}.${ENSTAG}.${grdNAME}.f${FH3}.grib2
        else
          msg="ABNORMAL EXIT: ERR in coping $cpfile "
          postmsg "$msg"
          echo ' '
          echo '******************************************************* '
          echo "*** FATAL ERROR: No $cpfile copied. *** "
          echo '******************************************************* '
          echo ' '
          echo "$cpfile missing." >> $ensemb_log
          export err=2;${errchk}
          #exit ${err}
        fi
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
    if [ ${iparam} -eq 3 ] ||[ ${iparam} -eq 4 ] || [ ${iparam} -eq 5 ] || \
       [ ${iparam} -eq 10 ] || [ ${iparam} -eq 15 ] || [ ${iparam} -eq 16 ] ||
       [ ${iparam} -eq 17 ] || [ ${iparam} -eq 18 ] || [ ${iparam} -eq 21 ]
    then
      echo "Parameter $snip not yet available for stats"
    else
        for me in $membn
          do
           ENSTAG=${ftype}${me}
           infile=${WAV_MOD_TAG}.${cycle}.${ENSTAG}.${grdNAME}.f${FH3}.grib2
           outfile=${nnip}_${me}.t${cyc}z.${grdNAME}.f${FH3}.grib2
           wgfileout=wgrib_${nnip}_${me}.out
		   if [ "${npart}" = "0" ]
		   then 
			   echo " $WGRIB2 -match ${nip} -match surface ${infile} -grib ${outfile} > ${wgfileout} 2>&1" >> ${fcmdnow}
		   else
			   echo " $WGRIB2 -match ${prepar} -match \"${paridx} in sequence\" ${infile} -grib ${outfile} > ${wgfileout} 2>&1" >> ${fcmdnow}
		   fi
        done    #for members
    fi
      iparam=`expr ${iparam} + 1`
  done    #for parameters
  # END all loops

# 2.c Execute poe or serial command files

  set +x
   echo ' '
   echo " Generating $nmembn hourly to ${FHMAX_WAV}h wave ensembles stats files "
   echo ' '
       [[ "$LOUD" = YES ]] && set -x
         
               
  "${HOMEgfs}/ush/run_mpmd.sh" $fcmdnow
   err=$?


#
# 2. Generate ensemble mean, spread and probability files
# 
# 2.b Populate command files with stats wave_ens_stats.sh calls
#
  rm -f cmdmfile cmdfile.$ cmdmprog
  rm -f ${fcmdnow} 
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
        echo "nip ngrib fhr: ${nip}, ${ngrib}, ${fhr}"
        echo " ${HOMEgfs}/ush/wave_ens_stat.sh ${nip} ${ngrib} ${fhr} 1 ${grdNAME} " >> cmdfile

      fi

        iparam=`expr ${iparam} + 1`

    done

# 2.c Execute poe or serial command files

  set +x
  echo ' '
  echo " Generating $nmembn hourly to ${FHMAX_WAV}h wave ensembles stats files "
  echo ' '
  [[ "$LOUD" = YES ]] && set -x


  "${HOMEgfs}/ush/run_mpmd.sh" cmdfile
   exit=$?


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


    FH3=$(printf "%03d" $fhr)
    valtime=`$NDATE ${fhr} ${CDATE}`  
    iparam=1

    while [ ${iparam} -le ${nparam} ]
    do
      nip=${arrpar[$iparam-1]}
      case $nip in
        HTSGW)   stypes='mean spread prob' ; snip=hs ;;
        PERPW)   stypes='mean spread prob' ; snip=tp ;;
        ICEC)    stypes='mean spread prob' ; snip=ice ;;
        DIRPW)   stypes='mean spread ' ; snip=pdir ;;
        IMWF)    stypes='mean spread prob' ; snip=tm ;;
        MWSP)    stypes='mean spread prob' ; snip=tz ;;
        WVHGT)   stypes='mean spread prob' ; snip=wshs ;;
        WVPER)   stypes='mean spread prob' ; snip=wstp ;;
        WVDIR)   stypes='mean spread' ; snip=wsdir ;;
        WWSDIR)  stypes='mean spread' ; snip=wwdir ;;
        WIND)    stypes='mean spread prob' ; snip=wnd ;;
        WDIR)    stypes='mean spread' ; snip=wnddir ;;
        SWELL1)  stypes='mean spread prob' ; snip=hswell1 ;;
        SWELL2)  stypes='mean spread prob' ; snip=hswell2 ;;
        SWELL3)  stypes='mean spread prob' ; snip=hswell3 ;;
        SWDIR1)  stypes='mean spread' ; snip=dswell1 ;;
        SWDIR2)  stypes='mean spread' ; snip=dswell2 ;;
        SWDIR3)  stypes='mean spread' ; snip=dswell3 ;;
        SWPER1)  stypes='mean spread prob' ; snip=tswell1 ;;
        SWPER2)  stypes='mean spread prob' ; snip=tswell2 ;;
        SWPER3)  stypes='mean spread prob' ; snip=tswell3 ;;
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
          outgrib=${WAV_MOD_TAG}.t${cyc}z.${stype}.${grdNAME}.f${FH3}.grib2 
          echo "$WGRIB2  ./${par_dir}/${valtime}/${ingrib} -append -grib ./${outgrib} " >> ncmdfile

        done

      fi
      iparam=$((iparam + 1))
      echo "IPARAM: $iparam"
    done

  chmod 744 ncmdfile

    set +x
    echo ' '
    echo " Generating $nmembn hourly to ${FHMAX_WAV}h wave ensembles stats files "
     echo ' '
   [[ "$LOUD" = YES ]] && set -x


   "${HOMEgfs}/ush/run_mpmd.sh" ncmdfile
   exit=$?

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

    FH3=$(printf "%03d" $fhr)
    MEMDIR="ensstat" GRID=${wavepostGRD} YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_GRIB_ENS:COM_WAVE_GRIB_GRID_TMPL


    for stype in mean spread prob
    do
      fcopy=${WAV_MOD_TAG}.t${cyc}z.${stype}.${grdNAME}.f${FH3}.grib2
      if [ -s ${fcopy} ]
      then
        set +x
        echo "   Copying ${fcopy} to ensstat and ALERT if SENDDBN=YES"
        [[ "$LOUD" = YES ]] && set -x
        #if [ $SENDCOM = "YES" ] ; then
          cp -f ${fcopy}  "${COMOUT_WAVE_GRIB_ENS}"
# 2.g Alert DBN
          if [ "$SENDDBN" = 'YES' ]
          then
           MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
           $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/gridded/${fcopy}
          fi
	#fi
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


  msg="$job completed normally"
  postmsg "$msg"
#
  echo "Ending at : `date`"
#
# END
#
