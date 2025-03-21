#!/bin/bash
#                                                                       
################################################################################
#                                                                              #
# exgefs_stats_pnt.sh - Compute point output statistics for gefs wave products #
#                                                                              #        
# Packs ensemble mean, spread and probabilities in bull_tar and station_tar    #
#                                                                              #             
# Requirements:                                                                #    
# - WGRIB2 with IPOLATES library                                               #              
#                                                                              #              
# Origination:                                                                 #
# - EMC Wave model Developer- Saeideh Banihashemi, March 2025 	               #               
#                                                                              #          
# Update log since 2025                                                        #
# Jan2025 SBanihash - Adding this script to the global workflow                #
################################################################################
#
  set -x
  #Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x

  set +x
  echo -e '                   ******************************************\n'
  echo '                   *** WAVE POINT JOB ENSEMBLE STATS SCRIPT ***'
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
  source "${USHgfs}/wave_domain_grid.sh"
  process_grdID "${wavepostGRD}"

  MEMDIR="ensstat" GRID=${wavepostGRD} YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_GRIB_ENS:COM_WAVE_GRIB_GRID_TMPL

#
#
# 0.c Time management
#
 
  ymdh=$($NDATE)
  YMD=$(echo $ymdh | cut -c1-8)
  HMS="$(echo $ymdh | cut -c9-10)0000"
  YMDHMS=${YMD}${HMS}


  mkdir output_$YMDHMS
  cd output_$YMDHMS

  STATION_TAR="./${WAV_MOD_TAG}.t${cyc}z.station_tar"
  BULL_TAR="./${WAV_MOD_TAG}.t${cyc}z.bull_tar"



# 1.a Check if buoy input files exist and copy
#
# 2. link station files

    # Correct indirect variable expansion
       dir_var="${COMOUT_WAVE_GRIB_ENS}"
#       cpdir="${!dir_var}"

    # Ensure directory exists before proceeding
       if  [ ! -d "$dir_var" ]; then
               echo "Error: Directory '$cpdir' does not exist or is empty."
        exit 2
       fi
    # Use ls to safely check for matching files
    for file in "${dir_var}/${WAV_MOD_TAG}.t${cyc}z.f"???.*_tar; do
      if [[ -f "$file" ]]; then  # Ensure it's a file before linking
	      cp -rp "$file" .
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

# Extract all .bull_tar files
 echo "Extracting all bull_tar files..."
 for tarfile in ./${WAV_MOD_TAG}.t*z.f*.bull_tar; do
	 tar -xf "$tarfile"  
 done

 for tarfile in ./${WAV_MOD_TAG}.t*z.f*.station_tar; do
	 tar -xf "$tarfile" 
 done


     # Get unique buoy numbers from extracted files
     BUOY_LIST=$(ls "${WAV_MOD_TAG}".*.*.bull | cut -d'.' -f2 | sort -u)

     # Merge files for each buoy
     for buoy in $BUOY_LIST; do
         cat "${WAV_MOD_TAG}"."$buoy".f*.bull > "${WAV_MOD_TAG}.${buoy}.bull"
	 cat "${WAV_MOD_TAG}"."$buoy".f*.ts > "${WAV_MOD_TAG}.${buoy}.ts"
	 rm  "${WAV_MOD_TAG}"."$buoy".f*.ts "${WAV_MOD_TAG}"."$buoy".f*.bull
     done

         # Step 3: Archive the processed buoy files
         echo "Creating final tar archive..."
         tar -cf "$BULL_TAR" "${WAV_MOD_TAG}".*.bull
	 tar -cf "$STATION_TAR" "${WAV_MOD_TAG}".*.ts


         echo "Processing complete. Final tar: $FINAL_TAR"


  MEMDIR="ensstat" GRID=${wavepostGRD} YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_GRIB_ENS:COM_WAVE_GRIB_GRID_TMPL

  set +x
  echo ' '
  echo 'Saving output files :'
  echo '---------------------'
  [[ "$LOUD" = YES ]] && set -x

  if [ -s ${WAV_MOD_TAG}.t${cyc}z.bull_tar ]
  then
    set +x
    echo "   Copying ${WAV_MOD_TAG}.t${cyc}z.bull_tar  to COMOUT_WAVE_GRIB_ENS"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${WAV_MOD_TAG}.t${cyc}z.bull_tar ${COMOUT_WAVE_GRIB_ENS}
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
    echo "   Copying ${WAV_MOD_TAG}.t${cyc}z.bull_tar  to ${COMOUT_WAVE_GRIB_ENS}"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${WAV_MOD_TAG}.t${cyc}z.station_tar ${COMOUT_WAVE_GRIB_ENS}
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
     echo "FATAL ERROR: Problem in WAVE POINT STAT"
     msg="ABNORMAL EXIT: Problem in WAVE POINT STAT"
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
