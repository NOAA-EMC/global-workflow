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

  MEMDIR="ensstat"  YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_STATION_ENS:COM_WAVE_STATION_TMPL

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

  STATION_TAR="./${RUN}.t${cyc}z.station_tar"
  BULL_TAR="./${RUN}.t${cyc}z.bull_tar"



# 1.a Check if buoy input files exist and copy
#
# 2. link station files

    # Correct indirect variable expansion
       dir_var="${COMOUT_WAVE_STATION_ENS}"

    # Ensure directory exists before proceeding
       if  [ ! -d "$dir_var" ]; then
               echo "Error: Directory '$cpdir' does not exist or is empty."
        exit 2
       fi
    # Use ls to safely check for matching files
    for file in "${dir_var}/${RUN}.t${cyc}z.f"???.*_tar; do
      if [[ -f "$file" ]]; then  # Ensure it's a file before linking
	      cp -rp "$file" .
	      #this line needs to be figured out when we decide where these temp files are saved, 
	      #right now I delete it from COM once they are copied to $DATA here 
	      rm "$file"
        else
              msg="ABNORMAL EXIT: Error in copying $cpfile"
              echo "$msg"
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
 for tarfile in ./${RUN}.t*z.f*.bull_tar; do
	 tar -xf "$tarfile"  
 done

 for tarfile in ./${RUN}.t*z.f*.station_tar; do
	 tar -xf "$tarfile" 
 done


     # Get unique buoy numbers from extracted files
     BUOY_LIST=$(ls gefs.wave.*.*.bull | cut -d'.' -f3 | sort -u)

     # Merge files for each buoy
     for buoy in $BUOY_LIST; do
	     cat "${RUN}.$buoy".f*.bull > "${RUN}.${buoy}.bull"
	     cat "${RUN}.$buoy".f*.ts > "${RUN}.${buoy}.ts"
	     rm  "${RUN}.$buoy".f*.ts "${RUN}.$buoy".f*.bull
     done

         # Step 3: Archive the processed buoy files
         echo "Creating final tar archive..."
         tar -cf "$BULL_TAR" "${RUN}".*.bull
	 tar -cf "$STATION_TAR" "${RUN}".*.ts


         echo "Processing complete. Final tar:"

  set +x
  echo ' '
  echo 'Saving output files :'
  echo '---------------------'
  [[ "$LOUD" = YES ]] && set -x

  if [ -s ${RUN}.t${cyc}z.bull_tar ]
  then
    set +x
    echo "   Copying ${RUN}.t${cyc}z.bull_tar  to COMOUT_WAVE_STATION_ENS"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${RUN}.t${cyc}z.bull_tar ${COMOUT_WAVE_STATION_ENS}
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
  if [ -s ${RUN}.t${cyc}z.station_tar ]
  then
    set +x
    echo "   Copying ${RUN}.t${cyc}z.bull_tar  to ${COMOUT_WAVE_STATION_ENS}"
    [[ "$LOUD" = YES ]] && set -x
    cp -f ${RUN}.t${cyc}z.station_tar ${COMOUT_WAVE_STATION_ENS}
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
       $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station/${RUN}.t${cyc}z.bull_tar
       $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/station/${RUN}.t${cyc}z.station_tar
  fi
#
  if [ "$exit_code" -ne '0' ]
  then
     echo "FATAL ERROR: Problem in WAVE POINT STAT"
     msg="ABNORMAL EXIT: Problem in WAVE POINT STAT"
     echo "$msg"
     echo $msg
     export err=12;${errchk}
     exit $err
  fi

  msg="$job completed normally"
  echo "$msg"
#
  echo "Ending at : `date`"
#
# END
#
