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
cat << EOF

    *******************************************
    ***WAVE POINT JOB ENSEMBLE STATS SCRIPT****
    *******************************************

Starting at : $(date)
-------------

EOF

#
# 0.a Define model grid
#
source "${USHgfs}/wave_domain_grid.sh"
process_grdID "${wavepostGRD}"

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
#
#
if [[ -z "${NTASKS}" ]]; then
  export err=1
  err_exit "Requires NTASKS to be set"
fi
MEMDIR="ensstat"  YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_STATION_ENS:COM_WAVE_STATION_TMPL

#
# 0.b Time management
#
#fhr3=$(printf %03i ${FORECAST_HOUR})
valid_time=$(date -u -d "${PDY} ${cyc}" "+%Y%m%d%H")
ymdh_init=$(date -u -d "${valid_time:0:8} ${valid_time:8:2} - ${WAVHINDH} hours" "+%Y%m%d%H")

mkdir output_${ymdh_init}
cd output_${ymdh_init}

STATION_TAR="./${RUN}.t${cyc}z.station_tar"
BULL_TAR="./${RUN}.t${cyc}z.bull_tar"



# 1.a Check if buoy input files exist and copy
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
    export err=2
    err_exit "ABNORMAL EXIT: Error in copying $cpfile, "
  fi
done

#

# 2.a Extract all .bull_tar files
echo "Extracting all bull_tar files..."
for tarfile in ./${RUN}.t*z.f*.bull_tar; do
  tar -xf "$tarfile"  
done

for tarfile in ./${RUN}.t*z.f*.station_tar; do
  tar -xf "$tarfile" 
done


# Get unique buoy numbers from extracted files
BUOY_LIST=$(ls gefs.wave.*.*.bull | cut -d'.' -f3 | sort -u)

# 2.b Merge files for each buoy
for buoy in $BUOY_LIST; do
  cat "${RUN}.$buoy".f*.bull > "${RUN}.${buoy}.bull"
  cat "${RUN}.$buoy".f*.ts > "${RUN}.${buoy}.ts"
  rm  "${RUN}.$buoy".f*.ts "${RUN}.$buoy".f*.bull
done

# 3. Archive the processed buoy files
echo "Creating final tar archive..."
tar -cf "$BULL_TAR" "${RUN}".*.bull
tar -cf "$STATION_TAR" "${RUN}".*.ts


echo "Processing complete. Final tar:"

set +x
echo ' '
echo 'Saving output files :'
echo '---------------------'

if [ -s ${RUN}.t${cyc}z.bull_tar ]
then
  set +x
  echo "   Copying ${RUN}.t${cyc}z.bull_tar  to COMOUT_WAVE_STATION_ENS"
  cp -f ${RUN}.t${cyc}z.bull_tar ${COMOUT_WAVE_STATION_ENS}
else
  set +x
  export err=10
  err_exit " FATAL ERROR: No bull_tar file found, $modIE fcst $date $cycle: bull_tar not found."
fi


# 4.b Compress time series into tar file and copy to COMOUT
if [ -s ${RUN}.t${cyc}z.station_tar ]
then
  set +x
  echo "   Copying ${RUN}.t${cyc}z.bull_tar  to ${COMOUT_WAVE_STATION_ENS}"
  cp -f ${RUN}.t${cyc}z.station_tar ${COMOUT_WAVE_STATION_ENS}
else
  set +x
  export err=11
  err_exit "FATAL ERROR: No station_tar file found, $modIE fcst $date $cycle: station_tar not found."
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

echo "$job completed normally"
#
echo "Ending at : `date`"
#
# END
#
