#! /usr/bin/env bash

#####################################################################
# echo "-----------------------------------------------------"
# echo " exglobal_grib2_special_npoess.sh"
# echo " Jan 2008 - Chuang - Produces 1x1 degree special Grib from master."
# echo "-----------------------------------------------------"
#####################################################################

source "${USHgfs}/preamble.sh"

cd $DATA

############################################################
#  Define Variables:
#  -----------------
#  SHOUR        is the starting forecast hour. normally 0 except for restarts.
#  FHOUR        is the ending forecast hour.
#  FHINC        is the increment hour for each forecast steps.
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off pgrb 
#               generation.
############################################################

############################################################
# NO processing Analysis special Files 
############################################################

# Set type of Interpolation for WGRIB2
export opt1=' -set_grib_type same -new_grid_winds earth '
export opt1uv=' -set_grib_type same -new_grid_winds grid '
export opt21=' -new_grid_interpolation bilinear -if '
export opt22=":(CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export opt23=' -new_grid_interpolation neighbor -fi '
export opt24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
export opt25=":(APCP|ACPCP|PRATE|CPRAT):"
export opt26=' -set_grib_max_bits 25 -fi -if '
export opt27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
export opt28=' -new_grid_interpolation budget -fi '

####################################
# Specify Timeout Behavior of Post
#
# SLEEP_TIME - Amount of time to wait for
#              a restart file before exiting
# SLEEP_INT  - Amount of time to wait between
#              checking for restart files
####################################
export SLEEP_TIME=${SLEEP_TIME:-900}
export SLEEP_INT=${SLEEP_TIME:-5}

SLEEP_LOOP_MAX=$(( SLEEP_TIME / SLEEP_INT ))

# TODO: Does this section do anything? I retained if for clarity of
# changes/updates, but it does not appear to do anything.

####################################
# Check if this is a restart
####################################
if [[ -f "${COM_ATMOS_GOES}/${RUN}.t${cyc}z.control.goessimpgrb2" ]]; then
   modelrecvy=$(cat < "${COM_ATMOS_GOES}/${RUN}.t${cyc}z.control.goessimpgrb")
   recvy_cyc="${modelrecvy:8:2}"
   recvy_shour="${modelrecvy:10:13}"

   if [[ ${RERUN} == "NO" ]]; then
      NEW_SHOUR=$(( recvy_shour + FHINC ))
      if (( NEW_SHOUR >= SHOUR )); then
         export SHOUR="${NEW_SHOUR}"
      fi
      if (( recvy_shour >= FHOUR )); then
         echo "Forecast Pgrb Generation Already Completed to ${FHOUR}"
      else
         echo "Starting: PDY=${PDY} cycle=t${recvy_cyc}z SHOUR=${SHOUR}"
      fi
   fi
fi

##############################################################################
# Specify Forecast Hour Range F000 - F024 for GFS_NPOESS_PGRB2_0P5DEG
##############################################################################
export SHOUR=000
export FHOUR=024
export FHINC=003
if [[ "${FHOUR}" -gt "${FHMAX_GFS}" ]]; then
   export FHOUR="${FHMAX_GFS}"
fi

############################################################
# Loop Through the Post Forecast Files 
############################################################
for (( fhr=$((10#${SHOUR})); fhr <= $((10#${FHOUR})); fhr = fhr + FHINC )); do

   fhr3=$(printf "%03d" "${fhr}")

   ###############################
   # Start Looping for the
   # existence of the restart files
   ###############################
   export pgm="postcheck"
   ic=1
   while (( ic <= SLEEP_LOOP_MAX )); do
      if [[ -f "${COM_ATMOS_GRIB_0p50}/gfs.t${cyc}z.pgrb2b.0p50.f${fhr3}.idx" ]]; then
         break
      else
         ic=$((ic + 1))
         sleep "${SLEEP_INT}"
      fi
      ###############################
      # If we reach this point assume
      # fcst job never reached restart
      # period and error exit
      ###############################
      if (( ic == SLEEP_LOOP_MAX )); then
         echo "FATAL ERROR: 0p50 grib file not available after max sleep time"
         export err=9
         err_chk || exit "${err}"
      fi
   done

   ######################################################################
   # Process Global NPOESS 0.50 GFS GRID PRODUCTS IN GRIB2 F000 - F024  #
   ######################################################################
   paramlist=${PARMgfs}/product/global_npoess_paramlist_g2
   cp "${COM_ATMOS_GRIB_0p50}/gfs.t${cyc}z.pgrb2.0p50.f${fhr3}" tmpfile2
   cp "${COM_ATMOS_GRIB_0p50}/gfs.t${cyc}z.pgrb2b.0p50.f${fhr3}" tmpfile2b
   cat tmpfile2 tmpfile2b > tmpfile
   ${WGRIB2} tmpfile | grep -F -f ${paramlist} | ${WGRIB2} -i -grib  pgb2file tmpfile
   export err=$?; err_chk

   cp pgb2file "${COM_ATMOS_GOES}/${RUN}.${cycle}.pgrb2f${fhr3}.npoess"

   if [[ ${SENDDBN} == "YES" ]]; then
       "${DBNROOT}/bin/dbn_alert" MODEL GFS_PGBNPOESS "${job}" \
				  "${COM_ATMOS_GOES}/${RUN}.${cycle}.pgrb2f${fhr3}.npoess"
   else
       msg="File ${RUN}.${cycle}.pgrb2f${fhr3}.npoess not posted to db_net."
       postmsg "${msg}" || echo "${msg}"
   fi
   echo "${PDY}${cyc}${fhr3}" > "${COM_ATMOS_GOES}/${RUN}.t${cyc}z.control.halfdeg.npoess"
   rm tmpfile pgb2file

done

################################################################
# Specify Forecast Hour Range F000 - F180 for GOESSIMPGRB files 
################################################################
export SHOUR=000
export FHOUR=180
export FHINC=003
if [[ "${FHOUR}" -gt "${FHMAX_GFS}" ]]; then
   export FHOUR="${FHMAX_GFS}"
fi

#################################
# Process GFS PGRB2_SPECIAL_POST
#################################

for (( fhr=$((10#${SHOUR})); fhr <= $((10#${FHOUR})); fhr = fhr + FHINC )); do

   fhr3=$(printf "%03d" "${fhr}")

   ###############################
   # Start Looping for the 
   # existence of the restart files
   ###############################
   set +x
   export pgm="postcheck"
   ic=1
   while (( ic <= SLEEP_LOOP_MAX )); do
      if [[ -f "${COM_ATMOS_GOES}/${RUN}.t${cyc}z.special.grb2if${fhr3}.idx" ]]; then
         break
      else
         ic=$((ic + 1))
         sleep "${SLEEP_INT}"
      fi
      ###############################
      # If we reach this point assume
      # fcst job never reached restart
      # period and error exit
      ###############################
      if (( ic == SLEEP_LOOP_MAX )); then
         echo "FATAL ERROR: Special goes grib file not available after max sleep time"
         export err=9
         err_chk || exit "${err}"
      fi
   done
   set_trace
   ###############################
   # Put restart files into /nwges 
   # for backup to start Model Fcst
   ###############################
   cp "${COM_ATMOS_GOES}/${RUN}.t${cyc}z.special.grb2if${fhr3}" masterfile
   export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
   ${WGRIB2} masterfile ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
      ${opt27} ${opt28} -new_grid ${grid0p25} pgb2file

   export gridconus="lambert:253.0:50.0:50.0 214.5:349:32463.0 1.0:277:32463.0"
   ${WGRIB2} masterfile ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
      ${opt27} ${opt28} -new_grid ${gridconus} pgb2file2

   ${WGRIB2} pgb2file -s > pgb2ifile

   cp pgb2file "${COM_ATMOS_GOES}/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr3}"
   cp pgb2ifile "${COM_ATMOS_GOES}/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr3}.idx"
   cp pgb2file2 "${COM_ATMOS_GOES}/${RUN}.${cycle}.goessimpgrb2f${fhr3}.grd221"

   if [[ ${SENDDBN} == "YES" ]]; then
       "${DBNROOT}/bin/dbn_alert" MODEL GFS_GOESSIMPGB2_0P25 "${job}" \
				  "${COM_ATMOS_GOES}/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr}"
       "${DBNROOT}/bin/dbn_alert" MODEL GFS_GOESSIMPGB2_0P25_WIDX "${job}" \
				  "${COM_ATMOS_GOES}/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr}.idx"
       "${DBNROOT}/bin/dbn_alert" MODEL GFS_GOESSIMGRD221_PGB2 "${job}" \
				  "${COM_ATMOS_GOES}/${RUN}.${cycle}.goessimpgrb2f${fhr}.grd221"
   fi

   echo "${PDY}${cyc}${fhr}" > "${COM_ATMOS_GOES}/${RUN}.t${cyc}z.control.goessimpgrb"
   rm pgb2file2 pgb2ifile

   if [[ ${SENDECF} == "YES" ]]; then
      # TODO Does this even do anything?
      export fhour=$(( fhr % 6 ))
   fi

done


################## END OF SCRIPT #######################
