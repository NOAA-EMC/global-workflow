#! /usr/bin/env bash

###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the              #
# GFSv16-wave output for gridded wave fields                                  #
#                                                                             #
# Remarks :                                                                   #
# - Supplemental error output is witten to the wave.log file.                 #
#                                                                             #
# COM inputs:                                                                 #
#  - ${COMIN_WAVE_GRID}/${RUNwave}.${cycle}.${grdIDin}.f${fhr}.grib2          #
#                                                                             #
# COM outputs:                                                                #
#  - ${COMOUT_WAVE_WMO}/grib2.${cycle}.f${fhr}.awipsww3_${grdOut}             #
#                                                                             #
# Origination  : 05/02/2007                                                   #
# Last update  : 10/08/2020                                                   # 
#                                                                             #
# Oct, 2020  Roberto.Padilla@noaa.gov, Henrique.HAlves@noaa.gov                # 
#         - Merging wave scripts to GFSv16 global workflow                    #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"
source "${USHgfs}/wave_domain_grid.sh"

# 0.a Basic modes of operation

 export RUNwave=${RUNwave:-${RUN}wave}
 export envir=${envir:-ops}
 export fstart=${fstart:-0}
 export FHMAX_WAV=${FHMAX_WAV_WMO:-180}      #180 Total of hours to process
 export FHMAX_HF_WAV=${FHMAX_HF_WAV_WMO:-72} #from 00 to 72 inc=3
 export FHOUT_WAV=${FHOUT_WAV_WMO:-6}        #from 72 to 180 inc=6
 export FHOUT_HF_WAV=${FHOUT_HF_WAV_WMO:-3}
 export maxtries=720
 export cyc=${cyc:-00}
 export cycle=${cycle:-t${cyc}z}
 export pgmout=OUTPUT.$$
 export DATA=${DATA:-${DATAROOT:?}/${job}.$$}
 mkdir -p "${DATA}"
 cd "${DATA}" || exit 1
 export wavelog=${DATA}/${RUNwave}_prdggridded.log
 
 echo "Starting MWW3 GRIDDED PRODUCTS SCRIPT"
# Input grid
grid_in="${waveinterpGRD:-glo_15mxt}"
# Output grids
grids=${GEMPAK_GRIDS:-ak_10m at_10m ep_10m wc_10m glo_30m}
# export grids=${wavepostGRD}
 maxtries=${maxtries:-720}
# 0.b Date and time stuff
 start_time=$(date)
 export date=${PDY}
 export YMDH=${PDY}${cyc}
 echo ' '
 echo '                         ****************************'
 echo '                         *** MWW3 PRODUCTS SCRIPT ***'
 echo '                         ****************************'
 echo "                                       ${date} ${cycle}"
 echo ' '
 echo "Starting at : ${start_time}"
 echo ' '
 echo "   AWIPS grib fields"
 echo "   Wave  Grids       : ${grids}"
 echo ' '
 set_trace

# --------------------------------------------------------------------------- #
# 1.  Get necessary files
 echo ' '
 echo 'Preparing input files :'
 echo '-----------------------'
 set_trace
#=======================================================================
 
 ASWELL=(SWELL1 SWELL2) # Indices of HS from partitions
 ASWPER=(SWPER1 SWPER2) # Indices of PERIODS from partitions 
 ASWDIR=(SWDIR1 SWDIR2) # Indices of DIRECTIONS from partitions 
                                #  (should be same as ASWELL)
 #export arrpar=(WIND UGRD VGRD HTSGW PERPW DIRPW WVHGT WVPER WVDIR WDIR ${ASWELL[@]} ${ASWDIR[@]} ${ASWPER[@]})
 export arrpar=(WIND WDIR UGRD VGRD HTSGW PERPW DIRPW WVHGT "${ASWELL[@]}" WVPER "${ASWPER[@]}" WVDIR "${ASWDIR[@]}" )
 export nparam=$(echo "${arrpar[@]}" | wc -w)


# 1.a Grib file (AWIPS and FAX charts)
 # Get input grid
 # TODO flesh this out with additional input grids if needed
 process_grdID "${grid_in}"
 grdIDin=${grdNAME}

 fhcnt=${fstart}
 while [[ "${fhcnt}" -le "${FHMAX_WAV}" ]]; do
   fhr=$(printf "%03d" "${fhcnt}")
   for grdOut in ${grids}; do
     process_grdID "${grdout}"
     grdIDin=${grdNAME}
     com_varname="${COMIN_WAVE_GRID}_${GRDREGION}_${GRDRES}"
     com_dir="${!com_varname}"

     GRIBIN="${com_dir}/${RUNwave}.${cycle}.${grdIDin}.f${fhr}.grib2"
     GRIBIN_chk="${GRIBIN}.idx"
     sleep_interval=5
     max_tries=1000
     if ! wait_for_file "${GRIBIN_chk}" "${sleep_interval}" "${max_tries}"; then
       echo "FATAL ERROR: ${GRIBIN_chk} not found after waiting $((sleep_interval * ( max_tries - 1))) secs"
       echo "${RUNwave} ${grdIDin} ${fhr} prdgen ${date} ${cycle} : GRIB file missing." >> "${wavelog}"
       err=1;export err;${errchk} || exit "${err}"
     fi
     GRIBOUT="${RUNwave}.${cycle}.${grdID}.f${fhr}.clipped.grib2"

     iparam=1
     while [[ "${iparam}" -le "${nparam}" ]]; do
       nip=${arrpar[${iparam}-1]}
       prepar=${nip::-1} # Part prefix (assumes 1 digit index)
       paridx="${nip:0-1}"
       npart=0   
       case ${prepar} in
         SWELL)  npart=1 ;;
         SWDIR)  npart=1 ;;
         SWPER)  npart=1 ;;
         *)     npart=0 ;;
       esac
       echo "${nip} ${prepar} ${paridx} ${npart}"
       rm -f temp.grib2 
       if [[ "${npart}" -eq 0 ]]; then 
         #shellcheck disable=SC2312
         ${WGRIB2} "${GRIBIN}" -s | grep ":${nip}" | "${WGRIB2}" -i "${GRIBIN}" -grib temp.grib2 > wgrib.out 2>&1 
         #shellcheck disable=SC2312
         ${WGRIB2} temp.grib2 -append -grib "${GRIBOUT}"
       else
         #shellcheck disable=SC2312
         ${WGRIB2} "${GRIBIN}" -s | grep ":${prepar}" |  grep "${paridx} in sequence" | \
                 ${WGRIB2} -i "${GRIBIN}" -grib temp.grib2  > wgrib.out 2>&1 
         ${WGRIB2} temp.grib2 -append -grib "${GRIBOUT}"
       fi
       iparam=$(( iparam + 1 ))
     done #end wave param loop
#======================================================================
     GRIBIN="${RUNwave}.${cycle}.${grdID}.f${fhr}.clipped.grib2"

     ${NLN} "${GRIBIN}" "gribfile.${grdID}.f${fhr}"

     #
# 1.d Input template files
     parmfile="${PARMgfs}/wave/grib2_${RUNwave}.${grdOut}.f${fhr}"
     if [[ -f "${parmfile}" ]]; then
       ${NLN} "${parmfile}" "awipsgrb.${grdID}.f${fhr}"
     else
       echo "FATAL ERROR: NO template  grib2_${RUNwave}.${grdID}.f${fhr}"
       echo "${RUNwave} ${grdID} ${fhr} prdgen ${date} ${cycle} : GRIB template file missing." >> "${wavelog}"
       err=3;export err;${errchk} || exit "${err}"
     fi
     #
# 2.  AWIPS product generation
# 2.a AWIPS GRIB file with headers
     echo ' '
     echo 'AWIPS headers to GRIB file ...'
     echo '------------------------------'

# 2.a.1 Set up for tocgrib2
     echo "   Do set up for tocgrib2."
     set_trace
     AWIPSGRB=awipsgrib
# 2.a.2 Make GRIB index
     echo "   Make GRIB index for tocgrib2."
     set_trace
     ${GRB2INDEX} "gribfile.${grdID}.f${fhr}" "gribindex.${grdID}.f${fhr}"
     OK=$?

     if [[ ${OK} -ne 0 ]]
     then
       msg="ABNORMAL EXIT: ERROR IN grb2index MWW3 for grid ${grdID}"
       #set +x
       echo ' '
       echo '******************************************** '
       echo '*** FATAL ERROR : ERROR IN grb2index MWW3 *** '
       echo '******************************************** '
       echo ' '
       echo "${msg}"
       #set_trace
       echo "${RUNwave} ${grdID} prdgen ${date} ${cycle} : error in grbindex." >> "${wavelog}"
       err=4;export err;err_chk
     fi

# 2.a.3 Run AWIPS GRIB packing program tocgrib2

     echo "   Run tocgrib2"
     set_trace
     export pgm=tocgrib2
     export pgmout=tocgrib2.out
     . prep_step

     export FORT11="gribfile.${grdID}.f${fhr}"
     export FORT31="gribindex.${grdID}.f${fhr}"
     export FORT51="${AWIPSGRB}.${grdID}.f${fhr}"

     ${TOCGRIB2} < "awipsgrb.${grdID}.f${fhr}" > tocgrib2.out 2>&1
     OK=$?
     if [[ ${OK} -ne 0 ]]; then
       cat tocgrib2.out
       msg="ABNORMAL EXIT: ERROR IN tocgrib2"
       #set +x
       echo ' '
       echo '*************************************** '
       echo '*** FATAL ERROR : ERROR IN tocgrib2 *** '
       echo '*************************************** '
       echo ' '
       echo "${msg}"
       #set_trace
       echo "${RUNwave} prdgen ${date} ${cycle} : error in tocgrib2." >> "${wavelog}"
       err=5;export err;err_chk
     else
       echo '*** tocgrib2 ran succesfully *** '
     fi
# 2.a.7 Get the AWIPS grib bulletin out ...
     #set +x
     echo "   Get awips GRIB bulletins out ..."
     #set_trace
     #set +x
     echo "      Saving ${AWIPSGRB}.${grdOut}.f${fhr} as grib2.${cycle}.awipsww3_${grdID}.f${fhr}"
     echo "          in ${COMOUT_WAVE_WMO}"
     #set_trace
     cp "${AWIPSGRB}.${grdID}.f${fhr}" "${COMOUT_WAVE_WMO}/grib2.${cycle}.f${fhr}.awipsww3_${grdOut}"
     #set +x


     if [[ "${SENDDBN}" != 'YES' ]]
     then
       echo "      Sending ${AWIPSGRB}.${grdID}.f${fhr} to DBRUN."
       "${DBNROOT}/bin/dbn_alert" GRIB_LOW "${RUN}" "${job}" "${COMOUT_WAVE_WMO}/grib2.${cycle}.f${fhr}.awipsww3_${grdOut}"
     fi
     rm -f "${AWIPSGRB}.${grdID}.f${fhr}" tocgrib2.out
   done # For grids

   if [[ ${fhcnt} -ge ${FHMAX_HF_WAV} ]]; then
     inc="${FHOUT_WAV}"
   else
     inc="${FHOUT_HF_WAV}"
   fi
   ((fhcnt = fhcnt+inc))
 done #For fcst time



# --------------------------------------------------------------------------- #
# 5.  Clean up

  set -v
  rm -f gribfile gribindex.* awipsgrb.* awipsbull.data
  set +v

# --------------------------------------------------------------------------- #
# 6.  Ending output



# End of GFSWAVE product generation script -------------------------------------- #
