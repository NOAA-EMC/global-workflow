#! /usr/bin/env bash

###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the              #
# GFSv16-wave output for gridded wave fields                                  #
#                                                                             #
# COM inputs:                                                                 #
#  - ${COMIN_WAVE_GRID}/${RUN}.wave.${cycle}.${grdIDin}.f${fhr}.grib2         #
#                                                                             #
# COM outputs:                                                                #
#  - ${COMOUT_WAVE_WMO}/grib2.${cycle}.f${fhr}.awipsww3_${grdOut}             #
#                                                                             #
# Origination  : 05/02/2007                                                   #
# Last update  : 10/08/2020                                                   #
#                                                                             #
# Oct, 2020  Roberto.Padilla@noaa.gov, Henrique.HAlves@noaa.gov               #
#         - Merging wave scripts to GFSv16 global workflow                    #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/wave_domain_grid.sh"

# 0.a Basic modes of operation

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
 cat << EOF

                         ****************************
                         *** MWW3 PRODUCTS SCRIPT ***
                         ****************************
                                    ${date} ${cycle}

Starting at : ${start_time}

   AWIPS grib fields
   Wave  Grids       : ${grids}
EOF

# --------------------------------------------------------------------------- #
# 1.  Get necessary files
 printf "\nPreparing input files\n-----------------------"

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
 grdIDin="${grdNAME}"

 fhcnt="${fstart}"
 while [[ "${fhcnt}" -le "${FHMAX_WAV}" ]]; do
   fhr=$(printf "%03d" "${fhcnt}")
   for grdOut in ${grids}; do
     process_grdID "${grdOut}"
     grdIDin="${grdNAME}"
     com_varname="COMIN_WAVE_GRID_${GRDREGION}_${GRDRES}"
     com_dir="${!com_varname}"

     GRIBIN="${com_dir}/${RUN}.wave.${cycle}.${grdIDin}.f${fhr}.grib2"
     GRIBIN_chk="${GRIBIN}.idx"
     sleep_interval=5
     max_tries=1000
     if ! wait_for_file "${GRIBIN_chk}" "${sleep_interval}" "${max_tries}"; then
       export err=1
       err_chk "FATAL ERROR: ${GRIBIN_chk} not found after waiting $((sleep_interval * ( max_tries - 1))) secs"
     fi
     GRIBOUT="${RUN}.wave.${cycle}.${grdID}.f${fhr}.clipped.grib2"

     iparam=1
     while [[ ${iparam} -le ${nparam} ]]; do
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
       if [[ ${npart} -eq 0 ]]; then
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
     GRIBIN="${RUN}.wave.${cycle}.${grdID}.f${fhr}.clipped.grib2"

     ${NLN} "${GRIBIN}" "gribfile.${grdID}.f${fhr}"

     #
# 1.d Input template files
     parmfile="${PARMgfs}/wave/grib2_${RUN}wave.${grdOut}.f${fhr}"
     if [[ -f "${parmfile}" ]]; then
       ${NLN} "${parmfile}" "awipsgrb.${grdID}.f${fhr}"
     else
       export err=3
       err_chk "FATAL ERROR: NO template  grib2_${RUN}wave.${grdID}.f${fhr}"
     fi
     #
# 2.  AWIPS product generation
# 2.a AWIPS GRIB file with headers
     echo ' '
     echo 'AWIPS headers to GRIB file ...'
     echo '------------------------------'

# 2.a.1 Set up for tocgrib2
     echo "   Do set up for tocgrib2."
     AWIPSGRB=awipsgrib
# 2.a.2 Make GRIB index
     echo "   Make GRIB index for tocgrib2."
     ${GRB2INDEX} "gribfile.${grdID}.f${fhr}" "gribindex.${grdID}.f${fhr}"
     OK=$?

     if [[ ${OK} -ne 0 ]]
     then
       export err=4
       export pgm="grb2index"
       err_chk "FATAL ERROR: ERROR IN grb2index MWW3 for grid ${grdID}"
     fi

# 2.a.3 Run AWIPS GRIB packing program tocgrib2

     echo "   Run tocgrib2"
     export pgm=tocgrib2
     export pgmout=tocgrib2.out
     source prep_step

     export FORT11="gribfile.${grdID}.f${fhr}"
     export FORT31="gribindex.${grdID}.f${fhr}"
     export FORT51="${AWIPSGRB}.${grdID}.f${fhr}"

     ${TOCGRIB2} < "awipsgrb.${grdID}.f${fhr}" > tocgrib2.out 2>&1
     export err=$?
     if [[ ${err} -ne 0 ]]; then
       cat tocgrib2.out
       err_chk "FATAL ERROR: ERROR IN tocgrib2"
     else
       echo '*** tocgrib2 ran succesfully *** '
     fi
# 2.a.7 Get the AWIPS grib bulletin out ...
     echo "   Get awips GRIB bulletins out ..."
     echo "      Saving ${AWIPSGRB}.${grdOut}.f${fhr} as grib2.${cycle}.awipsww3_${grdID}.f${fhr}"
     echo "          in ${COMOUT_WAVE_WMO}"
     cpfs "${AWIPSGRB}.${grdID}.f${fhr}" "${COMOUT_WAVE_WMO}/grib2.${cycle}.f${fhr}.awipsww3_${grdOut}"

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
