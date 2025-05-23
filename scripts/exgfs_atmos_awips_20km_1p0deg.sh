#! /usr/bin/env bash

##############################################################################
#  UTILITY SCRIPT NAME :  exgfs_awips_20km_1p0deg.sh
#         DATE WRITTEN :  11/01/2017
#
#  Abstract:  This utility script produces the GFS AWIPS 20km and 1.0 deg
#              grids GRIB2
#
#     Input:  1 arguments are passed to this script.
#             1st argument - Forecast Hour - format of 3I (3 digits)
#
###############################################################################
# echo "------------------------------------------------"
# echo "JGFS_AWIPS_00/06/12/18 GFS postprocessing"
# echo "------------------------------------------------"
# echo "History: NOV  2017 - First implementation of this new script to  "
# echo "                     process GFS AWIPS 20km and 1.0 deg grids products "
# echo "         MAR  2025 - Remove job reference from product name strings "
# echo " "
###############################################################################

fcsthr="$1"
num=$#

if [[ ${num} -ne 1 ]]; then
   echo ""
   echo " FATAL ERROR: Incorrect number of arguments "
   echo ""
   echo ""
   echo "Usage: $0  \${fcsthr} (3 digits) "
   echo ""
   exit 16
fi

cd "${DATA}" || exit 2

# "Import" functions used in this script
source "${USHgfs}/product_functions.sh"

###############################################
# Wait for the availability of the pgrb file
###############################################
sleep_interval=10
max_tries=180
idxfile="${COMIN_ATMOS_GRIB_0p25}/${RUN}.${cycle}.pgrb2b.0p25.f${fcsthr}.idx"
if ! wait_for_file "${idxfile}" "${sleep_interval}" "${max_tries}"; then
  msg="FATAL ERROR: No GFS pgrb2 file after waiting"
  err_exit "${msg}"
fi

########################################

echo " ------------------------------------------"
echo " BEGIN MAKING GFS AWIPS PRODUCTS"
echo " ------------------------------------------"

set +x
echo " "
echo "#######################################"
echo " Process GRIB AWIP GRIB2 PRODUCTS      "
echo "#######################################"
echo " "
set_trace

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

###############################################################
#    Process GFS GRIB AWIP PRODUCTS IN GRIB2                  #
###############################################################

cpreq "${COMIN_ATMOS_GRIB_0p25}/gfs.t${cyc}z.pgrb2.0p25.f${fcsthr}" "tmpfile2${fcsthr}"
cpreq "${COMIN_ATMOS_GRIB_0p25}/gfs.t${cyc}z.pgrb2b.0p25.f${fcsthr}" "tmpfile2b${fcsthr}"
cat "tmpfile2${fcsthr}" "tmpfile2b${fcsthr}" > "tmpfile${fcsthr}"
${WGRIB2} "tmpfile${fcsthr}" | grep -F -f "${PARMgfs}/product/gfs_awips_parmlist_g2" | \
   ${WGRIB2} -i -grib masterfile "tmpfile${fcsthr}" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
   err_exit "masterfile does not exist."
fi

${WGRIB2} masterfile -match ":PWAT:entire atmosphere" -grib gfs_pwat.grb
${WGRIB2} masterfile | grep -v ":PWAT:entire atmosphere" | ${WGRIB2} -i -grib temp_gfs masterfile
##################################################################
#  Process to change PWAT from level 200 to 10 (Entire Atmosphere)
#  in production defintion template (PDT) 4.0
##################################################################
${WGRIB2} gfs_pwat.grb -set_byte 4 23 10 -grib gfs_pwat_levels_10.grb && true
export err=$?
if [[ ${err} -ne 0 ]]; then
   err_exit "Failed to redefine PWAT for the entire atmosphere!"
fi

cat temp_gfs   gfs_pwat_levels_10.grb > tmp_masterfile

for GRID in conus ak prico pac 003; do
   case ${GRID} in
      conus)
         gridconus="lambert:265.0:25.0:25.0 226.541:369:20318.0 12.19:257:20318.0"
         # shellcheck disable=SC2086,SC2248
         ${WGRIB2} tmp_masterfile ${opt1uv} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
            ${opt27} ${opt28} -new_grid ${gridconus} "awps_file_f${fcsthr}_${GRID}"
         ;;
      ak)
         gridak="nps:210.0:60.0 170.0:277:22500 35.0:225:22500"
         # shellcheck disable=SC2086,SC2248
         ${WGRIB2} tmp_masterfile ${opt1uv} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
            ${opt27} ${opt28} -new_grid ${gridak} "awps_file_f${fcsthr}_${GRID}"
         ;;
      prico)
         gridprico="latlon 271.75:275:0.25 50.75:205:-0.25"
         # shellcheck disable=SC2086,SC2248
         ${WGRIB2} tmp_masterfile ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
            ${opt27} ${opt28} -new_grid ${gridprico} "awps_file_f${fcsthr}_${GRID}"
         ;;
      pac)
         gridpac="mercator:20.0 110.0:837:20000:270.0 -45.0:725:20000:65.7345"
         # shellcheck disable=SC2086,SC2248
         ${WGRIB2} tmp_masterfile ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
            ${opt27} ${opt28} -new_grid ${gridpac} "awps_file_f${fcsthr}_${GRID}"
         ;;
      003)
         ######################################################################
         #    Process GFS GRIB AWIP 1.0 DEGREE (GRID 003)  PRODUCTS IN GRIB2  #
         ######################################################################
         grid003="latlon 0:360:1.0 90:181:-1.0"
         # shellcheck disable=SC2086,SC2248
         ${WGRIB2} tmp_masterfile ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} \
            ${opt27} ${opt28} -new_grid ${grid003} "awps_file_f${fcsthr}_${GRID}"
         ;;
      *)
         export err=2
         err_exit "Unknown output grid ${GRID}"
         ;;
   esac
   trim_rh "awps_file_f${fcsthr}_${GRID}"
   scale_dec "awps_file_f${fcsthr}_${GRID}"
   ${GRB2INDEX} "awps_file_f${fcsthr}_${GRID}" "awps_file_fi${fcsthr}_${GRID}"

   ###########################################################################
   # Checking fields in awps_file_f${fcsthr}_${GRID} file
   # before TOCGRIB2 adding WMO headers for AWIPS products.
   #
   # NOTE: numparm is the total of fields in grib2_awpgfs_20km_conusf000 file
   ###########################################################################
   numparm=247
   numrec=$( ${WGRIB2} "awps_file_f${fcsthr}_${GRID}" | wc -l )

   if [[ ${numrec} -lt ${numparm} ]]; then
       export err=1
       msg="awps_file_f${fcsthr}_${GRID} file is missing fields for AWIPS !"
       err_exit "${msg}"
   fi

   # Processing AWIPS GRIB2 grids with WMO headers

   pgm=tocgrib2
   export pgm; prep_step
   startmsg

   if [[ ${GRID} = "003" && $(( 10#${fcsthr} % 6 )) == 0 ]]; then
      export FORT11="awps_file_f${fcsthr}_${GRID}"
      export FORT31="awps_file_fi${fcsthr}_${GRID}"
      export FORT51="grib2.awpgfs${fcsthr}.${GRID}"

      cpreq "${PARMgfs}/wmo/grib2_awpgfs${fcsthr}.${GRID}" "parm_list"

      ${TOCGRIB2} < "parm_list" >> "${pgmout}" 2> errfile && true
      export err=$?
      if [[ ${err} -ne 0 ]]; then
         err_exit "Failed to generate the awips Grib2 file!"
      fi

      ##############################
      # Post Files to ${COMOUT_ATMOS_WMO}
      ##############################

      cpfs "grib2.awpgfs${fcsthr}.${GRID}" \
         "${COMOUT_ATMOS_WMO}/grib2.awpgfs${fcsthr}.${GRID}"

      ##############################
      # Distribute Data
      ##############################

      if [[ "${SENDDBN}" == 'YES' || "${SENDAWIP}" == 'YES' ]]; then
          "${DBNROOT}/bin/dbn_alert" NTC_LOW "${NET}" "${job}" \
				     "${COMOUT_ATMOS_WMO}/grib2.awpgfs${fcsthr}.${GRID}"
      else
          echo "File ${COMOUT_ATMOS_WMO}/grib2.awpgfs${fcsthr}.${GRID} not posted to db_net."
      fi
   elif [[ ${GRID} != "003" ]]; then
      export FORT11="awps_file_f${fcsthr}_${GRID}"
      export FORT31="awps_file_fi${fcsthr}_${GRID}"
      export FORT51="grib2.awpgfs_20km_${GRID}_f${fcsthr}"

      cpreq "${PARMgfs}/wmo/grib2_awpgfs_20km_${GRID}f${fcsthr}" "parm_list"

      ${TOCGRIB2} < "parm_list" >> "${pgmout}" 2> errfile && true
      export err=$?
      if [[ ${err} -ne 0 ]]; then
         err_exit "Failed to write the AWIPS grib2 file"
      fi

      ##############################
      # Post Files to ${COMOUT_ATMOS_WMO}
      ##############################

      cpfs "grib2.awpgfs_20km_${GRID}_f${fcsthr}" \
         "${COMOUT_ATMOS_WMO}/grib2.awpgfs_20km_${GRID}_f${fcsthr}"

      ##############################
      # Distribute Data
      ##############################

      if [[ "${SENDDBN}" = 'YES' || "${SENDAWIP}" = 'YES' ]]; then
          "${DBNROOT}/bin/dbn_alert" NTC_LOW "${NET}" "${job}" \
          "${COMOUT_ATMOS_WMO}/grib2.awpgfs_20km_${GRID}_f${fcsthr}"
      else
          echo "File ${COMOUT_ATMOS_WMO}/grib2.awpgfs_20km_${GRID}_f${fcsthr} not posted to db_net."
      fi
   fi
   echo "Awip Processing ${fcsthr} hour completed normally"

done

if [[ -e "${pgmout}" ]]; then
   cat "${pgmout}"
fi


############## END OF SCRIPT #######################
