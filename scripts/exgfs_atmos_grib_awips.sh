#! /usr/bin/env bash

######################################################################
#  UTILITY SCRIPT NAME :  exgfs_grib_awips.sh
#         DATE WRITTEN :  10/04/2004
#
#  Abstract:  This utility script produces the  GFS AWIPS GRIB
#
#     Input:  1 arguments are passed to this script.
#             1st argument - Forecast Hour - format of 2I
#
#####################################################################
# echo "------------------------------------------------"
# echo "JGFS_AWIPS_00/06/12/18 GFS postprocessing"
# echo "------------------------------------------------"
# echo "History: OCT 2004 - First implementation of this new script."
# echo "         JUN 2014 - Modified to remove process for AWIPS in GRIB2"
# echo "                    to script exgfs_grib_awips_g2.sh and this "
# echo "                    script only process AWIPS GRIB1 (211 and 225)"
# echo "         AUG 2015 - Modified for WCOSS phase2"
# echo "         FEB 2019 - Removed grid 225"
#####################################################################

source "${USHgfs}/preamble.sh"

fcsthrs="$1"
num=$#
job_name=${job/[jpt]gfs/gfs}

if (( num != 1 )); then
   echo ""
   echo " FATAL ERROR: Incorrect number of arguments "
   echo ""
   echo ""
   echo "Usage: $0  \${fcsthrs} (3 digits) "
   echo ""
   exit 16
fi

cd "${DATA}/awips_g1" || exit 2

# "Import" functions used in this script
source "${USHgfs}/product_functions.sh"

###############################################
# Wait for the availability of the pgrb file
###############################################
icnt=1
while (( icnt < 1000 )); do
   if [[ -s "${COM_ATMOS_GRIB_0p25}/${RUN}.${cycle}.pgrb2b.0p25.f${fcsthrs}.idx" ]]; then
      break
   fi

   sleep 10
   icnt=$((icnt + 1))
   if (( icnt >= 180 )); then
      msg="FATAL ERROR: No GFS pgrb2 file after 30 min of waiting"
      err_exit "${msg}"
      exit 5
   fi
done

echo " ------------------------------------------"
echo " BEGIN MAKING GFS GRIB1 AWIPS PRODUCTS"
echo " ------------------------------------------"

set +x
echo " "
echo "###############################################"
echo " Process GFS GRIB1 AWIP PRODUCTS (211) "
echo "###############################################"
echo " "
set_trace

cp "${COM_ATMOS_GRIB_0p25}/gfs.t${cyc}z.pgrb2.0p25.f${fcsthrs}" "tmpfile2"
cp "${COM_ATMOS_GRIB_0p25}/gfs.t${cyc}z.pgrb2b.0p25.f${fcsthrs}" "tmpfile2b"
cat tmpfile2 tmpfile2b > tmpfile
${WGRIB2} tmpfile | grep -F -f "${PARMgfs}/product/gfs_awips_parmlist_g2" | \
   ${WGRIB2} -i -grib masterfile tmpfile
scale_dec masterfile
${CNVGRIB} -g21 masterfile masterfile.grib1

ln -s masterfile.grib1 fort.11

"${EXECgfs}/overgridid.x" << EOF
255
EOF

mv fort.51 "master.grbf${fcsthrs}"
rm fort.11

${GRBINDEX} "master.grbf${fcsthrs}" "master.grbif${fcsthrs}"

###############################################################
#    Process GFS GRIB1 AWIP GRIDS 211 PRODUCTS
###############################################################

DBNALERT_TYPE=GRIB_LOW

startmsg

# GRID=211 out to 240 hours:

export GRID=211
export FORT11="master.grbf${fcsthrs}"
export FORT31="master.grbif${fcsthrs}"
export FORT51="xtrn.awpgfs${fcsthrs}.${GRID}"
#   $MKGFSAWPS < ${PARMgfs}/wmo/grib_awpgfs${fcsthrs}.${GRID} parm=KWBC >> $pgmout 2>errfile
"${EXECgfs}/mkgfsawps.x" < "${PARMgfs}/wmo/grib_awpgfs${fcsthrs}.${GRID}" parm=KWBC >> "${pgmout}" 2>errfile
export err=$?; err_chk 
##############################
# Post Files to ${COM_ATMOS_WMO}
##############################

cp "xtrn.awpgfs${fcsthrs}.${GRID}" "${COM_ATMOS_WMO}/xtrn.awpgfs${fcsthrs}.${GRID}.${job_name}"

##############################
# Distribute Data
##############################

if [[ "${SENDDBN}" == 'YES' || "${SENDAWIP}" == 'YES' ]] ; then
    "${DBNROOT}/bin/dbn_alert" "${DBNALERT_TYPE}" "${NET}" "${job}" \
			       "${COM_ATMOS_WMO}/xtrn.awpgfs${fcsthrs}.${GRID}.${job_name}"
else
    echo "File xtrn.awpgfs${fcsthrs}.${GRID}.${job_name} not posted to db_net."
fi

if [[ -e "${pgmout}" ]] ; then
   cat "${pgmout}"
fi

###############################################################################


############## END OF SCRIPT #######################
