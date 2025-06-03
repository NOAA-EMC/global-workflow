#! /usr/bin/env bash

cd "${DATA}" || exit 2

outname=out.tif

convert gif:"${input}" fax:"${outname}"

#
#  Add the ntc heading:
#

WMO=QTUA11
ORIG=KWBC
PDYHH="${PDY}${cyc}"

if [[ "${HEADER}" == "YES" ]]; then
   INPATH="${DATA}/${outname}"
   SUB=DFAX1064
   "${HOMEgfs}/ush/make_NTC_file.pl" "${WMO}" "${ORIG}" "${PDYHH}" "${SUB}" "${INPATH}" "${OUTPATH}"
#
#  Send the graphic to TOC

   cpfs "${OUTPATH}" "${COMOUT_ATMOS_WMO}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif"
   if [[ "${SENDDBN}" == "YES" ]]; then

      "${DBNROOT}/bin/dbn_alert" GRIB_LOW "${NET}" "${job}" "${COMOUT_ATMOS_WMO}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif"
   fi
fi
