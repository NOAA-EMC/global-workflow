#! /usr/bin/env bash
#===============================================================================
#
#   FILE: make_tif.sh
#
#   DESCRIPTION: This script converts an input GIF graphic into a TIF format
#                using ImageMagick. If configured, it then prepends a standard
#                NOAA Telecommunications (NTC) header (WMO: QTUA11, ORIG: KWBC)
#                using a Perl utility. Finally, it copies the formatted file to
#                the operational WMO COM directory and issues a DBN alert for
#                downstream distribution.
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
    "${HOMEglobal}/ush/make_NTC_file.pl" "${WMO}" "${ORIG}" "${PDYHH}" "${SUB}" "${INPATH}" "${OUTPATH}"
    #
    #  Send the graphic to TOC

    cpfs "${OUTPATH}" "${COMOUT_ATMOS_WMO}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif"
    if [[ "${SENDDBN}" == "YES" ]]; then

        "${DBNROOT}/bin/dbn_alert" GRIB_LOW "${NET}" "${job}" "${COMOUT_ATMOS_WMO}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif"
    fi
fi
