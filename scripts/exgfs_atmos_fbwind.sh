#! /usr/bin/env bash

#####################################################################
# echo "------------------------------------------------"
# echo "JGFS_BULLS - 24hr GFS processing"
# echo "------------------------------------------------"
# echo "History: Jul 2004 - First implementation of this new script."
# echo "         FBWNDGFS (FB Winds) program for 15 sites outside" 
# echo "          the Hawaiian Islands."
# echo "         Feb 2006 - L Sager  Send bulletins to TOC via NTC.  "
# echo "         Jul 2014 - B Vuong  Modified to use GFS master GRIB2"
# echo "                             and Add bulletins WINTEMV process."
# echo "         Sep 2016 - B Vuong  Modified to use GFS 0p25 deg GRIB2"
# echo "         Nov 2019 - B Vuong  Removed WINTEMV bulletin (retired)"
#####################################################################

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 2

######################
# Set up Here Files.
######################

outfile_name="${COMOUT}/${RUN}.atmos.t${cyc}z.fbwind.pacific.ascii"

set +x
echo " "
echo "#############################################################"
echo " Process Bulletins of forecast winds and temps for Hawaii    "
echo " and 15 sites outside of the Hawaiian Islands.               "
echo "#############################################################"
echo " "
set_trace

export pgm=bulls_fbwndgfs
source prep_step

for fhr3 in 006 012 024; do
  cp "${COMIN_ATMOS_GRIB_0p25}/gfs.${cycle}.pgrb2.0p25.f${fhr3}"   "tmp_pgrb2_0p25${fhr3}" 
  cp "${COMIN_ATMOS_GRIB_0p25}/gfs.${cycle}.pgrb2b.0p25.f${fhr3}"  "tmp_pgrb2b_0p25${fhr3}"
  cat "tmp_pgrb2_0p25${fhr3}" "tmp_pgrb2b_0p25${fhr3}" > "tmp0p25filef${fhr3}"
  # shellcheck disable=SC2312
  ${WGRIB2} "tmp0p25filef${fhr3}" | grep -F -f "${PARMgfs}/product/gfs_fbwnd_parmlist_g2" | \
    ${WGRIB2} -i -grib "tmpfilef${fhr3}" "tmp0p25filef${fhr3}"
  ${CNVGRIB} -g21 "tmpfilef${fhr3}" "gfs.t${cyc}z.grbf${fhr3}_grb1"
  ${GRBINDEX} "gfs.t${cyc}z.grbf${fhr3}_grb1" "gfs.t${cyc}z.grbf${fhr3}_grb1.idx"
done

export FORT11="gfs.t${cyc}z.grbf006_grb1"
export FORT12="gfs.t${cyc}z.grbf012_grb1"
export FORT13="gfs.t${cyc}z.grbf024_grb1"

#       GFS grib index files

export FORT31="gfs.t${cyc}z.grbf006_grb1.idx"
export FORT32="gfs.t${cyc}z.grbf012_grb1.idx"
export FORT33="gfs.t${cyc}z.grbf024_grb1.idx"

#
#   1280 byte transmission file
#

export FORT51="tran.fbwnd_pacific"

cp "${PARMgfs}/product/fbwnd_pacific.stnlist" fbwnd_pacific.stnlist

"${EXECgfs}/fbwndgfs.x" < fbwnd_pacific.stnlist >> "${pgmout}" 2> errfile
export err=$?; err_chk

"${USHgfs}/make_ntc_bull.pl" WMOBH NONE KWNO NONE tran.fbwnd_pacific "${outfile_name}"

############################### END OF SCRIPT #######################
