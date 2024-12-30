#! /usr/bin/env bash

#
#  UTILITY SCRIPT NAME :  gfsbufr.sh
#               AUTHOR :  Hua-Lu Pan
#         DATE WRITTEN :  02/03/97
#
#  Abstract:  This utility script produces BUFR file of
#             station forecasts from the GFS suite.
#
#     Input:  none
# Script History Log:
# 2016-10-30  H Chuang: Tranistion to read nems output.
#             Change to read flux file fields in gfs_bufr
#             so remove excution of gfs_flux
# 2018-03-22 Guang Ping Lou: Making it works for either 1 hourly or 3 hourly output
# 2018-05-22 Guang Ping Lou: Making it work for both GFS and FV3GFS 
# 2018-05-30  Guang Ping Lou: Make sure all files are available.
# 2019-10-10  Guang Ping Lou: Read in NetCDF files
# 2024-03-03 Bo Cui: Add options to use different bufr table for different resolution NetCDF files
# 2024-08-08 Bo Cui: Update to handle one forecast at a time
# echo "History: February 2003 - First implementation of this utility script"
#

source "${USHgfs}/preamble.sh"

fhr="${1}"
fhr_p="${2}"
FINT="${3}"
F00FLAG="${4}"
workdir="${5}"

cd "${workdir}" || exit 2

if [[ "${F00FLAG}" == "YES" ]]; then
   f00flag=".true."
else
   f00flag=".false."
fi

export pgm="gfs_bufr.x"
#. prep_step

if [[ "${MAKEBUFR}" == "YES" ]]; then
   bufrflag=".true."
else
   bufrflag=".false."
fi

# check if read in bufr_ij_gfs_${CASE}.txt 

if [[ -s "${PARMgfs}/product/bufr_ij_gfs_${CASE}.txt"  ]]; then
  # use predetermined grid point(i,j) in bufr_gfs_${CASE}.txt 
  ${NLN} "${PARMgfs}/product/bufr_ij_gfs_${CASE}.txt" fort.7
  np1=0
else
  # find the nearest neighbor grid point(i,j) in the code
  np1=1
  echo "No bufr_ij_gfs_${CASE}.txt For CASE ${CASE}"
  echo "Find the nearest neighbor grid (i,j) in the code"
fi   

##fformat="netcdf"

CLASS="class1fv3"
cat << EOF > gfsparm
 &NAMMET
  levs=${LEVS},makebufr=${bufrflag},
  dird="${COM_ATMOS_BUFR}/bufr",
  nstart=${fhr},nend=${fhr},nint=${FINT},
  nend1=${NEND1},nint1=${NINT1},nint3=${NINT3},
  nsfc=80,f00=${f00flag},fformat=${fformat},np1=${np1},
  fnsig="sigf${fhr}",
  fngrib="flxf${fhr}", 
  fngrib2="flxf${fhr_p}" 
/
EOF

#---------------------------------------------------------
# Make sure all files are available:

filename="${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${fhr}.${logfm}"
if [[ -z ${filename} ]]; then
  echo "FATAL ERROR: COULD NOT LOCATE logf${fhr} file"
  exit 2
fi

filename="${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${fhr_p}.${logfm}"
if [[ -z ${filename} ]]; then
  echo "FATAL ERROR: COULD NOT LOCATE logf${fhr_p} file"
  exit 2
fi

#------------------------------------------------------------------
${NLN} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atmf${fhr}.${atmfm}" "sigf${fhr}"
${NLN} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.sfcf${fhr}.${atmfm}" "flxf${fhr}"
${NLN} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.sfcf${fhr_p}.${atmfm}" "flxf${fhr_p}"

#  define input BUFR table file.
${NLN} "${PARMgfs}/product/bufr_gfs_${CLASS}.tbl" fort.1
${NLN} "${STNLIST:-${PARMgfs}/product/bufr_stalist.meteo.gfs}" fort.8


#------------------------------------------------------------------
"${EXECgfs}/${pgm}" < gfsparm > "out_gfs_bufr_${fhr}"

export err=$?

if [[ "${err}" -ne 0 ]]; then
   echo "GFS postsnd job error, Please check files "
   echo "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atmf${fhr}.${atmfm}"
   echo "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.sfcf${fhr}.${atmfm}"
   err_chk
fi

exit "${err}"
