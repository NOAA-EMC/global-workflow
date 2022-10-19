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
# echo "History: February 2003 - First implementation of this utility script"
#
source "${HOMEgfs:?}/ush/preamble.sh"

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

##fformat="nc"
##fformat="nemsio"

CLASS="class1fv3"
cat << EOF > gfsparm
 &NAMMET
  levs=${LEVS},makebufr=${bufrflag},
  dird="${COMOUT}/bufr.${cycle}/bufr",
  nstart=${FSTART},nend=${FEND},nint=${FINT},
  nend1=${NEND1},nint1=${NINT1},nint3=${NINT3},
  nsfc=80,f00=${f00flag},fformat=${fformat},np1=0
/
EOF

hh=${FSTART}
while (( hh <= FEND )); do
  hh2=$(printf %02i ${hh})
  hh3=$(printf %03i ${hh})

#---------------------------------------------------------
# Make sure all files are available:
   ic=0
   while (( ic < 1000 )); do
      if [ ! -f "${COMIN}/${RUN}.${cycle}.logf${hh3}.${logfm}" ]; then
          sleep 10
          ic=$((ic + 1))
      else
          break
      fi

      if (( ic >= 360 )); then
         echo "FATAL: COULD NOT LOCATE logf${hh3} file AFTER 1 HOUR"
         exit 2
      fi
   done
#------------------------------------------------------------------
   ln -sf "${COMIN}/${RUN}.${cycle}.atmf${hh3}.${atmfm}" "sigf${hh2}" 
   ln -sf "${COMIN}/${RUN}.${cycle}.sfcf${hh3}.${atmfm}" "flxf${hh2}"

   hh=$((hh + FINT))
done

#  define input BUFR table file.
ln -sf "${PARMbufrsnd}/bufr_gfs_${CLASS}.tbl" fort.1
ln -sf "${STNLIST:-${PARMbufrsnd}/bufr_stalist.meteo.gfs}" fort.8
ln -sf "${PARMbufrsnd}/bufr_ij13km.txt" fort.7

${APRUN_POSTSND} "${EXECbufrsnd}/${pgm}" < gfsparm > "out_gfs_bufr_${FEND}"
export err=$?

if [ $err -ne 0 ]; then
   echo "GFS postsnd job error, Please check files "
   echo $COMIN/${RUN}.${cycle}.atmf${hh2}.${atmfm}
   echo $COMIN/${RUN}.${cycle}.sfcf${hh2}.${atmfm}
   err_chk
fi

exit ${err}
