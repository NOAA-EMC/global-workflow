#! /usr/bin/env bash

################################################################
# Script Name:		exgfs_atmos_postsnd.sh.sms
# Script Description:	Generate GFS BUFR sounding files
# Script History Log:
#   1) 2003-03-25       Hualu Pan       First Implementation
#   2) 2010-05-25       V. Krishna Kumar Modified for the GFS 
#                                  resolution upgrade
#   3) 2014-08-01       D. Carlis Updated to vertical structure 
#                                 and T1534 Resolution 
#   4) 2016-11-01       H. Chuang Update to read new model nems output
#   5) 2017-02-21       Guang Ping Lou setup mpmd to speedup the run
#                                 and 1 & 3 hourly output
#   6) 2018-03-22       Guang Ping Lou  Take FV3GFS configuration
#                          parameters as input; make it work for 
#                          both FV3GFS and GFS
#   7) 2018-07-18       Guang Ping Lou Generalize this version to other platforms
#   8) 2019-10-18       Guang Ping Lou Transition to reading in NetCDF model data
#   9) 2019-12-18       Guang Ping Lou generalizing to reading in NetCDF or nemsio
################################################################

source "${USHgfs}/preamble.sh"

cd $DATA

########################################

###################################################
## Run meteogram generator for T574
###################################################
export LEVS=${LEVS:-127}
export STARTHOUR=${STARTHOUR:-00}
export ENDHOUR=${ENDHOUR:-180}
export INCREMENT=12
export MAKEBUFR=NO
export F00FLAG=YES
export fformat=netcdf
export atmfm="nc"
export logfm="txt"
export NINT1=${FHOUT_HF_GFS:-1}
export NEND1=${FHMAX_HF_GFS:-120}
export NINT3=${FHOUT_GFS:-3}

rm -f -r "${COM_ATMOS_BUFR}"
mkdir -p "${COM_ATMOS_BUFR}"
GETDIM="${USHgfs}/getncdimlen"
LEVS=$(${GETDIM} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atmf000.${atmfm}" pfull)
declare -x LEVS

### Loop for the hour and wait for the sigma and surface flux file:
export FSTART=$STARTHOUR
#
while [ $FSTART -lt $ENDHOUR ]
do
export FINT=$NINT1
   # Define the end hour for the input
   export FEND=$(expr $FSTART + $INCREMENT) 
   if test $FEND -lt 100; then FEND=0$FEND; fi 
   if [ $FSTART -eq 00 ]
   then 
       export F00FLAG=YES
   else
       export F00FLAG=NO
   fi
   
   if [ $FEND -eq $ENDHOUR ]
   then
       export MAKEBUFR=YES
   fi

   ic=0
   while [ $ic -lt 1000 ]; do
      if [[ ! -f "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${FEND}.${logfm}" ]]; then
          sleep 10
          ic=$(expr $ic + 1)
      else
          break
      fi

      if [ $ic -ge 360 ]
      then
         err_exit "COULD NOT LOCATE logf$FEND file AFTER 1 HOUR"
      fi
   done

## 1-hourly output before $NEND1, 3-hourly output after
   if [ $FEND -gt $NEND1 ]; then
     export FINT=$NINT3
   fi
   ${USHgfs}/gfs_bufr.sh
  
   export FSTART=$FEND
done

##############################################################
# Tar and gzip the individual bufr files and send them to /com
##############################################################
cd "${COM_ATMOS_BUFR}" || exit 2
tar -cf - . | /usr/bin/gzip > "${RUN}.${cycle}.bufrsnd.tar.gz"
cd "${DATA}" || exit 2

########################################
# Send the single tar file to OSO
########################################
if [[ "${SENDDBN}" == 'YES' ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL GFS_BUFRSND_TAR "${job}" \
        "${COM_ATMOS_BUFR}/${RUN}.${cycle}.bufrsnd.tar.gz"
fi

########################################
# Create Regional Collectives of BUFR data and 
# add appropriate WMO Headers.
########################################
rm -rf poe_col
for (( m = 1; m <10 ; m++ )); do
    echo "sh ${USHgfs}/gfs_sndp.sh ${m} " >> poe_col
done

if [[ ${CFP_MP:-"NO"} == "YES" ]]; then
    nl -n ln -v 0 poe_col > cmdfile
else
    mv poe_col cmdfile
fi

cat cmdfile
chmod +x cmdfile

${APRUN_POSTSNDCFP} cmdfile

sh "${USHgfs}/gfs_bfr2gpk.sh"


############## END OF SCRIPT #######################
