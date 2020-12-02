#!/bin/ksh
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
set -xa

cd $DATA
########################################
msg="HAS BEGUN"
#postmsg "$jlogfile" "$msg"
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
export fformat=${OUTPUT_FILE:-netcdf}
if [ $fformat == "netcdf" ]
 then
export atmfm="nc"
export logfm="txt"
else 
export atmfm="nemsio"
export logfm="nemsio"
fi

    export NINT1=${FHOUT_HF_GFS:-1}
    export NEND1=${FHMAX_HF_GFS:-120}
    export NINT3=${FHOUT_GFS:-3}

rm -f -r ${COMOUT}/bufr.${cycle}
mkdir -p ${COMOUT}/bufr.${cycle}

    if [ -f $HOMEgfs/ush/getncdimlen ]
	then
	GETDIM=$HOMEgfs/ush/getncdimlen
	else
	GETDIM=$EXECbufrsnd/getncdimlen
	fi
if [ $fformat == "netcdf" ]
 then
export LEVS=`$GETDIM $COMIN/${RUN}.${cycle}.atmf000.${atmfm} pfull`
else
# Extract number of vertical levels from $STARTHOUR atmospheric file
export NEMSIOGET=${NEMSIOGET:-$EXECbufrsnd/nemsio_get}
fhr3=$(printf %03i $STARTHOUR)
ATMFCS=$COMIN/${RUN}.${cycle}.atmf${fhr3}.nemsio
export LEVS=$($NEMSIOGET $ATMFCS dimz | awk '{print $2}')
fi

### Loop for the hour and wait for the sigma and surface flux file:
export FSTART=$STARTHOUR
#
while [ $FSTART -lt $ENDHOUR ]
do
export FINT=$NINT1
   # Define the end hour for the input
   export FEND=`expr $FSTART + $INCREMENT` 
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
   while [ $ic -lt 1000 ]
   do
      if [ ! -f $COMIN/${RUN}.${cycle}.logf$FEND.${logfm} ]
      then
          sleep 10
          ic=`expr $ic + 1`
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
##   $USHbufrsnd/gfs_bufr.sh
   $USHbufrsnd/gfs_bufr.sh
  
   export FSTART=$FEND
done

##############################################################
# Tar and gzip the individual bufr files and send them to /com
##############################################################
cd ${COMOUT}/bufr.${cycle}
tar -cf - . | /usr/bin/gzip > ../${RUN}.${cycle}.bufrsnd.tar.gz
cd $DATA

########################################
# Send the single tar file to OSO
########################################
if test "$SENDDBN" = 'YES'
then
    $DBNROOT/bin/dbn_alert MODEL GFS_BUFRSND_TAR $job \
  $COMOUT/${RUN}.${cycle}.bufrsnd.tar.gz
fi

########################################
# Create Regional Collectives of BUFR data and 
# add appropriate WMO Headers.
########################################
collect=' 1 2 3 4 5 6 7 8 9'
if [ $machine == "HERA" -o  $machine == "JET" ]; then
for m in ${collect}
do
sh $USHbufrsnd/gfs_sndp.sh $m
done

################################################
# Convert the bufr soundings into GEMPAK files
################################################
sh $USHbufrsnd/gfs_bfr2gpk.sh

else
rm -rf poe_col
for m in ${collect}
do
echo "sh $USHbufrsnd/gfs_sndp.sh $m " >> poe_col
done

mv poe_col cmdfile

cat cmdfile
chmod +x cmdfile

${APRUN_POSTSNDCFP} cmdfile

sh $USHbufrsnd/gfs_bfr2gpk.sh
fi
################################################
# Convert the bufr soundings into GEMPAK files
################################################
##$USHbufrsnd/gfs_bfr2gpk.sh

#####################################################################
# GOOD RUN
set +x
echo "**************JOB GFS_meteogrm COMPLETED NORMALLY ON THE IBM"
echo "**************JOB GFS_meteogrm COMPLETED NORMALLY ON THE IBM"
echo "**************JOB GFS_meteogrm COMPLETED NORMALLY ON THE IBM"
set -x
#####################################################################

msg='HAS COMPLETED NORMALLY.'
#postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################
