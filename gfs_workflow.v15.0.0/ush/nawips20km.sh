#!/bin/ksh

########################################
# Runs GFS Postprocessing up to 24 hours
########################################

set -ux

set -a; . $CONFIG; set +a

. $MODULESHOME/init/sh
module load PrgEnv-intel/5.2.56
module load cfp-intel-sandybridge/1.1.0
module load ESMF-intel-sandybridge/3_1_0rp5
module load iobuf
module load craype-hugepages2M
module load craype-haswell
module load prod_envir
module load prod_util
module load grib_util/1.0.3
module use /usrx/local/nceplibs/util_shared.v1.0.4/modulefiles
module load util_shared/1.0.4

export RUN_ENVIR=para
export envir=para
export job=gfs_awips20km
export pid=$$
export DATAROOT=$STMP/$LOGNAME
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`

export SENDCOM=YES
export SENDDBN=NO
export SENDECF=NO

export RUN_ENVIR=${RUN_ENVIR:-prod}
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}

export gfs_ver=${gfs_ver:-v14.1.0}

export HOMEgfs=${HOMEgfs:-/nw${envir}/gfs.${gfs_ver}}
export FIXgfs=${FIXgfs:-/nw${envir}/gfs.${gfs_ver}/fix}
export USHgfs=${USHgfs:-/nw${envir}/gfs.${gfs_ver}/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export GFSAWIPS20KMSH=${GFSAWIPS20KMSH:-$SRCgfs/exgfs_grib_awips_20km.sh.ecf}
export COM_IN=$PRODNAMES_DIR/com2/gfs/${RUN_ENVIR}
export COM_OUT=$PRODNAMES_DIR/pcom2/${RUN_ENVIR}
export COMIN=${COM_IN}/${RUN}.${PDY}
export COMOUT=${COM_OUT}/${RUN}.${PDY}
#### export PCOM=$COM_OUT
export PCOM=$COMOUT
# export jlogfile=$COM_OUT/jlogfile.${job}.${pid}
export jlogfile=${COMOUT}/jlogfile.${job}.${pid}

if [ ! -f $PCOM ] ; then
  mkdir -p -m 775 $PCOM
fi


# #### 08/25/1999 ###################
# SET SHELL PROCESSING VARIABLES
# ###################################
export PS4='$SECONDS + ' 
date
# 
# obtain unique process id (pid) and make temp directories
#

###############################################################
# This block can be modified for different Production test
# environment. This is used for operational testings
###############################################################
if [ $RUN_ENVIR = "prod" -a $envir != "prod" ]
then
   export DBNROOT=/nwprod/spa_util/fakedbn
   export jlogfile=${jlogfile:-/com/logs/${envir}/jlogfile}
fi

export pid=$$
export DATA=$DATAROOT/${job}.${pid}
mkdir $DATA
cd $DATA 

####################################
# File To Log Msgs
####################################
export jlogfile=${jlogfile:-/com/logs/jlogfiles/jlogfile.${job}.${pid}}
####################################
# Determine Job Output Name on System
####################################
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"

export cycle=t${cyc}z 

export SENDCOM=${SENDCOM:-YES}
export SENDDBN=${SENDDBN:-YES}
export SENDECF=${SENDECF:-YES}

# For half-degree P Grib files 
export DO_HD_PGRB=${DO_HD_PGRB:-YES}

export DBN_ALERT_TYPE=${DBN_ALERT_TYPE:-GFS_GEMPAK}

env

echo "$GFSAWIPS20KMSH 000 027 &> $DATA/awips_20km_1.$$ " >>poescript
echo "$GFSAWIPS20KMSH 030 057 &> $DATA/awips_20km_2.$$ " >>poescript
echo "$GFSAWIPS20KMSH 060 084 &> $DATA/awips_20km_3.$$ " >>poescript
echo "$GFSAWIPS20KMSH 090 144 &> $DATA/awips_20km_4.$$ " >>poescript
echo "$GFSAWIPS20KMSH 150 204 &> $DATA/awips_20km_5.$$ " >>poescript
echo "$GFSAWIPS20KMSH 210 240 &> $DATA/awips_20km_6.$$ " >>poescript
cat poescript

chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript
export MP_LABELIO=YES
export MP_INFOLEVEL=3
export MP_STDOUTMODE=ordered
export NODES=2
rc_awips20km=0
# Execute the script.
launcher=${launcher:-$APRUN_GEMPAK}
$launcher $MP_CMDFILE

cat $pgmout

rc_awips20km=$?
if [ rc_awips20km = 0 -a ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi

#### Clean up after jobs finished
#### if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi


# If requested, submit gempak meta job
##if [ $CDUMP = gfs -a $GENGEMPAK_META = YES ];then
##  export mem=16384
##  export jn=${PSLOT}${CDATE}${CDUMP}gempak_meta
##  export out=$COMROT/${jn}.dayfile
##  $SUB -e 'CDATE=$CDATE CDUMP=$CDUMP CSTEP=$CSTEP CONFIG=$CONFIG' -q $CUE2RUN -a $ACCOUNT -g $GROUP -p 25/25/N -r $mem/1/8 -t 06:00 -j $jn -o $out $NAWIPSMETASH
##fi

##date
##cd $DATA
##cd ../
##rm -rf $DATA
##date

