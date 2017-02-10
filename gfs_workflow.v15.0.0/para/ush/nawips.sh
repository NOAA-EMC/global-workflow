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
module load prod_util/1.0.4
module unload grib_util/1.0.3
module use /usrx/local/nceplibs/grib_util.v1.0.4/modulefiles
module load grib_util/1.0.4
module use /usrx/local/nceplibs/util_shared.v1.0.4/modulefiles
module load util_shared/1.0.4

export RUN_ENVIR=para
export envir=para
export job=gfs_gempak
export pid=$$
export DATAROOT=$STMP/$LOGNAME
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`

export SENDCOM=YES
export SENDDBN=NO
export SENDECF=NO

export gfs_ver=${gfs_ver:-v14.1.0}
export HOMEgempak=${HOMEgempak:-/nw${envir}/gfs.${gfs_ver}/gempak}
export FIXgempak=${FIXgempak:-$HOMEgempak/fix}
export USHgempak=${USHgempak:-$HOMEgempak/ush/gfs}

export HOMEgfs=${HOMEgfs:-/nw${envir}/gfs.${gfs_ver}}
export FIXgfs=${FIXgfs:-/nw${envir}/gfs.${gfs_ver}/fix}
export USHgfs=${USHgfs:-/nw${envir}/gfs.${gfs_ver}/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}
export GFSNAWIPSSH=${GFSNAWIPSSH:-$SRCgfs/exgfs_nawips.sh.ecf}

export COM_IN=$PRODNAMES_DIR/com2/gfs/${RUN_ENVIR}
export COM_OUT=$PRODNAMES_DIR/com2/nawips/${RUN_ENVIR}
export jlogfile=$COM_OUT/jlogfile.${job}.${pid}

# #### 08/25/1999 ###################
# SET SHELL PROCESSING VARIABLES
# ###################################
export PS4='$SECONDS + ' 
date
# 
# obtain unique process id (pid) and make temp directories
#

export RUN_ENVIR=${RUN_ENVIR:-prod}

export NET=${NET:-gfs}
export RUN=${RUN:-gfs}

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

#
# Set up model and cycle specific variables
#
export ILPOST=${ILPOST:-1}
export finc=${finc:-3}
export fstart=${fstart:-0}
export model=${model:-gfs}
export GRIB=${GRIB:-pgrb2f}
export EXT=""
export DBN_ALERT_TYPE=${DBN_ALERT_TYPE:-GFS_GEMPAK}

export HOMEgfs=${HOMEgfs:-/nw${envir}/gfs.${gfs_ver}}
export FIXgfs=${FIXgfs:-/nw${envir}/gfs.${gfs_ver}/fix}
export USHgfs=${USHgfs:-/nw${envir}/gfs.${gfs_ver}/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}
export GFSNAWIPSSH=${GFSNAWIPSSH:-$SRCgfs/exgfs_nawips.sh.ecf}

export HOMEgempak=${HOMEgempak:-$HOMEgfs/gempak}
export FIXgempak=${FIXgempak:-$HOMEgempak/fix}
export USHgempak=${USHgempak:-$HOMEgempak/ush}

#
# Now set up GEMPAK/NTRANS environment
#
export IOBUF_PARAMS="*:size=32M:count=4:verbose"
module load gempak/7.3.0
export COM_IN=${COM_IN:-/com/${NET}/${envir}}
export COM_OUT=${COM_OUT:-/com/nawips/${envir}}
export COMIN=${COM_IN}/${RUN}.${PDY}
export COMOUT=${COM_OUT}/${RUN}.${PDY}
export jlogfile=${COMOUT}/jlogfile.${job}.${pid}
if [ ! -f $COMOUT ] ; then
  mkdir -p -m 775 $COMOUT
fi

env

#################################################################
# Execute the script for the 384 hour 1 degree grib

echo "$GFSNAWIPSSH gfs 384 GFS_GEMPAK &> $DATA/gfs.$$ " >>poescript
echo "$GFSNAWIPSSH gfs 384 GFS_GEMPAK &> $DATA/gfs.$$ " >>poescript

##################################################################

#################################################################
# Execute the script for the half-degree grib

echo "$GFSNAWIPSSH gfs_0p50 384 GFS_GEMPAK &> $DATA/gfs_0p5.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p50 384 GFS_GEMPAK &> $DATA/gfs_0p5.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p50 384 GFS_GEMPAK &> $DATA/gfs_0p5.$$ " >>poescript

#################################################################
# Execute the script for the quater-degree grib

echo "$GFSNAWIPSSH gfs_0p25 384 GFS_GEMPAK &> $DATA/gfs_0p25.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p25 384 GFS_GEMPAK &> $DATA/gfs_0p25.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p25 384 GFS_GEMPAK &> $DATA/gfs_0p25.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p25 384 GFS_GEMPAK &> $DATA/gfs_0p25.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p25 384 GFS_GEMPAK &> $DATA/gfs_0p25.$$ " >>poescript
echo "$GFSNAWIPSSH gfs_0p25 384 GFS_GEMPAK &> $DATA/gfs_0p25.$$ " >>poescript

####################################################################
# Execute the script to create the 35km Pacific grids for OPC

echo "$GFSNAWIPSSH gfs35_pac 180 GFS_GEMPAK_WWB &> $DATA/gfs35_pac.$$ " >>poescript
echo "$GFSNAWIPSSH gfs35_pac 180 GFS_GEMPAK_WWB &> $DATA/gfs35_pac.$$ " >>poescript

#####################################################################

####################################################################
# Execute the script to create the 35km Atlantic grids for OPC

echo "$GFSNAWIPSSH gfs35_atl 180 GFS_GEMPAK_WWB &> $DATA/gfs35_atl.$$ " >>poescript
echo "$GFSNAWIPSSH gfs35_atl 180 GFS_GEMPAK_WWB &> $DATA/gfs35_atl.$$ " >>poescript

#####################################################################

#####################################################################
# Execute the script to create the 40km grids for HPC

echo "$GFSNAWIPSSH gfs40 180 GFS_GEMPAK_WWB &> $DATA/gfs40.$$ " >>poescript
echo "$GFSNAWIPSSH gfs40 180 GFS_GEMPAK_WWB &> $DATA/gfs40.$$ " >>poescript

######################################################################

cat poescript

chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript
export MP_LABELIO=YES
export MP_INFOLEVEL=3
export MP_STDOUTMODE=ordered
export NODES=2

# Execute the script.
launcher=${launcher:-$APRUN_GEMPAK}
$launcher $MP_CMDFILE

#### cat $pgmout

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

