#!/bin/ksh
#Note: /nwprod version of this file is called JGFS_POSTSND
########################################
# Runs GFS BUFR SOUNDINGS
########################################

export PS4='$SECONDS + '
date
set -xa
# #### 08/25/1999 ###################
# SET SHELL PROCESSING VARIABLES
# ###################################
# 
# obtain unique process id (pid) and make temp directories
#
export pid=$$
export job=gfs_postsnd
#export DATA=/tmpnwprd/${job}.${pid}
#export DATA=${COMOUT:-/$STMP/$LOGNAME/bufrsnd.${pid}}
export DATAROOT=$STMP/$LOGNAME
export DATA=$DATAROOT/${job}.${pid}
mkdir -p $DATA
cd $DATA 

####################################
# File To Log Msgs
####################################
#export jlogfile=/com/logs/jlogfiles/jlogfile.${job}.${pid}

####################################
# Determine Job Output Name on System
####################################
#export outid="LL$job"
#export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"
#Set time
export PDY=${CDATE:-2012100100}
export PDY=`echo $PDY |cut -c 1-8`
export cyc=`echo $CDATE |cut -c 9-10`
export cycle=t${cyc}z 

#export SENDCOM=YES
#export SENDECF=YES
#export SENDDBN=YES
export SENDCOM=${SENDCOM:-NO}
export SENDECF=${SENDECF:-NO}
export SENDDBN=${SENDDBN:-NO}

export NET=gfs
export RUN=gfs
export model=gfs
export pcom=${pcom:-${PCOMROOT}/gfs}

###################################
# Set up the UTILITIES
###################################
. $MODULESHOME/init/sh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
module load prod_envir
module load prod_util
module load grib_util/1.0.3
module load util_shared/1.0.3
module load gempak/7.3.0
module load iobuf
##export IOBUF_PARAMS="sigf*:size=128M:count=20:prefetch=0:verbose"
export IOBUF_PARAMS='sigf*:size=128M:count=20:prefetch=0:verbose,gfs_collectiv*:size=128M:count=2:prefetch=0:verbose,*.snd:size=128M:count=30:prefetch=0:verbose'
#export HOMEbufr=/nw${envir}
export HOMEbufr=${HOMEbufr:-$BASEDIR}
export EXECbufr=$HOMEbufr/exec
export FIXbufr=$HOMEbufr/fix
export PARMbufr=${PARMbufr:-$HOMEbufr/parm}
export USHbufr=$HOMEbufr/ush

export HOMEbufrsnd=$HOMEbufr
export EXECbufrsnd=$EXECbufr
export FIXbufrsnd=$FIXbufr
export PARMbufrsnd=$PARMbufr
export USHbufrsnd=$USHbufr

export HOMEbufrsnd=${HOMEbufrsnd:-$NWROOT/gfs.${gfs_ver}}
export EXECbufrsnd=${EXECbufrsnd:-$HOMEbufrsnd/exec}
export FIXbufrsnd=${FIXbufrsnd:-$HOMEbufrsnd/fix}
export PARMbufrsnd=${PARMbufrsnd:-$HOMEbufrsnd/parm}
export USHbufrsnd=${USHbufrsnd:-$HOMEbufrsnd/ush}
export SCRbufrsnd=${SCRbufrsnd:-$HOMEbufrsnd/scripts}

export HOMEgsm=${HOMEgsm:-$NWROOT/global_shared.${global_shared_ver}}
export FIXgsm=$HOMEgsm/fix/fix_am


# Run setup to initialize working directory and utility scripts
#$utilscript/setup.sh
# Run setpdy and initialize PDY variables
#$utilscript/setpdy.sh
#. PDY

##############################
# Define COM Directories
##############################
#export COMIN=/com/${NET}/${envir}/${RUN}.${PDY}
#export COMOUT=/com/${NET}/${envir}/${RUN}.${PDY}
export COMIN=${COMIN:-$COMROOT/${NET}/${envir}/${RUN}.${PDY}}
export COMOUT=${COMOUT:-$DATA}
export COMAWP=${COMAWP:-${COMROOT}/nawips/${envir}/${RUN}.${PDY}}

#env

########################################################
# Execute the script.
#/nw${envir}/scripts/exgfs_postsnd.sh.ecf
export OMP_NUM_THREADS=$snd_nthreads
export APRUN="aprun -n $snd_nprocs -N $snd_ptile -j 1 -d $snd_nthreads -cc depth"
$HOMEbufr/scripts/exgfs_postsnd.sh.ecf
########################################################

cat $pgmout
cat out_gfs_bufr*

#cd /tmpnwprd
#rm -rf $DATA
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
date
