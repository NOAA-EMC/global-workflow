#!/bin/sh
## Below are PBS (Linux queueing system) commands
#BSUB -oo postsnd.out.%J
#BSUB -eo postsnd.out.%J
#BSUB -J postsnd
#BSUB -P GFS-T2O
#BSUB -q dev2
#BSUB -W 0:40
#BSUB -n 5 
#BSUB -R span[ptile=1]
#BSUB -network type=sn_all:mode=US
#BSUB -R affinity[core(24)]
#BSUB -a poe
#BSUB -x

#############################################################
#  Function been tested:            GFS BUFR SOUNDINGS.
#
#  Initial condition:               PDY=2016020900
#                                   ENDHOUR=24
#
#  Usage:                           bsub<run_postsnd.sh
#
#  Result verification:             postsnd.out(pid)
#############################################################

########################################
# Runs GFS BUFR SOUNDINGS
########################################
export PS4='$SECONDS + '
date
set -xa
export MP_EUIDEVELOP=min
export KMP_STACKSIZE=2048m
export MPICH_ALLTOALL_THROTTLE=0
export MP_SINGLE_THREAD=yes
export MP_EAGER_LIMIT=65536
export MP_USE_BULK_XFER=no
export MP_COLLECTIVE_OFFLOAD=no
export MP_SHARED_MEMORY=yes
export MP_MPILIB=mpich2
export MP_LABELIO=yes
export OMP_NUM_THREADS=24
export MP_TASK_AFFINITY=cpu:24
# #### 08/25/1999 ###################
# SET SHELL PROCESSING VARIABLES
# ###################################
#
#Specify whether the run is production or development
#
export envir=${envir:-prod}

####################################
# Specify version numbers
####################################
export gfs_bufrsnd_ver=${gfs_bufrsnd_ver:-v1.0.2}
export gfs_ver=${gfs_ver:-v13.0.0}
export util_ver=${util_ver:-v1.0.0}
# obtain unique process id (pid) and make temp directories
#
export pid=$$
#export DATA_IN=${DATA_IN:-/tmpnwprd1}
#export DATA=$DATA_IN/${job}.${pid}
export DATA_IN=${DATA_IN:-/ptmpp1/$USER}
export DATA=$DATA_IN/postsnd.${pid}
mkdir -p $DATA
cd $DATA
#PDY=20140811
PDY=20160201
export cyc=00
export STARTHOUR=00
export ENDHOUR=24
#export INCREMENT=24
#export JCAP=${JCAP:-574}
#export LEVS=${LEVS:-64}
#export LATB=${LATB:-880}
#export LONB=${LONB:-1760}

####################################
# File To Log Msgs
####################################
job=gfs_postsnd_test
export jlogfile=/com/logs/jlogfiles/jlogfile.${job}.${pid}

####################################
# Determine Job Output Name on System
####################################
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"

export cycle=t${cyc}z

export SENDCOM=NO
export SENDECF=NO
export SENDDBN=NO

export NET=gfs
export RUN=gfs
export model=gfs
export pcom=/pcom/gfs

###################################
# Set up the UTILITIES
###################################
export utilities=${utilities:-/nw${envir}/util.$util_ver/ush}
export utilscript=${utilscript:-/nw${envir}/util.$util_ver/ush}
export utilexec=${utilexec:-/nw${envir}/util.$util_ver/exec}

export HOMEutil=${HOMEutil:-/nw${envir}/util.$util_ver}
export EXECutil=${EXECutil:-$HOMEutil/exec}
export FIXutil=${FIXutil:-$HOMEutil/fix}
export PARMutil=${PARMutil:-$HOMEutil/parm}
export USHutil=${USHutil:-$HOMEutil/ush}

#### export HOMEbufrsnd=/global/save/Lin.Gan/prpost7/NCO/workspace/bufr_snd/gfs_bufrsnd.$gfs_bufrsnd_ver
export HOMEbufrsnd=/global/save/Lin.Gan/prpost7/NCO/gfs/gfs_nco_20160129

export EXECbufrsnd=${EXECbufrsnd:-$HOMEbufrsnd/exec}
export FIXbufrsnd=${FIXbufrsnd:-$HOMEbufrsnd/fix}
export PARMbufrsnd=${PARMbufrsnd:-$HOMEbufrsnd/parm}
export USHbufrsnd=${USHbufrsnd:-$HOMEbufrsnd/ush}
export SCRbufrsnd=${SCRbufrsnd:-$HOMEbufrsnd/scripts}

#### export HOMEgfs=${HOMEgfs:-${NWROOT}/gfs.${gfs_ver}}
HOMEgfs=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/gfs.v13.0.0
export FIXgfs=${FIXgfs:-$HOMEgfs/fix}

#### export HOMEglobal=${HOMEglobal:-$NWROOT/global_shared.${gfs_ver}}
export HOMEglobal=/global/save/Lin.Gan/prpost7/NCO/ncep_post_test/post_trunk_regression_test/svntags/global_shared.v13.0.0
export FIXglobal=${FIXglobal:-$HOMEglobal/fix}
export USHglobal=${USHglobal:-$HOMEglobal/ush}


# Run setup to initialize working directory and utility scripts
$utilscript/setup.sh
# Run setpdy and initialize PDY variables
$utilscript/setpdy.sh
. PDY

##############################
# Define COM Directories
##############################
export COMIN=/com/${NET}/${envir}/${RUN}.${PDY}
#export COMOUT=/com/${NET}/${envir}/${RUN}.${PDY}
#export COMIN=/global/noscrub/$USER/com/${NET}/para/${RUN}.${PDY}
export COMOUT=$DATA
mkdir -p $COMOUT
env

########################################################
# Execute the script.
$SCRbufrsnd/exgfs_postsnd.sh.ecf
########################################################

#cat $pgmout

#cd /tmpnwprd1
#rm -rf $DATA
date
