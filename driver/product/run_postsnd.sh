#!/bin/sh

#BSUB -oo postsnd.out.%J
#BSUB -eo postsnd.out.%J 
#BSUB -J postsnd_1hr 
#BSUB -W 02:30
#BSUB -q dev
#BSUB -P GFS-T2O
#BSUB -cwd /gpfs/hps/emc/meso/noscrub/Guang.Ping.Lou/bufr_fnl
#BSUB -M 500
#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + 12*{select[craylinux && vnode]span[ptile=6] cu[type=cabinet]}'

export IOBUF_PARAMS='sigf*:size=128M:count=20:prefetch=0:verbose,gfs_collectiv*:size=128M:count=2:prefetch=0:verbose,*.snd:size=128M:count=20:prefetch=0:verbose'
############################################
# Loading module
############################################
. $MODULESHOME/init/ksh
module load PrgEnv-intel ESMF-intel-haswell/3_1_0rp5 cfp-intel-sandybridge iobuf craype-hugepages2M craype-haswell
#module load cfp-intel-sandybridge/1.1.0
module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_envir
module load prod_util
module load prod_util/1.0.4
module load grib_util/1.0.3


########################################
# Runs GFS BUFR SOUNDINGS
########################################
set -xa

export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

#export PS4='$SECONDS + '
date
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
export gsm_ver=${gsm_ver:-v12.0.0}
export util_ver=${util_ver:-v1.0.0}

# obtain unique process id (pid) and make temp directories
#
export pid=$$
#export DATA_IN=${DATA_IN:-/tmpnwprd1}
#export DATA=$DATA_IN/${job}.${pid}
export DATA_IN=${DATA_IN:-/gpfs/hps/ptmp/$USER}
export DATA=$DATA_IN/postsnd.${pid}
mkdir -p $DATA
cd $DATA
#PDY=20140811
PDY=20170220
#PDY=20170208
#njmexport cyc=00
export cyc=00
export STARTHOUR=00
export ENDHOUR=240
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
export pcom=$DATA_IN/pcom/gfs
mkdir -p $pcom

###################################
# Set up the UTILITIES
###################################

export HOMEbufrsnd=/gpfs/hps/emc/meso/noscrub/Guang.Ping.Lou/bufr_fnl

#export STNLIST=$PARMbufrsnd/bufr_stalist.meteo.gfs3

# Run setup to initialize working directory and utility scripts
#$utilscript/setup.sh
# Run setpdy and initialize PDY variables
#$utilscript/setpdy.sh
#. PDY

##############################
# Define COM Directories
##############################
#export COMIN=/com2/${NET}/${envir}/${RUN}.${PDY}
export COMIN=/gpfs/hps/ptmp/emc.glopara/com2/${NET}/para/${RUN}.${PDY}
#export COMIN=/gpfs/hps/emc/meso/noscrub/Guang.Ping.Lou/para_look_alike/${RUN}.${PDY}
#export COMIN=/gpfs/hps/emc/global/noscrub/Hui-Ya.Chuang/para_look_alike/${RUN}.${PDY}
####export FIXgsm=${HOMEgsm:-$HOMEgsm/fix/fix_am}

#mkdir -p $COMIN
#NEMSIO Sample input 

#export COMOUT=/com/${NET}/${envir}/${RUN}.${PDY}
#export COMIN=/meso/save/$USER/com/${NET}/para/${RUN}.${PDY}
#export COMOUT=$DATA
export COMOUT=$DATA_IN/com2/${NET}/${envir}/${RUN}.${PDY}
mkdir -p $COMOUT
export COMAWP=$DATA_IN/com2/nawips/${envir}/${RUN}.${PDY}
mkdir -p $COMAWP
env

########################################################
# Execute the script.
#$SCRbufrsnd/exgfs_postsnd.sh.ecf
${HOMEbufrsnd}/jobs/JGFS_POSTSND
########################################################

#cat $pgmout

#cd /tmpnwprd1
#rm -rf $DATA
date

