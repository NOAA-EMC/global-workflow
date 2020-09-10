#!/bin/sh

#BSUB -oo postsnd.out.%J
#BSUB -eo postsnd.out.%J 
#BSUB -J postsnd
#BSUB -W 01:30
#BSUB -q dev
#BSUB -P GFS-DEV
#BSUB -cwd /gpfs/dell2/emc/verification/noscrub/Guang.Ping.Lou/gfsv16bnetcdf_mpmd/driver/product
#BSUB -R span[ptile=4]
#BSUB -n 40
#BSUB -R affinity[core(1):distribute=balance]

############################################
# Loading module
############################################
. $MODULESHOME/init/ksh
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load impi/18.0.1
module load lsf/10.1
module load prod_util/1.1.0
module load grib_util/1.0.6
module load prod_envir/1.1.0
module load CFP/2.0.1
##module load NetCDF/4.5.0
##module load HDF5-parallel/1.10.1
module load python/3.6.3

module use -a /gpfs/dell1/nco/ops/nwpara/modulefiles/
module load gempak/7.3.1
module use /usrx/local/nceplibs/dev/NCEPLIBS/modulefiles
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4
module load w3emc_para/2.4.0
module list

#module use /usrx/local/dev/modulefiles

########################################
# Runs GFS BUFR SOUNDINGS
########################################
set -xa

export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

#export machine="WCOSS_C"
machine="WCOSS_DELL_P3"
#machine="THEIA"
#machine="JET"
export npe_postsnd=40
export npe_postsndcfp=9
##export OUTPUT_FILE="nemsio"
export OUTPUT_FILE=${OUTPUT_FILE:-netcdf}
if [ $machine == "WCOSS_C" ]; then
##For WCOSS-Cray##################
export FHMAX_HF_GFS=120
export FHOUT_HF_GFS=6
export launcher="aprun"
export npe_node_postsnd=3
export NTHREADS_POSTSND=1
export npe_node_postsndcfp=3
export NTHREADS_POSTSNDCFP=1
export APRUN_POSTSND="$launcher -j 1 -n $npe_postsnd -N $npe_node_postsnd -d $NTHREADS_POSTSND -cc depth"
export APRUN_POSTSNDCFP="$launcher -j 1 -n $npe_postsndcfp -N $npe_node_postsndcfp -d $NTHREADS_POSTSNDCFP cfp"
elif [ $machine == "WCOSS_DELL_P3" ]; then
##For WCOSS-Dell ################
  if [ $OUTPUT_FILE == "netcdf" ]; then
   export FHMAX_HF_GFS=120
   export FHOUT_HF_GFS=3
   export FHOUT_GFS=3
  else
   export FHMAX_HF_GFS=120
   export FHOUT_HF_GFS=1
   export FHOUT_GFS=3
  fi
export launcher="mpirun -n"
export APRUN_POSTSND="$launcher $npe_postsnd"
export APRUN_POSTSNDCFP="$launcher $npe_postsndcfp cfp"
else
##For Theia, Jet################
export FHMAX_HF_GFS=0
export FHOUT_HF_GFS=0
export launcher="mpirun -np"
export APRUN_POSTSND="$launcher $npe_postsnd"
export APRUN_POSTSNDCFP="$launcher $npe_postsndcfp"
fi

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
export DATA_IN=${DATA_IN:-/gpfs/dell2/ptmp/$USER}
export DATA=$DATA_IN/postsnd.${pid}
mkdir -p $DATA
cd $DATA
export PDY=20200315
export cyc=00
export STARTHOUR=00
export ENDHOUR=180
export KEEPDATA="YES"

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

export SENDCOM=YES
export SENDECF=YES
export SENDDBN=YES

export NET=gfs
export RUN=gfs
export model=gfs
export pcom=$DATA_IN/pcom/gfs
mkdir -p $pcom

###################################
# Set up the UTILITIES
###################################

export HOMEgfs=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/global-workflow/feature_gfsv16b
export HOMEbufrsnd=/gpfs/dell2/emc/verification/noscrub/Guang.Ping.Lou/gfsv16bnetcdf_mpmd
##export HOMEbufrsnd=/gpfs/dell2/emc/verification/noscrub/Guang.Ping.Lou/fv3gfs
##export HOMEbufrsnd=/gpfs/hps3/emc/meso/noscrub/Guang.Ping.Lou/fv3gfs

##############################
# Define COM Directories
##############################
  if [ $OUTPUT_FILE == "netcdf" ]; then
export COMIN=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/v16rt2/${RUN}.${PDY}/$cyc
##export COMIN=/gpfs/dell2/emc/verification/noscrub/Guang.Ping.Lou/gfs_v16_data/2020021500
  else
export COMIN=/gpfs/dell1/nco/ops/com/gfs/prod/${RUN}.${PDY}/$cyc
  fi
##export EXPDIR=/gpfs/dell2/emc/verification/noscrub/Guang.Ping.Lou
##export COMIN=${EXPDIR}/data_netcdf/${RUN}.${PDY}
#export COMIN=${EXPDIR}/fv3gfs_data/${RUN}.${PDY}/$cyc
#export COMIN=/gpfs/dell2/ptmp/Russ.Treadon/ROTDIRS/prfv3rt1/${RUN}.${PDY}/$cyc
#export COMIN=/gpfs/dell2/emc/verification/noscrub/Guang.Ping.Lou/fv3gfs_data/${RUN}.${PDY}/$cyc
#export COMIN=/gpfs/hps3/ptmp/emc.glopara/fv3fy18retro2/${RUN}.${PDY}/$cyc
#export COMIN=/gpfs/dell2/ptmp/Fanglin.Yang/fv3test2/${RUN}.${PDY}/$cyc

export COMOUT=$DATA_IN/com2/${NET}/${envir}/${RUN}.${PDY}
mkdir -p $COMOUT
export COMAWP=$DATA_IN/com2/nawips/${envir}/${RUN}.${PDY}
mkdir -p $COMAWP
env

########################################################
# Execute the script.
#$SCRbufrsnd/exgfs_postsnd.sh
${HOMEbufrsnd}/jobs/JGFS_POSTSND
########################################################

#cat $pgmout

#cd /tmpnwprd1
#rm -rf $DATA
date

