#!/bin/sh --login

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -o gfs_analysis.o%J
#BSUB -e gfs_analysis.o%J
#BSUB -J gfs_analysis
#BSUB -q devonprod
#BSUB -M 3072
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 01:00
#BSUB -cwd /gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final/gfs.v14.1.0/driver

set -x

export NODES=240
export ntasks=480
export ptile=2
export threads=12

export CDATE=2017040700


#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gfs_analysis_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/gpfs/hps/stmp/$LOGNAME/test


#############################################################
# Specify versions
#############################################################
export gfs_ver=v14.1.0
export global_shared_ver=v14.1.0
export crtm_ver=2.2.4
export prod_envir_ver=1.0.1
export grib_util_ver=1.0.3
export prod_util_ver=1.0.8
export util_shared_ver=1.0.3


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/sh
module load crtm-intel/${crtm_ver}
module load prod_envir/$prod_envir_ver
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver
module load util_shared/$util_shared_ver
module load cfp-intel-sandybridge

module list


#############################################################
# WCOSS_C environment settings
#############################################################
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=2G
export OMP_NUM_THREADS=$threads
export FORT_BUFFERED=true

export OMP_NUM_THREADS_CY=24
export NTHREADS=$OMP_NUM_THREADS_CY
export NTHREADS_GSI=$threads
export NTHSTACK=1024000000


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17_final
export PARA_CONFIG=$NWTEST/gfs.${gfs_ver}/driver/para_config.gfs_analysis
export JOBGLOBAL=$NWTEST/gfs.${gfs_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGFS_ANALYSIS

exit
