#!/bin/sh --login

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -o gdas_enkf_update.o%J
#BSUB -e gdas_enkf_update.o%J
#BSUB -J gdas_enkf_update
#BSUB -q devonprod
#BSUB -M 3072
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 01:00
#BSUB -cwd /gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work/gdas.v14.1.0/driver

set -x

export NODES=40
export ntasks=240
export ptile=6
export threads=4

export CDATE=2017030806


#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gdas_enkf_update_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/gpfs/hps/stmp/$LOGNAME/test


#############################################################
# Specify versions
#############################################################
export gdas_ver=v14.1.0
export global_shared_ver=v14.1.0
export grib_util_ver=1.0.3
export prod_util_ver=1.0.6
export util_shared_ver=1.0.3


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/sh
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

export NTHREADS_ENKF=$nthreads


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work
export PARA_CONFIG=$NWTEST/gdas.${gdas_ver}/driver/para_config.gdas_enkf_update
export JOBGLOBAL=$NWTEST/gdas.${gdas_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ENKF_UPDATE

exit
