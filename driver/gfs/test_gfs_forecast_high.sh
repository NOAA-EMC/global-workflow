#!/bin/sh --login

#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -o gfs_forecast_high.o%J
#BSUB -e gfs_forecast_high.o%J
#BSUB -J gfs_forecast_high
#BSUB -q devonprod
#BSUB -M 768
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 02:00
#BSUB -cwd /gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work/gfs.v14.1.0/driver

set -x

# 65 nodes = 59 compute nodes + 6 i/o nodes
#   set WRT_GROUP=6 for 6 i/o nodes (see ../parm/gfs_forecast_high.parm)
#   set WRTPE_PER_GROUP=4 to match ptile
export NODES=65
export ntasks=260
export ptile=4
export threads=6

export CDATE=2017012506

#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gfs_forecast_high_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/gpfs/hps/stmp/$LOGNAME/test


#############################################################
# Specify versions
#############################################################
export global_shared_ver=v14.1.0
export gfs_ver=v14.1.0
export grib_util_ver=1.0.3
export prod_util_ver=1.0.5


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/sh
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver
module load craype-hugepages16M

module list


#############################################################
# WCOSS_C environment settings
#############################################################
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export OMP_NUM_THREADS=$threads
export NTHREADS=$threads


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work
export PARA_CONFIG=$NWTEST/gfs.${gfs_ver}/driver/para_config.gfs_forecast_high
export JOBGLOBAL=$NWTEST/gfs.${gfs_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGFS_FORECAST_HIGH

exit

