#!/bin/ksh

#BSUB -o gdas_analysis_high.o%J
#BSUB -e gdas_analysis_high.o%J
#BSUB -J gdas_analysis_high
#BSUB -n 480
#BSUB -x
#BSUB -R span[ptile=2]
#BSUB -R affinity[core(12)]
#BSUB -W 01:00
#BSUB -q devmax2
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export CDATE=2016030212


#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gdas_analysis_high_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/stmpd3/$LOGNAME/test
export COMROOT=/ptmpd3/$LOGNAME/com


#############################################################
# Specify versions
#############################################################
export gdas_ver=v13.0.0
export global_shared_ver=v13.0.0
export crtm_ver=v2.2.3
export grib_util_ver=v1.0.1
export prod_util_ver=v1.0.2
export util_shared_ver=v1.0.2


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/lib/modulefiles
module load crtm/${crtm_ver}

module use /nwprod2/modulefiles
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver
module load util_shared/$util_shared_ver

module unload ics/12.1
module load ics/15.0.3

module list


#############################################################
# WCOSS environment settings
#############################################################
export MP_EAGER_LIMIT=65536
export MP_LABELIO=yes
export MP_USE_BULK_XFER=yes
export MP_SINGLE_THREAD=yes
export MP_MPILIB=mpich2
export MP_SHARED_MEMORY=yes
export MP_USE_TOKEN_FLOW_CONTROL=yes
export FORT_BUFFERED=true
export KMP_STACKSIZE=2048m
export MPICH_ALLTOALL_THROTTLE=0

export OMP_NUM_THREADS_CY=24
export NTHREADS=$OMP_NUM_THREADS_CY
export NTHREADS_GSI=12
export NTHSTACK=1024000000


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/global/save/$LOGNAME/svn/gfs/branches
export PARA_CONFIG=$NWTEST/gdas.${gdas_ver}/driver/para_config.gdas_analysis_high
export JOBGLOBAL=$NWTEST/gdas.${gdas_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ANALYSIS_HIGH

exit
