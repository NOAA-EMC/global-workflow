#!/bin/ksh

#BSUB -o gdas_enkf_fcst.o%J
#BSUB -e gdas_enkf_fcst.o%J
#BSUB -J gdas_enkf_fcst
#BSUB -n 96
#BSUB -x
#BSUB -R span[ptile=6]
#BSUB -R affinity[core(4)]
#BSUB -W 02:00
#BSUB -q devmax2
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export CDATE=2016030212
export ENSGRP=1

export grp=$ENSGRP
if [[ $grp -lt 10 ]]; then
   export grp=0$grp
fi


#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gdas_enkf_innovate_obs_grp${grp}_${cyc}
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
export grib_util_ver=v1.0.1
export prod_util_ver=v1.0.2


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load grib_util/$grib_util_ver
module load prod_util/$prod_util_ver

module unload ics/12.1
module load ics/14.0.1

module list


#############################################################
# WCOSS environment settings
#############################################################
export MP_EAGER_LIMIT=65536
export MP_COREFILE_FORMAT=lite
export MP_EUIDEVELOP=min
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_MPILIB=mpich2
export MP_LABELIO=yes
export MP_USE_BULK_XFER=no
export MP_SHARED_MEMORY=yes
export KMP_STACKSIZE=2048m
export NTHREADS_EFCS=${NTHREADS_EFCS:-4}


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/global/save/$LOGNAME/svn/gfs/branches
export PARA_CONFIG=$NWTEST/gdas.${gdas_ver}/driver/para_config.gdas_enkf_fcst
export JOBGLOBAL=$NWTEST/gdas.${gdas_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ENKF_FCST

exit
