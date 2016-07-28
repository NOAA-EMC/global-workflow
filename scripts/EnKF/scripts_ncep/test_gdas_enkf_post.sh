#!/bin/ksh

#BSUB -o gdas_enkf_post.o%J
#BSUB -e gdas_enkf_post.o%J
#BSUB -J gdas_enkf_post
#BSUB -q devmax2
#BSUB -n 81
#BSUB -R span[ptile=24]
#BSUB -R affinity[core]
#BSUB -x
#BSUB -W 01:00
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
export job=gdas_enkf_post_${cyc}
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
module load ics/15.0.3

module list


#############################################################
# WCOSS environment settings
#############################################################
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered
export MP_PGMMODEL=mpmd
export POE=YES
export OMP_NUM_THREADS=1
export CHGRESTHREAD=24


#############################################################
# Set user specific variables
#############################################################
export NWTEST=/global/save/$LOGNAME/svn/gfs/branches
export PARA_CONFIG=$NWTEST/gdas.${gdas_ver}/driver/para_config.gdas_enkf_post
export JOBGLOBAL=$NWTEST/gdas.${gdas_ver}/jobs


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_ENKF_POST

exit
