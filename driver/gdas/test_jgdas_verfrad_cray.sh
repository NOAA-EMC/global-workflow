#!/bin/ksh

#BSUB -o gdas_verfrad.o%J
#BSUB -e gdas_verfrad.o%J
#BSUB -J gdas_verfrad
#BSUB -q dev
#BSUB -M 100
#BSUB -W 00:20
#BSUB -P GFS-T2O
#BSUB -R "select[mem>100] rusage[mem=100]"

set -x

export PDATE=${PDATE:-2016100106}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para

#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0
#export global_shared_ver=v14.1.0
#export gdas_radmon_ver=v2.0.0
#export radmon_shared_ver=v2.0.4


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export DATAROOT=${DATAROOT:-/gpfs/hps3/emc/da/noscrub/$LOGNAME/test_data}
export COMROOT=${COMROOT:-/gpfs/hps2/ptmp/$LOGNAME/com}
export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}
export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/gfs.${gfs_ver}}
export HOMEgfs=${HOMEgfs:-${NWTEST}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgfs}/jobs}
export HOMEradmon=${HOMEradmon:-${NWTEST}}
export COM_IN=${COM_IN:-${DATAROOT}}
export TANKverf=${TANKverf:-${COMROOT}/${RADMON_SUFFIX}}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFRAD

exit

