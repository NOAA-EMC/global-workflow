#!/bin/ksh

#BSUB -o gdas_verfrad.o%J
#BSUB -e gdas_verfrad.o%J
#BSUB -J gdas_verfrad
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 100
#BSUB -W 00:20
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export PDATE=${PDATE:-2018022112}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
me=`hostname | cut -c1`
export DATAROOT=${DATAROOT:-/gpfs/${me}d2/emc/da/noscrub/${LOGNAME}/test_data}
export COMROOT=${COMROOT:-/ptmpp1/$LOGNAME/com}


#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load prod_util
module load util_shared


module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}
export NWTEST=${NWTEST:-/gpfs/${me}d2/emc/da/noscrub/Edward.Safford/gfs.${gfs_ver}}
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

