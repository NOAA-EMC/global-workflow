#!/bin/ksh

#BSUB -o gdas_vminmon.o%J
#BSUB -e gdas_vminmon.o%J
#BSUB -J gdas_vminmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 80
#BSUB -W 00:05
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export PDATE=${PDATE:-2016030706}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
me=`hostname | cut -c1`

export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/gpfs/${me}d2/emc/da/noscrub/Edward.Safford/test_data
export COMROOT=/ptmpp1/$LOGNAME/com

#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load grib_util
module load prod_util

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon}
export NWTEST=${NWTEST:-/gpfs/${me}d2/emc/da/noscrub/Edward.Safford}
export HOMEgfs=${HOMEgfs:-${NWTEST}/gfs.${gfs_ver}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgfs}/jobs}

export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${M_TANKverf:-${COMROOT}/${MINMON_SUFFIX}}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VMINMON

exit

