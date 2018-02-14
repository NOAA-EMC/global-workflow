#!/bin/ksh

#BSUB -o gdas_verfozn.o%J
#BSUB -e gdas_verfozn.o%J
#BSUB -J gdas_verfozn
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 100
#BSUB -W 00:05
#BSUB -a poe
#BSUB -P GFS-T2O

##------------------------------------------------------------
##  This is the test driver script for the wcoss/ibm systems
##  to run the JGDAS_VERFOZN job.
##------------------------------------------------------------

set -x

export PDATE=${PDATE:-2018020806}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}


export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfozn.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}

me=`hostname | cut -c1`
export DATAROOT=${DATAROOT:-/gpfs/${me}d2/emc/da/noscrub/${LOGNAME}/test_data}
export COMROOT=${COMROOT:-/ptmpp1/${LOGNAME}/com}
export OZN_WORK_DIR=${OZN_WORK_DIR:-/stmpp1/${LOGNAME}/oznmon.${pid}}

#------------------------------------------------------------
# Specify versions
#
export gfs_ver=v15.0.0


#------------------------------------------------------------
# Load modules
#
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
module load prod_util
module load util_shared


module list


#------------------------------------------------------------
# WCOSS environment settings
#
export POE=YES


#------------------------------------------------------------
# Set user specific variables
#
export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/gpfs/${me}d2/emc/da/noscrub/${LOGNAME}/gfs.${gfs_ver}}

export HOMEgfs_ozn=${HOMEgfs_ozn:-${NWTEST}}
export SCRgfs_ozn=${SCRgfs_ozn:-${HOMEgfs_ozn}/scripts}
JOBgfs_ozn=${JOBgfs_ozn:-${HOMEgfs_ozn}/jobs}

export HOMEoznmon=${HOMEoznmon:-${NWTEST}}
export COM_IN=${COM_IN:-$DATAROOT}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}

#------------------------------------------------------------
# Execute job
#
${JOBgfs_ozn}/JGDAS_VERFOZN

exit

