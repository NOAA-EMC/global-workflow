#!/bin/ksh

#BSUB -o gdas_verfozn.o%J
#BSUB -e gdas_verfozn.o%J
#BSUB -J gdas_verfozn
#BSUB -q dev
#BSUB -M 80
#BSUB -W 00:05
#BSUB -P GFS-T2O
#BSUB -R "select[mem>80] rusage[mem=80]"

##------------------------------------------------------------
##  This is the test driver script for the cray systems
##  to run the JGDAS_VERFOZN job.
##------------------------------------------------------------

set -x

export PDATE=${PDATE:-2018020812}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}


export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfozn.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}

me=`hostname | cut -c1`
export DATAROOT=${DATAROOT:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/test_data}
export COMROOT=${COMROOT:-/gpfs/hps2/ptmp/${LOGNAME}/com}
export OZN_WORK_DIR=${OZN_WORK_DIR:-/gpfs/hps2/stmp/${LOGNAME}/oznmon.${pid}}

#------------------------------------------------------------
# Specify versions
#
export gfs_ver=v15.0.0


#------------------------------------------------------------
# Load modules
#
. $MODULESHOME/init/ksh

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
export NWTEST=${NWTEST:-/gpfs/hps3/emc/da/noscrub/${LOGNAME}/gfs.${gfs_ver}}

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

