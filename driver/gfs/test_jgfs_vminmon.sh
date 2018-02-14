#!/bin/ksh

#BSUB -o gfs_vminmon.o%J
#BSUB -e gfs_vminmon.o%J
#BSUB -J gfs_vminmon
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 80
#BSUB -W 00:05
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export NET='gfs'
export RUN='gfs'
export PDATE=${PDATE:-2016030206}

export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gfs_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export Z=${Z:-gz}
me=`hostname | cut -c1`
export DATAROOT=${DATAROOT:-/gpfs/${me}d2/emc/da/noscrub/Edward.Safford/test_data}
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
#module load grib_util
module load prod_util
#module load util_shared

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export MINMON_SUFFIX=testminmon
export NWTEST=${NWTEST:-/gpfs/${me}d2/emc/da/noscrub/Edward.Safford}
export HOMEgfs=${HOMEgfs:-${NWTEST}/gfs.${gfs_ver}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgfs}/jobs}
#export HOMEminmon=${HOMEminmon:-${NWTEST}/global_shared.${global_shared_ver}}

export COM_IN=${COM_IN:-${DATAROOT}}
export M_TANKverf=${M_TANKverf:-${COMROOT}/${MINMON_SUFFIX}}

jlogdir=${jlogdir:-/ptmpp1/${LOGNAME}/jlogs}
if [[ ! -d ${jlogdir} ]]; then
   mkdir -p ${jlogdir}
fi

export jlogfile=${jlogfile:-${jlogdir}/${MINMON_SUFFIX}.${NET}.${RUN}.jlogfile}
if [[ -e ${jlogfile} ]]; then
  rm -f ${jlogfile}
fi

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGFS_VMINMON

exit

