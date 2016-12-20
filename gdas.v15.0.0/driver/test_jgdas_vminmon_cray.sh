#!/bin/ksh

#BSUB -o gdas_vminmon.o%J
#BSUB -e gdas_vminmon.o%J
#BSUB -J gdas_vminmon
#BSUB -q dev
#BSUB -M 80
#BSUB -W 00:05
#BSUB -P GFS-T2O
#BSUB -R "select[mem>80] rusage[mem=80]"
##BSUB -cwd /gpfs/hps/ptmp/Edward.Safford
##BSUB -cwd ${PWD}

set -x

export PDATE=${PDATE:-2016030700}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_vminmon.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para


#############################################################
# Specify versions
#############################################################
export gdas_ver=v14.1.0
export global_shared_ver=v14.1.0
export gdas_minmon_ver=v1.0.0
export minmon_shared_ver=v1.0.0


#############################################################
# Load modules
#############################################################
. $MODULESHOME/init/ksh

module load prod_util
module load pm5

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export DATAROOT=${DATAROOT:-/gpfs/hps/emc/da/noscrub/$LOGNAME/test_data}
export COMROOT=${COMROOT:-/gpfs/hps/ptmp/$LOGNAME/com}
export MINMON_SUFFIX=${MINMON_SUFFIX:-testminmon}
export NWTEST=${NWTEST:-/gpfs/hps/emc/da/noscrub/${LOGNAME}/gfs_q3fy17}
export HOMEgdas=${NWTEST}/gdas.${gdas_ver}
export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEminmon=${NWTEST}/global_shared.${global_shared_ver}
export COM_IN=${DATAROOT}
export M_TANKverf=${M_TANKverf:-${COMROOT}/${MINMON_SUFFIX}}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VMINMON

exit

