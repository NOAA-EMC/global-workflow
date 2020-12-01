#!/bin/sh

#BSUB -J jgfs_pgrb2_spec_gempak_00
#BSUB -o /gpfs/hps3/ptmp/Boi.Vuong/output/gfs_pgrb2_spec_gempak_00.o%J
#BSUB -e /gpfs/hps3/ptmp/Boi.Vuong/output/gfs_pgrb2_spec_gempak_00.o%J
#BSUB -q debug
#BSUB -cwd /gpfs/hps3/ptmp/Boi.Vuong/output
#BSUB -W 00:30
#BSUB -P GFS-T2O
#BSUB -R rusage[mem=1000]

export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

export PDY=`date -u +%Y%m%d`
expor PDY=20180710

export PDY1=`expr $PDY - 1`

# export cyc=06
export cyc=00
export cycle=t${cyc}z

set -xa
export PS4='$SECONDS + '
date

####################################
##  Load the GRIB Utilities module
#####################################

. $MODULESHOME/init/sh
module load PrgEnv-intel/5.2.56
module load cfp-intel-sandybridge/1.1.0
module load ESMF-intel-sandybridge/3_1_0rp5
module load iobuf/2.0.8
module load craype-hugepages2M
module load craype-haswell
module load prod_envir
module load prod_util
module load grib_util/1.0.3

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak/7.3.0

module list

############################################
# GFS_PGRB2_SPEC_GEMPAK PRODUCT GENERATION
############################################

export LAUNCH_MODE=MPI

###############################################
# Set MP variables
###############################################
export OMP_NUM_THREADS=1
export MP_LABELIO=yes
export MP_PULSE=0
export MP_DEBUG_NOTIMEOUT=yes

##############################################
# Define COM, COMOUTwmo, COMIN  directories
##############################################

# set envir=prod or para to test with data in prod or para
 export envir=para
# export envir=prod

export SENDCOM=YES
export KEEPDATA=YES
export job=gfs_pgrb2_spec_gempak_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}

# Set FAKE DBNET for testing
export SENDDBN=YES
export DBNROOT=/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.24/fakedbn

export DATAROOT=/gpfs/hps3/ptmp/Boi.Vuong/output
export NWROOT=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn
export COMROOT2=/gpfs/hps3/ptmp/Boi.Vuong/com

mkdir -m 775 -p ${COMROOT2} ${COMROOT2}/logs ${COMROOT2}/logs/jlogfiles 
export jlogfile=${COMROOT2}/logs/jlogfiles/jlogfile.${jobid}

#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0

##########################################################
# obtain unique process id (pid) and make temp directory
#########################################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
mkdir -p $DATA
cd $DATA

################################
# Set up the HOME directory
################################
export HOMEgfs=${HOMEgfs:-${NWROOT}/gfs.${gfs_ver}}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}

###################################
# Specify NET and RUN Name and model
####################################
export NET=gfs

##############################################
# Define COM directories
##############################################
if [ $envir = "prod" ] ; then
#  This setting is for testing with GFS (production)
  export COMIN=/gpfs/hps/nco/ops/com/gfs/prod/gfs.${PDY}         ### NCO PROD
else
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/com2/gfs/para/gfs.${PDY}         ### EMC PARA Realtime
#   export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gfs.${PDY}/${cyc} ### EMC PARA Realtime

#  export COMIN=/gpfs/hps3/ptmp/Boi.Vuong/com/gfs/para/gfs.${PDY}/${cyc} ### Boi PARA

#  export COMIN=/gpfs/hps/nco/ops/com/gfs/para/gfs.${PDY}       ### NCO PARA
fi

export COMOUT=${COMROOT2}/${NET}/${envir}/${NET}.${PDY}/${cyc}/nawips

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT
fi

#################################################################
# Execute the script for the regular grib
#################################################################
export DATA_HOLD=$DATA
export DATA=$DATA_HOLD/SPECIAL
mkdir -p $DATA
cd $DATA

#############################################
# run the GFS job
#############################################
sh $HOMEgfs/jobs/JGFS_PGRB2_SPEC_GEMPAK
