#!/bin/sh

# LSBATCH: User input
#BSUB -J jgfs_gempak_00
#BSUB -oo gfs_gempak_00.o%J
#BSUB -eo gfs_gempak_00.o%J
#BSUB -L /bin/sh
#BSUB -q debug
#BSUB -cwd /gpfs/hps3/ptmp/Boi.Vuong/output
#BSUB -W 03:00
#BSUB -P GFS-T2O
#BSUB -R rusage[mem=2000]
#BSUB -extsched 'CRAYLINUX[]'    # Request to run on compute nodes

export KMP_AFFINITY=disabled

# export PDY=`date -u +%Y%m%d`
export PDY=20180226

# export cyc=06
export cyc=00
export cycle=t${cyc}z

set -xa
export PS4='$SECONDS + '
date

####################################
##  Load the GRIB Utilities module
####################################

. $MODULESHOME/init/sh
module load PrgEnv-intel/5.2.56
module load cfp-intel-sandybridge/1.1.0
module load ESMF-intel-sandybridge/3_1_0rp5
module load iobuf/2.0.7
module load craype-hugepages2M
module load craype-haswell
module load prod_envir
module load prod_util/1.0.5
module load util_shared/1.0.6

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak/7.3.0
module load grib_util/1.0.3
#
#   This is a test version of UTIL_SHARED.v1.0.7 on CRAY
#
# module use /gpfs/hps/nco/ops/nwtest/modulefiles
# module use /usrx/local/nceplibs/util_shared.v1.0.7/modulefiles
# module load util_shared/1.0.7

module list

############################################
# Define COM, PCOM, COMIN  directories
############################################
# set envir=prod or para to test with data in prod or para
 export envir=para
# export envir=prod

export SENDCOM=YES
export SENDDBN=NO
export KEEPDATA=YES
export job=gfs_gempak_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=/gpfs/hps3/ptmp/Boi.Vuong/output
# export NWROOT=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn
export NWROOT=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn/fv3gfs
export COMROOT2=/gpfs/hps3/ptmp/Boi.Vuong/com

mkdir -m 775 -p ${COMROOT2} ${COMROOT2}/logs ${COMROOT2}/logs/jlogfiles $PCOMROOT2
export jlogfile=${COMROOT2}/logs/jlogfiles/jlogfile.${jobid}

#############################################################
# Specify versions
#############################################################
export gdas_ver=v15.0.0
export gfs_ver=v15.0.0

##########################################################
# obtain unique process id (pid) and make temp directory
##########################################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
mkdir -p $DATA
cd $DATA

################################
# Set up the HOME directory
################################
export HOMEgfs=${HOMEgfs:-${NWROOT}}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}
export model=${model:-gfs}

##############################################
# Define COM directories
##############################################
if [ $envir = "prod" ] ; then
#  This setting is for testing with GFS (production)
  export COMIN=/gpfs/hps/nco/ops/com/gfs/prod/gfs.${PDY}         ### NCO PROD
else
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/com2/gfs/para/gfs.${PDY}         ### EMC PARA Realtime
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gfs.${PDY}/${cyc} ### EMC PARA Realtime
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/prfv3l65/gfs.${PDY}/${cyc} ### EMC PARA Realtime
  export COMIN=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn/gfs.${PDY}/${cyc} ### Boi PARA
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3test/gfs.${PDY}/${cyc}  ### EMC test PARA ####

#  export COMIN=/gpfs/hps/nco/ops/com/gfs/para/gfs.${PDY}       ### NCO PARA
fi

export COMOUT=${COMOUT:-${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}/nawips}

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT
fi

export NODES=5
export ntasks=17
export ptile=4
export threads=6

#############################################
# run the GFS job
#############################################
sh $HOMEgfs/jobs/JGFS_GEMPAK
