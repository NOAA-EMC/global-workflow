#!/bin/sh

#BSUB -J jgdas_bulls_navy_00
#BSUB -o /gpfs/hps3/ptmp/Boi.Vuong/output/gdas_bulls_navy_00.o%J
#BSUB -e /gpfs/hps3/ptmp/Boi.Vuong/output/gdas_bulls_navy_00.o%J
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
# export PDY=20180516

export PDY1=`expr $PDY - 1`

 export cyc=00
# export cyc=12
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
module load util_shared/1.0.6
#
#   This is a test version of UTIL_SHARED.v1.0.7 on CRAY
#
# module use /usrx/local/nceplibs/util_shared.v1.0.7/modulefiles
# module load util_shared/1.0.7

module list

###############################################
# GDAS BULLS NAVYL BULLETIN PRODUCT GENERATION
###############################################
# set envir=prod or para to test with data in prod or para
# export envir=para
export envir=prod

export SENDCOM=YES
export KEEPDATA=YES
export job=gdas_bulls_navy_${cyc}
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
##########################################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
mkdir -p $DATA
cd $DATA

################################
# Set up the HOME directory
################################
export HOMEgfs=${HOMEgfs:-${NWROOT}/gfs.${gfs_ver}}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export PARMwmo=${PARMwmo:-$HOMEgfs/parm/wmo}
export PARMproduct=${PARMproduct:-$HOMEgfs/parm/product}
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}
export UTILgfs=${UTILgfs:-$HOMEgfs/util}

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}
export model=${model:-gdas}

##############################################
# Define COM, COMOUTwmo, COMIN  directories
##############################################
if [ $envir = "prod" ] ; then
#  This setting is for testing with GFS (production)
  export COMIN=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${PDY}         ### NCO PROD
  export COMINm1=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${PDY}         ### NCO PROD
else
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/com2/gfs/para/gdas.${PDY}         ### EMC PARA Realtime
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gdas.${PDY}/${cyc} ### EMC PARA Realtime  GFDL MP  on SURGE for BETA

  export COMIN=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn/gdas.${PDY}/${cyc} ### Boi PARA
  export COMINm1=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn/gdas.${PDYm1}/${cyc} ### Boi PARA

#  export COMIN=/gpfs/hps3/nco/ops/com/gfs/para/gdas.${PDY}       ### NCO PARA
fi

export COMOUT=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}
export COMOUTwmo=${COMOUTwmo:-${COMOUT}/wmo}

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT $COMOUTwmo
fi

#############################################
# run the GFS job
#############################################
sh $HOMEgfs/jobs/JGDAS_BULLS_NAVY
