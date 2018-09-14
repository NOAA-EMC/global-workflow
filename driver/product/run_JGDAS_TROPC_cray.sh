#!/bin/sh

#BSUB -J gdas_tropc_f00_00
#BSUB -o /gpfs/hps3/ptmp/Qingfu.Liu/output/gdas_tropc_f012_00.o%J
#BSUB -e /gpfs/hps3/ptmp/Qingfu.Liu/output/gdas_tropc_f012_00.o%J
#BSUB -q dev
#BSUB -cwd /gpfs/hps3/ptmp/Qingfu.Liu/output
#BSUB -W 00:30
#BSUB -P GFS-T2O
#BSUB -M 1000

export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

export PDY=`date -u +%Y%m%d`
# export PDY=20180515

export PDY1=`expr $PDY - 1`

 export cyc=06
# export cyc=00
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
module load iobuf/2.0.7
module load craype-hugepages2M
module load craype-haswell
module load prod_envir
module load prod_util
module load grib_util/1.0.3
module load util_shared/1.0.6

#module unload grib_util/1.0.3
# module use /usrx/local/nceplibs/modulefiles
#module load grib_util/1.1.0
#
#   This is a test version of UTIL_SHARED.v1.0.7 on CRAY
#
# module use /usrx/local/nceplibs/util_shared.v1.0.7/modulefiles
# module load util_shared/1.0.7

export obsproc_dump_ver=v4.0.0
export obsproc_shared_bufr_dumplist_ver=v1.5.0
module load dumpjb/4.0.0

module use  /usrx/local/prod/modulefiles
module load  ncarg-intel-sandybridge/6.1.0

module list

############################################
# GDAS TROPC PRODUCT GENERATION
############################################

export fcsthrs="anl"
# export fcsthrs="000"

##############################################
# Define COM, COMOUTwmo, COMIN  directories
##############################################
# set envir=prod or para to test with data in prod or para
 export envir=para
# export envir=prod

export SENDCOM=YES
export KEEPDATA=YES
export job=gdas_tropc_${fcsthrs}_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}

# Set Fake DBNET for testing
export SENDDBN=YES
export DBNROOT=/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.24/fakedbn

export DATAROOT=/gpfs/hps3/ptmp/Qingfu.Liu/output
export NWROOT=/gpfs/hps3/emc/global/noscrub/Qingfu.Liu/svn
export COMROOT2=/gpfs/hps3/ptmp/Qingfu.Liu/com
export HOMEgfs=/gpfs/hps3/emc/global/noscrub/Qingfu.Liu/fv3gfs_port2dell2

mkdir -m 775 -p ${COMROOT2} ${COMROOT2}/logs ${COMROOT2}/logs/jlogfiles
export jlogfile=${COMROOT2}/logs/jlogfiles/jlogfile.${jobid}

#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0

################################
# Set up the HOME directory
################################
export HOMEgfs=${HOMEgfs:-${NWROOT}/gfs.${gfs_ver}}
export USHgfs=${USHgfs:-$HOMEgfs/ush}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export PARMwmo=${PARMwmo:-$HOMEgfs/parm/wmo}
export PARMproduct=${PARMproduct:-$HOMEgfs/parm/product}
export FIXgfs=${FIXgfs:-$HOMEgfs/fix}
export TMPDIR=/gpfs/hps3/ptmp/Qingfu.Liu/output

export HOMEobsproc_shared_bufr_dumplist=/gpfs/hps/nco/ops/nwtest/obsproc_shared/bufr_dumplist.v1.5.0
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
#  This setting is for testing with GDAS (production)
  export COMIN=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${PDY}         ### NCO PROD
else
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/com2/gfs/para/gdas.${PDY}         ### EMC PARA Realtime
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gdas.${PDY}/${cyc} ### EMC PARA Realtime
#  export COMIN=/gpfs/hps3/emc/global/noscrub/Qingfu.Liu/svn/gdas.${PDY}/${cyc} ### Boi PARA
  export COMIN=/gpfs/hps/nco/ops/com/gfs/prod/gdas.${PDY}
#  export COMIN=/gpfs/hps/nco/ops/com/gfs/para/gfs.${PDY}       ### NCO PARA
fi

export COMOUT=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}
export COMOUTwmo=${COMOUTwmo:-${COMOUT}/wmo}

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT $COMOUTwmo
fi

#########################################################
# obtain unique process id (pid) and make temp directory
#########################################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
mkdir -p $DATA
cd $DATA

#############################################
# run the GDAS job
#############################################
sh $HOMEgfs/jobs/JGDAS_TROPC
