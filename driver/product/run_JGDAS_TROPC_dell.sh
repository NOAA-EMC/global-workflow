#!/bin/sh

#BSUB -J gdas_tropc_f00_00
# #BSUB -o /gpfs/hps3/ptmp/Qingfu.Liu/output/gdas_tropc_f012_00.o%J
# #BSUB -e /gpfs/hps3/ptmp/Qingfu.Liu/output/gdas_tropc_f012_00.o%J

#BSUB -o /gpfs/dell2/ptmp/Boi.Vuong/output/gdas_tropc_f012_00.o%J
#BSUB -e /gpfs/dell2/ptmp/Boi.Vuong/output/gdas_tropc_f012_00.o%J
#BSUB -q debug
#BSUB -q dev
#BSUB -cwd /gpfs/dell2/ptmp/Boi.Vuong/output
#BSUB -W 00:30
#BSUB -P GFS-T2O
#BSUB -M 1000

export OMP_NUM_THREADS=1
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

export PDY=`date -u +%Y%m%d`

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
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load CFP/2.0.1
module load impi/18.0.1
module load lsf/10.1
module load prod_util/1.1.0
module load prod_envir/1.0.2
module load ips/18.0.1.163
module load NCL/6.4.0
#
#   This is a test version of GRIB_UTIL.v1.1.0 on DELL
#
module use -a /gpfs/dell1/nco/ops/nwpara/modulefiles/compiler_prod/ips/18.0.1
module load grib_util/1.1.0
module load  bufr_dumplist/2.0.0
module load  dumpjb/5.0.0

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

# export DATAROOT=/gpfs/hps3/ptmp/Qingfu.Liu/output
# export NWROOT=/gpfs/hps3/emc/global/noscrub/Qingfu.Liu/svn
# export COMROOT2=/gpfs/hps3/ptmp/Qingfu.Liu/com
# export HOMEgfs=/gpfs/hps3/emc/global/noscrub/Qingfu.Liu/fv3gfs_port2dell2

export DATAROOT=/gpfs/dell2/ptmp/Boi.Vuong/output
export NWROOT=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git
export COMROOT2=/gpfs/dell2/ptmp/Boi.Vuong/com

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
export UTILgfs=${UTILgfs:-$HOMEgfs/util}

# export TMPDIR=/gpfs/hps3/ptmp/Boi.Vuong/output

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
#  export COMIN=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gfs.${PDY}/${cyc} ### EMC PARA Realtime
#  export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gfs.${PDY}/${cyc} ### EMC PARA Realtime
#  export COMIN=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/${NET}/${envir}/${RUN}.${PDY}/${cyc}   ### Boi PARA
   export COMIN=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/${NET}/${envir}/${RUN}.${PDY}/${cyc}   ### Boi PARA
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
