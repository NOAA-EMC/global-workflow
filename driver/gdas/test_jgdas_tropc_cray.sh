#!/bin/bash 
#BSUB -J JGDAS_TROPC
#BSUB -W 0:30
####BSUB -extsched 'CRAYLINUX[]' -R '1*{order[slots] select[craylinux && !vnode]} + 24*{select[craylinux && vnode]span[ptile=24] cu[type=cabinet]}'
BSUB -extsched 'CRAYLINUX[]'
#BSUB -o /gpfs/hps/emc/global/noscrub/Qingfu.Liu/gdas.v14.1.0/driver/test_jgdas_tropc.o%J
#BSUB -e /gpfs/hps/emc/global/noscrub/Qingfu.Liu/gdas.v14.1.0/driver/test_jgdas_tropc.o%J
#BSUB -q "dev"
#BSUB -P "GFS-T2O"
#BSUB -M 500

module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_util
module unload grib_util
module load grib_util/1.0.3
module load util_shared/1.0.3
##module load crtm-intel/2.2.3
module load PrgEnv-intel craype cray-mpich ESMF-intel-haswell/3_1_0rp5
module load NCL-gnu-sandybridge/6.3.0
module load gcc
module list

set -x

export OMP_NUM_THREADS=12

 export MP_LABELIO=yes
 export MP_MPILIB=mpich2
 export MP_EUILIB=us
 export MP_TASK_AFFINITY=cpu:24
 export MP_USE_BULK_XFER=yes
 export MP_STDOUTMODE=unordered
 export MPICH_ALLTOALL_THROTTLE=0
 export MP_COREFILE_FORMAT=core.txt
 export OMP_STACKSIZE=3G
 export MP_COMPILER=intel

export envir=para
#export envir=prod
export cyc=06
export job=jgdas_tropc_${cyc}
export RUN_ENVIR=test
#export NWROOT=/nwprod2
#export NWROOT=/global/save/Qingfu.Liu
export NWROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu

export DATAROOT=/gpfs/hps/ptmp/Qingfu.Liu/com
#export DATAROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu
#export COMROOT=/com2
export COMROOT=/gpfs/hps/ptmp/Qingfu.Liu/com
#export COMROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com
export COMDATEROOT=/gpfs/hps/ptmp/Qingfu.Liu/com
#export COMDATEROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com
#export COMDATEROOT=/gpfs/tp2/nco/ops/com
export DATA_DUMPJB=/gpfs/hps/ptmp/Qingfu.Liu/com/111
#export archsyndir=/gpfs/tp1/nco/ops/com/arch/prod/syndat

#export DCOMROOT=/dcom
export DCOMROOT=/gpfs/tp1/nco/ops/dcom

#export COMROOTp1=/gpfs/gp1/nco/ops/com
export COMROOTp1=/gpfs/hps/ptmp/Qingfu.Liu/com
#export COMROOTp1=/gpfs/tp2/nco/ops/com
export KEEPDATA=YES
export CLEAN=NO
export cycle=t${cyc}z
export LOUD=on
export BACK=on

##which setpdy.sh
##setpdy.sh
##. PDY

#export PDY=20150723
#export PDY=20140814
export PDY=20170108

#export COMIN=/gpfs/tp2/nco/ops/com/gfs/prod/gdas.${PDY}
export COMIN=/gpfs/hps/ptmp/Qingfu.Liu/com/gfs/para/gdas.${PDY}
#export COMIN=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com/gfs/para/gdas.${PDY}

#export NWPROOT=/gpfs/tp1/nco/ops/nwprod/util
#export utilscript=$NWPROOT/util/ush
#export utilexec=$NWPROOT/util/exec
#export utilities=$NWPROOT/util/ush
#export HOMEutil=$NWPROOT/util
#export HOMEgraph=$NWPROOT/util

# versions file for tracker $tracker.ver
VERSION_FILE=${NWROOT}/versions/tropcy_qc_reloc.ver
if [ -f $VERSION_FILE ]; then
  . $VERSION_FILE
else
  ecflow_client --abort
  exit
fi

#export DUMP=/gpfs/hps/nco/ops/nwprod/hwrf_dump.v3.2.1/ush/dumpjb
#export HOMEobsproc_dump=/gpfs/hps/nco/ops/nwprod/hwrf_dump.v3.2.1
export DUMP=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/obsproc_dump.tkt-351.crayport/ush/dumpjb
export HOMEobsproc_dump=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/obsproc_dump.tkt-351.crayport
#export FIXobsproc_bufr_dumplist=/gpfs/hps/nco/ops/nwprod/obsproc_bufr_dumplist.v1.2.0/fix
export FIXobsproc_bufr_dumplist=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/gdas.v14.1.0/driver/fix
export HOMEobsproc_shared_bufr_dumplist=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/gdas.v14.1.0/driver
#export HOMEobroc_bufr_dumplist=/gpfs/hps/nco/ops/nwprod/obsproc_bufr_dumplist.v1.2.0

export HOMEgfs=$NWROOT/gfs.v14.1.0
export HOMEgdas=$NWROOT/gdas.v14.1.0

# CALL executable job script here
#export HOMERELO=${HOMEgdas}
#export HOMESYND=${HOMERELO}
#export envir_getges=prod
$HOMEgdas/jobs/JGDAS_TROPC

if [ $? -ne 0 ]; then
#  ecflow_client --abort
  exit
fi

#%include <tail.h> 
#%manual
######################################################################
#PURPOSE:  Executes the job that creates GFS TC track forecasts
######################################################################

######################################################################
# Job specific troubleshooting instructions:
#  see generic troubleshoot manual page
#
######################################################################

# include manual page below
#%end
