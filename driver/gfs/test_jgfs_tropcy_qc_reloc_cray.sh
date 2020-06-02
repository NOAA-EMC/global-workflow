#!/bin/bash 
#BSUB -J t1534
#BSUB -W 0:30
#BSUB -extsched 'CRAYLINUX[]' -R '1*{order[slots] select[craylinux && !vnode]} + 168*{select[craylinux && vnode]span[ptile=24] cu[type=cabinet]}'
#BSUB -o /gpfs/hps/emc/global/noscrub/Qingfu.Liu/gfs.v14.1.0/driver/gfs_tropcy_qc_reloc_06.o%J
#BSUB -e /gpfs/hps/emc/global/noscrub/Qingfu.Liu/gfs.v14.1.0/driver/gfs_tropcy_qc_reloc_06.o%J
###BSUB -o t574.stdout.%J
###BSUB -e t574.stderr.%J
#BSUB -q "dev"
#BSUB -P "GFS-T2O"
#BSUB -M 200
###BSUB -M "60"


module use /gpfs/hps/nco/ops/nwprod/modulefiles
module load prod_util
module unload grib_util
module load grib_util/1.0.3
##module load crtm-intel/2.2.3
module load PrgEnv-intel craype cray-mpich ESMF-intel-haswell/3_1_0rp5
module list

#export MKL_CBWR=AVX
#ulimit -s unlimited
#ulimit -a

set -x

export OMP_NUM_THREADS=24

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
export cyc=06
export job=gfs_tropcy_qc_reloc_${cyc}
export RUN_ENVIR=test
#export NWROOT=/nwprod2
#export NWROOT=/global/save/Qingfu.Liu
export NWROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu

#export DATAROOT=/tmpnwprd_p2
#export DATAROOT=/ptmpp2/Qingfu.Liu
export DATAROOT=/gpfs/hps/ptmp/Qingfu.Liu

#export COMROOT=/com2
#export COMROOT=/ptmpp2/Qingfu.Liu/com2
#export COMROOT=/gpfs/hps/ptmp/Qingfu.Liu/com
#export COMDATEROOT=/com
export COMROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com
export COMDATEROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com

#export DCOMROOT=/dcom

#export COMROOTp1=/gpfs/gp1/nco/ops/com
#export COMROOTp1=/gpfs/tp1/nco/ops/com
export COMROOTp1=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com
export KEEPDATA=YES
export CLEAN=NO
export cycle=t${cyc}z

#which setpdy.sh
#setpdy.sh
#. PDY

export PDY=20140814
##export PDY=20150723

#export COMINgfs=/com/gfs/prod/gfs.${PDY}
#export COMINgdas=/com/gfs/prod/gdas.${PDY}
#export COMINgfs=/gpfs/tp1/nco/ops/com/gfs/prod/gfs.${PDY}
#export COMINgdas=/gpfs/tp1/nco/ops/com/gfs/prod/gdas.${PDY}
export COMINgfs=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com/gfs/$envir/gfs.${PDY}
export COMINgdas=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com/gfs/$envir/gdas.${PDY}
export ARCHSYND=/gpfs/tp1/nco/ops/com/arch/prod/syndat
export HOMENHC=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/guidance/storm-data/ncep
#export GETGES_COM=/gpfs/tp1/nco/ops/com
export GETGES_COM=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com
export GESROOT=/gpfs/hps/emc/global/noscrub/Qingfu.Liu/com

# versions file for tracker $tracker.ver
VERSION_FILE=${NWROOT}/versions/tropcy_qc_reloc.ver
if [ -f $VERSION_FILE ]; then
  . $VERSION_FILE
else
  ecflow_client --abort
  exit
fi

export shared_global_home=$NWROOT/global_shared.v14.1.0
export gfs_global_home=$NWROOT/gfs.v14.1.0
export gdas_global_home=$NWROOT/gdas.v14.1.0

export files_override=F
export PROCESS_TROPCY=NO
export copy_back=NO
export SENDCOM=NO
export APRNRELOC="time aprun -b -j1 -n7 -N1 -d24 -cc depth "
export APRNGETTX="time aprun -q -j1 -n1 -N1 -d1 -cc depth"
#export APRNRELOC="time aprun -b -j0 -n7 -N1 -d32 -cc depth"

# CALL executable job script here
export HOMERELO=$shared_global_home
#export HOMERELO=${NWROOT}/tropcy_qc_reloc.${tropcy_qc_reloc_ver}_r62774_phase2
export HOMESYND=${HOMERELO}
#export envir_getges=prod
$gfs_global_home/jobs/JGFS_TROPCY_QC_RELOC


if [ $? -ne 0 ]; then
#  ecflow_client --abort
  exit
fi

#%include <tail.h> 
#%manual
######################################################################
#PURPOSE:  Executes the job JGFS_TROPCY_QC_RELOC
######################################################################
#############################################################
#  Function been tested:            TCvital quality control and archive, hurricane relocation
#
#  Calling sequence:                JGFS_TROPCY_QC_RELOC, extropcy_qc_reloc.sh.ecf, 
#  #                                   syndat_qctropcy.sh, tropcy_relocate.sh,syndat_getjtbul.sh,
#  #                                   tropcy_relocate_extrkr.sh,parse-storm-type.pl
#
#  Initial condition:               provide hours (cyc=?)
#
#  Usage:                           bsub < test_jgfs_tropcy_qc_reloc.ecf
#
#  Data_In:                         COMINgfs=/com/gfs/prod/gfs.${PDY}
#                                   COMINgdas=/com/gfs/prod/gdas.${PDY}
#
#  Data_Out:                        /ptmpp2/Qingfu.Liu/com2/gfs/dev2/gfs.${PDY}
#
#  Result verification:             compare with the operational results
#                      (results might be slightly different due to 3hourly/hourly tracker)
##############################################################
######################################################################
# Job specific troubleshooting instructions:
#  see generic troubleshoot manual page
#
######################################################################

# include manual page below
#%end
