#BSUB -J JGDAS_TROPC_TEST_06
#BSUB -o /ptmpp2/Qingfu.Liu/com2/jgdas_tropc_06.o%J
#BSUB -e /ptmpp2/Qingfu.Liu/com2/jgdas_tropc_06.o%J
#BSUB -L /bin/sh
#BSUB -q debug
#BSUB -W 00:30
#BSUB -cwd /ptmpp2/Qingfu.Liu/com2
#BSUB -P GFS-T2O
##BSUB -R rusage[mem=5000]
##BSUB -R affinity[core]
#BSUB -n 1 
#BSUB -R span[ptile=1]
#BSUB -R affinity[cpu(32):distribute=balance]
#BSUB -a poe
#BSUB -x

#%include <head.h> 
#%include <envir-p2.h>
. /usrx/local/Modules/default/init/ksh
module use /nwprod2/modulefiles
module load grib_util/v1.0.1
module load prod_util/v1.0.1
module load ibmpe ics lsf
#module load grib_util/v1.0.1
module use /nwpara2/modulefiles
module load util_shared/v1.0.3

set -x

export OMP_NUM_THREADS=32

 export MP_LABELIO=yes
 export MP_MPILIB=mpich2
 export MP_EUILIB=us
 export MP_TASK_AFFINITY=cpu:32
 export MP_USE_BULK_XFER=yes
 export MP_STDOUTMODE=unordered
 export MPICH_ALLTOALL_THROTTLE=0
 export MP_COREFILE_FORMAT=core.txt
 export OMP_STACKSIZE=3G
 export MP_COMPILER=intel

#export envir=dev2
export envir=prod
export cyc=06
export job=jgdas_tropc_${cyc}
export RUN_ENVIR=test
#export NWROOT=/nwprod2
export NWROOT=/global/save/Qingfu.Liu

#export DATAROOT=/tmpnwprd_p2
export DATAROOT=/ptmpp2/Qingfu.Liu

#export COMROOT=/com2
export COMROOT=/ptmpp2/Qingfu.Liu/com2
export COMDATEROOT=/com2
export DATA_DUMPJB=/ptmpp2/Qingfu.Liu/com2/111

#export DCOMROOT=/dcom

export COMROOTp1=/com
export KEEPDATA=YES
export CLEAN=NO
export cycle=t00z

#which setpdy.sh
setpdy.sh
. PDY

#export PDY=20160216

export COMIN=/com/gfs/prod/gdas.${PDY}

#export utilscript=/nwprod2/util/ush
#export utilexec=/nwprod2/util/exec
#export utilities=/nwprod2/util/ush
#export HOMEutil=/nwprod2/util
#export HOMEgfs=/nwprod2/util
#export HOMEgraph=/nwprod2/util

export utilscript=$NWROOT/util/ush
export utilexec=$NWROOT/util/exec
export utilities=$NWROOT/util/ush
export HOMEutil=$NWROOT/util
#export HOMEgfs=$NWROOT/util
export HOMEgraph=$NWROOT/util

# versions file for tracker $tracker.ver
VERSION_FILE=${NWROOT}/versions/tropcy_qc_reloc.ver
if [ -f $VERSION_FILE ]; then
  . $VERSION_FILE
else
  ecflow_client --abort
  exit
fi

#export shared_global_home=$NWROOT/shared_nco_20160129
export HOMEgfs=$NWROOT/gfs.v13.0.0
#export HOMEgdas=$NWROOT/gdas.v13.0.0
export HOMEgdas=$NWROOT/gdas_nco_20160129

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
