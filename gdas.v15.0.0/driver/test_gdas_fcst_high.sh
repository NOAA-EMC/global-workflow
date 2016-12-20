#!/bin/ksh --login
#BSUB -L /bin/ksh
#BSUB -a poe
#BSUB -P GFS-T2O
#BSUB -e /ptmpd2/Fanglin.Yang/gdas_forecast_high.o%J               
#BSUB -o /ptmpd2/Fanglin.Yang/gdas_forecast_high.o%J               
#BSUB -J gdas_forecast_high             
#BSUB -network type=sn_all:mode=US
#BSUB -q dev2
#BSUB -n 258
#BSUB -R span[ptile=6]
#BSUB -R affinity[core(4)]
#BSUB -x
#BSUB -W 01:00
set -x

export MP_EUIDEVELOP=min
export KMP_STACKSIZE=2048m
export MPICH_ALLTOALL_THROTTLE=0
export MP_SINGLE_THREAD=yes
export MP_EAGER_LIMIT=65536
export MP_USE_BULK_XFER=no
export MP_COLLECTIVE_OFFLOAD=no
export MP_SHARED_MEMORY=yes
export MP_MPILIB=mpich2
export MP_LABELIO=yes
export LSB_PJL_TASK_GEOMETRY="$(/usrx/local/bin/mktgs $((258-6))/6 1/1)"

##############################
# Set up the UTILITIES
##############################
. /usrx/local/Modules/default/init/ksh
module load prod_util


#------------------------------------------
testdir=/ptmpd2/Fanglin.Yang/test_nwpara$$
mkdir -p $testdir                             
cd $testdir ||exit 8
 
export CDATE=2016020800
initdir=/ptmpd3/emc.glopara/pr4devb
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`


#--- set up running environment ---
export RUNdir=gdas 
export NET=gfs  
export RUN_ENVIR=prod
export envir=prod
export util_ver=v1.0.1
export job=gdas_forecast_${CDATE}
export DATA=$testdir/${job}
export jlogfile=$DATA/jlogfile.${job}
export SENDECF=NO                  
export SENDCOM=NO                  
export gfs_ver=v13.0.0
export global_shared_ver=v13.0.0
export HOMEglobal=/global/save/Fanglin.Yang/nwpara/shared_nco_20160129
export HOMEgfs=/global/save/Fanglin.Yang/nwpara/gfs_nco_20160129
export HOMEgdas=/global/save/Fanglin.Yang/nwpara/gdas_nco_20160129

export COMIN=$DATA
export COMOUT=$testdir/com/${NET}/${envir}/${RUNdir}.${PDY}
export gespath=$testdir/nwges/$envir
export GESdir=$gespath/${RUNdir}.${PDY}
export RM_TMPDIR=NO
#-------------------------------------

#--- copy initial condition to guess dir ---
mkdir -p $GESdir
cp  $initdir/siganl.gdas.$CDATE $GESdir/gdas1.t${cyc}z.sanl
cp  $initdir/sfcanl.gdas.$CDATE $GESdir/gdas1.t${cyc}z.sfcanl
if [ -d $DATA ]; then rm -rf $DATA ; fi


# Execute job
export NWPARA=/global/save/$LOGNAME/nwpara
export FORECASTSH=$HOMEglobal/scripts/exglobal_fcst.sh.ecf
$HOMEgdas/jobs/JGDAS_FORECAST_HIGH

exit
