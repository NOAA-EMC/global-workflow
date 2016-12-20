#!/bin/ksh --login
#BSUB -L /bin/ksh
#BSUB -a poe
#BSUB -P GFS-T2O
#BSUB -e /ptmpd2/Fanglin.Yang/gfs_forecast_low.o%J               
#BSUB -o /ptmpd2/Fanglin.Yang/gfs_forecast_low.o%J               
#BSUB -J gfs_forecast_low             
#BSUB -network type=sn_all:mode=US
#BSUB -q dev2
##BSUB -n 96  
#BSUB -n 216 
#BSUB -R span[ptile=12]
#BSUB -R affinity[core(2)]
#BSUB -x
#BSUB -W 06:00
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
export RUN=gfs  
export NET=gfs  
export RUN_ENVIR=prod
export envir=prod
export util_ver=v1.0.1
export job=gfs_forecast_low_${CDATE}
export DATA=$testdir/${job}
export jlogfile=$DATA/jlogfile.${job}
export SENDECF=NO                  
export SENDCOM=NO                  
export gfs_ver=v13.0.0
export global_shared_ver=v13.0.0
export HOMEgfs=/global/save/Fanglin.Yang/nwpara/gfs_nco_20160129
export HOMEglobal=/global/save/Fanglin.Yang/nwpara/shared_nco_20160129

export COMIN=$DATA
export COMOUT=$testdir/com/${NET}/${envir}/${RUN}.${PDY}
export gespath=$testdir/nwges/$envir
export GESdir=$gespath/${RUN}.${PDY}
export RM_TMPDIR=NO
#-------------------------------------

#--- copy initial condition to guess dir ---
mkdir -p $GESdir
cp  $initdir/sigf240.gfs.$CDATE $GESdir/gfs.t${cyc}z.sf240
cp  $initdir/sfcf240.gfs.$CDATE $GESdir/gfs.t${cyc}z.bf240  
if [ -d $DATA ]; then rm -rf $DATA ; fi

# Determine whether or not to run 12-hour spin-up for low job
export SPINUPLOW=NO  

# Execute job
export NWPARA=/global/save/$LOGNAME/nwpara
export FORECASTSH=$HOMEglobal/scripts/exglobal_fcst.sh.ecf
$HOMEgfs/jobs/JGFS_FORECAST_LOW

exit
