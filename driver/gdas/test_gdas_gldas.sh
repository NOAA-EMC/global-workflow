#!/bin/sh

#BSUB -o /gpfs/dell2/ptmp/Youlong.Xia/gdas_gldas.o%J
#BSUB -e /gpfs/dell2/ptmp/Youlong.Xia/gdas_gldas.o%J
#BSUB -P NLDAS-T2O
#BSUB -J jgdas_gldas_12
#BSUB -W 01:00
#BSUB -q dev
#BSUB -n 112                     # number of tasks
#BSUB -R span[ptile=28]          # tasks per node
#BSUB -R affinity[core(1):distribute=balance]
#BSUB -M 3072
#BSUB -extsched 'CRAYLINUX[]'

set -x

date

export NODES=4
export ntasks=112
export ptile=28
export threads=1

export launcher="mpirun -n"
export npe_gaussian=6
export npe_gldas=112
export APRUN_GAUSSIAN="$launcher $npe_gaussian"
export APRUN_GLDAS="$launcher $npe_gldas"

export CDATE=${CDATE:-2019110700}

#############################################################
export KMP_AFFINITY=disabled

export PDY=`date -u +%Y%m%d`
export PDY=`echo $CDATE | cut -c1-8`

export PDY1=`expr $PDY - 1`

export cyc=`echo $CDATE | cut -c9-10`
export cycle=t${cyc}z

set -xa
export PS4='$SECONDS + '
date

####################################
##  Load theUtilities module
#####################################
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load CFP/2.0.1
module load impi/18.0.1
module load lsf/10.1
module load prod_envir/1.0.2
module load prod_util/1.1.4
module load grib_util/1.1.0
module load NetCDF/4.5.0
###########################################
# Now set up environment
###########################################
module list

############################################
# GDAS META PRODUCT GENERATION
############################################
# set envir=prod or para to test with data in prod or para
# export envir=prod
 export envir=para

export RUN=${RUN:-gdas}

export SENDCOM=YES
export KEEPDATA=YES
export job=gdas_gldas_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}

##############################################
# Define COM, COMOUTwmo, COMIN  directories
##############################################
  export HOMEgw=/gpfs/dell2/emc/modeling/noscrub/$LOGNAME/global-workflow
if [ $envir = "prod" ] ; then
#  This setting is for testing with GDAS (production)
  export HOMEgldas=/nwprod/gldas.${gldas_ver}
  export COMIN=/gpfs/dell1/nco/ops/com/gfs/prod/${RUN}.${PDY}         ### NCO PROD
  export COMROOT=/gpfs/dell1/nco/ops/com
  export DCOMROOT=/gpfs/dell1/nco/ops/dcom
elif [ $envir = "para" ] ; then
#  This setting is for testing with GDAS (production)
  export HOMEgldas=${HOMEgldas:-$HOMEgfs/sorc/gldas.fd}
  export COMIN=/gpfs/dell1/nco/ops/com/gfs/prod/${RUN}.${PDY}         ### NCO PROD
  export COMROOT=/gpfs/dell1/nco/ops/com
  export DCOMROOT=/gpfs/dell1/nco/ops/dcom
else
# export COMIN=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/${RUN}.${PDY}/${cyc}/nawips ### EMC PARA Realtime
# export COMINgdas=/gpfs/dell3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/${RUN} ### EMC PARA Realtime
  export workdir=${workdir:-$HOMEgfs}
  export HOMEgldas=$workdir/sorc/gldas.fd
  export COMROOT=$workdir/com
  export DCOMROOT=$workdir/dcom
  export COMINgdas=$COMROOT
  export DCOMIN=$DCOMROOT
  export COMIN=$workdir/comin
  export COMOUT=$workdir/comout
fi

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT $COMOUTncdc $COMOUTukmet $COMOUTecmwf
fi

# Set user specific variables
#############################################################
#export NWTEST=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/work
export PARA_CONFIG=$HOMEgw/driver/gdas/para_config.gdas_gldas
#export JOBGLOBAL=$NWTEST/gdas.${gdas_ver}/jobs
export JOBGLOBAL=$HOMEgldas/jobs

#############################################################
# Execute job
#############################################################
echo $JOBGLOBAL/JGDAS_GLDAS
$JOBGLOBAL/JGDAS_GLDAS

exit

