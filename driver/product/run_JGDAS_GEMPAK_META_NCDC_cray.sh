#!/bin/sh

#BSUB -J jgdas_gempak_meta_ncdc_00
#BSUB -o /gpfs/hps3/ptmp/Boi.Vuong/output/gdas_gempak_meta_ncdc_00.o%J
#BSUB -e /gpfs/hps3/ptmp/Boi.Vuong/output/gdas_gempak_meta_ncdc_00.o%J
#BSUB -W 00:30
#BSUB -q debug
#BSUB -cwd /gpfs/hps3/ptmp/Boi.Vuong/output
#BSUB -P GFS-T2O
#BSUB -R rusage[mem=1000]

export OMP_NUM_THREADS=1
export OMP_STACKSIZE=1024m
export MP_LABELIO=yes
export MP_STDOUTMODE=ordered

export PDY=`date -u +%Y%m%d`
export PDY=20180516


export PDY1=`expr $PDY - 1`

export cyc=00
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

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak/7.3.0

module list

############################################
# GDAS GEMPAK META PRODUCT GENERATION
############################################
# set envir=prod or para to test with data in prod or para
 export envir=para
# export envir=prod

export SENDCOM=YES
export KEEPDATA=YES
export job=gfs_gempak_meta_ncdc_${cyc}
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
export FIXgfs=${FIXgfs:-$HOMEgfs/fix}
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}

######################################
# Set up the GEMPAK directory
#######################################
export HOMEgempak=${HOMEgempak:-${NWROOTp1}/gempak}
export FIXgempak=${FIXgempak:-$HOMEgempak/fix}
export USHgempak=${USHgempak:-$HOMEgempak/ush}

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
  export COMIN=/gpfs/hps/nco/ops/com/nawips/prod/${RUN}.${PDY}         ### NCO PROD
  export COMROOT=/gpfs/hps/nco/ops/com
else
# export COMIN=/gpfs/dell2/ptmp/emc.glopara/ROTDIRS/prfv3rt1/${RUN}.${PDY}/${cyc}/nawips ### EMC PARA Realtime
#  export COMINgdas=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/${RUN}
  export COMIN=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/${NET}/${envir}/${RUN}.${PDY}/${cyc}/nawips   ### Boi PARA
  export COMINgdas=/gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/${NET}/${envir}/${RUN}  ### Boi PARA
fi

export COMINukmet=${COMINukmet:-$(compath.py nawips/prod/ukmet)}
export COMINecmwf=${COMINecmwf:-$(compath.py nawips/prod/ecmwf)}

export COMOUTukmet=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}/nawips
export COMOUTecmwf=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}/nawips

export COMOUTncdc=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}
export COMOUT=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}/nawips/meta

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT $COMOUTukmet $COMOUTecmwf
fi

#############################################
# run the GFS job
#############################################
sh $HOMEgfs/jobs/JGDAS_GEMPAK_META_NCDC
