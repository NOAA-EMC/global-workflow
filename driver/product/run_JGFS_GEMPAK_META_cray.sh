#!/bin/sh

#BSUB -J jgfs_gempak_meta_00
#BSUB -o /gpfs/hps3/ptmp/Boi.Vuong/output/gfs_gempak_meta_00.o%J
#BSUB -e /gpfs/hps3/ptmp/Boi.Vuong/output/gfs_gempak_meta_00.o%J
#BSUB -q debug
#BSUB -cwd /gpfs/hps3/ptmp/Boi.Vuong/output
#BSUB -W 03:00
#BSUB -P GFS-T2O
#BSUB -R rusage[mem=2000]

#BSUB -extsched 'CRAYLINUX[]'    # Request to run on compute nodes
export KMP_AFFINITY=disabled

export PDY=`date -u +%Y%m%d`
# expor PDY=20180606

export PDY1=`expr $PDY - 1`

# export cyc=06
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

export numproc=22

module list

############################################
# GFS GEMPAK META PRODUCT GENERATION
############################################

export LAUNCH_MODE=MPI

###############################################
# Set MP variables
###############################################
export OMP_NUM_THREADS=1
export MP_LABELIO=yes
export MP_PULSE=0
export MP_DEBUG_NOTIMEOUT=yes

##############################################
# Define COM, COMOUTwmo, COMIN  directories
##############################################

# set envir=prod or para to test with data in prod or para
 export envir=para
# export envir=prod

export SENDCOM=YES
export KEEPDATA=YES
export job=gfs_gempak_meta_${cyc}
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

#############################################
#set the fcst hrs for all the cycles
#############################################
export fhbeg=00
export fhend=384
export fhinc=12

#############################################################
# Specify versions
#############################################################
export gfs_ver=v15.0.0

##############################################
# Set up model and cycle specific variables
##############################################
export DBN_ALERT_TYPE=GFS_METAFILE

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
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}
export model=${model:-gfs}

##############################################
# Define COM directories
##############################################
if [ $envir = "prod" ] ; then
#  This setting is for testing with GFS (production)
  export COMIN=/gpfs/hps/nco/ops/com/nawips/prod/gfs.${PDY}
  export COMROOT=/gpfs/hps/nco/ops/com

else
  export COMIN=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1/gfs.${PDY}/${cyc}/nawips   ### EMC PARA Realtime
  export COMROOT=/gpfs/hps3/ptmp/emc.glopara/ROTDIRS/prfv3rt1                          ### EMC PARA Realtime

#  export COMIN=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn/gfs.${PDY}/${cyc}/nawips  ### Boi PARA
#  export COMROOT=/gpfs/hps3/emc/global/noscrub/Boi.Vuong/svn                         ### Boi PARA

fi

export COMINukmet=${COMINukmet:-$(compath.py nawips/prod/ukmet)}
export COMINecmwf=${COMINecmwf:-$(compath.py nawips/prod/ecmwf)}
export COMINnam=${COMINnam:-$(compath.py nawips/prod/nam)}

export COMOUT=${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}/${cyc}/nawips/meta

if [ ! -f $COMOUT ] ; then
  mkdir -p -m 775 $COMOUT
fi

#############################################
# run the GFS job
#############################################
sh $HOMEgfs/jobs/JGFS_GEMPAK_META
