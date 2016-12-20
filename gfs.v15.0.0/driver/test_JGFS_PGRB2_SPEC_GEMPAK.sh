#!/bin/sh

# LSBATCH: User input
#BSUB -J jgfs_pgrb2_spec_gempak_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gfs_pgrb2_spec_gempak_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gfs_pgrb2_spec_gempak_00.o%J
#BSUB -L /bin/sh
#BSUB -n 1
#BSUB -q debug
#BSUB -W 02:00
#BSUB -cwd /ptmpd3/Boi.Vuong/com
#BSUB -P GFS-T2O
#BSUB -R rusage[mem=5000]
#BSUB -R affinity[core]
#BSUB -a poe

export OMP_NUM_THREADS=1
export MP_MPILIB=mpich2
export MP_EUILIB=us
export MP_LABELIO=yes
export MP_COMPILER=intel

export PDY=20160225
export PDY1=`expr $PDY - 1`

export cyc=00
export cycle=t${cyc}z

set -xa
export PS4='$SECONDS + '
date

############################################
# GFS FBWIND PRODUCT GENERATION
############################################

####################################
##  Load the GRIB Utilities module
#####################################

#%include <head.h>
#%include <envir-p2.h>

. /usrx/local/Modules/default/init/ksh
module load ibmpe ics lsf

module load prod_util/v1.0.2

set -xa
export PS4='$SECONDS + '
date

############################################
# GFS_PGRB2_SPEC_GEMPAK PRODUCT GENERATION
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
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gfs_pgrb2_spec_gempak_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=/ptmpd3/$LOGNAME/test
export NWROOT=/global/save/Boi.Vuong/svn
export test=/global/save/Boi.Vuong/svn/test_gfs16
export NWROOTp1=/nwprod
export COMROOT=/com
export COMROOT2=/ptmpd3/$LOGNAME/com

mkdir -m 775 -p ${COMROOT2}/logs ${COMROOT2}/logs/jlogfiles
export jlogfile=${COMROOT2}/logs/jlogfiles/jlogfile.${jobid}

#############################################################
# Specify versions
#############################################################
export gdas_ver=v13.0.0
export gfs_ver=v13.0.0
export util_ver=v1.0.2

##########################################################
# obtain unique process id (pid) and make temp directory
#########################################################
export DATA=${DATA:-${DATAROOT}/${jobid}}
mkdir -p $DATA
cd $DATA

######################################
# Set up the cycle variable
######################################
export cycle=${cycle:-t${cyc}z}

###########################################
# Run setpdy and initialize PDY variables
###########################################
setpdy.sh
. PDY

############################################
# SENDCOM=YES--Copy output file to /com
# SENDECF=YES--Allow to talk back to ECF
# SENDDBN=YES--Alert output file to TOC
# KEEPDATA=NO--Remove temporary working
############################################
export SENDCOM=${SENDCOM:-YES}
export SENDDBN=${SENDDBN:-YES}
export SENDECF=${SENDECF:-YES}

################################
# Set up the HOME directory
################################
export HOMEgfs=${HOMEgfs:-${NWROOT}/gfs.${gfs_ver}}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush}
export SRCgfs=${SRCgfs:-$HOMEgfs/scripts}

######################################
# Set up the GEMPAK directory
#######################################
export HOMEgempak=${HOMEgempak:-${NWROOTp1}/gempak}
export FIXgempak=${FIXgempak:-$HOMEgempak/fix}
export USHgempak=${USHgempak:-$HOMEgempak/ush}

# For half-degree P Grib files
#export DO_HD_PGRB=YES

###################################
# Specify NET and RUN Name and model
####################################
export NET=gfs
export RUN=gfs_goessim
export finc=3
export model=gfs
export EXT=""

##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${NET}.${PDY}}
# export COMOUT=${COMOUT:-${COMROOT}/nawips/${envir}/${NET}.${PDY}}

export COMOUT=${COMOUT:-${COMROOT2}/nawips/${envir}/${NET}.${PDY}}
if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT
fi

env

export DATA_HOLD=$DATA

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak

#################################################################
# Execute the script for the regular grib
#################################################################

export DATA=$DATA_HOLD/SPECIAL
mkdir -p $DATA
cd $DATA

export DBN_ALERT_TYPE=GFS_GOESSIM_GEMPAK
export GRIB=goessimpgrb2.1p00.f
export EXT=" "
export fend=180
export finc=3
export fstart=0

echo "RUNS the Program"
set -xa

########################################################
# Execute the script.
$SRCgfs/exgoes_nawips.sh.ecf

#################################################################
# Execute the script for the 221 grib

export DATA=$DATA_HOLD/SPECIAL221
mkdir -p $DATA
cd $DATA

export DBN_ALERT_TYPE=GFS_GOESSIM221_GEMPAK
export RUN=gfs_goessim221
export GRIB=goessimpgrb2f
export EXT=".grd221"
export fend=180
export finc=3
export fstart=0

echo "RUNS the Program"
set -xa

########################################################
# Execute the script.
$SRCgfs/exgoes_nawips.sh.ecf
export err=$?; err_chk
########################################################

msg="JOB $job HAS COMPLETED NORMALLY!"
postmsg $jlogfile "$msg"

echo "end of program"
cd $DATA_HOLD
echo "######################################"
echo "  SPECIAL.OUT "
echo "######################################"
# cat $DATA_HOLD/special.out

############################################
# print exec I/O output
############################################
if [ -e "$pgmout" ] ; then
  cat $pgmout
fi

###################################
# Remove temp directories
###################################
if [ "$KEEPDATA" != "YES" ] ; then
  rm -rf $DATA
fi

date
