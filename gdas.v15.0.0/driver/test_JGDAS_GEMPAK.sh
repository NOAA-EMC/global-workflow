#!/bin/sh

# LSBATCH: User input
#BSUB -J jgdas_gempak_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gdas_gempak_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gdas_gempak_00.o%J
#BSUB -L /bin/sh
#BSUB -n 2
#BSUB -R rusage[mem=2000]; -R span[ptile=2]
#BSUB -q debug
#BSUB -W 00:35
#BSUB -cwd /ptmpd3/Boi.Vuong/com
#BSUB -P GDAS-OPS
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
module load grib_util/v1.0.1

set -xa
export PS4='$SECONDS + '
date

############################################
# GDAS GEMPAK PRODUCT GENERATION
############################################

##############################################
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gdas_gempak_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=/ptmpd3/$LOGNAME/test
export NWROOT=/global/save/Boi.Vuong/svn
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
##########################################################
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
export HOMEgdas=${HOMEgdas:-${NWROOT}/gdas.${gdas_ver}}
export EXECgdas=${EXECgdas:-$HOMEgdas/exec}
export PARMgdas=${PARMgdas:-$HOMEgdas/parm}
export FIXgdas=${FIXgdas:-$HOMEgdas/gempak/fix}
export USHgdas=${USHgdas:-$HOMEgdas/gempak/ush}
export SRCgdas=${SRCgdas:-$HOMEgdas/scripts}

######################################
# Set up the GEMPAK directory
#######################################
export HOMEgempak=${HOMEgempak:-${NWROOTp1}/gempak}
export FIXgempak=${FIXgempak:-$HOMEgempak/fix}
export USHgempak=${USHgempak:-$HOMEgempak/ush}

#
# Set up model and cycle specific variables
#

export fend=09
export finc=3
export fstart=00
export GRIB=pgrb2f
export EXT=""
export DBN_ALERT_TYPE=GDAS_GEMPAK

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}
export model=${model:-gdas1}

##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}}
# export COMOUT=${COMOUT:-${COMROOT}/nawips/${envir}/${RUN}.${PDY}}

export COMOUT=${COMOUT:-${COMROOT2}/nawips/${envir}/${RUN}.${PDY}}


if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT
fi

export pgmout=OUTPUT.$$

env

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak

########################################################
# Execute the script.
# echo "$SRCgdas/exgdas_nawips.sh.ecf gdas 009 GDAS_GEMPAK" >>poescript
$SRCgdas/exgdas_nawips.sh.ecf gdas 009 GDAS_GEMPAK
# $dir/exgdas_nawips.sh.ecf.test gdas 009 GDAS_GEMPAK
########################################################

########################################################
# Execute the script for quater-degree grib
# echo "SRCgdas/exgdas_nawips.sh.ecf gdas_0p25 009 GDAS_GEMPAK" >>poescript
SRCgdas/exgdas_nawips.sh.ecf gdas_0p25 009 GDAS_GEMPAK
# $dir/exgdas_nawips.sh.ecf.test gdas_0p25 009 GDAS_GEMPAK
########################################################

# chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript

# Execute the script.

# mpirun.lsf

export err=$?; err_chk
########################################################

msg="JOB $job HAS COMPLETED NORMALLY!"
postmsg $jlogfile "$msg"

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
