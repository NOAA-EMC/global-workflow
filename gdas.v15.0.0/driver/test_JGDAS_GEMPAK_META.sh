#!/bin/sh

# LSBATCH: User input
#BSUB -J jgdas_gempak_meta_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gdas_gempak_meta_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gdas_gempak_0meta_0.o%J
#BSUB -L /bin/sh
#BSUB -W 00:15
#BSUB -q debug
#BSUB -cwd /ptmpd3/Boi.Vuong/com
#BSUB -P GDAS-OPS
#BSUB -R rusage[mem=5000]
#BSUB -R affinity[core]

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
# GDAS GEMPAK META PRODUCT GENERATION
############################################

##############################################
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gdas_gempak_meta_${cyc}
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

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak

cp $FIXgempak/datatype.tbl datatype.tbl

###################################
# Specify NET and RUN Name and model
####################################
export RUN=${RUN:-gdas}

export DBN_ALERT_TYPE=GDAS_METAFILE

##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/nawips/${envir}/${RUN}.${PDY}}
# export COMOUT=${COMOUT:-${COMROOT}/nawips/${envir}/${RUN}.${PDY}/meta}

export COMOUT=${COMOUT:-${COMROOT2}/nawips/${envir}/${RUN}.${PDY}/meta}

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT
fi

export pgmout=OUTPUT.$$

env

########################################################
# Execute the script.
 $USHgdas/gdas_meta_na.sh
# $USHgdas/gdas_ecmwf_meta_ver.sh
# /global/save/Boi.Vuong/svn/test/gdas_ecmwf_meta_ver.sh
 $USHgdas/gdas_meta_loop.sh
# $USHgdas/gdas_ukmet_meta_ver.sh
# /global/save/Boi.Vuong/svn/test/gdas_ukmet_meta_ver.sh
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
