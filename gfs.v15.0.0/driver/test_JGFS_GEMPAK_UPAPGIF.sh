#!/bin/sh

# LSBATCH: User input
#BSUB -J jgfs_gempak_upapgif_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gfs_gempak_upapgif_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gfs_gempak_upapgif_00.o%J
#BSUB -L /bin/sh
#BSUB -n 1
#BSUB -q debug
#BSUB -W 00:30
#BSUB -cwd /ptmpd3/Boi.Vuong/com
#BSUB -R span[ptile=1]
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
module use /nwpara2/modulefiles
module load util_shared/v1.0.3

set -x
date

############################################
# GFS GEMPAK UPAPGIF PRODUCT GENERATION
############################################

##############################################
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gfs_gempak_upapgif_${cyc}
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

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}
export NMODEL=gfs
export model=${model:-gfs}

##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/${RUN}/${envir}/${RUN}.${PDY}}
# export MAGDIR=${MAGDIR:-${COMROOT}/mag/${envir}/mag.${PDY}}

export MAGDIR=${MAGDIR:-${COMROOT2}/mag/${envir}/mag.${PDY}}

if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $MAGDIR
fi

export pgmout=OUTPUT.$$

env

export fhr=000

msg="$job HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak

########################################################
# Execute MAG upper air processing with significant and
# mandatory level data to only create the GEMPAK
# upper air file
########################################################

########################################################
# Execute the script.
export RUN2=sktgfs
$SRCgfs/exmag_sigman_skew_t.sh.ecf
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
