#!/bin/sh


# LSBATCH: User input
#BSUB -J jgfs_pgrb2_f00_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gfs_pgrb2_f00_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gfs_pgrb2_f00_00.o%J
#BSUB -L /bin/sh
#BSUB -n 5
#BSUB -q debug
#BSUB -W 00:30
#BSUB -cwd /ptmpd3/Boi.Vuong/com
#BSUB -P GFS-T2O
#BSUB -R "span[ptile=5]"
#BSUB -R affinity[core:distribute=balance]
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

########################################
# GFS_PGRB2 PRODUCT GENERATION
########################################

export post_times=03

##############################################
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gfs_pgrb2_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export DATAROOT=/ptmpd3/$LOGNAME/test
export NWROOT=/global/save/Boi.Vuong/svn
export NWROOTp1=/nwprod
export COMROOT=/com
export COMROOT2=/ptmpd3/$LOGNAME/com

export PCOMROOT2=/ptmpd3/$LOGNAME/pcom/${envir}

mkdir -m 775 -p ${COMROOT2}/logs ${COMROOT2}/logs/jlogfiles $PCOMROOT2
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
export USHgfs=${USHgfs:-$HOMEgfs/ush}
export EXECgfs=${EXECgfs:-$HOMEgfs/exec}
export PARMgfs=${PARMgfs:-$HOMEgfs/parm}
export FIXgfs=${FIXgfs:-$HOMEgfs/fix}

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}
export model=${model:-gfs}

##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}}
# export COMOUT=${COMOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}}
# export PCOM=${PCOM:-${PCOMROOT}/${NET}}

export COMOUT=${COMOUT:-${COMROOT2}/${NET}/${envir}/${RUN}.${PDY}}
export PCOM=${PCOM:-${PCOMROOT2}/${NET}}
if [ $SENDCOM = YES ] ; then
  mkdir -m 775 -p $COMOUT $PCOM
fi

export pgmout=OUTPUT.$$

env

#######################################
# Specify Restart File Name to Key Off
#######################################
restart_file=$COMIN/${RUN}.t${cyc}z.master.grb2if

####################################
# Specify Timeout Behavior of Post
#
# SLEEP_TIME - Amount of time to wait for
#              a restart file before exiting
# SLEEP_INT  - Amount of time to wait between
#              checking for restart files
####################################
export SLEEP_TIME=900
export SLEEP_INT=5

#############################################################
# Execute the script
$HOMEgfs/scripts/exglobal_pgrb2_gfs_g2_poe.sh.ecf
# $dir/exglobal_pgrb2_gfs_g2_poe.sh.ecf
export err=$?;err_chk
#############################################################

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
