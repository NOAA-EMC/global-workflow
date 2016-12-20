#!/bin/sh

# LSBATCH: User input
#BSUB -J jgfs_gempak_metad_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gfs_gempak_meta_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gfs_gempak__meta00.o%J
#BSUB -L /bin/sh
#BSUB -n 1
#BSUB -q debug
#BSUB -W 03:00
#BSUB -R "span[ptile=8]"
#BSUB -cwd /ptmpd3/Boi.Vuong/com
#BSUB -P GFS-T2O
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

set -x
date

export numproc=25

set -xa
export PS4='$SECONDS + '
date

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
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gfs_gempak_meta_${cyc}
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

###########################################
# Now set up GEMPAK/NTRANS environment
###########################################
module load gempak

cp  $FIXgfs/datatype.tbl datatype.tbl

#############################################
#set the fcst hrs for all the cycles
#############################################
export fhbeg=00
export fhend=384
export fhinc=12

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}
export model=${model:-gfs}

#
# Set up model and cycle specific variables
#
export DBN_ALERT_TYPE=GFS_METAFILE

##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/nawips/${envir}/${RUN}.${PDY}}

export COMOUT=${COMOUT:-${COMROOT2}/nawips/${envir}/${RUN}.${PDY}/meta}

if [ ! -f $COMOUT ] ; then
  mkdir -p -m 775 $COMOUT
fi

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

export pgmout=OUTPUT.$$

env

########################################################
# Execute the script.
# $SRCgfs/exgfs_gempak_meta.sh.ecf
$dir/exgfs_gempak_meta.sh.ecf.test
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
