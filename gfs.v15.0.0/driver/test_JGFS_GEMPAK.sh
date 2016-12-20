#!/bin/sh

# LSBATCH: User input
#BSUB -J jgfs_gempak_00
#BSUB -o /ptmpd3/Boi.Vuong/com/gfs_gempak_00.o%J
#BSUB -e /ptmpd3/Boi.Vuong/com/gfs_gempak_00.o%J
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
export FOR_DISABLE_STACK_TRACE=true

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

set -x
date

export fhour=000

set -xa
export PS4='$SECONDS + '
date

############################################
# GFS GEMPAK PRODUCT GENERATION
############################################

##############################################
# Define COM, PCOM, COMIN  directories
##############################################
export dir=` pwd `
export envir=prod
export SENDDBN=NO
export job=gfs_gempak_${cyc}
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

# For half-degree P Grib files
export DO_HD_PGRB=${DO_HD_PGRB:-YES}

#
# Set up model and cycle specific variables
#
export finc=${finc:-3}
export fstart=${fstart:-0}
export model=${model:-gfs}
export GRIB=${GRIB:-pgrb2f}
export EXT=""
export DBN_ALERT_TYPE=${DBN_ALERT_TYPE:-GFS_GEMPAK}

###################################
# Specify NET and RUN Name and model
####################################
export NET=${NET:-gfs}
export RUN=${RUN:-gfs}
export model=${model:-gfs}

##############################################
# Define COM directories
##############################################
export COM_IN=${COM_IN:-${COMROOT}/${NET}/${envir}}
export COM_OUT=${COM_OUT:-${COMROOT}/nawips/${envir}}
export COMIN=${COMIN:-${COM_IN}/${RUN}.${PDY}}
# export COMOUT=${COMOUT:-${COM_OUT}/${RUN}.${PDY}}

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

#################################################################
# Execute the script for the 384 hour 1 degree grib

# echo "$SRCgfs/exgfs_nawips.sh.ecf gfs 384 GFS_GEMPAK" >>poescript
$SRCgfs/exgfs_nawips.sh.ecf gfs 384 GFS_GEMPAK
#  $dir/exgfs_nawips.sh.ecf.test gfs $fhour GFS_GEMPAK
##################################################################

#################################################################
# Execute the script for the half-degree grib

# echo "$SRCgfs/exgfs_nawips.sh.ecf gfs_0p50 384 GFS_GEMPAK" >>poescript
# $dir/exgfs_nawips.sh.ecf.test gfs_0p50 $fhour GFS_GEMPAK
##################################################################

#################################################################
# Execute the script for the quater-degree grib

# echo "$SRCgfs/exgfs_nawips.sh.ecf gfs_0p25 384 GFS_GEMPAK" >>poescript
# $dir/exgfs_nawips.sh.ecf.test gfs_0p25 $fhour GFS_GEMPAK
####################################################################

####################################################################
# Execute the script to create the 35km Pacific grids for OPC

# echo "$SRCgfs/exgfs_nawips.sh.ecf gfs35_pac 180 GFS_GEMPAK_WWB" >>poescript
# $dir/exgfs_nawips.sh.ecf.test gfs35_pac $fhour GFS_GEMPAK
#####################################################################

####################################################################
# Execute the script to create the 35km Atlantic grids for OPC

# echo "$SRCgfs/exgfs_nawips.sh.ecf gfs35_atl 180 GFS_GEMPAK_WWB" >>poescript
# $dir/exgfs_nawips.sh.ecf.test gfs35_atl $fhour GFS_GEMPAK
#####################################################################

#####################################################################
# Execute the script to create the 40km grids for HPC

# echo "$SRCgfs/exgfs_nawips.sh.ecf gfs40 180 GFS_GEMPAK_WWB" >>poescript
# $dir/exgfs_nawips.sh.ecf.test gfs40 $fhour GFS_GEMPAK
######################################################################

# cat poescript

# chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript

# Execute the script.

# mpirun.lsf

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
