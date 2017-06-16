#!/bin/ksh

set -ux

set -a; . $CONFIG; set +a

export RUN_ENVIR=para
export envir=prod
export DBNROOT=DBNROOT_OFF
export job=gfs_gempak_meta
export pid=$$
export DATAROOT=$STMP/$LOGNAME
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export numproc=$LSB_DJOB_NUMPROC

export SENDCOM=YES
export SENDDBN=NO
export SENDECF=NO

export gfs_ver=v12.0.8
export util_ver=v1.0.0

export COM_IN=$COMROT/nawips/gfs.$PDY
export HPCGFS=$COM_IN
export COM_OUT=$COM_IN/meta
export jlogfile=$COM_OUT/jlogfile.${job}.${pid}


########################################
# Creates GFS META files for NAWIPS
########################################

export LAUNCH_MODE=MPI

###############################################
# Set MP variables 
###############################################
export OMP_NUM_THREADS=1
export MP_LABELIO=yes
export MP_PULSE=0
export MP_DEBUG_NOTIMEOUT=yes

set -xa
# #### 08/25/1999 ###################
# SET SHELL PROCESSING VARIABLES
# ###################################
export PS4='$SECONDS + ' 
date
# 
# obtain unique process id (pid) and make temp directories
#
export pid=$$
export DATA=$DATAROOT/${job}.${pid}
mkdir $DATA
cd $DATA 

####################################
# File To Log Msgs
####################################
if [ $envir != prod ]; then
  export jlogfile=${jlogfile:-/com/logs/${envir}/jlogfile}
  export DBNROOT=/nwprod/spa_util/fakedbn
fi

export jlogfile=${jlogfile:-/com/logs/jlogfiles/jlogfile.${job}.${pid}}

#############################################
#set the fcst hrs for all the cycles
#############################################
export fhbeg=00
export fhend=384
export fhinc=12

####################################
# Determine Job Output Name on System
####################################
export outid="LL$job"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"

export cycle=t${cyc}z 

export SENDCOM=${SENDCOM:-YES}
if [ "$DBNROOT" = "DBNROOT_OFF" ]; then
  export SENDDBN=NO
else
  export SENDDBN=${SENDDBN:-YES}
fi
export SENDECF=${SENDECF:-YES}

#
# Set up model and cycle specific variables
#
export NET=gfs
export RUN=gfs
export DBN_ALERT_TYPE=GFS_METAFILE

export HOMEgfs=${HOMEgfs:-/nwprod/gfs.${gfs_ver}}
##export USHgfs=${USHgfs:-$HOMEgfs/gempak/ush/gfs}
export USHgfs=${USHgfs:-$BASEDIR/gempak/ush/gfs}
export FIXgfs=${FIXgfs:-$HOMEgfs/gempak/fix}
export FIXgempak=${FIXgempak:-$HOMEgfs/gempak/fix}

export HOMEutil=/nwprod/util.${util_ver}

export HOMEgempak=${HOMEgempak:-/nwprod/gempak}
export USHgempak=${USHgempak:-$HOMEgempak/ush}

#
# Now set up GEMPAK/NTRANS environment
#
. /nwprod/gempak/.gempak

 /nwprod/gempak/fix/datatype.tbl datatype.tbl

###################################
# Set up the UTILITIES
###################################
export HOMEutil=${HOMEutil:-/nw${envir}/util.${util_ver}}
export utilscript=${utilscript:-$HOMEutil/ush}
export utilities=${utilities:-$HOMEutil/ush}
export utilexec=${utilexec:-$HOMEutil/exec}

# Run setup to initialize working directory and utility scripts
$utilscript/setup.sh
# Run setpdy and initialize PDY variables
$utilscript/setpdy.sh
. PDY

export COM_IN=${COM_IN:-/com/nawips/${envir}/${RUN}.${PDY}}
export COM_OUT=${COM_OUT:-/com/nawips/${envir}/${RUN}.${PDY}/meta}
export COMIN=${COMIN:-${COM_IN}}
export COMOUT=${COMOUT:-${COM_OUT}}

if [ ! -d $COMOUT ] ; then
  mkdir -p -m 775 $COMOUT
fi
 
env

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

########################################################
# Execute the script.
${GFSMETASH:-$HOMEgfs/scripts/exgfs_gempak_meta.sh.ecf}
########################################################

msg="job has ended"
postmsg "$jlogfile" "$msg"

cat $pgmout

##cd /tmpnwprd1
##rm -rf $DATA

date
