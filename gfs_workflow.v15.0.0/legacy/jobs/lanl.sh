#!/bin/ksh

set -xa
#
# Specify whether the run is production or development
#
export RUN_ENVIR=${RUN_ENVIR:-prod}
export DEV_SMS=${DEV_SMS:-NO}
#
if [[ $RUN_ENVIR = prod || $DEV_SMS = YES ]] ; then
   $SMSBIN/smsinit $LOADL_STEP_ID
fi

#
#  If a CONFIG file is specified through the environment, execute it
#
CONFIG=${CONFIG:-""}
if [ ! -z $CONFIG ] ; then
 if [ -s $CONFIG ] ; then set -a ; . $CONFIG ; set +a ; fi
fi

#
####################################
# Specify NET and RUN Name and model
####################################
export NET=cfs
export RUN=cdas

####################################
# set up job name and  environment
####################################
export job=${job:-$RUN}
export envir=${envir:-prod}
export host=${host:-`hostname | cut -c1`}

###############################################

if [[ $RUN_ENVIR = prod || $RUN_ENVIR = devpara ]] ; then
 export HOMEcfs=${HOMEcfs:-${BASEDIR:-/nw$envir}}
else
 HOMEcfs=${HOMEcfs:-${BASEDIR:-..}}
fi
echo ' HOMEcfs is ' $HOMEcfs

export NWPROD=${NWPROD:-$HOMEcfs}

date
export PS4='$SECONDS + '

# #############################################
# SETUP CLDAS PREP PROCESSING VARIABLES
# ##############################################

####################################
# obtain unique process id (pid) and make temp directory
####################################
export pid=${pid:-$$}
if [[ $RUN_ENVIR = prod || $RUN_ENVIR = devpara ]] then
  export DATA_IN=${DATA_IN:-/tmpnwprd}
else
  export userid=$LOGNAME
  export DATA_IN=${DATA_IN:-$PTMP/$userid}
fi

export DATA=$DATA_IN/${job}.${pid}
mkdir -p $DATA
cd $DATA

####################################
# Determine Job Output Name on System
####################################
export cyc=${cyc:-00}
export outid="LL$job"
[ $envir != prod ]  && export outid="LL${job}_${envir}"
export jobid="${outid}.o${pid}"
export pgmout="OUTPUT.${pid}"
export pgmerr=errfile
export cycle=t${cyc}z

####################################
# SENDSMS  - Flag Events on SMS
# SENDCOM  - Copy Files From TMPDIR to $COMOUT
# SENDDBN  - Issue DBNet Client Calls
# VERBOSE  - Specify Verbose Output in excldas_anl.sh.sms
####################################
if [[ $RUN_ENVIR = prod && $envir = prod ]] then
   export SENDDBN=YES
fi
export SENDSMS=${SENDSMS:-YES}
export SENDCOM=${SENDCOM:-YES}
export SENDDBN=${SENDDBN:-NO}
export VERBOSE=${VERBOSE:-YES}

#################################
# Define the Log File directory
#################################
if [ $RUN_ENVIR = prod ] ; then
  if [ $envir = prod ] ; then
    export jlogfile=${jlogfile:-/com/logs/jlogfile}
  else
    export jlogfile=${jlogfile:-/com/logs/${envir}/jlogfile}
  fi
else
    export jlogfile=$DATA/jlogfile
fi

##############################
# Set up the UTILITIES
##############################
export utilscript=${utilscript:-$NWPROD/util/ush}
export utilexec=${utilexec:-$NWPROD/util/exec}
export ushscript=${ushscript:-$NWPROD/ush}
export utilparm=${utilparm:-$NWPROD/util/parm}

# Run setup to initialize working directory and utility scripts
##############################
sh $utilscript/setup.sh

####################################
# Specify Execution Areas
####################################

export EXECcfs=${EXECcfs:-$HOMEcfs/exec}
export FIXcfs=${FIXcfs:-$HOMEcfs/fix}
export USHcfs=${USHcfs:-$HOMEcfs/ush}

err_chk=${err_chk:-$utilscript/err_chk.sh}
startmsg=${startmsg:-$utilscript/startmsg.sh}
ERRSCRIPT=${ERRSCRIPT:-$err_chk}
LOGSCRIPT=${LOGSCRIPT:-$startmsg}

#####################################
# specify user and local directories if "dev" queue
#####################################
if [ $RUN_ENVIR != prod ] ; then
 export usergroup=${usergroup:-g01}
 export userid=${userid:-$LOGNAME}
 export userdir=${userdir:-misc/cfs_v2}
fi

if [[ $RUN_ENVIR = prod ]] ; then
  export COMDSK=${COMDSK:-$PTMP}
  export COMDIR=${COMDIR:-$COMDSK/com/$NET/$envir/}
  export LOGDIR=${COMDIR:-$COMDSK/com/logs/$envir}
  export DATA=${DATA:-/tmpnwprd/$NET}
else
  export COMDSK=${COMDSK:-$PTMP}
  export version=${version:-v2}
  export COMDIR=${COMDIR:-$COMDSK/$userid/$NET/$version}
  export LOGDIR=${COMDIR:-$PTMP/$userid/$NET}
  export DATA=${DATA:-$PTMP/$userid/$NET}
fi

##############################
# Run setpdy and initialize PDY variables
##############################
sh $utilscript/setpdy.sh
. PDY

export CDATE=${CDATE:-${PDY}$cyc}
##############################################
# Define COM directories
##############################################
export COMINIC=$COMDIR/cdas.$PDYm1
export COMIN=$COMDIR/cdas.$PDY
export COMOUT=$COMDIR/cdas.$PDY
mkdir -m 775 -p $COMOUT

##############################################
# Define GES directories
##############################################
export GESdir=$COMDIR/RESTART
mkdir -m 775 -p $GESdir

export RESDIR=${RESDIR:-$GESdir}
mkdir -p $RESDIR

env

#############################################################
# execute the script

${CLDASSH:-$HOMEcfs/scripts/excfs_cdas_gldas.sh.sms}

#############################################################

echo "`hostname`  --  `date`" > $COMOUT/where_${cycle}_${model}_analysis_ran

cat $pgmout

cd $DATA_IN
#rm -rf $DATA

date

if [[ $RUN_ENVIR = prod || $DEV_SMS = YES ]] ; then
 $SMSBIN/smscomplete
else
   export CSTEP=lanl
 if [ ! -z ${PEND:-""} ] ; then $PEND ; fi
fi
