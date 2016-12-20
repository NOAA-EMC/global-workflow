#!/bin/sh

set -ax

#--------------------------------------------------------------------
# Set environment variables

export SUFFIX=${1:-prd12q3k}
export string=${2:-ges}
export CDATE=${3:-2011061300}
export DATDIR=${4:-$PTMP/$LOGNAME/$SUFFIX}
export TANKDIR=${5:-/global/noscrub/$LOGNAME/ozone/stats/${SUFFIX}/${string}}

tmpdir=$STMP/$LOGNAME/ozone/vrfyozn_${SUFFIX}_${string}
export stmproot=${stmproot:-$STMP}
export ptmproot=${ptmproot:-$PTMP}
export data_extract=${data_extract:-1}

export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
if [ $machine = WCOSS ]; then
#  export ptmproot=/ptmp
#  export stmproot=/stmp
   export daroot=/da
elif [ $machine = WCOSS_C ]; then
#  export ptmproot=/gpfs/hps/ptmp
#  export stmproot=/gpfs/hps/stmp
   export daroot=/gpfs/hps/emc/da
elif [ $machine = THEIA ]; then
#  export ptmproot=/scratch2/portfolios/NCEPDEV/ptmp
#  export stmproot=/scratch2/portfolios/NCEPDEV/stmp
   export daroot=/scratch4/NCEPDEV/da
fi

export LOGDIR=$PTMP/$LOGNAME/logs/ozone/${SUFFIX}/${string}
export BASEDIR_OZNMON=${BASEDIR_OZNMON:-$daroot/$LOGNAME}
export SCRIPTS=${SCRIPTS_OZNMON:-$BASEDIR_OZNMON/scripts}
export EXEDIR=${EXEDIR_OZNMON:-$BASEDIR_OZNMON/source}
export GSCRIPTS=${GSCRIPTS_OZNMON:-$BASEDIR_OZNMON/gscripts}
export LLQ=${LLQ:-bjobs}
export SUB=${SUB:-/u/wx20mi/bin/sub}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export NCP=/bin/cp
export USER=$LOGNAME
export EXTRACT=1
export PLOT=0
ACOUNT=GDAS-MTN

if [ $machine = IBMP6 ] ; then
  export COMPRESS=compress
  export UNCOMPRESS=uncompress
  export COMPRESS_FLAG=Z
else
  export COMPRESS=gzip
  export UNCOMPRESS=gunzip
  export COMPRESS_FLAG=gz
fi

mkdir -p $TANKDIR
mkdir -p $LOGDIR

rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#--------------------------------------------------------------------
# Check status of monitoring job.  Is it already running?  If so, exit
# this script and wait for job to finish.

##count1=`$LLQ | grep $LOGNAME | grep "_$SUFFIX" | grep $string | wc -l`
##count2=`$LLQ | grep $LOGNAME | grep "${SUFFIX}_" | grep $string | wc -l`
##count=` expr $count1 + $count2 `
##if [[ $count -ne 0 ]] ; then
##   cd $tmpdir
##   cd ../
##   rm -rf $tmpdir
##   exit
##fi

#--------------------------------------------------------------------
# Get date of cycle to process.

##$NCP $TANKDIR/cycle/prodate ./prodate
##export PDATE=`cat 'prodate'`

export PDATE=$CDATE

sdate=`echo $PDATE|cut -c1-8`
export CYA=`echo $PDATE|cut -c9-10`

##export DATDIR=/com/gfs/prod/gdas.${sdate}

#--------------------------------------------------------------------
# Based on cycle, turn on/off plotting.  (ALWAYS generate data files).
##if [[ "$CYA" = "00" ]];then
##   export PLOT=1
##fi

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# ozone monitoring jobs.

export list0=PDATE,TANKDIR,LOGDIR,SCRIPTS,EXEDIR,GSCRIPTS,SUB,NDATE,SUFFIX,NCP,EXTRACT,PLOT,USER,DATDIR,CYA,string,machine,COMPRESS,UNCOMPRESS,COMPRESS_FLAG,stmproot,ptmproot,daroot,ACCOUNT,data_extract,list0
if [[ -s $DATDIR/gdas1.t${CYA}z.oznstat  ]]; then
   echo 'run oznopr.sh'
   /bin/sh $SCRIPTS/oznopr.sh
elif [[ -s $DATDIR/oznstat.gdas.$PDATE ]]; then
   echo 'run oznopr.sh'
   /bin/sh $SCRIPTS/oznopr.sh
#else
#   echo 'ozone diagnostic file ' $DATDIR/oznstat.gdas.$PDATE ' does not exist'
#  hpsstar get /hpssuser/g01/wx20hl/$SUFFIX/${PDATE}gdas.tar oznstat.gdas.$PDATE
#   hpsstar get /1year/hpsspara/runhistory/glopara/$SUFFIX/${PDATE}gdas.tar oznstat.gdas.$PDATE
#   mv  oznstat.gdas.$PDATE $DATDIR/oznstat.gdas.$PDATE 
#   /bin/sh $SCRIPTS/oznopr.sh
fi

#--------------------------------------------------------------------
# Clean up and exit
 cd $tmpdir
 cd ../
 rm -rf $tmpdir

exit
