#!/bin/sh
set -x
curdir=`pwd`

machine=WCOSS_C   ;##WCOSS or WCOSS_C

CONFIG=para_config
CDUMP=gfs  
cdate=2016100300
cstep=fcst1
if [ $machine = WCOSS ]; then
 psub=/global/save/emc.glopara/svn/gfs/q3fy17/gfs_workflow.v14.1.0/para/bin/psub
elif [ $machine = WCOSS_C ]; then
 #psub=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17/gfs_workflow.v14.1.0/para/bin/psub
 psub=/gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/gfs/fv3gfs/gfs_workflow.v15.0.0/para/bin/psub 
fi

###-------------------------------------------------------- 
export PSLOT=fv3gfs             ;#name of current experiment
if [ $machine = WCOSS ]; then
 export ROTDIR=/gpfs/td3/ptmp/$LOGNAME/pr${PSLOT}           
elif [ $machine = WCOSS_C ]; then
 export ROTDIR=/gpfs/hps/ptmp/$LOGNAME/pr${PSLOT}  
fi
mkdir -p $ROTDIR
cd $ROTDIR


#-------------------------------------
# start from parallel runs
#-------------------------------------
expt=prnemsrn     
exptdir=/gpfs/hps/ptmp/emc.glopara/$expt
#for file in  gfnanl.${CDUMP}.${cdate} sfnanl.${CDUMP}.${cdate} nsnanl.${CDUMP}.${cdate} ;do
# cp $exptdir/$file .
#done

cd $curdir
$psub $CONFIG $cdate $CDUMP $cstep

exit








