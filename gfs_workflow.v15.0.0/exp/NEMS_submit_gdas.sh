#!/bin/sh
set -x
curdir=`pwd`

CONFIG=para_config
CDUMP=gdas  
cdate=2015060100
cstep=fcst1
psub=/global/save/Fanglin.Yang/svn/gfs/tags/gfs_workflow.v2.0.0/para/bin/psub
hpsstar=/nwprod/util/ush/hpsstar                 

###-------------------------------------------------------- 
export PSLOT=nems2s             ;#name of current experiment
export ROTDIR=/ptmpd3/$LOGNAME/pr${PSLOT}           
mkdir -p $ROTDIR
cd $ROTDIR

# start from parallel runs
HPSS_ARCH=/NCEPDEV/emc-global/5year/emc.glopara/WCOSS
expt=pr4devbs15   
exptdir=/ptmpd3/emc.glopara/$expt

#----------------------------
#----------------------------
#for file in  biascr.${CDUMP}.${cdate}  biascr_pc.${CDUMP}.${cdate} siganl.${CDUMP}.${cdate} sfcanl.${CDUMP}.${cdate}  radstat.${CDUMP}.${cdate} aircraft_t_bias.${CDUMP}.${cdate} ;do
# cp $exptdir/$file .
#done
#cp -p $exptdir/siganl_${cdate}_* .
#cp -p $exptdir/sfcanl_${cdate}_* .
#
#$hpsstar get $HPSS_ARCH/$expt/${cdate}${CDUMP}.tar biascr.${CDUMP}.${cdate}  biascr_pc.${CDUMP}.${cdate} siganl.${CDUMP}.${cdate} sfcanl.${CDUMP}.${cdate}  radstat.${CDUMP}.${cdate} aircraft_t_bias.${CDUMP}.${cdate}
#$hpsstar get $HPSS_ARCH/$expt/${cdate}${CDUMP}.enkf.anl.tar  
#----------------------------
#----------------------------

#----------------------------
#---for running NEMS GFS
#----------------------------
rc=0
ICDIR=/global/save/Fanglin.Yang/NEMS/ICs/nemsio_$expt
for file in biascr.${CDUMP}.${cdate}  biascr_pc.${CDUMP}.${cdate}  radstat.${CDUMP}.${cdate} aircraft_t_bias.${CDUMP}.${cdate} gfnanl.${CDUMP}.${cdate} sfnanl.${CDUMP}.${cdate}  ;do
 cp $ICDIR/$file .
 if [ $? -ne 0 ]; then rc=1 ;fi
done
if [ $rc -ne 0 ]; then
 $hpsstar get $HPSS_ARCH/$expt/${cdate}${CDUMP}.tar biascr.${CDUMP}.${cdate}  biascr_pc.${CDUMP}.${cdate} radstat.${CDUMP}.${cdate} aircraft_t_bias.${CDUMP}.${cdate}
 rc=$?
fi

cp $ICDIR/siganl_${cdate}_mem* .    
cp $ICDIR/sfcanl_${cdate}_mem* .    

nsig=`ls -l siganl_${cdate}_mem* |wc -l `
nsfc=`ls -l sfcanl_${cdate}_mem* |wc -l `

cd $curdir
if [ $rc -eq 0 -a $nsig -eq 80 -a $nsfc -eq 80 ]; then 
 $psub $CONFIG $cdate $CDUMP efmn   
 $psub $CONFIG $cdate $CDUMP $cstep
fi


exit








