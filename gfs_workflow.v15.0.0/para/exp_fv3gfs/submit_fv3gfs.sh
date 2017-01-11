#!/bin/sh
#BSUB -L /bin/sh
#BSUB -P FV3GFS-T2O
#BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/fv3gfs_fcst.out
#BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/fv3gfs_fcst.out
#BSUB -J fv3gfs   
#BSUB -q dev
#BSUB -M 1024
##BSUB -x
#BSUB -W 10:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang
#BSUB -extsched 'CRAYLINUX[]'
set -x

#--------------------------------------------------------------------------------
# This script is used to create FV3 initial conditions using operational GFS ICs
# then run forecast-only FV3GFS experiments for multiple cases.
# December 2016, Fanglin Yang
#--------------------------------------------------------------------------------

export PSLOT=test          
paradir=/gpfs/hps/emc/global/noscrub/$LOGNAME/para_gfs/pr$PSLOT 

. $MODULESHOME/init/sh  2>>/dev/null
module load prod_util hpss >>/dev/null
export NODES=1

export workflow_ver=v15.0.0
export global_shared_ver=v15.0.0
export tags=trunk_r86472
export PTMP=/gpfs/hps/ptmp
export BASE_SVN=/gpfs/hps/emc/global/noscrub/$LOGNAME/svn
export BASEDIR=$BASE_SVN/fv3gfs/$tags/gfs_workflow.$workflow_ver/para     
export BASE_GSM=$BASE_SVN/fv3gfs/$tags/global_shared.$global_shared_ver
export PSUB=$BASEDIR/bin/psub
export SUB=$BASEDIR/bin/sub_wcoss_c
export HPSSTAR=/u/emc.glopara/bin/hpsstar

export CASE=C96
export res=`echo $CASE|cut -c 2-`
export ROTDIR=/gpfs/hps/ptmp/$LOGNAME/pr$PSLOT    
export FV3ICDIR=/gpfs/hps/ptmp/$LOGNAME/FV3IC       
mkdir -p $ROTDIR $FV3ICDIR

START=20170109
LAST=20170109

cyclist="00"
NDAT=1      ;##how many forecast only jobs to run at one time.

export sdate=${sdate:-$START}
export edate=`echo $($NDATE +$(expr ${NDAT} \* 24) ${sdate}00 ) |cut -c 1-8`
#--------------------------------------------------------------
while [ $sdate -lt $edate ]; do
for cyc in $cyclist; do
#--------------------------------------------------------------
export cdate=${sdate}${cyc}

#-- create ICs using operational GFS initial conditions
export out_dir=$FV3ICDIR/ICs                      
export tmp_dir=$FV3ICDIR/chgres_${CASE}_${cdate}
export inidir=$tmp_dir
if [ -s $tmp_dir ]; then rm -f $tmp_dir ; fi
mkdir -p $out_dir $tmp_dir ; cd $tmp_dir ||exit 8

#--start from operational GFS initial conditions
yymmdd=`echo $cdate |cut -c 1-8`
yymm=`echo $cdate |cut -c 1-6`
yy=`echo $cdate |cut -c 1-4`

d1=td1
#chost=`echo $(hostname) |cut -c 1-1`
#if [ $chost = l ]; then d1=td1; fi
#if [ $chost = s ]; then d1=gd1; fi

#------------------------------------------------------
#-- copy over pgbanl for verification
cp /gpfs/$d1/emc/global/noscrub/emc.glopara/global/gfs/pgbanl.gfs.$cdate $ROTDIR/.
cp $COMROOTp2/gfs/prod/gfs.$yymmdd/gfs.t${cyc}z.sanl $tmp_dir/siganl.gfs.$cdate
cp $COMROOTp2/gfs/prod/gfs.$yymmdd/gfs.t${cyc}z.sfcanl $tmp_dir/sfcanl.gfs.$cdate

#---------------------
if [ $? -ne 0 ]; then
#---------------------
cat >read_hpss.sh <<EOF
 cd $tmp_dir
 $HPSSTAR get /NCEPPROD/hpssprod/runhistory/rh$yy/$yymm/$yymmdd/com2_gfs_prod_gfs.$cdate.anl.tar ./gfs.t${cyc}z.sanl ./gfs.t${cyc}z.sfcanl
 mv gfs.t${cyc}z.sanl siganl.gfs.$cdate
 mv gfs.t${cyc}z.sfcanl sfcanl.gfs.$cdate
EOF
chmod u+x read_hpss.sh
$SUB -a FV3GFS-T2O -q dev_transfer -p 1/1/S -r 1024/1/1 -t 2:00:00 -j read_hpss -o read_hpss.out read_hpss.sh    
#---------------------
fi
#---------------------

testfile=$tmp_dir/sfcanl.gfs.$cdate                       
nsleep=0; tsleep=120;  msleep=50 
while test ! -s $testfile -a $nsleep -lt $msleep;do
  sleep $tsleep; nsleep=`expr $nsleep + 1`
done
#------------------------------------------------------

$BASE_GSM/ush/fv3gfs_driver_chgres.sh

cd $paradir
$PSUB para_config $cdate gfs fcst1
sleep 60

#----------------------------------------------------
done   ;#cycle
sdate=`echo $($NDATE +24 ${sdate}00 ) |cut -c 1-8`
done
#----------------------------------------------------

## wait for 6 hours before submitting next bunch of forecast cases.
export sdate=$edate
if [ $sdate -le $LAST ]; then
$SUB -e sdate -a FV3GFS-T2O -q dev -p 1/1/N -r 3072/1/1 -t 10:00:00 \
    -w "+0200" -j fv3gfs -o fv3gfs_fcst.out submit_fv3gfs.sh
fi

exit
