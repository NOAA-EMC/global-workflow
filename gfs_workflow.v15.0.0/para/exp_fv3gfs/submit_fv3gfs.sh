#!/bin/sh
#BSUB -L /bin/sh
#BSUB -P FV3GFS-T2O
#BSUB -oo fv3test.out
#BSUB -eo fv3test.out
#BSUB -J fv3test   
#BSUB -q dev
#BSUB -M 1024
##BSUB -x
#BSUB -W 10:00
#BSUB -extsched 'CRAYLINUX[]'
set -x

#--------------------------------------------------------------------------------
# This script is used to create FV3 initial conditions using operational GFS ICs
# then run forecast-only FV3GFS experiments for multiple cases.
# December 2016, Fanglin Yang
#--------------------------------------------------------------------------------

export PSLOT=fv3test          
paradir=/gpfs/hps/emc/global/noscrub/$LOGNAME/para_gfs/pr$PSLOT 

. $MODULESHOME/init/sh  2>>/dev/null
module load prod_util hpss >>/dev/null
export NODES=1

export workflow_ver=v15.0.0
export global_shared_ver=v15.0.0
export tags=trunk_r89554     
export PTMP=/gpfs/hps/ptmp
#export BASE_SVN=/gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs
export BASE_SVN=/gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/fv3gfs
export BASEDIR=$BASE_SVN/$tags/gfs_workflow.$workflow_ver/para     
export BASE_GSM=$BASE_SVN/$tags/global_shared.$global_shared_ver
export PSUB=$BASEDIR/bin/psub
export SUB=$BASEDIR/bin/sub_wcoss_c
export HPSSTAR=/u/emc.glopara/bin/hpsstar

export CASE=C96
export res=`echo $CASE|cut -c 2-`

export ictype=nemsgfs                   #initial condition from nemsgfs or opsgfs
export nemsgfs_exp=prnemsrn            #real-time nems gfs parallel

envir=para
NET=gfs
ROTDIR=/gpfs/hps/ptmp/$LOGNAME/pr${PSLOT}
export FV3ICDIR=/gpfs/hps/ptmp/$LOGNAME/FV3IC       
mkdir -p $ROTDIR $FV3ICDIR

CDUMP=gfs
START=20170426 
LAST=20170426

cyclist="00"
NDAT=1      ;##how many forecast only jobs to run at one time.

export sdate=${sdate:-$START}
export edate=`echo $($NDATE +$(expr ${NDAT} \* 24) ${sdate}00 ) |cut -c 1-8`
#--------------------------------------------------------------
while [ $sdate -lt $edate ]; do
for cyc in $cyclist; do
#--------------------------------------------------------------
export cdate=${sdate}${cyc}

#.......................................................
#-- create ICs using operational GFS initial conditions
export out_dir=$FV3ICDIR/ICs                      
if [ ! -s $out_dir/${CASE}_${cdate}/gfs_data.tile6.nc ]; then
#.......................................................

export inidir=$FV3ICDIR/chgres_${CASE}_${cdate}
export rundir=$FV3ICDIR/chgres_${CASE}_${cdate}
export tmp_dir=$inidir
if [ -s $inidir ]; then rm -rf $inidir ; fi
mkdir -p $out_dir $inidir ; cd $inidir ||exit 8

#--start from operational GFS initial conditions
yymmdd=`echo $cdate |cut -c 1-8`
yymm=`echo $cdate |cut -c 1-6`
yy=`echo $cdate |cut -c 1-4`

dev=$(cat /etc/dev)
if [ $dev = luna ]; then d1=td1; fi
if [ $dev = surge ]; then d1=gd1; fi

COMROT=$ROTDIR/${CDUMP}.${yymmdd}/${cyc}
if [ ! -d $COMROT ]; then mkdir -p $COMROT; fi

#******************************
if [ $ictype = opsgfs ]; then
#******************************
cp /gpfs/$d1/emc/global/noscrub/emc.glopara/global/gfs/pgbanl.gfs.$cdate $COMROT/gfs.t${cyc}z.pgrbanl
cp $COMROOTp2/gfs/prod/gfs.$yymmdd/gfs.t${cyc}z.sanl $inidir/siganl.gfs.$cdate
cp $COMROOTp2/gfs/prod/gfs.$yymmdd/gfs.t${cyc}z.sfcanl $inidir/sfcanl.gfs.$cdate

#---------------------
if [ $? -ne 0 ]; then
cat >read_hpss.sh <<EOF
 cd $inidir
 $HPSSTAR get /NCEPPROD/hpssprod/runhistory/rh$yy/$yymm/$yymmdd/com2_gfs_prod_gfs.$cdate.anl.tar ./gfs.t${cyc}z.sanl ./gfs.t${cyc}z.sfcanl
 mv gfs.t${cyc}z.sanl siganl.gfs.$cdate
 mv gfs.t${cyc}z.sfcanl sfcanl.gfs.$cdate
EOF
chmod u+x read_hpss.sh
$SUB -a FV3GFS-T2O -q dev_transfer -p 1/1/S -r 1024/1/1 -t 2:00:00 -j read_hpss -o read_hpss.out read_hpss.sh    
fi
#---------------------

testfile=$inidir/sfcanl.gfs.$cdate                       
nsleep=0; tsleep=120;  msleep=50 
while test ! -s $testfile -a $nsleep -lt $msleep;do
  sleep $tsleep; nsleep=`expr $nsleep + 1`
done

#********************************
elif [ $ictype = nemsgfs ]; then
#********************************

expdir=/gpfs/hps/ptmp/emc.glopara/$nemsgfs_exp
arcdir=/gpfs/hps/emc/global/noscrub/emc.glopara/archive/$nemsgfs_exp

cp $arcdir/pgbanl.gfs.$cdate $COMROT/gfs.t${cyc}z.pgrbanl
cp $expdir/gfnanl.gfs.$cdate $inidir/.                 
cp $expdir/sfnanl.gfs.$cdate $inidir/.                 
cp $expdir/nsnanl.gfs.$cdate $inidir/.                 

#---------------------
if [ $? -ne 0 ]; then
cat >read_hpss.sh <<EOF
 cd $inidir
 $HPSSTAR get /5year/NCEPDEV/emc-global/emc.glopara/WCOSS_C/$nemsgfs_exp/${cdate}gfs.tar gfnanl.gfs.$cdate sfnanl.gfs.$cdate nsnanl.gfs.$cdate 
EOF
chmod u+x read_hpss.sh
$SUB -a FV3GFS-T2O -q dev_transfer -p 1/1/S -r 1024/1/1 -t 2:00:00 -j read_hpss -o read_hpss.out read_hpss.sh    
fi
#---------------------

testfile=$inidir/sfnanl.gfs.$cdate                       
nsleep=0; tsleep=120;  msleep=50 
while test ! -s $testfile -a $nsleep -lt $msleep;do
  sleep $tsleep; nsleep=`expr $nsleep + 1`
done

#********************************
else
#********************************
 echo "ictype=$ictype not supported, exit"
 exit 1
#********************************
fi
#********************************

#------------------------------------------------------
#- old nsnanl uses lsmask, change to land           
#$BASE_GSM/exec/nemsio_read nsnanl.gfs.$cdate |grep land
#if [ $? -ne 0 ]; then
# mv nsnanl.gfs.$cdate fnsti
# APRUN_NST='aprun -q -j1 -n1 -N1 -d1 -cc depth' 
# $APRUN_NST $BASE_GSM/exec/nst_mask_namchg
# if [ -s fnsto ]; then
#  mv fnsto nsnanl.gfs.$cdate
# else
#  echo "nst_mask_namchg failed, exit"
#  exit
# fi
#fi

$BASE_GSM/ush/global_chgres_driver.sh
#.......................................................
fi
#.......................................................

cd $paradir
if [ -s $out_dir/${CASE}_${cdate}/gfs_data.tile6.nc ]; then
 $PSUB para_config $cdate gfs fcst1
fi
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
    -w "+0300" -j fv3test -o fv3gfs_fcst.out submit_fv3gfs.sh
fi

exit
