#!/bin/sh
#----WCOSS_DELL JOBCARD
#BSUB -P FV3GFS-T2O
#BSUB -o log.chgres.%J
#BSUB -e log.chgres.%J
#BSUB -J fv3_chgres
#BSUB -q devmax
#BSUB -M 2400
#BSUB -W 10:00
#BSUB -R span[ptile=14]
#BSUB -n 14

#----WCOSS_CRAY JOBCARD
##BSUB -L /bin/sh
##BSUB -P FV3GFS-T2O
##BSUB -oo log.chgres.%J
##BSUB -eo log.chgres.%J
##BSUB -J fv3_chgres
##BSUB -q dev
##BSUB -M 2400
##BSUB -W 10:00
##BSUB -extsched 'CRAYLINUX[]'

#----THEIA JOBCARD
##PBS -N fv3_chgres_driver
##PBS -A fv3-cpu
##PBS -o log.chgres
##PBS -e log.chgres
##PBS -l nodes=1:ppn=24
##PBS -q batch
##PBS -l walltime=06:00:00
set -x

#-------------------------------------------------------------------------------------------------
# This script calls ./ush/global_chgres_driver.sh to create high-res and/or enkf cold 
# start initial conditions, and stages all necessary DA files for starting a forecast-only 
# or cycled fv3gfs experiment.
# Fanglin Yang, 03/08/2018
#-------------------------------------------------------------------------------------------------

export machine=WCOSS_DELL_P3            ;#WCOSS_C, WCOSS_DELL_P3, THEIA

if [ $machine = WCOSS_C ]; then
 export HOMEgfs=/gpfs/hps3/emc/global/noscrub/$USER/git/fv3gfs/master
 export PTMP="/gpfs/hps2/ptmp/$USER"
 export zero_bias_dir=/gpfs/hps3/emc/global/noscrub/emc.glopara/ICS/bias_zero
elif [ $machine = WCOSS_DELL_P3 ]; then
 export HOMEgfs=/gpfs/dell2/emc/modeling/noscrub/$USER/git/fv3gfs/master
 export PTMP="/gpfs/dell2/ptmp/$USER"
 export zero_bias_dir=/gpfs/hps3/emc/global/noscrub/emc.glopara/ICS/bias_zero
elif [ $machine = THEIA ]; then
 export HOMEgfs=/scratch4/NCEPDEV/global/save/$USER/git/fv3gfs/master
 export PTMP="/scratch4/NCEPDEV/stmp4/$USER"
 export zero_bias_dir=/scratch4/NCEPDEV/global/save/Fanglin.Yang/git/bias_zero
else 
 echo "Please define HOMEgfs and PTMP for your machine. exit"
 exit
fi

export PSLOT=fv3test
export CDUMP=gdas
export CASE_HIGH=C768
export CASE_ENKF=C384
export CDATE=2018050100

export NSTSMTH=YES                                  ##apply 9-point smoothing to nsst tref
export NST_TF_CHG=$HOMEgfs/exec/nst_tf_chg.x
export ZERO_BIAS=YES                                ##zeroed out all bias and radsat files 

#===========================================================
#===========================================================

export ymd=`echo $CDATE | cut -c 1-8`
export cyc=`echo $CDATE | cut -c 9-10`
export yy=`echo $CDATE | cut -c 1-4`
export mm=`echo $CDATE | cut -c 5-6`
export dd=`echo $CDATE | cut -c 7-8`

export ROTDIR=$PTMP/$PSLOT
export RUNDIR=$ROTDIR/chgres

export NODES=1
export APRUNC=""

. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

if [ $machine = WCOSS_C ]; then
 # . $MODULESHOME/init/sh                 2>>/dev/null
 # module load prod_util prod_envir hpss  2>>/dev/null
 # module load PrgEnv-intel               2>>/dev/null
 export KMP_AFFINITY=disabled
 export OMP_NUM_THREADS_CH=24
 export APRUNC="aprun -n 1 -N 1 -j 1 -d $OMP_NUM_THREADS_CH -cc depth"
 export APRUNTF='aprun -q -j1 -n1 -N1 -d1 -cc depth'
 export SUB=/u/emc.glopara/bin/sub_wcoss_c
 export ACCOUNT=FV3GFS-T2O
 export QUEUE=dev
 export QUEUE_TRANS=dev_transfer 
elif [ $machine = WCOSS_DELL_P3 ]; then
 # . /usrx/local/prod/lmod/lmod/init/sh   2>>/dev/null
 # module purge                           2>>/dev/null
 # module load EnvVars/1.0.2              2>>/dev/null
 # module load lsf/10.1                   2>>/dev/null
 # module load ips/18.0.1.163             2>>/dev/null
 # module load impi/18.0.1                2>>/dev/null
 # module load prod_util/1.1.0            2>>/dev/null
 # module load prod_envir/1.0.2           2>>/dev/null
 # module load HPSS/5.0.2.5               2>>/dev/null
 # module load NetCDF/4.5.0               2>>/dev/null
 # module load HDF5-serial/1.10.1         2>>/dev/null
 export OMP_NUM_THREADS_CH=14
 export KMP_AFFINITY=disabled
 export APRUNC="time"
 export APRUNTF="time"
 export SUB=/u/emc.glopara/bin/sub_wcoss_d
 export ACCOUNT=FV3GFS-T2O
 export QUEUE=dev
 export QUEUE_TRANS=dev_transfer 
elif [ $machine = THEIA ]; then
 # source $HOMEgfs/sorc/machine-setup.sh
 # module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
 # module load netcdf hdf5/1.8.14    2>>/dev/null
 export APRUNC=time
 export APRUNTF=time
 export OMP_NUM_THREADS_CH=24
 export SUB=/home/Fanglin.Yang/bin/sub_theia
 export ACCOUNT=fv3-cpu
 export QUEUE=batch
 export QUEUE_TRANS=service
else
 echo "$machine not supported, exit"
 exit
fi



#----------------------------
#----------------------------
#--for high-res
#----------------------------
#----------------------------
export CASE=$CASE_HIGH
export INIDIR=$RUNDIR/$CDUMP/$CASE
export COMROT=$ROTDIR/${CDUMP}.$ymd/$cyc
export OUTDIR=$COMROT/INPUT
export DATA=$INIDIR/stmp
rm -rf $INIDIR $OUTDIR $DATA
mkdir -p $INIDIR $OUTDIR  $DATA
cd $INIDIR ||exit 8

#................................................
if [ -s $COMROOT/gfs/prod/${CDUMP}.${ymd} ]; then
#................................................
   ## get operational real-time data from COMROT
   atm=./${CDUMP}.t${cyc}z.atmanl.nemsio
   sfc=./${CDUMP}.t${cyc}z.sfcanl.nemsio
   nst=./${CDUMP}.t${cyc}z.nstanl.nemsio
   biascr=./${CDUMP}.t${cyc}z.abias 
   biascr_pc=./${CDUMP}.t${cyc}z.abias_pc
   aircraft_t_bias=./${CDUMP}.t${cyc}z.abias_air  
   radstat=./${CDUMP}.t${cyc}z.radstat
   for ff in $atm $sfc $nst ; do
     cp $COMROOT/gfs/prod/${CDUMP}.${ymd}/$ff .
   done
   if [ $CDUMP = gdas ]; then
    for ff in $biascr $biascr_pc $aircraft_t_bias $radstat ; do
      cp $COMROOT/gfs/prod/${CDUMP}.${ymd}/$ff $COMROT/.
    done
   fi

#................................................
else   ##get data from HPSS archive
#................................................

if [ $CDATE -lt 2014050100 ]; then

   HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd      ##use operational nems gfs nems gfs ics
   nst=" "
   if [ $CDUMP = gdas ]; then
     tarball_high=com_gfs_prod_${CDUMP}.${CDATE}.tar
     atm=./gdas1.t${cyc}z.sanl
     sfc=./gdas1.t${cyc}z.sfcanl
     biascr=./gdas1.t${cyc}z.abias
     biascr_pc=" "
     radstat=./gdas1.t${cyc}z.radstat
     aircraft_t_bias=" "
   else
     tarball_high=com_gfs_prod_${CDUMP}.${CDATE}.anl.tar
     atm=./gfs.t${cyc}z.sanl
     sfc=./gfs.t${cyc}z.sfcanl
   fi

elif [ $CDATE -le 2017072012 ]; then
   if [ $CDATE -ge 2016110100 ]; then
     oldexp=prnemsrn
   elif [ $CDATE -ge 2016050100 ]; then
     oldexp=pr4rn_1605
   elif [ $CDATE -ge 2015121500 ]; then
     oldexp=pr4rn_1512
   elif [ $CDATE -ge 2015050200 ]; then
     oldexp=pr4rn_1505
   elif [ $CDATE -ge 2014073000 ]; then
     oldexp=pr4rn_1408
   elif [ $CDATE -ge 2014050100 ]; then
     oldexp=pr4rn_1405
   else
     echo "NEMS GSM retro ICs do not exit, exit"
     exit 1
   fi

   HPSSPATH=/5year/NCEPDEV/emc-global/emc.glopara/WCOSS_C/$oldexp    ##use q3fy17 nems gfs parallel ics
   tarball_high=${CDATE}${CDUMP}.tar
   atm=gfnanl.${CDUMP}.$CDATE
   sfc=sfnanl.${CDUMP}.$CDATE
   nst=nsnanl.${CDUMP}.$CDATE
   biascr=biascr.${CDUMP}.$CDATE
   biascr_pc=biascr_pc.${CDUMP}.$CDATE
   aircraft_t_bias=aircraft_t_bias.${CDUMP}.$CDATE
   radstat=radstat.${CDUMP}.$CDATE
else
   HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd      ##use operational nems gfs nems gfs ics
   if [ $CDUMP = gfs ]; then
     tarball_high=gpfs_hps_nco_ops_com_gfs_prod_${CDUMP}.${CDATE}.anl.tar
   else
     tarball_high=gpfs_hps_nco_ops_com_gfs_prod_${CDUMP}.${CDATE}.tar
   fi
   atm=./${CDUMP}.t${cyc}z.atmanl.nemsio
   sfc=./${CDUMP}.t${cyc}z.sfcanl.nemsio
   nst=./${CDUMP}.t${cyc}z.nstanl.nemsio
   biascr=./${CDUMP}.t${cyc}z.abias 
   biascr_pc=./${CDUMP}.t${cyc}z.abias_pc
   aircraft_t_bias=./${CDUMP}.t${cyc}z.abias_air  
   radstat=./${CDUMP}.t${cyc}z.radstat
fi

#--extract ICs from hpss
cat > read_hpss.sh <<EOF1

   . $HOMEgfs/ush/load_fv3gfs_modules.sh   2>>/dev/null

   #export machine=$machine
   #if [ $machine = WCOSS_C ]; then
   # . $MODULESHOME/init/sh                 2>>/dev/null
   # module load hpss  2>>/dev/null
   #elif [ $machine = WCOSS_DELL_P3 ]; then
   # . /usrx/local/prod/lmod/lmod/init/sh   2>>/dev/null
   # module load HPSS/5.0.2.5               2>>/dev/null
   #elif [ $machine = THEIA ]; then
   # source $HOMEgfs/sorc/machine-setup.sh  2>>/dev/null
   # module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles  2>>/dev/null
   # module load hpss                       2>>/dev/null
   #fi

   cd $INIDIR
   htar -xvf  $HPSSPATH/$tarball_high $atm $sfc $nst  
   if [ $CDUMP = gdas ]; then
      cd $COMROT 
      htar -xvf  $HPSSPATH/$tarball_high $biascr $biascr_pc $aircraft_t_bias $radstat

      [[ '$biascr' = ./gdas1.t${cyc}z.abias ]] && mv $biascr ./gdas.t${cyc}z.abias
      [[ '$radstat' = ./gdas1.t${cyc}z.radstat ]] && mv $radstat ./gdas.t${cyc}z.radstat

      [[ '$biascr' = biascr.${CDUMP}.$CDATE ]] && mv biascr.${CDUMP}.$CDATE  ${CDUMP}.t${cyc}z.abias 
      [[ '${biascr_pc}' = biascr_pc.${CDUMP}.$CDATE ]] &&  mv biascr_pc.${CDUMP}.$CDATE ${CDUMP}.t${cyc}z.abias_pc
      [[ '$aircraft_t_bias' = aircraft_t_bias.${CDUMP}.$CDATE ]] && mv aircraft_t_bias.${CDUMP}.$CDATE ${CDUMP}.t${cyc}z.abias_air
      [[ '$radstat' = radstat.${CDUMP}.$CDATE ]] &&  mv radstat.${CDUMP}.$CDATE  ${CDUMP}.t${cyc}z.radstat

   fi
EOF1
chmod u+x read_hpss.sh
$SUB -a $ACCOUNT -q $QUEUE_TRANS -p 1/1/S -r 1024/1/1 -t 2:00:00 -j read_hpss -o read_hpss.out $INIDIR/read_hpss.sh

#................................................
fi
#................................................


testfile=$INIDIR/$sfc                 
nsleep=0; tsleep=120;  msleep=50
while test ! -s $testfile -a $nsleep -lt $msleep;do
  sleep $tsleep; nsleep=`expr $nsleep + 1`
done
sleep 300

if [ ! -s $testfile ]; then 
  echo "$testfile does not exist, exit !"
  exit 1
fi

[[ $atm = ./gdas1.t${cyc}z.sanl ]] && mv $INIDIR/$atm $INIDIR/gdas.t${cyc}z.sanl
[[ $sfc = ./gdas1.t${cyc}z.sfcanl ]] && mv $INIDIR/$sfc $INIDIR/gdas.t${cyc}z.sfcanl

#------------------------------
if [ $NSTSMTH = "YES" ]; then
#------------------------------
mv $nst fnsti
rm -f tf_chg_parm.input
cat >tf_chg_parm.input <<EOF1
&config
nsmth=3,istyp=0,
/
EOF1
$APRUNTF $NST_TF_CHG <tf_chg_parm.input 
mv fnsto $nst
if [ $? -ne 0 ] ; then
  echo "NST_TF_CHG for $CDUMP $CASE failed. exit"
  exit 1
fi
#------------------------------
fi
#------------------------------


$HOMEgfs/ush/global_chgres_driver.sh
if [ $? -ne 0 ] ; then
 echo "chgres for $CDUMP $CASE failed. exit"
 exit 1
fi

if [ $CDUMP = "gdas" -a $ZERO_BIAS = "YES" ]; then
   cd $COMROT
   for ff in abias abias_air abias_pc radstat; do
    mv gdas.t${cyc}z.${ff}  gdas.t${cyc}z.${ff}_save
    cp -p ${zero_bias_dir}/gdas.t18z.${ff} gdas.t${cyc}z.${ff}
   done
fi 


[[ $CDUMP = gfs ]] && exit
[[ $CDATE -lt 2012052100 ]] && exit  # no enkf data before this date

#----------------------------
#----------------------------
#--for ENKF 
#----------------------------
#----------------------------
export CASE=$CASE_ENKF
export INIDIR=$RUNDIR/$CDUMP/$CASE
rm -rf $INIDIR; mkdir -p $INIDIR 
cd $INIDIR ||exit 8

#................................................
if [ -s $COMROOT/gfs/prod/enkf.${ymd}/${cyc} ]; then
#................................................
   ## get operational real-time data from COMROT
   for ff in ratmanl sfcanl nstanl; do
    cp $COMROOT/gfs/prod/enkf.${ymd}/${cyc}/gdas.t${cyc}z.${ff}* .
   done
   testfile=$INIDIR/gdas.t${cyc}z.sfcanl.mem080.nemsio

#................................................
else   ## extract data from HPSS                      
#................................................
if [ $CDATE -lt 2014050100 ]; then
  HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd      ##use operational nems gfs nems gfs ics
  tarball_enkf_atm=com_gfs_prod_enkf.${ymd}_${cyc}.anl.tar    
  testfile=$INIDIR/siganl_${CDATE}_mem080
elif [ $CDATE -le 2017072012 ]; then
   if [ $CDATE -ge 2016110100 ]; then
     oldexp=prnemsrn
   elif [ $CDATE -ge 2016050100 ]; then
     oldexp=pr4rn_1605
   elif [ $CDATE -ge 2015121500 ]; then
     oldexp=pr4rn_1512
   elif [ $CDATE -ge 2015050200 ]; then
     oldexp=pr4rn_1505
   elif [ $CDATE -ge 2014073000 ]; then
     oldexp=pr4rn_1408
   elif [ $CDATE -ge 2014050100 ]; then
     oldexp=pr4rn_1405
   else
     echo "NEMS GSM retro ICs do not exit, exit"
     exit 1
   fi

 HPSSPATH=/5year/NCEPDEV/emc-global/emc.glopara/WCOSS_C/$oldexp    ##use q3fy17 nems gfs parallel ics
 tarball_enkf_atm=${CDATE}gdas.enkf.anl.tar
 tarball_enkf_sfcnst=${CDATE}gdas.enkf.sfcanl.tar
 testfile=$INIDIR/sfcanl_${CDATE}_mem080
else
 HPSSPATH=/NCEPPROD/hpssprod/runhistory/rh$yy/$yy$mm/$yy$mm$dd      ##use operational nems gfs nems gfs ics
 tarball_enkf_atm=gpfs_hps_nco_ops_com_gfs_prod_enkf.${ymd}_${cyc}.anl.tar    
 testfile=$INIDIR/gdas.t${cyc}z.sfcanl.mem080.nemsio
fi

#--extract ICs from hpss
cat > read_hpss.sh <<EOF

   . $HOMEgfs/ush/load_fv3gfs_modules.sh   2>>/dev/null

   #export machine=$machine
   #if [ $machine = WCOSS_C ]; then
   # . $MODULESHOME/init/sh                 2>>/dev/null
   # module load hpss  2>>/dev/null
   #elif [ $machine = WCOSS_DELL_P3 ]; then
   # . /usrx/local/prod/lmod/lmod/init/sh   2>>/dev/null
   # module load HPSS/5.0.2.5               2>>/dev/null
   #elif [ $machine = THEIA ]; then
   # source $HOMEgfs/sorc/machine-setup.sh  2>>/dev/null
   # module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles  2>>/dev/null
   # module load hpss                       2>>/dev/null
   #fi

   cd $INIDIR
   htar -xvf  $HPSSPATH/$tarball_enkf_atm 
   if [ $CDATE -le 2017072000 ] && [ $CDATE -gt 2014050100 ]; then
      htar -xvf  $HPSSPATH/$tarball_enkf_sfcnst 
   fi
EOF
chmod u+x read_hpss.sh
$SUB -a $ACCOUNT -q $QUEUE_TRANS -p 1/1/S -r 1024/1/1 -t 2:00:00 -j read_hpss -o read_hpss.out $INIDIR/read_hpss.sh

#................................................
fi
#................................................


nsleep=0; tsleep=120;  msleep=50
while test ! -s $testfile -a $nsleep -lt $msleep;do
  sleep $tsleep; nsleep=`expr $nsleep + 1`
done

if [ ! -s $testfile ]; then
 echo " $testfile does not exist, exit"
 exit
fi

#---------------------------
n=1
while [ $n -le 80 ]; do
#---------------------------
mem=$(printf %03i $n)
mchar=mem$(printf %03i $n)


export COMROT=$ROTDIR/enkfgdas.$ymd/$cyc/$mchar
export OUTDIR=$COMROT/INPUT
export DATA=$INIDIR/$mchar
rm -rf $OUTDIR $DATA
mkdir -p $OUTDIR $DATA

if [ $CDATE -lt 2014050100 ]; then
  atm=gdas.t${cyc}z.sanl
  sfc=gdas.t${cyc}z.sfcanl
  rm -fr $atm $sfc 
else
  atm=${CDUMP}.t${cyc}z.atmanl.nemsio
  sfc=${CDUMP}.t${cyc}z.sfcanl.nemsio
  nst=${CDUMP}.t${cyc}z.nstanl.nemsio
  rm -rf $atm $sfc $nst 
fi

if [ $CDATE -lt 2014050100 ]; then
   ln -fs siganl_${CDATE}_$mchar $atm
   ln -fs sfcanl_${CDATE}_$mchar $sfc
elif [ $CDATE -le 2017072000 ]; then
   ln -fs siganl_${CDATE}_$mchar $atm
   ln -fs sfcanl_${CDATE}_$mchar $sfc
   ln -fs nstanl_${CDATE}_$mchar $nst
   cp     nstanl_${CDATE}_$mchar fnsti 
else
   ln -fs gdas.t${cyc}z.ratmanl.${mchar}.nemsio $atm
   ln -fs gdas.t${cyc}z.sfcanl.${mchar}.nemsio $sfc
   ln -fs gdas.t${cyc}z.nstanl.${mchar}.nemsio $nst
   cp     gdas.t${cyc}z.nstanl.${mchar}.nemsio fnsti
fi

#------------------------------
if [ $NSTSMTH = "YES" ]; then
#------------------------------
rm -f tf_chg_parm.input
cat >tf_chg_parm.input <<EOF1
&config
nsmth=3,istyp=0,
/
EOF1
$APRUNTF $NST_TF_CHG <tf_chg_parm.input
rm -f $nst
mv fnsto $nst
if [ $? -ne 0 ] ; then
  echo "NST_TF_CHG for $CDUMP $CASE failed. exit"
  exit 1
fi
#------------------------------
fi
#------------------------------

$HOMEgfs/ush/global_chgres_driver.sh
if [ $? -ne 0 ] ; then
 echo "chgres for enkf $CASE ${mchar} failed. exit"
 exit
fi

#---------------------------
n=$((n+1))
done
#---------------------------

exit
