#!/bin/ksh
set -ux
################################################################################
# This script runs the archive and cleanup.
# Usage: arch.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
################################################################################

set -a;. $CONFIG;set +a
echo "-----end of $CONFIG ------------"
echo

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
eval export DATA=$DATATMP
export ROTDIR=${ROTDIR:-$PTMP/$LOGNAME/pr$PSLOT}
export COMROT=${COMROT:-$pwd}
export COMDAY=${COMDAY:-$COMROT}
export RESDIR=${RESDIR:-$COMROT/RESTART}
export ARCH_TO_HPSS=${ARCH_TO_HPSS:-YES}
export ARCH_TO_DISK=${ARCH_TO_DISK:-NO}
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export NWPROD=${NWPROD:-$BASEDIR}

cycn=$(echo $CDATE|cut -c 9-10)
export TCYC=${TCYC:-".t${cycn}z."} 
export PREFIX=${PREFIX:-${CDUMP}${TCYC}}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}

HRKTMP=${HRKTMP:-24}
HRKROT=${HRKROT:-120}
HRKVFYARC=${HRKVFYARC:-240}
HRKTMPGDAS=${HRKTMPGDAS:-$HRKTMP}
HRKTMPGFS=${HRKTMPGFS:-$HRKTMP}
HRKGEMPAK=${HRKGEMPAK:-$HRKTMP}
HRKBUFRSND=${HRKBUFRSND:-$HRKTMP}
HRKCOM=${HRKCOM:-$HRKTMP}

ARCHCOPY=${ARCHCOPY:-NO}
ARCHDAY=${ARCHDAY:-0}       # impacts delay for online archive only
BACKDATE=$((ARCHDAY*24))    # impacts delay for online archive only

ATARDIR=${ATARDIR:-null}
ATARFILE=${ATARFILE:-$CDATE$CDUMP.tar}
ATARDIR_DISK=${ATARDIR_DISK:-/gpfs/hps/nco/storage/gfs_retro}

DO_PRODNAMES=${DO_PRODNAMES:-NO}
PRODNAMES_DIR=${PRODNAMES_DIR:-$COMROT/prod}
SETUPPRODNAMESH=${SETUPPRODNAMESH:-$USHDIR/setup_prodnames.sh}

PARA_CHECK_STATUS=${PARA_CHECK_STATUS:-NO}
PARA_CHECK_CDUMP=${PARA_CHECK_CDUMP:-"gfs gdas"}
PARA_CHECK_CYCLE=${PARA_CHECK_CYCLE:-"00 06 12 18"}
PARA_CHECK_RUNSH=${PARA_CHECK_RUNSH:-$USHDIR/para_check_run.sh}
PARA_CHECKSH=${PARA_CHECKSH:-$USHDIR/para_check_status.sh}
PARA_CHECK_MAIL=${PARA_CHECK_MAIL:-$LOGNAME}
machine=${machine:-WCOSS}

GENGEMPAK=${GENGEMPAK:-NO}
GENGEMPAK_META=${GENGEMPAK_META:-NO}
NAWIPSSH=${NAWIPSSH:-$USHDIR/nawips.sh}
NAWIPSMETASH=${NAWIPSMETASH:-$USHDIR/nawips_meta.sh}

GENBUFRSND=${GENBUFRSND:-NO}
POSTSNDSH=${POSTSNDSH:-$USHDIR/postsnd.sh}

#RSTPRODSH=${RSTPRODSH:-$NWPROD/runhistory.v2.2.7/ush/rhist_restrict.sh}
RSTPRODSH=${RSTPRODSH:-$USHDIR/rhist_restrict.sh}
TSM_FLAG=${TSM_FLAG:-NO}

$PBEG
################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export SCP=${SCP:-/usr/bin/scp}
export NDATE=${NDATE:-${NWPROD}/util/exec/ndate}
export COPYGB=${COPYGB:-${NWPROD}/util/exec/copygb}
export NCEPPOST=${NCEPPOST:-NO}
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export GDATE=$($NDATE -$CYINC $CDATE)
export HPSSTAR=${HPSSTAR:-$BASEDIR/ush/hpsstar}
export SUB=${SUB:-$BASEDIR/bin/sub}
export HTAR=${HTAR:-/apps/hpss/htar}
export HSI=${HSI:-/apps/hpss/hsi}

SDATE=$CDATE
BDATE=$($NDATE -$BACKDATE $CDATE)    # online archive date only

#--------------------------------
#--# Copy files to online archive
if [[ $ARCHCOPY = YES ]];then
#--------------------------------
 if [ ! -s $ARCDIR ]; then mkdir -p $ARCDIR; fi
 if [ ! -s $VFYARC ]; then mkdir -p $VFYARC; fi
 export CDATE=$BDATE

# copy 1-def pgbf and other stats data direct to ARCDIR
cd $COMROT 
for f in *pgrbf* ; do
 fhr=$(echo $f |cut -c 15-18)
 fhr=$((fhr+0)); if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
 $NCP $f  $ARCDIR/pgbf${fhr}.$CDUMP.$CDATE
done
$NCP ${PREFIX}pgrbanl $ARCDIR/pgbanl.$CDUMP.$CDATE

for f in cnvstat oznstat radstst gsistat abias abias_air abias_pc ;do 
 $NCP ${PREFIX}$f $ARCDIR/.                 
done
for f in atcf tcinform_relocate tcvitals_relocate trak storm enkfstat ;do
 $NCP ${f}* $ARCDIR/.                 
done

#  copy pgbq to temporary directory for precipitation verification
if [ $CDUMP = gfs -a -d $VFYARC ]; then
fhr=00
while [ $fhr -le ${vhr_rain:-180} ]; do
 $NCP ${PREFIX}pgrbq$fhr  $VFYARC/pgbq${fhr}.$CDUMP.$CDATE
 fhr=$((fhr+6))
 if [ $fhr -lt 10 ]; then fhr=0$fhr; fi
done
fi
#-----------------------------
fi
#-----------------------------


#---------------------------------------------------
# Archive to tape 
#---------------------------------------------------
export CDATE=$SDATE      # so, instead, archive to tape asap
rc=0

SGDATE=$GDATE
if [ $ARCH_TO_HPSS = YES -o $ARCH_TO_DISK = YES ] ; then
 cycle=$(echo $CDATE|cut -c9-10)
 cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')

# GDATE and GDATE00 for SCORES and rain files
 export GDATE=$($NDATE -6 $CDATE)
 export CDATE00=$(echo $CDATE|cut -c1-8)
 export GDATE00=$(echo $GDATE|cut -c1-8)
# cd $DATA

 for a in ARCA ARCB ARCC ARCD ARCE ARCF ;do
   eval eval afile=\${$a$cycle$cdump:-null}
   if [[ $afile != null ]];then
    afiletmp=$afile
    adir=$ATARDIR
    if [ $ARCH_TO_HPSS = YES ]; then
     afile=$ATARDIR/$afiletmp
    elif [ $ARCH_TO_DISK = YES ]; then
     adir=${ATARDIR_DISK}${ATARDIR}
    fi
    if [ $ARCH_HWRF = YES ]; then
      for list in $ARCH_HWRF_LIST ;do
        if [ "$a" == "$list" ]; then
          if [ $ARCH_TO_HPSS = YES ]; then
            afile=$ATARDIR_HWRF/$afiletmp
          elif [ $ARCH_TO_DISK = YES ]; then
            adir=${ATARDIR_DISK}${ATARDIR_HWRF}
          fi
          break 1
        fi
      done
    fi
    if [ $ARCH_TO_HPSS = YES ]; then
      $HSI mkdir -p $adir
    elif [ $ARCH_TO_DISK = YES ]; then
      if [ ! -d $adir ]; then mkdir -p $adir; fi
    fi
    NDATA=$STMP/$LOGNAME/HPSS_${PSLOT}$CDATE$CDUMP${a}
    if [ $machine = THEIA -o $machine = WCOSS -o $machine = WCOSS_C ] ; then
     np='1/1/S'
     mem='1024/1'
     tl=${TIMELIMARCH:-'3:00:00'}
     qq=${CUE2RUNA:-transfer}
     jn=${PSLOT}${CDATE}${CDUMP}${a}
     out=$COMROT/$jn.dayfile
     trans_local=$COMROT/transfer_${a}_${CDATE}${CDUMP}${CSTEP}
     > $trans_local
     echo "#!/bin/ksh"                >> $trans_local
     echo "set -ax"                   >> $trans_local
     echo "export NDATA=$NDATA"       >> $trans_local
     echo "export HTAR=$HTAR"         >> $trans_local
     echo "export HSI=$HSI"         >> $trans_local
     echo "export PCOP=$PCOP"         >> $trans_local
     echo "export NCP='$NCP'"         >> $trans_local
     echo "export PSLOT=$PSLOT"       >> $trans_local
     echo "export BASEDIR=$BASEDIR"   >> $trans_local
     echo "export CDATE=$CDATE"       >> $trans_local
     echo "export CDUMP=$CDUMP"       >> $trans_local
     echo "export GDATE=$GDATE"       >> $trans_local
     echo "export GDUMP=$GDUMP"       >> $trans_local
     echo "export COMROT=$COMROT"     >> $trans_local
     echo "export RLIST=$RLIST"       >> $trans_local
     echo "export COMDAY=$COMDAY"     >> $trans_local
     echo "export a=$a"               >> $trans_local
     echo "export afile=$afile"       >> $trans_local
     echo "export KEEPDATA=$KEEPDATA" >> $trans_local
     echo ""                          >> $trans_local
     echo "mkdir -p $NDATA||exit 1;cd $NDATA||exit 1" >> $trans_local
     echo "rm *"                      >> $trans_local
     echo "$PCOP $CDATE/$CDUMP/arch/$a $COMROT $NDATA <$RLIST" >> $trans_local
     echo "[ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMP/arch/$a $COMDAY $NDATA <$RLIST" >> $trans_local
     echo "sleep 30"                  >> $trans_local
     echo ""                          >> $trans_local
     if [ $ARCH_TO_HPSS = YES ]; then
       echo "$HTAR -Hcrc -Hverify=1 -V -cvf $afile *" >> $trans_local
     elif [ $ARCH_TO_DISK = YES ]; then
       #echo 'crc32 `find . -maxdepth 1 -type f | sort -n `> ${afile}.idx' >> $trans_local
       echo "tar -W -cvf $afile *" >> $trans_local
     fi
     echo "rc=\$?"                    >> $trans_local
     echo "if [[ \$rc -ne 0 ]]; then exit 1 ; fi"   >> $trans_local
     echo "export TSM_FLAG=$TSM_FLAG" >> $trans_local
     echo "$RSTPRODSH $afile $ARCH_TO_HPSS $ARCH_TO_DISK"         >> $trans_local
     if [ $ARCH_TO_DISK = YES ]; then
       echo "$NCP ${afile}* $adir/" >> $trans_local
       echo "rc=\$?"                    >> $trans_local
     fi
     echo "if [[ \$rc -ne 0 ]]; then exit 1 ; fi"   >> $trans_local
     echo "if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $NDATA ; fi" >> $trans_local
     chmod 755 $trans_local
     en=CONFIG="$trans_local"
     $SUB -e $en -q $qq -a $ACCOUNT -g $GROUP -p $np -r $mem -t $tl -j $jn -o $out $trans_local
     ((rc+=$?))
    fi
   fi
 done
fi

#---------------------------------------------------
# Clean up.
#---------------------------------------------------
export CDATE=$SDATE
export GDATE=$SGDATE

# rm old work directories
HRKTMPDIR=$HRKTMP
if [[ $CDUMP = gdas ]] ; then
  HRKTMPDIR=$HRKTMPGDAS
elif [[ $CDUMP = gfs ]] ; then
  HRKTMPDIR=$HRKTMPGFS
fi
rdate=$($NDATE -$HRKTMPDIR $CDATE)
rm -rf $(CDATE=$rdate CDUMP=$CDUMP CSTEP='*' eval ls -d $DATATMP) 2>/dev/null


#---------------------------------------------------
#-clean NCO-like aged directory, delete all files
rdate=$($NDATE -$HRKROT $CDATE)
rmymd=$(echo $rdate |cut -c 1-8)
rmcyc=$(echo $rdate |cut -c 9-10)
rmdir=$ROTDIR/${CDUMP}.$rmymd
if [ -d $rmdir ]; then rm -rf $rmdir ; fi

#-clean aged vrfyarch archive
rdate=$($NDATE -$HRKVFYARC $CDATE)
rm -f $VFYARC/*${rdate}*

#---------------------------------------------------
# If requested, save special sigf and sfcf files for HWRF group      
if [ $CDUMP = gfs -a $ARCH_TO_HPSS = YES -a ${HWRF_ARCH:-NO}  = YES ] ; then
set -x
  export INHERIT_ENV=YES
  export PSLOT=$PSLOT               
  export CDATE=$CDATE               
  export FHMAX_HWRF=${FHMAX_HWRF:-126}
  export FHOUT_HWRF=${FHOUT_HWRF:-6}
  export ATARDIR_HWRF=${ATARDIR_HWRF:-/2year/NCEPDEV/emc-hwrf/GFS-PR4DEV}
  np='1/1/S'
  mem='1024/1'
  tl=${TIMELIMARCH:-'3:00:00'}
  qq=${CUE2RUNA:-transfer}
  jn=${PSLOT}${CDATE}${CDUMP}hwrf
  out=$COMROT/$jn.dayfile
  job=${HWRFARCHSH:-$USHDIR/global_save4hwrf.sh}
  en="PSLOT,CDATE,COMROT,FHMAX_HWRF,FHOUT_HWRF,HPSSTAR,ATARDIR_HWRF"
  $SUB -e $en -q $qq -a $ACCOUNT -g $GROUP -p $np -r $mem -t $tl -j $jn -o $out $job
  export INHERIT_ENV=NO
fi


#------------------------------------------
# If requsted, generate status message
#------------------------------------------
cd $DATA ||exit 8
if [ $PARA_CHECK_STATUS = YES ] ; then
   for PARA_CDUMP in $PARA_CHECK_CDUMP; do
      if [ $CDUMP = $PARA_CDUMP ] ; then
         CYCLE=$(echo $CDATE|cut -c9-10)
         for PARA_CYCLE in $PARA_CHECK_CYCLE; do
            if [ $CYCLE = $PARA_CYCLE ] ; then
               $PARA_CHECK_RUNSH
               if [ $machine = WCOSS_C ] ; then
                  np='1/1/S'
                  mem='1024/1'
                  tl=00:10:00
                  qq=${CUE2RUNA:-transfer}
                  jn=${PSLOT}${CDATE}${CDUMP}mail
                  out=$COMROT/$jn.dayfile
                  trans_mail=$COMROT/mail_${PSLOT}${CDATE}${CDUMP}
                  > $trans_mail
                  echo "#!/bin/sh"           >> $trans_mail
                  echo "set -ax"             >> $trans_mail
                  echo "export PSLOT=$PSLOT" >> $trans_mail
                  echo "export CDATE=$CDATE" >> $trans_mail
                  echo "export CDUMP=$CDUMP" >> $trans_mail
                  echo "mail -s '"$PSLOT $CDATE $CDUMP status"' $PARA_CHECK_MAIL < $DATA/check_status/temp.msg" >> $trans_mail
                  chmod 755 $trans_mail
                  en=CONFIG="$trans_mail"
                  $SUB -e $en -q $qq -a $ACCOUNT -g $GROUP -p $np -r $mem -t $tl -j $jn -o $out $trans_mail
               fi
            fi
         done
      fi
   done
fi


################################################################################
# If requested, make gempak.
if [ $CDUMP = gfs -a $DO_PRODNAMES = YES -a $GENGEMPAK = YES -a ${AWIPS20KM:-NO} = YES ];then
  export mem=2048
  export jn=${PSLOT}${CDATE}${CDUMP}gempak
  export out=$COMROT/${jn}.dayfile
  $SUB -e 'CDATE=$CDATE CDUMP=$CDUMP CSTEP=$CSTEP CONFIG=$CONFIG' -q $CUE2RUN -a $ACCOUNT -g $GROUP -p 6/6/N -r $mem/1/2 -t 06:00 -j $jn -o $out $NAWIPSSH

  if [ $AWIPS20KM = YES ];then
    export mem=2048
    export jn=${PSLOT}${CDATE}${CDUMP}awips20km
    export out=$COMROT/${jn}.dayfile
    $SUB -e 'CDATE=$CDATE CDUMP=$CDUMP CSTEP=$CSTEP CONFIG=$CONFIG' -q $CUE2RUN -a $ACCOUNT -g $GROUP -p 25/25/N -r $mem/1/8 -t 06:00 -j $jn -o $out $AWIPS20KMSH
  fi

  if [ $AWIPS20KM = YES ];then
    export mem=2048
    export jn=${PSLOT}${CDATE}${CDUMP}awips20km
    export out=$COMROT/${jn}.dayfile
    $SUB -e 'CDATE=$CDATE CDUMP=$CDUMP CSTEP=$CSTEP CONFIG=$CONFIG' -q $CUE2RUN -a $ACCOUNT -g $GROUP -p 25/25/N -r $mem/1/8 -t 06:00 -j $jn -o $out $AWIPS20KMSH
  fi

  if [ $GENGEMPAK_META = YES ];then
    export mem=2048
    export jn=${PSLOT}${CDATE}${CDUMP}gempak_meta
    export out=$COMROT/${jn}.dayfile
    $SUB -e 'CDATE=$CDATE CDUMP=$CDUMP CSTEP=$CSTEP CONFIG=$CONFIG' -q $CUE2RUN -a $ACCOUNT -g $GROUP -p 25/25/N -r $mem/1/8 -t 06:00 -j $jn -o $out $NAWIPSMETASH
  fi

  rdate=$($NDATE -$HRKGEMPAK $CDATE)   
  pdyr=`echo $rdate | cut -c1-8`
  cycr=`echo $rdate | cut -c9-10`
  cd $COMROT/nawips                    
  rm gfs.$pdyr/gfs*_${rdate}* 2>/dev/null
  rm gfs.$pdyr/gfs.t${cycr}z*idx 2>/dev/null
  rdate2=$($NDATE -24 $rdate)
  pdyr2=`echo $rdate2 | cut -c1-8`
  rm -r gfs.$pdyr2 2>/dev/null
  if [ $AWIPS20KM = YES ];then
    cd $PRODNAMES_DIR/pcom2/para
    rm gfs.$pdyr/grib2*_${cycr} 2>/dev/null
    rm -r gfs.$pdyr2 2>/dev/null
  fi
fi

################################################################################
# If requested, make GFS station profiles in bufr format
if [ $CDUMP = gfs -a $DO_PRODNAMES = YES -a $GENBUFRSND = YES ];then
   PDY=`echo $CDATE | cut -c1-8`
   export COMIN=$PRODNAMES_DIR/com2/gfs/para/$CDUMP.$PDY
   export COMOUT=$PRODNAMES_DIR/com2/gfs/para/$CDUMP.$PDY
   export COMAWP=$PRODNAMES_DIR/com2/nawips/para/$CDUMP.$PDY
   export pcom=$PRODNAMES_DIR/pcom2/para/$CDUMP.$PDY
   mkdir -p $COMOUT $COMAWP $pcom
   export pid=$$
   export JCAP=${JCAP:-574}
   export LEVS=${LEVS:-64}
   export LATB=${LATB:-880}
   export LONB=${LONB:-1760}
   export STARTHOUR=${STARTHOUR:-00}
   export ENDHOUR=${ENDHOUR:-240}
   export jn=${PSLOT}${CDATE}${CDUMP}bufrsnd
   export out=$COMROT/${jn}.dayfile
   export SENDCOM=YES
   export HOMEgsm=$BASE_GSM
   mac=`hostname |cut -c1`
   if [ $mac = g -o $mac = t ]; then
      export launcher=mpirun.lsf
      $SUB -a $ACCOUNT -e 'GFLUX=$GFLUX,GBUFR=$GBUFR,TOCSBUFR=$TOCSBUFR,launcher=$launcher,HOMEbufr=$HOMEbufr,PARMbufr=$PARMbufr,CDATE=$CDATE,COMIN=$COMIN,COMOUT=$COMOUT,COMAWP=$COMAWP,pcom=$pcom,DATA=$DATA,BASEDIR=$BASEDIR,JCAP=$JCAP,LEVS=$LEVS,LATB=$LATB,LONB=$LONB,STARTHOUR=$STARTHOUR,ENDHOUR=$ENDHOUR,FIXGLOBAL=$FIXGLOBAL,SENDCOM=$SENDCOM,STMP=$STMP' -q $CUE2RUN -p 5/5/N  -r 16000/16/1 -t 3:00:00 -j $jn -o $out $POSTSNDSH
   elif [ $mac = f ]; then
      export launcher=mpiexec_mpt
      CUE2RUN=bigmem
      export KMP_STACKSIZE=2048m
      $SUB -a $ACCOUNT -e machine=$machine,GFLUX=$GFLUX,GBUFR=$GBUFR,TOCSBUFR=$TOCSBUFR,KMP_STACKSIZE=$KMP_STACKSIZE,launcher=$launcher,PARMbufr=$PARMbufr,CDATE=$CDATE,COMROT=$COMROT,COMIN=$COMIN,COMOUT=$COMOUT,COMAWP=$COMAWP,pcom=$pcom,DATA=$DATA,BASEDIR=$BASEDIR,JCAP=$JCAP,LEVS=$LEVS,LATB=$LATB,LONB=$LONB,STARTHOUR=$STARTHOUR,ENDHOUR=$ENDHOUR,FIXGLOBAL=$FIXGLOBAL,SENDCOM=$SENDCOM,STMP=$STMP -q $CUE2RUN -p 3/2/g2  -r 36000/3/1 -t 3:00:00 -j $jn -o $out $POSTSNDSH
   elif [ $machine = WCOSS_C ] ; then
      export launcher=aprun
      export snd_nprocs=12
      export snd_ptile=6
      export snd_nthreads=1
      $SUB -a $ACCOUNT -e 'GFLUX=$GFLUX,GBUFR=$GBUFR,TOCSBUFR=$TOCSBUFR,HOMEbufr=$HOMEbufr,PARMbufr=$PARMbufr,CDATE=$CDATE,COMIN=$COMIN,COMOUT=$COMOUT,COMAWP=$COMAWP,pcom=$pcom,DATA=$DATA,BASEDIR=$BASEDIR,HOMEgsm=$BASE_GSM,JCAP=$JCAP,LEVS=$LEVS,LATB=$LATB,LONB=$LONB,STARTHOUR=$STARTHOUR,ENDHOUR=$ENDHOUR,FIXGLOBAL=$FIXGLOBAL,SENDCOM=$SENDCOM,STMP=$STMP,launcher=$launcher,snd_nprocs=$snd_nprocs,snd_ptile=$snd_ptile,snd_nthreads=$snd_nthreads,machine=$machine' -m $machine -q $CUE2RUN -p 5/5/N  -r 3072/16/1 -t 3:00:00 -j $jn -o $out $POSTSNDSH
   fi

   rdate=$($NDATE -$HRKBUFRSND $CDATE)
   pdyr=`echo $rdate | cut -c1-8`
   cycr=`echo $rdate | cut -c9-10`
   cd $PRODNAMES_DIR/com2/nawips/para
   rm gfs.$pdyr/gfs*_${rdate}*snd 2>/dev/null
   cd $PRODNAMES_DIR/com2/gfs/para/gfs.$pdyr
   rm -r bufr.t${cycr}z 2>/dev/null

fi

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
