#!/bin/ksh
################################################################################
# This script runs the enkf archive and cleanup.
# Usage: earc.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMROT
#   COMDAY
#   HRKTMP
#   HRKROT
#   HRKSIG
#   HRKSIGG
#   HRKPGBM
#   HRKVFY
#   HRKDAY
#   HRKOCN_NC
#   HRKOCN_ANL
#   HRKOCN_GRB
#   ALIST
#   PBEG
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a  
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
export COMROTTMP=${COMROTTMP:-$COMROT}
eval export COMROT=$COMROTTMP
eval export COMDAY=${COMDAY:-$COMROT}
export RESDIR=${RESDIR:-$COMROT/RESTART}
export ARCH_TO_HPSS=${ARCH_TO_HPSS:-YES}
export ARCH_TO_DISK=${ARCH_TO_DISK:-NO}
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export NWPROD=${NWPROD:-$BASEDIR}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}
ARCHCFSRRSH=${ARCHCFSRRSH:-$BASEDIR/ush/cfsrr/hpss.cfsrr.daily.qsub}
CFSRR_ARCH=${CFSRR_ARCH:-YES}
HRKTMP=${HRKTMP:-24}
HRKRES=${HRKRES:-24}
HRKSIG=${HRKSIG:-120}
HRKSIGG=${HRKSIGG:-120}
HRKPGBM=${HRKPGBM:-48}
HRKROT=${HRKROT:-120}
HRKDAY=${HRKDAY:-${HRKROT:-120}}
HRKVFY=${HRKVFY:-${HRKROT:-120}}
HRKOCN_NC=${HRKOCN_NC:-$HRKSIG}
HRKOCN_ANL=${HRKOCN_ANL:-$HRKROT}
HRKOCN_GRB=${HRKOCN_GRB:-$HRKROT}
HRKETMP=${HRKETMP:-24}
HRKENKF=${HRKENKF:-72}

ARCHCOPY=${ARCHCOPY:-NO}
ARCHDAY=${ARCHDAY:-2}       # impacts delay for online archive only
BACKDATE=$((ARCHDAY*24))    # impacts delay for online archive only

ATARDIR=${ATARDIR:-null}
ATARFILE=${ATARFILE:-$CDATE$CDUMP.tar}
ATARDIR_DISK=${ATARDIR_DISK:-/gpfs/hps/nco/storage/gfs_retro}

#RSTPRODSH=${RSTPRODSH-$NWPROD/runhistory.v2.1.26/ush/rhist_restrict.sh}
RSTPRODSH=${RSTPRODSH-$USHDIR/rhist_restrict.sh}
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

export fhmax_1=${fmax1:-192}
export fhmax_2=${fmax2:-384}
export FHOUT=${FHOUT_ENKF:-3}
export FHMAX=${FHMAX_ENKF:-9}
export io_save=${io_save:-144}
export jo_save=${jo_save:-73}
export pgbf_gfs=${pgbf_gfs:-3}     #resolution of gfs pgbf files saved in HPSS archive, 3-1x1,4-0.5x0.5
export pgbf_gdas=${pgbf_gdas:-4}   #resolution of gdas pgbf files saved in HPSS archive
export pgbf_grid=$(eval echo \$pgbf_$CDUMP)
if [ $pgbf_grid -eq 4 ] ; then
 export flag_pgb=h
elif [ $pgbf_grid -eq 3 ] ; then
 export flag_pgb=f
elif [ $pgbf_grid -eq 2 ] ; then
 export flag_pgb=l
fi
export flag_pgb=${flag_pgb:-q}

export CDUMPE=enkf

SDATE=$CDATE
BDATE=$($NDATE -$BACKDATE $CDATE)    # online archive date only


################################################################################# Copy files to online archive
if [[ $ARCHCOPY = YES ]];then
 if [[ ! -s $ARCDIR ]]; then
    mkdir -p $ARCDIR
 fi

 SPECIALARCHSH=${SPECIALARCHSH:-""}
 if [ ! -z $SPECIALARCHSH ]; then
   if [ -s $SPECIALARCHSH ] ; then
     $SPECIALARCHSH
   fi
   rc=$?
   if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
   $PEND
 fi

 export CDATE=$BDATE

# be sure we are in working directory $DATA
 cd $DATA
# rm *

# return code gets checked for any required files (ARCR)
 $PCOP $CDATE/$CDUMPE/arch/ARCR $COMROT   $DATA <$RLIST
 rc=$?

# dayfiles may not be stored in COMROT...
#    need to work on this to avoid unnecessary errors
 [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMPE/arch/ARCR $COMDAY   $DATA <$RLIST

 ((rc+=$?))

# optional files
 $PCOP $CDATE/$CDUMPE/arch/ARCO $COMROT $DATA <$RLIST
 [ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMPE/arch/ARCO $COMDAY   $DATA <$RLIST

  $NCP *${CDATE}* $ARCDIR/

fi

export CDATE=$SDATE


################################################################################
# Archive to tape.

# export CDATE=$BDATE    # commented out because short hrksigg is sometimes required
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

 for a in EARCA EARCB EARCC EARC06;do
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
     if [ $ARCH_TO_HPSS = YES ]; then $HSI mkdir -p ${ATARDIR}; fi
     if [ $machine = THEIA -o $machine = WCOSS -o $machine = WCOSS_C ] ; then
       np='1/1/S'
       mem='1024/1'
       tl=${TIMELIMEARC:-'3:00:00'}
       qq=${CUE2RUNA:-transfer}
       jn=${PSLOT}${CDATE}${CDUMP}${a}
       out=$COMROT/$jn.dayfile
       trans_local=$COMROT/transfer_${a}_${CDATE}${CDUMP}${CSTEP}
       > $trans_local
       echo "#!/bin/ksh"                >> $trans_local
       echo "set -ax"                   >> $trans_local
       echo "export NDATA=$NDATA"       >> $trans_local
       echo "export HTAR=$HTAR"         >> $trans_local
       echo "export HSI=$HSI"           >> $trans_local
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
       echo "export ARCH_TO_HPSS=$ARCH_TO_HPSS" >> $trans_local
       echo "export ARCH_TO_DISK=$ARCH_TO_DISK" >> $trans_local
       echo ""                          >> $trans_local
       echo "mkdir -p $NDATA||exit 1;cd $NDATA||exit 1" >> $trans_local
       echo "rm *"                      >> $trans_local
       echo "$PCOP $CDATE/$CDUMP/arch/$a $COMROT $NDATA <$RLIST" >> $trans_local
       echo "[ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMP/arch/$a $COMDAY $NDATA <$RLIST" >> $trans_local
       echo "sleep 30" >> $trans_local
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
       echo "$RSTPRODSH $afile $ARCH_TO_HPSS $ARCH_TO_DISK" >> $trans_local
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

# Archive EnKF forecast hours other than FH=6
 export FH=0
 while [ $FH -le $FHMAX ]; do
   if [ $FH -ne 6 ] ; then
     FHR=$FH
     if [ $FH -lt 10 ]; then
       export FHR=0$FH
     fi
     a=EARC${FHR}
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
       if [ $ARCH_TO_HPSS = YES ]; then $HSI mkdir -p ${ATARDIR}; fi
       if [ $machine = THEIA -o $machine = WCOSS -o $machine = WCOSS_C ] ; then
         np='1/1/S'
         mem='1024/1'
         tl=${TIMELIMEARC:-'3:00:00'}
         qq=${CUE2RUNA:-transfer}
         jn=${PSLOT}${CDATE}${CDUMP}${a}
         out=$COMROT/$jn.dayfile
         trans_local=$COMROT/transfer_${a}_${CDATE}${CDUMP}${CSTEP}
         > $trans_local
         echo "#!/bin/ksh"                >> $trans_local
         echo "set -ax"                   >> $trans_local
         echo "export NDATA=$NDATA"       >> $trans_local
         echo "export HTAR=$HTAR"         >> $trans_local
         echo "export HSI=$HSI"           >> $trans_local
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
         echo "export ARCH_TO_HPSS=$ARCH_TO_HPSS" >> $trans_local
         echo "export ARCH_TO_DISK=$ARCH_TO_DISK" >> $trans_local
         echo ""                          >> $trans_local
         echo "mkdir -p $NDATA||exit 1;cd $NDATA||exit 1" >> $trans_local
         echo "rm *"                      >> $trans_local
         echo "$PCOP $CDATE/$CDUMP/arch/$a $COMROT $NDATA <$RLIST" >> $trans_local
         echo "[ $COMDAY != $COMROT ] && $PCOP $CDATE/$CDUMP/arch/$a $COMDAY $NDATA <$RLIST" >> $trans_local
         echo "sleep 30" >> $trans_local
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
   fi
   FH=`expr $FH + $FHOUT`
 done
fi

export CDATE=$SDATE
export GDATE=$SGDATE

################################################################################
# Clean up.

# rm old work directories
rdate=$($NDATE -$HRKETMP $CDATE)
rm -rf $(CDATE=$rdate CDUMP=${CDUMP}e CSTEP='*' eval ls -d $DATATMP) 2>/dev/null

# define function to check that verifications files are archived online before clean up.
chkarc ()
{
  set -x
    ARCDIR=$1
    for verif_file in `ls $rmfiles 2>/dev/null`
    do
      if [ ! -s $ARCDIR/$verif_file ]; then
        set +x
        echo "****  VERIFICATION FILE $verif_file MISSING FROM $ARCDIR"
        echo "****  WILL ATTEMPT TO MOVE $verif_file TO $ARCDIR NOW"
        echo "****  TAPE ARCHIVE SHOULD BE CHECKED"
        set -x
        mv $verif_file $ARCDIR
      fi
    done
}

## for dayfiles, cd to COMDAY to avoid hitting unix line length limit ("Arg list too long.")
cd $COMDAY
rdate=$($NDATE -$HRKDAY $CDATE)
rm $PSLOT$rdate*dayfile* 2>/dev/null

## for other files, cd to COMROT to avoid hitting unix line length limit ("Arg list too long.")

cd $COMROT

# remove enkf files
if [ $DOENKF = YES ] ; then
 rdate=$($NDATE -$HRKENKF $CDATE)
 rm *${rdate}_ensmean 2>/dev/null
 rm *${rdate}_mem*    2>/dev/null
 rm *${rdate}_all     2>/dev/null
 rm *${rdate}_grp*    2>/dev/null
 rm *${rdate}_fhr*    2>/dev/null
 rm enkfstat_${rdate} 2>/dev/null
 rm sigpairs_${rdate} 2>/dev/null
 rm pertdates_${rdate} 2>/dev/null
fi


cd $DATA


################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
