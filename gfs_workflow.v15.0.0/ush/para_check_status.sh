#!/bin/ksh

# Set variables based on run time input
PSLOT=$1
CDATE=$2
CDUMP=$3


# Define local variables
export ARCDIR=${ARCDIR_SAVE:-$NOSCRUB/$LOGNAME/archive/pr$PSLOT}
export COMROT=${ROTDIR:-$PTMP/$LOGNAME/pr$PSLOT}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export vsdbsave=${vsdbsave:-$NOSCRUB/$LOGNAME/archive/vsdb_data}
export FIT_DIR=${FIT_DIR:-$ARCDIR/fits}
export PARA_CHECK_BACKUP=${PARA_CHECK_BACKUP:-72}
export QSTAT=${QSTAT:-/u/emc.glopara/bin/qjob}
export PARA_CHECK_HPSS_LIST=${PARA_CHECK_HPSS_LIST:-"gfs gdas gdas.enkf.obs gdas.enkf.anl gdas.enkf.fcs06"}


# Back up PARA_CHECK_BACKUP from CDATE.  
export BDATE=`$NDATE -${PARA_CHECK_BACKUP} $CDATE`
export BDATE_HPSS=`$NDATE -12 $CDATE`
export EDATE=$CDATE


# Check parallel status
echo "Check pr$PSLOT for $CDUMP $BDATE to $EDATE at `date`"

echo " "
echo "Check jobs queue"
$QSTAT | grep $PSLOT 

echo " "
echo "Check for abend jobs"
cd $COMROT
ls -ltr *dayfile*abend* | tail -10

echo " "
echo "Check for job errors"
cd $COMROT
CDATE=$BDATE
while [[ $CDATE -le $EDATE ]]; do
  for file in `ls *${CDATE}*dayfile`; do
    if (($(egrep -c "job killed" $file) > 0 || $(egrep -c "segmentation fault" $file) > 0));then
      echo " $file KILLED or SEG FAULT"
    fi
# additional error checking
    if (($(egrep -c "Exec format error" $file) > 0));then
      echo " Exec format error on $file "
    fi
    if (($(egrep -c "Abort trap signal" $file) > 0));then
      echo " Abort trap signal on $file "
    fi
    if (($(egrep -c "Fatal error" $file) > 0));then
      echo " Fatal error on $file "
    fi
    if (($(egrep -c "channel initialization failed" $file) > 0));then
      echo " MPI channel initialization failed on $file "
    fi
    if (($(egrep -c "trap signal" $file) > 0));then
      echo " trap signal condition happened on $file "
    fi
    if (($(egrep -c "OOM killer terminated this process" $file) > 0));then
      echo " OOM killer terminated this process on $file "
    fi
    if (($(egrep -c "Application aborted" $file) > 0));then
      echo " Application aborted on $file "
    fi
    if (($(egrep -c "Job not submitted" $file) > 0));then
      echo " Job not submitted on $file "
    fi
    if (($(egrep -c "nemsio_open failed" $file) > 0));then
      echo " nemsio_open failed on $file "
    fi
    if (($(egrep -c "Caught signal Terminated" $file) > 0));then
      echo " Caught signal Terminated on $file "
    fi
#    
    if (($(egrep -c "msgtype=ERROR" $file) > 0));then
      echo " $file had msgtype=ERROR"
    fi
    if (($(egrep -c "quota exceeded" $file) > 0));then
      echo " $file exceeded DISK QUOTA"
    fi
    if (($(egrep -c "missing or inaccessible and could not be copied to the archive" $file) > 0));then
      echo " HPSS imcomplete in $file "
    fi
  done
  ADATE=`$NDATE +06 $CDATE`
  CDATE=$ADATE
done

echo " "
echo "Check rain"
cd $ARCDIR
ls -l *rain* | tail -10

echo " "
echo "Check vsdb"
cd $vsdbsave/anom/00Z/pr$PSLOT
ls -l | tail -10

echo " "
echo "Check fits"
cd $FIT_DIR
CDATE=$BDATE
while [[ $CDATE -le $EDATE ]]; do
   echo "$CDATE `ls *${CDATE}* |wc -l`"
   ADATE=`$NDATE +06 $CDATE`
   CDATE=$ADATE
done

echo " "
echo "Check $ARCDIR"
cd $ARCDIR
rm -rf $TMPDIR/temp.disk
ls > $TMPDIR/temp.disk
CDATE=$BDATE
while [[ $CDATE -le $EDATE ]]; do
   echo "$CDATE `grep $CDATE $TMPDIR/temp.disk | wc -l`"
   ADATE=`$NDATE +06 $CDATE`
   CDATE=$ADATE
done

echo " "
echo "Check HPSS jobs"
cd $COMROT
CDATE=$BDATE_HPSS
while [ $CDATE -le $EDATE ] ; do
   for file in `ls *${CDATE}*ARC*dayfile`; do
      count=`grep "HTAR: HTAR FAIL" $file | wc -l`
      if [ $count -gt 0 ]; then
         echo `grep "HTAR: HTAR FAIL" $file` $file
      else
         count=`grep "HTAR: HTAR SUCC" $file | wc -l`
         if [ $count -gt 0 ]; then
# check on htar verification
            count_vy=`grep "HTAR: Verify complete" $file | wc -l`
            if [ $count_vy -gt 0 ]; then
              rate=`grep "MB/s"  $file |awk '{print "HPSS transfer rate:", $20, $21}'`
              echo `grep "HTAR: HTAR SUCC" $file` $file " verified $rate"
            else
              echo "HTAR FAILED to verify $file"
            fi
         else
            count=`grep "defined signal" $file | wc -l`
            if [ $count -gt 0 ]; then
               echo `grep "HTAR: HTAR signal FAIL" $file` $file
            else
               echo "$file RUNNING"
            fi
         fi
      fi
   done
   adate=`$NDATE +06 $CDATE`
   CDATE=$adate
done

echo " "
echo "Check $ATARDIR"
rm -rf $TMPDIR/temp.hpss
hsi -P "ls -l $ATARDIR" > $TMPDIR/temp.hpss
for type in $PARA_CHECK_HPSS_LIST; do
   CDATE=$BDATE_HPSS
   while [ $CDATE -le $EDATE ] ; do
      grep "${CDATE}$type.tar" $TMPDIR/temp.hpss
      adate=`$NDATE +06 $CDATE`
      CDATE=$adate
   done
   echo " "
done

# Check NOSCRUB, STMP, and PTMP usage
echo "Check quotas"
if [ $machine = WCOSS ] ; then
   grep "emc-global" /ptmpd1/fsets/${HOST}.filesets  | sort -n -r -k7
   grep "tmp-"       /ptmpd1/fsets/${HOST}.filesets  | sort -n -r -k7
   grep "emc-global" /ptmpp2/fsets/${HOST}2.filesets | sort -n -r -k7
   grep "tmp-"       /ptmpp2/fsets/${HOST}2.filesets | sort -n -r -k7
elif [ $machine = WCOSS_C ] ; then
   host=$(echo $SITE | tr '[A-Z]' '[a-z]')
   grep "hps-ptmp"           /gpfs/hps/ptmp/fsets/${host}.filesets
   grep "hps-stmp"           /gpfs/hps/ptmp/fsets/${host}.filesets
   grep "hps-emc-global-sv"  /gpfs/hps/ptmp/fsets/${host}.filesets
   grep "hps-emc-global-ns"  /gpfs/hps/ptmp/fsets/${host}.filesets
   grep "hps-emc-global-dum" /gpfs/hps/ptmp/fsets/${host}.filesets
fi

exit






