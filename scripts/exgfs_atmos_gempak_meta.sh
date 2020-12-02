#!/bin/ksh

set -x

msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

cd $DATA

GEMGRD1=${RUN}_${PDY}${cyc}f
#find out what fcst hr to start processing
fhr=$fhend

export numproc=23

while [ $fhr -ge $fhbeg ] ; do
   typeset -Z3 fhr
   ls -l $COMIN/$GEMGRD1${fhr}
   err1=$?
   if [ $err1 -eq 0 -o $fhr -eq $fhbeg ] ; then
      break
   fi
   fhr=`expr $fhr - $fhinc`
done

maxtries=180
first_time=0
do_all=0

#loop through and process needed forecast hours
while [ $fhr -le $fhend ]
do
   # 
   # First check to see if this is a rerun.  If so make all Meta files
   if [ $fhr -gt 126 -a $first_time -eq 0 ] ; then
     do_all=1
   fi
   first_time=1

   if [ $fhr -eq 120 ] ; then
      fhr=126
   fi
   icnt=1

   while [ $icnt -lt 1000 ]
   do
      ls -l $COMIN/$GEMGRD1${fhr}
      err1=$?
      if [ $err1 -eq 0 ] ; then
         break
      else
         sleep 20
         let "icnt= icnt + 1"
      fi
      if [ $icnt -ge $maxtries ]
      then
         msg="ABORTING after 1 hour of waiting for gempak grid F$fhr to end."
         postmsg "${jlogfile}" "$msg"
         export err=7 ; err_chk
         exit $err
      fi
   done

   export fhr

   ########################################################
   # Create a script to be poe'd
   #
   #  Note:  The number of scripts to be run MUST match the number
   #  of total_tasks set in the ecf script, or the job will fail.
   #
#   if [ -f $DATA/poescript ]; then
      rm $DATA/poescript
#   fi

   if [ $fhr -lt 100 ] ; then
      typeset -Z2 fhr
   fi

   if [ $do_all -eq 1 ] ; then
     do_all=0
     awk '{print $1}' $FIXgempak/gfs_meta > $DATA/tmpscript
   else
     #
     #     Do not try to grep out 12, it will grab the 12 from 126.
     #     This will work as long as we don't need 12 fhr metafiles
     #
     if [ $fhr -ne 12 ] ; then
       grep $fhr $FIXgempak/gfs_meta |awk -F" [0-9]" '{print $1}' > $DATA/tmpscript
     fi
   fi

   for script in `cat $DATA/tmpscript`
   do
     eval "echo $script" >> $DATA/poescript
   done

   num=`cat $DATA/poescript |wc -l`

   while [ $num -lt $numproc ] ; do
      echo "hostname" >>poescript
      num=`expr $num + 1`
   done

   chmod 775 $DATA/poescript
   cat $DATA/poescript
   export MP_PGMMODEL=mpmd
   export MP_CMDFILE=$DATA/poescript

#  If this is the final fcst hour, alert the
#  file to all centers.
# 
   if [ $fhr -ge $fhend ] ; then
      export DBN_ALERT_TYPE=GFS_METAFILE_LAST
   fi

   export fend=$fhr

  sleep 20
#   mpirun.lsf
  ntasks=${NTASKS_META:-$(cat $DATA/poescript | wc -l)}
  ptile=${PTILE_META:-4}
  threads=${NTHREADS_META:-1}
  export OMP_NUM_THREADS=$threads
  APRUN="mpirun -n $ntasks cfp "

  APRUN_METACFP=${APRUN_METACFP:-$APRUN}
  APRUNCFP=$(eval echo $APRUN_METACFP)

  $APRUNCFP $DATA/poescript
  export err=$?; err_chk

      typeset -Z3 fhr
      if [ $fhr -eq 126 ] ; then
        let fhr=fhr+6
      else
	let fhr=fhr+fhinc
      fi
done

#####################################################################
# GOOD RUN
set +x
echo "**************JOB GFS_META COMPLETED NORMALLY on the IBM-SP"
echo "**************JOB GFS_META COMPLETED NORMALLY on the IBM-SP"
echo "**************JOB GFS_META COMPLETED NORMALLY on the IBM-SP"
set -x
#####################################################################

echo EXITING $0
exit
#
