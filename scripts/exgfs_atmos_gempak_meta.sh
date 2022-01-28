#!/bin/ksh

set -x

echo "JOB $job HAS BEGUN"

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
         echo "ABORTING after 1 hour of waiting for gempak grid F$fhr to end."
         export err=7 ; err_chk
         exit $err
      fi
   done

   export fhr=$fhr

   ########################################################
   # Create a script to be poe'd
   #
   #  Note:  The number of scripts to be run MUST match the number
   #  of total_tasks set in the ecf script, or the job will fail.
   #
#   if [ -f $DATA/poescript ]; then
      rm $DATA/poescript
#   fi

   awk '{print $1}' $FIXgempak/gfs_meta > $DATA/tmpscript

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

   export DBN_ALERT_TYPE=GFS_METAFILE_LAST

   export fend=$fhr

  sleep 20
  ntasks=${NTASKS_META:-$(cat $DATA/poescript | wc -l)}
  ptile=${PTILE_META:-4}
  threads=${NTHREADS_META:-1}
  export OMP_NUM_THREADS=$threads
  APRUN="mpiexec -l -n $ntasks -ppn $ntasks --cpu-bind verbose,core cfp"
  APRUN_METACFP=${APRUN_METACFP:-$APRUN}
  APRUNCFP=$(eval echo $APRUN_METACFP)

  $APRUNCFP $DATA/poescript
  export err=$?; err_chk

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
