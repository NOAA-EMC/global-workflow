#! /bin/ksh
#
# Script name:         exgfs_pmgr.sh.sms
#
#  This script monitors the progress of the gfs_fcst job
#
set -x

hour=00
typeset -Z2 hour
TEND=384
TCP=385

if [ -e posthours ]; then
   rm -f posthours
fi

while [ $hour -lt $TCP ]; 
do
  echo $hour >>posthours
  if [ $hour -lt 240 ]
  then
     if [ $hour -eq 99 ]
     then
       typeset -Z3 hour
     fi
     # JY if [ $hour -lt 12 ]
     if [ $hour -lt 120 ]
     then
       let "hour=hour+1"
     else
       let "hour=hour+3"
     fi
  else
     let "hour=hour+12"
  fi
done
postjobs=`cat posthours`

#
# Wait for all fcst hours to finish 
#
icnt=1
while [ $icnt -lt 1000 ]
do
  for fhr in $postjobs
  do 
    fhr3=`printf "%03d" $fhr`   
    if [ -s ${COMIN}/gfs.${cycle}.logf${fhr}.txt -o  -s ${COMIN}/gfs.${cycle}.logf${fhr3}.txt ]
    then
      if [ $fhr -eq 0 ]
      then 
        ecflow_client --event release_postanl
      fi    
      ecflow_client --event release_post${fhr}
      # Remove current fhr from list
      postjobs=`echo $postjobs | sed "s/${fhr}//"`
    fi
  done
  
  result_check=`echo $postjobs | wc -w`
  if [ $result_check -eq 0 ]
  then
     break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 720 ]
  then
    msg="ABORTING after 2 hours of waiting for GFS FCST hours $postjobs."
    err_exit $msg
  fi

done

echo Exiting $0

exit
