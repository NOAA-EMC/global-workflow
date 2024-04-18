#! /usr/bin/env bash

#
# Script name:         exgfs_pmgr.sh.sms
#
#  This script monitors the progress of the gfs_fcst job
#

source "${USHgfs}/preamble.sh"

hour=00

case $RUN in
  gfs)
    TEND=384
    TCP=385
    ;;
  gdas)
    TEND=9
    TCP=10
    ;; 
esac

if [ -e posthours ]; then
  rm -f posthours
fi

while [ $hour -lt $TCP ]; do
  hour=$(printf "%02d" $hour)
  echo $hour >>posthours
  if [ 10#$hour -lt 120 ]; then
    let "hour=hour+1"
  else
    let "hour=hour+3"
  fi
done
postjobs=$(cat posthours)

#
# Wait for all fcst hours to finish 
#
icnt=1
while [ $icnt -lt 1000 ]; do
  for fhr in $postjobs; do 
    fhr3=$(printf "%03d" $fhr)   
    if [ -s ${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${fhr3}.txt ]; then
      if [ $fhr -eq 0 ]; then 
        ####        ecflow_client --event release_${RUN}_postanl
        ecflow_client --event release_postanl
      fi    
      ####      ecflow_client --event release_${RUN}_post${fhr}
      ecflow_client --event release_post${fhr3}
      # Remove current fhr from list
      postjobs=$(echo $postjobs | sed "s/${fhr}//")
    fi
  done

  result_check=$(echo $postjobs | wc -w)
  if [ $result_check -eq 0 ]; then
    break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 1080 ]; then
    msg="ABORTING after 3 hours of waiting for ${RUN} FCST hours $postjobs."
    err_exit $msg
  fi
done


exit
