#!/bin/sh

set -x

job_name=$1
rc_file=$2

while [[ $(grep -c '+ rc=0' ${job_name}.out) -ne 1 ]]; do
   grep '+ rc=' ${job_name}.out > ${rc_file}
   if [ -s ${rc_file} ]; then
      if [[ $(stat -c %s ${rc_file}) -ne '0' ]]; then
         if [[ $(awk '{ print $2 }' ${rc_file}) -ne 'rc=0' ]]; then
            echo ${job_name}" job has failed with return code of "$(awk '{ print $2 }' ${rc_file})"."
            rm -f ${rc_file}
            exit
         fi
      fi
   fi
   echo "Job "${job_name}" is not complete yet.  Will recheck in a minute."
   sleep 60
done

if [ "$#" -eq 3 ]; then
   while [[ $(grep -c 'Resource usage summary:' ${job_name}.out) -ne 1 ]]; do
      echo "Job "${job_name}" is not complete yet.  Will recheck in a minute."
      sleep 60
   done
fi

rm -f ${rc_file}

exit
