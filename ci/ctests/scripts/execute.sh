#!/usr/bin/env bash

CASE=$1
JOB=$2
idate=$3

rocotoboot_dryrun=/work2/noaa/global/mterry/rocoto_dryrun/bin/rocotoboot
job_card=$(${rocotoboot_dryrun} -d ${CASE}_${JOB}.db -w ${CASE}/${JOB}.xml -v 10 -c ${idate}00 -t ${JOB})

echo -e "scheduler job-card:\n${job_card}"
exit 0
