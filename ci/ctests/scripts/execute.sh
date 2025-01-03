#!/usr/bin/env bash

set -xe

CASE=$1
JOB=$2
idate=$3

rocotoboot_dryrun=/work2/noaa/global/mterry/rocoto_dryrun/bin/rocotoboot
CASEDIR="RUNTESTS/EXPDIR/${CASE}_${JOB}"
cd "${CASEDIR}"
rm -f *.db
rm -f jobcard                                                                             

job_card=$(yes | ${rocotoboot_dryrun} -d ${CASE}_${JOB}.db -w ${CASE}_${JOB}.xml -v 10 -c ${idate}00 -t ${JOB} 2> jobcard)
cat jobcard | sed '/^{{\|^}}/d' | sed '1d'  > "${CASE}_${JOB}.sub"

#TODO Generalize for batch system (hard coded to slurm)

sbatch < "${CASE}_${JOB}.sub"
