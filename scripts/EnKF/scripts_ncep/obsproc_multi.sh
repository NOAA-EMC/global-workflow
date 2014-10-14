#!/bin/tcsh

# import parameters
source /global/save/${LOGNAME}/enkf/work/scripts_ncep/current.enkfparms
##########################################################################
# current cycle starts

set username = `whoami`

setenv startupenv "${datapath}/analdate.csh"
source $startupenv

setenv SUB "/u/wx23sm/bin/sub_vapor"
echo "SUB=  ${SUB}"

#------------------------------------------------------------------------
mkdir -p $datapath
mkdir -p $logdir

echo "BaseDir: ${basedir}"
echo "EnKFBin: ${enkfbin}"
echo "DataPath: ${datapath}"
echo "LogDir: ${logdir}"

############################################################################
# Main Program
# Please do not edit the code below; it is not recommended except lines relevant to getsfcensmean.csh.

env
echo "starting the cycle"

set ncycles=10
set ncycle=1
echo "ncycle = " $ncycle "and ncycles = " $ncycles

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`
setenv ANALHR $hr
# set environment analdate
setenv datapath2 "${datapath}/${analdate}/"

# current analysis time.
setenv analdate $analdate
# previous analysis time.
setenv analdatem1 `${incdate} $analdate -$ANALINC`
# next analysis time.
setenv analdatep1 `${incdate} $analdate $ANALINC`
setenv datapathprev "${datapath}/${analdatem1}/"
setenv hrp1 `echo $analdatep1 | cut -c9-10`
setenv hrm1 `echo $analdatem1 | cut -c9-10`
setenv datapathp1 "${datapath}/${analdatep1}/"
setenv datapathm1 "${datapath}/${analdatem1}/"
mkdir -p $datapathp1

date
echo "analdate minus 1: $analdatem1"
echo "analdate: $analdate"
echo "analdate plus 1: $analdatep1"

# make log dir for analdate
setenv current_logdir "${logdir}/ensda_out_${analdate}"
echo "Current LogDir: ${current_logdir}"
mkdir -p ${current_logdir}

set alldone='no'
echo "${analdate} compute forward operator `date`"

setenv SETUP "lread_obs_save=.false.,lread_obs_skip=.true.,lwrite_predterms=.true.,"

## Submit chunks of jobs based on nanals/nmem_per_node settings
 @ num_jobs = $nanals / $nmem_per_node
 set max_jobs=50
 if ($num_jobs >= $max_jobs) then
    echo "THERE ARE TOO MANY JOB STEPS REQUIRED, NUM_JOBS = $num_jobs"
    echo "CHECK NANALS & NMEM_PER_NODE IN CONFIG"
    echo "STOPPING SCRIPTS HERE"
    exit 1
 else
    echo "there are $num_jobs job steps required for first ges processing"
 endif

 set job=1
 set n1=1
 set n2=$nmem_per_node

 while ($job <= $num_jobs)
   setenv NSTART $n1
   setenv NEND $n2
   echo "NSTART=$NSTART"
   echo "NEND=$NEND"
   $SUB -a GDAS-T2O -e NSTART,NEND,SETUP -g devonprod -j enkf_obs$job -o ${current_logdir}/run_obs$job.out -p 32/1/N -q dev -r 4096/1 -t 00:40:00 -u ${LOGNAME} -w +0000 ${enkfscripts}/${runobs}

   @ job = $job + 1
   @ n1 = $n1 + $nmem_per_node
   @ n2 = $n2 + $nmem_per_node

   if ($n2 > $nanals) then
     @ n2 = $nanals
   else
##
   endif
 end

# submit script to do file checking, setup to wait 15 minutes for now
$SUB -a GDAS-T2O -g devonprod -j enkf_check_obsfiles -o ${current_logdir}/check_obs_files.out -q dev -r 1000 -t 03:00:00 -u ${LOGNAME} -w +0015 ${enkfscripts}/check_obsfiles_serial.sh

exit 0
