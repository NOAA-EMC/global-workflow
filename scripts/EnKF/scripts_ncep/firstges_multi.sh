#!/bin/tcsh

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

set ncycle=1

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`
setenv ANALHR $hr
# set environment analdate
setenv datapath2 "${datapath}/${analdate}/"
# copy hostfileall to working dir.
##/bin/cp -f ${datapath}/hostfileall ${datapath2}

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


# run ensemble first guess.
# first, clean up old first guesses.
if ($cleanup_fg == 'true') then
set fhr=$FHMIN
while ( $fhr <= $FHMAX)
    set charfhr="fhr`printf %02i $fhr`"
    /bin/rm -f ${datapath}${analdatep1}/sfg_${analdatep1}_${charfhr}*_* 
    /bin/rm -f ${datapath}${analdatep1}/bfg_${analdatep1}_${charfhr}*_* 
    @ fhr = $fhr + $FHOUT
end
endif
mkdir -p ${datapath}${analdatep1}

## BITS FOR (HIRES) CONTROL FORECAST JOB
set LONB_save=$LONB
set LATB_save=$LATB
set JCAP_save=$JCAP
setenv LONB $LONB_HIGH
setenv LATB $LATB_HIGH
setenv JCAP $JCAP_HIGH

## FILES FOR SFC CYCLE ETC
setenv COMIN "${obs_datapath}/${analdate}/${datdump}"
setenv FNTSFA "${COMIN}/sstgrb.${datdump}.${analdate}"
setenv FNACNA "${COMIN}/icegrb.${datdump}.${analdate}"
setenv FNSNOA "${COMIN}/snogrb.${datdump}.${analdate}"


set filemissing='no'
set charnanal=control
set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
set sfcanlfile="${datapath2}/sfcanl_${analdate}_${charnanal}"
if { /bin/test ! -s $analfile } set filemissing='yes'
if { /bin/test ! -s $sfcanlfile } set filemissing='yes'

if($filemissing == 'yes') then
# Run surface cycle and chgres
  cd ${datapath2}
  setenv PGMOUT ${current_logdir}/chgres_sfccycle_control.out
  time sh $CYCLESH ${datapathm1}/enkf.t${hrm1}z.bf06  ${datapath2}/sfcanl_${analdate}_control
  $CHGRESSH ${datapath2}/sanl_${analdate}_ensmean /dev/null ${datapath2}/sanl_${analdate}_control /dev/null
  cd ${enkfscripts}
else
  echo "FILES ALREADY APPEAR TO EXIST"
endif

setenv LONB $LONB_save
setenv LATB $LATB_save
setenv JCAP $JCAP_save

# CHECK AGAIN
set filemissing='no'
set charnanal=control
set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
set sfcanlfile="${datapath2}/sfcanl_${analdate}_${charnanal}"
if { /bin/test ! -s $analfile } set filemissing='yes'
if { /bin/test ! -s $sfcanlfile } set filemissing='yes'

if($filemissing == 'yes') then
  echo "FILES ARE MISSING, CANNOT SUBMIT CTRL FCST JOB"
else
  echo "SUBMIT HI-RES GFS CNTRL"
  $SUB -a GDAS-T2O -g devonprod -j enkf_gfsctrl -o ${current_logdir}/run_gfsctrl.out -p 64/6/N -q dev -r 1640/1 -t 00:20:00 -u ${LOGNAME} -w +0000 ${enkfscripts}/${ctrl_gfs}
endif

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
   $SUB -a GDAS-T2O -e NSTART,NEND -g devonprod -j enkf_fg$job -o ${current_logdir}/run_fg$job.out -p 32 -q dev -r 1000 -t 00:45:00 -u ${LOGNAME} -w +0000 ${enkfscripts}/${fg_gfs}

   @ job = $job + 1
   @ n1 = $n1 + $nmem_per_node
   @ n2 = $n2 + $nmem_per_node

   if ($n2 > $nanals) then
     @ n2 = $nanals
   else
##
   endif
 end

# submit script to do file checking, set to wait 10 minutes before checking files
$SUB -a GDAS-T2O -g devonprod -j enkf_check_fgfiles -o ${current_logdir}/check_fg_files.out -q dev -r 1000 -t 03:00:00 -u ${LOGNAME} -w +0010 ${enkfscripts}/check_fgfiles_serial.sh
exit 0
