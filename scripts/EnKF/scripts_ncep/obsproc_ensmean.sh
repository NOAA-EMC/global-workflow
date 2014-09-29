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

echo "starting ens mean computation `date`"
set fh=${FHMIN}
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && { /bin/test ! -s ${datapath}/${analdate}/bfg_${analdate}_${charfhr}_ensmean })) then
  ${enkfexec}/getsfcensmean.x ${datapath}/${analdate}/ bfg_${analdate}_${charfhr}_ensmean bfg_${analdate}_${charfhr} ${nanals} 
  endif
  @ fh = $fh + $FHOUT
end
wait

set fh=$FHOUT # use this if getpstend is run
while ($fh <= $FHMAX)
  set charfhr="fhr`printf %02i $fh`"
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && { /bin/test ! -s ${datapath}/${analdate}/sfg_${analdate}_${charfhr}_ensmean })) then
  ${enkfexec}/getsigensmean.x ${datapath}/${analdate}/ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals} ${charfhr}
  endif
  @ fh = $fh + $FHOUT
end
wait
echo "done ens mean computation `date`"

# remove obsfiles from the previous cycle
if ($cleanup_obs == 'true') then
   /bin/rm -f ${datapathp1}/*abias
   /bin/rm -f ${datapathp1}/*satang
   /bin/rm -f ${datapath2}/diag*ensmean
   set nanal=1
   while ($nanal <= $nanals)
     set charnanal="mem`printf %03i $nanal`"
     /bin/rm -f  ${datapath2}/diag*${charnanal}
     @ nanal = $nanal + 1
   end
   /bin/rm -f ${datapath2}/hxprime*
endif

set niter=1
set alldone='no'
echo "${analdate} compute forward operator `date`"

setenv NSTART 61
setenv NEND 61
setenv SETUP "lread_obs_save=.true.,lread_obs_skip=.false.,lwrite_predterms=.true.,"
echo "NSTART=$NSTART"
echo "NEND=$NEND"
$SUB -a GDAS-T2O -e NSTART,NEND,SETUP -g devonprod -j enkf_a5_obsensmean -o ${current_logdir}/run_obsensmean.out -p 32/1/N -q dev -r 4096/1 -t 00:40:00 -u ${LOGNAME} -w +0000 ${enkfscripts}/${runobs}

exit 0
