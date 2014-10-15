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

set ncycles=10
set ncycle=1
##while ($ncycle <= $ncycles)
echo "ncycle = " $ncycle "and ncycles = " $ncycles

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

# run high res forecast.
set FHMAX_save=$FHMAX
set RUN_save=$RUN
set POSTPROC_save=$POSTPROC
set JCAP_save=$JCAP
set FHOUT_save=$FHOUT
set LONB_save=$LONB
set LATB_save=$LATB
setenv FHMAX 9
setenv FHOUT 1
setenv RUN enkf
setenv POSTPROC NO
setenv DATOUT ${datapath2}
setenv DATA ${datapath2}/chgrestmp.$$

setenv JCAP $JCAP_HIGH
setenv LONB $LONB_HIGH
setenv LATB $LATB_HIGH

#### IC is either just enkf ens mean (which res changed)..
####if ($JCAP_HIGH != $JCAP_save) then
##### Run surface cycle and chgres
####time sh $CYCLESH ${datapathm1}/enkf.t${hrm1}z.bf06  ${datapath2}/sfcanl_${analdate}_control
####$CHGRESSH ${datapath2}/sanl_${analdate}_ensmean /dev/null ${datapath2}/sanl_${analdate}_control /dev/null


setenv SIGO ${DATOUT}/${RUN}.t${hr}z.sf'${FH}'
setenv SFCO ${DATOUT}/${RUN}.t${hr}z.bf'${FH}'
setenv FLXO ${DATOUT}/${RUN}.t${hr}z.sfluxgrbf'${FH}'
setenv SIGI ${datapath}${analdate}/sanl_${analdate}_control
setenv SFCI ${datapath}${analdate}/sfcanl_${analdate}_control
setenv RERUN YES
echo "${analdate} run ${FHMAX} hour single forecast from eda solution `date`"
sh ${enkfscripts}/${drive_ensmean} >&! ${current_logdir}/gfs_fcst.out

unsetenv SIGO
unsetenv SFCO
unsetenv FLXO
echo "${analdate} done running ${FHMAX} hour single forecast `date`"

# do postprocessing
set nbackground = 0
setenv DATA ${datapath2}/postgptmp_00.$$
sh ${enkfscripts}/global_postgp.sh ${datapath2}/sanl_${analdate}_control /dev/null /dev/null ${datapath2}/${RUN}.t${hr}z.pgrbanl /dev/null $IO $JO >&! ${current_logdir}\
/gfs_fcst_pgrbanl.out &
@ nbackground = $nbackground + 1
set fh=$FHOUT
while ($fh <= $FHMAX)
  if ($fh < 10) then
     set fh=`printf %02i $fh`
  endif
  setenv DATA ${datapath2}/postgptmp_${fh}.$$
  sh ${enkfscripts}/global_postgp.sh ${datapath2}/${RUN}.t${hr}z.sf${fh} ${datapath2}/${RUN}.t${hr}z.sfluxgrbf${fh} /dev/null ${datapath2}/${RUN}.t${hr}z.pgrbf${fh} /de\
v/null $IO $JO >&! ${current_logdir}/gfs_fcst_pgrbf${fh}.out &
  @ nbackground = $nbackground + 1
  if ($nbackground == $nbackground_max) then
     echo "waiting at forecast hour $fh"
     wait
     set nbackground = 1
  endif
  @ fh = $fh + $FHOUT
end
sleep 45
wait
/bin/rm -f ${datapath}/postgptmp_*

endif

exit 0
