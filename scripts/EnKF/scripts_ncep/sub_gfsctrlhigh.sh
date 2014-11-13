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

mkdir -p ${datapath}${analdatep1}

echo "SUBMIT HI-RES GFS CNTRL"
$SUB -a GDAS-T2O -g devonprod -j enkf_a5_gfsctrl -o ${current_logdir}/run_gfsctrl.out -p 64/6/N -q dev -r 1640/1 -t 00:35:00 -u ${LOGNAME} -w +0000 ${enkfscripts}/${ctrl_gfs}

exit 0
