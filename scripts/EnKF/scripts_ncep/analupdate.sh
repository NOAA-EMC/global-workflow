#!/bin/tcsh
#@ shell = /usr/bin/tcsh
#@ job_name = enkfa_update
#@ output = $(job_name).$(jobid).out
#@ error  = $(job_name).$(jobid).err
#@ notification = never
#@ job_type = parallel
#@ wall_clock_limit = 01:30:00
#@ class = dev
#@ group = devonprod
#@ account_no = GDAS-T2O
#@ tasks_per_node = 48
#@ environment = COPY_ALL
#@ node = 8
#@ node_usage = not_shared
#@ node_resources = ConsumableMemory(110GB)
#@ task_affinity = cpu(1)
#@ network.MPI = csss,shared,us
#@ queue

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Environment variables from Carolyn
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_RFIFO_SIZE=16777216
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
#RDMA specific tunables:
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

source /global/save/${LOGNAME}/enkf/work/scripts_ncep/current.enkfparms

##########################################################################
# current cycle starts

set username = `whoami`

setenv startupenv "${datapath}/analdate.csh"
source $startupenv

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
# copy hostfileall to working dir.

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

set niter=1
set alldone='no'
echo "${analdate} compute analysis increment `date`"
# need symlinks for satbias_angle, satbias_in, satinfo

##ln -fs ${datapath2}/abias ${datapath2}/satbias_in

setenv COMINGES /global/noscrub/${LOGNAME}/bias_prd09q1o
setenv ABIAS ${COMINGES}/biascr.${datdump}.${analdate}
ln -fs ${COMINGES}/biascr.${datdump}.${analdate} ${datapath2}/satbias_in
ln -fs ${datapath2}/satang ${datapath2}/satbias_angle
##setenv ABIAS ${datapathp1}/${PREINP1}abias

ln -fs ${CONVINFO} ${datapath2}/convinfo
ln -fs ${SATINFO} ${datapath2}/satinfo
ln -fs ${OZINFO} ${datapath2}/ozinfo

set nanal=1
set filemissing='no'
while ($nanal <= $nanals)
 set charnanal="mem"`printf %03i $nanal`
 set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
  if { /bin/test ! -s $analfile } set filemissing='yes'
 @ nanal = $nanal + 1
end
##if { /bin/test ! -s $ABIAS } set filemissing='yes'

if($filemissing == 'yes') then
  while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
     csh ${enkfscripts}/${ensda} >&! ${current_logdir}/ensda.out
     set exitstat=$status
     echo "exit status $exitstat"
    else
     csh ${enkfscripts}/${ensda} >>& ${current_logdir}/ensda.out
     set exitstat=$status
    endif
    if ($exitstat == 0) then
       set alldone='yes'
    else
       echo "some files missing, try again .."
       @ niter = $niter + 1
    endif
  end
else
   set alldone='yes'
endif

if($alldone == 'no') then
    echo "Tried ${nitermax} times to run ensda and failed: ${analdate}"
    exit 1
endif
echo "${analdate} done computing analysis increment `date`"

# create ensemble mean sig anal files (runobs.csh creates
# ensemble mean surface file).
 
if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && { /bin/test ! -s ${datapath}/${analdate}/sanl_${analdate}_ensmean })) then
   ${enkfexec}/getsigensmean.x ${datapath}/${analdate}/ sanl_${analdate}_ensmean sanl_${analdate} ${nanals} anal
endif

##setenv PGMOUT ${current_logdir}/chgres_sfccycle_control.out
# ps tend diagnostics on ensemble member 1
###time ${enkfexec}/getpstend.x ${datapath}/${analdate}/ ${analdate} 1

## DTK This has been moved back to "gfscontrol.sh"
#set LONB_save=$LONB
#set LATB_save=$LATB
#set JCAP_save=$JCAP
#setenv LONB $LONB_HIGH
#setenv LATB $LATB_HIGH
#setenv JCAP $JCAP_HIGH
# Run surface cycle and chgres
#time sh $CYCLESH ${datapathm1}/enkf.t${hrm1}z.bf06  ${datapath2}/sfcanl_${analdate}_control
#$CHGRESSH ${datapath2}/sanl_${analdate}_ensmean /dev/null ${datapath2}/sanl_${analdate}_control /dev/null

#setenv LONB $LONB_save
#setenv LATB $LATB_save
#setenv JCAP $JCAP_save

set nanal=1
set filemissing='no'
while ($nanal <= $nanals)
 set charnanal="mem"`printf %03i $nanal`
 set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
  if { /bin/test ! -s $analfile } set filemissing='yes'
 @ nanal = $nanal + 1
end

#set charnanal=control
#set analfile="${datapath2}/sanl_${analdate}_${charnanal}"
#set sfcanlfile="${datapath2}/sfcanl_${analdate}_${charnanal}"
#if { /bin/test ! -s $analfile } set filemissing='yes'
#if { /bin/test ! -s $sfcanlfile } set filemissing='yes'

if($filemissing == 'yes') then
  echo "FILES ARE MISSING, CANNOT SUBMIT FIRST GUESS FOR NEXT CYCLE.  STOPPING HERE!"
else
    tcsh ${enkfscripts}/firstges_multi.sh
endif

exit 0
