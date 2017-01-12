# main driver script

# allow this script to submit other scripts on WCOSS
unsetenv LSB_SUB_RES_REQ 

source $datapath/fg_only.csh # define fg_only variable.
echo "nodes = $NODES"

# if fg_only == true, reset IAU to false
if ($fg_only == "true") then
   setenv IAU .false.
endif

setenv startupenv "${datapath}/analdate.csh"
source $startupenv

# add SATINFO here (instead of submit.sh) since it may depend on analysis time.
setenv SATINFO ${fixgsi}/global_satinfo.txt.2016020312

#------------------------------------------------------------------------
mkdir -p $datapath

echo "BaseDir: ${basedir}"
echo "EnKFBin: ${enkfbin}"
echo "DataPath: ${datapath}"

############################################################################
# Main Program
# Please do not edit the code below; it is not recommended except lines relevant to getsfcensmean.csh.

env
echo "starting the cycle"

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`
setenv ANALHR $hr
# set environment analdate
setenv datapath2 "${datapath}/${analdate}/"
/bin/cp -f ${ANAVINFO_ENKF} ${datapath2}/anavinfo

setenv mpitaskspernode `python -c "import math; print int(math.ceil(float(${nanals})/float(${NODES})))"`
if ($mpitaskspernode < 1) setenv mpitaskspernode 1
setenv OMP_NUM_THREADS `expr $corespernode \/ $mpitaskspernode`
echo "mpitaskspernode = $mpitaskspernode threads = $OMP_NUM_THREADS"
setenv nprocs $nanals
if ($machine != 'wcoss') then
    # HOSTFILE is machinefile to use for programs that require $nanals tasks.
    # if enough cores available, just one core on each node.
    # NODEFILE is machinefile containing one entry per node.
    setenv HOSTFILE $datapath2/machinesx
    setenv NODEFILE $datapath2/nodefile
    cat $PBS_NODEFILE | uniq > $NODEFILE
    if ($NODES >= $nanals) then
      ln -fs $NODEFILE $HOSTFILE
    else
      # otherwise, leave as many cores empty as possible
      awk "NR%${OMP_NUM_THREADS} == 1" ${PBS_NODEFILE} >&! $HOSTFILE
    endif
endif

# current analysis time.
setenv analdate $analdate
# previous analysis time.
setenv analdatem1 `${incdate} $analdate -$ANALINC`
# next analysis time.
setenv analdatep1 `${incdate} $analdate $ANALINC`
setenv hrp1 `echo $analdatep1 | cut -c9-10`
setenv hrm1 `echo $analdatem1 | cut -c9-10`
setenv hr `echo $analdate | cut -c9-10`
setenv datapathp1 "${datapath}/${analdatep1}/"
setenv datapathm1 "${datapath}/${analdatem1}/"
mkdir -p $datapathp1
setenv CDATE $analdate

date
echo "analdate minus 1: $analdatem1"
echo "analdate: $analdate"
echo "analdate plus 1: $analdatep1"

# make log dir for analdate
setenv current_logdir "${datapath2}/logs"
echo "Current LogDir: ${current_logdir}"
mkdir -p ${current_logdir}

if ($fg_only == 'false' && $readin_localization == ".true.") then
   /bin/rm -f $datapath2/hybens_locinfo
   /bin/rm -f $datapath2/hybens_smoothinfo
   if ( $?HYBENSLOC ) then
      /bin/cp -f ${HYBENSLOC} ${datapath2}/hybens_locinfo
   endif
   if ( $?HYBENSMOOTH ) then
      /bin/cp -f ${HYBENSMOOTH} $datapath2/hybens_smoothinfo
   endif
endif

setenv PREINP "${RUN}.t${hr}z."
setenv PREINP1 "${RUN}.t${hrp1}z."
setenv PREINPm1 "${RUN}.t${hrm1}z."

if ($fg_only ==  'false') then

echo "$analdate starting ens mean fcst computation `date`"
csh ${enkfscripts}/compute_ensmean_fcst.csh >&!  ${current_logdir}/compute_ensmeans_fcst.out
echo "$analdate done computing ensemble mean fcsts `date`"

# single res hybrid, just symlink ensmean to control (no separate control forecast)
set fh=0
while ($fh <= $FHMAX)
  set fhr=`printf %02i $fh`
  ln -fs $datapath2/sfg_${analdate}_fhr${fhr}_ensmean $datapath2/sfg_${analdate}_fhr${fhr}_control
  ln -fs $datapath2/bfg_${analdate}_fhr${fhr}_ensmean $datapath2/bfg_${analdate}_fhr${fhr}_control
  @ fh = $fh + $FHOUT
end

# run control and enkf analysse in background

cd $datapath2
if ($machine != 'wcoss') then
    split -d -l $cores_gsi -a 1 $PBS_NODEFILE hostfile
endif

# control analysis in background.
if ($enkfonly == "false") then
   echo "$analdate run hybrid `date`"
   if ($machine != 'wcoss') setenv hostfilein  ${datapath2}/hostfile0
   csh ${enkfscripts}/run_gsianal.csh >&! ${current_logdir}/run_gsi_hybrid.out  &
endif

# enkf forward operator and analysis in background
if ($machine != 'wcoss') then
    if ($enkfonly == "false") then
       setenv hostfilein  ${datapath2}/hostfile1
    else
       setenv hostfilein $PBS_NODEFILE
    endif
endif
echo "$analdate run enkf `date`"
csh ${enkfscripts}/run_enkfanal.csh  >&! ${current_logdir}/run_enkf.out  &

echo "waiting for backgrounded jobs to finish..."
wait # wait here for backgrounded jobs
echo "$analdate gsi and enkf finished `date`"

if ($enkfonly == "false") then
   # once hybrid has completed, check log files.
   set hybrid_done=`cat ${current_logdir}/run_gsi_hybrid.log`
   if ($hybrid_done == 'yes') then
     echo "$analdate hybrid analysis completed successfully `date`"
   else
     echo "$analdate hybrid analysis did not complete successfully, exiting `date`"
     exit 1
   endif
endif

# once enkf has completed, check log files.
set enkf_done=`cat ${current_logdir}/run_enkf.log`
if ($enkf_done == 'yes') then
  echo "$analdate enkf analysis completed successfully `date`"
else
  echo "$analdate enkf analysis did not complete successfully, exiting `date`"
  exit 1
endif

#echo "$analdate starting ens mean analysis computation `date`"
#csh ${enkfscripts}/compute_ensmean_enkf.csh >&!  ${current_logdir}/compute_ensmeans_enkf.out
#echo "$analdate done computing ensemble mean analyses `date`"

if ($enkfonly == 'false' && $recenter_anal == 'true') then
   echo "$analdate recenter enkf analysis ensemble around control analysis `date`"
   csh ${enkfscripts}/recenter_ens_anal.csh >&! ${current_logdir}/recenter_ens_anal.out 
   set recenter_done=`cat ${current_logdir}/recenter_ens.log`
   if ($recenter_done == 'yes') then
     echo "$analdate recentering enkf analysis completed successfully `date`"
   else
     echo "$analdate recentering enkf analysis did not complete successfully, exiting `date`"
     exit 1
   endif
endif

endif # skip to here if fg_only != false

echo "$analdate run enkf ens first guess `date`"
csh ${enkfscripts}/run_fg_ens.csh  >>& ${current_logdir}/run_fg_ens.out  
set enkf_done=`cat ${current_logdir}/run_fg_ens.log`
if ($enkf_done == 'yes') then
  echo "$analdate enkf first-guess completed successfully `date`"
else
  echo "$analdate enkf first-guess did not complete successfully, exiting `date`"
  exit 1
endif

if ($fg_only == 'false' && $do_cleanup == 'true') then

# cleanup
echo "clean up files `date`"
cd $datapath2

# move every member files to a temp dir.
mkdir fgens
mkdir fgens2
/bin/rm -f mem*/*history*nc # don't save history files, only restart files.
/bin/mv -f mem* fgens
/bin/mv -f sfg*mem* fgens2
/bin/mv -f bfg*mem* fgens2
echo "files moved to fgens, fgens2 `date`"
#if ($npefiles == 0) then
#   mkdir diagens
#   /bin/mv -f diag_conv_ges*mem* diagens
#endif

# these are too big to save
#/bin/rm -f diag*cris* diag*metop* diag*airs* diag*hirs4* 

/bin/rm -f hostfile*
/bin/rm -f fort*
/bin/rm -f *log
/bin/rm -f *lores *orig
/bin/rm -f ozinfo convinfo satinfo scaninfo anavinfo
/bin/rm -rf hybridtmp* gsitmp* gfstmp* nodefile* machinefile*
echo "unwanted files removed `date`"

# only save full ensemble data to hpss if checkdate.py returns 0
# a subset will be saved if save_hpss_subset="true"
set date_check=`python ${homedir}/checkdate.py ${analdate}`
if ($date_check == 0) then
  setenv save_hpss "true"
else
  setenv save_hpss "false"
endif

cd $homedir
cat ${machine}_preamble_hpss hpss.sh >! job.sh
if ($machine == 'wcoss') then
   bsub -env "all" < job.sh
else
   qsub -V job.sh
endif

endif # skip to here if fg_only = true or do_cleanup = false

# next analdate: increment by $ANALINC
setenv analdate `${incdate} $analdate $ANALINC`

echo "setenv analdate ${analdate}" >! $startupenv
echo "setenv analdate_end ${analdate_end}" >> $startupenv
echo "setenv fg_only false" >! $datapath/fg_only.csh

cd $homedir

if ( ${analdate} <= ${analdate_end}  && ${resubmit} == 'true') then
   echo "current time is $analdate"
   if ($resubmit == "true") then
      echo "resubmit script"
      echo "machine = $machine"
      cat ${machine}_preamble config.sh >! job.sh
      if ($machine == 'wcoss') then
          bsub < job.sh
      else
          qsub job.sh
      endif
   endif
endif

exit 0
