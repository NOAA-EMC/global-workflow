#!/bin/csh 

# run gsi observer.

setenv beta1_inv 1.0 # 3dvar for HXONLY
setenv OMP_NUM_THREADS $gsi_control_threads
setenv OMP_STACKSIZE 2048m
setenv nprocs `expr $cores_enkf \/ $OMP_NUM_THREADS`
setenv mpitaskspernode `expr $corespernode \/ $OMP_NUM_THREADS`
if ($machine != 'wcoss') then
   setenv KMP_AFFINITY scatter
   if ($OMP_NUM_THREADS > 1) then
      setenv HOSTFILE $datapath2/machinefile_enkf
      /bin/rm -f $HOSTFILE
      awk "NR%${gsi_control_threads} == 1" ${hostfilein} >&! $HOSTFILE
   else
      setenv HOSTFILE $hostfilein
   endif
   cat $HOSTFILE
   wc -l $HOSTFILE
   #setenv OMP_NUM_THREADS 1
endif
echo "running with $OMP_NUM_THREADS threads ..."

setenv GBIAS ${datapathm1}/${PREINPm1}abias
setenv GBIAS_PC ${datapathm1}/${PREINPm1}abias_pc
setenv GSATANG ${datapathm1}/${PREINPm1}satang

setenv tmpdir $datapath2/gsitmp_ensmean
if ($cleanup_obs == 'true') then
    /bin/rm -rf $tmpdir
    mkdir -p $tmpdir
    /bin/rm -f ${datapath2}/diag*ensmean*
endif

if ($npefiles == 0 && -s ${datapath2}/diag_conv_ges.${analdate}_ensmean) then
   set alldone='yes'
else if ($npefiles != 0 && -s ${datapath2}/gsitmp_ensmean/pe0000.conv_01) then
   set alldone='yes'
else
   set alldone='no'
endif

set niter=1
while ($alldone == 'no' && $niter <= $nitermax)
   setenv VERBOSE 'YES'
   setenv lread_obs_save ".false."
   setenv lread_obs_skip ".false."
   setenv charnanal2 "ensmean" # for diag files.
   echo "${analdate} compute gsi observer"
   if ($npefiles != 0) then
      setenv skipcat "true"
   else
      setenv skipcat "false"
   endif
   setenv HXONLY 'YES'
   setenv DOSFCANL 'NO'
   setenv CLEAN 'NO'
   setenv SKIP_ANGUPDATE 'YES'
   time sh ${enkfscripts}/${rungsi}
   set exitstat=$status
   if ( $skipcat == "false" && ! -s ${datapath2}/diag_conv_ges.${analdate}_${charnanal2} ) then
       echo "gsi observer step failed"
       set alldone='no'
   else if ( $skipcat == "true" && ! -s ${datapath2}/gsitmp_ensmean/pe0000.conv_01 ) then
       echo "gsi observer step failed"
       set alldone='no'
   else if ( $exitstat == 0 ) then
       set alldone='yes'
   endif
   if ($alldone == 'no') then 
      echo "some files missing, try again .."
      @ niter = $niter + 1
   endif
end

# now run EnKF.
 
ln -fs $GBIAS   ${datapath2}/satbias_in
ln -fs $GBIAS_PC   ${datapath2}/satbias_pc
ln -fs $GSATANG ${datapath2}/satbias_angle
ln -fs ${SATINFO} ${datapath2}/satinfo
ls -l ${datapath2}/satinfo
ln -fs ${CONVINFO} ${datapath2}/convinfo
ls -l ${datapath2}/convinfo
ln -fs ${gsipath}/fix/global_ozinfo.txt ${datapath2}/ozinfo
ln -fs ${gsipath}/fix/global_scaninfo.txt ${datapath2}/scaninfo
ln -fs ${current_logdir}/satinfo.out ${datapath2}/fort.207
ln -fs ${current_logdir}/ozinfo.out ${datapath2}/fort.206
ln -fs ${current_logdir}/convinfo.out ${datapath2}/fort.205
setenv ABIAS ${datapath2}/${PREINP}abias

# remove previous analyses
if ($cleanup_anal == 'true') then
   /bin/rm -f ${datapath2}/sanl_*mem*
endif

set niter=1
set alldone='no'
echo "${analdate} compute enkf analysis increment `date`"
while ($alldone == 'no' && $niter <= $nitermax)
    csh ${enkfscripts}/${ensda} 
    set exitstat=$status
    if ($exitstat == 0) then
       set alldone='yes'
    else
       echo "some files missing, try again .."
       @ niter = $niter + 1
    endif
end
if($alldone == 'no') then
    echo "Tried ${nitermax} times to run ensda and failed: ${analdate}"
    echo "no" >&! ${current_logdir}/run_enkf.log
else
    echo "yes" >&! ${current_logdir}/run_enkf.log
endif
echo "${analdate} done computing enkf analysis increment `date`"
