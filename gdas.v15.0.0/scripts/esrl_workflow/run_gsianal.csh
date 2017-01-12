#!/bin/csh
# do hybrid analysis.

setenv CO2DIR $fixgsi

setenv NOSAT "NO"
setenv charnanal "control"
setenv SKIP_ANGUPDATE "YES"
setenv SIGANL ${datapath2}/sanl_${analdate}_${charnanal}
setenv SIGANL03 ${datapath2}/sanl_${analdate}_fhr03_${charnanal}
setenv SIGANL04 ${datapath2}/sanl_${analdate}_fhr04_${charnanal}
setenv SIGANL05 ${datapath2}/sanl_${analdate}_fhr05_${charnanal}
setenv SIGANL06 ${datapath2}/sanl_${analdate}_fhr06_${charnanal}
setenv SIGANL07 ${datapath2}/sanl_${analdate}_fhr07_${charnanal}
setenv SIGANL08 ${datapath2}/sanl_${analdate}_fhr08_${charnanal}
setenv SIGANL09 ${datapath2}/sanl_${analdate}_fhr09_${charnanal}
setenv SFCANL ${datapath2}/sfcanl_${analdate}_${charnanal}
setenv SFCANLm3 ${datapath2}/sfcanl_${analdate}_fhr03_${charnanal}
setenv BIASO ${datapath2}/${PREINP}abias
setenv BIASO_PC ${datapath2}/${PREINP}abias_pc
setenv SATANGO ${datapath2}/${PREINP}satang

if ($cleanup_controlanl == 'true') then
   /bin/rm -f ${SIGANL}
   /bin/rm -f ${datapath2}/diag*${charnanal}
endif

set niter=1
set alldone='no'
if ( $NOSAT == "YES" ) then
    if ( -s $SIGANL && -s $BIASO ) set alldone='yes'
else
    if ( -s $SIGANL ) set alldone='yes'
endif

while ($alldone == 'no' && $niter <= $nitermax)

setenv OMP_NUM_THREADS $gsi_control_threads
setenv OMP_STACKSIZE 2048m
setenv nprocs `expr $cores_gsi \/ $OMP_NUM_THREADS`
setenv mpitaskspernode `expr $corespernode \/ $OMP_NUM_THREADS`
if ($machine != 'wcoss') then
   setenv KMP_AFFINITY scatter
   if ($OMP_NUM_THREADS > 1) then
      setenv HOSTFILE $datapath2/machinefile_envar
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

setenv tmpdir $datapath2/gsitmp_control
/bin/rm -rf $tmpdir
mkdir -p $tmpdir
setenv VERBOSE 'YES'
setenv HXONLY 'NO'
setenv charnanal2 "control"
setenv skipcat "false"
setenv lread_obs_save ".false."
setenv lread_obs_skip ".false."
setenv CLEAN 'YES'
echo "${analdate} compute gsi hybrid high-res analysis increment `date`"
/bin/cp -f $datapath2/hybens_locinfo $tmpdir
time sh ${enkfscripts}/${rungsi}
if ($status != 0) then
  echo "gsi hybrid high-res analysis did not complete sucessfully"
  set exitstat=1
else
  if ( ! -s $SIGANL ) then
    echo "gsi hybrid high-res analysis did not complete sucessfully"
    set exitstat=1
  else
    echo "gsi hybrid high-res completed sucessfully"
    set exitstat=0
  endif
endif

if ($exitstat == 0) then
   set alldone='yes'
else
   echo "some files missing, try again .."
   @ niter = $niter + 1
endif
end

# calculate increment file.
setenv nprocs 1
setenv mpitaskspernode 1
setenv PGM ${utilexec}/calc_increment.x
mkdir -p ${datapath2}/ensmean/INPUT
pushd ${datapath2}/ensmean/INPUT
cat > calc-increment.input <<EOF
&share
debug=F
analysis_filename="${datapath2}/sanl_${analdate}_${charnanal}"
firstguess_filename="${datapath2}/sfg_${analdate}_fhr06_${charnanal}"
increment_filename="fv3_increment.nc"
/
EOF
sh ${enkfscripts}/runmpi
popd

if($alldone == 'no') then
    echo "Tried ${nitermax} times and to do gsi high-res hybrid analysis and failed"
    echo "no" >&! ${current_logdir}/run_gsi_hybrid.log
else
    echo "yes" >&! ${current_logdir}/run_gsi_hybrid.log
endif
