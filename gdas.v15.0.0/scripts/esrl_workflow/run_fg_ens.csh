# turn on stochastic physics
#setenv sppt_import 1
#setenv sppt_export 1
#setenv shum_import 1
#setenv shum_export 1
#setenv skeb_import 1
#setenv skeb_export 1
#setenv vc_import 0
#setenv vc_export 0
#
#setenv WRT_GROUP 1
#setenv WRTPE_PER_GROUP 1
#
#setenv postprocnodes $postprocnodes_ens
#setenv postprocthreads $postprocthreads_ens

# run ensemble first guess.
# first, clean up old first guesses.
if ($cleanup_fg == 'true') then
echo "deleting existing files..."
set nanal=1
while ( $nanal <= $nanals)
    set charnanal="mem`printf %03i $nanal`"
    /bin/rm -f ${datapath}/${analdatep1}/sfg_${analdatep1}*${charnanal}
    /bin/rm -f ${datapath}/${analdatep1}/bfg_${analdatep1}*${charnanal} 
    @ nanal = $nanal + 1
end
endif
mkdir -p ${datapath}/${analdatep1}

set niter=1
set alldone='no'
echo "${analdate} compute first guesses `date`"
while ($alldone == 'no' && $niter <= $nitermax)
    if ($niter == 1) then
    csh ${enkfscripts}/${fg_gfs} >&! ${current_logdir}/run_fg.out
    set exitstat=$status
    else
    csh ${enkfscripts}/${fg_gfs} >>& ${current_logdir}/run_fg.out
    set exitstat=$status
    endif
    if ($exitstat == 0) then
       set alldone='yes'
    else
       echo "some files missing, try again .."
       @ niter = $niter + 1
    endif
end

if($alldone == 'no') then
    echo "Tried ${nitermax} times to run ens first-guesses and failed: ${analdate}"
    echo "no" >&! ${current_logdir}/run_fg_ens.log
else
    echo "yes" >&! ${current_logdir}/run_fg_ens.log
endif
