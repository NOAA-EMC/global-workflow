#!/bin/csh

setenv HOSTFILE ${datapath2}/machinesx

cd ${datapath2}

set fh=${FHMIN}
while ($fh <= $FHMAX)

  set charfhr="fhr`printf %02i $fh`"

  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/bfg_${analdate}_${charfhr}_ensmean)) then
      echo "running  ${utilexec}/getsfcensmeanp.x ${datapath2}/ bfg_${analdate}_${charfhr}_ensmean bfg_${analdate}_${charfhr} ${nanals}"
      /bin/rm -f ${datapath2}/bfg_${analdate}_${charfhr}_ensmean
      setenv PGM "${utilexec}/getsfcensmeanp.x ${datapath2}/ bfg_${analdate}_${charfhr}_ensmean bfg_${analdate}_${charfhr} ${nanals}"
      sh ${enkfscripts}/runmpi
  endif
  if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/sfg_${analdate}_${charfhr}_ensmean)) then
      /bin/rm -f ${datapath2}/sfg_${analdate}_${charfhr}_ensmean
      echo "running ${utilexec}/getsigensmeanp_smooth.x ${datapath2}/ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals}"
      setenv PGM "${utilexec}/getsigensmeanp_smooth.x ${datapath2}/ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals}"
      sh ${enkfscripts}/runmpi
      if ($fh == $ANALINC) then
      echo "running ${utilexec}/getsigensstatp.x ${datapath2}/ sfg_${analdate}_${charfhr} ${nanals}"
      setenv PGM "${utilexec}/getsigensstatp.x ${datapath2}/ sfg_${analdate}_${charfhr} ${nanals}"
      sh ${enkfscripts}/runmpi
      endif
  endif

  @ fh = $fh + $FHOUT

end

# now compute ensemble mean restart files.
if ( $cleanup_ensmean == 'true' || ( $cleanup_ensmean == 'false' && ! -s ${datapath2}/ensmean/INPUT/fv_core.res.tile1.nc ) ) then
if ( $fg_only == 'false') then
   echo "compute ensemble mean restart files `date`"
   setenv nprocs 1
   setenv mpitaskspernode 1
   setenv OMP_NUM_THREADS $corespernode
   if ($machine == 'wcoss') then
      module load nco-gnu-sandybridge  
   else
      module load nco
   endif
   set pathout=${datapath2}/ensmean/INPUT
   mkdir -p $pathout
   set ncount=1
   foreach tile (tile1 tile2 tile3 tile4 tile5 tile6)
      foreach filename (fv_core.res.${tile}.nc fv_tracer.res.${tile}.nc fv_srf_wnd.res.${tile}.nc sfc_data.${tile}.nc)
         setenv PGM "nces -O `ls -1 ${datapath2}/mem*/INPUT/${filename}` ${pathout}/${filename}"
         if ($machine != 'wcoss') then
            set host=`head -$ncount $NODEFILE | tail -1`
            setenv HOSTFILE ${datapath2}/hostfile_nces_${ncount}
            echo $host >! $HOSTFILE
         endif
         echo "computing ens mean for $filename"
         sh ${enkfscripts}/runmpi &
         if ($ncount == $NODES) then
            echo "waiting for backgrounded jobs to finish..."
            wait
            set ncount=1
         else
            @ ncount = $ncount + 1
         endif
      end
   end
   wait
   /bin/rm -f ${datapath2}/hostfile_nces*
   /bin/cp -f ${datapath2}/mem001/INPUT/coupler.res ${pathout}
   /bin/cp -f ${datapath2}/mem001/INPUT/fv_core.res.nc ${pathout}
   echo "done computing ensemble mean restart files `date`"
endif
endif

exit 0
