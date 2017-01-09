setenv VERBOSE YES

date
# run model
setenv DATOUT ${datapath}/${analdatep1}

setenv OMP_NUM_THREADS $fg_threads
setenv nprocs `expr $fg_proc \/ $OMP_NUM_THREADS`
setenv mpitaskspernode `expr $corespernode \/ $OMP_NUM_THREADS`

if ($machine != 'wcoss') then
   set hosts = `cat $PBS_NODEFILE`
endif

set nhosts = $cores
echo "nhosts = $nhosts"

set nhost1=1
set nhost=$nhost1

set nanal=1

while ($nanal <= $nanals)
 setenv charnanal "mem`printf %03i $nanal`"

# check to see if output files already created.
 set fhr=$FHMIN
 set outfiles=""
 while ($fhr <= $FHMAX)
    set charhr="fhr`printf %02i $fhr`"
    set outfiles = "${outfiles} ${datapath}/${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}/${analdatep1}/bfg_${analdatep1}_${charhr}_${charnanal}"
    @ fhr = $fhr + $FHOUT
 end
 set filemissing='no'
 foreach outfile ($outfiles) 
   if ( ! -s $outfile) then
     echo "${outfile} is missing"
     set filemissing='yes'
   else
     echo "${outfile} is OK"
   endif
 end

 set node=$nhost
 set node_end=$node
 @ node_end = $node_end + $fg_proc - 1
 if ($filemissing == 'yes') then
   #if ($node_end > $nhosts) set node_end=$nhosts
   echo "nanal = ${nanal}, nhost = ${nhost}, node = ${node}, node_end = ${node_end}"
   if ($machine != 'wcoss') then
      setenv HOSTFILE ${datapath2}/hostfile${node}
      /bin/rm -f $HOSTFILE
      set hostindx=$nhost
      while ($hostindx <= $node_end) 
         set host1 = `echo $hosts[$hostindx]`
         echo ${host1} >> ${HOSTFILE}
         @ hostindx = $hostindx + $fg_threads
      end
      echo "HOSTFILE = $HOSTFILE"
      cat $HOSTFILE
   endif
   sh ${enkfscripts}/${rungfs} >&! ${current_logdir}/run_fg_${charnanal}.out &
   @ nhost = $nhost + $fg_proc
 else
   echo "skipping nanal = ${nanal}, output files already created"
 endif

 @ node_end_next = $node_end + $fg_proc - 1
 if ($node_end > $nhosts || $node_end_next > $nhosts) then
  echo "$node_end $node_end_next $nhosts"
  echo "waiting at nanal = ${nanal} `date`"
  wait
  set nhost=$nhost1
 endif

@ nanal = $nanal + 1
end
echo "waiting at nanal = ${nanal} `date`"
wait
echo "all done `date`"

# check to see all files created
echo "checking output files .."`date`

set nanal=1
set anyfilemissing='no'
while ($nanal <= $nanals)
    setenv charnanal  "mem`printf %03i $nanal`"
    set fhr=$FHMIN
    set outfiles=""
    while ($fhr <= $FHMAX)
       set charhr="fhr`printf %02i $fhr`"
       set outfiles = "${outfiles} ${datapath}/${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}/${analdatep1}/bfg_${analdatep1}_${charhr}_${charnanal}"
       @ fhr = $fhr + $FHOUT
    end
    set filemissing='no'
    foreach outfile ($outfiles) 
      ls -l $outfile
      if ( ! -s $outfile) then 
        echo "${outfile} is missing"
        set filemissing='yes'
        set anyfilemissing='yes'
      else
        echo "${outfile} is OK"
      endif
    end
    @ nanal = $nanal + 1
end

if ($anyfilemissing == 'yes') then
    echo "there are output files missing!"
    exit 1
else
    echo "all output files seem OK"
    date
    exit 0
endif
