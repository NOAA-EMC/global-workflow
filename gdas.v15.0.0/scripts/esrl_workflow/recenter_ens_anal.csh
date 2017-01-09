#!/bin/csh

setenv VERBOSE YES
setenv OMP_STACKSIZE 256M
set charnanal="control"

set iaufhrs2=`echo $iaufhrs | sed 's/,/ /g'`
echo  "iaufhrs2= $iaufhrs2"
foreach nhr_anal ( $iaufhrs2 )
set charfhr="fhr"`printf %02i $nhr_anal`

echo "recenter ensemble perturbations about low resolution hybrid analysis"
if ($IAU == ".true.") then
set filename_meanin=${datapath2}/sanl_${analdate}_${charfhr}_ensmean
set filename_meanout=${datapath2}/sanl_${analdate}_${charfhr}_${charnanal}
set filenamein="${datapath2}/sanl_${analdate}_${charfhr}"
set filenameout="${datapath2}/sanl_${analdate}_${charfhr}"
else
set filename_meanin=${datapath2}/sanl_${analdate}_ensmean
set filename_meanout=${datapath2}/sanl_${analdate}_${charnanal}
set filenamein="${datapath2}/sanl_${analdate}"
set filenameout="${datapath2}/sanl_${analdate}"
endif

setenv PGM "${utilexec}/recentersigp.x $filenamein $filename_meanin $filename_meanout $filenameout $nanals"
sh ${enkfscripts}/runmpi
if ($status == 0) then
  /bin/mv -f $filename_meanin  ${filename_meanin}.orig
  /bin/ln -fs $filename_meanout $filename_meanin
  if ($status == 0) then
     echo "yes" >! ${current_logdir}/recenter_ens.log
  else
     echo "no" >! ${current_logdir}/recenter_ens.log
     exit 1
  endif
else
  echo "no" >! ${current_logdir}/recenter_ens.log
  exit 1
endif

end # next time

exit 0
