#!/bin/csh

setenv HOSTFILE ${datapath2}/machinesx

cd ${datapath2}

set iaufhrs2=`echo $iaufhrs | sed 's/,/ /g'`

echo "compute ensemble mean analyses..."
setenv HOSTFILE $datapath2/machinesx # set in main.csh

foreach nhr_anal ( $iaufhrs2 )
set charfhr="fhr"`printf %02i $nhr_anal`
set charfhr2=`printf %02i $nhr_anal`

if ($IAU == ".true.") then
   if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/sanl_${analdate}_${charfhr}_ensmean)) then
   setenv PGM "${utilexec}/getsigensmeanp_smooth.x ${datapath2}/ sanl_${analdate}_${charfhr}_ensmean sanl_${analdate}_${charfhr} ${nanals}"
   sh ${enkfscripts}/runmpi
   if ($fh == $ANALINC) then
      setenv PGM "${utilexec}/getsigensstatp.x ${datapath2}/ sanl_${analdate}_${charfhr} ${nanals}"
      sh ${enkfscripts}/runmpi
   endif
   endif
else
   if ($cleanup_ensmean == 'true' || ($cleanup_ensmean == 'false' && ! -s ${datapath}/${analdate}/sanl_${analdate}_ensmean)) then
   setenv PGM "${utilexec}/getsigensmeanp_smooth.x ${datapath2}/ sanl_${analdate}_ensmean sanl_${analdate} ${nanals}"
   sh ${enkfscripts}/runmpi
   setenv PGM "${utilexec}/getsigensstatp.x ${datapath2}/ sanl_${analdate} ${nanals}"
   sh ${enkfscripts}/runmpi
   endif
endif

end

exit 0
