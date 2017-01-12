#!/bin/csh

setenv VERBOSE YES
setenv OMP_STACKSIZE 256M
setenv SIGLEVEL $FIXGLOBAL/global_hyblev.l${LEVS}.txt
set charnanal="control"
# reduce resolution of control forecast and recenter enkf ensemble around it.
set fh=$FHMIN
while ($fh <= $FHMAX) 
  set charfhr="fhr`printf %02i $fh`"
  set SIGI=${datapath2}/sfg_${analdate}_${charfhr}_${charnanal}
  set SFCI=${datapath2}/bfg_${analdate}_${charfhr}_${charnanal}
  set SIGO=${datapath2}/sfg_${analdate}_${charfhr}_${charnanal}_lores
  #${utilexec}/nemsio_reformat.x ${SIGI} ${SIGI}.bin4 bin4
  set SFCO=${datapath2}/bfg_${analdate}_${charfhr}_${charnanal}_lores
  #/bin/rm -f ${SIGO}.bin4
  /bin/rm -f $SIGO
  /bin/rm -f $SFCO
  setenv DATA $datapath2/chgrestmp_${charfhr}
  mkdir -p $DATA
  #time $CHGRESSH ${SIGI}.bin4 $SFCI ${SIGO}.bin4 $SFCO $JCAP_ENS $LEVS $LONB_ENS $LATB_ENS   &
  time $CHGRESSH ${SIGI} $SFCI ${SIGO} $SFCO $JCAP_ENS $LEVS $LONB_ENS $LATB_ENS   &
  @ fh = $fh + $FHOUT
end
wait
ls -l ${datapath2}/*lores
/bin/rm -rf $datapath2/chgrestmp*

set fh=$FHMIN
while ($fh <= $FHMAX) 
  set charfhr="fhr`printf %02i $fh`"
  set SIGI=${datapath2}/sfg_${analdate}_${charfhr}_${charnanal}
  set SIGO=${datapath2}/sfg_${analdate}_${charfhr}_${charnanal}_lores
  #/bin/rm -f ${SIGI}.bin4
  #${utilexec}/nemsio_reformat.x ${SIGIO}.bin4 ${SIGIO} grib
  #/bin/rm -rf ${SIGIO}.bin4
  if ( -s $SIGO ) then
    echo "recenter $charfhr ensemble perturbations about control forecast"
    set filename_meanin=${datapath2}/sfg_${analdate}_${charfhr}_ensmean
    set filename_meanout=$SIGO
    set filenamein="${datapath2}/sfg_${analdate}_${charfhr}"
    set filenameout="${datapath2}/sfg_${analdate}_${charfhr}"
    setenv PGM "${utilexec}/recentersigp_tst.x $filenamein $filename_meanin $filename_meanout $filenameout $nanals"
    ${enkfscripts}/runmpi
    if ($status == 0) then
       /bin/cp -f $filename_meanin  ${filename_meanin}.orig
       /bin/mv -f $filename_meanout $filename_meanin
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
  else
    echo "no" >! ${current_logdir}/recenter_ens.log
    exit 1
  endif
  @ fh = $fh + $FHOUT
end
