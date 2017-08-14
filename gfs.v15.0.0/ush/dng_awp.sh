#!/bin/ksh
#=======================================================================
# Script developed by NCO to :
# bsm - add processing for conversion to grib2 and awips files
# called by : smartinit.sh
# INPUT:
#  RGIN :  2 letter Region id (eg: CS, HI, PR,AK..)
#  cyc  :  UTC run cycle
#  fhr  :  Forecasts hour
#  ogrd :  output NDFD grid number (eg: 197,196,195,198...)
#  outreg  :  output file region name (eg: conus,ak,pr,hi,conus2p5,ak3
#=======================================================================
set -x
outreg=$1    # mdlgrd used for guam to distinguish arw/nmm
ihindex=0
case $cyc in 
  00|12) cyctp=on;;
  06|18) cyctp=off;;
esac

REGCP=`echo $outreg |tr '[a-z]'  '[A-Z]' `
echo BEGIN NCO sminit Post-Processing for REG $RGIN $outreg $ogrd CYC $cyc FHR $fhr 

if [ $outreg != guam ]; then   
# Create HAINES INDEX GRIB FILE
  if [ $ihindex -eq 1 ];then
    ${WGRIB} MESO${RGIN}${fhr}.tm00 |grep ":HINDEX" | \
    ${WGRIB} -i -grib  MESO${RGIN}${fhr}.tm00 -o hindex.t${cyc}z.smart${outreg}${fhr}.tm00
    $CNVGRIB -g12 -p40 hindex.t${cyc}z.smart${outreg}${fhr}.tm00  hindex.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
  fi
fi

$CNVGRIB -g12 -p40 MESO${RGIN}${fhr}.tm00 ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2

# Processing grids for AWIPS
 pgm=tocgrib2
 export pgm;  #. prep_step
 startmsg
export FORTREPORTS=unit_vars=yes
export FORT11=${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 
export FORT31=" ";
export FORT51=grib2.t${cyc}z.smart${outreg}f${fhr}

# Define grib2 awips parm file 
if [ $outreg = conus2p5 ];then
  awpparm=$PARMdng/grib2_awp${mdl}dngconus${cyctp}f${fhr}.${ogrd}
elif [ $outreg = ak3 ];then
  awpparm=$PARMdng/grib2_awp${mdl}dngak${cyctp}f${fhr}.${ogrd}
elif [ $outreg = guam ];then
  awpparm=$PARMdng/grib2_${mdl}_smart${outreg}${cyctp}f${fhr}.${ogrd}
else
  awpparm=$PARMdng/grib2_awp${mdl}smart${outreg}${cyctp}f${fhr}.${ogrd}
fi

if [ -s "$awpparm" ];then
  $TOCGRIB2 < ${awpparm} 1 >> $pgmout 2>> errfile
  err=$?
  echo " error from tocgrib="  $err
else 
  if [ $fhr -ne 1 -a $fhr -ne 2 -a $fhr -ne 4 -a $fhr -ne 5 -a $fhr -ne 7 \
       -a $fhr -ne 8 -a $fhr -ne 10 -a $fhr -ne 11 ];then
  echo AWP PARM FILE not found: $awpparm
  fi
fi

mv MESO${RGIN}${fhr}.tm00 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00
mv ${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
if [ $ihindex -eq 1 ];then
 mv hindex.t${cyc}z.smart${outreg}${fhr}.tm00 $COMOUT/hindex.t${cyc}z.smart${outreg}${fhr}.tm00
 mv hindex.t${cyc}z.smart${outreg}${fhr}.tm00.grib2 $COMOUT/hindex.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
fi

# Move grib2 awips file to pcom
if [ $outreg = ak3 ];then
  mv grib2.t${cyc}z.smart${outreg}f${fhr} $pcom/grib2.awp${mdl}smart3.ak${fhr}_awips_f${fhr}_${cyc}
else
  if [ $fhr -ne 1 -a $fhr -ne 2 -a $fhr -ne 4 -a $fhr -ne 5 -a $fhr -ne 7 \
       -a $fhr -ne 8 -a $fhr -ne 10 -a $fhr -ne 11 ];then
  mv grib2.t${cyc}z.smart${outreg}f${fhr} $pcom/grib2.awp${mdl}smart.${outreg}${fhr}_awips_f${fhr}_${cyc}
  fi
fi

if [ -s "$awpparm" ];then
  if [ $SENDDBN = YES ];then #bsm 25 feb 2008 - added code for awips alerts
    if [ $outreg = ak3 ];then
      $DBNROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job $pcom/grib2.awp${mdl}smart3.ak${fhr}_awips_f${fhr}_${cyc}
    else
      $DBNROOT/bin/dbn_alert NTC_LOW SMART${REGCP} $job $pcom/grib2.awp${mdl}smart.${outreg}${fhr}_awips_f${fhr}_${cyc}
    fi
  fi
fi

if [ $SENDDBN = YES ];then
  if [ $outreg = guam ];then
    $DBNROOT/bin/dbn_alert MODEL GFS_SMART${REGCP}_GB2 $job $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
  else
    $DBNROOT/bin/dbn_alert MODEL NAM_SMART${REGCP}_GB2 $job $COMOUT/${mdl}.t${cyc}z.smart${outreg}${fhr}.tm00.grib2
  fi
fi
