#! /usr/bin/env bash
###########################################################################
# this script gets cpc daily precipitation and using gdas hourly precipitation
# to disaggregate daily value into hourly value
########################################################################### 

source "$HOMEgfs/ush/preamble.sh"

bdate=$1
edate=$2

# HOMEgldas - gldas directory
# EXECgldas - gldas exec directory
# PARMgldas - gldas param directory
# FIXgldas  - gldas fix field directory
export LISDIR=$HOMEgldas
export fpath=${RUNDIR}/force
export xpath=${RUNDIR}/force
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}

#-------------------------------
#--- extract variables of each timestep and create forcing files
sdate=$bdate
edate=$(sh $FINDDATE $edate d-1)
while [ $sdate -lt $edate ]; do
#-------------------------------

sdat0=$(sh $FINDDATE $sdate d-1)
[[ ! -d $xpath/cpc.${sdate} ]] && mkdir -p $xpath/cpc.${sdate}
[[ ! -d $xpath/cpc.${sdat0} ]] && mkdir -p $xpath/cpc.${sdat0}

cd $xpath
rm -f fort.* grib.*

COMPONENT=${COMPONENT:-"atmos"}
pathp1=$CPCGAUGE/gdas.$sdate/00/$COMPONENT
pathp2=$DCOMIN/$sdate/wgrbbul/cpc_rcdas
yyyy=$(echo $sdate |cut -c 1-4)
cpc_precip="PRCP_CU_GAUGE_V1.0GLB_0.125deg.lnx.$sdate.RT"
if [ $RUN_ENVIR = "emc" ] && [ $sdate -gt $bdate ]; then 
    cpc_precip="PRCP_CU_GAUGE_V1.0GLB_0.125deg.lnx.$sdate.RT_early"
fi
cpc=$pathp1/$cpc_precip
if [ ! -s $cpc ]; then cpc=$pathp2/$cpc_precip ; fi
if [ $RUN_ENVIR = "nco" ]; then cpc=$pathp2/$cpc_precip ; fi
if [ ! -s $cpc ]; then 
 echo "WARNING: GLDAS MISSING $cpc, WILL NOT RUN."
 exit 3
fi
cp $cpc $xpath/cpc.$sdate/.

sflux=$fpath/gdas.${sdat0}/gdas1.t12z.sfluxgrbf06
prate=gdas.${sdat0}12
$WGRIB -s $sflux | grep "PRATE:sfc" | $WGRIB -i $sflux -grib -o $prate

sflux=$fpath/gdas.${sdat0}/gdas1.t18z.sfluxgrbf06
prate=gdas.${sdat0}18
$WGRIB -s $sflux | grep "PRATE:sfc" | $WGRIB -i $sflux -grib -o $prate

sflux=$fpath/gdas.${sdate}/gdas1.t00z.sfluxgrbf06
prate=gdas.${sdate}00
$WGRIB -s $sflux | grep "PRATE:sfc" | $WGRIB -i $sflux -grib -o $prate

sflux=$fpath/gdas.${sdate}/gdas1.t06z.sfluxgrbf06
prate=gdas.${sdate}06
$WGRIB -s $sflux | grep "PRATE:sfc" | $WGRIB -i $sflux -grib -o $prate

if [ $USE_CFP = "YES" ] ; then
  rm -f ./cfile
  touch ./cfile
  echo "$COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdat0}12 grib.12" >> ./cfile
  echo "$COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdat0}18 grib.18" >> ./cfile
  echo "$COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdate}00 grib.00" >> ./cfile
  echo "$COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdate}06 grib.06" >> ./cfile
  $APRUN_GLDAS_DATA_PROC ./cfile
else
  $COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdat0}12 grib.12
  $COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdat0}18 grib.18
  $COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdate}00 grib.00
  $COPYGB -i3 '-g255 0 2881 1441 90000 0 128 -90000 360000 125 125' -x gdas.${sdate}06 grib.06
fi

rm -f fort.10
touch fort.10
echo ${sdat0} >> fort.10
echo ${sdate}  >> fort.10

export pgm=gldas_forcing
. prep_step

$WGRIB -d -bin grib.12 -o fort.11
$WGRIB -d -bin grib.18 -o fort.12
$WGRIB -d -bin grib.00 -o fort.13
$WGRIB -d -bin grib.06 -o fort.14

ln -fs $xpath/cpc.$sdate/$cpc_precip fort.15

$EXECgldas/gldas_forcing     1>&1 2>&2

export err=$?
$ERRSCRIPT || exit 3

cp fort.21 $xpath/cpc.$sdat0/precip.gldas.${sdat0}12
cp fort.22 $xpath/cpc.$sdat0/precip.gldas.${sdat0}18
cp fort.23 $xpath/cpc.$sdate/precip.gldas.${sdate}00
cp fort.24 $xpath/cpc.$sdate/precip.gldas.${sdate}06

rm -f fort.* grib.*

#-------------------------------
sdate=$(sh $FINDDATE $sdate d+1)
done
#-------------------------------

exit $err
