#! /usr/bin/env bash
#
#########################################################
# This script collects gldas output to archive directory
# Replace soil moisture and temperature for gdas nemsio file
# generate from Gaussian grid fileds to 6-tile netcdf file 
#
# usage - gldas_archive.sh sdate edate
#         sdate/edate in yyyymmdd
#
# HOMEgldas - software directory
# COMDIR - output archive directory
# RUNDIR - run directory
# GDAS   - /lfs/h1/ops/prod/com/gfs/prod
#
#########################################################

source "$HOMEgfs/ush/preamble.sh"

if [ $# -lt 1 ]; then
  echo "usage: $0 sdate [edate]"
  exit $?
fi

sdate=$1
edate=$(sh $FINDDATE $1 d+1)
if [ $# -gt 1 ]; then edate=$2 ; fi

yyyy=$(echo $sdate | cut -c1-4)

### save all output to day1 directory
export COMDIR=${COM_OUT}

mkdir -p $COMDIR/gldas.$sdate
yyyymmdd=$(sh $FINDDATE $sdate d+1)
while [ $yyyymmdd -le $edate ]; do

mkdir -p $COMDIR/gldas.$yyyymmdd

yyyy=$(echo $yyyymmdd | cut -c1-4)
cp $RUNDIR/EXP901/NOAH/$yyyy/$yyyymmdd/* $COMDIR/gldas.$sdate

yyyymmdd=$(sh $FINDDATE $yyyymmdd d+1)
done

cp $RUNDIR/sfc.gaussian.nemsio.$sdate $COMDIR/gldas.$sdate

### rename grb files

yyyymmdd=$sdate
while [ $yyyymmdd -lt $edate ]; do

day1=$yyyymmdd
day2=$(sh $FINDDATE $yyyymmdd d+1)
mv $COMDIR/gldas.$sdate/LIS.E901.${day2}00.NOAH.grb $COMDIR/gldas.$sdate/LIS.E901.${day1}00.NOAH.grb

yyyymmdd=$(sh $FINDDATE $yyyymmdd d+1)
done

### save noah.rst.day2 to day2 directory for next day gldas restart 

yyyymmdd=$(sh $FINDDATE $sdate d+1)
yyyy=$(echo $yyyymmdd | cut -c1-4)
mkdir -p $COMDIR/gldas.$edate
cp $RUNDIR/EXP901/NOAH/$yyyy/$edate/LIS.E901.${edate}00.Noahrst $COMDIR/gldas.$yyyymmdd/noah.rst.$edate

### generate and save gdas.t${cyc1}z.sfcanl.nemsio.gldas.day4 to day4 directory for next cycle gfs restart

mkdir -p $COMDIR/gldas.$edate
gdate=${edate}
gdas_date=${gdate}.${cyc}0000
cp sfc_data.tile1.nc $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile1.nc
cp sfc_data.tile2.nc $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile2.nc
cp sfc_data.tile3.nc $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile3.nc
cp sfc_data.tile4.nc $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile4.nc
cp sfc_data.tile5.nc $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile5.nc
cp sfc_data.tile6.nc $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile6.nc

cp sfc.gaussian.nemsio.gldas $COMDIR/gldas.$edate/${gdas_date}.sfc.gaussian.nemsio.gldas

cp sfc.gaussian.nemsio $COMDIR/gldas.$edate/${gdas_date}.sfc.gaussian.nemsio.gdas

echo $COMDIR/gldas.$edate/${gdas_date}.sfcanl_data.tile6.nc
