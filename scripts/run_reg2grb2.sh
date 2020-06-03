#!/bin/bash
set -euax

# Adapted from: Xingren Wu
# 2016-09-02
# This scripts is for CFS/UGCS ocean post

# Christopher Melhauser
# 2017-09-11
# This script is for UGCS CICE5/MOM5 ocean and ice post

# Xingren Wu
# 2017-10-23
# Update and bug/fix
#module purge
#module load ics/12.1
#module load prod_util/v1.0.7
#module load NetCDF/4.2/serial
#module load prod_util/1.0.18
module load grib_util/1.1.1
module list

#### export NWPROD=${NWPROD:-/scratch1/NCEPDEV/global/glopara/svn/verif/global/tags/vsdb/nwprod}
# on Hera
#### module purge
#### module load intel/18.0.5.274
#### module load netcdf/4.7.0
#### module load wgrib2/2.0.8
#### module use -a $MOD_PATH
#### module load ip/3.0.1
#### module load sp/2.0.2
#### module load w3nco/2.0.6
#### module load bacio/2.0.2
#### module load landsfcutil/2.1.0
#### set FCMP ifort

#export execdir=${execdir:-/climate/save/emc.climpara/Xingren/regrid/iceocnpost/exec}
#export executable=${executable:-$execdir/reg2grb2.x}

#### export execdir=$MOM6REGRID/exec
#### export execdir=${execdir:-/scratch2/NCEPDEV/climate/Bin.Li/S2S/fix/ocean_ice_post/mom6_regrid_025/exec}
#### export executable=${executable:-$execdir/reg2grb2.x}
#### export mask_file=/scratch2/NCEPDEV/climate/Bin.Li/S2S/fix/ocean_ice_post/mom6_regrid_025/mask.0p25x0p25.grb2
export mask_file=$MOM6REGRID/fix/mask.0p25x0p25.grb2
#export icefile=/climate/save/emc.climpara/Xingren/regrid/out/icer2015040106.01.2015040100_0p5x0p5_CICE.nc
#export ocnfile=/climate/save/emc.climpara/Xingren/regrid/out/ocnr2015040106.01.2015040100_0p5x0p5_MOM6.nc
#export outfile=/climate/save/emc.climpara/Xingren/regrid/out/ocnh2015040106.01.2015040100.grb2
#
# offline testing:
#export DATA=
#export icefile=$DATA/DATA0p5/icer2012010106.01.2012010100_0p5x0p5.nc
#export ocnfile=$DATA/DATA0p5/ocnr2012010106.01.2012010100_0p5x0p5.nc
#export outfile=$DATA/DATA0p5/out/ocnh2012010106.01.2012010100.grb2
#
# workflow testing:
export icefile=icer${CDATE}.${ENSMEM}.${IDATE}_0p25x0p25_CICE.nc
export ocnfile=ocnr${CDATE}.${ENSMEM}.${IDATE}_0p25x0p25_MOM6.nc
export outfile=ocn_ice${CDATE}.${ENSMEM}.${IDATE}_0p25x0p25.grb2
export outfile0p5=ocn_ice${CDATE}.${ENSMEM}.${IDATE}_0p5x0p5.grb2

export mfcstcpl=${mfcstcpl:-1}
export IGEN_OCNP=${IGEN_OCNP:-197}

# PT This is the forecast date
export year=`echo $CDATE | cut -c1-4`
export month=`echo $CDATE | cut -c5-6`
export day=`echo $CDATE | cut -c7-8`
export hour=`echo $CDATE | cut -c9-10`

# PT This is the initialization date
export syear=`echo $IDATE | cut -c1-4`
export smonth=`echo $IDATE | cut -c5-6`
export sday=`echo $IDATE | cut -c7-8`
export shour=`echo $IDATE | cut -c9-10`

# PT Need to get this from above - could be 6 or 1 hour
export hh_inc_ocn=6
#
# set for 1p0 lat-lon
#export im=360
#export jm=181
# export km=40
#export imo=360
#export jmo=181
#
# set for 0p5 lat-lon
#export im=720
#export jm=361
#export km=40
#export imo=720
#export jmo=361
#
# set for 0p25 lat-lon
export im=1440
export jm=721
export imo=1440
export jmo=721
export km=40

export flats=-90.
export flatn=90.
export flonw=0.0
export flone=359.75

ln -sf $mask_file ./iceocnpost.g2
$executable > reg2grb2.$CDATE.$IDATE.out

# interpolated from 0p25 to 0p5 grid
grid2p05="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000  500000  0"
#### $NWPROD/util/exec/copygb2 -g "${grid2p05}" -i0 -x $outfile $outfile0p5
$COPYGB2 -g "${grid2p05}" -i0 -x $outfile $outfile0p5
