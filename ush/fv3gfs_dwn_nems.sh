#!/bin/ksh
set -x

# this script generates 0.25/0.5/1/2.5 deg pgb files for each small Grib file
# Hui-Ya Chuang 01/2014: First Version
# Fanglin Yang  09/2015: Modified to use WGRIB2 instead of COPYGB2 for interpolation
# Fanglin Yang  02/2016: remove 0.5-deg and 2.5deg output to speed up post            
# Fanglin Yang  09/11/2017: add option opt24 to turn on bitmap (from Wen Meng) 
# Wen Meng 12/2017: add trim_rh.sh for triming RH values larger than 100.
# Wen Meng 01/2018: add flag PGB1F for turning on/off wgrib1 pgb data at 1.00 deg. generation.
# Wen Meng 02/2018: add flag PGBS for turning on/off pgb data at 1.0 and 0.5 deg. generation.
# Wen Meng 10/2019: Use bilinear interpolation for LAND, It can trancate land-sea mask as 0 or 1.
# Wen Meng 11/2019: Teak sea ice cover via land-sea mask.

export tmpfile=$1
export fhr3=$2
export iproc=$3
export nset=$4

export CNVGRIB=${CNVGRIB:-$${NWPROD:-/nwprod}/util/exec/cnvgrib21}
export COPYGB2=${COPYGB2:-$${NWPROD:-/nwprod}/util/exec/copygb2}
export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}
export TRIMRH=${TRIMRH:-$USHgfs/trim_rh.sh}
export MODICEC=${MODICEC:-$USHgfs/mod_icec.sh}

export opt1=' -set_grib_type same -new_grid_winds earth '
export opt21=' -new_grid_interpolation bilinear  -if '
export opt22=":(CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export opt23=' -new_grid_interpolation neighbor -fi '
export opt24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
export opt25=":(APCP|ACPCP|PRATE|CPRAT):"
export opt26=' -set_grib_max_bits 25 -fi -if '
export opt27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
export opt28=' -new_grid_interpolation budget -fi '
if [ $machine = "S4" ]; then
  export optncpu=' -ncpu 1 '
fi
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"

export PGB1F=${PGB1F:-"NO"}
export PGBS=${PGBS:-"NO"}

if [ $nset = 1 ]; then
  if [ "$PGBS" = "YES" ]; then
    $WGRIB2 $optncpu $tmpfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 \
      -new_grid $grid0p25 pgb2file_${fhr3}_${iproc}_0p25 \
      -new_grid $grid1p0  pgb2file_${fhr3}_${iproc}_1p0  \
      -new_grid $grid0p5  pgb2file_${fhr3}_${iproc}_0p5   
    export err=$?; err_chk
    $TRIMRH pgb2file_${fhr3}_${iproc}_0p25
    $TRIMRH pgb2file_${fhr3}_${iproc}_0p5
    $TRIMRH pgb2file_${fhr3}_${iproc}_1p0
    #tweak sea ice cover 
    count=$($WGRIB2 $optncpu pgb2file_${fhr3}_${iproc}_0p25 -match "LAND|ICEC" |wc -l)
    if [ $count -eq 2 ]; then
      $MODICEC pgb2file_${fhr3}_${iproc}_0p25
      $MODICEC pgb2file_${fhr3}_${iproc}_0p5
      $MODICEC pgb2file_${fhr3}_${iproc}_1p0
    fi
    #$CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_0p25 pgbfile_${fhr3}_${iproc}_0p25          
    if [ "$PGB1F" = 'YES' ]; then
      $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_1p0 pgbfile_${fhr3}_${iproc}_1p0  
      export err=$?; err_chk
    fi
  else
    $WGRIB2 $optncpu $tmpfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 \
    -new_grid $grid0p25 pgb2file_${fhr3}_${iproc}_0p25 
    export err=$?; err_chk
    $TRIMRH pgb2file_${fhr3}_${iproc}_0p25
    #tweak sea ice cover
    count=$($WGRIB2 $optncpu pgb2file_${fhr3}_${iproc}_0p25 -match "LAND|ICEC" |wc -l)
    if [ $count -eq 2 ]; then
      $MODICEC pgb2file_${fhr3}_${iproc}_0p25 
    fi
  fi
elif [ $nset = 2 ]; then
  if [ "$PGBS" = "YES" ]; then
    $WGRIB2 $optncpu $tmpfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 \
      -new_grid $grid0p25 pgb2bfile_${fhr3}_${iproc}_0p25 \
      -new_grid $grid1p0  pgb2bfile_${fhr3}_${iproc}_1p0  \
      -new_grid $grid0p5  pgb2bfile_${fhr3}_${iproc}_0p5  
    export err=$?; err_chk
    $TRIMRH pgb2bfile_${fhr3}_${iproc}_0p25
    $TRIMRH pgb2bfile_${fhr3}_${iproc}_0p5
    $TRIMRH pgb2bfile_${fhr3}_${iproc}_1p0
  else 
    $WGRIB2 $optncpu $tmpfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 \
    -new_grid $grid0p25 pgb2bfile_${fhr3}_${iproc}_0p25
    export err=$?; err_chk
    $TRIMRH pgb2bfile_${fhr3}_${iproc}_0p25
  fi
fi

#----------------------------------------------------------------------------------------------
#--Hui-Ya Chuang
# export grid1p0="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
# $COPYGB2 -g "${grid1p0}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_1p0
# export grid0p5="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
# $COPYGB2 -g "${grid0p5}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_0p5
# export grid2p5="0 6 0 0 0 0 0 0 144 73 0 0 90000000 0 48 -90000000 357500000 2500000 2500000 0"
# $COPYGB2 -g "${grid2p5}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_2p5
# $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_1p0 pgbfile_${fhr3}_${iproc}_1p0          
# $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_2p5 pgbfile_${fhr3}_${iproc}_2p5 
#----------------------------------------------------------------------------------------------

exit 0
