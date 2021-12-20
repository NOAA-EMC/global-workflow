#!/bin/ksh
set -x

#-----------------------------------------------------------------------
#-Wen Meng, 03/2019:  First version.
#  This scripts is for interpolating flux file from model native grid
#  into lat-lon grids.
#-----------------------------------------------------------------------


echo "!!!!!CREATING $RUN FLUX PRODUCTS FOR FH = $FH !!!!!!"

export CNVGRIB=${CNVGRIB:-${NWPROD:-/nwprod}/util/exec/cnvgrib21}
export COPYGB2=${COPYGB2:-${NWPROD:-/nwprod}/util/exec/copygb2}
export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}
export GRBINDEX=${GRBINDEX:-${NWPROD:-nwprod}/util/exec/grbindex}
export RUN=${RUN:-"gfs"}
export cycn=$(echo $CDATE |cut -c 9-10)
export TCYC=${TCYC:-".t${cycn}z."}
export PREFIX=${PREFIX:-${RUN}${TCYC}}
export PGB1F=${PGB1F:-"NO"}

#--wgrib2 regrid parameters
export option1=' -set_grib_type same -new_grid_winds earth '
export option21=' -new_grid_interpolation bilinear  -if '
export option22=":(LAND|CRAIN|CICEP|CFRZR|CSNOW|ICSEV):"
export option23=' -new_grid_interpolation neighbor -fi '
export option24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
export option25=":(APCP|ACPCP|PRATE|CPRAT):"
export option26=' -set_grib_max_bits 25 -fi -if '
export option27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
export option28=' -new_grid_interpolation budget -fi '
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"


if [ $FH -eq 0 ] ; then
  export fhr3=000
else
  export fhr3=$(expr $FH + 0 )
  if [ $fhr3 -lt 100 ]; then export fhr3="0$fhr3"; fi
  if [ $fhr3 -lt 10 ];  then export fhr3="0$fhr3"; fi
fi

#---------------------------------------------------------------

  if [ $INLINE_POST = ".false." ]; then
    $WGRIB2 $PGBOUT $option1 $option21 $option22 $option23 $option24 \
                          $option25 $option26 $option27 $option28 \
                          -new_grid $grid1p0  fluxfile_${fhr3}_1p00
  else 
    $WGRIB2 $COMOUT/${FLUXFL} $option1 $option21 $option22 $option23 $option24 \
                          $option25 $option26 $option27 $option28 \
                          -new_grid $grid1p0  fluxfile_${fhr3}_1p00
  fi
  export err=$?; err_chk


  $WGRIB2 -s fluxfile_${fhr3}_1p00 > $COMOUT/${PREFIX}flux.1p00.f${fhr3}.idx
  cp fluxfile_${fhr3}_1p00  $COMOUT/${PREFIX}flux.1p00.f${fhr3}

#---------------------------------------------------------------
echo "!!!!!CREATION OF SELECT $RUN FLUX PRODUCTS COMPLETED FOR FHR = $FH !!!!!"
#---------------------------------------------------------------


exit 0
