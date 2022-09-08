#!/bin/bash

 set -x

 rflux=$1
 fcsty=$2
 fflux=$3
 gflux=$4
 f=$5

 $WGRIB2 $rflux | grep "TMP:1 hybrid"     | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "SPFH:1 hybrid"    | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "UGRD:1 hybrid"    | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "VGRD:1 hybrid"    | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "HGT:1 hybrid"     | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "PRES:surface"     | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "PRATE:surface"    | grep ave  | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "VEG:surface"      | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "SFCR:surface"     | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "SFEXC:surface"    | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "TMP:surface"      | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "WEASD:surface"    | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "SNOD:surface"     | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
# $WGRIB2 $rflux | grep "SOILW:0-0"        | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
# $WGRIB2 $rflux | grep "SOILW:0.1"        | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
# $WGRIB2 $rflux | grep "SOILW:0.4"        | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux
# $WGRIB2 $rflux | grep "SOILW:1-2"        | grep $fcsty | $WGRIB2 -i $rflux -append -grib $fflux

 $WGRIB2 $rflux | grep "DSWRF:surface:$f hour fcst"  | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "DLWRF:surface:$f hour fcst"  | $WGRIB2 -i $rflux -append -grib $fflux
 $WGRIB2 $rflux | grep "USWRF:surface:$f hour fcst"  | $WGRIB2 -i $rflux -append -grib $fflux

#gds='255 4 3072 1536 89909 0 128 -89909 -117 117 768 0 0 0 0 0 0 0 0 0 255 0 0 0 0 0'
#$COPYGB -g"$gds" -x $fflux flux1534
#mv flux1534 $fflux

 $CNVGRIB -g21 $fflux $gflux

 exit $?
