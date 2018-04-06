#!/bin/ksh
set -x

#This is scripts is used to trim RH vaule larger than 100.
# Wen Meng 12/2017: First Version

f=$1

export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}

$WGRIB2 $f -not_if ':RH:' -grib $f.new \
        -if ':RH:' -rpn "10:*:0.5:+:floor:1000:min:10:/" -set_grib_type same \
        -set_scaling -1 0 -grib_out $f.new 
mv $f.new $f

