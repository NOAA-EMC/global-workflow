#! /usr/bin/env bash

#This is scripts is used to trim RH vaule larger than 100.
# Wen Meng 12/2017: First Version

source "$HOMEgfs/ush/preamble.sh"

f=$1

export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}

$WGRIB2 ${optncpu:-} $f -not_if ':RH:' -grib $f.new \
        -if ':RH:' -rpn "10:*:0.5:+:floor:1000:min:10:/" -set_grib_type same \
        -set_scaling -1 0 -grib_out $f.new 
export err=$?; err_chk
mv $f.new $f

exit 0
