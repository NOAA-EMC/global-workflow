#!/bin/sh
set -x
#This script is used for modifing icee via land-sea mask
#Wen Meng 11/2019: First Version

f=$1

export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}

$WGRIB2 $optncpu $f \
        -if 'LAND' -rpn 'sto_1' -fi \
        -if 'ICEC' -rpn 'rcl_1:0:==:*' -fi \
        -set_grib_type same \
        -set_scaling same same \
        -grib_out $f.new 
export err=$?; err_chk
mv $f.new $f

exit 0

#-if 'ICEC' -rpn 'rcl_1:-1:*:1:+:*' -fi \


