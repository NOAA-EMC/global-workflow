#! /usr/bin/env bash

#
#  This script uses WGRIB2 to change binary scale factor
#  and Decimal scale factor in GRIB2 file
#
#      -set_scaling D B
#  D = decimal scaling or the text 'same' with no quotes
#  B = binary scaling or the text 'same' with no quotes
#

source "$HOMEgfs/ush/preamble.sh"

f=$1

export WGRIB2=${WGRIB2:-${wgrib2_ROOT}/bin/wgrib2}

# export WGRIB2=/gpfs/dell1/nco/ops/nwprod/grib_util.v1.1.0/exec/wgrib2

$WGRIB2 $f -not_if ':(TMP|PWAT|WEASD):' -grib $f.new \
        -if ':(TMP|PWAT):' -set_grib_type same \
        -set_scaling -1 0 -grib_out $f.new \
        -if ':(WEASD):' -set_grib_type same \
        -set_scaling 0 0 -grib_out $f.new
export err=$?; err_chk
mv $f.new $f

exit 0
