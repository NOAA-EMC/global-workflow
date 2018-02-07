#!/bin/sh
# ---------------------------------------------------------------------
# author: Henry Juang  date: Feb 14, 2013
# purpose: Use to create Gaussian latitudes and reduced grid number for
#          all latitudes. The ouput is only north hemisphere, for the
#          Gaussian latitudes over south hemisphere are mirror image the
#          values of north hemisphere except with negative sign. For
#          the reduced grid number for south hemisphere is mirror 
#          image of north hemisphere.
# usage: to run this script, provide following 
#        export UTIL=/nwprod/util
#        export JCAP=1148
#        export LONF=2304
#        export LATG=1152
#        export OUTD=/tmp/reduce
#        then run script
#        $UTIL/ush/reduced_gaussian_grid.sh
# note: make sure you have FFTable LONF, which you can refer to
#       Acceptable_Lengths_for_the_Transforms.pdf
#       in $UTIL/sorc/reduced_gaussian_grid.fd
# ---------------------------------------------------------------------

set -ex

JCAP=${JCAP:-1148}
LONF=${LONF:-2304}
LATG=${LATG:-1152}
NUMREDUCE=4
echo "$JCAP $LONF $LATG $NUMREDUCE" >inp.$$

UTIL=${UTIL:-/gpfs/t3/global/save/wx23hh/2013/gfs/ticket48/util}
OUTD=${OUTD:-/gpfs/t3/global/save/wx23hh/2013/gfs/ticket48/util/fix}
pgm=reduced_gaussian_grid.exec
$UTIL/exec/$pgm <inp.$$

if [ $? = 0 ]; then
  mkdir -p $OUTD
  mv fort.10 $OUTD/global_gaussian_latitudes.t$JCAP.$LONF.$LATG.txt
  mv fort.20 $OUTD/global_lonsperlat.t$JCAP.$LONF.$LATG.txt
fi

rm inp.$$

