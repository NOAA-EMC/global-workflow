#!/bin/sh
# This script changes the date of a sigma or surface file
if [[ $# -lt 2 ]];then
 echo Usage: $0 yyyymmddhh sigma.in [sigma.out]
 exit 1
fi
d=$1
[[ $d > 0000000000 && $d < 9999999999 ]]||exit 2
i=$2
[[ -s $i ]]||exit 2
o=${3:-$i}
if [ "$DATA" != "" ]
then
   t=$DATA/tmp$$
else
   t=/ptmpp1/tmp$$
fi
export XLFRTEOPTS="unit_vars=yes"
export XLFUNIT_11="$i"
export XLFUNIT_51="$t"
echo $d|/nwprod/util/exec/overdate.sigma ||exit 3
mv $t $o ||exit 3
