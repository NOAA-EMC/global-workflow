#!/bin/ksh
# this script generates 0.25/0.5/1/2.5 deg pgb files for each small Grib file
# Author:       Hui-Ya Chuang

set -x
export CNVGRIB=${CNVGRIB:-$NWPROD/util/exec/cnvgrib21_gfs}
export FH=$1
export iproc=$2
export grid1p0="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
$COPYGB2 -g "${grid1p0}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_1p0

export grid0p5="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
$COPYGB2 -g "${grid0p5}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_0p5

export grid2p5="0 6 0 0 0 0 0 0 144 73 0 0 90000000 0 48 -90000000 357500000 2500000 2500000 0"
$COPYGB2 -g "${grid2p5}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_2p5

$CNVGRIB -g21 pgb2file_${FH}_${iproc}_1p0 pgbf${FH}_${iproc}.${CDUMP}.${CDATE}
$CNVGRIB -g21 pgb2file_${FH}_${iproc}_2p5 pgbfile_${FH}_${iproc}_2p5 
#$CNVGRIB -g21 tmpfile_${FH}_${iproc} pgbq${FH}_${iproc}.${CDUMP}.${CDATE}
