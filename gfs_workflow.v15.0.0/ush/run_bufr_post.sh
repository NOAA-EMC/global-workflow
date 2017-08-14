#!/bin/sh
set -eua
#
#   This script originally written by Jack Woollen
#

[ $# -ne 5 ] && echo "$0 <siganl file> <cnvstat file> <prepbufr.in> <prepbufr.out> <date>"
[ $# -ne 5 ] && exit

siganl=$1 cnvstat=$2 prepbufr=$3 preppost=$4 date=$5

# run the old postevents first

DATA=`pwd` # working directory 
PREP=prepbufr_in_old_post; cp $prepbufr $PREP
SANL=$siganl; SANLA=
PSTX=$NWPROD/exec/global_postevents
PSTS=$NWPROD/ush/global_postevents.sh
$PSTS $PREP $date 

prepbufr=$PREP # prepbufr into new post is prepbufr out of old post

# now run the new postevents to layer on the gsi events

export DISK_GLOB=${DISK_GLOB:-/cdev/save}
export BASEDIR=${BASEDIR:-$DISK_GLOB/wx23sm/para_exp/cfs_para}
export CNVDIAGEXEC=${CNVDIAGEXEC:-$BASEDIR/exec/post_cnvdiag}
export DUPREPEXEC=${DUPREPEXEC:-$BASEDIR/exec/duprep.x}
export COMBFRSH=${COMBFRSH:-$BASEDIR/ush/combfr.sh}
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')

if [ $machine = IBMP6 ] ; then
  COMPRESS=compress
  UNCOMPRESS=uncompress
else
  COMPRESS=gzip
  UNCOMPRESS=gunzip
fi

# setup the convstat files

tar -xvf $cnvstat; $UNCOMPRESS diag*
mv diag_conv_anl.* diag_conv_anl 
mv diag_conv_ges.* diag_conv_ges 

#link the prepbufr input and run postevents

ln -sf $prepbufr fort.20
$CNVDIAGEXEC ###>stdout 2>&1
rm -f fort.20

#combine the postevents outp messages together

$COMBFRSH preppost_* prep_w_dups ; rm -f preppost_*

#run the duplicate elimination
delxy=.005 # this is the lat/lon tolerance in degrees
delhr=.001 # this is the ob time tolerance in hundredths of an hour
delelv=1    # this is the elevation tolerance in meters

cat<<eof|$DUPREPEXEC 
prep_w_dups 
$preppost
$delxy $delhr $delelv
eof

rm -f diag_conv_anl diag_conv_ges prep_w_dups fort.*
