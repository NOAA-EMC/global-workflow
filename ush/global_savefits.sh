#!/bin/ksh
set -xeua
 
########################################################
#  save fit and horiz files for all analysis cycles
########################################################
export FIT_DIR=${FIT_DIR:-$COMOUT/fits}
export HORZ_DIR=${HORZ_DIR:-$COMOUT/horiz}
export fh1=06
export fh2=00
#
#dir=$FIT_DIR/$EXP 
dir=$FIT_DIR
if [ ! -d $dir ] ; then
  mkdir -p $dir
fi
cd $dir
/bin/cp $COMOUT/f$fh1.raob.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.raob.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh1.sfc.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.sfc.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh1.acar.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.acar.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh1.acft.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.acft.$CDATE .
[ $? -ne 0 ] && exit 8
export typ=anl
#dir=$HORZ_DIR/$EXP/$typ
dir=$HORZ_DIR/$typ
if [ ! -d $dir ] ; then
  mkdir -p $dir
fi
cd $dir
/bin/cp $COMOUT/adpupa.mand.$typ.$CDATE adpupa.mand.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/adpsfc.$typ.$CDATE adpsfc.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/sfcshp.$typ.$CDATE sfcshp.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/aircar.$typ.$CDATE aircar.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/aircft.$typ.$CDATE aircft.$CDATE
[ $? -ne 0 ] && exit 8
#########################################################################
#  save fit and horiz files for forecasts verifying at 00Z and 12Z cycles
#########################################################################
hh=$(echo $CDATE | cut -c9-10)
if [[ $hh = "00" || $hh = "12" ]] ; then
if [[ $hh = "00" ]] ; then
export fh1=24
export fh2=48
fi
if [[ $hh = "12" ]] ; then
export fh1=12
export fh2=36
fi
#dir=$FIT_DIR/$EXP 
dir=$FIT_DIR
if [ ! -d $dir ] ; then
  mkdir -p $dir
fi
cd $dir
/bin/cp $COMOUT/f$fh1.raob.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.raob.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh1.sfc.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.sfc.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh1.acar.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.acar.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh1.acft.$CDATE .
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/f$fh2.acft.$CDATE .
[ $? -ne 0 ] && exit 8
export typ=fcs
#dir=$HORZ_DIR/$EXP/$typ
dir=$HORZ_DIR/$typ
if [ ! -d $dir ] ; then
  mkdir -p $dir
fi
cd $dir
/bin/cp $COMOUT/adpupa.mand.$typ.$CDATE adpupa.mand.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/adpsfc.$typ.$CDATE adpsfc.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/sfcshp.$typ.$CDATE sfcshp.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/aircar.$typ.$CDATE aircar.$CDATE
[ $? -ne 0 ] && exit 8
/bin/cp $COMOUT/aircft.$typ.$CDATE aircft.$CDATE
[ $? -ne 0 ] && exit 8
fi
#########################################################################
