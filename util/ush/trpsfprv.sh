#!/bin/sh

#####################################################################
# Script trpsfprv.sh generates the TRPSFPRV product for the GFS
#
# History: OCT 2004 Julia Zhu First implementation of this script
# History: SEPT 2005 Steve Lilly remove snd2forgn
# HISTORY Oct. 2005 - Converted fax graphics to T4 format and
#                     stopped writing to the stat file.
#####################################################################

cd $DATA

model=${model:-gfs}
set +x

echo "######################################"
echo " Execute TRPSFPRV"
echo "######################################"
set -x
  
###################################
# Copy Grib files
###################################

cp ${COMIN}/gfs.$cycle.pgrb2.1p00.f024  .
export err=$?
if [[ $err -ne 0 ]] ; then
    echo " File gfs.$cycle.pgrb2.1p00.f024 does not exist."
    exit $err
fi

cp ${COMIN}/gfs.$cycle.pgrb2.1p00.f048  .
export err=$?
if [[ $err -ne 0 ]] ; then
    echo " File gfs.$cycle.pgrb2.1p00.f048 does not exist."
    exit $err
fi
$CNVGRIB -g21 gfs.$cycle.pgrb2.1p00.f024 pgrbf24
export err=$?
if [[ $err -ne 0 ]] ; then
    echo " CNVGRIB failed to convert GRIB2 to GRIB1 "
    exit $err
fi
$GRBINDEX pgrbf24 pgrbif24
err1=$?

$CNVGRIB -g21 gfs.$cycle.pgrb2.1p00.f048 pgrbf48
export err=$?
if [[ $err -ne 0 ]] ; then
    echo " CNVGRIB failed to convert GRIB2 to GRIB1 "
    exit $err
fi
$GRBINDEX pgrbf48 pgrbif48
err2=$?

tot=`expr $err1 + $err2`
if test "$tot" -ne 0
then
   msg="File not yet available in com"
   postmsg "$jlogfile" "$msg"
   err_exit
fi

# cp $PARMshared/graph_trpsfprv_ft08.$cycle trpsfprv_ft08.$cycle
cp ${UTILgfs}/parm/graph_trpsfprv_ft08.$cycle trpsfprv_ft08.$cycle

# cp $PARMshared/graph_trpsfprv_ft05.$cycle trpsfprv_ft05.$cycle
cp ${UTILgfs}/parm/graph_trpsfprv_ft05.$cycle trpsfprv_ft05.$cycle

# cp $FIXshared/graph_awpseed awpseed
cp ${UTILgfs}/fix/graph_awpseed awpseed

# cp ${FIXshared}/graph_gphbg/mr4002.pur mr4002.pur
cp ${UTILgfs}/fix/graph_gphbg/mr4002.pur mr4002.pur
  
export pgm=trpsfprv;. prep_step
  
export FORT8="trpsfprv_ft08.$cycle" 
export FORT11="mr4002.pur"
export FORT12="pgrbf24"
export FORT22="pgrbif24"
export FORT15="pgrbf48"
export FORT25="pgrbif48"
export FORT55="f55"
export FORT60="f60"
export FORT61="f61"
export FORT62="f62"
export FORT63="f63"
export FORT71="ras"
export FORT72="rs2"
export FORT52="x6b"
export FORT48="awpseed"
export FORT81="trpsfprv.faxx.${model}_${cyc}"
  
msg="$pgm start"
postmsg "$jlogfile" "$msg"
  
# $TRPSFPRV < trpsfprv_ft05.$cycle >> $pgmout 2> errfile
${UTILgfs}/exec/trpsfprv < trpsfprv_ft05.$cycle >> $pgmout 2> errfile
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File ${UTILgfs}/exec/trpsfprv does not exist."
   exit $err
fi

echo "Leaving trpsfprv.sh"
exit
