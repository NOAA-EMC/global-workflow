#!/bin/ksh
echo "------------------------------------------------"
echo "JGFS_BULLS - 24hr GFS processing"
echo "------------------------------------------------"
echo "History: Jul 2004 - First implementation of this new script."
echo "         FBWNDGFS (FB Winds) program for 15 sites outside" 
echo "          the Hawaiian Islands."
echo "         Feb 2006 - L Sager  Send bulletins to TOC via NTC.  "
echo "         Jul 2014 - B Vuong  Modified to use GFS master GRIB2"
echo "                             and Add bulletins WINTEMV process."
echo "         Sep 2016 - B Vuong  Modified to use GFS 0p25 deg GRIB2"
echo "         Nov 2019 - B Vuong  Removed WINTEMV bulletin (retired)"
#####################################################################

cd $DATA

######################
# Set up Here Files.
######################

set -x
msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

job_name=`echo $job|sed 's/[jpt]gfs/gfs/'`

set +x
echo " "
echo "#############################################################"
echo " Process Bulletins of forecast winds and temps for Hawaii    "
echo " and 15 sites outside of the Hawaiian Islands.               "
echo "#############################################################"
echo " "
set -x

export pgm=bulls_fbwndgfs
. prep_step

for fhr in 006 012 024
do

  cp $COMIN/gfs.${cycle}.pgrb2.0p25.f${fhr}   tmp_pgrb2_0p25${fhr} 
  cp $COMIN/gfs.${cycle}.pgrb2b.0p25.f${fhr}  tmp_pgrb2b_0p25${fhr} 
  cat tmp_pgrb2_0p25${fhr} tmp_pgrb2b_0p25${fhr} > tmp0p25filef${fhr} 
  $WGRIB2 tmp0p25filef${fhr} | grep  -F -f $PARMproduct/gfs_fbwnd_parmlist_g2 | $WGRIB2 -i -grib tmpfilef${fhr} tmp0p25filef${fhr}
  $CNVGRIB -g21 tmpfilef${fhr} tmpfilef${fhr}.grib1
  $GRBINDEX tmpfilef${fhr}.grib1 tmpfilef${fhr}.grib1i
  mv tmpfilef${fhr}.grib1   gfs.t${cyc}z.grbf${fhr}_grb1
  mv tmpfilef${fhr}.grib1i  gfs.t${cyc}z.grbif${fhr}_grb1

done

export FORT11="gfs.t${cyc}z.grbf006_grb1"
export FORT12="gfs.t${cyc}z.grbf012_grb1"
export FORT13="gfs.t${cyc}z.grbf024_grb1"

#       GFS grib index files

export FORT31="gfs.t${cyc}z.grbif006_grb1"
export FORT32="gfs.t${cyc}z.grbif012_grb1"
export FORT33="gfs.t${cyc}z.grbif024_grb1"

#
#   1280 byte transmission file
#

export FORT51="tran.fbwnd_pacific"

startmsg

$EXECgfs/fbwndgfs < $PARMproduct/fbwnd_pacific.stnlist >> $pgmout 2> errfile
export err=$?; err_chk

if test "$SENDCOM" = 'YES'
then
    cp tran.fbwnd_pacific ${COMOUTwmo}/tran.fbwnd_pacific.$job_name
fi

if test "$SENDDBN" = 'YES'
then
#    make_ntc_bull.pl WMOBH NONE KWNO NONE tran.fbwnd_pacific ${COMOUTwmo}/tran.fbwnd_pacific.$job_name
   ${UTILgfs}/ush/make_ntc_bull.pl WMOBH NONE KWNO NONE tran.fbwnd_pacific ${COMOUTwmo}/tran.fbwnd_pacific.$job_name
fi

#
#  EMC is proposing to retire WINTEMV bulletin in GFS v16.0
#
   
# if test ${cycle} = 't00z' -o ${cycle} = 't12z'
# then
#
#      set +x
#      echo " "
#      echo "#################################################"
#      echo " Process 06, 12, 18 and 24 fcsthrs WINTEM Bulletins. "
#      echo "#################################################"
#      echo " "
#      set -x
#      sh $USHgfs/mkwintem.sh
#
#fi

#####################################################################
# GOOD RUN
set +x
echo "**************JOB JGFS_FBWIND COMPLETED NORMALLY ON IBM-SP"
echo "**************JOB JGFS_FBWIND COMPLETED NORMALLY ON IBM-SP"
echo "**************JOB JGFS_FBWIND COMPLETED NORMALLY ON IBM-SP"
set -x
#####################################################################

msg='Job completed normally.'
echo $msg
postmsg "$jlogfile" "$msg"

############################### END OF SCRIPT #######################
