#!/bin/ksh
######################################################################
#  UTILITY SCRIPT NAME :  exgfs_grib_awips.sh
#         DATE WRITTEN :  10/04/2004
#
#  Abstract:  This utility script produces the  GFS AWIPS GRIB
#
#     Input:  1 arguments are passed to this script.
#             1st argument - Forecast Hour - format of 2I
#
#####################################################################
echo "------------------------------------------------"
echo "JGFS_AWIPS_00/06/12/18 GFS postprocessing"
echo "------------------------------------------------"
echo "History: OCT 2004 - First implementation of this new script."
echo "         JUN 2014 - Modified to remove process for AWIPS in GRIB2"
echo "                    to script exgfs_grib_awips_g2.sh and this "
echo "                    script only process AWIPS GRIB1 (211 and 225)"
echo "         AUG 2015 - Modified for WCOSS phase2"
echo "         FEB 2019 - Removed grid 225"
#####################################################################
set +x
fcsthrs="$1"
num=$#
job_name=`echo $job|sed 's/[jpt]gfs/gfs/'`

typeset -Z3 fcsthrs

export PS4='gfs_grib_awips:f$fcsthrs:$SECONDS + '
export SCALEDEC=${SCALDEC:-$USHgfs/scale_dec.sh}

#if [ $fhcsthrs -t 100 ]; then
#  fcsthrs=0$fcsthrs
#fi  
if test "$num" -ge 1
then
   echo ""
   echo " Appropriate number of arguments were passed"
   echo ""
else
   echo ""
   echo " FATAL ERROR: Number of arguments were not passed."
   echo ""
   echo ""
   echo "Usage: $0  \$fcsthrs  (3-digit) "
   echo ""
   exit 16
fi

cd $DATA/awips_g1

set -x

###############################################
# Wait for the availability of the pgrb file
###############################################
icnt=1
while [ $icnt -lt 1000 ]
do
  if [ -s $COMIN/${RUN}.${cycle}.pgrb2b.0p25.f${fcsthrs}.idx ]
  then
     break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 180 ]
  then
    msg="ABORTING after 30 min of waiting for the pgrb file!"
    err_exit $msg
  fi
done

########################################
msg="HAS BEGUN!"
postmsg "$jlogfile" "$msg"
########################################

echo " ------------------------------------------"
echo " BEGIN MAKING GFS GRIB1 AWIPS PRODUCTS"
echo " ------------------------------------------"

set +x
echo " "
echo "###############################################"
echo " Process GFS GRIB1 AWIP PRODUCTS (211) "
echo "###############################################"
echo " "
set -x

   cp $COMIN/gfs.t${cyc}z.pgrb2.0p25.f${fcsthrs}   tmpfile2
   cp $COMIN/gfs.t${cyc}z.pgrb2b.0p25.f${fcsthrs}  tmpfile2b
   cat tmpfile2    tmpfile2b   >  tmpfile
   $WGRIB2 tmpfile | grep  -F -f $PARMproduct/gfs_awips_parmlist_g2 | $WGRIB2 -i -grib masterfile  tmpfile
   $SCALEDEC masterfile
   $CNVGRIB -g21  masterfile  masterfile.grib1

   ln -s masterfile.grib1   fort.11

#   $OVERGRIDID << EOF
   ${UTILgfs}/exec/overgridid << EOF
255
EOF

   mv fort.51 master.grbf${fcsthrs}
   rm fort.11

   $GRBINDEX master.grbf${fcsthrs}   master.grbif${fcsthrs}

###############################################################
#    Process GFS GRIB1 AWIP GRIDS 211 PRODUCTS
###############################################################

   executable=mkgfsawps
   DBNALERT_TYPE=GRIB_LOW

   startmsg

# GRID=211 out to 240 hours:

   export GRID=211
   export FORT11=master.grbf${fcsthrs}
   export FORT31=master.grbif${fcsthrs}
   export FORT51=xtrn.awpgfs${fcsthrs}.${GRID}
#   $MKGFSAWPS < $PARMwmo/grib_awpgfs${fcsthrs}.${GRID} parm=KWBC >> $pgmout 2>errfile
    ${UTILgfs}/exec/mkgfsawps < $PARMwmo/grib_awpgfs${fcsthrs}.${GRID} parm=KWBC >> $pgmout 2>errfile
   export err=$?; err_chk 
   ##############################
   # Post Files to ${COMOUTwmo}
   ##############################

   if test "$SENDCOM" = 'YES'
   then
      cp xtrn.awpgfs${fcsthrs}.${GRID} ${COMOUTwmo}/xtrn.awpgfs${fcsthrs}.${GRID}.$job_name

      ##############################
      # Distribute Data
      ##############################

      if [ "$SENDDBN" = 'YES' -o "$SENDAWIP" = 'YES' ] ; then
         $DBNROOT/bin/dbn_alert $DBNALERT_TYPE $NET $job ${COMOUTwmo}/xtrn.awpgfs${fcsthrs}.${GRID}.$job_name
      else
         msg="File $output_grb.$job_name not posted to db_net."
         postmsg "$jlogfile" "$msg"
      fi
   fi

if [ -e "$pgmout" ] ; then
   cat $pgmout
fi

###############################################################################
# GOOD RUN
set +x
echo "**************JOB EXGFS_GRIB_AWIPS.SH.ECF COMPLETED NORMALLY ON THE IBM"
echo "**************JOB EXGFS_GRIB_AWIPS.SH.ECF COMPLETED NORMALLY ON THE IBM"
echo "**************JOB EXGFS_GRIB_AWIPS.SH.ECF COMPLETED NORMALLY ON THE IBM"
set -x
###############################################################################

msg="HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################
