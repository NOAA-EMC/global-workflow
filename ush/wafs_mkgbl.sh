#  UTILITY SCRIPT NAME :  wafs_mkgbl.sh
#               AUTHOR :  Mary Jacobs
#         DATE WRITTEN :  11/06/96
#
#  Abstract:  This utility script produces the GFS WAFS
#             bulletins.  
#
#     Input:  2 arguments are passed to this script.   
#             1st argument - Forecast Hour - format of 2I
#             2nd argument - In hours 12-30, the designator of 
#                            a  or  b.
#
#     Logic:   If we are processing hours 12-30, we have the
#              added variable of the   a   or    b, and process
#              accordingly.  The other hours, the a or b  is dropped.
#
echo "History: SEPT    1996 - First implementation of this utility script"
echo "History: AUG     1999 - Modified for implementation on IBM SP"
echo "                      - Allows users to run interactively" 
#

set -x
hour_list="$1"
sets_key=$2
num=$#

if test $num -ge 2
then
   echo " Appropriate number of arguments were passed"
   set -x
   if [ -z "$DATA" ]
   then
      export DATA=`pwd`
      cd $DATA
      setpdy.sh
      . PDY
   fi
else
   echo ""
   echo "Usage: wafs_mkgbl.sh \$hour [a|b]"
   echo ""
   exit 16
fi

echo " ------------------------------------------"
echo " BEGIN MAKING ${NET} WAFS PRODUCTS"
echo " ------------------------------------------"

echo "Enter Make WAFS utility."

for hour in $hour_list
do
   ##############################
   # Copy Input Field to $DATA
   ##############################

   if test ! -f pgrbf${hour}
   then
#       cp $COMIN/${RUN}.${cycle}.pgrbf${hour} pgrbf${hour}

#      file name and forecast hour of GFS model data in Grib2 are 3 digits
#      export fhr3=$hour
#      if test $fhr3 -lt 100
#      then
#         export fhr3="0$fhr3"
#      fi
       fhr3="$(printf "%03d" $(( 10#$hour )) )"

#      To solve Bugzilla #408: remove the dependency of grib1 files in gfs wafs job in next GFS upgrade
#      Reason: It's not efficent if simply converting from grib2 to grib1 (costs 6 seconds with 415 records)
#      Solution: Need to grep 'selected fields on selected levels' before CNVGRIB (costs 1 second with 92 records)
       ln -s $COMIN/${RUN}.${cycle}.pgrb2.1p00.f$fhr3  pgrb2f${hour}
       $WGRIB2 pgrb2f${hour} | grep -F -f $FIXgfs/grib_wafs.grb2to1.list | $WGRIB2 -i pgrb2f${hour} -grib pgrb2f${hour}.tmp
#       on Cray, IOBUF_PARAMS has to used to speed up CNVGRIB
#       export IOBUF_PARAMS='*:size=32M:count=4:verbose'
       $CNVGRIB -g21 pgrb2f${hour}.tmp  pgrbf${hour}
#       unset IOBUF_PARAMS
   fi

   #
   # BAG - Put in fix on 20070925 to force the percision of U and V winds
   #       to default to 1 through the use of the grib_wafs.namelist file.
   #
   $COPYGB -g3 -i0 -N$FIXgfs/grib_wafs.namelist -x pgrbf${hour} tmp
   mv tmp pgrbf${hour}
   $GRBINDEX pgrbf${hour} pgrbif${hour}

   ##############################
   # Process WAFS
   ##############################

   if test $hour -ge '12' -a $hour -le '30'
   then
       sets=$sets_key
       set +x
       echo "We are processing the primary and secondary sets of hours."
       echo "These sets are the   a   and   b   of hours 12-30."
       set -x
   else
     # This is for hours 00/06 and 36-72.
     unset sets
   fi

   export pgm=wafs_makewafs
   . prep_step

   export FORT11="pgrbf${hour}"
   export FORT31="pgrbif${hour}"
   export FORT51="xtrn.wfs${NET}${hour}${sets}"
   export FORT53="com.wafs${hour}${sets}"

   startmsg
   $EXECgfs/wafs_makewafs.x < $FIXgfs/grib_wfs${NET}${hour}${sets} >>$pgmout 2>errfile
   export err=$?;err_chk


   ##############################
   # Post Files to PCOM 
   ##############################

   if test "$SENDCOM" = 'YES'
   then
      cp xtrn.wfs${NET}${hour}${sets} $PCOM/xtrn.wfs${NET}${cyc}${hour}${sets}.$jobsuffix
#      cp com.wafs${hour}${sets} $PCOM/com.wafs${cyc}${hour}${sets}.$jobsuffix

#      if test "$SENDDBN_NTC" = 'YES'
#      then
#         if test "$NET" = 'gfs'
#         then
#               $DBNROOT/bin/dbn_alert MODEL GFS_WAFS $job \
#                         $PCOM/com.wafs${cyc}${hour}${sets}.$jobsuffix
#               $DBNROOT/bin/dbn_alert MODEL GFS_XWAFS $job \
#                         $PCOM/xtrn.wfs${NET}${cyc}${hour}${sets}.$jobsuffix
#         fi
#      fi
   fi

   ##############################
   # Distribute Data 
   ##############################

   if [ "$SENDDBN_NTC" = 'YES' ] ; then
      $DBNROOT/bin/dbn_alert GRIB_LOW $NET $job $PCOM/xtrn.wfs${NET}${cyc}${hour}${sets}.$jobsuffix
   else
      echo "xtrn.wfs${NET}${cyc}${hour}${sets}.$job file not posted to db_net."
   fi

   echo "Wafs Processing $hour hour completed normally"

done

exit
