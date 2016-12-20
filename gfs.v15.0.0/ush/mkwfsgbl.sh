#  UTILITY SCRIPT NAME :  mkwfsgbl.sh
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
job_name=`echo $job|sed 's/[jpt]gfs/gfs/'`

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
   echo "Usage: mkwfsgbl.sh \$hour [a|b]"
   echo ""
   exit 16
fi

echo " ------------------------------------------"
echo " BEGIN MAKING ${NET} WAFS PRODUCTS"
echo " ------------------------------------------"

msg="Enter Make WAFS utility."
postmsg "$jlogfile" "$msg"

for hour in $hour_list
do
   ##############################
   # Copy Input Field to $DATA
   ##############################

   if test ! -f pgrbf${hour}
   then
       cpfs $COMIN/${RUN}.${cycle}.pgrbf${hour} pgrbf${hour}

#      file name and forecast hour of GFS model data in Grib2 are 3 digits
#      hour000="$(printf "%03d" $hour)"
#      export fhr3=$hour
#      if test $fhr3 -lt 100
#      then
#         export fhr3="0$fhr3"
#      fi
#
#      $CNVGRIB -g21 $COMIN/${RUN}.${cycle}.pgrb2.1p00.f$fhr3  pgrbf${hour}

   fi

   #
   # BAG - Put in fix on 20070925 to force the percision of U and V winds
   #       to default to 1 through the use of the wafs.namelist file.
   #
   $COPYGB -g3 -i0 -N$PARMgfs/wafs.namelist -x pgrbf${hour} tmp
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

   export pgm=makewafs
   . prep_step

   export FORT11="pgrbf${hour}"
   export FORT31="pgrbif${hour}"
   export FORT51="xtrn.wfs${NET}${hour}${sets}"
   export FORT53="com.wafs${hour}${sets}"

   startmsg
   $EXECgfs/makewafs < $PARMgfs/grib_wfs${NET}${hour}${sets} >>$pgmout 2>errfile
   export err=$?;err_chk


   ##############################
   # Post Files to PCOM 
   ##############################

   if test "$SENDCOM" = 'YES'
   then
      cpfs xtrn.wfs${NET}${hour}${sets} $PCOM/xtrn.wfs${NET}${cyc}${hour}${sets}.$job_name
      cpfs com.wafs${hour}${sets} $PCOM/com.wafs${cyc}${hour}${sets}.$job_name

      if test "$SENDDBN_NTC" = 'YES'
      then
         if test "$NET" = 'gfs'
         then
               $DBNROOT/bin/dbn_alert MODEL GFS_WAFS $job \
                         $PCOM/com.wafs${cyc}${hour}${sets}.$job_name
               $DBNROOT/bin/dbn_alert MODEL GFS_XWAFS $job \
                         $PCOM/xtrn.wfs${NET}${cyc}${hour}${sets}.$job_name
         fi
      fi
   fi

   ##############################
   # Distribute Data 
   ##############################

   if [ "$SENDDBN_NTC" = 'YES' ] ; then
      $DBNROOT/bin/dbn_alert GRIB_LOW $NET $job $PCOM/xtrn.wfs${NET}${cyc}${hour}${sets}.$job_name
   else
      msg="xtrn.wfs${NET}${cyc}${hour}${sets}.$job file not posted to db_net."
      postmsg "$jlogfile" "$msg"
   fi

   msg="Wafs Processing $hour hour completed normally"
   postmsg "$jlogfile" "$msg"

done

exit
