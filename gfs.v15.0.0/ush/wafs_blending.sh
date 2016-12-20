#!/bin/ksh
#######################################################################
#########
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:        wafs_blending.sh 
# Script description:  this script retrieves US and UK WAFS Grib2 products, 
# performs averaging, and then write the new blended products in Grib2 to a 
# new Grib file
#
# Author:        Hui-Ya Chuang       Org: EMC         Date: 2010-12-30
#
# Script history log:
# 2010-12-30  Hui-Ya Chuang
#


set -x

cd $DATA

# retrieve UK products

cpfs $COMINuk/EGRR_WAFS_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 .
#Chuang: remove CAT data from UK unblended for testing
#$WGRIB2 EGRR_WAFS_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 |grep -v CAT|\
#$WGRIB2 -i EGRR_WAFS_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 -grib test.grib2
#rm -f ./EGRR_WAFS_unblended_${PDY}_${cyc}z_t${ffhr}.grib2
#mv test.grib2 ./EGRR_WAFS_unblended_${PDY}_${cyc}z_t${ffhr}.grib2

# pick up US data

cpfs ${COMINus}/grib2.t${cyc}z.wafs_grb_wifsf${ffhr}.45 .

# run blending code
startmsg
$EXECgfs/blending grib2.t${cyc}z.wafs_grb_wifsf${ffhr}.45 EGRR_WAFS_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 \
blended_${PDY}${cyc}f${ffhr}.grib2 > f${ffhr}.out

err1=$?
if test "$err1" -ne 0
then
 echo "WAFS blending program failed at " ${PDY}${cyc}F${ffhr} " turning back on dbn alert for unblended US WAFS product"
 SEND_US_WAFS=YES

##############################################################################################
#
#  checking any US WAFS product was sent due to No UK WAFS GRIB2 file or WAFS blending program
#
   if [ $SEND_US_WAFS = "YES" -a $SEND_AWC_ALERT = "NO" ] ; then
      msg="No UK WAFS GRIB2 file or WAFS blending program. Send alert message to AWC ......"
      postmsg "$jlogfile" "$msg"
      make_NTC_file.pl NOXX10 KKCI $PDY$cyc NONE $FIXgfs/wafs_admin_msg $PCOM/wifs_admin_msg
      make_NTC_file.pl NOXX10 KWBC $PDY$cyc NONE $FIXgfs/wafs_admin_msg $PCOM/iscs_admin_msg
      if [ $SENDDBN_NTC = "YES" ] ; then
           $DBNROOT/bin/dbn_alert NTC_LOW WAFS  $job $PCOM/wifs_admin_msg
           $DBNROOT/bin/dbn_alert NTC_LOW WAFS  $job $PCOM/iscs_admin_msg
      fi
      export SEND_AWC_ALERT=YES
   fi
##############################################################################################
 #
 #   Distribute US WAFS unblend Data to NCEP FTP Server (WOC) and TOC
 #
 echo "altering the unblended US WAFS products - $COMOUT/gfs.t${cyc}z.wafs_grb45f${ffhr}.grib2 "
 echo "and $COMOUT/gfs.t${cyc}z.wafs_grb45f${ffhr}.grib2.idx "
 echo "and $PCOM/grib2.t${cyc}z.wafs_grb_wifsf${ffhr}.45 "

 if [ $SENDDBN_GB2 = "YES" -a $SEND_US_WAFS = "YES" ] ; then
   $DBNROOT/bin/dbn_alert MODEL GFS_WAFSA_GB2 $job $COMOUT/gfs.t${cyc}z.wafs_grb45f${ffhr}.grib2
   $DBNROOT/bin/dbn_alert MODEL GFS_WAFSA_GB2_WIDX $job $COMOUT/gfs.t${cyc}z.wafs_grb45f${ffhr}.grib2.idx
 fi

 if [ $SENDDBN_NTC = "YES" -a $SEND_US_WAFS = "YES" ] ; then
 # $DBNROOT/bin/dbn_alert MODEL GFS_WAFSA_GB2 $job $PCOM/grib2.t${cyc}z.wafs_grb_wifsf${ffhr}.45
   $DBNROOT/bin/dbn_alert NTC_LOW $NET $job   $PCOM/grib2.t${cyc}z.wafs_grb_wifsf${ffhr}.45
 fi
 export SEND_US_WAFS=NO
exit
else
# put different field in different file with file names consistent with UK's
 export day=`echo $PDY | cut -c7-8`
 export uktime=$day$cyc
 if [ ${ffhr} = "06" ] ; then
  export ukffhr=C
 elif [ ${ffhr} = "09" ] ; then
  export ukffhr=D
 elif [ ${ffhr} = "12" ] ; then
  export ukffhr=E
 elif [ ${ffhr} = "15" ] ; then
  export ukffhr=F
 elif [ ${ffhr} = "18" ] ; then
  export ukffhr=G
 elif [ ${ffhr} = "21" ] ; then
  export ukffhr=H
 elif [ ${ffhr} = "24" ] ; then
  export ukffhr=I
 elif [ ${ffhr} = "27" ] ; then
  export ukffhr=J
 elif [ ${ffhr} = "30" ] ; then
  export ukffhr=K
 elif [ ${ffhr} = "33" ] ; then
  export ukffhr=L
 elif [ ${ffhr} = "36" ] ; then
  export ukffhr=M
 fi  
   
# else
#  echo "undefined time for UK time, exitting"          
#  exit
# fi

###########################################################################################
# 20130115 - RFC#978 - Remove the obsolete WAFS BLENDED UKMET individual GRIB files that 
# were used for testing during the initial set up of the WAFS blending job.
###########################################################################################

#$WGRIB2 -V blended_${PDY}${cyc}f${ffhr}.grib2 | grep "cumulonimbus base"|grep "ICAHT"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YHX${ukffhr}02_KWBC_${uktime}00.grib2
 
#$WGRIB2 -V blended_${PDY}${cyc}f${ffhr}.grib2 | grep "cumulonimbus top"|grep "ICAHT"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YHX${ukffhr}03_KWBC_${uktime}00.grib2
 
#$WGRIB2 -V blended_${PDY}${cyc}f${ffhr}.grib2 | grep "entire atmosphere"|grep "CBHE"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YBX${ukffhr}01_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:700 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}70_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:700 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}71_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:600 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}60_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:600 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}61_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:500 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}50_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:500 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}51_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:400 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}40_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:400 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}41_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:300 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}30_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CTP:300 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YFX${ukffhr}31_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:400 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}40_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:400 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}41_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:350 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}35_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:350 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}36_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:300 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}30_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:300 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}31_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:250 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}25_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:250 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}26_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:200 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}20_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:200 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}21_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:150 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}15_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "CAT:150 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YLX${ukffhr}16_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:800 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}80_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:800 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}81_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:700 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}70_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:700 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}71_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:600 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}60_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:600 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}61_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:500 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}50_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:500 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}51_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:400 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}40_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:400 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}41_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:300 mb"|grep "spatial ave"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}30_KWBC_${uktime}00.grib2
 
#$WGRIB2 blended_${PDY}${cyc}f${ffhr}.grib2 | grep "ICIP:300 mb"|grep "spatial max"| \
#$WGRIB2 -i blended_${PDY}${cyc}f${ffhr}.grib2 -grib YIX${ukffhr}31_KWBC_${uktime}00.grib2

fi

if [ $SENDCOM = YES ]; then
 cpfs blended_${PDY}${cyc}f${ffhr}.grib2 $COMOUT/WAFS_blended_${PDY}${cyc}f${ffhr}.grib2
#cp *${ukffhr}*KWBC_${uktime}00.grib2 $COMOUT/
fi

########################
#  testing  only       #
########################
#ls -ltr $COMOUT/*${ukffhr}*_KWBC_${uktime}00.grib2 |wc -l  
# for grib2_file in `ls $COMOUT/*${ukffhr}*_KWBC_${uktime}00.grib2`; do
#     echo "  ${grib2_file} is generated and ready for alerting .... "
# done

############################################################################################
#  Distribute Data to NCEP FTP Server (WOC) for parallel UK evulation, small files only    #
############################################################################################
#if [ $SENDDBN_GB2 = "YES" ] ; then
#   for grib2_file in `ls  $COMOUT/*${ukffhr}*_KWBC_${uktime}00.grib2`; do
#       $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE $job ${grib2_file}
#   done
#fi 


. prep_step
startmsg

# Processing WAFS Blending GRIB2 (Icing, CB, CAT)

export FORT11=blended_${PDY}${cyc}f${ffhr}.grib2
export FORT31=" "
export FORT51=grib2.t${cyc}z.WAFS_blended_f${ffhr}

$TOCGRIB2 <  $PARMgfs/grib2_blended_wafs_wifs_f${ffhr}.45 >> $pgmout 2> errfile

err=$?;export err ;err_chk
echo " error from tocgrib=",$err

if [ $SENDCOM = YES ]; then
 cpfs  grib2.t${cyc}z.WAFS_blended_f${ffhr}  $PCOM/grib2.t${cyc}z.WAFS_blended_f${ffhr}
fi

if [ $SENDDBN_NTC = "YES" ] ; then
#
#   Distribute Data to NCEP FTP Server (WOC) and TOC
#
#   #$DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE $job $PCOM/grib2.t${cyc}z.WAFS_blended_f${ffhr}
    $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/grib2.t${cyc}z.WAFS_blended_f${ffhr}
fi

if [ $SENDDBN_GB2 = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL GFS_WAFSA_BL_GB2 $job $COMOUT/WAFS_blended_${PDY}${cyc}f${ffhr}.grib2
fi 
