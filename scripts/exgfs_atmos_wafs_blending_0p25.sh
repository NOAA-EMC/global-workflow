#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgfs_atmos_wafs_blending_0p25.sh (copied from exgfs_atmos_wafs_blending.sh)
# Script description:  This scripts looks for US and UK WAFS Grib2 products at 1/4 deg,
# wait for specified period of time, and then run $USHgfs/wafs_blending_0p25.sh
# if both WAFS data are available.  Otherwise, the job aborts with error massage
#
# Author:        Y Mao       Org: EMC         Date: 2020-04-02
#
#
# Script history log:
# 2020-04-02 Y Mao
# Oct 2021 - Remove jlogfile
# 2022-05-25 | Y Mao | Add ICAO new milestone Nov 2023

set -x
echo "JOB $job HAS BEGUN"
export SEND_AWC_US_ALERT=NO
export SEND_AWC_UK_ALERT=NO
export SEND_US_WAFS=NO
export SEND_UK_WAFS=NO

cd $DATA
export SLEEP_LOOP_MAX=`expr $SLEEP_TIME / $SLEEP_INT`

echo "start blending US and UK WAFS products at 1/4 degree for " $cyc " z cycle"
export ffhr=$SHOUR

export ic_uk=1

while test $ffhr -le $EHOUR
do

##########################
# look for US WAFS data
##########################

     export ic=1
     while [ $ic -le $SLEEP_LOOP_MAX ]
     do 
       if [ -s ${COMINus}/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2 ] ; then
          break
       fi
       if [ $ic -eq $SLEEP_LOOP_MAX ] ; then
          echo "US WAFS GRIB2 file  $COMINus/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2 not found after waiting over $SLEEP_TIME seconds"
	  echo "US WAFS GRIB2 file " $COMINus/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2 "not found after waiting ",$SLEEP_TIME, "exitting"
	  SEND_UK_WAFS=YES
	  break
       else
	   ic=`expr $ic + 1`
	   sleep $SLEEP_INT
       fi
     done

##########################
# look for UK WAFS data.
##########################

     SLEEP_LOOP_MAX_UK=$SLEEP_LOOP_MAX
     
    #  export ic=1
     while [ $ic_uk -le $SLEEP_LOOP_MAX_UK ]
     do
       # Three(3) unblended UK files for each cycle+fhour: icing, turb, cb
       ukfiles=`ls $COMINuk/EGRR_WAFS_0p25_*_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 | wc -l`
       if [ $ukfiles -ge 3 ] ; then
           break
       fi

       if [ $ic_uk -eq $SLEEP_LOOP_MAX_UK ] ; then
          echo "UK WAFS GRIB2 file  $COMINuk/EGRR_WAFS_0p25_*_unblended_${PDY}_${cyc}z_t${ffhr}.grib2  not found"
	  echo "UK WAFS GRIB2 file " $COMINuk/EGRR_WAFS_0p25_*_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 " not found"
          export SEND_US_WAFS=YES
	  break
       else
          ic_uk=`expr $ic_uk + 1`
          sleep $SLEEP_INT
       fi
     done

##########################
# If both UK and US data are missing.
##########################

     if [ $SEND_UK_WAFS = 'YES' -a $SEND_US_WAFS = 'YES' ] ; then
	 SEND_US_WAFS=NO
	 SEND_UK_WAFS=NO
	 echo "BOTH UK and US data are missing, no blended for $PDY$cyc$ffhr"
	 export err=1; err_chk
	 continue
     fi
 
##########################
# Blending or unblended
##########################

     if [ $SEND_US_WAFS = 'YES' ] ; then
	 echo "turning back on dbn alert for unblended US WAFS product"
     elif [ $SEND_UK_WAFS = 'YES' ] ; then
	 echo "turning back on dbn alert for unblended UK WAFS product"
	 # retrieve UK products
	 # Three(3) unblended UK files for each cycle+fhour: icing, turb, cb
	 cat $COMINuk/EGRR_WAFS_0p25_*_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 > EGRR_WAFS_0p25_unblended_${PDY}_${cyc}z_t${ffhr}.grib2
     else # elif [ $SEND_US_WAFS = "NO" -a $SEND_UK_WAFS = "NO" ] ; then
	 # retrieve UK products
	 # Three(3) unblended UK files for each cycle+fhour: icing, turb, cb
	 cat $COMINuk/EGRR_WAFS_0p25_*_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 > EGRR_WAFS_0p25_unblended_${PDY}_${cyc}z_t${ffhr}.grib2

	 # pick up US data
	 cp ${COMINus}/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2 .

	 # run blending code
	 export pgm=wafs_blending_0p25.x
	 . prep_step

	 startmsg
	 $EXECgfs/$pgm gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2 \
                              EGRR_WAFS_0p25_unblended_${PDY}_${cyc}z_t${ffhr}.grib2 \
                              0p25_blended_${PDY}${cyc}f${ffhr}.grib2 > f${ffhr}.out

	 err1=$?
	 if test "$err1" -ne 0
	 then
	     echo "WAFS blending 0p25 program failed at " ${PDY}${cyc}F${ffhr} " turning back on dbn alert for unblended US WAFS product"
	     SEND_US_WAFS=YES
	 fi
     fi

##########################
# Date dissemination
##########################

     if [ $SEND_US_WAFS = "YES" ] ; then

	 ##############################################################################################
	 #
	 #  checking any US WAFS product was sent due to No UK WAFS GRIB2 file or WAFS blending program
	 #  (Alert once for all forecast hours)
	 #
	 if [ $SEND_AWC_US_ALERT = "NO" ] ; then
	     echo "WARNING! No UK WAFS GRIB2 0P25 file for WAFS blending. Send alert message to AWC ......"
	     make_NTC_file.pl NOXX10 KKCI $PDY$cyc NONE $FIXgfs/wafs_blending_0p25_admin_msg $PCOM/wifs_0p25_admin_msg
	     make_NTC_file.pl NOXX10 KWBC $PDY$cyc NONE $FIXgfs/wafs_blending_0p25_admin_msg $PCOM/iscs_0p25_admin_msg
	     if [ $SENDDBN_NTC = "YES" ] ; then
		 $DBNROOT/bin/dbn_alert NTC_LOW WAFS  $job $PCOM/wifs_0p25_admin_msg
		 $DBNROOT/bin/dbn_alert NTC_LOW WAFS  $job $PCOM/iscs_0p25_admin_msg
	     fi

             if [ $envir != prod ]; then
		 export maillist='nco.spa@noaa.gov'
             fi
             export maillist=${maillist:-'nco.spa@noaa.gov,ncep.sos@noaa.gov'}
             export subject="WARNING! No UK WAFS GRIB2 0P25 file for WAFS blending, $PDY t${cyc}z $job"
             echo "*************************************************************" > mailmsg
             echo "*** WARNING! No UK WAFS GRIB2 0P25 file for WAFS blending ***" >> mailmsg
             echo "*************************************************************" >> mailmsg
             echo >> mailmsg
             echo "Send alert message to AWC ...... " >> mailmsg
             echo >> mailmsg
             cat mailmsg > $COMOUT/${RUN}.t${cyc}z.wafs_blend_0p25_usonly.emailbody
             cat $COMOUT/${RUN}.t${cyc}z.wafs_blend_0p25_usonly.emailbody | mail.py -s "$subject" $maillist -v

	     export SEND_AWC_US_ALERT=YES
	 fi
	 ##############################################################################################
	 #
	 #   Distribute US WAFS unblend Data to NCEP FTP Server (WOC) and TOC
	 #
	 echo "altering the unblended US WAFS products - $COMINus/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2 "
	 echo "and $COMINus/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2.idx "

	 if [ $SENDDBN = "YES" ] ; then
	     $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_0P25_UBL_GB2 $job $COMINus/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2
	     $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_0P25_UBL_GB2_WIDX $job $COMINus/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2.idx
	 fi

#	 if [ $SENDDBN_NTC = "YES" ] ; then
#	     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $COMOUT/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr}.grib2
#	 fi


	 export SEND_US_WAFS=NO

     elif [ $SEND_UK_WAFS = "YES" ] ; then
	 ##############################################################################################
	 #
	 #  checking any UK WAFS product was sent due to No US WAFS GRIB2 file
	 #  (Alert once for all forecast hours)
	 #
	 if [ $SEND_AWC_UK_ALERT = "NO" ] ; then
	     echo "WARNING: No US WAFS GRIB2 0P25 file for WAFS blending. Send alert message to AWC ......"
	     make_NTC_file.pl NOXX10 KKCI $PDY$cyc NONE $FIXgfs/wafs_blending_0p25_admin_msg $PCOM/wifs_0p25_admin_msg
	     make_NTC_file.pl NOXX10 KWBC $PDY$cyc NONE $FIXgfs/wafs_blending_0p25_admin_msg $PCOM/iscs_0p25_admin_msg
	     if [ $SENDDBN_NTC = "YES" ] ; then
		 $DBNROOT/bin/dbn_alert NTC_LOW WAFS  $job $PCOM/wifs_0p25_admin_msg
		 $DBNROOT/bin/dbn_alert NTC_LOW WAFS  $job $PCOM/iscs_0p25_admin_msg
	     fi

             if [ $envir != prod ]; then
                 export maillist='nco.spa@noaa.gov'
             fi
             export maillist=${maillist:-'nco.spa@noaa.gov,ncep.sos@noaa.gov'}
             export subject="WARNING! No US WAFS GRIB2 0P25 file for WAFS blending, $PDY t${cyc}z $job"
             echo "*************************************************************" > mailmsg
             echo "*** WARNING! No US WAFS GRIB2 0P25 file for WAFS blending ***" >> mailmsg
             echo "*************************************************************" >> mailmsg
             echo >> mailmsg
             echo "Send alert message to AWC ...... " >> mailmsg
             echo >> mailmsg
             cat mailmsg > $COMOUT/${RUN}.t${cyc}z.wafs_blend_0p25_ukonly.emailbody
             cat $COMOUT/${RUN}.t${cyc}z.wafs_blend_0p25_ukonly.emailbody | mail.py -s "$subject" $maillist -v

	     export SEND_AWC_UK_ALERT=YES
	 fi
	 ##############################################################################################
	 #
	 #   Distribute UK WAFS unblend Data to NCEP FTP Server (WOC) and TOC
	 #
	 echo "altering the unblended UK WAFS products - EGRR_WAFS_0p25_unblended_${PDY}_${cyc}z_t${ffhr}.grib2"

	 if [ $SENDDBN = "YES" ] ; then
	     $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_UKMET_0P25_UBL_GB2 $job EGRR_WAFS_0p25_unblended_${PDY}_${cyc}z_t${ffhr}.grib2
	 fi

#	 if [ $SENDDBN_NTC = "YES" ] ; then
#	     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job EGRR_WAFS_0p25_unblended_${PDY}_${cyc}z_t${ffhr}.grib2
#	 fi
	 export SEND_UK_WAFS=NO


     else
	 ##############################################################################################
	 #
	 # TOCGRIB2 Processing WAFS Blending GRIB2 (Icing, CB, GTG)

	 # As in August 2020, no WMO header is needed for WAFS data at 1/4 deg
	 ## . prep_step
	 ## export pgm=$TOCGRIB2
	 ## startmsg

	 ## export FORT11=0p25_blended_${PDY}${cyc}f${ffhr}.grib2
	 ## export FORT31=" "
	 ## export FORT51=grib2.t${cyc}z.WAFS_0p25_blended_f${ffhr}

	 ## $TOCGRIB2 <  $FIXgfs/grib2_blended_wafs_wifs_f${ffhr}.0p25 >> $pgmout 2> errfile

	 ## err=$?;export err ;err_chk
	 ## echo " error from tocgrib=",$err

	 ##############################################################################################
	 #
	 #   Distribute US WAFS unblend Data to NCEP FTP Server (WOC) and TOC
	 #
	 if [ $SENDCOM = YES ]; then
	     cp 0p25_blended_${PDY}${cyc}f${ffhr}.grib2 $COMOUT/WAFS_0p25_blended_${PDY}${cyc}f${ffhr}.grib2
	     ## cp grib2.t${cyc}z.WAFS_0p25_blended_f${ffhr}  $PCOM/grib2.t${cyc}z.WAFS_0p25_blended_f${ffhr}
	 fi

	 if [ $SENDDBN_NTC = "YES" ] ; then
	     #   Distribute Data to NCEP FTP Server (WOC) and TOC
	     echo "No WMO header yet"
	     ## $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/grib2.t${cyc}z.WAFS_0p25_blended_f${ffhr}
	 fi

	 if [ $SENDDBN = "YES" ] ; then
	     $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_0P25_BL_GB2 $job $COMOUT/WAFS_0p25_blended_${PDY}${cyc}f${ffhr}.grib2
	 fi 
     fi

##########################
# Next loop
##########################

     echo "$PDY$cyc$ffhr" > $COMOUT/${RUN}.t${cyc}z.control.wafsblending_0p25

     if [ $FHOUT_GFS -eq 3 ] ; then
	 FHINC=03
     else
	 if [ $ffhr -lt 24 ] ; then
	     FHINC=01
	 else
	     FHINC=03
	 fi
     fi

     ffhr=`expr $ffhr + $FHINC`
     if test $ffhr -lt 10
     then
         ffhr=0${ffhr}
     fi

done
################################################################################

exit 0
#
