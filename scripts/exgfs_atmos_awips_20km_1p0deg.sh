#!/bin/ksh
##############################################################################
#  UTILITY SCRIPT NAME :  exgfs_awips_20km_1p0deg.sh
#         DATE WRITTEN :  11/01/2017
#
#  Abstract:  This utility script produces the GFS AWIPS 20km and 1.0 deg
#              grids GRIB2
#
#     Input:  1 arguments are passed to this script.
#             1st argument - Forecast Hour - format of 3I (3 digits)
#
###############################################################################
echo "------------------------------------------------"
echo "JGFS_AWIPS_00/06/12/18 GFS postprocessing"
echo "------------------------------------------------"
echo "History: NOV  2017 - First implementation of this new script to  "
echo "                     process GFS AWIPS 20km and 1.0 deg grids products "
echo " "
###############################################################################
fcsthrs="$1"
num=$#
job_name=`echo $job|sed 's/[jpt]gfs/gfs/'`

if test "$num" -ge 1
then
   echo ""
   echo " Appropriate number of arguments were passed"
   echo ""
else
   echo ""
   echo " Number of arguments were not passed "
   echo ""
   echo ""
   echo "Usage: $0  \$fcsthrs (3 digits) "
   echo ""
   exit 16
fi

cd $DATA

set -x

###############################################
# Wait for the availability of the pgrb file
###############################################
icnt=1
while [ $icnt -lt 1000 ]
do
  if [ -s $COMIN/${RUN}.${cycle}.pgrb2b.0p25.f$fcsthrs.idx ]
  then
     break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 180 ]
  then
    msg="ABORTING after 30 min of waiting for the GFS pgrb2 file!"
    err_exit $msg
  fi
done

########################################
msg="HAS BEGUN!"
postmsg "$jlogfile" "$msg"
########################################

echo " ------------------------------------------"
echo " BEGIN MAKING GFS AWIPS PRODUCTS"
echo " ------------------------------------------"

set +x
echo " "
echo "#######################################"
echo " Process GRIB AWIP GRIB2 PRODUCTS      "
echo "#######################################"
echo " "
set -x

# Set type of Interpolation for WGRIB2
export opt1=' -set_grib_type same -new_grid_winds earth '
export opt1uv=' -set_grib_type same -new_grid_winds grid '
export opt21=' -new_grid_interpolation bilinear -if '
export opt22=":(CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export opt23=' -new_grid_interpolation neighbor -fi '
export opt24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
export opt25=":(APCP|ACPCP|PRATE|CPRAT):"
export opt26=' -set_grib_max_bits 25 -fi -if '
export opt27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
export opt28=' -new_grid_interpolation budget -fi '
export TRIMRH=${TRIMRH:-$USHgfs/trim_rh.sh}

###############################################################
#    Process GFS GRIB AWIP PRODUCTS IN GRIB2                  #
###############################################################

cp $COMIN/gfs.t${cyc}z.pgrb2.0p25.f${fcsthrs}    tmpfile2${fcsthrs}
cp $COMIN/gfs.t${cyc}z.pgrb2b.0p25.f${fcsthrs}   tmpfile2b${fcsthrs}
cat tmpfile2${fcsthrs}   tmpfile2b${fcsthrs}  >  tmpfile${fcsthrs}
$WGRIB2 tmpfile${fcsthrs} | grep  -F -f $PARMproduct/gfs_awips_parmlist_g2 | $WGRIB2 -i -grib masterfile  tmpfile${fcsthrs}
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " FATAL ERROR: masterfile does not exist."
   exit $err
fi

$WGRIB2 masterfile -match ":PWAT:entire atmosphere" -grib gfs_pwat.grb
$WGRIB2 masterfile | grep -v ":PWAT:entire atmosphere" | $WGRIB2 -i -grib temp_gfs  masterfile
##################################################################
#  Process to change PWAT from level 200 to 10 (Entire Atmosphere)
#  in production defintion template (PDT) 4.0
##################################################################
$WGRIB2 gfs_pwat.grb -set_byte 4 23 10 -grib gfs_pwat_levels_10.grb
export err=$?; err_chk

cat temp_gfs   gfs_pwat_levels_10.grb > tmp_masterfile

for GRID in conus ak prico pac 003
do
   case $GRID in
     conus)
        #  Grid 20km_conus - CONUS - 20 km Quadruple Resolution (Lambert Conformal)
        # export grid_20km_conus="30 6 0 0 0 0 0 0 369 257 12190000 226541000 8 25000000 265000000 20318000 20318000 0 64 25000000 25000000 0 0"
        # $COPYGB2 -g "$grid_20km_conus" -i0 -x tmp_masterfile  awps_file_f${fcsthrs}_${GRID}

        export gridconus="lambert:265.0:25.0:25.0 226.541:369:20318.0 12.19:257:20318.0"
        $WGRIB2  tmp_masterfile $opt1uv $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridconus awps_file_f${fcsthrs}_${GRID}
        ;;
     ak)
        #  Grid 20km_ak - Alaska - Double Resolution (Polar Stereographic)
        #  Redefined grid 217 for Alaska region
        # export grid_20km_ak="20 6 0 0 0 0 0 0 277 213 30000000 187000000 8 60000000 225000000 22500000 22500000 0 64"
        # $COPYGB2 -g "$grid_20km_ak" -i0 -x tmp_masterfile  awps_file_f${fcsthrs}_${GRID}

        export gridak="nps:210.0:60.0 170.0:277:22500 35.0:225:22500"
        $WGRIB2  tmp_masterfile $opt1uv $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridak awps_file_f${fcsthrs}_${GRID}
        ;;
    prico)
        #  Grid 20km_prico - 0.25 degree Lat/Lon grid for Puerto Rico (20km)
        # export grid_20km_prico="0 6 0 0 0 0 0 0 275 205 0 0 50750000 271750000 48 -250000 340250000 250000 250000 0"
        # $COPYGB2 -g "$grid_20km_prico" -i0 -x tmp_masterfile  awps_file_f${fcsthrs}_${GRID}

        export gridprico="latlon 271.75:275:0.25 50.75:205:-0.25"
        $WGRIB2  tmp_masterfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridprico awps_file_f${fcsthrs}_${GRID}
        ;;
     pac)
        #  Grid 20km_pac - 20 km Mercator grid for Pacific Region
        # export grid_20km_pac="10 6 0 0 0 0 0 0 837 692 -45000000 110000000 48 20000000 65720000 270000000 64 0 20000000 20000000"
        # NEW export grid_20km_pac="10 6 0 0 0 0 0 0 837 725 -45000000 110000000 48 20000000 65734500 270000000 64 0 20000000 20000000"
        # $COPYGB2 -g "$grid_20km_pac" -i0 -x tmp_masterfile  awps_file_f${fcsthrs}_${GRID}

        export gridpac="mercator:20.0 110.0:837:20000:270.0 -45.0:725:20000:65.7345"
        $WGRIB2  tmp_masterfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridpac awps_file_f${fcsthrs}_${GRID}
        ;;
     003)
        ######################################################################
        #    Process GFS GRIB AWIP 1.0 DEGREE (GRID 003)  PRODUCTS IN GRIB2  #
        ######################################################################
        export grid003="latlon 0:360:1.0 90:181:-1.0"
        $WGRIB2  tmp_masterfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $grid003 awps_file_f${fcsthrs}_${GRID}
        ;;
   esac
   $TRIMRH awps_file_f${fcsthrs}_${GRID}   
   $GRB2INDEX awps_file_f${fcsthrs}_${GRID}  awps_file_fi${fcsthrs}_${GRID}

###########################################################################
# Checking fields in awps_file_f${fcsthrs}_${GRID} file
# before TOCGRIB2 adding WMO headers for AWIPS products.
#
# NOTE: numparm is the total of fields in grib2_awpgfs_20km_conusf000 file
###########################################################################
numparm=247
numrec=` $WGRIB2 awps_file_f${fcsthrs}_${GRID} | wc -l `

if [ $numrec -lt $numparm ]
then
    msg="ABORTING : awps_file_f${fcsthrs}_${GRID} file is missing fields for AWIPS !"
    err_exit $msg
fi

# Processing AWIPS GRIB2 grids with WMO headers

   pgm=tocgrib2
   export pgm; prep_step
   startmsg

   if [ $GRID = "003" -a `expr ${fcsthrs} % 6` -eq 0 ] ; then
      export FORT11=awps_file_f${fcsthrs}_${GRID}
      export FORT31=awps_file_fi${fcsthrs}_${GRID}
      export FORT51=grib2.awpgfs${fcsthrs}.${GRID}

      $TOCGRIB2 < $PARMwmo/grib2_awpgfs${fcsthrs}.${GRID} >> $pgmout 2> errfile
      export err=$?; err_chk
      echo " error from tocgrib2=",$err

      if [ $SENDCOM = "YES" ] ; then
        ##############################
        # Post Files to  ${COMOUTwmo}
        ##############################

        mv grib2.awpgfs${fcsthrs}.${GRID}    ${COMOUTwmo}/grib2.awpgfs${fcsthrs}.${GRID}.gfs_awips_f${fcsthrs}_1p0deg_${cyc}

        ##############################
        # Distribute Data
        ##############################

        if [ "$SENDDBN" = 'YES' -o "$SENDAWIP" = 'YES' ] ; then
           $DBNROOT/bin/dbn_alert NTC_LOW $NET $job  ${COMOUTwmo}/grib2.awpgfs${fcsthrs}.${GRID}.gfs_awips_f${fcsthrs}_1p0deg_${cyc}
        else
           msg="File ${COMOUTwmo}/grib2.awpgfs${fcsthrs}.${GRID}.gfs_awips_f${fcsthrs}_1p0deg_${cyc} not posted to db_net."
           postmsg "$jlogfile" "$msg"
        fi
      fi
   elif [ $GRID != "003" ] ; then
      export FORT11=awps_file_f${fcsthrs}_${GRID}
      export FORT31=awps_file_fi${fcsthrs}_${GRID}
      export FORT51=grib2.awpgfs_20km_${GRID}_f${fcsthrs}

      $TOCGRIB2 < $PARMwmo/grib2_awpgfs_20km_${GRID}f${fcsthrs} >> $pgmout 2> errfile
      export err=$? ;err_chk
      echo " error from tocgrib2=",$err

      if [ $SENDCOM = "YES" ] ; then

      ##############################
      # Post Files to  ${COMOUTwmo} 
      ##############################

      mv grib2.awpgfs_20km_${GRID}_f${fcsthrs}    ${COMOUTwmo}/grib2.awpgfs_20km_${GRID}_f${fcsthrs}.$job_name

      ##############################
      # Distribute Data
      ##############################

      if [ "$SENDDBN" = 'YES' -o "$SENDAWIP" = 'YES' ] ; then
         $DBNROOT/bin/dbn_alert NTC_LOW $NET $job  ${COMOUTwmo}/grib2.awpgfs_20km_${GRID}_f${fcsthrs}.$job_name
      else
         msg="File  ${COMOUTwmo}/grib2.awpgfs_20km_${GRID}_f${fcsthrs}.$job_name not posted to db_net."
         postmsg "$jlogfile" "$msg"
      fi
     fi
   fi
   msg="Awip Processing ${fcsthrs} hour completed normally"
   postmsg "$jlogfile" "$msg"

done

if [ -e "$pgmout" ] ; then
   cat $pgmout
fi

############################################################################################
# GOOD RUN
set +x
echo "**************JOB EXGFS_AWIPS_20KM_1P0DEG.SH.ECF COMPLETED NORMALLY ON THE WCOSS"
echo "**************JOB EXGFS_AWIPS_20KM_1P0DEG.SH.ECF COMPLETED NORMALLY ON THE WCOSS"
echo "**************JOB EXGFS_AWIPS_20KM_1P0DEG.SH.ECF COMPLETED NORMALLY ON THE WCOSS"
set -x
############################################################################################

msg="HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################
