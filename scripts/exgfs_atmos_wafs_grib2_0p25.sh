#!/bin/sh
######################################################################
#  UTILITY SCRIPT NAME :  exgfs_atmos_wafs_grib2_0p25.sh
#         DATE WRITTEN :  03/20/2020
#
#  Abstract:  This utility script produces the WAFS GRIB2 at 0.25 degree.
#             The output GRIB files are posted on NCEP ftp server and the
#             grib2 files are pushed via dbnet to TOC to WAFS (ICSC).  
#             This is a joint project of WAFC London and WAFC Washington.
#
#             We are processing WAFS grib2 for ffhr:
#             hourly: 006 - 024
#             3 hour: 027 - 048
#             6 hour: 054 - 120 (for U/V/T/RH, not for turbulence/icing/CB)
#
# History:  
#####################################################################
echo "-----------------------------------------------------"
echo "JGFS_ATMOS_WAFS_GRIB2_0P25 at 00Z/06Z/12Z/18Z GFS postprocessing"
echo "-----------------------------------------------------"
echo "History: MARCH  2020 - First implementation of this new script."
echo "Oct 2021 - Remove jlogfile"
echo "Aug 2022 - ffhr expanded from 36 to 120"
echo " "
#####################################################################

cd $DATA

set -x


ffhr=$1
export ffhr="$(printf "%03d" $(( 10#$ffhr )) )"
export ffhr2="$(printf "%02d" $(( 10#$ffhr )) )"

DATA=$DATA/$ffhr
mkdir -p $DATA
cd $DATA


if [ $ffhr -le 48 ] ; then
    hazard_timewindow=yes
else
    hazard_timewindow=no
fi


##########################################################
# Wait for the availability of the gfs WAFS file
##########################################################

# 3D data (on new ICAO model pressure levels) and 2D data (CB)
wafs2=$COMIN/${RUN}.${cycle}.wafs.grb2f${ffhr}
wafs2i=$COMIN/${RUN}.${cycle}.wafs.grb2f${ffhr}.idx

# 2D data from master file (U/V/H on max wind level, T/H at tropopause)
master2=$COMIN/${RUN}.${cycle}.master.grb2f${ffhr}

# 3D data (on standard atmospheric pressure levels)
# Up to fhour=48
# Will be removed in GFS.v17
icao2=$COMIN/${RUN}.${cycle}.wafs_icao.grb2f${ffhr}
  
icnt=1
while [ $icnt -lt 1000 ]
do
    if [[ -s $wafs2i ]] ; then
      break
    fi

    sleep 10
    icnt=$((icnt + 1))
    if [ $icnt -ge 180 ] ;    then
        msg="ABORTING after 30 min of waiting for the gfs wafs file!"
        err_exit $msg
    fi
done


########################################
echo "HAS BEGUN!"
########################################

echo " ------------------------------------------"
echo " BEGIN MAKING GFS WAFS GRIB2 0.25 DEG PRODUCTS"
echo " ------------------------------------------"

set +x
echo " "
echo "#####################################"
echo "      Process GRIB2 WAFS 0.25 DEG PRODUCTS     "
echo "#####################################"
echo " "
set -x

opt1=' -set_grib_type same -new_grid_winds earth '
opt21=' -new_grid_interpolation bilinear  -if '
opt22="(:ICESEV|parm=37):"
opt23=' -new_grid_interpolation neighbor -fi '
opt24=' -set_bitmap 1 -set_grib_max_bits 16 '
opt25=":(UGRD|VGRD):max wind"
newgrid="latlon 0:1440:0.25 90:721:-0.25"

# WAFS 3D data
$WGRIB2 $wafs2 $opt1 $opt21 $opt22 $opt23 $opt24 -new_grid $newgrid tmp_wafs_0p25.grb2
# Master 2D data
$WGRIB2 $master2 | grep -F -f $FIXgfs/grib2_0p25_gfs_master2d.list \
    | $WGRIB2 -i $master2 -set master_table 25 -grib tmp_master.grb2
$WGRIB2 tmp_master.grb2 $opt1 $opt21 ":(UGRD|VGRD):max wind" $opt23 $opt24 -new_grid $newgrid tmp_master_0p25.grb2

#---------------------------
# Product 1: WAFS u/v/t/rh gfs.tHHz.wafs_0p25.fFFF.grib2
#---------------------------
$WGRIB2 tmp_wafs_0p25.grb2 | egrep "UGRD|VGRD|TMP|HGT|RH" \
    | $WGRIB2 -i tmp_wafs_0p25.grb2 -set master_table 25 -grib tmp.gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2
cat tmp_master_0p25.grb2 >> tmp.gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2
# Convert template 5 to 5.40
#$WGRIB2 tmp.gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2 -set_grib_type jpeg -grib_out gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2
mv tmp.gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2 gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2
$WGRIB2 -s gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2 > gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2.idx

if [ $hazard_timewindow = 'yes' ] ; then
#---------------------------
# Product 2: For AWC and Delta airline: EDPARM CAT MWT ICESEV CB  gfs.tHHz.awf_0p25.fFFF.grib2
#---------------------------
    criteria1=":EDPARM:|:ICESEV:|parm=37:"
    criteria2=":CATEDR:|:MWTURB:"
    criteria3=":CBHE:|:ICAHT:"
    $WGRIB2 tmp_wafs_0p25.grb2 | egrep "${criteria1}|$criteria2|$criteria3" \
	| $WGRIB2 -i tmp_wafs_0p25.grb2 -grib gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2
    $WGRIB2 -s gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2 > gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2.idx

#---------------------------
# Product 3: WAFS unblended EDPARM, ICESEV, CB (No CAT MWT) gfs.tHHz.wafs_0p25_unblended.fFF.grib2
#---------------------------
    $WGRIB2 tmp_wafs_0p25.grb2 | grep -F -f $FIXgfs/grib2_0p25_gfs_hazard.list \
	| $WGRIB2 -i tmp_wafs_0p25.grb2 -set master_table 25 -grib tmp_wafs_0p25.grb2.forblend

    # Convert template 5 to 5.40
    #$WGRIB2 tmp_wafs_0p25.grb2.forblend -set_grib_type jpeg -grib_out gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2
    mv tmp_wafs_0p25.grb2.forblend gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2
    $WGRIB2 -s gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2 > gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2.idx
fi

if [ $SENDCOM = "YES" ] ; then

   ##############################
   # Post Files to COM
   ##############################

    mv gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2 $COMOUT/gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2
    mv gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2.idx $COMOUT/gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2.idx

   if [ $hazard_timewindow = 'yes' ] ; then
       mv gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2 $COMOUT/gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2
       mv gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2.idx $COMOUT/gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2.idx
       
       mv gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2 $COMOUT/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2
       mv gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2.idx $COMOUT/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2.idx
   fi

   #############################
   # Post Files to PCOM
   ##############################
   ## mv gfs.t${cyc}z.wafs_0p25_unblended_wifs.f${ffhr2}.grib2 $PCOM/gfs.t${cyc}z.wafs_0p25_unblended_wifs.f${ffhr2}.grib2
fi


if [ $SENDDBN = "YES" ] ; then
   ######################
   # Distribute Data
   ######################

    if [ $hazard_timewindow = 'yes' ] ; then
	# Hazard WAFS data (ICESEV EDR CAT MWT on 100mb to 1000mb or on new ICAO 2023 levels) sent to AWC and to NOMADS for US stakeholders
	$DBNROOT/bin/dbn_alert MODEL GFS_AWF_0P25_GB2 $job $COMOUT/gfs.t${cyc}z.awf_0p25.f${ffhr}.grib2

	# Unblended US WAFS data sent to UK for blending, to the same server as 1.25 deg unblended data: wmo/grib2.tCCz.wafs_grb_wifsfFF.45
	$DBNROOT/bin/dbn_alert MODEL GFS_WAFS_0P25_UBL_GB2 $job $COMOUT/gfs.t${cyc}z.wafs_0p25_unblended.f${ffhr2}.grib2
    fi

    # WAFS U/V/T/RH data sent to the same server as the unblended data as above
    $DBNROOT/bin/dbn_alert MODEL GFS_WAFS_0P25_GB2 $job $COMOUT/gfs.t${cyc}z.wafs_0p25.f${ffhr}.grib2

fi

################################################################################
# GOOD RUN
set +x
echo "**************JOB EXGFS_ATMOS_WAFS_GRIB2_0P25.SH COMPLETED NORMALLY ON THE IBM"
echo "**************JOB EXGFS_ATMOS_WAFS_GRIB2_0P25.SH COMPLETED NORMALLY ON THE IBM"
echo "**************JOB EXGFS_ATMOS_WAFS_GRIB2_0P25.SH COMPLETED NORMALLY ON THE IBM"
set -x
################################################################################

echo "HAS COMPLETED NORMALLY!"

exit 0

############## END OF SCRIPT #######################
