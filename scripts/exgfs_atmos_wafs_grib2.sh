#!/bin/sh
######################################################################
#  UTILITY SCRIPT NAME :  exgfs_atmos_wafs_grib2.sh
#         DATE WRITTEN :  07/15/2009
#
#  Abstract:  This utility script produces the WAFS GRIB2. The output 
#             GRIB files are posted on NCEP ftp server and the grib2 files
#             are pushed via dbnet to TOC to WAFS (ICSC).  
#             This is a joint project of WAFC London and WAFC Washington.
#
#             We are processing WAFS grib2 for fcsthrs from 06 - 36 
#             with 3-hour time increment.
#
# History:  08/20/2014
#              - ingest master file in grib2 (or grib1 if grib2 fails)
#              - output of icng tcld cat cb are in grib2
#           02/21/2020
#              - Prepare unblended icing severity and GTG tubulence
#                for blending at 0.25 degree
#           02/22/2022
#              - Add grib2 data requested by FAA
#              - Stop generating grib1 data for WAFS
#####################################################################
echo "-----------------------------------------------------"
echo "JGFS_ATMOS_WAFS_GRIB2 at 00Z/06Z/12Z/18Z GFS postprocessing"
echo "-----------------------------------------------------"
echo "History: AUGUST  2009 - First implementation of this new script."
echo "Oct 2021 - Remove jlogfile"
echo "Feb 2022 - Add FAA data, stop grib1 data"
echo " "
#####################################################################

set -x

fcsthrs=$1

DATA=$DATA/$fcsthrs
mkdir -p $DATA
cd $DATA

##########################################################
# Wait for the availability of the gfs master pgrib file
##########################################################
# file name and forecast hour of GFS model data in Grib2 are 3 digits
export fcsthrs000="$(printf "%03d" $(( 10#$fcsthrs )) )"

# 2D data
master2=$COMIN/${RUN}.${cycle}.master.grb2f${fcsthrs000}
master2i=$COMIN/${RUN}.${cycle}.master.grb2if${fcsthrs000}
# 3D data
wafs2=$COMIN/${RUN}.${cycle}.wafs.grb2f${fcsthrs000}
wafs2i=$COMIN/${RUN}.${cycle}.wafs.grb2f${fcsthrs000}.idx
# 3D data (on ICAO standard level)
icao2=$COMIN/${RUN}.${cycle}.wafs_icao.grb2f${fcsthrs000}
icao2i=$COMIN/${RUN}.${cycle}.wafs_icao.grb2f${fcsthrs000}.idx

icnt=1
while [ $icnt -lt 1000 ]
do
    if [[ -s $master2i && -s $wafs2i ]] ; then
      break
    fi

    sleep 10
    icnt=$((icnt + 1))
    if [ $icnt -ge 180 ] ;    then
        msg="ABORTING after 30 min of waiting for the gfs master and wafs file!"
        err_exit $msg
    fi
done

########################################
echo "HAS BEGUN!"
########################################

echo " ------------------------------------------"
echo " BEGIN MAKING GFS WAFS GRIB2 PRODUCTS"
echo " ------------------------------------------"

set +x
echo " "
echo "#####################################"
echo "      Process GRIB WAFS PRODUCTS     "
echo " FORECAST HOURS 06 - 36."
echo "#####################################"
echo " "
set -x


if [ $fcsthrs -le 36 -a $fcsthrs -gt 0 ] ; then
    wafs_timewindow=yes
else
    wafs_timewindow=no
fi

#---------------------------
# 1) Grib2 data for FAA
#---------------------------
$WGRIB2 $master2 | grep -F -f $FIXgfs/grib2_gfs_awf_master.list | $WGRIB2 -i $master2 -grib tmpfile_gfsf${fcsthrs}
# F006 master file has two records of 0-6 hour APCP and ACPCP each, keep only one
# FAA APCP ACPCP: included every 6 forecast hour (0, 48], every 12 forest hour [48, 72] (controlled by $FIXgfs/grib2_gfs_awf_master3d.list)
if [ $fcsthrs -eq 6 ] ; then
    $WGRIB2 tmpfile_gfsf${fcsthrs} -not "(APCP|ACPCP)" -grib tmp.grb2
    $WGRIB2 tmpfile_gfsf${fcsthrs} -match APCP -append -grib tmp.grb2 -quit
    $WGRIB2 tmpfile_gfsf${fcsthrs} -match ACPCP -append -grib tmp.grb2 -quit
    mv tmp.grb2 tmpfile_gfsf${fcsthrs}
fi
# U V will have the same grid message number by using -ncep_uv.
# U V will have the different grid message number without -ncep_uv.
$WGRIB2 tmpfile_gfsf${fcsthrs} \
                      -set master_table 6 \
                      -new_grid_winds earth -set_grib_type jpeg \
                      -new_grid_interpolation bilinear -if ":(UGRD|VGRD):max wind" -new_grid_interpolation neighbor -fi \
                      -new_grid latlon 0:288:1.25 90:145:-1.25 gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2
$WGRIB2 -s gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2 > gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2.idx

# For FAA, add WMO header. The header is different from WAFS
export pgm=$TOCGRIB2
. prep_step
startmsg
export FORT11=gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2
export FORT31=" "
export FORT51=grib2.t${cyc}z.awf_grbf${fcsthrs}.45
$TOCGRIB2 <  $FIXgfs/grib2_gfs_awff${fcsthrs}.45 >> $pgmout 2> errfile
err=$?;export err ;err_chk
echo " error from tocgrib=",$err

if [ $wafs_timewindow = 'yes' ] ; then
#---------------------------
# 2) traditional WAFS fields
#---------------------------
    # 3D data from $wafs2, on exact model pressure levels
    $WGRIB2 $wafs2 | grep -F -f $FIXgfs/grib2_gfs_wafs_wafsmaster.list | $WGRIB2 -i $wafs2 -grib tmpfile_gfsf${fcsthrs}
    # 2D data from $master2
    tail -5 $FIXgfs/grib2_gfs_wafs_wafsmaster.list > grib2_gfs_wafs_wafsmaster.list.2D
    $WGRIB2 $master2 | grep -F -f grib2_gfs_wafs_wafsmaster.list.2D | $WGRIB2 -i $master2 -grib tmpfile_gfsf${fcsthrs}.2D
    # Complete list of WAFS data
    cat tmpfile_gfsf${fcsthrs}.2D >> tmpfile_gfsf${fcsthrs}
    # WMO header
    cp $FIXgfs/grib2_gfs_wafsf${fcsthrs}.45 wafs_wmo_header45
    # U V will have the same grid message number by using -ncep_uv.
    # U V will have the different grid message number without -ncep_uv.
    $WGRIB2 tmpfile_gfsf${fcsthrs} \
            -set master_table 6 \
            -new_grid_winds earth -set_grib_type jpeg \
            -new_grid_interpolation bilinear -if ":(UGRD|VGRD):max wind" -new_grid_interpolation neighbor -fi \
            -new_grid latlon 0:288:1.25 90:145:-1.25 gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2
    $WGRIB2 -s gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2 > gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2.idx

    # For WAFS, add WMO header. Processing WAFS GRIB2 grid 45 for ISCS and WIFS
    export pgm=$TOCGRIB2
    . prep_step
    startmsg
    export FORT11=gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2
    export FORT31=" "
    export FORT51=grib2.t${cyc}z.wafs_grbf${fcsthrs}.45
    $TOCGRIB2 < wafs_wmo_header45 >> $pgmout 2> errfile
    err=$?;export err ;err_chk
    echo " error from tocgrib=",$err

fi # wafs_timewindow

if [ $SENDCOM = "YES" ] ; then

    ##############################
    # Post Files to COM
    ##############################

    # FAA data
    mv gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2 $COMOUT/gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2
    mv gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2.idx $COMOUT/gfs.t${cyc}z.awf_grb45f${fcsthrs}.grib2.idx

    # WAFS data
    if [ $wafs_timewindow = 'yes' ] ; then
	mv gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2 $COMOUT/gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2
	mv gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2.idx $COMOUT/gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2.idx
    fi

    ##############################
    # Post Files to PCOM
    ##############################

    mv grib2.t${cyc}z.awf_grbf${fcsthrs}.45  $PCOM/grib2.t${cyc}z.awf_grbf${fcsthrs}.45

    if [ $wafs_timewindow = 'yes' ] ; then
	mv grib2.t${cyc}z.wafs_grbf${fcsthrs}.45  $PCOM/grib2.t${cyc}z.wafs_grbf${fcsthrs}.45
    fi
fi

######################
# Distribute Data
######################

if [ $SENDDBN = "YES" ] ; then

#  
#    Distribute Data to WOC
#  
    if [ $wafs_timewindow = 'yes' ] ; then
	$DBNROOT/bin/dbn_alert MODEL GFS_WAFS_1P25_GB2 $job $COMOUT/gfs.t${cyc}z.wafs_grb45f${fcsthrs}.grib2
#
#       Distribute Data to TOC TO WIFS FTP SERVER (AWC)
#
	$DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/grib2.t${cyc}z.wafs_grbf${fcsthrs}.45
    fi
#
#   Distribute data to FAA
#
    $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/grib2.t${cyc}z.awf_grbf${fcsthrs}.45


fi

################################################################################
# GOOD RUN
set +x
echo "**************JOB EXGFS_ATMOS_WAFS_GRIB2.SH COMPLETED NORMALLY ON THE IBM"
echo "**************JOB EXGFS_ATMOS_WAFS_GRIB2.SH COMPLETED NORMALLY ON THE IBM"
echo "**************JOB EXGFS_ATMOS_WAFS_GRIB2.SH COMPLETED NORMALLY ON THE IBM"
set -x
################################################################################

echo "HAS COMPLETED NORMALLY!"

exit 0

############## END OF SCRIPT #######################
