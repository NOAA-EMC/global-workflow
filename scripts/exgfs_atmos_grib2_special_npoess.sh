#!/bin/ksh
#####################################################################
echo "-----------------------------------------------------"
echo " exglobal_grib2_special_npoess.sh"
echo " Jan 2008 - Chuang - Produces 1x1 degree special Grib from master."
echo "-----------------------------------------------------"
#####################################################################

set -x

cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

############################################################
#  Define Variables:
#  -----------------
#  SHOUR        is the starting forecast hour. normally 0 except for restarts.
#  FHOUR        is the ending forecast hour.
#  FHINC        is the increment hour for each forecast steps.
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off pgrb 
#               generation.
############################################################

############################################################
# NO processing Analysis special Files 
############################################################

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

SLEEP_LOOP_MAX=`expr $SLEEP_TIME / $SLEEP_INT`

##############################################################################
# Specify Forecast Hour Range F000 - F024 for GFS_NPOESS_PGRB2_0P5DEG
##############################################################################
export SHOUR=000
export FHOUR=024
export fhr=$SHOUR
typeset -Z3 fhr
############################################################
# Loop Through the Post Forecast Files 
############################################################
while test $fhr -le $FHOUR
do

    ###############################
    # Start Looping for the
    # existence of the restart files
    ###############################
    export pgm="postcheck"
    ic=1
    while [ $ic -le $SLEEP_LOOP_MAX ]
    do
       if test -f $COMIN/gfs.t${cyc}z.pgrb2b.0p50.f${fhr}.idx
       then
          break
       else
          ic=`expr $ic + 1`
          sleep $SLEEP_INT
       fi
       ###############################
       # If we reach this point assume
       # fcst job never reached restart
       # period and error exit
       ###############################
       if [ $ic -eq $SLEEP_LOOP_MAX ]
       then
          export err=9
          err_chk
       fi
    done

######################################################################
# Process Global NPOESS 0.50 GFS GRID PRODUCTS IN GRIB2 F000 - F024  #
######################################################################
    set -x
    msg="Starting half degree grib generation for fhr=$fhr"
    postmsg "$jlogfile" "$msg"

    paramlist=${PARMproduct}/global_npoess_paramlist_g2
    cp $COMIN/gfs.t${cyc}z.pgrb2.0p50.f${fhr}  tmpfile2
    cp $COMIN/gfs.t${cyc}z.pgrb2b.0p50.f${fhr}  tmpfile2b
    cat tmpfile2    tmpfile2b   > tmpfile
    $WGRIB2 tmpfile | grep -F -f $paramlist | $WGRIB2 -i -grib  pgb2file tmpfile
    export err=$?; err_chk

    if test $SENDCOM = "YES"
    then
       cp pgb2file $COMOUT/${RUN}.${cycle}.pgrb2f${fhr}.npoess

       if test $SENDDBN = "YES"
       then
          $DBNROOT/bin/dbn_alert MODEL GFS_PGBNPOESS $job $COMOUT/${RUN}.${cycle}.pgrb2f${fhr}.npoess
       else
          msg="File ${RUN}.${cycle}.pgrb2f${fhr}.npoess not posted to db_net."
          postmsg "$msg"
       fi
       echo "$PDY$cyc$fhr" > $COMOUT/${RUN}.t${cyc}z.control.halfdeg.npoess
    fi
    rm tmpfile pgb2file
    export fhr=`expr $fhr + $FHINC`
    typeset -Z3 fhr

done

################################################################
# Specify Forecast Hour Range F000 - F180 for GOESSIMPGRB files 
################################################################
export SHOUR=000
export FHOUR=180
export fhr=$SHOUR
typeset -Z3 fhr

#################################
# Process GFS PGRB2_SPECIAL_POST
#################################

while test $fhr -le $FHOUR
do
    ###############################
    # Start Looping for the 
    # existence of the restart files
    ###############################
    set +x
    export pgm="postcheck"
    ic=1
    while [ $ic -le $SLEEP_LOOP_MAX ]
    do
       if test -f $restart_file$fhr
       then
          break
       else
          ic=`expr $ic + 1`
          sleep $SLEEP_INT
       fi
       ###############################
       # If we reach this point assume
       # fcst job never reached restart 
       # period and error exit
       ###############################
       if [ $ic -eq $SLEEP_LOOP_MAX ]
       then
          export err=9
          err_chk
       fi
    done
    set -x

    msg="Starting special grib file generation for fhr=$fhr"
    postmsg "$jlogfile" "$msg"

    ###############################
    # Put restart files into /nwges 
    # for backup to start Model Fcst
    ###############################

    cp $COMIN/${RUN}.t${cyc}z.special.grb2f$fhr masterfile

#    $COPYGB2 -g "0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0" -i1,1 -x masterfile pgb2file

#    export grid1p0="latlon 0:360:1.0 90:181:-1.0" 
    export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
    $WGRIB2  masterfile $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $grid0p25 pgb2file

# creating higher resolution goes files for US centers    
#    $COPYGB2 -g "30 6 0 0 0 0 0 0 349 277 1000000  214500000 8 50000000 253000000 32463000 32463000 0 64 50000000 50000000 0 0" -i1,1 -x masterfile pgb2file2 

    export gridconus="lambert:253.0:50.0:50.0 214.5:349:32463.0 1.0:277:32463.0"
    $WGRIB2  masterfile $opt1uv $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid $gridconus pgb2file2

    $WGRIB2 pgb2file -s > pgb2ifile

    if test $SENDCOM = "YES"
    then

       cp pgb2file $COMOUT/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr}
       cp pgb2ifile $COMOUT/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr}.idx

       cp pgb2file2 $COMOUT/${RUN}.${cycle}.goessimpgrb2f${fhr}.grd221

       if test $SENDDBN = "YES"
       then
          $DBNROOT/bin/dbn_alert MODEL GFS_GOESSIMPGB2_1P0 $job $COMOUT/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr}
          $DBNROOT/bin/dbn_alert MODEL GFS_GOESSIMPGB2_1P0_WIDX $job $COMOUT/${RUN}.${cycle}.goessimpgrb2.0p25.f${fhr}.idx
          $DBNROOT/bin/dbn_alert MODEL GFS_GOESSIMGRD221_PGB2 $job $COMOUT/${RUN}.${cycle}.goessimpgrb2f${fhr}.grd221
       fi

       echo "$PDY$cyc$fhr" > $COMOUT/${RUN}.t${cyc}z.control.goessimpgrb
    fi
    rm pgb2file2  pgb2ifile

    if test "$SENDECF" = 'YES'
    then
       export fhour=`expr ${fhr} % 6 `
    fi

    export fhr=`expr $fhr + $FHINC`
    typeset -Z3 fhr
done

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
