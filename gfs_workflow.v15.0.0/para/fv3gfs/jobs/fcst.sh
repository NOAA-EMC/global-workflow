#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
## Author: Rahul Mahajan  Org: NCEP/EMC  Date: April 2017

## Abstract:
## Model forecast driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base fcst"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env fcst
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
export DATA=$RUNDIR/$CDATE/$CDUMP/fcst
[[ -d $DATA ]] && rm -rf $DATA

cymd=$(echo $CDATE | cut -c1-8)
chh=$(echo  $CDATE | cut -c9-10)

# Default warm_start is OFF
export warm_start=".false."

# If RESTART conditions exist; warm start the model
if [ -f $ROTDIR/${CDUMP}.$cymd/$chh/RESTART/${cymd}.${chh}0000.coupler.res ]; then
    export warm_start=".true."
    if [ -f $ROTDIR/${CDUMP}.$cymd/$chh/${CDUMP}.t${chh}z.atminc.nc ]; then
        export read_increment=".true."
    else
        echo "WARNING: WARM START $CDUMP $CDATE WITHOUT READING INCREMENT!"
    fi
fi

# Restart files for GFS cycle come from the GDAS cycle
if [ $CDUMP = "gfs" ]; then
    if [ -f $ROTDIR/gdas.$cymd/$chh/RESTART/${cymd}.${chh}0000.coupler.res ]; then
        export warm_start=".true."
        mkdir -p $ROTDIR/${CDUMP}.$cymd/$chh/RESTART
        cd $ROTDIR/${CDUMP}.$cymd/$chh/RESTART
        $NCP $ROTDIR/gdas.$cymd/$chh/RESTART/${cymd}.${chh}0000.* .
        if [ -f $ROTDIR/${CDUMP}.$cymd/$chh/${CDUMP}.t${chh}z.atminc.nc ]; then
            export read_increment=".true."
        else
            echo "WARNING: WARM START $CDUMP $CDATE WITHOUT READING INCREMENT!"
        fi
    fi
fi

# Since we do not update SST, SNOW or ICE via global_cycle;
# Pass these to the model; it calls surface cycle internally
if [ $warm_start = ".true." ]; then
    export FNTSFA="$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.sstgrb"
    export FNACNA="$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.engicegrb"
    export FNSNOA="$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.snogrb"
fi

# Forecast length for GFS forecast
if [ $CDUMP = "gfs" ]; then
    export FHMIN=$FHMIN_GFS
    export FHOUT=$FHOUT_GFS
    export FHMAX=$FHMAX_GFS
fi

# Output frequency
if [ $FHMIN -eq 0 ]; then
    fdiag_=$(echo $DELTIM 3600 | awk '{printf "%f", $1/$2}')
else
    fdiag_=$FHMIN
fi
FHMIN_=$(($FHMIN + $FHOUT))
for fhr in `seq $FHMIN_ $FHOUT $FHMAX`; do
    fdiag_="$fdiag_,$fhr"
done
export fdiag=$fdiag_

###############################################################
# Run relevant exglobal script
$FORECASTSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Convert model native history files to nemsio

export DATA=$ROTDIR/${CDUMP}.$cymd/$chh

if [ $CDUMP = "gdas" ]; then

    # Regrid 6-tile output to global array in NEMSIO gaussian grid for DA
    $REGRID_NEMSIO_SH
    status=$?
    [[ $status -ne 0 ]] && exit $status

elif [ $CDUMP = "gfs" ]; then

    # Remap 6-tile output to global array in NetCDF latlon
    $REMAPSH
    status=$?
    [[ $status -ne 0 ]] && exit $status

    # Convert NetCDF to nemsio
    $NC2NEMSIOSH
    status=$?
    [[ $status -ne 0 ]] && exit $status

fi

###############################################################
# Exit out cleanly
exit 0
