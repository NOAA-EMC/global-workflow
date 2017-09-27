#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
## Author: Fanglin Yang   Org: NCEP/EMC  Date: October 2016
##         Rahul Mahajan  Org: NCEP/EMC  Date: April 2017

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

export GDATE=$($NDATE -$assim_freq $CDATE)
gymd=$(echo $GDATE | cut -c1-8)
ghh=$(echo  $GDATE | cut -c9-10)

# Default warm_start is OFF
export warm_start=".false."

# If RESTART conditions exist; warm start the model
# Restart conditions for GFS cycle come from GDAS
rCDUMP=$CDUMP
[[ $CDUMP = "gfs" ]] && rCDUMP="gdas"

if [ -f $ROTDIR/${rCDUMP}.$gymd/$ghh/RESTART/${cymd}.${chh}0000.coupler.res ]; then
    export warm_start=".true."
    if [ $CDUMP = "gfs" ]; then
        mkdir -p $ROTDIR/${CDUMP}.$gymd/$ghh/RESTART
        cd $ROTDIR/${CDUMP}.$gymd/$ghh/RESTART
        $NCP $ROTDIR/${rCDUMP}.$gymd/$ghh/RESTART/${cymd}.${chh}0000.* .
    fi
    if [ -f $ROTDIR/${CDUMP}.$cymd/$chh/${CDUMP}.t${chh}z.atminc.nc ]; then
        export read_increment=".true."
    else
        echo "WARNING: WARM START $CDUMP $CDATE WITHOUT READING INCREMENT!"
    fi
fi

# Forecast length for GFS forecast
if [ $CDUMP = "gfs" ]; then
    export FHMIN=$FHMIN_GFS
    export FHOUT=$FHOUT_GFS
    export FHMAX=$FHMAX_GFS
    export FHMAX_HF=$FHMAX_HF_GFS
    export FHOUT_HF=$FHOUT_HF_GFS
fi

res=$(echo $CASE | cut -c2-)
export JCAP=$((res*2-2))
export LONB=$((4*res))
export LATB=$((2*res))

###############################################################
# Run relevant exglobal script
$FORECASTSH
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Convert model native history files to nemsio

export DATA=$ROTDIR/${CDUMP}.$cymd/$chh

if [ $CDUMP = "gdas" ]; then

   if [ $OUTPUT_GRID = 'cubed_sphere_grid' -o $QUILTING = ".false." ]; then
    # Regrid 6-tile output to global array in NEMSIO gaussian grid for DA
    $REGRID_NEMSIO_SH
    status=$?
    [[ $status -ne 0 ]] && exit $status
   fi

elif [ $CDUMP = "gfs" ]; then

   if [ $OUTPUT_GRID = 'cubed_sphere_grid' -o $QUILTING = ".false." ]; then
    # Remap 6-tile output to global array in NetCDF latlon
    $REMAPSH
    status=$?
    [[ $status -ne 0 ]] && exit $status
   fi

   if [ $WRITE_NEMSIOFILE = ".false." -o $QUILTING = ".false." ]; then
    # Convert NetCDF to nemsio
    $NC2NEMSIOSH
    status=$?
    [[ $status -ne 0 ]] && exit $status
   fi

fi

###############################################################
# Exit out cleanly
exit 0
