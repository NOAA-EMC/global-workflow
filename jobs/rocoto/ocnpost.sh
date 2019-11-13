#!/bin/bash
set -x

###############################################################
## CICE5/MOM6 post driver script 
## FHRGRP : forecast hour group to post-process (e.g. 0, 1, 2 ...)
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

#############################
# Source relevant config files
#############################
configs="base ocnpost"
config_path=${EXPDIR:-$NWROOT/gfs.${gfs_ver}/parm/config}
for config in $configs; do
    . $config_path/config.$config
    status=$?
    [[ $status -ne 0 ]] && exit $status
done


##########################################
# Source machine runtime environment
##########################################
. $HOMEgfs/env/${machine}.env ocnpost
status=$?
[[ $status -ne 0 ]] && exit $status


##############################################
# Obtain unique process id (pid) and make temp directory
##############################################
export job=${job:-"ocnpost"}
export pid=${pid:-$$}
export outid=${outid:-"LL$job"}
export jobid=${jobid:-"${outid}.o${pid}"}

if [ $RUN_ENVIR = "nco" ]; then
    export DATA="$DATAROOT/${job}.${pid}"
else
    export DATAROOT="$RUNDIR/$CDATE/$CDUMP"
    export DATA="$DATAROOT/DATAocnpost$FHRGRP"
fi
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

##############################################
# Run setpdy and initialize PDY variables
##############################################
export cycle="t${cyc}z"
setpdy.sh
. ./PDY

##############################################
# Define the Log File directory
##############################################
export jlogfile=${jlogfile:-$COMROOT/logs/jlogfiles/jlogfile.${job}.${pid}}

##############################################
# Determine Job Output Name on System
##############################################
export pgmout="OUTPUT.${pid}"
export pgmerr=errfile


##############################################
# Set variables used in the exglobal script
##############################################
export CDATE=${CDATE:-${PDY}${cyc}}
export CDUMP=${CDUMP:-${RUN:-"gfs"}}
if [ $RUN_ENVIR = "nco" ]; then
    export ROTDIR=${COMROOT:?}/$NET/$envir
fi

##############################################
# Begin JOB SPECIFIC work
##############################################

if [ $RUN_ENVIR = "nco" ]; then
    export COMIN=${COMIN:-$ROTDIR/$RUN.$PDY/$cyc}
    export COMOUT=${COMOUT:-$ROTDIR/$RUN.$PDY/$cyc}
else
    export COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
    export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
fi
[[ ! -d $COMOUT ]] && mkdir -m 775 -p $COMOUT

if [ $FHRGRP -eq 0 ]; then
    fhrlst="anl"
else
    fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')
fi

export OMP_NUM_THREADS=1
export ENSMEM=${ENSMEM:-01}

export IDATE=$CDATE

#---------------------------------------------------------------
echo "PT DEBUG fhrlst is $fhrlst"

FHOUT=$FHOUT_GFS

for fhr in $fhrlst; do
  export fhr=$fhr
  #  --------------------------------------
  #  cp cice data to COMOUT directory
  #  --------------------------------------

  cd $RUNDIR/$IDATE/$CDUMP/fcst
  echo "PT DEBUG : Where am I?"
  pwd

  YYYY0=`echo $IDATE | cut -c1-4`
  MM0=`echo $IDATE | cut -c5-6`
  DD0=`echo $IDATE | cut -c7-8`
  HH0=`echo $IDATE | cut -c9-10`
  SS0=$((10#$HH0*3600))

  VDATE=$($NDATE $fhr $IDATE)
  YYYY=`echo $VDATE | cut -c1-4`
  MM=`echo $VDATE | cut -c5-6`
  DD=`echo $VDATE | cut -c7-8`
  HH=`echo $VDATE | cut -c9-10`
  SS=$((10#$HH*3600))

#  DDATE=$($NDATE -$FHOUT $VDATE)

  if [[ 10#$fhr -eq 0 ]]; then
    $NCP -p history/iceh_ic.${YYYY0}-${MM0}-${DD0}-`printf "%5.5d" ${SS0}`.nc $COMOUT/iceic$VDATE.$ENSMEM.$IDATE.nc
    status=$?
    [[ $status -ne 0 ]] && exit $status
    echo "fhr is 0, only copying ice initial conditions... exiting"
  else
    $NCP -p history/iceh_`printf "%0.2d" $FHOUT`h.${YYYY}-${MM}-${DD}-`printf "%5.5d" ${SS}`.nc $COMOUT/ice$VDATE.$ENSMEM.$IDATE.nc
    status=$?
    [[ $status -ne 0 ]] && exit $status
  fi

done
# copy ocn files
for fhr in $fhrlst; do
  export fhr=$fhr
  if [[ 10#$fhr -ge 6 ]]; then
  hh_inc_m=$((10#$FHOUT/2))
#hh_inc_m=3
#hh_in_o=6
  hh_inc_o=$((10#$FHOUT  ))

  # ------------------------------------------------------
  #  adjust the dates on the mom filenames and save
  # ------------------------------------------------------
  VDATE=$($NDATE $fhr $IDATE)
  YYYY=`echo $VDATE | cut -c1-4`
  MM=`echo $VDATE | cut -c5-6`
  DD=`echo $VDATE | cut -c7-8`
  HH=`echo $VDATE | cut -c9-10`
  SS=$((10#$HH*3600))

#  m_date=$($NDATE $hh_inc_m $DDATE)
#  p_date=$($NDATE $hh_inc_o $DDATE)

  m_date=$($NDATE -$hh_inc_m $VDATE)
  p_date=$VDATE

  # This loop probably isn't needed
    year=`echo $m_date | cut -c1-4`
    month=`echo $m_date | cut -c5-6`
    day=`echo $m_date | cut -c7-8`
    hh=`echo $m_date | cut -c9-10`

# ocn_2016_01_01_03.nc
# ocn_2016_01_01_09.nc
# ocn_2016_01_01_15.nc
# ocn_2016_01_01_21.nc

    export ocnfile=ocn_${year}_${month}_${day}_${hh}.nc

    echo "cp -p $ocnfile $COMOUT/ocn$p_date.$ENSMEM.$IDATE.nc"
    $NCP -p $ocnfile $COMOUT/ocn$p_date.$ENSMEM.$IDATE.nc
    status=$?
    [[ $status -ne 0 ]] && exit $status
  fi
done

#BL2019
  #  --------------------------
  #  interpolate ocn/ice netcdf output data to regular grid and generate grib2 files 
  #  --------------------------
for fhr in $fhrlst; do
#  export fhr=$fhr
  VDATE=$($NDATE $fhr $IDATE)
  # Regrid the MOM6 and CICE5 output from tripolar to regular grid via NCL
  # This can take .25 degree input and convert to .5 degree - other opts avail
  # The regrid scripts use CDATE for the current day, restore it to IDATE afterwards
  export CDATE=$VDATE
  cd $DATA
if [ $fhr -gt 0 ]; then
  export MOM6REGRID=$UGCSsrc/mom6_regrid_025
  $MOM6REGRID/run_regrid.sh
  status=$?
  [[ $status -ne 0 ]] && exit $status

  # Convert the netcdf files to grib2
  export executable=$MOM6REGRID/exec/reg2grb2.x
  $MOM6REGRID/run_reg2grb2.sh
  status=$?
  [[ $status -ne 0 ]] && exit $status
fi

done
# Restore CDATE to what is expected
  export CDATE=$IDATE
  echo $pwd
  $NMV SST*nc $COMOUT/
  $NMV ocnr*.nc $COMOUT
  $NMV ocnr*.grb2 $COMOUT
  status=$?
  [[ $status -ne 0 ]] && exit $status

# clean up working folder
#rm -Rf $DATA
#BL2019
###############################################################
# Exit out cleanly
exit 0
