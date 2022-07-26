#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

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

export DATAROOT="$RUNDIR/$CDATE/$CDUMP"
[[ ! -d $DATAROOT ]] && mkdir -p $DATAROOT

export DATA="$DATAROOT/${job}.${pid}"
# DATA dir not used for now.

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
[[ ! -d $COMOUTocean ]] && mkdir -p $COMOUTocean
[[ ! -d $COMOUTice ]] && mkdir -p $COMOUTice

fhrlst=$(echo $FHRLST | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

export OMP_NUM_THREADS=1
export ENSMEM=${ENSMEM:-01}

export IDATE=$CDATE

for fhr in $fhrlst; do
  export fhr=$fhr
  VDATE=$($NDATE $fhr $IDATE)
  # Regrid the MOM6 and CICE5 output from tripolar to regular grid via NCL
  # This can take .25 degree input and convert to .5 degree - other opts avail
  # The regrid scripts use CDATE for the current day, restore it to IDATE afterwards
  export CDATE=$VDATE
  cd $DATA
  if [ $fhr -gt 0 ]; then
    export MOM6REGRID=${MOM6REGRID:-$HOMEgfs}
    $MOM6REGRID/scripts/run_regrid.sh
    status=$?
    [[ $status -ne 0 ]] && exit $status

    # Convert the netcdf files to grib2
    export executable=$MOM6REGRID/exec/reg2grb2.x
    $MOM6REGRID/scripts/run_reg2grb2.sh
    status=$?
    [[ $status -ne 0 ]] && exit $status
  

    #break up ocn netcdf into multiple files:  
    if [ -f $COMOUTocean/ocn_2D_$VDATE.$ENSMEM.$IDATE.nc ]; then 
      echo "File $COMOUTocean/ocn_2D_$VDATE.$ENSMEM.$IDATE.nc already exists"
    else
      ncks -x -v vo,uo,so,temp $COMOUTocean/ocn$VDATE.$ENSMEM.$IDATE.nc $COMOUTocean/ocn_2D_$VDATE.$ENSMEM.$IDATE.nc
      status=$?
      [[ $status -ne 0 ]] && exit $status
    fi 
    if [ -f $COMOUTocean/ocn_3D_$VDATE.$ENSMEM.$IDATE.nc ]; then 
       echo "File $COMOUTocean/ocn_3D_$VDATE.$ENSMEM.$IDATE.nc already exists" 
    else 
      ncks -x -v Heat_PmE,LW,LwLatSens,MLD_003,MLD_0125,SSH,SSS,SST,SSU,SSV,SW,cos_rot,ePBL,evap,fprec,frazil,latent,lprec,lrunoff,sensible,sin_rot,speed,taux,tauy,wet_c,wet_u,wet_v $COMOUTocean/ocn$VDATE.$ENSMEM.$IDATE.nc $COMOUTocean/ocn_3D_$VDATE.$ENSMEM.$IDATE.nc
      status=$?
      [[ $status -ne 0 ]] && exit $status
    fi 
    if [ -f $COMOUTocean/ocn-temp-EQ_$VDATE.$ENSMEM.$IDATE.nc ]; then 
       echo "File $COMOUTocean/ocn-temp-EQ_$VDATE.$ENSMEM.$IDATE.nc already exists" 
    else 
      ncks -v temp -d yh,503 -d xh,-299.92,60.03 $COMOUTocean/ocn_3D_$VDATE.$ENSMEM.$IDATE.nc $COMOUTocean/ocn-temp-EQ_$VDATE.$ENSMEM.$IDATE.nc
      status=$?
      [[ $status -ne 0 ]] && exit $status
    fi 
    if [ -f $COMOUTocean/ocn-uo-EQ_$VDATE.$ENSMEM.$IDATE.nc ]; then 
       echo "File $COMOUTocean/ocn-uo-EQ_$VDATE.$ENSMEM.$IDATE.nc already exists" 
    else 
      ncks -v uo -d yh,503 -d xh,-299.92,60.03 $COMOUTocean/ocn_3D_$VDATE.$ENSMEM.$IDATE.nc $COMOUTocean/ocn-uo-EQ_$VDATE.$ENSMEM.$IDATE.nc
      status=$?
      [[ $status -ne 0 ]] && exit $status
    fi
  fi

done

# Restore CDATE to what is expected
export CDATE=$IDATE
$NMV ocn_ice*.grb2 $COMOUTocean/
status=$?
[[ $status -ne 0 ]] && exit $status

# clean up working folder
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
###############################################################
# Exit out cleanly


exit 0
