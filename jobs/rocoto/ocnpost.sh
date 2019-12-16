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
    fhrlst=$(echo $OCN_FHRLST | sed -e 's/_/ /g; s/\[/ /g; s/\]/ /g; s/f/ /g; s/,/ /g')
fi

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
  $NMV ocn_ice*.grb2 $COMOUT
  status=$?
  [[ $status -ne 0 ]] && exit $status

# clean up working folder
#rm -Rf $DATA
#BL2019
###############################################################
# Exit out cleanly
exit 0
