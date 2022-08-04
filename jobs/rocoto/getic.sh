#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
## Abstract:
## Get GFS intitial conditions
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base getic init"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env getic
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables

export yy=$(echo $CDATE | cut -c1-4)
export mm=$(echo $CDATE | cut -c5-6)
export dd=$(echo $CDATE | cut -c7-8)
export hh=${cyc:-$(echo $CDATE | cut -c9-10)}
export GDATE=$($NDATE -${assim_freq:-"06"} $CDATE)
export gyy=$(echo $GDATE | cut -c1-4)
export gmm=$(echo $GDATE | cut -c5-6)
export gdd=$(echo $GDATE | cut -c7-8)
export ghh=$(echo $GDATE | cut -c9-10)

export DATA=${DATA:-${DATAROOT}/getic}
export EXTRACT_DIR=${DATA:-$EXTRACT_DIR}
export PRODHPSSDIR=${PRODHPSSDIR:-/NCEPPROD/hpssprod/runhistory}
export COMPONENT="atmos"
export gfs_ver=${gfs_ver:-"v16"}
export OPS_RES=${OPS_RES:-"C768"}
export GETICSH=${GETICSH:-${GDASINIT_DIR}/get_v16.data.sh}

# Create ROTDIR/EXTRACT_DIR
if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR ; fi
if [ ! -d $EXTRACT_DIR ]; then mkdir -p $EXTRACT_DIR ; fi
cd $EXTRACT_DIR

# Check version, cold/warm start, and resolution
if [[ $gfs_ver = "v16" && $EXP_WARM_START = ".true." && $CASE = $OPS_RES ]]; then # Pull warm start ICs - no chgres

  # Pull RESTART files off HPSS
  if [ ${RETRO:-"NO"} = "YES" ]; then # Retrospective parallel input

    # Pull prior cycle restart files
    htar -xvf ${HPSSDIR}/${GDATE}/gdas_restartb.tar
    status=$?
    [[ $status -ne 0 ]] && exit $status

    # Pull current cycle restart files
    htar -xvf ${HPSSDIR}/${CDATE}/gfs_restarta.tar
    status=$?
    [[ $status -ne 0 ]] && exit $status

    # Pull IAU increment files
    htar -xvf ${HPSSDIR}/${CDATE}/gfs_netcdfa.tar
    status=$?
    [[ $status -ne 0 ]] && exit $status

  else # Opertional input - warm starts

    cd $ROTDIR
    # Pull CDATE gfs restart tarball
    htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/com_gfs_prod_gfs.${yy}${mm}${dd}_${hh}.gfs_restart.tar
    # Pull GDATE gdas restart tarball
    htar -xvf ${PRODHPSSDIR}/rh${gyy}/${gyy}${gmm}/${gyy}${gmm}${gdd}/com_gfs_prod_gdas.${gyy}${gmm}${gdd}_${ghh}.gdas_restart.tar
  fi

else # Pull chgres cube inputs for cold start IC generation

  # Run UFS_UTILS GETICSH
  sh ${GETICSH} ${CDUMP}
  status=$?
  [[ $status -ne 0 ]] && exit $status

fi

# Move extracted data to ROTDIR
if [ ! -d ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT} ]; then mkdir -p ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT} ; fi
if [ $gfs_ver = v16 -a $RETRO = "YES" ]; then
  mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}
else
  mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}
fi

# Pull pgbanl file for verification/archival - v14+
if [ $gfs_ver = v14 -o $gfs_ver = v15 -o $gfs_ver = v16 ]; then
  for grid in 0p25 0p50 1p00
  do
    file=gfs.t${hh}z.pgrb2.${grid}.anl

    if [ $gfs_ver = v14 ]; then # v14 production source

      cd $ROTDIR/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}
      export tarball="gpfs_hps_nco_ops_com_gfs_prod_gfs.${yy}${mm}${dd}${hh}.pgrb2_${grid}.tar"
      htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./${file}

    elif [ $gfs_ver = v15 ]; then # v15 production source

      cd $EXTRACT_DIR
      export tarball="com_gfs_prod_gfs.${yy}${mm}${dd}_${hh}.gfs_pgrb2.tar"
      htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./${CDUMP}.${yy}${mm}${dd}/${hh}/${file}
      mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${file} ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}/${file}

    elif [ $gfs_ver = v16 ]; then # v16 - determine RETRO or production source next

      if [ $RETRO = "YES" ]; then # Retrospective parallel source

        cd $EXTRACT_DIR
        if [ $grid = "0p25" ]; then # anl file spread across multiple tarballs
          export tarball="gfsa.tar"
        elif [ $grid = "0p50" -o $grid = "1p00" ]; then
          export tarball="gfsb.tar"
        fi
        htar -xvf ${HPSSDIR}/${yy}${mm}${dd}${hh}/${tarball} ./${CDUMP}.${yy}${mm}${dd}/${hh}/${file}
        mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${file} ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}/${file}

      else # Production source

        cd $ROTDIR
        export tarball="com_gfs_prod_gfs.${yy}${mm}${dd}_${hh}.gfs_pgrb2.tar"
        htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./${CDUMP}.${yy}${mm}${dd}/${hh}/atmos/${file}

      fi # RETRO vs production

    fi # Version check
  done # grid loop
fi # v14-v16 pgrb anl file pull

##########################################
# Remove the Temporary working directory
##########################################
cd $DATAROOT
[[ $KEEPDATA = "NO" ]] && rm -rf $DATA

###############################################################
# Exit out cleanly


exit 0
