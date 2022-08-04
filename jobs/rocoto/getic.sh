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

export DATA=${DATA:-${DATAROOT}/getic_${jobid:?}}
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

    if [[ $CDUMP == "gefs" ]]; then
      cd $EXTRACT_DIR
      if [[ $RUNMEM == "c00" ]]; then
        for stype in gfs gdas
        do
          if [[ $stype == "gfs" ]]; then
            yy_c=$yy; mm_c=$mm; dd_c=$dd; hh_c=$hh;
          else 
            yy_c=$gyy; mm_c=$gmm; dd_c=$gdd; hh_c=$ghh;
          fi
          htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/com_gfs_prod_${stype}.${yy}${mm}${dd}_${hh}.${stype}_restart.tar
          sPath_out=${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}
          if [ ! -d $sPath_out ]; then mkdir -p $sPath_out; fi
          mv ${EXTRACT_DIR}/gfs.${yy}${mm}${dd}/${hh}/${COMPONENT}/* ${sPath_out}/
          rm -rf ${stype}.${yy}${mm}${dd}
          cd $sPath_out
          for f in `ls ${stype}.*`; do fnew=${f/${stype}/gefs}; mv $f $fnew; done
          cd RESTART
          for f in `ls *tile*.nc`; do ncatted -a checksum,,d,, $f; done
          cd $EXTRACT_DIR
        done
      else
        case $cyc in
          00) memshift=0;;
          06) memshift=20;;
          12) memshift=40;;
          18) memshift=60;;
        esac
        nmem=`echo ${RUNMEM:-"c00"}|cut -c2-3`
        (( cmem = nmem + memshift ))
        if (( cmem > 80 )); then
          (( cmem = cmem - 80 ))
        fi
        memchar="mem"$(printf %03i $cmem)
        sgrp=$(($cmem/10 + 1))

        for CDATE_C in $CDATE $GDATE
        do
          export yy_c=$(echo $CDATE_C | cut -c1-4)
          export mm_c=$(echo $CDATE_C | cut -c5-6)
          export dd_c=$(echo $CDATE_C | cut -c7-8)
          export hh_c=$(echo $CDATE_C | cut -c9-10)

          shtar_path=${PRODHPSSDIR}/rh${yy_c}/${yy_c}${mm_c}/${yy_c}${mm_c}${dd_c}
          htar -xvf ${shtar_path}/com_gfs_prod_enkfgdas.${yy_c}${mm_c}${dd_c}_${hh_c}.enkfgdas_restart_grp${sgrp}.tar ./enkfgdas.${yy_c}${mm_c}${dd_c}/${hh_c}/atmos/$memchar
          sPath_out=${ROTDIR}/${CDUMP}.${yy_c}${mm_c}${dd_c}/${hh_c}/${RUNMEM}/${COMPONENT}
          if [ ! -d $sPath_out ]; then mkdir -p $sPath_out; fi
          mv ${EXTRACT_DIR}/enkfgdas.${yy_c}${mm_c}${dd_c}/${hh_c}/atmos/${memchar}/* ${sPath_out}/
          rm -rf enkfgdas.${yy_c}${mm_c}${dd_c}
          cd $sPath_out
          for f in `ls gdas.*`; do fnew=${f/gdas/gefs}; mv $f $fnew; done
          cd RESTART
          for f in `ls *tile*.nc`; do ncatted -a checksum,,d,, $f; done
          cd $EXTRACT_DIR
        done
      fi
    else
      cd $ROTDIR
      # Pull CDATE gfs restart tarball
      htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/com_gfs_prod_gfs.${yy}${mm}${dd}_${hh}.gfs_restart.tar
      # Pull GDATE gdas restart tarball
      htar -xvf ${PRODHPSSDIR}/rh${gyy}/${gyy}${gmm}/${gyy}${gmm}${gdd}/com_gfs_prod_gdas.${gyy}${gmm}${gdd}_${ghh}.gdas_restart.tar
    fi
  fi

else # Pull chgres cube inputs for cold start IC generation

  # Run UFS_UTILS GETICSH
  sh ${GETICSH} ${CDUMP}
  status=$?
  [[ $status -ne 0 ]] && exit $status

fi

# Move extracted data to ROTDIR
if [ ! -d ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT} ]; then mkdir -p ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT} ; fi
if [ $gfs_ver = v16 -a $RETRO = "YES" ]; then
  if [[ $CDUMP == "gefs" ]]; then
    ls -l
    mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}
  else
    mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}
  fi
else
  if [[ $CDUMP == "gefs" ]]; then
    ls -l
    if [ $gfs_ver = v13 -o $gfs_ver = v14 -o $gfs_ver = v15 ]; then
      mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}
    else
      if [[ $EXP_WARM_START = ".true." ]]; then
        echo "$RUNMEM is warm start"
      else
        mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}
      fi
    fi
  else
    mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/* ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}
  fi
fi

# Pull pgbanl file for verification/archival - v14+
if [ $gfs_ver = v14 -o $gfs_ver = v15 -o $gfs_ver = v16 ]; then
  for grid in 0p25 0p50 1p00
  do
    file=gfs.t${hh}z.pgrb2.${grid}.anl

    if [ $gfs_ver = v14 ]; then # v14 production source
      if [[ $CDUMP == "gefs" ]]; then
        cd $ROTDIR/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}
      else
        cd $ROTDIR/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}
      fi
      export tarball="gpfs_hps_nco_ops_com_gfs_prod_gfs.${yy}${mm}${dd}${hh}.pgrb2_${grid}.tar"
      htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./${file}
    elif [ $gfs_ver = v15 ]; then # v15 production source

      cd $EXTRACT_DIR
      export tarball="com_gfs_prod_gfs.${yy}${mm}${dd}_${hh}.gfs_pgrb2.tar"
      if [[ $CDUMP == "gefs" ]]; then
        htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./gfs.${yy}${mm}${dd}/${hh}/${file}
        mv ${EXTRACT_DIR}/gfs.${yy}${mm}${dd}/${hh}/${file} ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}/${file}
      else
        htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./${CDUMP}.${yy}${mm}${dd}/${hh}/${file}
        mv ${EXTRACT_DIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${file} ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${COMPONENT}/${file}
      fi

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
        if [[ $CDUMP == "gefs" ]]; then
          htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./gfs.${yy}${mm}${dd}/${hh}/atmos/${file}
          mv ./gfs.${yy}${mm}${dd}/${hh}/atmos/${file} ${ROTDIR}/${CDUMP}.${yy}${mm}${dd}/${hh}/${RUNMEM}/${COMPONENT}/${file}
          rm -rf ./gfs.${yy}${mm}${dd}
        else
          htar -xvf ${PRODHPSSDIR}/rh${yy}/${yy}${mm}/${yy}${mm}${dd}/${tarball} ./${CDUMP}.${yy}${mm}${dd}/${hh}/atmos/${file}
        fi

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
