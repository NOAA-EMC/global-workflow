#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exgfs_wave_post_gridded_sbs.sh
# Script description:  Creates output products from gridded binary WW3 data
#
# Abstract: This script is the postprocessor for the wave component in GFS.
#           This version runs side-by-side with the GFS fcst step.
#           It executes several scripts forpreparing and creating output data
#           as follows:
#
#  wave_grib2_sbs.sh         : generates GRIB2 files.
#  wave_grid_interp_ush.sh   : interpolates data from new grids to old grids
#
# COM inputs:
#
# Attributes:
#   Language: Bourne-again (Bash) Shell
#
###############################################################################

source "${USHgfs}/wave_domain_grid.sh"

DOGRI_WAV=${DOGRI_WAV:-"NO"}  # Interpolate to a grid
DOGRB_WAV=${DOGRB_WAV:-"YES"} # Create grib2 files

export waveGRD=${waveGRD?"FATAL ERROR: Required variable 'waveGRD' not set"}
export waveinterpGRD=${waveinterpGRD?"FATAL ERROR: Required variable 'wavepostGRD' not set"}
export wavepostGRD=${wavepostGRD?"FATAL ERROR: Required variable 'wavepostGRD' not set"}

cat << EOF
  INFO: Grid information:
  INFO:   Native wave grids:  ${waveGRD}
  INFO:   Interpolated grids: ${waveinterpGRD}
  INFO:   Post-process grids: ${wavepostGRD}
EOF

fhr3=$(printf %03i ${FORECAST_HOUR})
valid_time=$(date -u -d "${PDY} ${cyc} + ${FORECAST_HOUR} hours" "+%Y%m%d%H")

# Copy model definition files
for grdID in ${waveGRD} ${wavepostGRD} ${waveinterpGRD}; do
  cpreq "${COMIN_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" "mod_def.${grdID}"
done

# Copy model forecast data to DATA
cpreq "${COMIN_WAVE_HISTORY}/${RUN}.wave.t${cyc}z.${waveGRD}.f${fhr3}.bin" "./out_grd.${waveGRD}"

# Check for input templates for grib2 products (copying will be done in the grib2 script)
if [[ "${DOGRB_WAV}" == "YES" ]]; then
  for grbGRD in ${waveinterpGRD} ${wavepostGRD}; do
    if [[ ! -f "${PARMgfs}/wave/ww3_grib2.${grbGRD}.inp.tmpl" ]]; then
      export err=1
      err_exit "No template for grib generation"
    fi
  done
fi

# Data summary
cat << EOF
  INFO: Summary:
  INFO:   Grid interp: DOGRI_WAV="${DOGRI_WAV}"
  INFO:   Grib files:  DOGRB_WAV="${DOGRB_WAV}"
  INFO:   Fields to be included in grib files:
  INFO:     OUTPARS_WAV="${OUTPARS_WAV}"
EOF

if [[ "${DOGRB_WAV}" == "NO" ]]; then
  export err=1
  err_exit "DOGRB_WAV = NO; No grib2 products will be created, ABORT!"
fi

# 2.a Command file set-up
rm -f cmdfile.* cmdfile
count=0  # Counter for command files

# Products on the interpolation grid "waveinterpGRD"
if [[ "${DOGRI_WAV}" == "YES" ]]; then
  dt_int=3600.
  n_int=9999
  ymdh_int=$(date -u -d "${valid_time:0:8} ${valid_time:8:2} - ${WAVHINDH} hours" "+%Y%m%d%H")
  for grdID in ${waveinterpGRD}; do
    count=$(( count+1 ))
    echo "#!/bin/bash" > "cmdfile.${count}"
    echo "${USHgfs}/wave_grid_interp_sbs.sh ${grdID} ${ymdh_int} ${dt_int} ${n_int} > ${DATA}/grid_interp_${grdID}.out 2>&1" >> "${DATA}/cmdfile.${count}"
    echo "cat ${DATA}/grid_interp_${grdID}.out" >> "cmdfile.${count}"
    if [[ "${DOGRB_WAV}" == "YES" ]]; then
      process_grdID "${grdID}"
      echo "${USHgfs}/wave_grib2_sbs.sh ${grdID} ${GRIDNR} ${MODNR} ${valid_time} ${FORECAST_HOUR} ${GRDREGION} ${GRDRES} '${OUTPARS_WAV}' > ${DATA}/grib2_${grdID}.out 2>&1" >> "${DATA}/cmdfile.${count}"
      echo "cat ${DATA}/grib2_${grdID}.out" >> "${DATA}/cmdfile.${count}"
    fi
    chmod 755 "cmdfile.${count}"
    echo "${DATA}/cmdfile.${count}" >> "${DATA}/cmdfile"
  done
fi

# Products on the post-processing grid "wavepostGRD"
if [[ "${DOGRB_WAV}" == "YES" ]]; then
  for grdID in ${wavepostGRD}; do # First concatenate grib files for sbs grids
    count=$(( count+1 ))
    process_grdID "${grdID}"
    echo "#!/bin/bash" > "cmdfile.${count}"
    echo "${USHgfs}/wave_grib2_sbs.sh ${grdID} ${GRIDNR} ${MODNR} ${valid_time} ${FORECAST_HOUR} ${GRDREGION} ${GRDRES} '${OUTPARS_WAV}' > grib2_${grdID}.out 2>&1" >> "${DATA}/cmdfile.${count}"
    echo "cat ${DATA}/grib2_${grdID}.out" >> "${DATA}/cmdfile.${count}"
    chmod 755 "cmdfile.${count}"
    echo "${DATA}/cmdfile.${count}" >> "${DATA}/cmdfile"
  done
fi

# Ensure there are enough processors for MPMD else use serial
if [[ ${ntasks} -lt ${count} ]]; then
  if [[ "${USE_CFP:-}" == "YES" ]]; then
    echo "WARNING: Not enough processors for MPMD, '${ntasks} < ${count}', running in serial mode"
    export USE_CFP="NO"
  fi
fi

# Execute command file
echo "INFO: Running MPMD job with ${count} commands"
"${USHgfs}/run_mpmd.sh" "${DATA}/cmdfile" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
  err_exit "run_mpmd.sh failed!"
fi

# Check if grib2 file created
# TODO: Should this check be over all waveInterpGRD and wavePostGRD?
com_varname="COMOUT_WAVE_GRID_${GRDREGION}_${GRDRES}"
com_dir=${!com_varname}
gribchk="${RUN}.wave.t${cyc}z.${GRDREGION}.${GRDRES}.f${fhr3}.grib2"
if [[ ! -s "${com_dir}/${gribchk}" ]]; then
  export err=2
  err_exit "'${gribchk}' not generated in this job"
fi

exit 0
