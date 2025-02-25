#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exgfs_wave_post_gridded_sbs.sh
# Script description:  Creates output products from binary WW3 data
#
# Author:   Jose-Henrique Alves Org: NCEP/EMC      Date: 2019-12-06
# Abstract: This script is the postprocessor for the wave component in GFS.
#           This version runs side-by-side with the GFS fcst step.
#           It executes several scripts forpreparing and creating output data
#           as follows:
#
#  wave_grib2_sbs.sh         : generates GRIB2 files.
#  wave_grid_interp_ush.sh   : interpolates data from new grids to old grids
#
# Script history log:
# 2019-12-06  J-Henrique Alves: First Version adapted from HTolman post.sh 2007
# 2020-06-10  J-Henrique Alves: Porting to R&D machine Hera
# 2020-07-31  Jessica Meixner: Removing points, now gridded data only
#
# COM inputs:
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (Bash) Shell
#
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"
source "${USHgfs}/wave_domain_grid.sh"

# 0.a Basic modes of operation

# Set wave model ID tag to include member number
export WAV_MOD_TAG="${RUN}.wave"

cd "${DATA}" || exit 1

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
if [[ -z ${NTASKS} ]]; then
  echo "FATAL ERROR: requires NTASKS to be set"
  err=1; export err; ${errchk}
  exit "${err}"
fi

# 0.c Defining model grids

# 0.c.1 Grids

export waveGRD=${waveGRD?Var waveGRD Not Set}

# 0.c.2 extended global grid and rtma transfer grid
export waveinterpGRD=${waveinterpGRD?Var wavepostGRD Not Set}
export wavepostGRD=${wavepostGRD?Var wavepostGRD Not Set}

cat << EOF
Grid information  :
-------------------
   Native wave grids  : ${waveGRD}
   Interpolated grids : ${waveinterpGRD}
   Post-process grids : ${wavepostGRD}
EOF

export FHRUN=0

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

export DOGRB_WAV=${DOGRB_WAV:-'YES'} #Create grib2 files
export DOGRI_WAV=${DOGRI_WAV:-'NO'} #Create interpolated grids

cat << EOF
Preparing input files :
-----------------------
EOF

# 1.a Model definition files and output files (set up using poe)

# 1.a.1 Copy model definition files
# Eliminate duplicate grids
declare -A grdALL
for grd in ${waveGRD} ${wavepostGRD} ${waveinterpGRD}; do
  # For ease of access, make the value the same as the key
  grdALL["${grd}"]="${grd}"
done

for grdID in "${grdALL[@]}"; do
  if [[ -f "${COMIN_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" ]]; then
    echo "INFO: Mod def file for ${grdID} found in ${COMIN_WAVE_PREP}. copying ...."
    cp -f "${COMIN_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" "mod_def.${grdID}"
  fi
done

# 1.a.2 Check that model definition files exist
for grdID in "${grdALL[@]}"; do
  if [[ ! -f "mod_def.${grdID}" ]]; then
    echo "FATAL ERROR : No mod_def file mod_def.${grdID}"
    err=2; export err;${errchk}
    exit "${err}"
  else
    echo "INFO: File mod_def.${grdID} found. Syncing to all nodes ..."
  fi
done

# 1.b Input template files

if [[ "${DOGRI_WAV}" == 'YES' ]]; then
  for intGRD in ${waveinterpGRD}; do
    if [[ -f "${PARMgfs}/wave/${intGRD}_interp.inp.tmpl" ]]; then
      cp -f "${PARMgfs}/wave/${intGRD}_interp.inp.tmpl" "${intGRD}_interp.inp.tmpl"
    fi

    if [[ -f "${intGRD}_interp.inp.tmpl" ]]; then
      echo "${intGRD}_interp.inp.tmpl copied. Syncing to all nodes ..."
    else
      echo "FATAL ERROR: No template for ${intGRD} input file"
      err=1
      DOGRI_WAV='NO'
    fi
  done
fi

if [[ "${DOGRB_WAV}" == 'YES' ]]; then
  for grbGRD in ${waveinterpGRD} ${wavepostGRD}; do
    if [[ -f "${PARMgfs}/wave/ww3_grib2.${grbGRD}.inp.tmpl" ]]; then
      cp -f "${PARMgfs}/wave/ww3_grib2.${grbGRD}.inp.tmpl" "ww3_grib2.${grbGRD}.inp.tmpl"
    fi

    if [[ -f "ww3_grib2.${grbGRD}.inp.tmpl" ]]; then
      echo "INFO: ww3_grib2.${grbGRD}.inp.tmpl copied."
    else
      echo "FATAL ERROR: No template for ${grbGRD} grib input file"
      err=2
      DOGRB_WAV='NO'
    fi
  done
fi


# 1.c Data summary
#shellcheck disable=SC2312
cat << EOF

Input files read and processed at : $(date)

Data summary:
---------------------------------------------
  Sufficient data for GRID interpolation    : ${DOGRI_WAV}
  Sufficient data for GRIB files            : ${DOGRB_WAV}

EOF

# --------------------------------------------------------------------------- #
# 2.  Make consolidated grib2 file for side-by-side grids and interpolate
#     onto extended grids
#
# 2.a Command file set-up

echo 'INFO: Making command file for grib2 and grid interpolation'
valid_time=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} +${FORECAST_HOUR} hours")
fhr3=$(printf '%03i' "${FORECAST_HOUR}")

rm -f "mpmd_script"
touch "mpmd_script"

# Input model data
gfile="${RUN}.wave.t${cyc}z.${waveGRD}.f${fhr3}.bin"
if [[ ! -s "${COMIN_WAVE_HISTORY}/${gfile}" ]]; then
  echo "FATAL ERROR: No raw field output file ${COMIN_WAVE_HISTORY}/${gfile}"
  err=3; export err; "${errchk}"
  exit "${err}"
fi
cp "${COMIN_WAVE_HISTORY}/${gfile}" "./out_grd.${waveGRD}"

if [[ "${DOGRI_WAV}" == 'YES' ]]; then
  for grdID in ${waveinterpGRD}; do
    interp_time=$(date --utc +%Y%m%d%H -d "${valid_time:0:8} ${valid_time:8:2} -${WAVHINDH} hours")
    dt_int=3600.
    n_int=9999
    echo "#! /usr/bin/env bash" > "${grdID}.sh"
    echo "${USHgfs}/wave_grid_interp_sbs.sh ${grdID} ${interp_time} ${dt_int} ${n_int} ${FORECAST_HOUR}" >> "${grdID}.sh"
    if [[ "${DOGRB_WAV}" == 'YES' ]]; then
      gribFL="${OUTPARS_WAV}"
      process_grdID "${grdID}"
      echo "${USHgfs}/wave_grib2_sbs.sh ${grdID} ${GRIDNR} ${MODNR} ${valid_time} ${FORECAST_HOUR} ${GRDREGION} ${GRDRES} '${gribFL}'" >> "${grdID}.sh"
    fi
    echo "${DATA}/${grdID}.sh" >> "mpmd_script"
    chmod 744 "${grdID}.sh"
  done
fi

if [[ "${DOGRB_WAV}" == 'YES' ]]; then
  # First concatenate grib files for sbs grids
  for grdID in ${wavepostGRD}; do
    gribFL="${OUTPARS_WAV}"
    process_grdID "${grdID}"
    echo "${USHgfs}/wave_grib2_sbs.sh ${grdID} ${GRIDNR} ${MODNR} ${valid_time} ${FORECAST_HOUR} ${GRDREGION} ${GRDRES} '${gribFL}'" >> "mpmd_script"
  done
fi

# Run with MPMD or serial
echo ""
if [[ "${USE_CFP:-}" == "YES" ]]; then
  OMP_NUM_THREADS=1 "${USHgfs}/run_mpmd.sh" "${DATA}/mpmd_script"
  export err=$?
else
  chmod 755 "${DATA}/mpmd_script"
  bash +x "${DATA}/mpmd_script" > mpmd.out 2>&1
  export err=$?
fi
err_chk

cat mpmd.out

# End of MWW3 prostprocessor script ---------------------------------------- #
