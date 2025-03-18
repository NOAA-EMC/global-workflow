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
  # if ensemble; waveMEMB var empty in deterministic
export WAV_MOD_TAG="${RUN}.wave"

cd "${DATA}" || exit 99

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
if [[ -z "${NTASKS}" ]]; then
  echo "FATAL ERROR: requires NTASKS to be set "
  err=1; export err; ${errchk}
  exit "${err}"
fi

# 0.c Defining model grids

# 0.c.1 Grids

export waveGRD=${waveGRD?Var waveGRD Not Set}

# 0.c.2 extended global grid and rtma transfer grid
export waveinterpGRD=${waveinterpGRD?Var wavepostGRD Not Set}
export wavepostGRD=${wavepostGRD?Var wavepostGRD Not Set}


echo ' '
echo 'Grid information  :'
echo '-------------------'
echo "   Native wave grids  : $waveGRD"
echo "   Interpolated grids : $waveinterpGRD"
echo "   Post-process grids : $wavepostGRD"
echo ' '

export FHRUN=0

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

export DOGRB_WAV=${DOGRB_WAV:-'YES'} #Create grib2 files
export DOGRI_WAV=${DOGRI_WAV:-'NO'} #Create interpolated grids

exit_code=0

echo ' '
echo 'Preparing input files :'
echo '-----------------------'

# 1.a Model definition files and output files (set up using poe)

# 1.a.1 Copy model definition files
for grdID in ${waveGRD} ${wavepostGRD} ${waveinterpGRD}; do
  if [[ -f "${COMIN_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" ]]; then
    echo " Mod def file for ${grdID} found in ${COMIN_WAVE_PREP}. copying ...."
    cp -f "${COMIN_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" "mod_def.${grdID}"
  fi
done

# 1.a.2 Check that model definition files exist
for grdID in ${waveGRD} ${wavepostGRD} ${waveinterpGRD}; do
  if [[ ! -f "mod_def.${grdID}" ]]; then
    echo ' '
    echo '*************************************************** '
    echo " FATAL ERROR : NO MOD_DEF FILE mod_def.${grdID}"
    echo '*************************************************** '
    echo ' '
    err=2; export err; ${errchk}
    exit "${err}"
    # DOGRB_WAV='NO'  TODO: check if this is needed, if script errors out on the above line, this line is never executed
  else
    echo "File mod_def.${grdID} found. Syncing to all nodes ..."
  fi
done


# 1.b Input template files

if [[ "${DOGRI_WAV}" == "YES" ]]; then
  for intGRD in ${waveinterpGRD}; do
    if [[ -f ${PARMgfs}/wave/${intGRD}_interp.inp.tmpl ]]; then
      cp -f ${PARMgfs}/wave/${intGRD}_interp.inp.tmpl ${intGRD}_interp.inp.tmpl
    fi

    if [[ -f "${intGRD}_interp.inp.tmpl" ]]; then
      echo "   ${intGRD}_interp.inp.tmpl copied. Syncing to all nodes ..."
    else
      echo ' '
      echo '*********************************************** '
      echo '*** ERROR : NO TEMPLATE FOR GRINT INPUT FILE *** '
      echo '*********************************************** '
      echo ' '
      echo "${WAV_MOD_TAG} post ${PDY} ${cycle} : GRINT template file missing."
      exit_code=1
      DOGRI_WAV='NO'
    fi
  done
fi

if [[ "${DOGRB_WAV}" = 'YES' ]]; then
  for grbGRD in ${waveinterpGRD} ${wavepostGRD}; do
    if [[ -f "${PARMgfs}/wave/ww3_grib2.${grbGRD}.inp.tmpl" ]]; then
      cp -f "${PARMgfs}/wave/ww3_grib2.${grbGRD}.inp.tmpl" "ww3_grib2.${grbGRD}.inp.tmpl"
    fi

    if [[ -f "ww3_grib2.${grbGRD}.inp.tmpl" ]]; then
      echo "   ww3_grib2.${grbGRD}.inp.tmpl copied. Syncing to all nodes ..."
    else
      echo ' '
      echo '*********************************************** '
      echo "*** ERROR : NO TEMPLATE FOR ${grbGRD} GRIB INPUT FILE *** "
      echo '*********************************************** '
      echo ' '
      exit_code=2
      DOGRB_WAV='NO'
    fi
  done
fi


# 1.c Data summary

echo ' '
echo "   Input files read and processed at : $(date)"
echo ' '
echo '   Data summary : '
echo '   ---------------------------------------------'
echo "      Sufficient data for GRID interpolation    : $DOGRI_WAV"
echo "      Sufficient data for GRIB files            : $DOGRB_WAV"
echo ' '

# --------------------------------------------------------------------------- #
# 2.  Make consolidated grib2 file for side-by-side grids and interpolate
#     onto extended grids
#
# 2.a Command file set-up

echo '   Making command file for sbs grib2 and GRID Interpolation '
fhr=$(( 10#${FHR3} ))
FH3=$(printf %03i ${fhr})
YMDHMS=$(date -u -d "${PDY} ${cyc} + ${fhr} hours" "+%Y%m%d%H%0000")

rm -rf "output_${YMDHMS}"
mkdir -p "output_${YMDHMS}"
cd "output_${YMDHMS}"
fcmdnow="cmdfile.${FH3}"
fcmdigrd="icmdfile.${FH3}"
rm -f "${fcmdnow}" "${fcmdigrd}"
touch "${fcmdnow}" "${fcmdigrd}"

# Create instances of directories for gridded output
export GRIBDATA="${DATA}/output_${YMDHMS}"
export GRDIDATA="${DATA}/output_${YMDHMS}"

# Gridded data (main part, need to be run side-by-side with forecast
gfile="${COMIN_WAVE_HISTORY}/${RUN}.wave.t${cyc}z.${waveGRD}.f${FH3}.bin"
if [[ ! -s "${gfile}" ]]; then
  echo " FATAL ERROR : NO RAW FIELD OUTPUT FILE ${gfile}"
  err=3; export err; "${errchk}"
  exit "${err}"
fi
${NLN} "${gfile}" "./out_grd.${waveGRD}"

if [[ "$DOGRI_WAV" = 'YES' ]]; then
  dt_int=3600.
  n_int=9999 ;
  nigrd=1
  for grdID in ${waveinterpGRD}; do
    ymdh_int=$(date -u -d "${YMDHMS:0:10} - ${WAVHINDH} hours" "+%Y%m%d%H")
    echo "${USHgfs}/wave_grid_interp_sbs.sh ${grdID} ${ymdh_int} ${dt_int} ${n_int} > grint_${grdID}.out 2>&1" >> "${fcmdigrd}.${nigrd}"
    if [[ "$DOGRB_WAV" = 'YES' ]]; then
      gribFL=\'$(echo ${OUTPARS_WAV})\'
      process_grdID "${grdID}"
      echo "${USHgfs}/wave_grib2_sbs.sh ${grdID} ${GRIDNR} ${MODNR} ${ymdh} ${fhr} ${GRDREGION} ${GRDRES} ${gribFL} > grib_${grdID}.out 2>&1" >> "${fcmdigrd}.${nigrd}"
    fi
    echo "${GRIBDATA}/${fcmdigrd}.${nigrd}" >> "${fcmdnow}"
    chmod 744 "${fcmdigrd}.${nigrd}"
    nigrd=$((nigrd+1))
  done
fi

if [[ "${DOGRB_WAV}" = 'YES' ]]; then
  for grdID in ${wavepostGRD}; do # First concatenate grib files for sbs grids
    gribFL=\'$(echo ${OUTPARS_WAV})\'
    process_grdID "${grdID}"
    echo "${USHgfs}/wave_grib2_sbs.sh ${grdID} ${GRIDNR} ${MODNR} ${ymdh} ${fhr} ${GRDREGION} ${GRDRES} ${gribFL} > grib_${grdID}.out 2>&1" >> "${fcmdnow}"
  done
fi

if [[ ${USE_CFP:-"NO"} = "YES" ]]; then
  nfile=0
  iline=1
  ifirst='yes'
  nlines=$( wc -l "${fcmdnow}" | awk '{print $1}' )
  while [[ ${iline} -le ${nlines} ]]; do
    line=$( sed -n ''${iline}'p' "${fcmdnow}" )
    if [[ -z "${line}" ]]; then
      break
    else
      if [[ "${ifirst}" = 'yes' ]]; then
        echo "#!/bin/sh" > cmdmfile.${nfile}
        echo "${nfile} cmdmfile.${nfile}" >> cmdmprog
        chmod 744 "cmdmfile.${nfile}"
      fi
      echo "${line}" >> "cmdmfile.${nfile}"
      nfile=$(( nfile + 1 ))
      if [[ "${nfile}" -eq "${NTASKS}" ]]; then
        nfile=0
        ifirst='no'
      fi
      iline=$(( iline + 1 ))
    fi
  done
fi

wavenproc=$(wc -l ${fcmdnow} | awk '{print $1}')
wavenproc=$(echo $((${wavenproc}<${NTASKS}?${wavenproc}:${NTASKS})))

echo ' '
echo "   Executing the grib2_sbs scripts at : $(date)"
echo '   ------------------------------------'
echo ' '

if [[ "$wavenproc" -gt '1' ]]; then
  if [[ ${USE_CFP:-"NO"} = "YES" ]]; then
    ${wavempexec} -n ${wavenproc} ${wave_mpmd} cmdmprog
  else
    ${wavempexec} ${wavenproc} ${wave_mpmd} ${fcmdnow}
  fi
  rc=$?
else
  chmod 744 ${fcmdnow}
  ./${fcmdnow}
  rc=$?
fi

if [[ "${rc}" != '0' ]]; then
  echo ' '
  echo '*************************************'
  echo '*** FATAL ERROR: CMDFILE FAILED   ***'
  echo '*************************************'
  echo '     See Details Below '
  echo ' '
  err=4; export err; ${errchk}
  exit "${err}"
fi

rm -f out_grd.* # Remove large binary grid output files

cd "${DATA}" || exit 99

# Check if grib2 file created
com_varname="COMOUT_WAVE_GRID_${GRDREGION}_${GRDRES}"
com_dir=${!com_varname}
gribchk="${RUN}.wave.t${cyc}z.${GRDREGION}.${GRDRES}.f${FH3}.grib2"
if [[ ! -s "${com_dir}/${gribchk}" ]]; then
  echo ' '
  echo '********************************************'
  echo "*** FATAL ERROR: ${gribchk} not generated "
  echo '********************************************'
  echo '     See Details Below '  # FIXME: what details?
  echo ' '
  err=5; export err; ${errchk}
  exit "${err}"
fi

# --------------------------------------------------------------------------- #
# 7.  Ending output

echo "${exit_code}"

# End of MWW3 prostprocessor script ---------------------------------------- #
