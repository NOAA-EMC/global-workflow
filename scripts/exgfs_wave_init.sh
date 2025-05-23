#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         exwave_init.sh
# Script description:  Creates model definition files for WW3
#
# Abstract: This script is the init config for the global multi_grid wave model.
#           It creates model definition files with all configurations of spatial
#           and spectral grids, as well as physics parameters and time steps.
#
# $Id$
#
# Attributes:
#   Language: Bourne-again (BASH) shell
#
###############################################################################
#
# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most child scripts

cat << EOF

Preparing input files :
-----------------------
EOF

# 1.a Model definition files


# Eliminate duplicate grids
# Use an associative array, since they don't allow duplicate keys
declare -A grdALL
for grd in ${WAVECUR_FID} ${WAVEICE_FID} ${WAVEWND_FID} ${waveuoutpGRD} ${waveGRD} ${wavepostGRD} ${waveinterpGRD}; do
  # For ease of access, make the value the same as the key
  grdALL["${grd}"]="${grd}"
done

for grdID in "${grdALL[@]}"; do
  if [[ -f "${COMOUT_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" ]]; then
    echo "INFO: Mod def file for ${grdID} found in ${COMOUT_WAVE_PREP}. copying ...."
    cpreq "${COMOUT_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" "mod_def.${grdID}"

  else
    echo "INFO: Mod def file for ${grdID} not found in ${COMOUT_WAVE_PREP}. Setting up to generate ..."
    if [[ -f "${FIXgfs}/wave/ww3_grid.inp.${grdID}" ]]; then
      cpreq "${FIXgfs}/wave/ww3_grid.inp.${grdID}" "ww3_grid.inp.${grdID}"
      echo "INFO: ww3_grid.inp.${grdID} copied (${FIXgfs}/wave/ww3_grid.inp.${grdID})."
    else
      export err=2
      err_exit "No inp file for model definition file for grid ${grdID}"
    fi

    if [[ -f "${FIXgfs}/wave/${grdID}.msh" ]]; then
      cpreq "${FIXgfs}/wave/${grdID}.msh" "${grdID}.msh"
    fi
    #TODO: how do we say "it's unstructured, and therefore need to have error check here"

    echo "${USHgfs}/wave_grid_moddef.sh ${grdID}" >> mpmd_script
  fi
done

# 1.a.1 Execute MPMD or process serially
"${USHgfs}/run_mpmd.sh" "${DATA}/mpmd_script" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
  err_exit "run_mpmd.sh failed!"
fi

# 1.a.3 File check
for grdID in "${grdALL[@]}"; do
  if [[ -f "${COMOUT_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" ]]; then
    echo "INFO: mod_def.${grdID} succesfully created/copied"
  else
    export err=3
    err_exit "No model definition file for grid ${grdID}"
  fi
done

# Copy to other members if needed
if [[ "${NET}" == "gefs" && ${NMEM_ENS} -gt 0 ]]; then
  for mem in $(seq -f "%03g" 1 "${NMEM_ENS}"); do
    MEMDIR="mem${mem}" YMD="${PDY}" HH="${cyc}" declare_from_tmpl COMOUT_WAVE_PREP_MEM:COM_WAVE_PREP_TMPL
    mkdir -p "${COMOUT_WAVE_PREP_MEM}"
    for grdID in "${grdALL[@]}"; do
      cpfs "${COMOUT_WAVE_PREP}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin" "${COMOUT_WAVE_PREP_MEM}/${RUN}.wave.t${cyc}z.mod_def.${grdID}.bin"
    done
  done
fi

# End of MWW3 init config script ------------------------------------------- #
