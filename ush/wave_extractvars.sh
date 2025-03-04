#! /usr/bin/env bash

################################################################################
## UNIX Script Documentation Block
## Script name:         wave_extractvars.sh
## Script description:  Extracts variables from wave products
##                      and saves these variables in arcdir
#######################
# Main body starts here
#######################

source "${USHgfs}/preamble.sh"
source "${USHgfs}/wave_domain_grid.sh"

process_grdID "${waveGRD}"
com_varname="COMIN_WAVE_GRID_${GRDREGION}_${GRDRES}"
com_dir=${!com_varname}

subdata=${1}

if [[ ! -d "${subdata}" ]]; then
   mkdir -p "${subdata}"
fi

for (( nh = FHOUT_WAV_EXTRACT; nh <= FHMAX_WAV; nh = nh + FHOUT_WAV_EXTRACT )); do
  fnh=$(printf "%3.3d" "${nh}")

  infile=${com_dir}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}.grib2
  new_infile=${subdata}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}_ext.grib2
  outfile=${subdata}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}.grib2
  rm -f "${outfile}" # Remove outfile if it already exists before extraction

  if [[ -f "${infile}" ]]; then # Check if input file exists before extraction
    if ! cpfs "${infile}" "${new_infile}"; then
      echo "FATAL ERROR: Failed to copy ${infile} to ${new_infile}."
      exit 1
    fi
    # shellcheck disable=SC2312
    ${WGRIB2} "${new_infile}" | grep -F -f "${varlist_wav}" | ${WGRIB2} -i "${new_infile}" -append -grib "${outfile}"
  else
    echo "WARNING: ${infile} does not exist in ${com_dir}."
  fi
  copy_to_comout "${outfile}" "${ARC_RFCST_PROD_WAV}"
done # nh

exit 0
