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

[[ -d "${subdata}" ]] || mkdir -p "${subdata}"

for (( nh = FHOUT_WAV_EXTRACT; nh <= FHMAX_WAV; nh = nh + FHOUT_WAV_EXTRACT )); do
  fnh=$(printf "%3.3d" "${nh}")

  infile=${com_dir}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}.grib2
  new_infile=${subdata}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}_ext.grib2
  outfile=${subdata}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}.grib2
  rm -f "${outfile}" # Remove outfile if it already exists before extraction

  if [[ -f "${infile}" ]]; then # Check if input file exists before extraction
    cpfs "${infile}" "${new_infile}" # Copy infile to the subdata directory
    # shellcheck disable=SC2312
    ${WGRIB2} "${new_infile}" | grep -F -f "${varlist_wav}" | ${WGRIB2} -i "${new_infile}" -append -grib "${outfile}"
  elif [[ -f "${infile}" ]];
    echo "WARNING: ${infile} does not exist in ${com_dir}."
  elif [[ -f "${new_infile}" ]];
    echo "WARNING: ${new_infile} does not exist in ${subdata}. Copying skipped."
  else
    echo "WARNING: ${infile} and ${new_infile} do not exist."
  fi
  copy_to_comout "${outfile}" "${ARC_RFCST_PROD_WAV}"
done # nh

exit 0
