#! /usr/bin/env bash                                                                                                                                                                          
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

DATA=${1}

[[ -d "${DATA}" ]] || mkdir -p "${DATA}"

for (( nh = FHOUT_WAV_NOSCRUB; nh <= FHMAX_WAV; nh = nh + FHOUT_WAV_NOSCRUB )); do
  fnh=$(printf "%3.3d" "${nh}")

  infile=${COMIN_WAVE_GRID}/${RUN}wave.${cycle}.global.${wavres}.f${fnh}.grib2
  outfile=${DATA}/${RUN}wave.${cycle}.global.${wavres}.f${fnh}.grib2
  rm -f "${outfile}" #remove outfile if it already exists before extraction

  if [[ -f "${infile}" ]]; then #check if input file exists before extraction
    # shellcheck disable=SC2312 
    ${WGRIB2} "${infile}" | grep -F -f "${varlist_wav}" | ${WGRIB2} -i "${infile}" -append -grib "${outfile}">/dev/null
  else
    echo "WARNING: ${infile} does not exist."
  fi 
  copy_to_comout "${outfile}" "${COMOUT_RFCST_PROD_WAV}"
done #fhr

exit 0                                                                                                                                                                                        
