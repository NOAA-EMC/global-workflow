#! /usr/bin/env bash                                                                                                                                                                          
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

DATA=${1}
cd "${DATA}" || true

nh=6
while [[ ${nh} -le ${FHMAX_WAV} ]];do
  fnh=$(printf "%3.3d" "${nh}")

  infile=${COMIN_WAVE_GRID}/gefswave.${cycle}.global.${wavres}.f${fnh}.grib2
  outfile=${DATA}/gefswave.${cycle}.global.${wavres}.f${fnh}.grib2
  rm -f "${outfile}" #remove outfile if it already exists before extraction

  if [[ -f "${infile}" ]]; then #check if input file exists before extraction
    # shellcheck disable=SC2312 
    ${WGRIB2} "${infile}" | grep -F -f "${varlist_wav}" | ${WGRIB2} -i "${infile}" -append -grib "${outfile}">/dev/null
  else
    echo "WARNING: ${infile} does not exist."
  fi 

    nh=$(( nh + FHOUT_WAV_NOSCRUB ))

done #fhr

exit 0                                                                                                                                                                                        
