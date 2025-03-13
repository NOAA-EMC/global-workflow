#! /usr/bin/env bash

################################################################################
## UNIX Script Documentation Block
## Script name:         ocnice_extractvars.sh
## Script description:  Extracts and optionally compresses variables
##                      from ocean and ice products
##                      and saves these variables in arcdir
#######################
# Main body starts here
#######################

source "${USHgfs}/preamble.sh"

subdata=${1}
varlist=${2}
datares=${3}
datacompress=${4}
fhout_ocnice=${5}
comout_rfcst_prod_ocnice=${6}

if [[ ! -d "${subdata}" ]]; then
   mkdir -p "${subdata}"
fi

for (( nh = FHMIN_GFS + fhout_ocnice; nh <= FHMAX_GFS; nh = nh + fhout_ocnice )); do
  fnh=$(printf "%3.3d" "${nh}")

  if [[ ${component_name} == "ocn" ]]; then
    if [[ "${datares}" == "native" ]]; then
      com_dir=${COMIN_OCEAN_HISTORY}
      infile="${com_dir}/${RUN}.ocean.t${cyc}z.${fhout_ocnice}hr_avg.f${fnh}.nc"
      new_infile=${subdata}/${RUN}.ocean.t${cyc}z.${fhout_ocnice}hr_avg.f${fnh}_ext.nc
    else
      com_dir=${COMIN_OCEAN_NETCDF}
      infile="${com_dir}/${datares}/${RUN}.ocean.t${cyc}z.${datares}.f${fnh}.nc"
      new_infile=${subdata}/${RUN}.ocean.t${cyc}z.${datares}.f${fnh}_ext.nc
    fi
    # For ocean products, add an argument to extract a subset of levels
    otherargs=(-d "${depthvar_name},""${zmin},""${zmax}")
  elif [[ ${component_name} == "ice" ]]; then
    if [[ "${datares}" == "native" ]]; then
      com_dir=${COMIN_ICE_HISTORY}
      infile="${com_dir}/${RUN}.ice.t${cyc}z.${fhout_ocnice}hr_avg.f${fnh}.nc"
      new_infile=${subdata}/${RUN}.ice.t${cyc}z.${fhout_ocnice}hr_avg.f${fnh}_ext.nc
    else
      com_dir=${COMIN_ICE_NETCDF}
      infile="${com_dir}/${datares}/${RUN}.ice.t${cyc}z.${datares}.f${fnh}.nc"
      new_infile=${subdata}/${RUN}.ice.t${cyc}z.${datares}.f${fnh}_ext.nc
    fi
    otherargs=()
  fi
  outfile=${subdata}/${RUN}.${component_name}.t${cyc}z.${datares}.f${fnh}.nc

  if [[ -f "${infile}" ]]; then #check if input file exists before extraction
    if ! cpfs "${infile}" "${new_infile}"; then
      echo "FATAL ERROR: Failed to copy ${infile} to ${new_infile}."
      exit 1
    fi
    varsrequested=$(paste -s "${varlist}")
    varsinfile=$(cdo -showname "${new_infile}")
    varsavailable=""
    for i in ${varsrequested}; do
      # Check if variable from parm file is available in netcdf file. If variable is not in netcdf file, do not try to extract that variable.
      if [[ ${varsinfile} == *"${i}"* ]]; then
        varsavailable+="${i},"
      else
        echo "WARNING: ${i} is not available in ${new_infile}."
      fi
    done
    if [[ -z "${varsavailable}" ]]; then
      echo "WARNING: No variables from parm file ${varlist} are available in netcdf file ${new_infile}."
    else
      ocnice_vars=${varsavailable::-1}
      ncks -v "${ocnice_vars}" "${otherargs[@]}" "${new_infile}" "${outfile}"
    fi
    if [[ ${datacompress} -eq 1 ]]; then
      ${COMPRSCMD} "${outfile}"
      copy_to_comout "${outfile}.bz2" "${comout_rfcst_prod_ocnice}"
    else
      copy_to_comout "${outfile}" "${comout_rfcst_prod_ocnice}"
    fi
  else
    echo "WARNING: ${infile} does not exist in ${com_dir}."
  fi
done # nh

exit 0
