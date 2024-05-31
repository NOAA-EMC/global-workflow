#! /usr/bin/env bash
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

ensname=${1}
outdirpre=${2}
varlist=${3}
dataformat=${4}
datares=${5}
datacompress=${6}
cd ${outdirpre}

nh=6
while [[ $nh -le ${FHMAX} ]];do
  fnh=`printf "%3.3d" ${nh}`
  VDATE=$($NDATE +${nh} ${PDY}${cyc})
  yyyyv=$(echo  $VDATE | cut -c1-4)
  mmv=$(echo $VDATE | cut -c5-6)
  ddv=$(echo $VDATE | cut -c7-8)
  hhv=$(echo $VDATE | cut -c9-10)
  echo "extracting f${fnh} valid at ${VDATE} for ${ensname}"

  if [[ "${dataformat}" == "grib2" ]];then
    if [[ ${component_name} == "ocn" ]];then
      infile=${COM_OCEAN_GRIB}/${datares}/gefs.ocean.${cycle}.${datares}.f${fnh}.grib2
    fi
    if [[ ${component_name} == "ice" ]];then
      infile=${COM_ICE_GRIB}/${datares}/gefs.ice.${cycle}.${datares}.f${fnh}.grib2
    fi                                                                                                                                                                                                                                   
    oufile1=$outdirpre/gefs.${component_name}.${cycle}.${datares}.f${fnh}.rfcst.grib2
  fi

  if [[ "${dataformat}" == "netcdf" ]];then
    if [[ ${component_name} == "ocn" ]];then
      infile=${COM_OCEAN_NETCDF}/${datares}/gefs.ocean.${cycle}.${datares}.f${fnh}.nc
    fi
    if [[ ${component_name} == "ice" ]];then
      infile=${COM_ICE_NETCDF}/${datares}/gefs.ice.${cycle}.${datares}.f${fnh}.nc
    fi   
    oufile1=$outdirpre/gefs.${component_name}.${cycle}.${datares}.f${fnh}.rfcst.grib2
  fi

  if [ -f $infile ]; then #check if input file exists before extraction
    $WGRIB2 $infile | grep -F -f ${varlist} | $WGRIB2 -i $infile -append -grib $oufile1>/dev/null 
    if [[ ${datacompress} -eq 1 ]];then
      echo "Compressing ${oufile1}"
      bzip2 $oufile1
    fi 
  else
    echo "WARNING: ${infile} does not exist."
  fi

  nh=$((${nh} + 6))
done

exit 0                                                                                                                                                                                        
