#! /usr/bin/env bash

source "${HOMEgfs:?}/ush/preamble.sh" "$1"

rflux=$2
fcsty=$3
fflux=$4
gflux=$5
f=$6

WGRIB2=${WGRIB2:?}
CNVGRIB=${CNVGRIB:?}

${WGRIB2} "${rflux}" | grep "TMP:1 hybrid"     | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "SPFH:1 hybrid"    | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "UGRD:1 hybrid"    | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "VGRD:1 hybrid"    | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "HGT:1 hybrid"     | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "PRES:surface"     | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "PRATE:surface"    | grep ave  | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "VEG:surface"      | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "SFCR:surface"     | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "SFEXC:surface"    | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "TMP:surface"      | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "WEASD:surface"    | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "SNOD:surface"     | grep "${fcsty}" | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"

${WGRIB2} "${rflux}" | grep "DSWRF:surface:${f} hour fcst"  | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "DLWRF:surface:${f} hour fcst"  | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"
${WGRIB2} "${rflux}" | grep "USWRF:surface:${f} hour fcst"  | ${WGRIB2} -i "${rflux}" -append -grib "${fflux}"

${CNVGRIB} -g21 "${fflux}" "${gflux}"

exit $?
