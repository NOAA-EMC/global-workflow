#! /usr/bin/env bash

###################################################################
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
###################################################################

source "${USHgfs}/preamble.sh"

cd "${DATA}" || exit 1
fhr3=$1

# "Import" functions used in this script
source "${USHgfs}/product_functions.sh"

for table in g2varswmo2.tbl g2vcrdwmo2.tbl g2varsncep1.tbl g2vcrdncep1.tbl; do
  cp "${HOMEgfs}/gempak/fix/${table}" "${table}" || \
    ( echo "FATAL ERROR: ${table} is missing" && exit 2 )
done

NAGRIB_TABLE="${HOMEgfs}/gempak/fix/nagrib.tbl"
NAGRIB="${GEMEXE}/nagrib2"

# shellcheck disable=SC2312
entry=$(grep "^${RUN2} " "${NAGRIB_TABLE}" | awk 'index($1,"#") != 1 {print $0}')

if [[ "${entry}" != "" ]] ; then
  cpyfil=$(echo "${entry}"  | awk 'BEGIN {FS="|"} {print $2}')
  garea=$(echo "${entry}"   | awk 'BEGIN {FS="|"} {print $3}')
  gbtbls=$(echo "${entry}"  | awk 'BEGIN {FS="|"} {print $4}')
  maxgrd=$(echo "${entry}"  | awk 'BEGIN {FS="|"} {print $5}')
  kxky=$(echo "${entry}"    | awk 'BEGIN {FS="|"} {print $6}')
  grdarea=$(echo "${entry}" | awk 'BEGIN {FS="|"} {print $7}')
  proj=$(echo "${entry}"    | awk 'BEGIN {FS="|"} {print $8}')
  output=$(echo "${entry}"  | awk 'BEGIN {FS="|"} {print $9}')
else
  cpyfil=gds
  garea=dset
  gbtbls=
  maxgrd=4999
  kxky=
  grdarea=
  proj=
  output=T
fi  
pdsext=no



GEMGRD="${RUN2}_${PDY}${cyc}f${fhr3}"
GRIBIN="${COM_ATMOS_GOES}/${model}.${cycle}.${GRIB}${fhr3}${EXT}"
GRIBIN_chk="${GRIBIN}"

if [[ ! -r "${GRIBIN_chk}" ]]; then
  echo "FATAL ERROR: GRIB index file ${GRIBIN_chk} not found!"
  export err=7 ; err_chk
  exit "${err}"
fi

cp "${GRIBIN}" "grib${fhr3}"

export pgm="nagrib_nc F${fhr3}"
startmsg

${NAGRIB} << EOF
GBFILE   = grib${fhr3}
INDXFL   = 
GDOUTF   = ${GEMGRD}
PROJ     = ${proj}
GRDAREA  = ${grdarea}
KXKY     = ${kxky}
MAXGRD   = ${maxgrd}
CPYFIL   = ${cpyfil}
GAREA    = ${garea}
OUTPUT   = ${output}
GBTBLS   = ${gbtbls}
GBDIAG   = 
PDSEXT   = ${pdsext}
l
r
EOF

export err=$?; err_chk

cpfs "${GEMGRD}" "${COM_ATMOS_GEMPAK_0p25}/${GEMGRD}"
if [[ ${SENDDBN} == "YES" ]] ; then
  "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
    "${COM_ATMOS_GEMPAK_0p25}/${GEMGRD}"
fi

"${GEMEXE}/gpend"

############################### END OF SCRIPT #######################
