#! /usr/bin/env bash

###################################################################
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
###################################################################

source "${USHgfs}/preamble.sh" "${2}"

cd "${DATA}" || exit 1
grid=$1
fhr3=$2
DBN_ALERT_TYPE=$3
destination=$4

DATA_RUN="${DATA}/${grid}"
mkdir -p "${DATA_RUN}"
cd "${DATA_RUN}" || exit 1

# "Import" functions used in this script
source "${USHgfs}/product_functions.sh"

for table in g2varswmo2.tbl g2vcrdwmo2.tbl g2varsncep1.tbl g2vcrdncep1.tbl; do
  cp "${HOMEgfs}/gempak/fix/${table}" "${table}" || \
    ( echo "FATAL ERROR: ${table} is missing" && exit 2 )
done

NAGRIB="${GEMEXE}/nagrib2"

cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no



GEMGRD="${RUN}_${grid}_${PDY}${cyc}f${fhr3}"
source_dirvar="COM_ATMOS_GRIB_${grid}"
export GRIBIN="${!source_dirvar}/${model}.${cycle}.pgrb2.${grid}.f${fhr3}"
GRIBIN_chk="${GRIBIN}.idx"

if [[ ! -r "${GRIBIN_chk}" ]]; then
  echo "FATAL ERROR: GRIB index file ${GRIBIN_chk} not found!"
  export err=7 ; err_chk
  exit "${err}"
fi

cp "${GRIBIN}" "grib${fhr3}"

export pgm="nagrib2 F${fhr3}"
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

cpfs "${GEMGRD}" "${destination}/${GEMGRD}"
if [[ ${SENDDBN} = "YES" ]] ; then
  "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
    "${destination}/${GEMGRD}"
fi

"${GEMEXE}/gpend"

############################### END OF SCRIPT #######################
