#! /usr/bin/env bash

###################################################################
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
###################################################################

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
  source_table="${HOMEgfs}/gempak/fix/${table}"
  if [[ ! -f "${source_table}" ]]; then
    err_exit "${table} is missing"
  fi
  cpreq "${source_table}" "${table}"
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
source_dirvar="COMOUT_ATMOS_GRIB_${grid}"
export GRIBIN="${!source_dirvar}/${model}.${cycle}.pgrb2.${grid}.f${fhr3}"
GRIBIN_chk="${GRIBIN}.idx"

if [[ ! -r "${GRIBIN_chk}" ]]; then
  err_exit "GRIB index file ${GRIBIN_chk} not found!"
fi

cpreq "${GRIBIN}" "grib${fhr3}"

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

export err=$?
if [[ ${err} -ne 0 ]]; then
  err_exit "${NAGRIB} failed to create ${GEMGRD}!"
fi

cpfs "${GEMGRD}" "${destination}/${GEMGRD}"
if [[ ${SENDDBN} = "YES" ]] ; then
  "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
    "${destination}/${GEMGRD}"
fi

"${GEMEXE}/gpend"

############################### END OF SCRIPT #######################
