#! /usr/bin/env bash

###################################################################
# echo "----------------------------------------------------"
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
# echo "----------------------------------------------------"
# echo "History: Mar 2000 - First implementation of this new script."
# echo "S Lilly: May 2008 - add logic to make sure that all of the "
# echo "                    data produced from the restricted ECMWF"
# echo "                    data on the CCS is properly protected."
#####################################################################

source "${USHgfs}/preamble.sh" "${2}"

cd "${DATA}" || exit 1
grid=$1
fend=$2
DBN_ALERT_TYPE=$3
destination=$4

DATA_RUN="${DATA}/${grid}"
mkdir -p "${DATA_RUN}"
cd "${DATA_RUN}" || exit 1

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

sleep_interval=10
max_tries=180

fhr=$(( 10#${fstart} ))
while (( fhr <= 10#${fend} )); do
  fhr3=$(printf "%03d" "${fhr}")

  source_dirvar="COM_ATMOS_GRIB_${grid}"
  GEMGRD="${RUN}_${grid}_${PDY}${cyc}f${fhr3}"
  export GRIBIN="${!source_dirvar}/${model}.${cycle}.pgrb2.${grid}.f${fhr3}"
  GRIBIN_chk="${GRIBIN}.idx"

  if ! wait_for_file "${GRIBIN_chk}" "${sleep_interval}" "${max_tries}"; then
    echo "FATAL ERROR: after 1 hour of waiting for ${GRIBIN_chk} file at F${fhr3} to end."
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

  cp "${GEMGRD}" "${destination}/${GEMGRD}"
  export err=$?
  if (( err != 0 )) ; then
      echo "FATAL ERROR: ${GEMGRD} does not exist."
      exit "${err}"
  fi

  if [[ ${SENDDBN} = "YES" ]] ; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
				 "${destination}/${GEMGRD}"
  else
      echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
  fi

  if (( fhr >= 240 )) ; then
    fhr=$((fhr+12))
  else
    fhr=$((fhr+finc))
  fi
done

"${GEMEXE}/gpend"
#####################################################################


############################### END OF SCRIPT #######################
