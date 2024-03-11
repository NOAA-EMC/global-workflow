#! /usr/bin/env bash

###################################################################
# echo "----------------------------------------------------"
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
# echo "----------------------------------------------------"
# echo "History: Mar 2000 - First implementation of this new script."
# echo "S Lilly: May 2008 - add logic to make sure that all of the "
# echo "                    data produced from the restricted ECMWF"
# echo "                    data on the CCS is properly protected."
# echo "C. Magee: 10/2013 - swap X and Y for rtgssthr Atl and Pac."
#####################################################################

source "${USHgfs}/preamble.sh"

cd "${DATA}" || exit 2

for table in g2varswmo2.tbl g2vcrdwmo2.tbl g2varsncep1.tbl g2vcrdncep1.tbl; do
  cp "${HOMEgfs}/gempak/fix/${table}" "${table}" || \
    ( echo "FATAL ERROR: ${table} is missing" && exit 2 )
done

#
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

sleep_interval=20
max_tries=180
fhr=${fstart}
for (( fhr=fstart; fhr <= fend; fhr=fhr+finc )); do
  fhr3=$(printf "%03d" "${fhr}")
  GRIBIN="${COM_ATMOS_GOES}/${model}.${cycle}.${GRIB}${fhr3}${EXT}"
  GEMGRD="${RUN2}_${PDY}${cyc}f${fhr3}"

  GRIBIN_chk="${GRIBIN}"

  if ! wait_for_file "${GRIBIN_chk}" "${sleep_interval}" "${max_tries}"; then
    echo "FATAL ERROR: after 1 hour of waiting for ${GRIBIN_chk} file at F${fhr3} to end."
    export err=7 ; err_chk
    exit "${err}"
  fi

  cp "${GRIBIN}" "grib${fhr3}"

  export pgm="nagrib_nc F${fhr3}"

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
  export err=$?;err_chk

  "${GEMEXE}/gpend"

  cpfs "${GEMGRD}" "${COM_ATMOS_GEMPAK_0p25}/${GEMGRD}"
  if [[ ${SENDDBN} == "YES" ]] ; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
			     "${COM_ATMOS_GEMPAK_0p25}/${GEMGRD}"
  else
      echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
  fi

done

#####################################################################


############################### END OF SCRIPT #######################
