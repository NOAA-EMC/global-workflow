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

#### If EMC GFS PARA runs hourly file are not available, The ILPOST
#### will set to 3 hour in EMC GFS PARA.
#### Note:  ILPOST default set to 1
export ILPOST=${ILPOST:-1}

cd "${DATA}" || exit 1
grid=$1
fend=$2
DBN_ALERT_TYPE=$3
destination=$4

DATA_RUN="${DATA}/${grid}"
mkdir -p "${DATA_RUN}"
cd "${DATA_RUN}" || exit 1

# "Import" functions used in this script
source "${USHgfs}/product_functions.sh"

#
NAGRIB="${GEMEXE}/nagrib2"
#

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
max_tries=360
fhr=$(( 10#${fstart} ))
while (( fhr <= 10#${fend} )) ; do

  fhr3=$(printf "%03d" "${fhr}")

  if mkdir "lock.${fhr3}" ; then
    cd "lock.${fhr3}" || exit 1

    for table in g2varswmo2.tbl g2vcrdwmo2.tbl g2varsncep1.tbl g2vcrdncep1.tbl; do
      cp "${HOMEgfs}/gempak/fix/${table}" "${table}" || \
        ( echo "FATAL ERROR: ${table} is missing" && exit 2 )
    done

    GEMGRD="${RUN}_${grid}_${PDY}${cyc}f${fhr3}"

    # Set type of Interpolation for WGRIB2
    export opt1=' -set_grib_type same -new_grid_winds earth '
    export opt1uv=' -set_grib_type same -new_grid_winds grid '
    export opt21=' -new_grid_interpolation bilinear -if '
    export opt22=":(CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
    export opt23=' -new_grid_interpolation neighbor -fi '
    export opt24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
    export opt25=":(APCP|ACPCP|PRATE|CPRAT):"
    export opt26=' -set_grib_max_bits 25 -fi -if '
    export opt27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
    export opt28=' -new_grid_interpolation budget -fi '

    case ${grid} in
      # TODO: Why aren't we interpolating from the 0p25 grids for 35-km and 40-km?
      '0p50' | '0p25') grid_in=${grid};;
      *) grid_in="1p00";;
    esac

    source_var="COM_ATMOS_GRIB_${grid_in}"
    export GRIBIN="${!source_var}/${model}.${cycle}.pgrb2.${grid_in}.f${fhr3}"
    GRIBIN_chk="${!source_var}/${model}.${cycle}.pgrb2.${grid_in}.f${fhr3}.idx"

    if ! wait_for_file "${GRIBIN_chk}" "${sleep_interval}" "${max_tries}"; then
      echo "FATAL ERROR: after 1 hour of waiting for ${GRIBIN_chk} file at F${fhr3} to end."
      export err=7 ; err_chk
      exit "${err}"
    fi

    case "${grid}" in
      35km_pac) grid_spec='latlon 130.0:416:0.312 75.125:186:-0.312';;
      35km_atl) grid_spec='latlon 230.0:480:0.312 75.125:242:-0.312';;
      40km)     grid_spec='lambert:265.0:25.0:25.0 226.541:185:40635.0 12.19:129:40635.0';;
      *)        grid_spec='';;
    esac

    if [[ "${grid_spec}" != "" ]]; then
      # shellcheck disable=SC2086,SC2248
      "${WGRIB2}" "${GRIBIN}" ${opt1uv} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} ${opt27} ${opt28} -new_grid ${grid_spec} "grib${fhr3}"
      trim_rh "grib${fhr3}"
    else
      cp "${GRIBIN}" "grib${fhr3}"
    fi

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
    export err=$?;err_chk

    cpfs "${GEMGRD}" "${destination}/${GEMGRD}"
    if [[ ${SENDDBN} == "YES" ]] ; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
           "${destination}/${GEMGRD}"
    fi
    cd "${DATA_RUN}" || exit 1
  else
    if (( fhr >= 240 )) ; then
      if (( fhr < 276 )) && [[ "${grid}" = "0p50" ]] ; then
          fhr=$((fhr+6))
      else
          fhr=$((fhr+12))
      fi
    elif ((fhr < 120)) && [[ "${grid}" = "0p25" ]] ; then
      fhr=$((fhr + ILPOST))
    else
      fhr=$((ILPOST > finc ? fhr+ILPOST : fhr+finc ))
    fi
  fi
done

"${GEMEXE}/gpend"
#####################################################################


############################### END OF SCRIPT #######################
