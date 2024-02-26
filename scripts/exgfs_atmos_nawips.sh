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

source "${HOMEgfs}/ush/preamble.sh" "${2}"

#### If EMC GFS PARA runs hourly file are not available, The ILPOST
#### will set to 3 hour in EMC GFS PARA.
#### Note:  ILPOST default set to 1
export ILPOST=${ILPOST:-1}

cd "${DATA}" || exit 1
RUN2=$1
fend=$2
DBN_ALERT_TYPE=$3
destination=$4

DATA_RUN="${DATA}/${RUN2}"
mkdir -p "${DATA_RUN}"
cd "${DATA_RUN}" || exit 1

# "Import" functions used in this script
source "${HOMEgfs}/ush/product_functions.sh"

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

maxtries=360
fhcnt=${fstart}
while (( fhcnt <= fend )) ; do

  if mkdir "lock.${fhcnt}" ; then
    cd "lock.${fhcnt}" || exit 1
    cp "${HOMEgfs}/gempak/fix/g2varswmo2.tbl" "g2varswmo2.tbl"
    cp "${HOMEgfs}/gempak/fix/g2vcrdwmo2.tbl" "g2vcrdwmo2.tbl"
    cp "${HOMEgfs}/gempak/fix/g2varsncep1.tbl" "g2varsncep1.tbl"
    cp "${HOMEgfs}/gempak/fix/g2vcrdncep1.tbl" "g2vcrdncep1.tbl"

    fhr=$(printf "%03d" "${fhcnt}")

    GEMGRD="${RUN2}_${PDY}${cyc}f${fhr}"

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

    case ${RUN2} in
      # TODO: Why aren't we interpolating from the 0p25 grids for 35-km and 40-km?
      'gfs_0p50' | 'gfs_0p25') res=${RUN2: -4};;
      *) res="1p00";;
    esac

    source_var="COM_ATMOS_GRIB_${res}"
    export GRIBIN="${!source_var}/${model}.${cycle}.pgrb2.${res}.f${fhr}"
    GRIBIN_chk="${!source_var}/${model}.${cycle}.pgrb2.${res}.f${fhr}.idx"

    icnt=1
    while (( icnt < 1000 )); do
      if [[ -r "${GRIBIN_chk}" ]] ; then
        # File available, wait 5 seconds then proceed
        sleep 5
        break
      else
        # File not available yet, wait 10 seconds and try again
        echo "The process is waiting ... ${GRIBIN_chk} file to proceed."
        sleep 10
        icnt=$((icnt+1))
      fi
      if (( icnt >= maxtries )); then
        echo "FATAL ERROR: after 1 hour of waiting for ${GRIBIN_chk} file at F${fhr} to end."
        export err=7 ; err_chk
        exit "${err}"
      fi
    done

    case "${RUN2}" in
      gfs35_pac)
        export gfs35_pac='latlon 130.0:416:0.312 75.125:186:-0.312'
        # shellcheck disable=SC2086,SC2248
        "${WGRIB2}" "${GRIBIN}" ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} ${opt27} ${opt28} -new_grid ${gfs35_pac} "grib${fhr}"
        trim_rh "grib${fhr}"
        ;;
      gfs35_atl)
        export gfs35_atl='latlon 230.0:480:0.312 75.125:242:-0.312'
        # shellcheck disable=SC2086,SC2248
        "${WGRIB2}" "${GRIBIN}" ${opt1} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} ${opt27} ${opt28} -new_grid ${gfs35_atl} "grib${fhr}"
        trim_rh "grib${fhr}"
        ;;
      gfs40)
        export gfs40='lambert:265.0:25.0:25.0 226.541:185:40635.0 12.19:129:40635.0'
        # shellcheck disable=SC2086,SC2248
        "${WGRIB2}" "${GRIBIN}" ${opt1uv} ${opt21} ${opt22} ${opt23} ${opt24} ${opt25} ${opt26} ${opt27} ${opt28} -new_grid ${gfs40} "grib${fhr}"
        trim_rh "grib${fhr}"
        ;;
     *)
        cp "${GRIBIN}" "grib${fhr}"
    esac

    export pgm="nagrib2 F${fhr}"
    startmsg

    ${NAGRIB} << EOF
GBFILE   = grib${fhr}
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
    if (( fhcnt <= 240 )) ; then
    	if (( fhcnt < 276 )) && [[ "${RUN2}" = "gfs_0p50" ]] ; then
    	    fhcnt=$((fhcnt+6))
    	else
    	    fhcnt=$((fhcnt+12))
    	fi
    elif ((fhcnt < 120)) && [[ "${RUN2}" = "gfs_0p25" ]] ; then
      ####    let fhcnt=fhcnt+1
    	fhcnt=$((hcnt + ILPOST))
    else
      fhcnt=$((ILPOST > finc ? fhcnt+ILPOST : fhcnt+finc ))
    fi
  fi
done

"${GEMEXE}/gpend"
#####################################################################


############################### END OF SCRIPT #######################
