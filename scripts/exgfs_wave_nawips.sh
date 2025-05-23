#! /usr/bin/env bash

###################################################################
# echo "----------------------------------------------------"
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
# echo "----------------------------------------------------"
# echo "History: Mar 2000 - First implementation of this new script."
# echo "Sept 2011 - First implementation of this new script based on"
# echo "               /nwprod/scripts/exnawips.sh.sms"
# echo " March 2020- Modified for GEFSv12.0"
#  March-2020 Roberto.Padilla@noaa.gov
#####################################################################

source "${USHgfs}/wave_domain_grid.sh"
source "${USHgfs}/atparse.bash"

NAGRIB="nagrib2"
fhr3=$(printf "%03d" "${FORECAST_HOUR}")

cpreq "${HOMEgfs}/gempak/fix/g2varswmo2.tbl" "${DATA}/"

grids=${GEMPAK_GRIDS:-${waveinterpGRD:-'aoc_9km gnh_10m gsh_15m'}} 

# Create a template for the GEMPAK control file
rm -f "${DATA}/gempak.parm.tmpl"
cat << EOF > "${DATA}/gempak.parm.tmpl"
GBFILE   = @[GBFILE]
INDXFL   =
GDOUTF   = @[GEMGRD]
PROJ     =
GRDAREA  =
KXKY     =
MAXGRD   = 4999
CPYFIL   = gds
GAREA    = dset
OUTPUT   = T
GBTBLS   = g2varswmo2.tbl
G2TBLS   =
GBDIAG   =
PDSEXT   = no
l
r
EOF

# Loop over the grids
for grid in ${grids}; do
  case ${grid} in
    aoc_9km)
      grdIDout='gfswavearc'
    ;;
    at_10m)
      grdIDout='gfswaveat10m'
    ;;
    ep_10m)
      grdIDout='gfswaveep10m'
    ;;
    wc_10m)
      grdIDout='gfswavewc10m'
    ;;
    glo_30m)
      grdIDout='gfswavegl30m'
    ;;
    gnh_10m)
      grdIDout='gfswavenh'
    ;;
    gsh_15m)
      grdIDout='gfswavesh'
    ;;
    glo_200)
      grdIDout='gfswaves200k'
    ;;
    *)
      echo "FATAL ERROR: Unspecified grid '${grid}'"
      exit 9
    ;;
  esac
  process_grdID "${grid}"

  com_varname="COMIN_WAVE_GRID_${GRDREGION}_${GRDRES}"
  com_dir=${!com_varname}
  GRIBIN="${RUN}.wave.${cycle}.${GRDREGION}.${GRDRES}.f${fhr3}.grib2"
  cpreq "${com_dir}/${GRIBIN}" "./${GRIBIN}"

  nagrib_file="${GRIBIN}"
  if [[ "${GRDREGION}.${GRDRES}" = "global.0p25" ]]; then
    nagrib_file="${RUN}.wave.${cycle}.global.${gridIDout}.${fhr3}.grib2"
    ${WGRIB2} -lola 0:720:0.5 -90:361:0.5 "${nagrib_file}" grib "${GRIBIN}"
    export err=$?
    if [[ ${err} -ne 0 ]]; then
      export pgm="${WGRIB2}"
      err_exit "wgrib2 failed to interpolate"
    fi
  fi

  GEMGRD="${grdIDout}_${PDY}${cyc}f${fhr3}"
  GBFILE="grib_${grid}"

  cpreq "${nagrib_file}" "${GBFILE}"

  rm -f "gempak.parm.${grid}"
  atparse < "${DATA}/gempak.parm.tmpl" >> "${DATA}/gempak.parm.${grid}"
  cat "${DATA}/gempak.parm.${grid}"

  startmsg
  export pgm="${NAGRIB}"
  ${pgm} < "${DATA}/gempak.parm.${grid}" && true
  export err=$?
  if [[ ${err} -ne 0 ]]; then
     err_exit "${pgm} failed during the generation of ${GEMGRD}"
  fi
  #####################################################
  # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
  # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
  # FOR THIS CASE HERE.
  #####################################################
  if [[ -f "${GEMGRD}" ]]; then
     ls -l "${GEMGRD}"
  else
     export err=1
     export pgm="GEMPAK CHECK FILE"
     err_exit "Gempak failed to generate the desired grid ${GEMGRD}"
  fi

  if [[ "${NAGRIB}" == "nagrib2" ]]; then gpend; fi

  # Copy output to COMOUT
  cpfs "${GEMGRD}" "${COMOUT_WAVE_GEMPAK}/${GEMGRD}"

  if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" "${COMOUT_WAVE_GEMPAK}/${GEMGRD}"
  fi

done
