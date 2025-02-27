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

source "${USHgfs}/preamble.sh"
source "${USHgfs}/wave_domain_grid.sh"

cd "${DATA}" || exit 1
cp "${HOMEgfs}/gempak/fix/g2varswmo2.tbl" .

cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no
g2tbls=g2varswmo2.tbl
NAGRIB=nagrib2

fhr3=$(printf "%03d" "${FORECAST_HOUR}")
for grid in ${GEMPAK_GRIDS};do
    case "${grid}" in
      ao_9km)
        #grdIDout='gfswaveao9km' ;;
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
      glo_10m)
        #grdIDout='gfswaveg16k' ;;
        grdIDout='gfswavenh'
        ;;
      gso_15m)
        #grdIDout='gfswaves25k' ;;
        grdIDout='gfswavesh'
        ;;
      glo_200)
        grdIDout='gfswaves200k'
        ;;
      *)
        echo "FATAL ERROR: Unknown wave grid ${grid}"
        exit 10
        ;;
    esac
    process_grdID "${grid}"
    com_varname="COMIN_WAVE_GRID_${GRDREGION}_${GRDRES}"
    com_dir=${!com_varname}
    GRIBIN="${RUN}.wave.${cycle}.${GRDRES}.f${fhr3}.${GRDREGION}.grib2"
    cp "${com_dir}/${GRIBIN}" "./${GRIBIN}"

    if [[ "${GRDREGION}.${GRDRES}" == "global.0p25" ]]; then
      ${WGRIB2} -lola 0:720:0.5 -90:361:0.5 \
        "${RUN}.wave.t${cyc}z.f${fhr3}.${grdIDout}.grib2" grib "${GRIBIN}"
      err=$?
      if [[ "${err}" -ne 0 ]]; then
        echo 'FATAL ERROR: Error interolating the global grid'
        err=2; export err; err_chk
      else
        GRIBIN="${RUN}.wave.t${cyc}z.f${fhr3}.${grdIDout}.grib2"
      fi
    fi
    echo "INFO: ${GRIBIN}"

    GEMGRD="${RUN}.wave.t${cyc}z.gempak.f${fhr3}.${grdIDout}.gem"

    pgm=${NAGRIB}
    startmsg
    cat << EOF > gempak_ctrl
        GBFILE   = ${GRIBIN}
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
        G2TBLS   = ${g2tbls}
        GBDIAG   = 
        PDSEXT   = ${pdsext}
        l
        r
EOF
    ${pgm} < gempak_ctrl
    export err=$?; err_chk
    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################
    ls -l "${GEMGRD}"
    export err=$?
    if [[ "${err}" -ne 0 ]]; then
        echo "FATAL ERROR: Gempak failed to create gem file"
        pgm="GEMPAK CHECK FILE" err_chk
    fi

    if [[ "${NAGRIB}" == "nagrib2" ]] ; then
      gpend
    fi

    cpfs "${GEMGRD}" "${COMOUT_WAVE_GEMPAK}/${GEMGRD}"
    if [[ "${SENDDBN}" == "YES" ]] ; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" "${COMOUT_WAVE_GEMPAK}/${GEMGRD}"
    else
        echo "INFO: DBN_ALERT is: MODEL ${DBN_ALERT_TYPE} ${job} ${COMOUT_WAVE_GEMPAK}/${GEMGRD}"
    fi
done
#####################################################################


############################### END OF SCRIPT #######################
