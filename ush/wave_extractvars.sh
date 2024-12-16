#! /usr/bin/env bash                                                                                                                                                                          

################################################################################
## UNIX Script Documentation Block
## Script name:         wave_extractvars.sh
## Script description:  Extracts variables from wave products
##                      and saves these variables in arcdir
#######################
# Main body starts here
#######################

source "${USHgfs}/preamble.sh"

subdata=${1}

for grdID in ${waveGRD}; do
    case ${grdID} in
        glo_10m) GRDNAME='global' ; GRDRES=0p16;;
        glo_15mxt) GRDNAME='global' ; GRDRES=0p25;;       
        glo_30mxt) GRDNAME='global' ; GRDRES=0p50 ;;
        glo_30m) GRDNAME='global' ; GRDRES=0p50 ;;
        glo_025) GRDNAME='global' ; GRDRES=0p25 ;;
        glo_100) GRDNAME='global' ; GRDRES=1p00;;
        glo_200) GRDNAME='global' ; GRDRES=2p00;;
        glo_500) GRDNAME='global' ; GRDRES=5p00;;
        at_10m) GRDNAME='atlocn' ; GRDRES=0p16 ;;
        ep_10m) GRDNAME='epacif' ; GRDRES=0p16;;
        wc_10m) GRDNAME='wcoast' ; GRDRES=0p16 ;;
        ak_10m) GRDNAME='alaska' ; GRDRES=0p16;;
        aoc_9km) GRDNAME='arctic' ; GRDRES=9km;;
        ant_9km) GRDNAME='antarc' ; GRDRES=9km ;;
        gnh_10m) GRDNAME='global' ; GRDRES=0p16;;
        gsh_15m) GRDNAME='gsouth' ; GRDRES=0p25;;
        ao_20m) GRDNAME='arctic' ; GRDRES=0p33;;
        so_20m) GRDNAME='antarc' ; GRDRES=0p33;;
        reg025) GRDNAME='global' ; GRDRES=0p25;;
        gwes_30m) GRDNAME='global' ; GRDRES=0p50;;
        *)
        echo "FATAL ERROR: No grid specific wave config values exist for ${grdID}. Aborting."
        exit 1;;
    esac
done

[[ -d "${subdata}" ]] || mkdir -p "${subdata}"

for (( nh = FHOUT_WAV_EXTRACT; nh <= FHMAX_WAV; nh = nh + FHOUT_WAV_EXTRACT )); do
  fnh=$(printf "%3.3d" "${nh}")

  infile="${COMIN_WAVE_GRID}/${GRDNAME}.${GRDRES}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}.grib2"
  outfile="${subdata}/${RUN}wave.t${cyc}z.global.${wavres}.f${fnh}.grib2"
  rm -f "${outfile}" # Remove outfile if it already exists before extraction

  if [[ -f "${infile}" ]]; then # Check if input file exists before extraction
    # shellcheck disable=SC2312 
    ${WGRIB2} "${infile}" | grep -F -f "${varlist_wav}" | ${WGRIB2} -i "${infile}" -append -grib "${outfile}"
  else
    echo "WARNING: ${infile} does not exist."
  fi 
  copy_to_comout "${outfile}" "${ARC_RFCST_PROD_WAV}"
done # nh

exit 0                                                                                                                                                                                        
