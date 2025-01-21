#! /usr/bin/env bash                                                                                                                                                                          

################################################################################
## UNIX Script Documentation Block
## Script name:         wave_domain_grid.sh
## Script description:  provides the wave grid specific values that
##                      are needed for the wave related jobs
#######################
# Main body starts here
#######################

process_grdID() {
  grdID=$1
  case ${grdID} in
    glo_10m) GRDREGION='global' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
    glo_15mxt) GRDREGION='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;       
    glo_30mxt) GRDREGION='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=11   ;;
    glo_30m) GRDREGION='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=11   ;;
    glo_025) GRDREGION='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;
    glo_100) GRDREGION='global' ; GRDRES=1p00 ; GRIDNR=255  ; MODNR=11   ;;
    glo_200) GRDREGION='global' ; GRDRES=2p00 ; GRIDNR=255  ; MODNR=11   ;;
    glo_500) GRDREGION='global' ; GRDRES=5p00 ; GRIDNR=255  ; MODNR=11   ;;
    at_10m) GRDREGION='atlocn' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
    ep_10m) GRDREGION='epacif' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
    wc_10m) GRDREGION='wcoast' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
    ak_10m) GRDREGION='alaska' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
    aoc_9km) GRDREGION='arctic' ; GRDRES=9km ; GRIDNR=255  ; MODNR=11   ;;
    ant_9km) GRDREGION='antarc' ; GRDRES=9km ; GRIDNR=255  ; MODNR=11   ;;
    gnh_10m) GRDREGION='global' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
    gsh_15m) GRDREGION='gsouth' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;
    ao_20m) GRDREGION='arctic' ; GRDRES=0p33 ; GRIDNR=255  ; MODNR=11   ;;
    so_20m) GRDREGION='antarc' ; GRDRES=0p33 ; GRIDNR=255  ; MODNR=11   ;;
    reg025) GRDREGION='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;
    gwes_30m) GRDREGION='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=10   ;;
    *)
    echo "FATAL ERROR: No grid specific wave config values exist for ${grdID}. Aborting."
    exit 1 ;;
  esac
  grdNAME="${GRDREGION}.${GRDRES}"
  echo "grdNAME=${grdNAME}"
  echo "GRIDNR=${GRIDNR}"
  echo "MODNR=${MODNR}"
}
