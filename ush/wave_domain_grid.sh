#! /usr/bin/env bash                                                                                                                                                                          

################################################################################
## UNIX Script Documentation Block
## Script name:         wave_domain_grid.sh
## Script description:  provides the wave grid specific values that
##                      are needed for the wave related jobs
#######################
# Main body starts here
#######################

source "${USHgfs}/preamble.sh"

process_grdID() {
    grdID=$1
    case ${grdID} in
        glo_10m) GRDNAME='global' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
        glo_15mxt) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;       
        glo_30mxt) GRDNAME='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=11   ;;
        glo_30m) GRDNAME='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=11   ;;
        glo_025) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;
        glo_100) GRDNAME='global' ; GRDRES=1p00 ; GRIDNR=255  ; MODNR=11   ;;
        glo_200) GRDNAME='global' ; GRDRES=2p00 ; GRIDNR=255  ; MODNR=11   ;;
        glo_500) GRDNAME='global' ; GRDRES=5p00 ; GRIDNR=255  ; MODNR=11   ;;
        at_10m) GRDNAME='atlocn' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
        ep_10m) GRDNAME='epacif' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
        wc_10m) GRDNAME='wcoast' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
        ak_10m) GRDNAME='alaska' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
        aoc_9km) GRDNAME='arctic' ; GRDRES=9km ; GRIDNR=255  ; MODNR=11   ;;
        ant_9km) GRDNAME='antarc' ; GRDRES=9km ; GRIDNR=255  ; MODNR=11   ;;
        gnh_10m) GRDNAME='global' ; GRDRES=0p16 ; GRIDNR=255  ; MODNR=11   ;;
        gsh_15m) GRDNAME='gsouth' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;
        ao_20m) GRDNAME='arctic' ; GRDRES=0p33 ; GRIDNR=255  ; MODNR=11   ;;
        so_20m) GRDNAME='antarc' ; GRDRES=0p33 ; GRIDNR=255  ; MODNR=11   ;;
        reg025) GRDNAME='global' ; GRDRES=0p25 ; GRIDNR=255  ; MODNR=11   ;;
        gwes_30m) GRDNAME='global' ; GRDRES=0p50 ; GRIDNR=255  ; MODNR=11   ;;
        *)
        echo "FATAL ERROR: No grid specific wave config values exist for ${grdID}. Aborting."
        exit 1 ;;
    esac
    export grdNAME="${GRDNAME}.${GRDRES}"
    echo "grdNAME=${grdNAME}"
    echo "GRDRES=${GRDRES}"
    echo "GRIDNR=${GRIDNR}"
    echo "MODNR=${MODNR}"
}