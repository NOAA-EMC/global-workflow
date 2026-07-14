#! /usr/bin/env bash

############################################################################
# echo "---------------------------------------------------------------------"
# echo "exglobal_atmos_tropcy_qc.sh - Tropical Cyclone tcvitals QC Processing"
# echo "---------------------------------------------------------------------"
# echo "History: Jun 13 2006 - Original script."
# echo "          March 2013 - No changes needed for WCOSS transition"
# echo "                       MP_LABELIO default added"
# echo "            Oct 2013 - Use main USH vars as part of minor pkg cleanup"
# echo "            2026     - Removed tropical cyclone relocation processing;"
# echo "                       this job now only performs tcvitals QC."
############################################################################

# Set default pgm for err_exit
pgm=$(basename "${BASH_SOURCE[0]}")
export pgm

# Make sure we are in the $DATA directory
cd "${DATA}" || exit 1

tmhr=${tmmark:2:2}
cdate10=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${tmhr} hours")

NET_uc=${RUN^^}
tmmark_uc=${tmmark^^}

if [[ "${RUN}" = ndas ]]; then
    echo "CENTER PROCESSING TIME FOR NDAS TROPICAL CYCLONE QC IS ${cdate10}"
    echo "Output tcvitals files will be copied forward in time to proper \
output file directory path locations"
else
    echo "CENTER PROCESSING TIME FOR ${tmmark_uc} ${NET_uc} TROPICAL CYCLONE QC \
IS ${cdate10}"
fi

if [[ "${PROCESS_TROPCY}" = 'YES' ]]; then

    ####################################
    ####################################
    #  QC tcvitals for tropical cyclones
    ####################################
    ####################################

    #echo $PDY

    "${USHglobal}/syndat_qctropcy.sh" "${cdate10}"
    errsc=$?
    if [[ ${errsc} -ne 0 ]]; then
        echo "syndat_qctropcy.sh failed. exit"
        exit "${errsc}"
    fi

    cd "${COMOUT_OBS}" || exit 1
    pwd
    ls -ltr ./*syndata*
    cd "${ARCHSYND}" || exit 1
    pwd
    ls -ltr ./*syndata* || true
    cat syndat_dateck
    cd "${HOMENHC}" || exit 1
    pwd
    ls -ltr
    cd "${DATA}" || exit 1

else

    # Copy null files into "syndata.tcvitals" and "jtwc-fnoc.tcvitals"
    #  (Note: Only do so if files don't already exist - don't want to wipe out
    #         files that may have been created by a previous run)
    #

    if [[ ! -s "${COMOUT_OBS}/${RUN}.t${cyc}z.syndata.tcvitals.${tmmark}" ]]; then
        cpfs "/dev/null" "${COMOUT_OBS}/${RUN}.t${cyc}z.syndata.tcvitals.${tmmark}"
    fi
    if [[ ! -s "${COMOUT_OBS}/${RUN}.t${cyc}z.jtwc-fnoc.tcvitals.${tmmark}" ]]; then
        cpfs "/dev/null" "${COMOUT_OBS}/${RUN}.t${cyc}z.jtwc-fnoc.tcvitals.${tmmark}"
    fi

#  endif loop $PROCESS_TROPCY
fi

################## END OF SCRIPT #######################
