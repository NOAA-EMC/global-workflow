#! /usr/bin/env bash

############################################################################
# echo "---------------------------------------------------------------------"
# echo "exglobal_atmos_tropcy_qc_reloc.sh - Tropical Cyclone QC/Relocation Prcocessing"
# echo "---------------------------------------------------------------------"
# echo "History: Jun 13 2006 - Original script."
# echo "          March 2013 - No changes needed for WCOSS transition"
# echo "                       MP_LABELIO default added"
# echo "            Oct 2013 - Use main USH vars as part of minor pkg cleanup"
############################################################################

# Make sure we are in the $DATA directory
cd "${DATA}"

tmhr=${tmmark:2:2}
cdate10=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${tmhr} hours")

NET_uc=${RUN^^}
tmmark_uc=${tmmark^^}

iflag=0
if [[ "${RUN}" = ndas ]]; then
   if [[ "${DO_RELOCATE}" = NO ]]; then
      echo "CENTER PROCESSING TIME FOR NDAS TROPICAL CYCLONE QC IS ${cdate10}"
      echo "Output tcvitals files will be copied forward in time to proper \
output file directory path locations"
      iflag=1
   else
      echo "CENTER PROCESSING TIME FOR ${tmmark_uc} NDAS TROPICAL CYCLONE \
RELOCATION IS ${cdate10}"
   fi
else
   echo "CENTER PROCESSING TIME FOR ${tmmark_uc} ${NET_uc} TROPICAL CYCLONE QC/\
RELOCATION IS ${cdate10}"
fi


if [[ "${PROCESS_TROPCY}" = 'YES' ]]; then

####################################
####################################
#  QC tcvitals for tropical cyclones
####################################
####################################

#echo $PDY

   "${USHgfs}/syndat_qctropcy.sh" "${cdate10}"
   errsc=$?
   if [[ ${errsc} -ne 0 ]]; then
    echo "syndat_qctropcy.sh failed. exit"
    exit ${errsc}
   fi
   

   cd "${COMOUT_OBS}" || exit 1
   pwd
   ls -ltr *syndata*
   cd "${ARCHSYND}"
   pwd;ls -ltr
   cat syndat_dateck
   cd "${HOMENHC}"
   pwd;ls -ltr
   cd "${DATA}"

else

# Copy null files into "syndata.tcvitals" and "jtwc-fnoc.tcvitals"
#  (Note: Only do so if files don't already exist - need because in NDAS this
#         script is run twice, first time with DO_RELOCATE=NO, copying these
#         files, and second time with PROCESS_TROPCY=NO and thus coming here -
#         don't want to wipe out these files)
#         

   if [[ ! -s "${COMOUT_OBS}/${RUN}.t${cyc}z.syndata.tcvitals.${tmmark}" ]]; then
      cpfs "/dev/null" "${COMOUT_OBS}/${RUN}.t${cyc}z.syndata.tcvitals.${tmmark}"
   fi
   if [[ ! -s "${COMOUT_OBS}/${RUN}.t${cyc}z.jtwc-fnoc.tcvitals.${tmmark}" ]]; then
      cpfs "/dev/null" "${COMOUT_OBS}/${RUN}.t${cyc}z.jtwc-fnoc.tcvitals.${tmmark}"
   fi

#  endif loop $PROCESS_TROPCY
fi


if [[ "${DO_RELOCATE}" = 'YES' ]]; then

###################################################
###################################################
#  Relocate tropical cyclones in global sigma guess
###################################################
###################################################

   export MP_LABELIO=${MP_LABELIO:-yes}
   "${USHgfs}/tropcy_relocate.sh" "${cdate10}"
   export err=$?

   if [[ ${err} -ne 0 ]]; then
      err_exit "Failed while updating tropical cyclone data!"
   fi


# save global sigma guess file(s) possibly updated by tropical cyclone
#  relocation processing in COMSP path
   qual_last=".${tmmark}"  # need this because gfs and gdas don't add $tmmark
                         #  qualifer to end of output sigma guess files
   if [[ "${RUN}" == gfs || "${RUN}" == gdas || "${NET}" == cfs ]]; then
      qual_last=""
   fi

   if [[ ${BKGFREQ} -eq 1 ]]; then
      if [[ -s sgm3prep ]]; then
         cpfs "sgm3prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgm3prep${qual_last}"
      fi
      if [[ -s sgm2prep ]]; then
         cpfs "sgm2prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgm2prep${qual_last}"
      fi
      if [[ -s sgm1prep ]]; then
         cpfs "sgm1prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgm1prep${qual_last}"
      fi
      if [[ -s sgesprep ]]; then
         cpfs "sgesprep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgesprep${qual_last}"
      fi
      if [[ -s sgp1prep ]]; then
         cpfs "sgp1prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgp1prep${qual_last}"
      fi
      if [[ -s sgp2prep ]]; then
         cpfs "sgp2prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgp2prep${qual_last}"
      fi
      if [[ -s sgp3prep ]]; then
         cpfs "sgp3prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgp3prep${qual_last}"
      fi
   elif [[ ${BKGFREQ} -eq 3 ]]; then
      if [[ -s sgm3prep ]]; then
         cpfs "sgm3prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgm3prep${qual_last}"
      fi
      if [[ -s sgesprep ]]; then
         cpfs "sgesprep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgesprep${qual_last}"
      fi
      if [[ -s sgp3prep ]]; then
         cpfs "sgp3prep" "${COMOUT_OBS}/${RUN}.t${cyc}z.sgp3prep${qual_last}"
      fi
   fi

# The existence of ${COMOUT_OBS}/${RUN}.t${cyc}z.tropcy_relocation_status.$tmmark file will tell the
#  subsequent PREP processing that RELOCATION processing occurred, if this file
#  does not already exist at this point, echo "RECORDS PROCESSED" into it to
#  further tell PREP processing that records were processed by relocation and
#  the global sigma guess was modified by tropical cyclone relocation
#  Note: If ${COMOUT_OBS}/${RUN}.t${cyc}z.tropcy_relocation_status.$tmmark already exists at this
#        point it means that it contains the string "NO RECORDS to process"
#        and was created by the child script tropcy_relocate.sh because records
#        were not processed by relocation and the global sigma guess was NOT
#        modified by tropical cyclone relocation (because no tcvitals records
#        were found in the relocation step)
# ----------------------------------------------------------------------------

   if [[ ! -s "${COMOUT_OBS}/${RUN}.t${cyc}z.tropcy_relocation_status.${tmmark}" ]]; then
      echo "RECORDS PROCESSED" > "${COMOUT_OBS}/${RUN}.t${cyc}z.tropcy_relocation_status.${tmmark}"
   fi

#  endif loop $DO_RELOCATE
fi


################## END OF SCRIPT #######################
