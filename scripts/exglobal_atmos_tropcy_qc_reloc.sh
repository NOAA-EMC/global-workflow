############################################################################
echo "---------------------------------------------------------------------"
echo "exglobal_atmos_tropcy_qc_reloc.sh - Tropical Cyclone QC/Relocation Prcocessing"
echo "---------------------------------------------------------------------"
echo "History: Jun 13 2006 - Original script."
echo "          March 2013 - No changes needed for WCOSS transition"
echo "                       MP_LABELIO default added"
echo "            Oct 2013 - Use main USH vars as part of minor pkg cleanup"
############################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

cat break > $pgmout

export COMSP=$COMOUT/${RUN}.${cycle}.

tmhr=`echo $tmmark|cut -c3-4`
cdate10=` ${NDATE:?} -$tmhr $PDY$cyc`

NET_uc=$(echo $RUN | tr [a-z] [A-Z])
tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="$NET_uc ANALYSIS TIME IS $PDY$cyc"
postmsg "$jlogfile" "$msg"

iflag=0
if [ $RUN = ndas ]; then
   if [ $DO_RELOCATE = NO ]; then
      msg="CENTER PROCESSING TIME FOR NDAS TROPICAL CYCLONE QC IS $cdate10"
      postmsg "$jlogfile" "$msg"
      msg="Output tcvitals files will be copied forward in time to proper \
output file directory path locations"
      postmsg "$jlogfile" "$msg"
      iflag=1
   else
      msg="CENTER PROCESSING TIME FOR $tmmark_uc NDAS TROPICAL CYCLONE \
RELOCATION IS $cdate10"
      postmsg "$jlogfile" "$msg"
   fi
else
   msg="CENTER PROCESSING TIME FOR $tmmark_uc $NET_uc TROPICAL CYCLONE QC/\
RELOCATION IS $cdate10"
   postmsg "$jlogfile" "$msg"
fi


if [ "$PROCESS_TROPCY" = 'YES' ]; then

####################################
####################################
#  QC tcvitals for tropical cyclones
####################################
####################################

#echo $PDY

   ${USHSYND:-$HOMEgfs/ush}/syndat_qctropcy.sh $cdate10
   errsc=$?
   if [ "$errsc" -ne '0' ]; then
    msg="syndat_qctropcy.sh failed. exit"
    postmsg "$jlogfile" "$msg"
    exit $errsc
   fi
   

   cd $COMOUT
   pwd
   set +x
   ls -ltr *syndata*
   set -x
   cd $ARCHSYND
   pwd;ls -ltr
   cat syndat_dateck
   cd $HOMENHC
   pwd;ls -ltr
   cd $DATA

else

# Copy null files into "syndata.tcvitals" and "jtwc-fnoc.tcvitals"
#  (Note: Only do so if files don't already exist - need because in NDAS this
#         script is run twice, first time with DO_RELOCATE=NO, copying these
#         files, and second time with PROCESS_TROPCY=NO and thus coming here -
#         don't want to wipe out these files)
#         

   [ ! -s ${COMSP}syndata.tcvitals.$tmmark ]  &&  \
    cp /dev/null ${COMSP}syndata.tcvitals.$tmmark
   [ ! -s ${COMSP}jtwc-fnoc.tcvitals.$tmmark ]  &&  \
    cp /dev/null ${COMSP}jtwc-fnoc.tcvitals.$tmmark

#  endif loop $PROCESS_TROPCY
fi


if [ "$DO_RELOCATE" = 'YES' ]; then

###################################################
###################################################
#  Relocate tropical cyclones in global sigma guess
###################################################
###################################################

   export MP_LABELIO=${MP_LABELIO:-yes}
   $USHRELO/tropcy_relocate.sh $cdate10
   errsc=$?

   [ "$errsc" -ne '0' ]  &&  exit $errsc


# save global sigma guess file(s) possibly updated by tropical cyclone
#  relocation processing in COMSP path
   qual_last=".$tmmark"  # need this because gfs and gdas don't add $tmmark
                         #  qualifer to end of output sigma guess files
   [ $RUN = gfs -o $RUN = gdas -o $NET = cfs ]  &&  qual_last=""

   if [ $BKGFREQ -eq 1 ]; then
      [ -s sgm3prep ]  &&  cp sgm3prep ${COMSP}sgm3prep${qual_last}
      [ -s sgm2prep ]  &&  cp sgm2prep ${COMSP}sgm2prep${qual_last}
      [ -s sgm1prep ]  &&  cp sgm1prep ${COMSP}sgm1prep${qual_last}
      [ -s sgesprep ]  &&  cp sgesprep ${COMSP}sgesprep${qual_last}
      [ -s sgp1prep ]  &&  cp sgp1prep ${COMSP}sgp1prep${qual_last}
      [ -s sgp2prep ]  &&  cp sgp2prep ${COMSP}sgp2prep${qual_last}
      [ -s sgp3prep ]  &&  cp sgp3prep ${COMSP}sgp3prep${qual_last}
   elif [ $BKGFREQ -eq 3 ]; then
      [ -s sgm3prep ]  &&  cp sgm3prep ${COMSP}sgm3prep${qual_last}
      [ -s sgesprep ]  &&  cp sgesprep ${COMSP}sgesprep${qual_last}
      [ -s sgp3prep ]  &&  cp sgp3prep ${COMSP}sgp3prep${qual_last}
   fi

# The existence of ${COMSP}tropcy_relocation_status.$tmmark file will tell the
#  subsequent PREP processing that RELOCATION processing occurred, if this file
#  does not already exist at this point, echo "RECORDS PROCESSED" into it to
#  further tell PREP processing that records were processed by relocation and
#  the global sigma guess was modified by tropical cyclone relocation
#  Note: If ${COMSP}tropcy_relocation_status.$tmmark already exists at this
#        point it means that it contains the string "NO RECORDS to process"
#        and was created by the child script tropcy_relocate.sh because records
#        were not processed by relocation and the global sigma guess was NOT
#        modified by tropical cyclone relocation (because no tcvitals records
#        were found in the relocation step)
# ----------------------------------------------------------------------------

   [ ! -s ${COMSP}tropcy_relocation_status.$tmmark ]  &&  \
    echo "RECORDS PROCESSED" > ${COMSP}tropcy_relocation_status.$tmmark

#  endif loop $DO_RELOCATE
fi


########################################################

# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x


# save standard output
cat break $pgmout break > allout
cat allout
# rm allout

sleep 10

if [ $iflag -eq 0 ]; then
   msg='ENDED NORMALLY.'
   postmsg "$jlogfile" "$msg"
fi

################## END OF SCRIPT #######################
