#!/bin/ksh

set -ax

export COMOUT=${COMOUT:-$COMROT}

export SENDCOM=YES

export HOMEglobal=${HOMEglobal:-$BASE_EMCSFC}
export FIXglobal=${FIXglobal:-$HOMEglobal/fix}
export FIXglobal_am=${FIXglobal_am:-$HOMEglobal/fix/fix_am}
export EXECglobal=${EXECglobal:-$HOMEglobal/exec}
export USHglobal=${USHglobal:-$HOMEglobal/ush}

GDATE=$($NDATE -06 $CDATE)

export IMS_FILE=${IMS_FILE:-$DMPDIR/$CDATE/$CDUMP/imssnow96.grib2.$CDUMP.$CDATE}
export FIVE_MIN_ICE_FILE=${FIVE_MIN_ICE_FILE:-$DMPDIR/$CDATE/$CDUMP/seaice.5min.grib2.$CDUMP.$CDATE}
export AFWA_NH_FILE=${AFWA_NH_FILE:-$DMPDIR/$CDATE/$CDUMP/NPR.SNWN.SP.S1200.MESH16.grb.$CDUMP.$CDATE}
export AFWA_SH_FILE=${AFWA_SH_FILE:-$DMPDIR/$CDATE/$CDUMP/NPR.SNWS.SP.S1200.MESH16.grb.$CDUMP.$CDATE}

export FIVE_MIN_ICE_MASK_FILE=${FIVE_MIN_ICE_MASK_FILE:-${FIXglobal}/emcsfc_gland5min.grib2}

export BLENDED_ICE_FILE=${BLENDED_ICE_FILE:-$COMOUT/seaice.5min.blend.grb.$CDUMP.$CDATE}
export BLENDED_ICE_FILE_m6hrs=${BLENDED_ICE_FILE_m6hrs:-$COMOUT/seaice.5min.blend.grb.$GDUMP.$GDATE}
if [ ! -s $BLENDED_ICE_FILE_m6hrs ] ; then
   echo "SFCPREP:  $BLENDED_ICE_FILE_m6hrs not found"
   export BLENDED_ICE_FILE_m6hrs=$DMPDIR/$GDATE/$GDUMP/seaice.5min.blend.grb.$GDUMP.$GDATE
   if [ ! -s $BLENDED_ICE_FILE_m6hrs ] ; then
      echo "SFCPREP:  $BLENDED_ICE_FILE_m6hrs not found.  ABORT"
      exit 9
   fi
fi


export JCAP=${JCAP:-"1534"}
export LONB=${LONB:-"3072"}
export LATB=${LATB:-"1536"}

export JCAP_ENKF=${JCAP_ENKF:-"574"}
export LONB_ENKF=${LONB_ENKF:-"1152"}
export LATB_ENKF=${LATB_ENKF:-"576"}

export FNSNOAJCAP=snogrb_t$JCAP.$LONB.$LATB.$CDUMP.$CDATE
export FNSNOAJCAP_OUT=${FNSNOAJCAP_OUT:-$COMOUT/snogrb_t$JCAP.$LONB.$LATB.$CDUMP.$CDATE}
export FNSNOGJCAP=${FNSNOGJCAP:-$COMOUT/snogrb_t$JCAP.$LONB.$LATB.$GDUMP.$GDATE}
if [ ! -s $FNSNOGJCAP ] ; then
   echo "SFCPREP:  $FNSNOGJCAP not found"
   export FNSNOGJCAP=$DMPDIR/$GDATE/$GDUMP/snogrb_t$JCAP.$LONB.$LATB.$GDUMP.$GDATE
   if [ ! -s $FNSNOGJCAP ] ; then
      echo "SFCPREP:  $FNSNOGJCAP not found.  ABORT"
      exit 9
   fi
fi

export FNSNOAJCAP_ENKF=snogrb_t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.$CDUMP.$CDATE
export FNSNOAJCAP_ENKF_OUT=${FNSNOAJCAP_ENKF_OUT:-$COMOUT/snogrb_t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.$CDUMP.$CDATE}
export FNSNOGJCAP_ENKF=${FNSNOGJCAP_ENKF:-$COMOUT/snogrb_t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.$GDUMP.$GDATE}
if [ ! -s $FNSNOGJCAP_ENKF ] ; then
   echo "SFCPREP:  $FNSNOGJCAP_ENKF not found"
   export FNSNOGJCAP_ENKF=$DMPDIR/$GDATE/$GDUMP/snogrb_t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.$GDUMP.$GDATE
   if [ ! -s $FNSNOGJCAP_ENKF ] ; then
      echo "SFCPREP:  $FNSNOGJCAP_ENKF not found.  ABORT"
      exit 9
   fi
fi

export BLENDICEEXEC=${BLENDICEEXEC:-${EXECglobal}/emcsfc_ice_blend}
export SNOW2MDLEXEC=${SNOW2MDLEXEC:-${EXECglobal}/emcsfc_snow2mdl}


#############################################################
# Execute the script
${EMCSFC_GLOBAL_SFC_PREPSH:-$HOMEglobal/scripts/exemcsfc_global_sfc_prep.sh.ecf}
#############################################################


exit
