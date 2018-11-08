#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base prep prepbufr"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env prep
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Set script and dependency variables
export OPREFIX="${CDUMP}.t${cyc}z."
export COMOUT="$ROTDIR/$CDUMP.$PDY/$cyc"
[[ ! -d $COMOUT ]] && mkdir -p $COMOUT

###############################################################
# If ROTDIR_DUMP=YES, copy dump files to rotdir 
if [ $ROTDIR_DUMP = "YES" ]; then
    $HOMEgfs/ush/getdump.sh $CDATE $CDUMP $DMPDIR/${CDATE}/${CDUMP}${DUMP_SUFFIX} $COMOUT
    status=$?
    [[ $status -ne 0 ]] && exit $status

#   Ensure previous cycle gdas dumps are available (used by cycle & downstream)
    GDATE=$($NDATE -$assim_freq $CDATE)
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)
    GDUMP=gdas
    gCOMOUT="$ROTDIR/$GDUMP.$gPDY/$gcyc"
    if [ ! -s $gCOMOUT/$GDUMP.t${gcyc}z.updated.status.tm00.bufr_d ]; then
     $HOMEgfs/ush/getdump.sh $GDATE $GDUMP $DMPDIR/${GDATE}/${GDUMP}${DUMP_SUFFIX} $gCOMOUT
     status=$?
     [[ $status -ne 0 ]] && exit $status
    fi
    
fi

###############################################################

###############################################################
# For running real-time parallels on WCOSS_C, execute tropcy_qc and 
# copy files from operational syndata directory to a local directory.
# Otherwise, copy existing tcvital data from globaldump.

if [ $PROCESS_TROPCY = "YES" ]; then

    export ARCHSYNDNCO=$COMROOTp1/arch/prod/syndat
    if [ $RUN_ENVIR != "nco" ]; then
        export ARCHSYND=${ROTDIR}/syndat
        if [ ! -d ${ARCHSYND} ]; then mkdir -p $ARCHSYND; fi
        if [ ! -s $ARCHSYND/syndat_akavit ]; then 
            for file in syndat_akavit syndat_dateck syndat_stmcat.scr syndat_stmcat syndat_sthisto syndat_sthista ; do
                cp $ARCHSYNDNCO/$file $ARCHSYND/. 
            done
        fi
    fi

    [[ $ROTDIR_DUMP = "YES" ]] && rm $COMOUT${CDUMP}.t${cyc}z.syndata.tcvitals.tm00

    $HOMEgfs/jobs/JGLOBAL_TROPCY_QC_RELOC
    status=$?
    [[ $status -ne 0 ]] && exit $status

else
    [[ $ROTDIR_DUMP = "NO" ]] && cp $DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${cyc}z.syndata.tcvitals.tm00 $COMOUT/
fi


###############################################################
# Generate prepbufr files from dumps or copy from OPS
if [ $DO_MAKEPREPBUFR = "YES" ]; then
    if [ $ROTDIR_DUMP = "YES" ]; then
	rm $COMOUT/${OPREFIX}prepbufr
	rm $COMOUT/${OPREFIX}prepbufr.acft_profiles
	rm $COMOUT/${OPREFIX}nsstbufr
    fi
    if [ $machine = "WCOSS_C" -o $machine = "WCOSS_DELL_P3" -o $machine = "THEIA" ]; then

        export job="j${CDUMP}_prep_${cyc}"
        export DATAROOT="$RUNDIR/$CDATE/$CDUMP/prepbufr"
        if [ $ROTDIR_DUMP = "NO" ]; then
          COMIN_OBS=${COMIN_OBS:-$DMPDIR/$CDATE/$CDUMP}
          export COMSP=${COMSP:-$COMIN_OBS/$CDUMP.t${cyc}z.}
        fi
        export COMIN=${COMIN:-$ROTDIR/$CDUMP.$PDY/$cyc}
        export COMINgdas=${COMINgdas:-$ROTDIR/gdas.$PDY/$cyc}
        export COMINgfs=${COMINgfs:-$ROTDIR/gfs.$PDY/$cyc}

        $HOMEobsproc_network/jobs/JGLOBAL_PREP
        status=$?
        [[ $status -ne 0 ]] && exit $status

    else
        echo "WARNING:  prep step is not supported on $machine, exit"
        exit 1
    fi
else
    if [ $ROTDIR_DUMP = "NO" ]; then
	$NCP $DMPDIR/$CDATE/$CDUMP/${OPREFIX}prepbufr               $COMOUT/${OPREFIX}prepbufr
	$NCP $DMPDIR/$CDATE/$CDUMP/${OPREFIX}prepbufr.acft_profiles $COMOUT/${OPREFIX}prepbufr.acft_profiles
	[[ $DONST = "YES" ]] && $NCP $DMPDIR/$CDATE/$CDUMP/${OPREFIX}nsstbufr $COMOUT/${OPREFIX}nsstbufr
    fi
fi

################################################################################
# Exit out cleanly
exit 0
