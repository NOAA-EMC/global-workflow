#!/bin/ksh -x

###############################################################
## Abstract:
## Ensemble archive driver script
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
## CDUMP  : cycle name (gdas / gfs)
## ENSGRP : ensemble sub-group to archive (0, 1, 2, ...)
###############################################################

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base earc"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Run relevant tasks

# CURRENT CYCLE
APREFIX="${CDUMP}.t${cyc}z."
ASUFFIX=".nemsio"

###############################################################
# Determine if this cycle is going to save ensemble restarts
EARC_CYC=${EARC_CYC:-"00"}
if [ $ENSGRP -gt 0 ]; then

    arch_ens_rst="NO"
    for ens_cyc in $EARC_CYC; do
        [[ "$ens_cyc" = $cyc ]] && arch_ens_rst="YES"
    done

    if [ $arch_ens_rst = "NO" ]; then
        echo "Nothing to archive for ENSGRP = $ENSGRP and cyc = $cyc, EXITING!"
        exit 0
    fi

fi

###############################################################
# Create temporary DATA directory
COMIN_ENS="$ROTDIR/enkf.$CDUMP.$PDY/$cyc"

DATA="$RUNDIR/$CDATE/$CDUMP/earc$ENSGRP"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

###############################################################
# ENSGRP -gt 0 archives ensemble member restarts
if [ $ENSGRP -gt 0 ]; then

    mkdir -p $DATA/enkf.${CDUMP}restart
    cd $DATA/enkf.${CDUMP}restart

    # Get ENSBEG/ENSEND from ENSGRP and NMEM_EARCGRP
    ENSEND=$((NMEM_EARCGRP * ENSGRP))
    ENSBEG=$((ENSEND - NMEM_EARCGRP + 1))

    for imem in $(seq $ENSBEG $ENSEND); do

        memchar="mem"$(printf %03i $imem)

        memdir="$COMIN_ENS/$memchar"
        tmpmemdir="$DATA/enkf.${CDUMP}restart/$memchar"

        mkdir -p $tmpmemdir
        cd $tmpmemdir

        restart_dir="$memdir/RESTART"
        if [ -d $restart_dir ]; then
            mkdir -p RESTART
            files=$(ls -1 $restart_dir)
            for file in $files; do
                $NCP $restart_dir/$file RESTART/$file
            done
        fi

        increment_file="$memdir/${APREFIX}atminc.nc"
        [[ -f $increment_file ]] && $NCP $increment_file .

        cd $DATA/enkf.${CDUMP}restart

        htar -P -cvf $ATARDIR/$CDATE/enkf.${CDUMP}restart.$memchar.tar $memchar
        status=$?
        if [ $status -ne 0 ]; then
            echo "HTAR $CDATE enkf.${CDUMP}restart.$memchar.tar failed"
            exit $status
        fi

        hsi ls -l $ATARDIR/$CDATE/enkf.${CDUMP}restart.$memchar.tar
        status=$?
        if [ $status -ne 0 ]; then
            echo "HSI $CDATE enkf.${CDUMP}restart.$memchar.tar failed"
            exit $status
        fi

        rm -rf $tmpmemdir

    done

    cd $DATA

    rm -rf enkf.${CDUMP}restart

fi

###############################################################
# ENSGRP 0 archives extra info, ensemble mean, verification stuff
if [ $ENSGRP -eq 0 ]; then

    ###############################################################
    # Archive extra information that is good to have
    mkdir -p $DATA/enkf.$CDUMP
    cd $DATA/enkf.$CDUMP

    # Ensemble mean related files
    ENSMEAN_STATS="gsistat.ensmean cnvstat.ensmean enkfstat atmf006.ensmean.nc4 atmf006.ensspread.nc4"
    for file in $ENSMEAN_STATS; do
        $NCP $COMIN_ENS/${APREFIX}$file .
    done

    # Ensemble member related files
    # Only archive gsistat and cnvstat files, user can provide other to ENKF_STAT
    # in config.earc if desired
    ENKF_STATS=${ENKF_STATS:-"gsistat cnvstat"}
    for imem in $(seq 1 $NMEM_ENKF); do

        memchar="mem"$(printf %03i $imem)

        memdir="$COMIN_ENS/$memchar"
        tmpmemdir="$DATA/enkf.${CDUMP}/$memchar"

        mkdir -p $tmpmemdir

        for file in $ENKF_STATS; do
            $NCP $memdir/${APREFIX}$file $tmpmemdir/.
        done

        cd $DATA/enkf.$CDUMP

    done

    cd $DATA

    htar -P -cvf $ATARDIR/$CDATE/enkf.${CDUMP}.tar enkf.$CDUMP
    status=$?
    if [ $status -ne 0 ]; then
        echo "HTAR $CDATE enkf.${CDUMP}.tar failed"
        exit $status
    fi

    hsi ls -l $ATARDIR/$CDATE/enkf.${CDUMP}.tar
    status=$?
    if [ $status -ne 0 ]; then
        echo "HSI $CDATE enkf.${CDUMP}.tar failed"
        exit $status
    fi

    rm -rf enkf.$CDUMP

    ###############################################################
    # Archive online for verification and diagnostics
    [[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
    cd $ARCDIR

    $NCP $COMIN_ENS/${APREFIX}enkfstat         enkfstat.${CDUMP}.$CDATE
    $NCP $COMIN_ENS/${APREFIX}gsistat.ensmean  gsistat.${CDUMP}.${CDATE}.ensmean

fi

###############################################################
# ENSGRP 0 also does clean-up
if [ $ENSGRP -eq 0 ]; then
    ###############################################################
    # Clean up previous cycles; various depths
    # PRIOR CYCLE: Leave the prior cycle alone
    GDATE=$($NDATE -$assim_freq $CDATE)

    # PREVIOUS to the PRIOR CYCLE
    # Now go 2 cycles back and remove the directory
    GDATE=$($NDATE -$assim_freq $GDATE)
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)

    COMIN_ENS="$ROTDIR/enkf.$CDUMP.$gPDY/$gcyc"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

    # PREVIOUS day 00Z remove the whole day
    GDATE=$($NDATE -48 $CDATE)
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)

    COMIN_ENS="$ROTDIR/enkf.$CDUMP.$gPDY"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

fi

###############################################################
# Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
exit 0
