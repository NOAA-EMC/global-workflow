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

n=$((ENSGRP))
SAVEIC="NO"
firstday=$($NDATE +24 $SDATE)
weekday=$(date -d "$PDY" +%u)
if [ $weekday -eq 7 -o $CDATE -eq $firstday ]; then SAVEIC="YES" ; fi

# ICS are restarts and always lag INC by $assim_freq hours
EARCINC_CYC=$ARCH_CYC
EARCICS_CYC=$($NDATE -$assim_freq $ARCH_CYC)

# EnKF update in GFS, GDAS or both
CDUMP_ENKF=$(echo ${EUPD_CYC:-"gdas"} | tr a-z A-Z)

DATA="$RUNDIR/$CDATE/$CDUMP/earc$ENSGRP"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

$HOMEgfs/ush/hpssarch_gen.sh enkf.${CDUMP}
status=$?
if [ $status -ne 0 ]; then
   echo "$HOMEgfs/ush/hpssarch_gen.sh enkf.${CDUMP} failed, ABORT!"
   exit $status
fi

cd $ROTDIR


###################################################################
# ENSGRP > 0 archives a group of ensemble members
if [[ $ENSGRP -gt 0 ]] && [[ $HPSSARCH = "YES" ]]; then

   htar -P -cvf $ATARDIR/$CDATE/enkf.${CDUMP}_grp${ENSGRP}.tar `cat $DATA/enkf.${CDUMP}_grp${n}.txt`
   status=$?
   if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
       echo "HTAR $CDATE enkf.${CDUMP}_grp${ENSGRP}.tar failed"
       exit $status
   fi

   if [ $SAVEIC = "YES" -a $cyc -eq $EARCINC_CYC ]; then
       htar -P -cvf $ATARDIR/$CDATE/enkf.${CDUMP}_restarta_grp${ENSGRP}.tar `cat $DATA/enkf.${CDUMP}_restarta_grp${n}.txt`
       status=$?
       if [ $status -ne 0 ]; then
           echo "HTAR $CDATE enkf.${CDUMP}_restarta_grp${ENSGRP}.tar failed"
           exit $status
       fi
   fi

   if [ $SAVEIC = "YES"  -a $cyc -eq $EARCICS_CYC ]; then
       htar -P -cvf $ATARDIR/$CDATE/enkf.${CDUMP}_restartb_grp${ENSGRP}.tar `cat $DATA/enkf.${CDUMP}_restartb_grp${n}.txt`
       status=$?
       if [ $status -ne 0 ]; then
           echo "HTAR $CDATE enkf.${CDUMP}_restartb_grp${ENSGRP}.tar failed"
           exit $status
       fi
   fi

fi


###################################################################
# ENSGRP 0 archives ensemble means and copy data to online archive
if [ $ENSGRP -eq 0 ]; then

    if [ $HPSSARCH = "YES" ]; then

        htar -P -cvf $ATARDIR/$CDATE/enkf.${CDUMP}.tar `cat $DATA/enkf.${CDUMP}.txt`
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "HTAR $CDATE enkf.${CDUMP}.tar failed"
            exit $status
        fi
    fi

    #-- Archive online for verification and diagnostics
    [[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
    cd $ARCDIR

    $NCP $ROTDIR/enkf.${CDUMP}.$PDY/$cyc/${CDUMP}.t${cyc}z.enkfstat       enkfstat.${CDUMP}.$CDATE
    $NCP $ROTDIR/enkf.$CDUMP.$PDY/$cyc/${CDUMP}.t${cyc}z.gsistat.ensmean  gsistat.${CDUMP}.${CDATE}.ensmean

    if [ $CDUMP_ENKF != "gdas" ]; then
		$NCP $ROTDIR/enkf.gfs.$PDY/$cyc/${CDUMP}.t${cyc}z.enkfstat         enkfstat.gfs.$CDATE
		$NCP $ROTDIR/enkf.gfs.$PDY/$cyc/${CDUMP}.t${cyc}z.gsistat.ensmean  gsistat.gfs.${CDATE}.ensmean
	fi

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

	# Handle GDAS and GFS EnKF directories separately
    COMIN_ENS="$ROTDIR/enkf.gdas.$gPDY/$gcyc"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS
    COMIN_ENS="$ROTDIR/enkf.gfs.$gPDY/$gcyc"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

    # PREVIOUS day 00Z remove the whole day
    GDATE=$($NDATE -48 $CDATE)
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)

	# Handle GDAS and GFS EnKF directories separately
    COMIN_ENS="$ROTDIR/enkf.gdas.$gPDY"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS
    COMIN_ENS="$ROTDIR/enkf.gfs.$gPDY"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

fi

###############################################################
# Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
exit 0
