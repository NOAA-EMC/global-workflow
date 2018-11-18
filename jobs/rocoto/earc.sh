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

# ICS are restarts and always lag INC by $assim_freq hours.
EARCINC_CYC=$ARCH_CYC
EARCICS_CYC=$((ARCH_CYC-assim_freq))
if [ $EARCICS_CYC -lt 0 ]; then
    EARCICS_CYC=$((EARCICS_CYC+24))
fi

# EnKF update in GFS, GDAS or both
CDUMP_ENKF=$(echo ${EUPD_CYC:-"gdas"} | tr a-z A-Z)

ARCH_LIST="$ROTDIR/enkf${CDUMP}.$PDY/$cyc/earc$ENSGRP"
[[ -d $ARCH_LIST ]] && rm -rf $ARCH_LIST
mkdir -p $ARCH_LIST
cd $ARCH_LIST

$HOMEgfs/ush/hpssarch_gen.sh enkf${CDUMP}
status=$?
if [ $status -ne 0 ]; then
   echo "$HOMEgfs/ush/hpssarch_gen.sh enkf${CDUMP} failed, ABORT!"
   exit $status
fi

cd $ROTDIR


###################################################################
# ENSGRP > 0 archives a group of ensemble members
firstday=$($NDATE +24 $SDATE)
if [[ $ENSGRP -gt 0 ]] && [[ $HPSSARCH = "YES" ]]; then

#--determine when to save ICs for warm start
   SAVEWARMICA="NO"
   SAVEWARMICB="NO"
   mm=`echo $CDATE|cut -c 5-6`
   dd=`echo $CDATE|cut -c 7-8`
   nday=$(( (mm-1)*30+dd ))
   mod=$(($nday % $ARCH_WARMICFREQ))
   if [ $CDATE -eq $firstday -a $cyc -eq $EARCINC_CYC ]; then SAVEWARMICA="YES" ; fi
   if [ $CDATE -eq $firstday -a $cyc -eq $EARCICS_CYC ]; then SAVEWARMICB="YES" ; fi
   if [ $mod -eq 0 -a $cyc -eq $EARCINC_CYC ]; then SAVEWARMICA="YES" ; fi
   if [ $mod -eq 0 -a $cyc -eq $EARCICS_CYC ]; then SAVEWARMICB="YES" ; fi

   if [ $EARCICS_CYC -eq 18 ]; then
       nday1=$((nday+1))
       mod1=$(($nday1 % $ARCH_WARMICFREQ))
       if [ $mod1 -eq 0 -a $cyc -eq $EARCICS_CYC ] ; then SAVEWARMICB="YES" ; fi
       if [ $mod1 -ne 0 -a $cyc -eq $EARCICS_CYC ] ; then SAVEWARMICB="NO" ; fi
       if [ $CDATE -eq $SDATE -a $cyc -eq $EARCICS_CYC ] ; then SAVEWARMICB="YES" ; fi
   fi

   htar -P -cvf $ATARDIR/$CDATE/enkf${CDUMP}_grp${ENSGRP}.tar `cat $ARCH_LIST/enkf${CDUMP}_grp${n}.txt`
   status=$?
   if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
       echo "HTAR $CDATE enkf${CDUMP}_grp${ENSGRP}.tar failed"
       exit $status
   fi

   if [ $SAVEWARMICA = "YES" -a $cyc -eq $EARCINC_CYC ]; then
       htar -P -cvf $ATARDIR/$CDATE/enkf${CDUMP}_restarta_grp${ENSGRP}.tar `cat $ARCH_LIST/enkf${CDUMP}_restarta_grp${n}.txt`
       status=$?
       if [ $status -ne 0 ]; then
           echo "HTAR $CDATE enkf${CDUMP}_restarta_grp${ENSGRP}.tar failed"
           exit $status
       fi
   fi

   if [ $SAVEWARMICB = "YES"  -a $cyc -eq $EARCICS_CYC ]; then
       htar -P -cvf $ATARDIR/$CDATE/enkf${CDUMP}_restartb_grp${ENSGRP}.tar `cat $ARCH_LIST/enkf${CDUMP}_restartb_grp${n}.txt`
       status=$?
       if [ $status -ne 0 ]; then
           echo "HTAR $CDATE enkf${CDUMP}_restartb_grp${ENSGRP}.tar failed"
           exit $status
       fi
   fi

fi


###################################################################
# ENSGRP 0 archives ensemble means and copy data to online archive
if [ $ENSGRP -eq 0 ]; then

    if [ $HPSSARCH = "YES" ]; then

        htar -P -cvf $ATARDIR/$CDATE/enkf${CDUMP}.tar `cat $ARCH_LIST/enkf${CDUMP}.txt`
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "HTAR $CDATE enkf${CDUMP}.tar failed"
            exit $status
        fi
    fi

    #-- Archive online for verification and diagnostics
    [[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
    cd $ARCDIR

    $NCP $ROTDIR/enkf${CDUMP}.$PDY/$cyc/${CDUMP}.t${cyc}z.enkfstat       enkfstat.${CDUMP}.$CDATE
    $NCP $ROTDIR/enkf$CDUMP.$PDY/$cyc/${CDUMP}.t${cyc}z.gsistat.ensmean  gsistat.${CDUMP}.${CDATE}.ensmean

    if [ $CDUMP_ENKF != "GDAS" ]; then
		$NCP $ROTDIR/enkfgfs.$PDY/$cyc/${CDUMP}.t${cyc}z.enkfstat         enkfstat.gfs.$CDATE
		$NCP $ROTDIR/enkfgfs.$PDY/$cyc/${CDUMP}.t${cyc}z.gsistat.ensmean  gsistat.gfs.${CDATE}.ensmean
	fi

fi


if [[ "${DELETE_COM_IN_ARCHIVE_JOB:-YES}" == NO ]] ; then
    exit 0
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
    COMIN_ENS="$ROTDIR/enkfgdas.$gPDY/$gcyc"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS
    COMIN_ENS="$ROTDIR/enkfgfs.$gPDY/$gcyc"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

    # PREVIOUS day 00Z remove the whole day
    GDATE=$($NDATE -48 $CDATE)
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)

	# Handle GDAS and GFS EnKF directories separately
    COMIN_ENS="$ROTDIR/enkfgdas.$gPDY"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS
    COMIN_ENS="$ROTDIR/enkfgfs.$gPDY"
    [[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

fi

###############################################################
exit 0
