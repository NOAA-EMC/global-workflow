#!/bin/ksh -x

###############################################################
## Abstract:
## Archive driver script
## RUN_ENVIR : runtime environment (emc | nco)
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base arch"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

# ICS are restarts and always lag INC by $assim_freq hours
ARCHINC_CYC=$ARCH_CYC
ARCHICS_CYC=$($NDATE -$assim_freq $ARCH_CYC)

# CURRENT CYCLE
APREFIX="${CDUMP}.t${cyc}z."
ASUFFIX=".nemsio"


###############################################################
# Archive online for verification and diagnostics
###############################################################

COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"
cd $COMIN

[[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
$NCP ${APREFIX}gsistat $ARCDIR/gsistat.${CDUMP}.${CDATE}
$NCP ${APREFIX}pgrb.1p00.anl $ARCDIR/pgbanl.${CDUMP}.${CDATE}

# Archive 1 degree forecast GRIB1 files for verification
if [ $CDUMP = "gfs" ]; then
    fhmax=$FHMAX_GFS
    fhr=0
    while [ $fhr -le $fhmax ]; do
        fhr2=$(printf %02i $fhr)
        fhr3=$(printf %03i $fhr)
        $NCP ${APREFIX}pgrb.1p00.f$fhr3 $ARCDIR/pgbf${fhr2}.${CDUMP}.${CDATE}
        (( fhr = $fhr + $FHOUT_GFS ))
    done
fi
if [ $CDUMP = "gdas" ]; then
    flist="000 003 006 009"
    for fhr in $flist; do
        fname=${APREFIX}pgrb.1p00.f${fhr}
        fhr2=$(printf %02i $fhr)
        $NCP $fname $ARCDIR/pgbf${fhr2}.${CDUMP}.${CDATE}
    done
fi

if [ -s avn.t${cyc}z.cyclone.trackatcfunix ]; then
    PLSOT4=`echo $PSLOT|cut -c 1-4 |tr '[a-z]' '[A-Z]'`
    cat avn.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunix.${CDUMP}.$CDATE
fi
if [ $CDUMP = "gfs" ]; then
    $NCP storms.gfso.atcf_gen.$CDATE      ${ARCDIR}/.
    $NCP storms.gfso.atcf_gen.altg.$CDATE ${ARCDIR}/.
    $NCP trak.gfso.atcfunix.$CDATE        ${ARCDIR}/.
    $NCP trak.gfso.atcfunix.altg.$CDATE   ${ARCDIR}/.
fi

# Archive atmospheric nemsio gfs forecast files for fit2obs
VFYARC=$ROTDIR/vrfyarch
[[ ! -d $VFYARC ]] && mkdir -p $VFYARC
if [ $CDUMP = "gfs" -a $FITSARC = "YES" ]; then
    mkdir -p $VFYARC/${CDUMP}.$PDY/$cyc
    fhmax=${FHMAX_FITS:-$FHMAX_GFS}
    fhr=0
    while [[ $fhr -le $fhmax ]]; do
      fhr3=$(printf %03i $fhr)
      sfcfile=${CDUMP}.t${cyc}z.sfcf${fhr3}.nemsio
      sigfile=${CDUMP}.t${cyc}z.atmf${fhr3}.nemsio
      $NCP $sfcfile $VFYARC/${CDUMP}.$PDY/$cyc/
      $NCP $sigfile $VFYARC/${CDUMP}.$PDY/$cyc/
      (( fhr = $fhr + 6 ))
    done
fi


###############################################################
# Archive data to HPSS
if [ $HPSSARCH = "YES" ]; then
###############################################################
SAVEIC="NO"
firstday=$($NDATE +24 $SDATE)
weekday=$(date -d "$PDY" +%u)
if [ $weekday -eq 7 -o $CDATE -eq $firstday ]; then SAVEIC="YES" ; fi

DATA="$RUNDIR/$CDATE/$CDUMP/arch"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

$HOMEgfs/ush/hpssarch_gen.sh $CDUMP
status=$?
if [ $status -ne 0  ]; then
    echo "$HOMEgfs/ush/hpssarch_gen.sh $CDUMP failed, ABORT!"
    exit $status
fi

cd $ROTDIR

if [ $CDUMP = "gfs" ]; then

    #for targrp in gfs gfs_flux gfs_nemsio gfs_pgrb2b; do
    for targrp in gfs gfs_flux gfs_nemsioa gfs_nemsiob; do
        htar -P -cvf $ATARDIR/$CDATE/${targrp}.tar `cat $DATA/${targrp}.txt`
    done
fi


if [ $CDUMP = "gdas" ]; then

    htar -P -cvf $ATARDIR/$CDATE/gdas.tar `cat $DATA/gdas.txt`
    status=$?
    if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
        echo "HTAR $CDATE gdas.tar failed"
        exit $status
    fi

    if [ $SAVEIC = "YES" -a $cyc -eq $ARCHINC_CYC ]; then
        htar -P -cvf $ATARDIR/$CDATE/gdas_restarta.tar `cat $DATA/gdas_restarta.txt`
        status=$?
        if [ $status -ne 0 ]; then
            echo "HTAR $CDATE gdas_restarta.tar failed"
            exit $status
        fi
    fi
    if [ $SAVEIC = "YES" -a $cyc -eq $ARCHICS_CYC ]; then
        htar -P -cvf $ATARDIR/$CDATE/gdas_restartb.tar `cat $DATA/gdas_restartb.txt`
        status=$?
        if [ $status -ne 0 ]; then
            echo "HTAR $CDATE gdas_restartb.tar failed"
            exit $status
        fi
    fi
fi

###############################################################
fi  ##end of HPSS archive
###############################################################



###############################################################
# Clean up previous cycles; various depths
# PRIOR CYCLE: Leave the prior cycle alone
GDATE=$($NDATE -$assim_freq $CDATE)

# PREVIOUS to the PRIOR CYCLE
GDATE=$($NDATE -$assim_freq $GDATE)
gPDY=$(echo $GDATE | cut -c1-8)
gcyc=$(echo $GDATE | cut -c9-10)

# Remove the TMPDIR directory
COMIN="$RUNDIR/$GDATE"
[[ -d $COMIN ]] && rm -rf $COMIN

if [[ "${DELETE_COM_IN_ARCHIVE_JOB:-YES}" == NO ]] ; then
    exit 0
fi

# Remove the hour directory
COMIN="$ROTDIR/$CDUMP.$gPDY/$gcyc"
[[ -d $COMIN ]] && rm -rf $COMIN

# Step back every assim_freq hours
# and remove old rotating directories for successfull cycles
# defaults from 24h to 120h
GDATEEND=$($NDATE -${RMOLDEND:-24}  $CDATE)
GDATE=$(   $NDATE -${RMOLDSTD:-120} $CDATE)
while [ $GDATE -le $GDATEEND ]; do
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)
    COMIN="$ROTDIR/$CDUMP.$gPDY/$gcyc"
    if [ -d $COMIN ]; then
        rocotolog="$EXPDIR/logs/${GDATE}.log"
        testend=$(tail -n 1 $rocotolog | grep "This cycle is complete: Success")
        rc=$?
        [[ $rc -eq 0 ]] && rm -rf $COMIN
    fi
    # Remove any empty directories
    COMIN="$ROTDIR/$CDUMP.$gPDY"
    if [ -d $COMIN ]; then
        [[ ! "$(ls -A $COMIN)" ]] && rm -rf $COMIN
    fi
    GDATE=$($NDATE +$assim_freq $GDATE)
done

# Remove archived stuff in $VFYARC that are (48+$FHMAX_GFS) hrs behind
# 1. atmospheric nemsio files used for fit2obs
if [ $CDUMP = "gfs" ]; then
    GDATE=$($NDATE -$FHMAX_GFS $GDATE)
    gPDY=$(echo $GDATE | cut -c1-8)
    COMIN="$VFYARC/$CDUMP.$gPDY"
    [[ -d $COMIN ]] && rm -rf $COMIN
fi

###############################################################
# Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
exit 0
