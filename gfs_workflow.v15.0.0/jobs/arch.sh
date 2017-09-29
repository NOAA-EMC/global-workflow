#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
## Author: Rahul Mahajan  Org: NCEP/EMC  Date: April 2017

## Abstract:
## Archive driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

###############################################################
# Source relevant configs
configs="base arch"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Run relevant tasks

# CURRENT CYCLE
PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo  $CDATE | cut -c9-10)
APREFIX="${CDUMP}.t${cyc}z."
ASUFFIX=".nemsio"

COMIN="$ROTDIR/$CDUMP.$PDY/$cyc"

DATA="$RUNDIR/$CDATE/$CDUMP/arch"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

###############################################################
# Archive what is needed to restart the experiment
mkdir -p $DATA/${CDUMP}restart
cd $DATA/${CDUMP}restart

restart_dir="$COMIN/RESTART"
if [ -d $restart_dir ]; then
    mkdir -p RESTART
    files=$(ls -1 $restart_dir)
    for file in $files; do
        $NCP $restart_dir/$file RESTART/$file
    done
fi

increment_file="$COMIN/${APREFIX}atminc.nc"
[[ -f $increment_file ]] && $NCP $increment_file .

files="abias abias_pc abias_air radstat"
for file in $files; do
    $NCP $COMIN/${APREFIX}$file .
done

cd $DATA

htar -P -cvf $ATARDIR/$CDATE/${CDUMP}restart.tar ${CDUMP}restart
status=$?
if [ $status -ne 0 ]; then
    echo "HTAR $CDATE ${CDUMP}restart.tar failed"
    exit $status
fi

hsi ls -l $ATARDIR/$CDATE/${CDUMP}restart.tar
status=$?
if [ $status -ne 0 ]; then
    echo "HSI $CDATE ${CDUMP}restart.tar failed"
    exit $status
fi

rm -rf ${CDUMP}restart

###############################################################
# Archive extra information that is good to have
mkdir -p $DATA/$CDUMP
cd $DATA/$CDUMP

files="gsistat cnvstat prepbufr prepbufr.acft_profiles"
for file in $files; do
    $NCP $COMIN/${APREFIX}$file .
done

$NCP $COMIN/${APREFIX}atmanl${ASUFFIX} .
$NCP $COMIN/${APREFIX}pgrb2.*.fanl* .
$NCP $COMIN/${APREFIX}pgrb2.*.f* .

cd $DATA

htar -P -cvf $ATARDIR/$CDATE/${CDUMP}.tar $CDUMP
status=$?
if [ $status -ne 0 ]; then
    echo "HTAR $CDATE ${CDUMP}restart.tar failed"
    exit $status
fi

hsi ls -l $ATARDIR/$CDATE/${CDUMP}.tar
status=$?
if [ $status -ne 0 ]; then
    echo "HSI $CDATE ${CDUMP}.tar failed"
    exit $status
fi

rm -rf $CDUMP

###############################################################
# Archive online for verification and diagnostics
cd $COMIN

[[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
$NCP ${APREFIX}gsistat $ARCDIR/gsistat.${CDUMP}.${CDATE}
$NCP ${APREFIX}pgrbanl $ARCDIR/pgbanl.${CDUMP}.${CDATE}

# Archive 1 degree forecast GRIB1 files for verification
if [ $CDUMP = "gfs" ]; then
    for fname in ${APREFIX}pgrbf*; do
        fhr=$(echo $fname | cut -d. -f3 | cut -c 6-)
        $NCP $fname $ARCDIR/pgbf${fhr}.${CDUMP}.${CDATE}
    done
fi
if [ $CDUMP = "gdas" ]; then
    flist="00 03 06 09"
    for fhr in $flist; do
        fname=${APREFIX}pgrbf${fhr}
        $NCP $fname $ARCDIR/pgbf${fhr}.${CDUMP}.${CDATE}
    done
fi

# Archive
# 1. quarter degree GRIB1 files for precip verification
# 2. atmospheric nemsio gfs forecast files for fit2obs
VFYARC=$ROTDIR/vrfyarch
[[ ! -d $VFYARC ]] && mkdir -p $VFYARC
if [ $CDUMP = "gfs" ]; then

    for fname in pgbq*${CDUMP}.${CDATE}.grib1; do
       fileout=$(echo $fname | cut -d. -f1-3)  # strip off ".grib1" suffix
       $NCP $fname $ARCDIR/$fileout
    done

    mkdir -p $VFYARC/${CDUMP}.$PDY/$cyc
    fhmax=$FHMAX_GFS
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
# Clean up previous cycles; various depths
# PRIOR CYCLE: Leave the prior cycle alone
GDATE=$($NDATE -$assim_freq $CDATE)

# PREVIOUS to the PRIOR CYCLE
GDATE=$($NDATE -$assim_freq $GDATE)
gymd=$(echo $GDATE | cut -c1-8)
ghh=$(echo  $GDATE | cut -c9-10)

# Remove the TMPDIR directory
COMIN="$RUNDIR/$GDATE"
[[ -d $COMIN ]] && rm -rf $COMIN

# Remove the hour directory
COMIN="$ROTDIR/$CDUMP.$gymd/$ghh"
[[ -d $COMIN ]] && rm -rf $COMIN

# PREVIOUS 00Z day; remove the whole day
GDATE=$($NDATE -48 $CDATE)
gymd=$(echo $GDATE | cut -c1-8)
ghh=$(echo  $GDATE | cut -c9-10)

COMIN="$ROTDIR/$CDUMP.$gymd"
[[ -d $COMIN ]] && rm -rf $COMIN

# Remove archived stuff in $VFYARC that are (48+$FHMAX_GFS) hrs behind
# 1. atmospheric nemsio files used for fit2obs
if [ $CDUMP = "gfs" ]; then
    GDATE=$($NDATE -$FHMAX_GFS $GDATE)
    gymd=$(echo $GDATE | cut -c1-8)
    COMIN="$VFYARC/$CDUMP.$gymd"
    [[ -d $COMIN ]] && rm -rf $COMIN
fi

###############################################################
# Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
exit 0
