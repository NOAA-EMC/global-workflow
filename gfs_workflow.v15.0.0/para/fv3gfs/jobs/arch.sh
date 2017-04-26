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
cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`
export APREFIX="${CDUMP}.t${chh}z."
export ASUFFIX=".nemsio"

export COMIN="$ROTDIR/$CDUMP.$cymd/$chh"

export DATA="$RUNDIR/$CDATE/$CDUMP/arch"
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
    files=`ls -1 $restart_dir`
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
$NCP $COMIN/pgrbfanl.${CDUMP}.${CDATE}.grib2* .
[[ $CDUMP = "gfs" ]] && $NCP $COMIN/pgrbf*.${CDUMP}.${CDATE}.grib2* .

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
[[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
cd $ARCDIR

$NCP $COMIN/${APREFIX}gsistat gsistat.${CDUMP}.${CDATE}
$NCP $COMIN/pgrbfanl.${CDUMP}.${CDATE}.grib2* .
[[ $CDUMP = "gfs" ]] && $NCP $COMIN/pgrbf*.${CDUMP}.${CDATE}.grib2* .

###############################################################
# Clean up previous cycles; various depths
# PRIOR CYCLE: Leave the prior cycle alone
export GDATE=`$NDATE -$assim_freq $CDATE`

# PREVIOUS to the PRIOR CYCLE
export GDATE=`$NDATE -$assim_freq $GDATE`
gymd=`echo $GDATE | cut -c1-8`
ghh=`echo  $GDATE | cut -c9-10`

# Remove the TMPDIR directory
export COMIN="$RUNDIR/$GDATE"
[[ -d $COMIN ]] && rm -rf $COMIN

# Remove the COMROT directory
export COMIN="$ROTDIR/$CDUMP.$gymd/$ghh"
[[ -d $COMIN ]] && rm -rf $COMIN

# PREVIOUS 00Z day; remove the whole day
export GDATE=`$NDATE -48 $CDATE`
gymd=`echo $GDATE | cut -c1-8`
ghh=`echo  $GDATE | cut -c9-10`

export COMIN="$ROTDIR/$CDUMP.$gymd"
[[ -d $COMIN ]] && rm -rf $COMIN

###############################################################
# Exit out cleanly
exit 0
