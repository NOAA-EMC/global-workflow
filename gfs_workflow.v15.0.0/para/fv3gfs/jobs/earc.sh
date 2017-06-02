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
## Ensemble archive driver script
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
###############################################################

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
cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`
APREFIX="${CDUMP}.t${chh}z."
ASUFFIX=".nemsio"

COMIN_ENS="$ROTDIR/enkf.$CDUMP.$cymd/$chh"

DATA="$RUNDIR/$CDATE/$CDUMP/earc"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA
cd $DATA

###############################################################
# Archive what is needed to restart the experiment
mkdir -p $DATA/enkf.${CDUMP}restart
cd $DATA/enkf.${CDUMP}restart

for imem in `seq 1 $NMEM_ENKF`; do

    memchar="mem"`printf %03i $imem`

    memdir="$COMIN_ENS/$memchar"
    tmpmemdir="$DATA/enkf.${CDUMP}restart/$memchar"

    mkdir -p $tmpmemdir
    cd $tmpmemdir

    restart_dir="$memdir/RESTART"
    if [ -d $restart_dir ]; then
        mkdir -p RESTART
        files=`ls -1 $restart_dir`
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

###############################################################
# Archive extra information that is good to have
mkdir -p $DATA/enkf.$CDUMP
cd $DATA/enkf.$CDUMP

# Ensemble mean related files
files="gsistat.ensmean cnvstat.ensmean enkfstat atmf006.ensmean.nc4 atmf006.ensspread.nc4"
for file in $files; do
    $NCP $COMIN_ENS/${APREFIX}$file .
done

# Ensemble member related files
files="gsistat cnvstat"
for imem in `seq 1 $NMEM_ENKF`; do

    memchar="mem"`printf %03i $imem`

    memdir="$COMIN_ENS/$memchar"
    tmpmemdir="$DATA/enkf.${CDUMP}/$memchar"

    mkdir -p $tmpmemdir

    for file in $files; do
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

###############################################################
# Clean up previous cycles; various depths
# PRIOR CYCLE: Leave the prior cycle alone
GDATE=`$NDATE -$assim_freq $CDATE`

# PREVIOUS to the PRIOR CYCLE
# Now go 2 cycles back and remove the directory
GDATE=`$NDATE -$assim_freq $GDATE`
gymd=`echo $GDATE | cut -c1-8`
ghh=`echo  $GDATE | cut -c9-10`

COMIN_ENS="$ROTDIR/enkf.$CDUMP.$gymd/$ghh"
[[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

# PREVIOUS day 00Z remove the whole day
GDATE=`$NDATE -48 $CDATE`
gymd=`echo $GDATE | cut -c1-8`
ghh=`echo  $GDATE | cut -c9-10`

COMIN_ENS="$ROTDIR/enkf.$CDUMP.$gymd"
[[ -d $COMIN_ENS ]] && rm -rf $COMIN_ENS

exit 0
