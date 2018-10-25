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
ARCHICS_CYC=$((ARCH_CYC-assim_freq))
if [ $ARCHICS_CYC -lt 0 ]; then
    ARCHICS_CYC=$((ARCHICS_CYC+24))
fi

# CURRENT CYCLE
APREFIX="${CDUMP}.t${cyc}z."
ASUFFIX=".nemsio"

# Realtime parallels run GFS MOS on 1 day delay
# If realtime parallel, back up CDATE_MOS one day
CDATE_MOS=$CDATE
if [ $REALTIME = "YES" ]; then
    CDATE_MOS=$($NDATE -24 $CDATE)
fi
PDY_MOS=$(echo $CDATE_MOS | cut -c1-8)

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

if [ -s avno.t${cyc}z.cyclone.trackatcfunix ]; then
    PLSOT4=`echo $PSLOT|cut -c 1-4 |tr '[a-z]' '[A-Z]'`
    cat avno.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunix.${CDUMP}.$CDATE
    cat avnop.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunixp.${CDUMP}.$CDATE
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

#--determine when to save ICs for warm start and forecat-only runs 
SAVEWARMICA="NO"
SAVEWARMICB="NO"
SAVEFCSTIC="NO"
firstday=$($NDATE +24 $SDATE)
mm=`echo $CDATE|cut -c 5-6`
dd=`echo $CDATE|cut -c 7-8`
nday=$(( (mm-1)*30+dd ))
mod=$(($nday % $ARCH_WARMICFREQ))
if [ $CDATE -eq $firstday -a $cyc -eq $ARCHINC_CYC ]; then SAVEWARMICA="YES" ; fi
if [ $CDATE -eq $firstday -a $cyc -eq $ARCHICS_CYC ]; then SAVEWARMICB="YES" ; fi
if [ $mod -eq 0 -a $cyc -eq $ARCHINC_CYC ]; then SAVEWARMICA="YES" ; fi
if [ $mod -eq 0 -a $cyc -eq $ARCHICS_CYC ]; then SAVEWARMICB="YES" ; fi

if [ $ARCHICS_CYC -eq 18 ]; then
    nday1=$((nday+1))
    mod1=$(($nday1 % $ARCH_WARMICFREQ))
    if [ $mod1 -eq 0 -a $cyc -eq $ARCHICS_CYC ] ; then SAVEWARMICB="YES" ; fi
    if [ $mod1 -ne 0 -a $cyc -eq $ARCHICS_CYC ] ; then SAVEWARMICB="NO" ; fi
    if [ $CDATE -eq $SDATE -a $cyc -eq $ARCHICS_CYC ] ; then SAVEWARMICB="YES" ; fi
fi

mod=$(($nday % $ARCH_FCSTICFREQ))
if [ $mod -eq 0 -o $CDATE -eq $firstday ]; then SAVEFCSTIC="YES" ; fi


ARCH_LIST="$COMIN/archlist"
[[ -d $ARCH_LIST ]] && rm -rf $ARCH_LIST
mkdir -p $ARCH_LIST
cd $ARCH_LIST

$HOMEgfs/ush/hpssarch_gen.sh $CDUMP
status=$?
if [ $status -ne 0  ]; then
    echo "$HOMEgfs/ush/hpssarch_gen.sh $CDUMP failed, ABORT!"
    exit $status
fi

cd $ROTDIR

if [ $CDUMP = "gfs" ]; then

    #for targrp in gfsa gfsb - NOTE - do not check htar error status
    for targrp in gfsa gfsb; do
        htar -P -cvf $ATARDIR/$CDATE/${targrp}.tar `cat $ARCH_LIST/${targrp}.txt`
    done

    #for targrp in gfs_flux gfs_nemsio gfs_pgrb2b; do
    for targrp in gfs_flux gfs_nemsioa gfs_nemsiob; do
        htar -P -cvf $ATARDIR/$CDATE/${targrp}.tar `cat $ARCH_LIST/${targrp}.txt`
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "HTAR $CDATE ${targrp}.tar failed"
            exit $status
        fi
    done
    
    if [ $SAVEFCSTIC = "YES" ]; then
        htar -P -cvf $ATARDIR/$CDATE/gfs_restarta.tar `cat $ARCH_LIST/gfs_restarta.txt`
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "HTAR $CDATE gfs_restarta.tar failed"
            exit $status
        fi
    fi

   #--save mdl gfsmos output from all cycles in the 18Z archive directory
   if [ -d gfsmos.$PDY_MOS -a $cyc -eq 18 ]; then
        htar -P -cvf $ATARDIR/$CDATE_MOS/gfsmos.tar ./gfsmos.$PDY_MOS
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "HTAR $CDATE gfsmos.tar failed"
            exit $status
        fi
   fi

fi


if [ $CDUMP = "gdas" ]; then

    htar -P -cvf $ATARDIR/$CDATE/gdas.tar `cat $ARCH_LIST/gdas.txt`
    status=$?
    if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
        echo "HTAR $CDATE gdas.tar failed"
        exit $status
    fi

    if [ $SAVEWARMICA = "YES" -o $SAVEFCSTIC = "YES" ]; then
        htar -P -cvf $ATARDIR/$CDATE/gdas_restarta.tar `cat $ARCH_LIST/gdas_restarta.txt`
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "HTAR $CDATE gdas_restarta.tar failed"
            exit $status
        fi
    fi
    if [ $SAVEWARMICB = "YES" -o $SAVEFCSTIC = "YES" ]; then
        htar -P -cvf $ATARDIR/$CDATE/gdas_restartb.tar `cat $ARCH_LIST/gdas_restartb.txt`
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
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

# Step back every assim_freq hours
# and remove old rotating directories for successful cycles
# defaults from 24h to 120h
GDATEEND=$($NDATE -${RMOLDEND:-24}  $CDATE)
GDATE=$(   $NDATE -${RMOLDSTD:-120} $CDATE)
while [ $GDATE -le $GDATEEND ]; do
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)
    COMIN="$ROTDIR/$CDUMP.$gPDY/$gcyc"
    if [ -d $COMIN ]; then
        rocotolog="$EXPDIR/logs/${GDATE}.log"
	if [ -f $rocotolog ]; then
            testend=$(tail -n 1 $rocotolog | grep "This cycle is complete: Success")
            rc=$?
            [[ $rc -eq 0 ]] && rm -rf $COMIN
	fi
    fi

    # Remove any empty directories
    COMIN="$ROTDIR/$CDUMP.$gPDY"
    if [ -d $COMIN ]; then
        [[ ! "$(ls -A $COMIN)" ]] && rm -rf $COMIN
    fi

    # Remove mdl gfsmos directory
    if [ $CDUMP = "gfs" ]; then
	COMIN="$ROTDIR/gfsmos.$gPDY"
	if [ -d $COMIN -a $GDATE -lt $CDATE_MOS ]; then rm -rf $COMIN ; fi
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
exit 0
