#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_archive.sh
# Script description:  Run GDAS/GFS EMC archive
#
# Author:        Lin Gan      Org: NCEP/EMC     Date: 2022-10-24
#
# Abstract: This script configure archive and rapid submit thread of archive jobs
#
# Required module: prod_util, python
#
####
################################################################################

source "$HOMEgfs/ush/preamble.sh"

# Directories.
pwd=$(pwd)
COMIN_OBS=${COMIN_OBS:-$(compath.py prod/obsproc/${obsproc_ver})/$RUN.$PDY/$cyc/atmos}
COMIN=${COMINatmos:-"$ROTDIR/$CDUMP.$PDY/$cyc/atmos"}
cd $COMIN

# Utilities
export NCP=${NCP:-"/bin/cp -p"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}

# Initial exception handling
err=0
err_inc=0

###############################################################
# Perform on-line archiving
###############################################################

[[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
$NCP ${APREFIX}gsistat $ARCDIR/gsistat.${CDUMP}.${CDATE}
((errs + $?))
$NCP ${APREFIX}pgrb2.1p00.anl $ARCDIR/pgbanl.${CDUMP}.${CDATE}.grib2
((errs + $?))

# Archive 1 degree forecast GRIB2 files for verification
if [ $CDUMP = "gfs" ]; then
    fhmax=$FHMAX_GFS
    fhr=0
    while [ $fhr -le $fhmax ]; do
        fhr2=$(printf %02i $fhr)
        fhr3=$(printf %03i $fhr)
        $NCP ${APREFIX}pgrb2.1p00.f$fhr3 $ARCDIR/pgbf${fhr2}.${CDUMP}.${CDATE}.grib2
        ((errs + $?))
        (( fhr = $fhr + $FHOUT_GFS ))
    done
fi
if [ $CDUMP = "gdas" ]; then
    flist="000 003 006 009"
    for fhr in $flist; do
        fname=${APREFIX}pgrb2.1p00.f${fhr}
        fhr2=$(printf %02i $fhr)
        $NCP $fname $ARCDIR/pgbf${fhr2}.${CDUMP}.${CDATE}.grib2
        ((errs + $?))
    done
fi

if [ -s avno.t${cyc}z.cyclone.trackatcfunix ]; then
    PLSOT4=`echo $PSLOT|cut -c 1-4 |tr '[a-z]' '[A-Z]'`
    cat avno.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunix.${CDUMP}.$CDATE
    cat avnop.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunixp.${CDUMP}.$CDATE
fi

if [ $CDUMP = "gdas" -a -s gdas.t${cyc}z.cyclone.trackatcfunix ]; then
    PLSOT4=`echo $PSLOT|cut -c 1-4 |tr '[a-z]' '[A-Z]'`
    cat gdas.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunix.${CDUMP}.$CDATE
    cat gdasp.t${cyc}z.cyclone.trackatcfunix | sed s:AVNO:${PLSOT4}:g  > ${ARCDIR}/atcfunixp.${CDUMP}.$CDATE
fi

if [ $CDUMP = "gfs" ]; then
    $NCP storms.gfso.atcf_gen.$CDATE      ${ARCDIR}/.
    $NCP storms.gfso.atcf_gen.altg.$CDATE ${ARCDIR}/.
    $NCP trak.gfso.atcfunix.$CDATE        ${ARCDIR}/.
    $NCP trak.gfso.atcfunix.altg.$CDATE   ${ARCDIR}/.

    mkdir -p ${ARCDIR}/tracker.$CDATE/$CDUMP
    blist="epac natl"
    for basin in $blist; do
        cp -rp $basin                     ${ARCDIR}/tracker.$CDATE/$CDUMP
    done
fi

# Archive required gaussian gfs forecast files for Fit2Obs
if [ $CDUMP = "gfs" -a $FITSARC = "YES" ]; then
    VFYARC=${VFYARC:-$ROTDIR/vrfyarch}
    [[ ! -d $VFYARC ]] && mkdir -p $VFYARC
    mkdir -p $VFYARC/${CDUMP}.$PDY/$cyc
    prefix=${CDUMP}.t${cyc}z
    fhmax=${FHMAX_FITS:-$FHMAX_GFS}
    fhr=0
    while [[ $fhr -le $fhmax ]]; do
        fhr3=$(printf %03i $fhr)
        sfcfile=${prefix}.sfcf${fhr3}${ASUFFIX}
        sigfile=${prefix}.atmf${fhr3}${ASUFFIX}
        $NCP $sfcfile $VFYARC/${CDUMP}.$PDY/$cyc/
        ((errs + $?))
        $NCP $sigfile $VFYARC/${CDUMP}.$PDY/$cyc/
        ((errs + $?))
        (( fhr = $fhr + 6 ))
    done
fi

err_chk

###############################################################
# Archive data to HPSS
if [ $HPSSARCH = "YES" ]; then
###############################################################

#--determine when to save ICs for warm start and forecast-only runs
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


export ARCH_LIST="$COMIN/archlist"
[[ -d $ARCH_LIST ]] && rm -rf $ARCH_LIST
mkdir -p $ARCH_LIST
cd $ARCH_LIST

$HOMEgfs/ush/hpssarch_gen.sh $CDUMP
err=$?
if [ $err -ne 0  ]; then
    echo "$HOMEgfs/ush/hpssarch_gen.sh $CDUMP failed, ABORT!"
    err_chk
fi

cd $ROTDIR

if [ $CDUMP = "gfs" ]; then

    #Common gfsa gfsb - NOTE - do not check htar error status
    #  Some files not found can be acceptable
    for targrp in gfsa gfsb; do
        export TRANSFER_TARGET_FILE=$targrp
        $HOMEgfs/ush/hpss_global_archive_driver.sh
    done

    if [ ${ARCH_GAUSSIAN:-"NO"} = "YES" ]; then
        # Both cycled and fcst only
        for targrp in gfs_flux gfs_${format}b gfs_pgrb2b; do
            export TRANSFER_TARGET_FILE=$targrp
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE ${targrp}.tar"
                err_chk
            fi
        done
        # Only cycled
        if [ $MODE = "cycled" ]; then
            targrp="$targrp_list gfs_${format}a"
            export TRANSFER_TARGET_FILE=$targrp
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE ${targrp}.tar"
                err_chk
            fi
        fi
    fi

    # gfs wave
    if [ $DO_WAVE = "YES" -a "$WAVE_CDUMP" != "gdas" ]; then
        for targrp in gfswave; do
            export TRANSFER_TARGET_FILE=$targrp
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE ${targrp}.tar"
                err_chk
            fi
        done
    fi

    # gfs ocean
    if [ $DO_OCN = "YES" ]; then
        for targrp in ocn_ice_grib2_0p5 ocn_ice_grib2_0p25 ocn_2D ocn_3D ocn_xsect ocn_daily gfs_flux_1p00; do
            export TRANSFER_TARGET_FILE=$targrp
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE ${targrp}.tar"
                err_chk
            fi
        done
    fi

    # gfs ice
    if [ $DO_ICE = "YES" ]; then
        for targrp in ice; do
            export TRANSFER_TARGET_FILE=$targrp
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE ${targrp}.tar"
                err_chk
            fi
        done
    fi

    # gfs aerosol
    if [ $DO_AERO = "YES" ]; then
        for targrp in chem; do
            export TRANSFER_TARGET_FILE=$targrp
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE ${targrp}.tar"
                err_chk
            fi
        done
    fi

    # restarts
    if [ $SAVEFCSTIC = "YES" ]; then
        export TRANSFER_TARGET_FILE=gfs_restarta
        $HOMEgfs/ush/hpss_global_archive_driver.sh
        err=$?
        if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
            echo "Fail to create $CDATE gfs_restarta.tar"
            err_chk
        fi
    fi

    # downstream products
    if [ $DO_BUFRSND = "YES" -o $WAFSF = "YES" ]; then
        export TRANSFER_TARGET_FILE=gfs_downstream
        $HOMEgfs/ush/hpss_global_archive_driver.sh
        err=$?
        if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
            echo "Fail to create $CDATE gfs_downstream.tar"
            err_chk
        fi
    fi

fi


if [ $CDUMP = "gdas" ]; then

    #Common file
    export TRANSFER_TARGET_FILE=gdas
    $HOMEgfs/ush/hpss_global_archive_driver.sh
    err=$?
    if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
        echo "Fail to create $CDATE gdas.tar"
        err_chk
    fi

    #gdas wave
    if [ $DO_WAVE = "YES" ]; then
        export TRANSFER_TARGET_FILE=gdaswave
        $HOMEgfs/ush/hpss_global_archive_driver.sh
        err=$?
        if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
            echo "Fail to create $CDATE gdaswave.tar"
            err_chk
        fi
    fi

    # restart A
    if [ $SAVEWARMICA = "YES" -o $SAVEFCSTIC = "YES" ]; then
        export TRANSFER_TARGET_FILE=gdas_restarta
        $HOMEgfs/ush/hpss_global_archive_driver.sh
        err=$?
        if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
            echo "Fail to create $CDATE gdas_restarta.tar"
            err_chk
        fi
        if [ $DO_WAVE = "YES" ]; then
            export TRANSFER_TARGET_FILE=gdaswave_restart
            $HOMEgfs/ush/hpss_global_archive_driver.sh
            err=$?
            if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
                echo "Fail to create $CDATE gdaswave_restart.tar"
                err_chk
            fi
        fi
    fi

    # restart B
    if [ $SAVEWARMICB = "YES" -o $SAVEFCSTIC = "YES" ]; then
        export TRANSFER_TARGET_FILE=gdas_restartb
        $HOMEgfs/ush/hpss_global_archive_driver.sh
        err=$?
        if [ $err -ne 0  -a $CDATE -ge $firstday ]; then
            echo "Fail to create $CDATE gdas_restartb.tar"
            err_chk
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

###############################################################
# Clean up COM
###############################################################
if [[ "${DELETE_COM_IN_ARCHIVE_JOB:-YES}" == NO -o "${ROCOTO_WORKFLOW:-YES}" == NO ]] ; then
    exit 0
fi

# Step back every assim_freq hours and remove old rotating directories
# for successful cycles (defaults from 24h to 120h).  If GLDAS is
# active, retain files needed by GLDAS update.  Independent of GLDAS,
# retain files needed by Fit2Obs
DO_GLDAS=${DO_GLDAS:-"NO"}
GDATEEND=$($NDATE -${RMOLDEND:-24}  $CDATE)
GDATE=$($NDATE -${RMOLDSTD:-120} $CDATE)
GLDAS_DATE=$($NDATE -96 $CDATE)
RTOFS_DATE=$($NDATE -48 $CDATE)
while [ $GDATE -le $GDATEEND ]; do
    gPDY=$(echo $GDATE | cut -c1-8)
    gcyc=$(echo $GDATE | cut -c9-10)
    COMIN="$ROTDIR/${CDUMP}.$gPDY/$gcyc/atmos"
    COMINwave="$ROTDIR/${CDUMP}.$gPDY/$gcyc/wave"
    COMINrtofs="$ROTDIR/rtofs.$gPDY"
    if [ -d $COMIN ]; then
        rocotolog="$EXPDIR/logs/${GDATE}.log"
        if [ -f $rocotolog ]; then
            testend=$(tail -n 1 $rocotolog | grep "This cycle is complete: Success")
            rc=$?
            if [ $rc -eq 0 ]; then
                if [ -d $COMINwave ]; then rm -rf $COMINwave ; fi
                if [ -d $COMINrtofs -a $GDATE -lt $RTOFS_DATE ]; then rm -rf $COMINrtofs ; fi
                if [ $CDUMP != "gdas" -o $DO_GLDAS = "NO" -o $GDATE -lt $GLDAS_DATE ]; then
                    if [ $CDUMP = "gdas" ]; then
                        for file in $(ls $COMIN |grep -v prepbufr |grep -v cnvstat |grep -v atmanl.nc); do
                            rm -rf $COMIN/$file
                        done
                    else
                        rm -rf $COMIN
                    fi
                else
                    if [ $DO_GLDAS = "YES" ]; then
                        for file in $(ls $COMIN |grep -v sflux |grep -v RESTART |grep -v prepbufr |grep -v cnvstat |grep -v atmanl.nc); do
                            rm -rf $COMIN/$file
                        done
                        for file in $(ls $COMIN/RESTART |grep -v sfcanl ); do
                            rm -rf $COMIN/RESTART/$file
                        done
                    else
                        for file in $(ls $COMIN |grep -v prepbufr |grep -v cnvstat |grep -v atmanl.nc); do
                            rm -rf $COMIN/$file
                        done
                    fi
                fi
            fi
        fi
    fi

    # Remove any empty directories
    if [ -d $COMIN ]; then
        [[ ! "$(ls -A $COMIN)" ]] && rm -rf $COMIN
    fi

    if [ -d $COMINwave ]; then
        [[ ! "$(ls -A $COMINwave)" ]] && rm -rf $COMINwave
    fi

    GDATE=$($NDATE +$assim_freq $GDATE)
done

# Remove archived gaussian files used for Fit2Obs in $VFYARC that are
# $FHMAX_FITS plus a delta before $CDATE.  Touch existing archived
# gaussian files to prevent the files from being removed by automatic
# scrubber present on some machines.

if [ $CDUMP = "gfs" ]; then
    fhmax=$((FHMAX_FITS+36))
    RDATE=$($NDATE -$fhmax $CDATE)
    rPDY=$(echo $RDATE | cut -c1-8)
    COMIN="$VFYARC/$CDUMP.$rPDY"
    [[ -d $COMIN ]] && rm -rf $COMIN

    TDATE=$($NDATE -$FHMAX_FITS $CDATE)
    while [ $TDATE -lt $CDATE ]; do
        tPDY=$(echo $TDATE | cut -c1-8)
        tcyc=$(echo $TDATE | cut -c9-10)
        TDIR=$VFYARC/$CDUMP.$tPDY/$tcyc
        [[ -d $TDIR ]] && touch $TDIR/*
        TDATE=$($NDATE +6 $TDATE)
    done
fi

# Remove $CDUMP.$rPDY for the older of GDATE or RDATE
GDATE=$($NDATE -${RMOLDSTD:-120} $CDATE)
fhmax=$FHMAX_GFS
RDATE=$($NDATE -$fhmax $CDATE)
if [ $GDATE -lt $RDATE ]; then
    RDATE=$GDATE
fi
rPDY=$(echo $RDATE | cut -c1-8)
COMIN="$ROTDIR/$CDUMP.$rPDY"
[[ -d $COMIN ]] && rm -rf $COMIN

###############################################################

echo "ENDED NORMALLY."

##########################################

