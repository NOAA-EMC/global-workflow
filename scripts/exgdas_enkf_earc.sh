#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_earc.sh
# Script description:  Run ENKFGDAS EMC archive
#
# Author:        Lin Gan      Org: NCEP/EMC     Date: 2022-11-04
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

export QUEUE_ARCH=${QUEUE_ARCH:-${QUEUE_SERVICE}}
export ARCH_LIST=${ARCH_LIST:-$ROTDIR/enkf${CDUMP}.$PDY/$cyc/$COMPONENT/earc$ENSGRP}
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

##############################################
# ENSGRP > 0 archives a group of ensemble members
##############################################
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

   if [ $CDATE -gt $SDATE ]; then # Don't run for first half cycle

     export TRANSFER_TARGET_FILE=enkf${CDUMP}_grp${n}
     export TRANSFER_TARGET_FILE_2D=enkf${CDUMP}_grp${ENSGRP}
     $HOMEgfs/ush/hpss_global_archive_driver.sh
     status=$?
     if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
         echo "OFFLINE TAR $CDATE enkf${CDUMP}_grp${ENSGRP}.tar failed"
         exit $status
     fi

     if [ $SAVEWARMICA = "YES" -a $cyc -eq $EARCINC_CYC ]; then
       export TRANSFER_TARGET_FILE=enkf${CDUMP}_restarta_grp${n}
       export TRANSFER_TARGET_FILE_2D=enkf${CDUMP}_restarta_grp${ENSGRP}
       $HOMEgfs/ush/hpss_global_archive_driver.sh
       status=$?
       if [ $status -ne 0 ]; then
           echo "OFFLINE TAR $CDATE enkf${CDUMP}_restarta_grp${ENSGRP}.tar failed"
           exit $status
       fi
     fi

     if [ $SAVEWARMICB = "YES"  -a $cyc -eq $EARCICS_CYC ]; then
       export TRANSFER_TARGET_FILE=enkf${CDUMP}_restartb_grp${n}
       export TRANSFER_TARGET_FILE_2D=enkf${CDUMP}_restartb_grp${ENSGRP}
       $HOMEgfs/ush/hpss_global_archive_driver.sh
       status=$?
       if [ $status -ne 0 ]; then
           echo "OFFLINE TAR $CDATE enkf${CDUMP}_restartb_grp${ENSGRP}.tar failed"
           exit $status
       fi
     fi

   fi # CDATE>SDATE

fi


###################################################################
# ENSGRP 0 archives ensemble means and copy data to online archive
###################################################################
if [ $ENSGRP -eq 0 ]; then

    if [ $HPSSARCH = "YES" ]; then
        export TRANSFER_TARGET_FILE=enkf${CDUMP}
        $HOMEgfs/ush/hpss_global_archive_driver.sh
        status=$?
        if [ $status -ne 0  -a $CDATE -ge $firstday ]; then
            echo "OFFLINE TAR $CDATE enkf${CDUMP}.tar failed"
            exit $status
        fi
    fi

    #-- Archive online for verification and diagnostics
    [[ ! -d $ARCDIR ]] && mkdir -p $ARCDIR
    cd $ARCDIR

    if [[ ! $SDATE = $CDATE ]]; then
      $NCP $ROTDIR/enkf${CDUMP}.$PDY/$cyc/$COMPONENT/${CDUMP}.t${cyc}z.enkfstat         enkfstat.${CDUMP}.$CDATE
      $NCP $ROTDIR/enkf${CDUMP}.$PDY/$cyc/$COMPONENT/${CDUMP}.t${cyc}z.gsistat.ensmean  gsistat.${CDUMP}.${CDATE}.ensmean
    fi

    if [ $CDUMP_ENKF != "GDAS" ]; then
      $NCP $ROTDIR/enkfgfs.$PDY/$cyc/$COMPONENT/${CDUMP}.t${cyc}z.enkfstat         enkfstat.gfs.$CDATE
      $NCP $ROTDIR/enkfgfs.$PDY/$cyc/$COMPONENT/${CDUMP}.t${cyc}z.gsistat.ensmean  gsistat.gfs.${CDATE}.ensmean
    fi

fi

##############################################################
# ENSGRP 0 also does clean-up
###############################################################
if [ $ENSGRP -eq 0 -a $DELETE_COM_IN_ARCHIVE_JOB = "YES" -a $ROCOTO_WORKFLOW = "YES" ]; then

    # Start start and end dates to remove
    GDATEEND=$($NDATE -${RMOLDEND_ENKF:-24}  $CDATE)
    GDATE=$($NDATE -${RMOLDSTD_ENKF:-120} $CDATE)
    while [ $GDATE -le $GDATEEND ]; do

        gPDY=$(echo $GDATE | cut -c1-8)
        gcyc=$(echo $GDATE | cut -c9-10)

        # Loop over GDAS and GFS EnKF directories separately.
        clist="gdas gfs"
        for ctype in $clist; do
            COMIN_ENS="$ROTDIR/enkf$ctype.$gPDY/$gcyc/$COMPONENT"
            if [ -d $COMIN_ENS ]; then
                rocotolog="$EXPDIR/logs/${GDATE}.log"
                if [ -f $rocotolog ]; then
                    testend=$(tail -n 1 $rocotolog | grep "This cycle is complete: Success")
                    cycle_completed=$?
                    cycle_clean_up=0
                    if [ $HPSSARCH = "YES" ]; then
                      cd ${ROTDIR}/logs/${GDATE}
                      hpss_archive_files=`grep "Output sent to" *arc*.log|grep ${CDUMP}earc|awk '{print $4}'`
                      for file in $hpss_archive_files; do
                        hst=`grep "HTAR: HTAR SUCCESSFUL" $file|wc -l`
                        [[ $hst -eq 0 ]] && cycle_clean_up=1
                      done
                    fi
                    if [ $cycle_completed -eq 0 -a $cycle_clean_up -eq 0 ]; then
                        # Retain f006.ens files.  Remove everything else
                        for file in $(ls $COMIN_ENS | grep -v f006.ens); do
                            rm -rf $COMIN_ENS/$file
                        done
                    fi
                fi
            fi

            # Remove empty directories
            if [ -d $COMIN_ENS ] ; then
                [[ ! "$(ls -A $COMIN_ENS)" ]] && rm -rf $COMIN_ENS
            fi
        done

        # Advance to next cycle
        GDATE=$($NDATE +$assim_freq $GDATE)

    done

fi

# Remove enkf*.$rPDY for the older of GDATE or RDATE
GDATE=$($NDATE -${RMOLDSTD_ENKF:-120} $CDATE)
fhmax=$FHMAX_GFS
RDATE=$($NDATE -$fhmax $CDATE)
if [ $GDATE -lt $RDATE ]; then
    RDATE=$GDATE
fi
rPDY=$(echo $RDATE | cut -c1-8)
clist="gdas gfs"
for ctype in $clist; do
    COMIN="$ROTDIR/enkf$ctype.$rPDY"
    [[ -d $COMIN ]] && rm -rf $COMIN
done

###############################################################

echo "ENDED NORMALLY."

##########################################
# Remove the Temporary working directory
##########################################
cd $DATAROOT
[[ $KEEPDATA = "NO" ]] && rm -rf $DATA

date
exit 0
