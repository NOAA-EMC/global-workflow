#!/bin/ksh
set -ux
################################################################################
# This script runs the verification step.
# Usage: vrfy.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
################################################################################

set -a;. $CONFIG;set +a
echo "-----end of $CONFIG ------------"
echo

export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export NCP=${NCP:-/bin/cp}
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
chmod ${permission:-750} $DATA
#
export BASEDIR=${BASEDIR:-/nwprod}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export SCRDIR=${SCRDIR:-$BASEDIR/scripts}
export JOBSDIR=${JOBSDIR:-$BASEDIR/jobs}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export NWPROD=${NWPROD:-$BASEDIR}
export HOMEcfs=${HOMEcfs:-$BASEDIR}
export USHcfs=${USHcfs:-$USHDIR}
export COMROT=${MEMDIR:-$COMROT}
#
PBEG=${PBEG:-$SHDIR/pbeg}
PEND=${PEND:-$SHDIR/pend}
PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables
export DISK_GLOB=${DISK_GLOB:-/global/save}
export SUB=${SUB:-$SHDIR/sub}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export PREPQFITSH=${PREPQFITSH:-$USHDIR/global_prepqfit.sh}
export SAVEFITSSH=${SAVEFITSSH:-$USHDIR/global_savefits.sh}
export TRACKERSH=${TRACKERSH:-$USHDIR/global_tracker.sh}
#
export PRPX=${PRPX:-$NWPROD/exec/prepobs_prepdata}
export PREX=${PREX:-$NWPROD/exec/prepobs_prevents}
export PSTX=${PSTX:-$NWPROD/exec/global_postevents}
export SYNDX=${SYNDX:-$NWPROD/exec/syndat_syndata}
export PRVT=${PRVT:-$BASEDIR/fix/fix_prep/prepobs_errtable.global}
#
export STORMPSH=${STORMPSH:-$USHDIR/stormp.sh}
export VSDBSH=${VSDBSH:-$USHDIR/vsdbjob.sh}
export VRFYRADSH=${VRFYRADSH:-$USHDIR/VrfyRad_glbl.sh}
export VRFYOZNSH=${VRFYOZNSH:-$USHDIR/VrfyOzn_glbl.sh}
export VRFYMINSH=${VRFYMINSH:-$USHDIR/MinMon_DE.sh}
export VRFYFITS=${VRFYFITS:-NO}
export SAVEFITS=${SAVEFITS:-NO}
export VRFYPRCP=${VRFYPRCP:-NO}
export VRFYRAD=${VRFYRAD:-NO}
export VRFYOZN=${VRFYOZN:-NO}
export VRFYMINMON=${VRFYMINMON:-NO}
export SAVERAD=${SAVERAD:-NO}
export VRFYTRAK=${VRFYTRAK:-NO}
export VRFYGENESIS=${VRFYGENESIS:-NO}
export VRFYGMPK=${VRFYGMPK:-NO}
export VSDB_STEP1=${VSDB_STEP1:-NO}
export VSDB_STEP2=${VSDB_STEP2:-NO}
export webhost=${webhost:-rzdm.ncep.noaa.gov}
export webhostid=${webhostid:-$LOGNAME}
export SEND2WEB=${SEND2WEB:-NO}
export WEBDIR=${WEBDIR:-/home/people/emc/www/htdocs/gmb/$webhostid}
ARCHDAY=${ARCHDAY:-0}       # impacts delay for online archive only
BACKDATE=$((ARCHDAY*24))    # impacts delay for online archive only
BACKDATEVSDB=$((BACKDATE+24))
#
[[ $SAVEFITS = YES ]]&&VRFYFITS=YES
export CYINC=${CYINC:-06}
export CDFNL=${CDFNL:-gdas}
export VDUMP=${VDUMP:-gfs}
export GDUMP=${GDUMP:-$CDFNL}
export fdump=${fdump:-gfs}
export adump=${adump:-gdas}
export GDATE=$($NDATE -$CYINC $CDATE)
export COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
p=pr$(echo $PSLOT|tr '[A-Z]' '[a-z]')
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
nknd=${CKND:-1}

export FHMAX2=${fmax2:-384}                                            
if [ $fseg -eq 1 -o $CDUMP = gdas ]; then export FHMAX2=$FHMAX ; fi
export FHMAX2=$(((FHMAX2/24)*24))
export FHOUT=${FHOUT:-6}                                                 

################################################################################
# If requested, make a subset of 0.25-deg grib2 data for NHC
NHCOUTPUT=${NHCOUTPUT:-/global/noscrub/$LOGNAME/nhc_subset}
NHCSUBSET_LIST=${NHCSUBSET_LIST:-/global/save/emc.glopara/misc/subset4nhc_list.txt}
NHCSUBSETSH=${NHCSUBSETSH:-/global/save/emc.glopara/misc/extract_grib2_nhc.sh}
if [ ${DO_NHCSUBSET:-NO} = YES -a $CDUMP = gfs ] ; then

##$NHCSUBSETSH $COMROT $NHCOUTPUT $NHCSUBSET_LIST $CDATE $CDUMP 120
  export NHCSUBSET_LIST=$NHCSUBSET_LIST_REAL
  export NHCSUBSET_PGB=${NHCSUBSET_PGB:-pgbq}
  export NHCSUBSET_FHOUT=${NHCSUBSET_FHOUT:-6}
  export NHCSUBSET_FHMAX=${NHCSUBSET_FHMAX:-180}
  export PGB_FORMAT=${PGB_FORMAT:-2}
# $NHCSUBSETSH $COMROT $NHCOUTPUT $NHCSUBSET_LIST $NHCSUBSET_PGB $NHCSUBSET_FHOUT $NHCSUBSET_FHMAX $PGB_FORMAT $CDATE $CDUMP 120
  $NHCSUBSETSH $COMROT $NHCOUTPUT $NHCSUBSET_LIST $NHCSUBSET_PGB $NHCSUBSET_FHOUT $NHCSUBSET_FHMAX $PGB_FORMAT $CDATE $CDUMP 87600

  export NHCSUBSET_LIST=$NHCSUBSET_LIST_RETRO
  export NHCSUBSET_PGB=pgrbh
  export NHCSUBSET_FHOUT=6
  export NHCSUBSET_FHMAX=168
  export PGB_FORMAT=3
  $NHCSUBSETSH $COMROT $NHCOUTPUT $NHCSUBSET_LIST $NHCSUBSET_PGB $NHCSUBSET_FHOUT $NHCSUBSET_FHMAX $PGB_FORMAT $CDATE $CDUMP 87600
fi

################################################################################
for cdm in $(eval echo $COMDMP|tr , ' ') ; do
 if [[ -s $cdm/tcvitl.$CDUMP.$CDATE ]] ; then
  $NCP $cdm/tcvitl.$CDUMP.$CDATE  $DATA/tcvitl.$CDUMP.$CDATE
 fi
done
for cdm in $(eval echo $COMDMPG|tr , ' ') ; do
 if [[ -s $cdm/tcvitl.$GDUMP.$GDATE ]] ; then
  $NCP $cdm/tcvitl.$GDUMP.$GDATE $DATA/tcvitl.$GDUMP.$GDATE
 fi
done

################################################################################
# Make observation fit files.
if [[ $VRFYFITS = YES && $CDUMP = $CDFNL ]] ; then
  if [ $nknd -gt 1 ] ; then
    export CDUMPANAL=${CDFNL}$nknd
    export CDUMPFCST=${VDUMP}$nknd
  else
    export CDUMPFCST=$VDUMP
  fi
  export TMPDIR=$PTMP/$LOGNAME/tmpdir
  if [[ ! -d $TMPDIR ]] ; then 
     mkdir -p $TMPDIR
  fi
  if [ $machine = IBMP6 -o $machine = THEIA -o $machine = GAEA ] ; then
    $PREPQFITSH $CDATE $COMROT $DATA
  else
    $PREPQFITSH pr$PSLOT $CDATE $COMROT $ARCDIR $TMPDIR
  fi
fi
  
################################################################################
# Fit to obs archive.
if [[ $SAVEFITS = YES && $CDUMP = $CDFNL ]];then
  export CDATE=$CDATE
  export EXP=$p
  export COMOUT=$COMROT
  export HORZ_DIR=$HORZ_DIR
  export FIT_DIR=$FIT_DIR
  $SAVEFITSSH

#
  if [ $CYINC -gt 12 ] ; then
    CDATE12=$(echo $CDATE | cut -c1-8)12          ### <------ replace 00Z to 12Z
    EXP=$p COMOUT=$COMROT
    $SUB -a $ACCOUNT -e 'CDATE12=$CDATE12,EXP=$EXP,COMOUT=$COMOUT,FIT_DIR=$FIT_DIR,HORZ_DIR=$HORZ_DIR,EXECDIR_FITS=$EXECDIR_FITS' -j savefits.$PSLOT.$CDATE12 -p 1/1 -o $DATA/savefitsm.$PSLOT.$CDATE12 $SAVEFITSSH
  fi
  cd $DATA
fi

################################################################################
# Extract radiance data files and run integrity checks

if [[ $VRFYRAD = YES && $CDUMP = $CDFNL ]]; then

  EXP=$p COMOUT=$COMROT
  if [ $machine = IBMP6 ] ; then
    $SUB -a $ACCOUNT  -g $GROUP -e 'CDATE,EXP,COMOUT,VRFYRAD_DIR' -j vrfyrad.$PSLOT.$CDATE -o $DATA/vrfyrad.$PSLOT.$CDATE $VRFYRADSH
  else
##  $SUB -a $ACCOUNT -e 'CDATE=$CDATE,EXP=$EXP,COMOUT=$COMOUT,VRFYRAD_DIR=$VRFYRAD_DIR' -j vrfyrad.$PSLOT.$CDATE -p 1/1 -q service -o $DATA/vrfyrad.$PSLOT.$CDATE $VRFYRADSH
    export CDATE=$CDATE
    export EXP=$p
    export COMOUT=$COMROT
    export VRFYRAD_DIR=$VRFYRAD_DIR
    export biascr=$COMROT/biascr.$CDUMP.$CDATE
    export radstat=$COMROT/radstat.$CDUMP.$CDATE
    export DATA_IN=$STMP/$LOGNAME/radmon.$PSLOT.$CDATE
    mkdir -p $PTMP/$LOGNAME/logs
    export jlogfile=$PTMP/$LOGNAME/logs/radmon_${PSLOT}.$CDUMP.$CDATE.log
    $VRFYRADSH
  fi
fi


################################################################################
# Extract ozone data files

if [[ $VRFYOZN = YES && $CDUMP = $CDFNL ]]; then
  list="ges anl"
  for ozntype in $list; do
     $VRFYOZNSH $PSLOT $ozntype $CDATE $COMROT $OZNDIR/$ozntype
  done
fi


################################################################################
# Run Minimization Monitoring scripts

if [[ $VRFYMINMON = YES ]]; then
  export gsistat=$COMROT/gsistat.$CDUMP.$CDATE
  export DATA_IN=$STMP/$LOGNAME/minmon.$PSLOT.$CDATE
  mkdir -p $PTMP/$LOGNAME/logs
  export jlogfile=$PTMP/$LOGNAME/logs/minmon_${PSLOT}.$CDUMP.$CDATE.log
  $VRFYMINSH
fi


################################################################################
# Make tracks.
if [[ $VRFYTRAK = YES ]];then
  if [[ $nknd -gt 1 ]] ; then
    $TRACKERSH $CDATE $CDUMP $COMROT $DATA $nknd
  else
    $TRACKERSH $CDATE $CDUMP $COMROT $DATA
  fi
fi



################################################################################
# Make tracks.
if [ $VRFYGENESIS = YES -a $CDUMP = gfs ] ; then
   $GENESISSH $CDATE $CDUMP $COMROT $DATA
fi

################################################################################
# Make gempak.
if [[ $VRFYGMPK = YES && $LOGNAME = glopara && $PSLOT = X ]];then
  RX=MRF$PSLOT
  export AWIPSSH=${AWIPSSH:-$DISK_GLOB/wx20mi/para/nawips.sh}
  $SUB -j nawips.$RX.$CDATE -o $STMP/glopara/nawips.$RX.$CDATE.out $AWIPSSH $CDATE $RX $PARROT
fi


################################################################################
#   Run VSDB step1 script here

export vsdbsave=${vsdbsave:-$DISK_GLOB/$LOGNAME/archive/vsdb_data} ;#place where vsdb database is saved
export vsdbhome=${vsdbhome:-$BASEDIR/vsdb}
export mdlist=${mdlist:-""}
VSDB_START_DATE=${VSDB_START_DATE:-$CDATE}
anltype=${anltype:-gfs}
vlength=${vlength:-$FHMAX2}
vhr_rain=${vhr_rain:-$FHMAX}
xdate=$(echo $($NDATE -$BACKDATEVSDB $CDATE) | cut -c1-8)
if [ $CDUMP = gfs  ] ; then
 if [ $VRFYPRCP = YES -o $VSDB_STEP1 = YES ] ; then
  export gfs_cyc=${gfs_cyc:-1}
  $VSDBSH $xdate $xdate $vlength $cycle pr$PSLOT $VSDB_START_DATE $anltype $gfs_cyc
 fi
fi

################################################################################
# Exit gracefully
rc=0
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND
