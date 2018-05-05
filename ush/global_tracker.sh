#! /bin/ksh
set -x

export CDATE=${1:-?}
export CDUMP=${2:-?}
export COMOUT=${3:-?}
export DATA=${4:-?}
export nknd=${5:-0}

export PSLOT=${PSLOT:-x}
export FHMAX2=${FHMAX2:-252}
export FHOUT=${FHOUT:-3}
user=$LOGNAME

# Load production utility module.  Used by PARATRKR
shell=${SHELL:-ksh}
export machine=${machine:-WCOSS_C}
if [ $machine = WCOSS_C ]; then
# . $MODULESHOME/init/sh      2>>/dev/null
# module load iobuf/2.0.5     2>>/dev/null
 export IOBUF_PARAMS="*:size=32M:count=4:verbose"
 export APRUNTRACK="aprun -j1 -n1 -N1 -d1"
elif [ $machine = WCOSS_DELL_P3 ]; then
 export IOBUF_PARAMS="*:size=32M:count=4:verbose"
 export APRUNTRACK="mpirun -n 1 "
fi
#
export HOMEDIR=$HOMEgfs
export USHDIR=${USHDIR:-$HOMEDIR/ush}
export archsyndir=${archsyndir:-$COMROOTp1/arch/prod/syndat}
export HOMERELO=${HOMERELO:-$HOMEDIR}
FIXRELO=${FIXRELO:-${HOMERELO}/fix}
GRIBVERSION=${GRIBVERSION:-"grib2"}
#
export GETTRKEXEC=${GETTRKEXEC:-$HOMEDIR/exec/gettrk}
export inpdate=$CDATE
export paradir=$COMIN
export prxtrak=$DATA
export vdir=$DATA

if [ ! -d ${vdir} ]; then
  mkdir -p ${vdir}
fi
if [ ! -d ${prxtrak} ]; then
  mkdir -p ${prxtrak}
fi

cd $vdir

# Always keep cmodel as "para"

export cmodel=para
if [ "$GRIBVERSION" = "grib1" ]; then
  export gribver=1
else
  export gribver=2
fi

# TRACKID can be changed to "parx", "pary", "parw", etc....

#
nn=$((`echo $PSLOT | wc -c`-1))
if [ $nn -eq 1 ] ; then
 export TRACKID=`echo par$PSLOT | cut -c1-4`
else
# export TRACKID=pr$PSLOT
 export TRACKID=`echo pr$PSLOT | cut -c1-4`
fi


export YYYYMMDDHH=${inpdate}
export PDY=` echo ${YYYYMMDDHH} | cut -c1-8`
export cyc=` echo ${YYYYMMDDHH} | cut -c9-10`
if [[ -r tcvitl.$CDUMP.$CDATE ]]; then
  export AUXTCVIT=$DATA/auxtcvit.$CDATE
  export GDATE=$($NDATE -06 $CDATE)
  cat tcvitl.gdas.$GDATE tcvitl.$CDUMP.$CDATE >$AUXTCVIT
else
  export AUXTCVIT=JUNK_NOFILE
fi

if [ $nknd -gt 1 ] ; then
 export CDUMP=${CDUMP}${nknd}
fi

gfstrackhour1=180; gfstrackhour2=252
if [ $gfstrackhour1 -gt $FHMAX2 ]; then gfstrackhour1=$FHMAX2 ; fi
if [ $gfstrackhour2 -gt $FHMAX2 ]; then gfstrackhour2=$FHMAX2 ; fi

if [[ $CDUMP = gdas ]]; then
    ${APRUNTRACK} ${PARATRKR:-$USHDIR/global_extrkr.sh} --gdas-last-hour 9
    cp $DATA/trak.$TRACKID.atcfunix.$CDATE $COMOUT/atcfunix.$CDUMP.$CDATE
else
    # Run first tracker for 180 hours for NHC/JTWC operational forecast:
    ${APRUNTRACK} ${PARATRKR:-$USHDIR/global_extrkr.sh} --gfs-last-hour $gfstrackhour1 --wait-for-data 900
    cp $DATA/trak.$TRACKID.atcfunix.$CDATE $COMOUT/atcfunix.$CDUMP.$CDATE
    if [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
     # Run a second tracker for 252 hours for experimental ten day forecasts:
     if [ $gfstrackhour2 -gt $gfstrackhour1 ]; then;
      export SENDNHC=NO  
      ${APRUNTRACK} ${PARATRKR:-$USHDIR/global_extrkr.sh} --gfs-last-hour $gfstrackhour2 --wait-for-data 900
      cp $DATA/trak.$TRACKID.atcfunix.$CDATE $COMOUT/atcf252hr.$CDUMP.$CDATE
     fi
    fi
fi


