#!/bin/ksh
###################################################################
echo "----------------------------------------------------"
echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
echo "----------------------------------------------------"
echo "History: Mar 2000 - First implementation of this new script."
echo "S Lilly: May 2008 - add logic to make sure that all of the "
echo "                    data produced from the restricted ECMWF"
echo "                    data on the CCS is properly protected."
#####################################################################

set -xa

cd $DATA
RUN=$1
fend=$2
DBN_ALERT_TYPE=$3

export 'PS4=$RUN:$SECONDS + '

DATA_RUN=$DATA/$RUN
mkdir -p $DATA_RUN
cd $DATA_RUN

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

cp $FIXgempak/g2varswmo2.tbl g2varswmo2.tbl
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File g2varswmo2.tbl file is missing."
   exit $err
fi
cp $FIXgempak/g2vcrdwmo2.tbl g2vcrdwmo2.tbl
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File g2vcrdwmo2.tbl file is missing."
   exit $err
fi

cp $FIXgempak/g2varsncep1.tbl g2varsncep1.tbl
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File g2varsncep1.tbl file is missing."
   exit $err
fi

cp $FIXgempak/g2vcrdncep1.tbl g2vcrdncep1.tbl
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File g2vcrdncep1.tbl file is missing."
   exit $err
fi

#
NAGRIB=$GEMEXE/nagrib2_nc
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File $GEMEXE/nagrib2_nc is missing."
   echo " WARNING: module GEMPAK was not loaded"
   exit $err
fi

cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no

maxtries=180
fhcnt=$fstart
while [ $fhcnt -le $fend ] ; do
  typeset -Z3 fhr

  fhr=$fhcnt
  fhcnt3=`expr $fhr % 3`

  fhr3=$fhcnt
  typeset -Z3 fhr3

  GEMGRD=${RUN}_${PDY}${cyc}f${fhr3}

  if [ $RUN = "gdas_0p25" ]; then 
    export GRIBIN=$COMIN/${model}.${cycle}.pgrb2.0p25.f${fhr}
    if [ ! -f $GRIBIN ] ; then
       echo "WARNING: $GRIBIN FILE is missing"
       msg=" $GRIBIN file is missing "
       postmsg "$jlogfile" "$msg"
    fi
    GRIBIN_chk=$COMIN/${model}.${cycle}.pgrb2.0p25.f${fhr}.idx
  else
    export GRIBIN=$COMIN/${model}.${cycle}.pgrb2.1p00.f${fhr}
    if [ ! -f $GRIBIN ] ; then
       echo "WARNING: $GRIBIN FILE is missing"
       msg=" $GRIBIN file is missing "
       postmsg "$jlogfile" "$msg"
    fi
    GRIBIN_chk=$COMIN/${model}.${cycle}.pgrb2.1p00.f${fhr}.idx
  fi

  icnt=1
  while [ $icnt -lt 1000 ]
  do
    if [ -r $GRIBIN_chk ] ; then
      sleep 5
      break
    else
      msg="The process is waiting ... ${GRIBIN_chk} file to proceed."
      postmsg "${jlogfile}" "$msg"
      sleep 20
      let "icnt=icnt+1"
    fi
    if [ $icnt -ge $maxtries ]
    then
      msg="ABORTING: after 1 hour of waiting for ${GRIBIN_chk} file at F$fhr to end."
      postmsg "${jlogfile}" "$msg"
      export err=7 ; err_chk
      exit $err
    fi
  done

  cp $GRIBIN grib$fhr

  export pgm="nagrib2 F$fhr"
  startmsg

   $NAGRIB << EOF
   GBFILE   = grib$fhr
   INDXFL   = 
   GDOUTF   = $GEMGRD
   PROJ     = $proj
   GRDAREA  = $grdarea
   KXKY     = $kxky
   MAXGRD   = $maxgrd
   CPYFIL   = $cpyfil
   GAREA    = $garea
   OUTPUT   = $output
   GBTBLS   = $gbtbls
   GBDIAG   = 
   PDSEXT   = $pdsext
  l
  r
EOF
  export err=$?;err_chk

  if [ $SENDCOM = "YES" ] ; then
     cp $GEMGRD $COMOUT/.$GEMGRD
     export err=$?
     if [[ $err -ne 0 ]] ; then
        echo " File $GEMGRD does not exist."
        exit $err
     fi

     mv $COMOUT/.$GEMGRD $COMOUT/$GEMGRD
     if [ $SENDDBN = "YES" ] ; then
         $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
           $COMOUT/$GEMGRD
     else
       echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
     fi
  fi

  if [ $fhcnt -ge 240 ] ; then
    let fhcnt=fhcnt+12
  else
    let fhcnt=fhcnt+finc
  fi
done

$GEMEXE/gpend
#####################################################################
# GOOD RUN
set +x
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE IBM"
set -x
#####################################################################

msg='Job completed normally.'
echo $msg
postmsg "$jlogfile" "$msg"

############################### END OF SCRIPT #######################
