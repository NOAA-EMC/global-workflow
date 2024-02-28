#! /usr/bin/env bash

###################################################################
# echo "----------------------------------------------------"
# echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
# echo "----------------------------------------------------"
# echo "History: Mar 2000 - First implementation of this new script."
# echo "S Lilly: May 2008 - add logic to make sure that all of the "
# echo "                    data produced from the restricted ECMWF"
# echo "                    data on the CCS is properly protected."
# echo "C. Magee: 10/2013 - swap X and Y for rtgssthr Atl and Pac."
#####################################################################

source "$HOMEgfs/ush/preamble.sh"

cd $DATA

cp ${HOMEgfs}/gempak/fix/g2varswmo2.tbl g2varswmo2.tbl
cp ${HOMEgfs}/gempak/fix/g2vcrdwmo2.tbl g2vcrdwmo2.tbl
cp ${HOMEgfs}/gempak/fix/g2varsncep1.tbl g2varsncep1.tbl
cp ${HOMEgfs}/gempak/fix/g2vcrdncep1.tbl g2vcrdncep1.tbl

#
# NAGRIB_TABLE=${HOMEgfs}/gempak/fix/nagrib.tbl
NAGRIB=$GEMEXE/nagrib2
#

entry=$(grep "^$RUN2 " $NAGRIB_TABLE | awk 'index($1,"#") != 1 {print $0}')

if [ "$entry" != "" ] ; then
  cpyfil=$(echo $entry  | awk 'BEGIN {FS="|"} {print $2}')
  garea=$(echo $entry   | awk 'BEGIN {FS="|"} {print $3}')
  gbtbls=$(echo $entry  | awk 'BEGIN {FS="|"} {print $4}')
  maxgrd=$(echo $entry  | awk 'BEGIN {FS="|"} {print $5}')
  kxky=$(echo $entry    | awk 'BEGIN {FS="|"} {print $6}')
  grdarea=$(echo $entry | awk 'BEGIN {FS="|"} {print $7}')
  proj=$(echo $entry    | awk 'BEGIN {FS="|"} {print $8}')
  output=$(echo $entry  | awk 'BEGIN {FS="|"} {print $9}')
else
  cpyfil=gds
  garea=dset
  gbtbls=
  maxgrd=4999
  kxky=
  grdarea=
  proj=
  output=T
fi  
pdsext=no

maxtries=180
fhcnt=$fstart
while [ $fhcnt -le $fend ] ; do
  fhr=$(printf "%03d" $fhcnt)
  fhcnt3=$(expr $fhr % 3)

  fhr3=$(printf "03d" $fhcnt)
  GRIBIN=$COMIN/${model}.${cycle}.${GRIB}${fhr}${EXT}
  GEMGRD=${RUN2}_${PDY}${cyc}f${fhr3}

  GRIBIN_chk=$GRIBIN

  icnt=1
  while [ $icnt -lt 1000 ]
  do
    if [ -r $GRIBIN_chk ] ; then
      break
    else
      sleep 20
      let "icnt=icnt+1"
    fi
    if [ $icnt -ge $maxtries ]
    then
      echo "ABORTING after 1 hour of waiting for F$fhr to end."
      export err=7 ; err_chk
      exit $err
    fi
  done

  cp $GRIBIN grib$fhr

  export pgm="nagrib_nc F$fhr"
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

  $GEMEXE/gpend

  cp $GEMGRD $COMOUT/.$GEMGRD
  mv $COMOUT/.$GEMGRD $COMOUT/$GEMGRD
  if [ $SENDDBN = "YES" ] ; then
      $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
			     $COMOUT/$GEMGRD
  else
      echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
  fi

  let fhcnt=fhcnt+finc
done

#####################################################################


############################### END OF SCRIPT #######################
