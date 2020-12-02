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

#### If EMC GFS PARA runs hourly file are not available, The ILPOST
#### will set to 3 hour in EMC GFS PARA.
#### Note:  ILPOST default set to 1
export ILPOST=${ILPOST:-1}

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


#
NAGRIB=$GEMEXE/nagrib2_nc
#

cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no

maxtries=360
fhcnt=$fstart
while [ $fhcnt -le $fend ] ; do

if mkdir lock.$fhcnt  ; then
  cd lock.$fhcnt
  cp $FIXgempak/g2varswmo2.tbl g2varswmo2.tbl
  cp $FIXgempak/g2vcrdwmo2.tbl g2vcrdwmo2.tbl
  cp $FIXgempak/g2varsncep1.tbl g2varsncep1.tbl
  cp $FIXgempak/g2vcrdncep1.tbl g2vcrdncep1.tbl

  typeset -Z3 fhr

  fhr=$fhcnt
  fhcnt3=`expr $fhr % 3`

  fhr3=$fhcnt
  typeset -Z3 fhr3

  GEMGRD=${RUN}_${PDY}${cyc}f${fhr3}

# Set type of Interpolation for WGRIB2
  export opt1=' -set_grib_type same -new_grid_winds earth '
  export opt1uv=' -set_grib_type same -new_grid_winds grid '
  export opt21=' -new_grid_interpolation bilinear -if '
  export opt22=":(CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
  export opt23=' -new_grid_interpolation neighbor -fi '
  export opt24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
  export opt25=":(APCP|ACPCP|PRATE|CPRAT):"
  export opt26=' -set_grib_max_bits 25 -fi -if '
  export opt27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
  export opt28=' -new_grid_interpolation budget -fi '
  export TRIMRH=$HOMEgfs/ush/trim_rh.sh

  if [ $RUN = "gfs_0p50" ]; then
    export GRIBIN=$COMIN/${model}.${cycle}.pgrb2.0p50.f${fhr}
    GRIBIN_chk=$COMIN/${model}.${cycle}.pgrb2.0p50.f${fhr}.idx
  elif [ $RUN = "gfs_0p25" -o $RUN = "gdas_0p25" -o $RUN = "gfs35_atl" -o $RUN = "gfs35_pac" -o $RUN = "gfs40" ]; then 
    export GRIBIN=$COMIN/${model}.${cycle}.pgrb2.0p25.f${fhr}
    GRIBIN_chk=$COMIN/${model}.${cycle}.pgrb2.0p25.f${fhr}.idx
  else
    export GRIBIN=$COMIN/${model}.${cycle}.pgrb2.1p00.f${fhr}
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
      sleep 10
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

case $RUN in
 gfs35_pac)
#   $COPYGB2 -g "0 6 0 0 0 0 0 0 416 186 0 0 75125000 130000000 48 17000000 260000000 312000 312000 0" -x $GRIBIN grib$fhr
#   NEW define gfs35_pac="0 6 0 0 0 0 0 0 416 186 0 -1 75125000 130000000 48 17405000 259480000 312000 312000 0"
#   $COPYGB2 -g "0 6 0 0 0 0 0 0 416 186 0 -1 75125000 130000000 48 17405000 259480000 312000 312000 0" -x $GRIBIN grib$fhr

    export gfs35_pac='latlon 130.0:416:0.312 75.125:186:-0.312'
    $WGRIB2  $GRIBIN $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid ${gfs35_pac} grib$fhr
    $TRIMRH grib$fhr
   ;;
 gfs35_atl)
#    $COPYGB2 -g "0 6 0 0 0 0 0 0 480 242 0 0 75125000 230000000 48 -500000 20000000 312000 312000 0" -x $GRIBIN grib$fhr
# NEW  define gfs35_atl="0 6 0 0 0 0 0 0 480 242 0 -1 75125000 230000000 48  -67000 19448000 312000 312000 0"
#   $COPYGB2 -g "0 6 0 0 0 0 0 0 480 242 0 -1 75125000 230000000 48  -67000 19448000 312000 312000 0" -x $GRIBIN grib$fhr

    export gfs35_atl='latlon 230.0:480:0.312 75.125:242:-0.312'
    $WGRIB2  $GRIBIN $opt1 $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid ${gfs35_atl} grib$fhr
    $TRIMRH grib$fhr
   ;;
 gfs40)
#   $COPYGB2 -g "30 6 0 0 0 0 0 0 185 129 12190000 226541000 8 25000000 265000000 40635000 40635000 0 64 25000000 25000000 0 0" -x $GRIBIN grib$fhr

    export gfs40='lambert:265.0:25.0:25.0 226.541:185:40635.0 12.19:129:40635.0'
    $WGRIB2  $GRIBIN $opt1uv $opt21 $opt22 $opt23 $opt24 $opt25 $opt26 $opt27 $opt28 -new_grid ${gfs40} grib$fhr
    $TRIMRH grib$fhr
   ;;
 *)
     cp $GRIBIN grib$fhr
esac

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
     cpfs $GEMGRD $COMOUT/$GEMGRD
     if [ $SENDDBN = "YES" ] ; then
         $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
           $COMOUT/$GEMGRD
     fi
  fi
  cd $DATA_RUN
else
    if [ $fhcnt -ge 240 ] ; then
	if [ $fhcnt -lt 276 -a $RUN = "gfs_0p50" ] ; then
	    let fhcnt=fhcnt+6
	else
	    let fhcnt=fhcnt+12
	fi
    elif [ $fhcnt -lt 120 -a $RUN = "gfs_0p25" ] ; then
####    let fhcnt=fhcnt+1
	let fhcnt=fhcnt+$ILPOST
    else
	let fhcnt=fhcnt+finc
    fi
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
