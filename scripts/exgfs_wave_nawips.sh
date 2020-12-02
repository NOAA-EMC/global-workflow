#!/bin/ksh
###################################################################
echo "----------------------------------------------------"
echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
echo "----------------------------------------------------"
echo "History: Mar 2000 - First implementation of this new script."
echo "Sept 2011 - First implementation of this new script based on"
echo "               /nwprod/scripts/exnawips.sh.sms"
echo " March 2020- Modified for GEFSv12.0"
#  March-2020 Roberto.Padilla@noaa.gov                                   
#####################################################################

set -xa

#export grids=${grids:-'glo_30m at_10m ep_10m wc_10m ao_9km'} #Interpolated grids
export grids=${grids:-'glo_10m gso_15m ao_9km'}  #Native grids
export RUNwave=${RUNwave:-${RUN}${COMPONENT}}
export fstart=${fstart:-0}
export FHMAX_WAV=${FHMAX_WAV:-180}  #180 Total of hours to process
export FHMAX_HF_WAV=${FHMAX_HF_WAV:-72}
export FHOUT_WAV=${FHOUT_WAV:-6}
export FHOUT_HF_WAV=${FHOUT_HF_WAV:-3}
export maxtries=${maxtries:-720}
export cycle=${cycle:-t${cyc}z}
export GEMwave=${GEMwave:-$HOMEgfs/gempak}
export FIXwave=${FIXwave:-HOMEgfs/fix}
export DATA=${DATA:-${DATAROOT:?}/${jobid}}
if [ ! -d $DATA ];then
  mkdir -p $DATA
fi

cd $DATA
cp $GEMwave/fix/g2varswmo2.tbl .

cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no
g2tbls=g2varswmo2.tbl
NAGRIB=nagrib2

maxtries=15
fhcnt=$fstart
while [ $fhcnt -le $FHMAX_WAV ]; do
  fhr=$(printf "%03d" $fhcnt)
  for grid in $grids;do
    case $grid in
      ao_9km)  grdIDin='arctic.9km'
               grdIDout='gfswaveao9km' ;;
      at_10m)  grdIDin='atlocn.0p16'
               grdIDout='gfswaveat10m' ;;
      ep_10m)  grdIDin='epacif.0p16'
               grdIDout='gfswaveep10m' ;;
      wc_10m)  grdIDin='wcoast.0p16'
               grdIDout='gfswavewc10m' ;;
      glo_30m) grdIDin='global.0p25'
               grdIDout='gfswavegl30m' ;;
      glo_10m) grdIDin='global.0p16'   
               grdIDout='gfswaveg16k' ;;
      gso_15m) grdIDin='gsouth.0p25' 
               grdIDout='gfswaves25k' ;;
      *)       gridIDin= 
               grdIDout= ;;
    esac
    GRIBIN=$COMIN/gridded/$RUNwave.$cycle.$grdIDin.f${fhr}.grib2
    GRIBIN_chk=$GRIBIN.idx

    icnt=1
    while [ $icnt -lt 1000 ]; do
      if [ -r $GRIBIN_chk ] ; then
        break
      else
        let "icnt=icnt+1"
        sleep 20
      fi
      if [ $icnt -ge $maxtries ]; then
        msg="ABORTING after 5 minutes of waiting for $GRIBIN."
        postmsg "$jlogfile" "$msg"
        echo ' '
        echo '**************************** '
        echo '*** ERROR : NO GRIB FILE *** '
        echo '**************************** '
        echo ' '
        echo $msg
        [[ "$LOUD" = YES ]] && set -x
        echo "$RUNwave $grdID ${fhr} prdgen $date $cycle : GRIB file missing." >> $wavelog
        err=1;export err;${errchk} || exit ${err}
      fi
    done

    #if [ "$grdIDin" = "global.0p25" && "$grid" = "glo_30m" ]; then
    if [ "$grdIDin" = "global.0p25" ]; then
      $WGRIB2 -lola 0:720:0.5 -90:361:0.5 gribfile.$grdIDout.f${fhr}  grib \
                                          $GRIBIN 1> out 2>&1
      OK=$?
      if [ "$OK" != '0' ]; then 
        msg="ABNORMAL EXIT: ERROR IN interpolation the global grid"
        postmsg "$jlogfile" "$msg"
        #set +x
        echo ' '
        echo '************************************************************* '
        echo '*** FATAL ERROR : ERROR IN making  gribfile.$grdID.f${fhr}*** '
        echo '************************************************************* '
        echo ' '
        echo $msg
        #[[ "$LOUD" = YES ]] && set -x
        echo "$RUNwave $grdID prdgen $date $cycle : error in grbindex." >> $wavelog
        err=2;export err;err_chk
      else
        #cp $GRIBIN gribfile.$grdID.f${fhr}
        GRIBIN=gribfile.$grdIDout.f${fhr}
      fi
    fi
    echo $GRIBIN  

    GEMGRD=${grdIDout}_${PDY}${cyc}f${fhr}

    cp $GRIBIN grib_$grid

    startmsg

	$NAGRIB <<-EOF
	GBFILE   = grib_$grid
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
	G2TBLS   = $g2tbls
	GBDIAG   = 
	PDSEXT   = $pdsext
	l
	r
	EOF
    export err=$?;pgm=$NAGRIB;err_chk
    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################
    ls -l $GEMGRD
    export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

    if [ "$NAGRIB" = "nagrib2" ] ; then
      gpend
    fi

    if [ $SENDCOM = "YES" ] ; then
      cpfs $GEMGRD $COMOUT/$GEMGRD
      if [ $SENDDBN = "YES" ] ; then
        $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        $COMOUT/$GEMGRD
      else
        echo "##### DBN_ALERT is: MODEL ${DBN_ALERT_TYPE} $job $COMOUT/$GEMGRD#####"
      fi
    fi
    rm grib_$grid
  done
  if [ $fhcnt -ge $FHMAX_HF_WAV ]; then
    inc=$FHOUT_WAV
  else
    inc=$FHOUT_HF_WAV
  fi
  let fhcnt=fhcnt+inc
done
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
