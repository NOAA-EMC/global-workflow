#!/bin/ksh
###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the              #
# GFSv16-wave output for gridded wave fields                                  #
#                                                                             #
# Remarks :                                                                   #
# - Supplemental error output is witten to the wave.log file.                 #
#                                                                             #
#                                                                             #
# Origination  : 05/02/2007                                                   #
# Last update  : 10/08/2020                                                   # 
#                                                                             #
# Oct, 2020  Roberto.Padilla@noaa.gov, Henrique.HAlves@noaa.gov                # 
#         - Merging wave scripts to GFSv16 global workflow                    #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation
 set -xa
 # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
 export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
 [[ "$LOUD" != YES ]] && set +x

 export RUNwave=${RUNwave:-${RUN}${COMPONENT}}
 export envir=${envir:-ops}
 export fstart=${fstart:-0}
 export FHMAX_WAV=${FHMAX_WAV:-180}       #180 Total of hours to process
 export FHMAX_HF_WAV=${FHMAX_HF_WAV:-72}  #from 00 to  72 inc=3
 export FHOUT_WAV=${FHOUT_WAV:-6}         #from 72 to 180 inc=6
 export FHOUT_HF_WAV=${FHOUT_HF_WAV:-3}
 export maxtries=720
 export FIXwave=${FIXwave:-$HOMEgfs/fix}
 export PARMwave=${PARMwave:-$HOMEgfs/parm/parm_wave}
 export USHwave=${USHwave:-$HOMEgfs/ush}
 export cyc=${cyc:-00}
 export cycle=${cycle:-t${cyc}z}
 export pgmout=OUTPUT.$$
 export DATA=${DATA:-${DATAROOT:?}/${job}.$$}
 mkdir -p $DATA
 cd $DATA
 export wavelog=${DATA}/${COMPONENTwave}_prdggridded.log
 
 postmsg "$jlogfile" "HAS BEGUN on `hostname`"
 msg="Starting MWW3 GRIDDED PRODUCTS SCRIPT"
 postmsg "$jlogfile" "$msg"
# Output grids
# grids=${grids:-ao_9km at_10m ep_10m wc_10m glo_30m}
 grids=${grids:-ak_10m at_10m ep_10m wc_10m glo_30m}
 maxtries=${maxtries:-720}
# 0.b Date and time stuff
 export date=$PDY
 export YMDH=${PDY}${cyc}
 echo ' '
 echo '                         ****************************'
 echo '                         *** MWW3 PRODUCTS SCRIPT ***'
 echo '                         ****************************'
 echo "                                       $date $cycle"
 echo ' '
 echo "Starting at : `date`"
 echo ' '
 echo "   AWIPS grib fields"
 echo "   Wave  Grids       : $grids"
 echo ' '
 [[ "$LOUD" = YES ]] && set -x

# --------------------------------------------------------------------------- #
# 1.  Get necessary files
 echo ' '
 echo 'Preparing input files :'
 echo '-----------------------'
 [[ "$LOUD" = YES ]] && set -x
#=======================================================================
 
 ASWELL=(SWELL1 SWELL2) # Indices of HS from partitions
 ASWPER=(SWPER1 SWPER2) # Indices of PERIODS from partitions 
 ASWDIR=(SWDIR1 SWDIR2) # Indices of DIRECTIONS from partitions 
                                #  (should be same as ASWELL)
 #export arrpar=(WIND UGRD VGRD HTSGW PERPW DIRPW WVHGT WVPER WVDIR WDIR ${ASWELL[@]} ${ASWDIR[@]} ${ASWPER[@]})
 export arrpar=(WIND WDIR UGRD VGRD HTSGW PERPW DIRPW WVHGT ${ASWELL[@]} WVPER ${ASWPER[@]} WVDIR ${ASWDIR[@]} )
 export nparam=`echo ${arrpar[@]} | wc -w`


# 1.a Grib file (AWIPS and FAX charts)
 fhcnt=$fstart
 while [ $fhcnt -le $FHMAX_WAV ]; do
   fhr=$(printf "%03d" $fhcnt)
   for grdOut in $grids;do
     case $grdOut in
       ao_9km)  grdID='arctic.9km' ;;
       at_10m)  grdID='atlocn.0p16' ;;
       ep_10m)  grdID='epacif.0p16' ;;
       wc_10m)  grdID='wcoast.0p16' ;;
#       glo_30m) grdID='global.0p25' ;;
       glo_30m) grdID='global.0p50' ;;
       ak_10m)  grdID='alaska.0p16' ;;
       *)       grdID= ;;
     esac
     #

     GRIBIN=$COMIN/gridded/$RUNwave.$cycle.$grdID.f${fhr}.grib2
     GRIBIN_chk=$GRIBIN.idx

     icnt=1
     while [ $icnt -lt 1000 ]; do
       if [ -r $GRIBIN_chk ] ; then
         break
       else
         echo "Waiting for input file: $GRIBIN"
         let "icnt=icnt+1"
         sleep 5
       fi
       if [ $icnt -ge $maxtries ]; then
         msg="ABNORMAL EXIT: NO GRIB FILE FOR GRID $GRIBIN"
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

     GRIBOUT=$RUNwave.$cycle.$grdID.f${fhr}.clipped.grib2

     iparam=1
     while [ ${iparam} -le ${nparam} ]; do
       nip=${arrpar[$iparam-1]}
       prepar=`echo $nip | rev | cut -c2- | rev` #Part prefix (assumes 1 digit index)
       paridx=`echo $nip | rev | cut -c-1`
       npart=0   
       case $prepar in
         SWELL)  npart=1 ;;
         SWDIR)  npart=1 ;;
         SWPER)  npart=1 ;;
         *)     npart=0 ;;
       esac
       echo $nip $prepar $paridx $npart
       rm temp.grib2 
       if [ "${npart}" = "0" ]; then 
         $WGRIB2 $GRIBIN -s | grep ":${nip}" | $WGRIB2 -i $GRIBIN -grib temp.grib2 > wgrib.out 2>&1 
         $WGRIB2 temp.grib2 -append -grib  $GRIBOUT
       else
         $WGRIB2 $GRIBIN -s | grep ":${prepar}" |  grep "${paridx} in sequence" | \
                 $WGRIB2 -i $GRIBIN -grib temp.grib2  > wgrib.out 2>&1 
         $WGRIB2 temp.grib2 -append -grib  $GRIBOUT
       fi
       iparam=`expr ${iparam} + 1`
     done #end wave param loop
#======================================================================
     GRIBIN=$RUNwave.$cycle.$grdID.f${fhr}.clipped.grib2
     GRIBIN_chk=$GRIBIN.idx

     ln -s $GRIBIN gribfile.$grdID.f${fhr}

     #
# 1.d Input template files
     parmfile=$PARMwave/grib2_${RUNwave}.$grdOut.f${fhr}
     if [ -f $parmfile ]; then
       ln -s $parmfile awipsgrb.$grdID.f${fhr}
     else
       echo '*** ERROR : NO template  grib2_${RUNwave}.$grdID.f${fhr} *** '
       echo "$RUNwave $grdID $fhr prdgen $date $cycle : GRIB template file missing." >> $wavelog
       err=3;export err;${errchk} || exit ${err}
     fi
     #
# 2.  AWIPS product generation
# 2.a AWIPS GRIB file with headers
     echo ' '
     echo 'AWIPS headers to GRIB file ...'
     echo '------------------------------'

# 2.a.1 Set up for tocgrib2
     echo "   Do set up for tocgrib2."
     [[ "$LOUD" = YES ]] && set -x
     #AWIPSGRB=awipsgrib.$grdID.f${fhr}
     AWIPSGRB=awipsgrib
# 2.a.2 Make GRIB index
     echo "   Make GRIB index for tocgrib2."
     [[ "$LOUD" = YES ]] && set -x
     $GRB2INDEX gribfile.$grdID.f${fhr} gribindex.$grdID.f${fhr}
     OK=$?

     if [ "$OK" != '0' ]
     then
       msg="ABNORMAL EXIT: ERROR IN grb2index MWW3 for grid $grdID"
       postmsg "$jlogfile" "$msg"
       #set +x
       echo ' '
       echo '******************************************** '
       echo '*** FATAL ERROR : ERROR IN grb2index MWW3 *** '
       echo '******************************************** '
       echo ' '
       echo $msg
       #[[ "$LOUD" = YES ]] && set -x
       echo "$RUNwave $grdID prdgen $date $cycle : error in grbindex." >> $wavelog
       err=4;export err;err_chk
     fi

# 2.a.3 Run AWIPS GRIB packing program tocgrib2

     echo "   Run tocgrib2"
     [[ "$LOUD" = YES ]] && set -x
     export pgm=tocgrib2
     export pgmout=tocgrib2.out
     . prep_step

     export FORT11="gribfile.$grdID.f${fhr}"
     export FORT31="gribindex.$grdID.f${fhr}"
     export FORT51="$AWIPSGRB.$grdID.f${fhr}"

     $TOCGRIB2 < awipsgrb.$grdID.f${fhr} > tocgrib2.out 2>&1
     OK=$?
     if [ "$OK" != '0' ]; then
       cat tocgrib2.out
       msg="ABNORMAL EXIT: ERROR IN tocgrib2"
       postmsg "$jlogfile" "$msg"
       #set +x
       echo ' '
       echo '*************************************** '
       echo '*** FATAL ERROR : ERROR IN tocgrib2 *** '
       echo '*************************************** '
       echo ' '
       echo $msg
       #[[ "$LOUD" = YES ]] && set -x
       echo "$RUNwave prdgen $date $cycle : error in tocgrib2." >> $wavelog
       err=5;export err;err_chk
     else
       echo '*** tocgrib2 ran succesfully *** '
     fi
# 2.a.7 Get the AWIPS grib bulletin out ...
     #set +x
     echo "   Get awips GRIB bulletins out ..."
     #[[ "$LOUD" = YES ]] && set -x
     if [ "$SENDCOM" = 'YES' ]
     then
       #set +x
       echo "      Saving $AWIPSGRB.$grdOut.f${fhr} as grib2.$cycle.awipsww3_${grdID}.f${fhr}"
       echo "          in $PCOM"
       #[[ "$LOUD" = YES ]] && set -x
       cp $AWIPSGRB.$grdID.f${fhr} $PCOM/grib2.$cycle.f${fhr}.awipsww3_${grdOut}
       #set +x
     fi

     if [ "$SENDDBN" = 'YES' ]
     then
       echo "      Sending $AWIPSGRB.$grdID.f${fhr} to DBRUN."
       $DBNROOT/bin/dbn_alert GRIB_LOW $RUN $job $PCOM/grib2.$cycle.f${fhr}.awipsww3_${grdOut}
     fi
     rm -f $AWIPSGRB.$grdID.f${fhr} tocgrib2.out
   done # For grids

   if [ $fhcnt -ge $FHMAX_HF_WAV ]; then
     inc=$FHOUT_WAV
   else
     inc=$FHOUT_HF_WAV
   fi
   let fhcnt=fhcnt+inc
 done #For fcst time



# --------------------------------------------------------------------------- #
# 5.  Clean up

  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f gribfile gribindex.* awipsgrb.* awipsbull.data
  set +v

# --------------------------------------------------------------------------- #
# 6.  Ending output

  echo ' '
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo '                *** End of MWW3 product generation ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of GFSWAVE product generation script -------------------------------------- #
