#!/bin/ksh
#  UTILITY SCRIPT NAME :  mkawpgrb.sh
#               AUTHOR :  Mary Jacobs
#         DATE WRITTEN :  11/06/96
#
#  Abstract:  This utility script produces AWIPS GRIB bulletins.  
#
#     Input:  1 argument are passed to this script.   
#             1st argument - Forecast Hour - format of 2I
#             
#     Usage:  mkawpgbl.sh $hour
#             (For NAM Products, this script look for env var of $GRID)
#

echo "History:  SEP 1996 - First implementation of this utility script"
echo "Modified: APR 1997 - Added logic for KWBH parm when MRF, default"
echo "                     KWBC (program default) for GFS products.   "
echo "                     This logic lost when processing converted  "
echo "                     to SMS...field never noticed! (MAF)        "
echo "Modified: AUG 1999 - Modified for IBM SP"
echo "                     Allows interactive use"
echo "Modified: MAY 2003 - Added call to new executable, tocgrib, which"
echo "                     will send unblocked files with headers to   "
echo "                     the TOC for transmission to AWIPS."
echo "Modified: AUG 2014 - Removed processing GFS in GRIB2"
echo "Modified: MAY 2015 - Updated for WCOSS Phase 2"
#

set +x
hour_list="$1"
num=$#

if test "$num" -ge 1
then
   echo " Appropriate number of arguments were passed"
   set -x
#  export EXECutil=${EXECutil:-${NWROOT}/util/exec}
#  export PARMutil=${PARMutil:-${NWROOT}/util/parm}
   export envir=${envir:-prod}
   export jlogfile=${jlogfile:-jlogfile}
   export NET=${NET:-gfs}
   export RUN=${RUN:-gfs}
   export DBNALERT_TYPE=${DBNALERT_TYPE:-GRIB_LOW}
   if [ $NET = "nam" ]
   then
      if [ -z "$GRID" ]
      then
         export GRID=207
      fi
   fi
   export cyc=${cyc:-00}
   export cycle=${cycle:-t${cyc}z}
   export SENDCOM=${SENDCOM:-NO}
   export SENDDBN=${SENDDBN:-NO}
   if [ -z "$DATA" ]
   then
      export DATA=`pwd`
      cd $DATA
      ${NWROOT}/util/ush/setup.sh
      ${NWROOT}/util/ush/setpdy.sh
      . PDY
   fi
   export COMIN=${COMIN:-${COMROOT}/$NET/$envir/$NET.$PDY/${cyc}}
   export COMOUTwmo=${COMOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}/${cyc}/wmo}
   export job=${job:-interactive}
   export pgmout=${pgmout:-OUTPUT.$$}
else
   echo ""
   echo "Usage: mkawpgbl.sh \$hour"
   echo ""
   exit 16
fi

set +x
echo " ------------------------------------------"
echo " BEGIN MAKING $NET XTRN/GRIB AWIPS PRODUCTS"  
echo " ------------------------------------------"
set -x

msg="Enter Make AWIP GRIB utility."
postmsg "$jlogfile" "$msg"

############################################
# Figure out INPUT/OUTPUT/PARM/EXEC Fields
############################################
##############################################
# NOTE:  STOP process 6-hour output GFS Grids
#        in this script remove gfs part.
##############################################

for hour in $hour_list
do
   case $NET in
      mrf)input_grb=pgrbf${hour}
          input_grbi=pgrbif${hour}
          output_grb=xtrn.awp${NET}${hour}
          parmcard=grib_awp${NET}${hour}
          parm="parm=KWBH"
#         executable=mkgfsawps
          executable=$MKGFSAWPS
          DBNALERT_TYPE=GRIB_LOW 
          ;;
    smoke)case $GRID in
          sfc)input_grb=grib2_sfc.1hr_227
          input_grbi=""
          output_grb=smoke_sfc.1hr_227.grib2
          parmcard=grib2_smokesfc.227
          parm="parm=KWBP"
#         executable=tocgrib2
          executable=$TOCGRIB2
          DBNALERT_TYPE=GRIB_LOW
          ;;
          pbl)input_grb=grib2_pbl.1hr_227
          input_grbi=""
          output_grb=smoke_pbl.1hr_227.grib2
          parmcard=grib2_smokepbl.227
          parm="parm=KWBP"
#         executable=tocgrib2
          executable=$TOCGRIB2
          DBNALERT_TYPE=GRIB_LOW
          ;;
      esac
      ;;
      nam)case $GRID in
          ########################################
          # 6-hour output NAM Grids
          ########################################
          207)input_grb=awp207${hour}.tm00
              input_grbi=awp207i${hour}
              output_grb=xtrn.awp${NET}${hour}.207
              parmcard=grib_awp${NET}${hour}.207
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
          211)input_grb=awp211${hour}.tm00
              input_grbi=awp211i${hour}
              output_grb=xtrn.awp${NET}${hour}.211
              parmcard=grib_awp${NET}${hour}.211
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
          237)input_grb=awp237${hour}.tm00
              input_grbi=awp237i${hour}
              output_grb=xtrn.awp${NET}${hour}.237
              parmcard=grib_awp${NET}${hour}.237
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
         icwf)input_grb=awip3d${hour}.tm00_icwf
              input_grbi=awip3di${hour}.tm00_icwf
              output_grb=xtrn.awp_icwf_${NET}${hour}.212
              parmcard=grib_icwf${NET}${hour}
              parm="parm=KWBD"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
          ########################################
          # 3-hour output NAM Grids
          ########################################
          212)input_grb=awip3d${hour}.tm00
              input_grbi=awip3di${hour}.tm00
              output_grb=xtrn.awp${NET}${hour}
              parmcard=grib_meso${hour}.40
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
          215)input_grb=awip20${hour}.tm00
              input_grbi=awip20i${hour}.tm00
              output_grb=xtrn.awp${NET}${hour}.GRIB215
              parmcard=grib_meso${hour}.20
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
          216)input_grb=awipak${hour}.tm00
              input_grbi=awipaki${hour}.tm00
              output_grb=xtrn.awpak${NET}${hour}
              parmcard=grib_mesoak${hour}.40
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
          217)input_grb=awp217${hour}.tm00
              input_grbi=awp217i${hour}.tm00
              output_grb=xtrn.awp${NET}${hour}.GRIB217
              parmcard=grib_mesoak${hour}.20
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
      237_off)input_grb=awp237${hour}.tm00
              input_grbi=awp237i${hour}
              output_grb=xtrn.awp${NET}${hour}.237
              parmcard=grib_awp${NET}${hour}.237o
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
     icwf_off)input_grb=awip20${hour}.tm00_icwf
              input_grbi=awip20i${hour}.tm00_icwf
              output_grb=xtrn.awp_icwf_${NET}${hour}
              parmcard=grib_icwfmeso${hour}
              parm="parm=KWBE"
#             executable=tocgrib
              executable=$TOCGRIB
              DBNALERT_TYPE=GRIB_LOW
              ;;
        218g2)input_grb=awphys${hour}.grb2.tm00
              input_grbi=""
              output_grb=grib2.awp${NET}${hour}.218
              parmcard=grib2_awp${NET}${hour}.218
#             executable=tocgrib2
              executable=$TOCGRIB2
	      DBNALERT_TYPE=GRIB_LOW
              ;;
        242g2)input_grb=awak3d${hour}.grb2.tm00
              input_grbi=""
              output_grb=grib2.awp${NET}${hour}.242
              parmcard=grib2_awp${NET}${hour}.242
#             executable=tocgrib2
              executable=$TOCGRIB2
              DBNALERT_TYPE=GRIB_LOW
              ;;
     218_icwf)input_grb=awip12${hour}.grb2.tm00_icwf
              input_grbi=""
              output_grb=grib2.awp${NET}${hour}.${GRID}
              parmcard=grib2_awp${NET}${hour}.${GRID}
#             executable=tocgrib2
              executable=$TOCGRIB2
	      DBNALERT_TYPE=GRIB_LOW
              ;;
     242_icwf)input_grb=awak3d${hour}.grb2.tm00_icwf
              input_grbi=""
              output_grb=grib2.awp${NET}${hour}.${GRID}
              parmcard=grib2_awp${NET}${hour}.${GRID}
#             executable=tocgrib2
              executable=$TOCGRIB2
              DBNALERT_TYPE=GRIB_LOW
              ;;
          esac
       ;;
     dgex)case $GRID in
          185)RUN=dgex_conus
              input_grb=awpgrb2${GRID}${hour}.tm00
              input_grbi=""
	      output_grb=grib2.awpdgex${hour}.${GRID}
	      parmcard=grib2_awpdgex${hour}.${GRID}
#             executable=tocgrib2
              executable=$TOCGRIB2
	      DBNALERT_TYPE=GRIB_LOW
              ;;
          186)RUN=dgex_alaska
              input_grb=awpgrb2${GRID}${hour}.tm00
              input_grbi=""
              output_grb=grib2.awpdgex${hour}.${GRID}
              parmcard=grib2_awpdgex${hour}.${GRID}
#             executable=tocgrib2
              executable=$TOCGRIB2
	      DBNALERT_TYPE=GRIB_LOW
              ;;
     185_icwf)RUN=dgex_conus
              input_grb=awpgrb2185${hour}.tm00_icwf
              input_grbi=""
              output_grb=grib2.awpdgex${hour}.${GRID}
              parmcard=grib2_awpdgex${hour}.${GRID}
#             executable=tocgrib2
              executable=$TOCGRIB2
	      DBNALERT_TYPE=GRIB_LOW
              ;;
     186_icwf)RUN=dgex_alaska
              input_grb=awpgrb2186${hour}.tm00_icwf
              input_grbi=""
              output_grb=grib2.awpdgex${hour}.${GRID}
              parmcard=grib2_awpdgex${hour}.${GRID}
#             executable=tocgrib2
              executable=$TOCGRIB2
	      DBNALERT_TYPE=GRIB_LOW
              ;;
         esac
         ;;
       aqm)
         case $GRID in
          227)input_grb=grib2.227
	      input_grbi=""
	      output_grb=grib2.awpaqm.${GRID}
	      parmcard=grib2_awpaqm.227.${cycle}
#             executable=tocgrib2
              executable=$TOCGRIB2
              DBNALERT_TYPE=GRIB_LOW
              ;;
        227_3X)input_grb=grib2_3x.227
               input_grbi=""
               output_grb=grib2.awpaqm_3x.${GRID}
               parmcard=grib2_awpaqm_3x.227.${cycle}
#              executable=tocgrib2
               executable=$TOCGRIB2
               DBNALERT_TYPE=GRIB_LOW
               ;;
	227_5X)input_grb=grib2_5x.227
               input_grbi=""
               output_grb=grib2.awpaqm_5x.${GRID}
               parmcard=grib2_awpaqm_5x.227.${cycle}
#              executable=tocgrib2
               executable=$TOCGRIB2
               DBNALERT_TYPE=GRIB_LOW
               ;;
         esac
         ;;
      sref)input_grb=pgrb${GRID}.${type}_3hrly.grib2
           input_grbi=""
           output_grb=grib2.t${cyc}z.awipsref${GRID}.${type}
           parmcard=grib2_awpsref${GRID}.${type}
#          executable=tocgrib2
           executable=$TOCGRIB2
           DBNALERT_TYPE=NTC_LOW
         ;;
      ruc)RUN=ruc2
          input_grb=pgrb13f${hour}.grib2
          input_grbi=""
          output_grb=grib2.t${cyc}z.awpruc13f${hour}
          parmcard=grib2_awpruc13f${hour}
#         executable=tocgrib2
          executable=$TOCGRIB2
#         DBNALERT_TYPE=NTC_LOW
          DBNALERT_TYPE=
         ;;
   esac

   executable_name=`basename $executable`

   ##############################
   # Copy Input Field to $DATA
   ##############################

   if test ! -f $input_grb
   then
      cp $COMIN/${RUN}.${cycle}.$input_grb $input_grb
   fi

   if test ! -f $COMIN/${RUN}.${cycle}.$input_grbi 
   then
#     if test $executable != "tocgrib2"
      if test $executable_name != "tocgrib2"
      then
#       $EXECutil/grbindex $input_grb $input_grbi
        $GRBINDEX $input_grb $input_grbi
      else
	input_grbi=""
      fi
   else
      cp $COMIN/${RUN}.${cycle}.$input_grbi $input_grbi
   fi

   ##############################
   # Create AWIPS GRIB data
   ##############################

   export pgm=$executable
   . prep_step
   export FORT11="$input_grb"
   export FORT31="$input_grbi"
   export FORT51="$output_grb"

   startmsg

#   $executable < $PARMshared/$parmcard $parm >> $pgmout 2>errfile
   $executable < ${UTILgfs}/parm/$parmcard $parm >> $pgmout 2>errfile
#  $EXECutil/$executable < $PARMutil/$parmcard $parm >> $pgmout 2>errfile
#    export err=$?;err_chk

   ##############################
   # Post Files to COMOUTwmo
   ##############################

   if test "$SENDCOM" = 'YES'
   then
     cp $output_grb $COMOUTwmo/$output_grb.$job

     ##############################
     # Distribute Data
     ##############################

     if [ "$SENDDBN" = 'YES' -o "$SENDAWIP" = 'YES' ] ; then
        $DBNROOT/bin/dbn_alert $DBNALERT_TYPE $NET $job $COMOUTwmo/$output_grb.$job
     else
        msg="File $output_grb.$job not posted to db_net."
        postmsg "$jlogfile" "$msg"
     fi
  fi

   msg="Awip Processing ${hour} hour  completed normally"
   postmsg "$jlogfile" "$msg"

done

exit
