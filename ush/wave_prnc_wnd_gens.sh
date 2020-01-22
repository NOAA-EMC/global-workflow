#!/bin/bash
###############################################################################
#                                                                             #
# This script preprocesses wind fields for the wave ensemble model.           #
# It is run as a child scipt by the corresponding preprocessig script.        #
#
# Remarks :                                                                   #
# - This script runs in the work directory designated in the mother script in #
#   which it generates its own sub-directory 'wind_${memb}'.                  #
# - See section 0.b for variables that need to be set.                        #
#                                                                             #
#  Update record :                                                            #
#                                                                             #
# - Origination:                                               26-Mar-2013    #
# - Changes made to run in mpiserial mode                      10-Aug-2013    #
# - Introduced hindcast phase using 80-member ET data          15-Nov-2013    #
# - Removed use of bias corrected winds                        02-Feb-2014    #
# - Updated to use files from GEFS Q3FY15 upgrade              02-Feb-2015    #
#                                                                             #
# Update log                                                                  #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
#                                                                             #
###############################################################################
#
# set execution trace prompt.  ${0##*/} adds the script's basename
#  PS4=" \${SECONDS} ${0##*/} L\${LINENO} + "
  set -x
# Use LOUD variable to turn on/off trace.  Defaults to YES (on).
  export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
  [[ "$LOUD" != YES ]] && set +x
#
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

  cd $DATA

# Read modIE from exwave script
  memb=$1

  modIE="gwes${memb}"

  rm -rf wind_${memb}
  mkdir wind_${memb}
  cd wind_${memb}
  ln -s ../postmsg .


# 0.b Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make wind felds        |'
  echo '+--------------------------------+'
  echo "   Model ID        : $MDC"
  echo "   Member ID       : $modIE"
  echo "   Wind grid ID    : $wndID"
  echo "   Wind Member ID  : $wndIE"
  echo ' '
  [[ "$LOUD" = YES ]] && set -x
  postmsg "$jlogfile" "Making wind fields."

  if [ -z "$YMDH" ] || [ -z "$cycle" ] || [ -z "$modIE" ] || \
     [ -z "$COMOUT" ] || [ -z "$FIXwave" ] || [ -z "$EXECcode" ] || \
     [ -z "$MDC" ] || [ -z "$wndID" ] || [ -z "$SENDCOM" ] || \
     [ -z "$COMINGEFS" ] || [ -z "$time_beg" ] || [ -z "$time_end" ]
  then
    set +x
    echo ' '
    echo '**************************************************'
    echo '*** EXPORTED VARIABLES IN preprocessor NOT SET ***'
    echo '**************************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    postmsg "$jlogfile" "NON-FATAL ERROR - EXPORTED VARIABLES IN preprocessor NOT SET"
    exit 0
  fi

# 0.c Links to working directory

  ln -s ../mod_def.$wndID mod_def.ww3
  ln -s ../sst.ww3 

# --------------------------------------------------------------------------- #
# 1. Input data 
#
# 1.a Copy raw wind files.
#
  fymdh0=${YMDH}

# Added hindcast phase (extent from hcst_hour parameter in ex-script)
  fhour=${hcst_hour}

  fymdh=`$NDATE $fhour $fymdh0`

  CYCrun=${cycle}

  while [ "$fhour" -le "$FHMAXWAV" ]
  do
    if [ "${fhour}" -lt 0 ]
    then
#
# This section pulls the legacy GEFS perturbed analysis data
# and will be replaced by a new one in the next upgrade
#
      PDYanl=`echo ${fymdh} | cut -c1-8`
      CYCanl=`echo ${fymdh} | cut -c9-10`
#      COMINENS=${COMINGEFSA}/gefs.${PDYanl}/$CYCanl/pgrb2a
#
# Points search of wind for hindcast phase to gefs_legacy
      COMINENS=${COMINGEFSA}/gefs_legacy.${PDYanl}/$CYCanl/pgrb2a

# Add extension for member continuity in hindcast phase
      if [ ${CYCanl} -eq ${cyc} ] || [ "${modIE}" = "gwes00" ]
      then # Cycles match, no extension required
        sfHH="pgrb2af00"
      else
        sfHH="pgrb2af00.cycfs${cyc}"
      fi
      sfHHfb=${sfHH}
      cycle="t${CYCanl}z"
    else
#
#   Forecast Time slices
# Copy forecast files from GEFS upgraded files as of FY15Q3
#
      cycle=$CYCrun
# Point to 0.5 deg data
      COMINENS=${COMINGEFS}/gefs.${PDY}/$cyc/pgrb2ap5

      if [ "$fhour" -lt "10" ]
      then
        sfHH="pgrb2a.0p50.f00${fhour}" 
      elif [ "$fhour" -lt "100" ]
      then
        sfHH="pgrb2a.0p50.f0$fhour"
      else
        sfHH="pgrb2a.0p50.f$fhour"
      fi
#
    fi
#
      if [ $modIE != "gwes00" ]; then
        wndIE="gep${memb}"
      elif [ $modIE = "gwes00" ]; then
        wndIE="gec${memb}"
      fi     

     if [ -f $COMINENS/$wndIE.$cycle.$sfHH ]
     then
# TEMP FIX: convert grib2 data for legacy hindcast from 1x1 to 0.5x0.5
       if [ "${fhour}" -lt 0 ]
       then
         /nwprod/util/exec/copygb2 \
       -xg'0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0' \
        $COMINENS/$wndIE.$cycle.$sfHH wind.grib
       else
         cp  $COMINENS/$wndIE.$cycle.$sfHH  wind.grib
       fi
       set +x
       echo ' '
       echo " Raw GEFS wind copied ($COMINENS/$wndIE.$cycle.$sfHH)."
       echo ' '
       [[ "$LOUD" = YES ]] && set -x
     else
       msg="ABNORMAL EXIT: ERR in coping $wndIE file"
       ./postmsg "$jlogfile" "$msg"
       set +x
       echo ' '
       echo '***************************************** '
       echo "*** ERR : No $COMINENS/$wndIE.$cycle.$sfHH file *** "
       echo '***************************************** '
       echo ' '
       [[ "$LOUD" = YES ]] && set -x
       echo "No $COMINEBC/$wndIE.$cycle.$sfHH " >> $wavelog
       exit 1
     fi

# 1.b Extract input data from grib files
# 
# Get TMP components
#     $WGRIB2 wind.grib -order we:ns -match "TMP" -match "2 m above" -text T1.$fymdh > dum
     $WGRIB2 wind.grib -match "TMP" -match "2 m above" -text T1.$fymdh > dum

# Get UGRD components
#     $wgrib2 wind.grib -order we:ns -match UGRD -match "10 m above" -text UU.$fymdh > dum
     $WGRIB2 wind.grib -match UGRD -match "10 m above" -text UU.$fymdh > dum

# Get VGRD components
#     $wgrib2 wind.grib -order we:ns -match VGRD -match "10 m above" -text VV.$fymdh > dum
     $WGRIB2 wind.grib -match VGRD -match "10 m above" -text VV.$fymdh > dum

# Prepare for 3h data till 192h
    if [ ${fhour} -lt 192 ] && [ ${fhour} -ge 0 ]
    then
      fhour=`expr $fhour + 3`
    else
      fhour=`expr $fhour + 6`
    fi
    fymdh=`$NDATE $fhour $fymdh0`

  done

  cycle=${CYCrun}
#
# --------------------------------------------------------------------------- #
# 2. Generate WW3 wind input file
#
# 2.a Convert grib2 data to wind file in WW3-friendly format
#
  set +x
  echo ' '
  echo '   Extract wind fields from spectral files ...'
  [[ "$LOUD" = YES ]] && set -x
#
# Get into single file (${MDC}gfs)
#
     rm -f gfsinput

     echo "$IDIML, $JDIML,$JCLIP"                              > gfsinput
     echo "$time_beg $time_end"                               >> gfsinput

     cat gfsinput | $EXECwave/wavegfsens
     err=$?

     if [ "$err" != '0' ]
     then
       msg="ABNORMAL EXIT: ERROR IN ${MDC}gfs"
       ../postmsg "$jlogfile" "$msg"
       set +x
       echo ' '
       echo '****************************************** '
       echo "*** FATAL ERROR : ERROR IN ${MDC}gfs *** "
       echo '****************************************** '
       echo ' '
       [[ "$LOUD" = YES ]] && set -x
       echo "$modIE prep $ymd $cycle : error in ${MDC}gfs." >> $wavelog
       exit 2
     fi
#
     if [ ! -f gfs.wind ]
     then
       msg="ABNORMAL EXIT: FILE gfs.wind MISSING"
       ../postmsg "$jlogfile" "$msg"
       set +x
       echo ' '
       cat ${MDC}gfs.out
       echo ' '
       echo '****************************************'
       echo '*** FATAL ERROR : gfs.wind NOT FOUND ***'
       echo '****************************************'
       echo ' '
       echo "$modIE prep $ymd $cycle : gfs.wind missing." >> $wavelog
       [[ "$LOUD" = YES ]] && set -x
       exit 3
     fi
#
  if [ "${modIE}" == "gwes00" ]
  then
    # Copy sst file to com for later archiving
    cp sst.ww3 ${COMOUT}/rundata/${MDC}.${wndID}.t${cyc}z.sst
  fi

  rm -f sst.ww3
  rm -f swnd.*
#
# 2.b Run waveprep
#
  set +x
  echo ' '
  echo '   Running wind fields through preprocessor.'
  [[ "$LOUD" = YES ]] && set -x

  sed -e "s/HDRFL/T/g" ../waveprep.$wndID.tmpl > ww3_prep.inp

  echo "Executing $EXECcode/ww3_prep"

  $EXECcode/ww3_prep
  err=$?

# 2.c Check for errors

  if [ "$err" != '0' ]
  then
    msg="ABNORMAL EXIT: ERROR IN waveprep"
    ../postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    echo '*************************************** '
    echo '*** FATAL ERROR : ERROR IN waveprepens *** '
    echo '*************************************** '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$modIE prep $ymd $cycle : error in waveprepens." >> $wavelog
    exit 4
  fi
#
  if [ ! -f wind.ww3 ]
  then
    msg="ABNORMAL EXIT: FILE wind.ww3 MISSING"
    ../postmsg "$jlogfile" "$msg"
    set +x
    echo ' '
    cat waveprep.out
    echo ' '
    echo '****************************************'
    echo '*** FATAL ERROR : wind.ww3 NOT FOUND ***'
    echo '****************************************'
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "$modIE prep $ymd $cycle : wind.ww3 missing." >> $wavelog
    exit 5
  fi

# --------------------------------------------------------------------------- #
# 3. Cleanup
#
  rm -f gfs.wind
  rm -f ww3_prep.inp
  rm -f mod_def.ww3

#
# --------------------------------------------------------------------------- #
# End of wave_gwes_wind.sh
