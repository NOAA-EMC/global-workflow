#!/bin/bash
###############################################################################
#                                                                             #
# This script is the postprocessor for the Global-workflow  WW3 wave model.   #
#  it sets some shell script variables for export to child scripts and copies #
# some generally used files to the work directory. After this the actual      #
# postprocessing is performed by the following child scripts :                #
#                                                                             #
#  ???.sh              : generates GRIB2 files.                               #
#  ???.sh              : generates spectral data files for output             #
#                             locations.                                      #
#                                                                             #
# Remarks :                                                                   #
# - ??? above scripts are (mostly) grdID using mpiserial or cfp.              #
#   ??? script grdIDs in its own directory created in DATA. If all is well    #
#   ...............                                                           #
#                                                                             #
# Origination  : Mar 2000                                                     #
#                                                                             #
# Update log                                                                  #
#  May2008 S Lilly:  - add logic to make sure that all of the "               #
#                     data produced from the restricted ECMWF"                #
#                     data on the CCS is properly protected."                 #
# Oct 2011 D Stokes: - specific version for wave_multi_1."                    #
#                     Use grib2 for akw, wna and enp."                        #
# Jan 2020 RPadilla, JHAlves                                                  #
#                    - Merging wave scripts to global-workflow                #
#                                                                             #
###############################################################################
#

echo "----------------------------------------------------"
echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
echo "----------------------------------------------------"


set -xa
#export GRIDproc=$1
export model=$1
export grdID=${model} 
export COMOUTgem=$2
#
membTAG='p'
if [ "${waveMEMB}" == "00" ]; then membTAG='c'; fi
export membTAG
ftype=${membTAG}${waveMEMB}

NAGRIB=nagrib_nc

entry=`grep "^$grdID " $NAGRIB_TABLE | awk 'index($1,"#") != 1 {print $0}'`

if [ "$entry" != "" ] ; then
  cpyfil=`echo $entry  | awk 'BEGIN {FS="|"} {print $2}'`
  garea=`echo $entry   | awk 'BEGIN {FS="|"} {print $3}'`
  gbtbls=`echo $entry  | awk 'BEGIN {FS="|"} {print $4}'`
  maxgrd=`echo $entry  | awk 'BEGIN {FS="|"} {print $5}'`
  kxky=`echo $entry    | awk 'BEGIN {FS="|"} {print $6}'`
  grdarea=`echo $entry | awk 'BEGIN {FS="|"} {print $7}'`
  proj=`echo $entry    | awk 'BEGIN {FS="|"} {print $8}'`
  output=`echo $entry  | awk 'BEGIN {FS="|"} {print $9}'`
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
while [ $fhcnt -le $FHMAX_WAV ]; do
  fhr=$(printf "%03d" $fhcnt)
  fhr3=$fhr

  echo "fhr: $fhr"
  echo "fhr3: $fhr3"
  pgm="$NAGRIB for F$fhr $model"

  GRIBIN=$COMIN/gridded/gefs.wave.${cycle}.${ftype}.global.0p25.f${fhr3}.grib2

  echo "GRIBIN: $GRIBIN"
  GEMGRD=${model}${waveMEMB}_0p25_${PDY}${cyc}f${fhr3}

  FileOut=${model}${waveMEMB}_0p25_${PDY}${cyc}f${fhr3}
# Only create file if not present in COM
  if [ ! -s ${COMOUT}/gempak/$FileOut ]; then
    NAGRIB=nagrib2
    GRIBIN_chk=${GRIBIN}.idx
    icnt=1
    while [ $icnt -lt 1000 ]
    do
      if [ -r $GRIBIN_chk ] ; then
        break
      else
        echo "Waiting for input file: $GRIBIN_chk"
        let "icnt=icnt+1"
        sleep 5
      fi
      if [ $icnt -ge $maxtries ]
      then
        msg="FATAL ERROR: aborting after waiting 1 hour for F$fhr to end."
        echo "$msg"
        export err=1; $err_chk
        exit $err
      fi
    done

    ln -s $GRIBIN grib$fhr

    echo "GRIBIN:   $GRIBIN"
    echo "GEMGRD:   $GEMGRD"
    echo "model :   $model"

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

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################

    ls -l $GEMGRD
    export err=$?;export pgm="GEMPAK CHECK FILE for $GEMGRD";err_chk
    #
    gpend
    #
    if [ $SENDCOM = "YES" ] ; then
       if [ $grdID = "ecmwf_hr" -o $grdID = "ecmwf_wave" ] ; then
         chgrp rstprod $GEMGRD
         chmod 750 $GEMGRD
       fi
       cpfs $GEMGRD $COMOUT/gempak/$GEMGRD
       if [ $SENDDBN = "YES" ] ; then
           $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
             $COMOUT/gempak/$GEMGRD
       else
         echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
       fi
    fi
  else
    set +x
    echo ' '
    echo " File ${COMOUT}/gempak/$FileOut  found, skipping generation process"
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
  fi


  if [ $fhcnt -ge $FHMAX_HF_WAV ]; then
    inc=$FHOUT_WAV
  else
    inc=$FHOUT_HF_WAV
  fi
  let fhcnt=fhcnt+inc

done

#####################################################################
# GOOD grdID
set +x
echo "**************JOB $grdID NAWIPS COMPLETED NORMALLY ON THE DELL"
echo "**************JOB $grdID NAWIPS COMPLETED NORMALLY ON THE DELL"
echo "**************JOB $grdID NAWIPS COMPLETED NORMALLY ON THE DELL"
set -x
#####################################################################


############################### END OF SCRIPT #######################
