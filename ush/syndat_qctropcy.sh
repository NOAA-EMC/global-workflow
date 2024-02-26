#! /usr/bin/env bash

#          SCRIPT NAME :  syndat_qctropcy.sh
#               AUTHOR :  Steven Lord/Hua-Lu pan/Dennis Keyser/Diane Stokes
#         DATE WRITTEN :  04/21/97; MODIFIED 05/27/1997 (Keyser)
#                                   MODIFIED 07/06/1997 (Keyser)
#                                   MODIFIED 03/03/2000 (Keyser)
#
#   Abstract: This script handles the pre-processing of the tcvital 
#             files that are made by NHC and other tropical
#             prediction centers by the executable syndat_qctropcy
#
#
# echo "History: JUN     1997 - First implementation of this utility script"
# echo "         JUL     1997 - Added tcvitals made manually by SDM; Added "
# echo "                        jtwc/fnoc tcvitals                         "
# echo "         MAR     2000   Converted to IBM-SP                        "
# echo "         MAR     2013   Converted to WCOSS                         "
# echo "                        Added option files_override which can set  "
# echo "                        namelist var used for logical variable     "
# echo "                        FILES in syndat_qctropcy to control final  "
# echo "                        copying of records and file manipulation.  "
# echo "                        (typically F for testing, otherwise not set)"
# echo "                        Added dateck fallback if archive file misg."
# echo "         OCT     2013   Remove defaults for parm, exec, fix and ush "
# echo "                        directories.  These must now be passed in. "
#
#
# Positional parameters passed in:
#   1 - Run date (YYYYMMDDHH)
#        (NOTE: If $tmmark below is 'tm00', then RUN date is the same as
#                CYCLE date; otherwise RUN date is earlier than CYCLE date
#                by "hh" hours where "hh" is "tmhh" in $tmmark)

# Imported variables that must be passed in:
#   envir  - processing environment (such as 'prod' or 'test')
#   DATA   - path to working directory
#   pgmout - string indicating path to for standard output file
#   NET    - string indicating network ('nam', 'gfs')
#   RUN    - string indicating run ('nam', 'ndas', 'gfs', or 'gdas')
#   cyc    - cycle hour (e.g., '00', '06', '12', or '18')
#   tmmark - string indicating relative time of run to cycle time
#             (e.g., 'tm00', 'tm03', 'tm06', 'tm09', or 'tm12')
#   COMSP  - path to both output jtwc-fnoc file and output tcvitals file (this
#             tcvitals file is read by subsequent relocation processing and/or
#             subsequent program SYNDAT_SYNDATA)
#   PARMSYND  - path to syndat parm field directory
#   EXECSYND  - path to syndat executable directory
#   USHSYND   - path to syndat ush directory

# Imported variables that can be passed in:
#   ARCHSYND  - path to syndat archive directory
#                (Default: /com/arch/${envir}/syndat)
#   HOMENHC   - path to NHC directory containing tcvitals records
#                (Default: /nhc/save/guidance/storm-data/ncep)
#   TANK_TROPCY
#             - path to home directory containing tropical cyclone record
#                data base
#                (Default: /dcom/us007003)
#   slmask    - path to t126 32-bit gaussian land/sea mask file
#                (Default: ${FIXgfs}/am/syndat_slmask.t126.gaussian)
#   copy_back - switch to copy updated files back to archive directory and
#                to tcvitals directory
#                (Default: YES)
#   files_override - switch to override default "files" setting for given run
#                (Default: not set)
#   TIMEIT   - optional time and resource reporting (Default: not set)

source "$HOMEgfs/ush/preamble.sh"

ARCHSYND=${ARCHSYND:-$COMROOTp3/gfs/prod/syndat}
HOMENHCp1=${HOMENHCp1:-/gpfs/?p1/nhc/save/guidance/storm-data/ncep}
HOMENHC=${HOMENHC:-/gpfs/dell2/nhc/save/guidance/storm-data/ncep}
TANK_TROPCY=${TANK_TROPCY:-${DCOMROOT}/us007003}

USHSYND=${USHSYND:-$HOMEgfs/ush}
EXECSYND=${EXECSYND:-$HOMEgfs/exec}
PARMSYND=${PARMSYND:-$HOMEgfs/parm/relo}

slmask=${slmask:-${FIXgfs}/am/syndat_slmask.t126.gaussian}
copy_back=${copy_back:-YES}
files_override=${files_override:-""}

cd $DATA

msg="Tropical Cyclone tcvitals QC processing has begun"
set +x
echo
echo $msg
echo
set_trace
echo $msg >> $pgmout

if [ "$#" -ne '1' ]; then
   msg="**NON-FATAL ERROR PROGRAM  SYNDAT_QCTROPCY  run date not in \
positional parameter 1"
   set +x
   echo
   echo $msg
   echo
   set_trace
   echo $msg >> $pgmout
   msg="**NO TROPICAL CYCLONE tcvitals processed --> non-fatal"
   set +x
   echo
   echo $msg
   echo
   set_trace
   echo $msg >> $pgmout

# Copy null files into "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.$tmmark" and
#  "${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.$tmmark" so later ftp attempts will find and
#  copy the zero-length file and avoid wasting time with multiple attempts
#  to remote machine(s)
#  (Note: Only do so if files don't already exist)

   if [[ ! -s "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.${tmmark}" ]]; then
       cp "/dev/null" "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.${tmmark}"
   fi
   if [[ ! -s "${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.${tmmark}" ]]; then
       cp "/dev/null" "${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.${tmmark}"
   fi

   exit
fi

CDATE10=$1

set +x
echo
echo "Run date is $CDATE10"
echo
set_trace

year=$(echo $CDATE10 | cut -c1-4)

 
#  Copy the seasonal statistics from archive directory to local
 
cp $ARCHSYND/syndat_akavit akavit; touch akavit
cp $ARCHSYND/syndat_dateck dateck
cp $ARCHSYND/syndat_stmcat.scr stmcat.scr; touch stmcat.scr
cp $ARCHSYND/syndat_stmcat stmcat; touch stmcat
cp $ARCHSYND/syndat_sthisto sthisto
cp $ARCHSYND/syndat_sthista sthista
 
touch dateck
dateck_size=$(ls -l dateck  | awk '{ print $5 }')
if [ $dateck_size -lt 10 ]; then
   msg="***WARNING: Archive run date check file not available or shorter than expected.\
  Using dummy date 1900010100 to allow code to continue"
   echo 1900010100 > dateck
   set +x
   echo -e "\n${msg}\n"
   set_trace
   echo $msg >> $pgmout
fi


#  Generate the correct RUNID and FILES value based on $NET, $RUN and $cyc
#  Copy this into parm file, then cat the remaining switches in the parm file
#     Note: FILES=T for 00Z GDAS at tm00 (last run of day centered on 00Z)
#                       06Z GDAS at tm00 (last run of day centered on 06Z)
#                       12Z GDAS at tm00 (last run of day centered on 12Z)
#                       18Z GDAS at tm00 (last run of day centered on 18Z)
 
net=$NET
files=F,
if [ "$RUN" = 'ndas' ]; then
   net=ndas
elif [ "$RUN" = 'gdas' ]; then
   files=T,
fi
if [ -n "$files_override" ]; then  # for testing, typically want FILES=F
  files_override=$(echo "$files_override" | tr [a-z] [A-Z] | tr -d [.] | cut -c 1)
  if [ "$files_override" = 'T' -o "$files_override" = 'F' ]; then
    msg="***WARNING: Variable files setting will be overriden from $files to $files_override. Override expected if testing." 
    files=$files_override
  else
    msg="***WARNING: Invalid attempt to override files setting. Will stay with default for this job" 
  fi 
  set +x
  echo -e "\n${msg}\n"
  set_trace
  echo $msg >> $pgmout
fi

echo " &INPUT  RUNID = '${net}_${tmmark}_${cyc}', FILES = $files " > vitchk.inp
cat $PARMSYND/syndat_qctropcy.${RUN}.parm >> vitchk.inp
 
#  Copy the fixed fields
 
cp ${FIXgfs}/am/syndat_fildef.vit fildef.vit
cp ${FIXgfs}/am/syndat_stmnames stmnames


rm -f nhc fnoc lthistry


#########################################################################

#  There are five possible sources of tropical cyclone bogus messages
#  All are input to program syndat_qctropcy
#  ------------------------------------------------------------------

if [ -s $HOMENHC/tcvitals ]; then
   echo "tcvitals found" >> $pgmout
   cp $HOMENHC/tcvitals nhc
elif [ -s $HOMENHCp1/tcvitals ]; then
   echo "tcvitals found" >> $pgmout
   cp $HOMENHCp1/tcvitals nhc
else
   echo "WARNING: tcvitals not found, create empty tcvitals" >> $pgmout
   > nhc
fi

# NHC ... copy into working directory as nhc; copy to archive
touch nhc
[ "$copy_back" = 'YES' ]  &&  cat nhc >> $ARCHSYND/syndat_tcvitals.$year

mv -f nhc nhc1
$USHSYND/parse-storm-type.pl nhc1 > nhc

cp -p nhc nhc.ORIG
# JTWC/FNOC ... execute syndat_getjtbul script to write into working directory
#               as fnoc; copy to archive
$USHSYND/syndat_getjtbul.sh $CDATE10
touch fnoc
[ "$copy_back" = 'YES' ]  &&  cat fnoc >> $ARCHSYND/syndat_tcvitals.$year

mv -f fnoc fnoc1
$USHSYND/parse-storm-type.pl fnoc1 > fnoc

if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL SYNDAT_TCVITALS $job $ARCHSYND/syndat_tcvitals.$year
fi

#########################################################################


cp $slmask slmask.126
 
 
#  Execute program syndat_qctropcy

pgm=$(basename $EXECSYND/syndat_qctropcy.x)
export pgm
if [ -s prep_step ]; then
   set +u
   . prep_step
   set -u
else
   [ -f errfile ] && rm errfile
   unset FORT00 $(env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}')
fi

echo "$CDATE10"      > cdate10.dat
export FORT11=slmask.126
export FORT12=cdate10.dat
${EXECSYND}/${pgm} >> $pgmout 2> errfile
errqct=$?
###cat errfile
cat errfile >> $pgmout
rm errfile
set +x
echo
echo "The foreground exit status for SYNDAT_QCTROPCY is " $errqct
echo
set_trace
if [ "$errqct" -gt '0' ];then
   msg="**NON-FATAL ERROR PROGRAM  SYNDAT_QCTROPCY  RETURN CODE $errqct"
   set +x
   echo
   echo $msg
   echo
   set_trace
   echo $msg >> $pgmout
   msg="**NO TROPICAL CYCLONE tcvitals processed --> non-fatal"
   set +x
   echo
   echo $msg
   echo
   set_trace
   echo $msg >> $pgmout

# In the event of a ERROR in PROGRAM SYNDAT_QCTROPCY, copy null files into
#  "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.$tmmark" and "${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.$tmmark"
#  so later ftp attempts will find and copy the zero-length file and avoid
#  wasting time with multiple attempts to remote machine(s)
#  (Note: Only do so if files don't already exist)

   if [[ ! -s "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.${tmmark}" ]]; then
       cp "/dev/null" "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.${tmmark}"
   fi
   if [[ ! -s ${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.${tmmark} ]]; then
       cp "/dev/null" "${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.${tmmark}"
   fi

   exit
fi
set +x
echo
echo "----------------------------------------------------------"
echo "**********  COMPLETED PROGRAM syndat_qctropcy   **********"
echo "----------------------------------------------------------"
echo
set_trace

if [ "$copy_back" = 'YES' ]; then
   cat lthistry>>$ARCHSYND/syndat_lthistry.$year
   cp akavit $ARCHSYND/syndat_akavit
   cp dateck $ARCHSYND/syndat_dateck
   cp stmcat.scr $ARCHSYND/syndat_stmcat.scr
   cp stmcat $ARCHSYND/syndat_stmcat
   cp sthisto $ARCHSYND/syndat_sthisto
   cp sthista $ARCHSYND/syndat_sthista
fi


diff nhc nhc.ORIG > /dev/null
errdiff=$?

###################################
#  Update NHC file in $HOMENHC
###################################

if test "$errdiff" -ne '0'
then

   if [ "$copy_back" = 'YES' -a ${envir} = 'prod' ]; then
      if [ -s $HOMENHC/tcvitals ]; then
         cp nhc $HOMENHC/tcvitals
      fi
      if [ -s $HOMENHCp1/tcvitals ]; then
         cp nhc $HOMENHCp1/tcvitals
      fi

      err=$?

      if [ "$err" -ne '0' ]; then
         msg="###ERROR: Previous NHC Synthetic Data Record File \
$HOMENHC/tcvitals not updated by syndat_qctropcy"
      else
         msg="Previous NHC Synthetic Data Record File \
$HOMENHC/tcvitals successfully updated by syndat_qctropcy"
      fi

      set +x
      echo
      echo $msg
      echo
      set_trace
      echo $msg >> $pgmout
   fi

else

   msg="Previous NHC Synthetic Data Record File $HOMENHC/tcvitals \
not changed by syndat_qctropcy"
   set +x
   echo
   echo $msg
   echo
   set_trace
   echo $msg >> $pgmout

fi

###################################


#  This is the file that connects to the later RELOCATE and/or PREP scripts
cp current "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.${tmmark}"

#  Create the DBNet alert 
if [ $SENDDBN = "YES" ]
then
   "${DBNROOT}/bin/dbn_alert" "MODEL" "GDAS_TCVITALS" "${job}" "${COM_OBS}/${RUN}.${cycle}.syndata.tcvitals.${tmmark}"
fi
    
#  Write JTWC/FNOC Tcvitals to /com path since not saved anywhere else
cp fnoc "${COM_OBS}/${RUN}.${cycle}.jtwc-fnoc.tcvitals.${tmmark}"

exit
