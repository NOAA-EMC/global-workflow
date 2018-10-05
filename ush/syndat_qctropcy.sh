
set +x

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
echo "History: JUN     1997 - First implementation of this utility script"
echo "         JUL     1997 - Added tcvitals made manually by SDM; Added "
echo "                        jtwc/fnoc tcvitals                         "
echo "         MAR     2000   Converted to IBM-SP                        "
echo "         MAR     2013   Converted to WCOSS                         "
echo "                        Added option files_override which can set  "
echo "                        namelist var used for logical variable     "
echo "                        FILES in syndat_qctropcy to control final  "
echo "                        copying of records and file manipulation.  "
echo "                        (typically F for testing, otherwise not set)"
echo "                        Added dateck fallback if archive file misg."
echo "         OCT     2013   Remove defaults for parm, exec, fix and ush "
echo "                        directories.  These must now be passed in. "
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
#   FIXSYND   - path to syndat fix field directory
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
#                (Default: $FIXSYND/syndat_slmask.t126.gaussian)
#   copy_back - switch to copy updated files back to archive directory and
#                to tcvitals directory
#                (Default: YES)
#   jlogfile  - path to job log file (skipped over by this script if not
#                 passed in)
#   SENDCOM     switch  copy output files to $COMSP
#                (Default: YES)
#   files_override - switch to override default "files" setting for given run
#                (Default: not set)
#   TIMEIT   - optional time and resource reporting (Default: not set)

set -xua

ARCHSYND=${ARCHSYND:-$COMROOTp1/arch/prod/syndat}
HOMENHC=${HOMENHC:-/gpfs/?p1/nhc/save/guidance/storm-data/ncep}
TANK_TROPCY=${TANK_TROPCY:-${DCOMROOT}/us007003}

FIXSYND=${FIXSYND:-$HOMEgfs/fix/fix_am}
USHSYND=${USHSYND:-$HOMEgfs/ush}
EXECSYND=${EXECSYND:-$HOMEgfs/exec}
PARMSYND=${PARMSYND:-$HOMEgfs/parm/relo}

slmask=${slmask:-$FIXSYND/syndat_slmask.t126.gaussian}
copy_back=${copy_back:-YES}
SENDCOM=${SENDCOM:-YES}
files_override=${files_override:-""}

cd $DATA

msg="Tropical Cyclone tcvitals QC processing has begun"
set +x
echo
echo $msg
echo
set -x
echo $msg >> $pgmout
set +u
[ -n "$jlogfile" ]  && postmsg "$jlogfile" "$msg"
set -u

if [ "$#" -ne '1' ]; then
   msg="**NON-FATAL ERROR PROGRAM  SYNDAT_QCTROPCY  run date not in \
positional parameter 1"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u
   msg="**NO TROPICAL CYCLONE tcvitals processed --> non-fatal"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u

# Copy null files into "${COMSP}syndata.tcvitals.$tmmark" and
#  "${COMSP}jtwc-fnoc.tcvitals.$tmmark" so later ftp attempts will find and
#  copy the zero-length file and avoid wasting time with multiple attempts
#  to remote machine(s)
#  (Note: Only do so if files don't already exist)

   if [ $SENDCOM = YES ]; then
      [ ! -s ${COMSP}syndata.tcvitals.$tmmark ]  &&  \
       cp /dev/null ${COMSP}syndata.tcvitals.$tmmark
      [ ! -s ${COMSP}jtwc-fnoc.tcvitals.$tmmark ]  &&  \
       cp /dev/null ${COMSP}jtwc-fnoc.tcvitals.$tmmark
   fi

   exit
fi

CDATE10=$1

set +x
echo
echo "Run date is $CDATE10"
echo
set -x

year=`echo $CDATE10 | cut -c1-4`

 
#  Copy the seasonal statistics from archive directory to local
 
cp $ARCHSYND/syndat_akavit akavit; touch akavit
cp $ARCHSYND/syndat_dateck dateck
cp $ARCHSYND/syndat_stmcat.scr stmcat.scr; touch stmcat.scr
cp $ARCHSYND/syndat_stmcat stmcat; touch stmcat
cp $ARCHSYND/syndat_sthisto sthisto
cp $ARCHSYND/syndat_sthista sthista
 
touch dateck
dateck_size=`ls -l dateck  | awk '{ print $5 }'`
if [ $dateck_size -lt 10 ]; then
   msg="***WARNING: Archive run date check file not available or shorter than expected.\
  Using dummy date 1900010100 to allow code to continue"
   echo 1900010100 > dateck
   set +x
   echo -e "\n${msg}\n"
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u
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
  files_override=`echo "$files_override" | tr [a-z] [A-Z] | tr -d [.] | cut -c 1`
  if [ "$files_override" = 'T' -o "$files_override" = 'F' ]; then
    msg="***WARNING: Variable files setting will be overriden from $files to $files_override. Override expected if testing." 
    files=$files_override
  else
    msg="***WARNING: Invalid attempt to override files setting. Will stay with default for this job" 
  fi 
  set +x
  echo -e "\n${msg}\n"
  set -x
  echo $msg >> $pgmout
  set +u
  [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
  set -u
fi

echo " &INPUT  RUNID = '${net}_${tmmark}_${cyc}', FILES = $files " > vitchk.inp
cat $PARMSYND/syndat_qctropcy.${RUN}.parm >> vitchk.inp
 
#  Copy the fixed fields from FIXSYND
 
cp $FIXSYND/syndat_fildef.vit fildef.vit
cp $FIXSYND/syndat_stmnames stmnames


rm -f nhc fnoc gtsbtab gtsbufr human.btab lthistry


#########################################################################

#  There are five possible sources of tropical cyclone bogus messages
#  All are input to program syndat_qctropcy
#  ------------------------------------------------------------------

if [ -s $HOMENHC/tcvitals ]; then
   echo "tcvitals found" >> $pgmout
   cp $HOMENHC/tcvitals nhc
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


##cp $???????????????? gtsbtab


##cp $???????????????? gtsbufr


# Manual ... copy into working directory as human.btab; copy to archive
cp $TANK_TROPCY/maksynrc human.btab
touch human.btab
[ "$copy_back" = 'YES' ]  &&  cat human.btab >> $ARCHSYND/syndat_tcvitals.$year
cp -p human.btab human.btab.ORIG

if [ $SENDDBN = YES ]; then
  $DBNROOT/bin/dbn_alert MODEL SYNDAT_TCVITALS $job $ARCHSYND/syndat_tcvitals.$year
fi

#########################################################################


cp $slmask slmask.126
 
 
#  Execute program syndat_qctropcy

pgm=`basename $EXECSYND/syndat_qctropcy`
export pgm
if [ -s prep_step ]; then
   set +u
   . prep_step
   set -u
else
   [ -f errfile ] && rm errfile
   unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi

echo "$CDATE10"      > cdate10.dat
export FORT11=slmask.126
export FORT12=cdate10.dat
$EXECSYND/syndat_qctropcy >> $pgmout 2> errfile
errqct=$?
###cat errfile
cat errfile >> $pgmout
rm errfile
set +x
echo
echo "The foreground exit status for SYNDAT_QCTROPCY is " $errqct
echo
set -x
if [ "$errqct" -gt '0' ];then
   msg="**NON-FATAL ERROR PROGRAM  SYNDAT_QCTROPCY  RETURN CODE $errqct"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u
   msg="**NO TROPICAL CYCLONE tcvitals processed --> non-fatal"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u

# In the event of a ERROR in PROGRAM SYNDAT_QCTROPCY, copy null files into
#  "${COMSP}syndata.tcvitals.$tmmark" and "${COMSP}jtwc-fnoc.tcvitals.$tmmark"
#  so later ftp attempts will find and copy the zero-length file and avoid
#  wasting time with multiple attempts to remote machine(s)
#  (Note: Only do so if files don't already exist)

   if [ $SENDCOM = YES ]; then
      [ ! -s ${COMSP}syndata.tcvitals.$tmmark ]  &&  \
       cp /dev/null ${COMSP}syndata.tcvitals.$tmmark
      [ ! -s ${COMSP}jtwc-fnoc.tcvitals.$tmmark ]  &&  \
       cp /dev/null ${COMSP}jtwc-fnoc.tcvitals.$tmmark
   fi

   exit
fi
set +x
echo
echo "----------------------------------------------------------"
echo "**********  COMPLETED PROGRAM syndat_qctropcy   **********"
echo "----------------------------------------------------------"
echo
set -x

if [ -s current ]; then
   msg="program  SYNDAT_QCTROPCY  completed normally - tcvitals records \
processed"
else
msg="no records available for program  SYNDAT_QCTROPCY - null tcvitals file \
produced"
fi
set +u
[ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
set -u


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
      cp nhc $HOMENHC/tcvitals
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
      set -x
      echo $msg >> $pgmout
      set +u
      [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
      set -u
   fi

else

   msg="Previous NHC Synthetic Data Record File $HOMENHC/tcvitals \
not changed by syndat_qctropcy"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u

fi


diff human.btab human.btab.ORIG > /dev/null
errdiff=$?

###############################################################################
#  Update Manual file in $TANK_TROPCY if it has been changed by syndat_qctropcy
###############################################################################

if test "$errdiff" -ne '0'
then

   if [ "$copy_back" = 'YES' -a ${envir} = 'prod' ]; then
      cp human.btab $TANK_TROPCY/maksynrc
      err=$?

      if test "$err" -ne '0'
      then
         msg="###ERROR: Previous Manual Synthetic Data Record File \
$TANK_TROPCY/maksynrc not updated by syndat_qctropcy"
      else
         msg="Previous Manual Synthetic Data Record File $TANK_TROPCY/maksynrc \
successfully updated by syndat_ qctropcy"
      fi
      set +x
      echo
      echo $msg
      echo
      set -x
      echo $msg >> $pgmout
      set +u
      [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
      set -u
   fi

else

   msg="Previous Manual Synthetic Data Record File $TANK_TROPCY/maksynrc \
not changed by syndat_qctropcy"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u

fi

###################################


#  This is the file that connects to the later RELOCATE and/or PREP scripts
[ $SENDCOM = YES ]  &&  cp current ${COMSP}syndata.tcvitals.$tmmark

#  Create the DBNet alert 
if [ $SENDDBN = "YES" ]
then
   $DBNROOT/bin/dbn_alert MODEL GDAS_TCVITALS $job ${COMSP}syndata.tcvitals.$tmmark
fi
    
#  Write JTWC/FNOC Tcvitals to /com path since not saved anywhere else
[ $SENDCOM = YES ]  &&  cp fnoc ${COMSP}jtwc-fnoc.tcvitals.$tmmark

msg="TROPICAL CYCLONE TCVITALS QC PROCESSING HAS COMPLETED FOR $CDATE10"
set +x
echo
echo $msg
echo
set -x
echo $msg >> $pgmout
echo " "  >> $pgmout
set +u
[ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
set -u

exit
