
# Script to recover JTWC Bulletins from Tank
#  $TANK_TROPCY/$ymddir/wtxtbul/tropcyc

#  Y2K VERSION --  This script can process JTWC bulletins with EITHER a
#                    2-digit year starting in column 20 or a 4-digit year
#                    starting in column 20.
# Mar 2013, DStokes - modified for WCOSS.  Added option to email developer.
# Oct 2013, DStokes - Add check of stormname length and truncate if needed 
#                     in response to recent problems with JTWC reports.
#                     Remove option to email developer.
#
#
# Positional parameters passed in:
#   1 - Run date (YYYYMMDDHH)

# Imported variables that must be passed in:
#   DATA         - path to working directory
#   pgmout       - string indicating path to for standard output file
#   EXECSYND     - path to syndat executable directory
#   TANK_TROPCY  - path to home directory containing tropical cyclone record
#                  data base

# Imported variables that can be passed in:
#   jlogfile  - path to job log file (skipped over by this script if not
#                 passed in)


set -xua

EXECSYND=${EXECSYND:-${HOMESYND}/exec}

cd $DATA

if [ "$#" -ne '1' ]; then
   msg="**NON-FATAL ERROR PROGRAM  SYNDAT_GETJTBUL  run date not in \
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

echo "Leaving sub-shell syndat_getjtbul.sh to recover JTWC Bulletins" \
 >> $pgmout
echo " " >> $pgmout

   exit
fi

CDATE10=$1

ymd=`echo $CDATE10 | cut -c3-8`
pdy=`echo $CDATE10 | cut -c1-8`
hour=`echo $CDATE10 | cut -c9-10`

echo $PDYm1
pdym1=$PDYm1

#pdym1=`sh $utilscript/finddate.sh $pdy d-1`

echo " " >> $pgmout
echo "Entering sub-shell syndat_getjtbul.sh to recover JTWC Bulletins" \
 >> $pgmout
echo " " >> $pgmout


if test $hour -eq "00"
then

# For 00Z cycle, need to go to prior day's tank
# ---------------------------------------------

   ymddir=$pdy
   jtwcdir=$TANK_TROPCY/$pdy/wtxtbul
   jtwcdirm1=$TANK_TROPCY/$pdym1/wtxtbul
else
   ymddir=$pdy
   jtwcdir=$TANK_TROPCY/$pdy/wtxtbul
fi


set +x
echo
echo "  Run date is $CDATE10"
echo
echo "  pdy is      $pdy"
echo
echo "  pdym1 is    $pdym1"
echo
echo "  ymddir is   $ymddir"
echo
set -x

find=$ymd" "$hour
echo "looking for string  $find  in $jtwcdir/tropcyc" >> $pgmout

rm -f jtwcbul
grep "$ymd $hour" $jtwcdir/tropcyc | grep JTWC > jtwcbul
if [ -s jtwcbul ]; then
   echo "String found: contents of JTWC bulletin are:" >> $pgmout
   cat jtwcbul >> $pgmout
else
   echo "String not found: no JTWC bulletins available for this run" >> $pgmout
fi

if test $hour -eq "00"
then
   grep "$ymd $hour" $jtwcdirm1/tropcyc | grep JTWC >> jtwcbul
   if [ -s jtwcbul ]; then
      echo "String found: contents of JTWC bulletin are:" >> $pgmout
      cat jtwcbul >> $pgmout
   else
      echo "String not found: no JTWC bulletins available for this run" >> $pgmout
   fi
fi

# Check for and truncate stormnames with length greater than nine characters and leave rest of record intact.
#  This spell makes no attempt to correct any other potential errors in the record format.
perl -wpi.ORIG -e 's/(^.... ... )(\S{9,9})(\S{1,})/$1$2/' jtwcbul
diff jtwcbul.ORIG jtwcbul > jtwcbul_changes.txt
if [ -s jtwcbul_changes.txt ]; then
  msg="***WARNING:  SOME JTWC VITALS SEGMENTS REQUIRED PRELIMINARY MODIFICATION!"
  [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
  echo -e "\n${msg}.  Changes follow:" >> $pgmout
  cat jtwcbul_changes.txt >> $pgmout
  echo -e "\n" >> $pgmout
fi

# Execute bulletin processing

[ -s jtwcbul ] && echo "Processing JTWC bulletin halfs into tcvitals records" >> $pgmout

pgm=`basename $EXECSYND/syndat_getjtbul`
export pgm
if [ -s prep_step ]; then
   set +u
   . prep_step
   set -u
else
   [ -f errfile ] && rm errfile
   unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi

rm -f fnoc

export FORT11=jtwcbul
export FORT51=fnoc
time -p $EXECSYND/syndat_getjtbul >> $pgmout 2> errfile
errget=$?
###cat errfile
cat errfile >> $pgmout
rm errfile
set +x
echo
echo 'The foreground exit status for SYNDAT_GETJTBUL is ' $errget
echo
set -x
if [ "$errget" -gt '0' ];then
   if [ "$errget" -eq '1' ];then
      msg="No JTWC bulletins in $jtwcdir/tropcyc, no JTWC tcvitals \
available for qctropcy for $CDATE10"
      if [ "$RUN" = 'gfs' ]; then
        if [ "$SENDSDM" = 'YES' ]; then
         export ecf_family=`echo $ECF_NAME |awk 'BEGIN {FS="/j"} {print $1}'`
         echo $msg > $COMOUT/${NET}_${RUN}.t${cyc}z.emailbody
         echo "export subject='No JTWC bulletins available for $CDATE10 $RUN run'" >$COMOUT/${NET}_${RUN}.t${cyc}z.emailvar
         # JY echo "export maillist='sdm@noaa.gov'" >> $COMOUT/${NET}_${RUN}.t${cyc}z.emailvar
         echo "export maillist=$maillist" >> $COMOUT/${NET}_${RUN}.t${cyc}z.emailvar
         ecflow_client --run ${ecf_family}/j${RUN}_jtwc_bull_email
        fi
      fi
   else
      msg="**NON-FATAL ERROR PROGRAM  SYNDAT_GETJTBUL  FOR $CDATE10 \
RETURN CODE $errget"
   fi
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
   set -u
else
   msg="program  SYNDAT_GETJTBUL  completed normally for $CDATE10, JTWC \
rec. passed to qctropcy"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
   set -u
fi
set +x
echo
echo "----------------------------------------------------------"
echo "***********  COMPLETED PROGRAM syndat_getjtbul  **********"
echo "----------------------------------------------------------"
echo
set -x

if [ "$errget" -eq '0' ];then
   echo "Completed JTWC tcvitals records are:" >> $pgmout
   cat fnoc >> $pgmout
fi

echo "Leaving sub-shell syndat_getjtbul.sh to recover JTWC Bulletins" \
 >> $pgmout
echo " " >> $pgmout

exit
