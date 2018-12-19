# UTILITY SCRIPT NAME : mkbull_ntc.sh
#              AUTHOR : David Michaud
# 
# ABSTRACT : This utility script generates GRID Bulletins from analysis
#            and forecast files.
#
# INPUT: 4 arguments are input to this script.
#         1st argument - hour list   - Fcst hour list
#         2nd argument - bullparm    - parameter input.
#         3rd argument - out         - bulletin file
#
echo "History: JAN     2000 - Modified for IBM SP"
echo "History: AUG     2005 - Converted to send the bulletins to TOC via" 
echo "                        NTC rather than using the status file"
#

set +x
hour_list=$1
bullparm=$2
out=$3
num=$#

if test $num -eq 3
then
   echo ""
   echo " Appropriate number of arguments were passed"
   echo ""
   set -x
else
   echo ""
   echo "Usage: $0 bullparm outputname"
   echo ""
   exit 16
fi

set +x
echo "########################################################"
echo "#  Begin making the VARIAN FAX CHART $out maps"
echo "########################################################"
set -x

msg="Enter Make mkbull utility."
postmsg "$jlogfile" "$msg"

export pgm=gridbull
. prep_step

##############################
# Copy Input Field to $DATA
##############################
pgbunit=11
pgbiunit=31

for i in $hour_list
do
   if test "$i" = "anl"
   then
      pgrbfile="pgrbanl"
      pgrbifile="pgrbianl"
   elif test "$i" = "sstgrb"
   then
      pgrbfile="sstgrb"
      pgrbifile="sstgrb.index"
#      cp $COMIN/${RUN}.${cycle}.${pgrbfile}  ${pgrbfile}
      cp $COMIN/${cyc}/${RUN}.${cycle}.${pgrbfile}.grib2  .
      export err=$?
      if [[ $err -ne 0 ]] ; then
         echo " File ${RUN}.${cycle}.${pgrbfile}.grib2 does not exist."
         exit $err
      fi

      $CNVGRIB -g21 ${RUN}.${cycle}.${pgrbfile}.grib2  $pgrbfile
      $GRBINDEX $pgrbfile $pgrbifile
   else
      pgrbfile="pgrbf$i"
      pgrb2file="pgrb2.1p00.f$i"
      pgrbifile="pgrbif$i"
   fi
   if test ! -f $pgrbfile
   then
      cp $COMIN/${cyc}/${RUN}.${cycle}.$pgrb2file .
      export err=$?
      if [[ $err -ne 0 ]] ; then
         echo " File ${RUN}.${cycle}.$pgrb2file does not exist."
         exit $err
      fi

      $CNVGRIB -g21 ${RUN}.${cycle}.$pgrb2file $pgrbfile
   fi

   if test ! -f $pgrbifile
   then
      $GRBINDEX $pgrbfile $pgrbifile
   fi

   eval export FORT${pgbunit}="$pgrbfile"
   eval export FORT${pgbiunit}="$pgrbifile"

   pgbunit=`expr $pgbunit + 1`
   pgbiunit=`expr $pgbiunit + 1`
done

export FORT20=$bullparm
export FORT88="$out"
 
startmsg
$EXECgfs/gridbull >> $pgmout 2> errfile
export err=$?;err_chk

##############################
# Post Files to ${COMOUTwmo}
##############################

$UTILgfs/ush/make_ntc_bull.pl WMOBH NONE KWBC NONE $DATA/$out ${COMOUTwmo}/$out

msg="mkbull_ntc.sh completed normally"
postmsg "$jlogfile" "$msg"

exit
