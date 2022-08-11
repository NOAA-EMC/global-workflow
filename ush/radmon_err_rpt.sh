#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_err_rpt.sh
# Script description:  Compare the contents of error files from two different 
#                      cycles.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script compares the contents of two error files from two different
#            sets of radiance diagnostic files (which are an output from GSI runs).
#            All unique satellite instrument/channel/region combinations that appear
#            in both files are reported.
#
#            This script is run as a child script of radmon_verf_time.sh.  The parent
#            script creates/copies the error files into a temporary working 
#            directory before invoking this script.
#
#
# Usage:  radmon_err_rpt.sh file1 file2 type cycle1 cycle2 diag_rpt outfile
#
#   Input script positional parameters:
#     file1		obs, penalty, or channel error file
#                       required
#     file2		obs, penalty, or channel error file
#                       required
#     type              type of error file
#                       choices are obs, pen, chan, or cnt; required
#     cycle1		first cycle processing date
#                       yyyymmddcc format; required
#     cycle2		second cycle processing date
#                       yyyymmddcc format; required
#     diag_rpt          diagnostic report text file
#                       required
#     outfile           output file name
#                       required
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
####################################################################

echo "-->  radmon_err_rpt.sh"

#  Command line arguments.
file1=${1:-${file1:?}}
file2=${2:-${file2:?}}
type=${3:-${type:?}}
cycle1=${4:-${cycle1:?}}
cycle2=${5:-${cycle2:?}}
diag_rpt=${6:-${diag_rpt:?}}
outfile=${7:-${outfile:?}}

# Directories
HOMEradmon=${HOMEradmon:-$(pwd)}

# Other variables
VERBOSE=${VERBOSE:-NO}
err=0
RADMON_SUFFIX=${RADMON_SUFFIX}

if [[ "$VERBOSE" = "YES" ]]; then
   echo EXECUTING $0 $* >&2
   set -ax
fi


have_diag_rpt=0
if [[ -s $diag_rpt ]]; then
   have_diag_rpt=1
else
   err=1
fi
echo "have_diag_rpt = $have_diag_rpt"

#-----------------------------------------------------------------------------
#  read each line in the $file1 
#  search $file2 for the same satname, channel, and region 
#  if same combination is in both files, add the values to the output file
#  
{ while read myline; do
   echo "myline = $myline"
   bound=""

   echo $myline
   satname=`echo $myline | gawk '{print $1}'`
   channel=`echo $myline | gawk '{print $3}'`
   region=`echo $myline | gawk '{print $5}'`
   value1=`echo $myline | gawk '{print $7}'`
   bound=`echo $myline | gawk '{print $9}'`

#
#     Check findings against diag_report.  If the satellite/instrument is on the 
#     diagnostic report it means the diagnostic file file for the
#     satelite/instrument is missing for this cycle, so skip any additional
#     error checking for that source.  Otherwise, evaluate as per normal.
#

   diag_match=""
   diag_match_len=0 

   if [[ $have_diag_rpt == 1 ]]; then
      diag_match=`gawk "/$satname/" $diag_rpt`
      diag_match_len=`echo ${#diag_match}`
   fi


   if [[ $diag_match_len == 0 ]]; then  

      if [[ $type == "chan" ]]; then
         echo "looking for match for $satname and $channel"
         { while read myline2; do
            satname2=`echo $myline2 | gawk '{print $1}'`
            channel2=`echo $myline2 | gawk '{print $3}'`

            if [[ $satname == $satname2 && $channel == $channel2 ]]; then
               match="$satname  channel=  $channel" 
               echo "match from gawk = $match"
	       break;
            else 
	       match=""
            fi

         done } < $file2
     

      else
         match=`gawk "/$satname/ && /channel= $channel / && /region= $region /" $file2`
         echo match = $match

         match_len=`echo ${#match}`
         if [[ $match_len > 0 ]]; then
            channel2=`echo $match | gawk '{print $3}'`

            if [[ $channel2 != $channel ]]; then
               match=""
            fi
         fi            

      fi
      match_len=`echo ${#match}`
         
      if [[ $match_len > 0 ]]; then

         value2=`echo $match | gawk '{print $7}'`
         bound2=`echo $match | gawk '{print $9}'`

         if [[ $type == "chan" ]]; then
            tmpa="    $satname              channel= $channel"
            tmpb=""

         elif [[ $type == "pen" ]]; then
            tmpa="$satname  channel= $channel region= $region"
            tmpb="$cycle1         	$value1	$bound"

         elif [[ $type == "cnt" ]]; then
            tmpa="$satname  channel= $channel region= $region"
            tmpb="$cycle1         	$value1	$bound"

         else
            tmpa="$satname  channel= $channel region= $region"
            tmpb="$cycle1: $type= $value1"
         fi

         line1="$tmpa $tmpb"
         echo "$line1" >> $outfile

         if [[ $type != "chan" ]]; then
            tmpc=`echo $tmpa |sed 's/[a-z]/ /g' | sed 's/[0-9]/ /g' | sed 's/=/ /g' | sed 's/_/ /g' | sed 's/-/ /g'`

            if [[ $type == "pen" || $type == "cnt" ]]; then
               line2=" $tmpc $cycle2         	$value2	$bound2"
            else
               line2=" $tmpc $cycle2: $type= $value2"
            fi 

            echo "$line2" >> $outfile
         fi

         #-----------------------------------------
         # add hyperlink to warning entry
         #
         line3="   http://www.emc.ncep.noaa.gov/gmb/gdas/radiance/es_rad/${RADMON_SUFFIX}/index.html?sat=${satname}&region=${region}&channel=${channel}&stat=${type}"
         if [[ $channel -gt 0 ]]; then
            echo "$line3" >> $outfile
            echo "" >> $outfile
         fi
      fi
   fi
done } < $file1


################################################################################
#  Post processing
if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 with error code ${err} >&2
fi

echo "<--  radmon_err_rpt.sh"

set +x
exit ${err}

