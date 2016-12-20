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
#            This script is a child script of radmon_verf_time.sh.  The parent
#            script creates/copies the error files into a temporary working 
#            directory before invoking this script.
#
#
# Script history log:
# 2012-02-02  Safford  initial script
#
# Usage:  radmon_err_rpt.sh file1 file2 type cycle1 cycle2 diag_rpt outfile
#
#   Input script positional parameters:
#     file1		obs, penalty, or channel error file
#                       required
#     file2		obs, penalty, or channel error file
#                       required
#     type              type of error file
#                       choises are obs, pen, or chan; required
#     cycle1		first cycle processing date
#                       yyyymmddcc format; required
#     cycle2		second cycle processing date
#                       yyyymmddcc format; required
#     diag_rpt          diagnostic report text file
#                       required
#     outfile           output file name
#                       required
#
#   Imported Shell Variables:
#
#     HOMEradmon        package's nwprod subdirectory
#                       defaults to pwd
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#
#   Exported Shell Variables:
#     err           Last return code
#
#   Modules and files referenced:
#     scripts    : 
#
#     programs   : radmon_getchgrp.pl 
#                 
#
#     fixed data : $ctlfile
#
#     input data : $file1
#                  $file2
#
#     output data: $outfile
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
####################################################################
export scr=radmon_err_rpt.sh

msg="${scr} HAS STARTED"
postmsg "$jlogfile" "$msg"

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


#-----------------------------------------------------------------------------
#  read each line in the $file1 
#  search $file2 for the same satname, channel, and region 
#  if same combination is in both files, add the values to the output file
#  
{ while read myline;do
   bound=""

   echo $myline
   satname=`echo $myline | gawk '{print $1}'`
   echo satname = $satname
   channel=`echo $myline | gawk '{print $3}'`
   echo channel = $channel
   region=`echo $myline | gawk '{print $5}'`
   echo region = $region
   value1=`echo $myline | gawk '{print $7}'`
   echo value1 = $value1
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
         match=`gawk "/$satname/ && /channel=  $channel/" $file2`
      else
         match=`gawk "/$satname/ && /channel= $channel / && /region= $region /" $file2`
         echo match = $match

         match_len=`echo ${#match}`
         if [[ $match_len > 0 ]]; then
            channel2=`echo $match | gawk '{print $3}'`
            echo channel2 = $channel2
            if [[ $channel2 != $channel ]]; then
               match=""
            fi
         fi            
         echo match = $match
      fi
      match_len=`echo ${#match}`
         
      if [[ $match_len > 0 ]]; then
         echo $match_len
         value2=`echo $match | gawk '{print $7}'`
         bound2=`echo $match | gawk '{print $9}'`

         if [[ $type == "chan" ]]; then
            tmpa="$satname  channel= $channel"
            tmpb=""

         elif [[ $type == "pen" ]]; then
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

            if [[ $type == "pen" ]]; then
               line2=" $tmpc $cycle2         	$value2	$bound2"
            else
               line2=" $tmpc $cycle2: $type= $value2"
            fi 

            echo "$line2" >> $outfile
         fi

         #----------------------------------------------------------
         #  Access the control file to deterimine channel grouping 
         #  number.  Not all sources have consecutively numbered 
         #  channels, and we need to map the channel to the correct
         #  grouping number in order to produce an accurate hyperlink.
         #
         #  Update: with the new js plotting the actual channel number
         #  can be sent.  This applies to all glb sources now; it's not
         #  yet implemented for regional sources.
         if [[ $RAD_AREA == 'glb' ]]; then
            changrp=${channel}
            echo "for glb using actual channel as changrp value"
         else 
            ctlfile="time.${satname}.ctl"
            if [[ -s ${ctlfile}.Z || -s ${ctlfile}.gz ]]; then
               uncompress ${ctlfile}.*
            fi
            changrp=`${HOMEradmon}/ush/radmon_getchgrp.pl ${ctlfile} ${channel}`
         fi
         echo changrp = $changrp

         line3="   http://www.emc.ncep.noaa.gov/gmb/gdas/radiance/esafford/${RADMON_SUFFIX}/index.html?sat=${satname}&region=${region}&channel=${changrp}&stat=${type}"
         if [[ $changrp -gt 0 ]]; then
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

msg="${scr} HAS ENDED"
postmsg "$jlogfile" "$msg"

set +x
exit ${err}

