#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_ck_stdout.sh
# Script description:  Scan stdout files for problems reading the diagnostic
#                      files.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script scans the stdout files produced by the radmon_time 
#            executable for problems reading the diagnostic files.
#
#            This script is a child script of radmon_verf_time.sh.  The parent
#            script generates the stdout files which this script examines.
#
#
# Script history log:
# 2012-02-02  Safford  initial script
#
# Usage:  radmon_ck_stdout.sh outfile
#
#   Input script positional parameters:
#     outfile           output file name
#                       required
#
#   Imported Shell Variables:
#     SATYPE            list of satellite/instrument sources
#                       defaults to none
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#
#   Exported Shell Variables:
#     err           Last return code
#
#   Modules and files referenced:
#     scripts    : 
#
#     programs   :
#
#     fixed data : 
#
#     input data : $data_file
#
#     output data: $outfile
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#    2 Environment variable.
#    3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
####################################################################
export scr=radmon_ck_stdout.sh
msg="${scr} HAS STARTED"
postmsg "$jlogfile" "$msg"


#  Command line arguments.
outfile=${1:-${outfile:?}}

# Other variables
SATYPE=${SATYPE:-}
err=0

if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
   echo "$(date) executing $0 $* >&2"
fi
################################################################################
#  Preprocessing

outfile=$1
error_msg="PROBLEM reading diagnostic file"

for type in ${SATYPE}; do
   if [[ "$VERBOSE" = "YES" ]]; then
      echo ${type}
   fi
   if [[ -s "stdout.${type}" ]]; then
      if [[ "$VERBOSE" = "YES" ]]; then
         echo  stdout.${type}
      fi
      match=`gawk "/$error_msg/" stdout.$type`

      match_len=`echo ${#match}`
      if [[ $match_len > 0 ]]; then
         echo "${type}  ${match}" >> $outfile
      fi
   fi
done

################################################################################
#  Post processing
if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 with error code ${err} >&2
fi

msg="${scr} HAS ENDED"
postmsg "$jlogfile" "$msg"

set +x
exit ${err}
