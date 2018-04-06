#!/bin/sh

#####################################################################
# setpdy.sh - Generate PDY script to set date variables.
#             This utility will output a file PDY in the current
#             working directory which can be sourced in the parent
#             script to set PDYm7, PDYm6, ..., PDYm1, PDY, PDYp1,
#             PDYp2, ..., PDYp7.
# History: Jul 27, 1998 - Implement new script
#          sep 17, 2015 - Add ability to specify number of dates
# Usage:
#    setpdy.sh
#       Generate PDY script with PDYm7 to PDYp7 variables
#    setpdy.sh $num
#       Generate PDY script with PDYm${num} to PDYp${num} variables
#    setpdy.sh $num1 $num2
#       Generate PDY script with PDYm${num1} to PDYp${num2} variables
#####################################################################

# Check to be sure that cycle var is set
if [[ ! "$cycle" =~ t??z ]]; then
   err_exit "cycle variable not set or formatted incorrectly"
   exit 1
fi

case $# in
   0) dates_before_PDY=7
      dates_after_PDY=7;;
   1) if [[ "$1" =~ ^[0-9]*$ ]]; then
         dates_before_PDY=$1
         dates_after_PDY=$1
      else
         err_exit "setpdy.sh parameter(s) must be a positive integer."
         exit 1
      fi;;
   2) if [[ "$1" =~ ^[0-9]*$ ]] && [[ "$2" =~ ^[0-9]*$ ]]; then
         dates_before_PDY=$1
         dates_after_PDY=$2
      else
         err_exit "setpdy.sh parameters must be positive integers."
         exit 1
      fi;;
   *) err_exit "setpdy.sh does not accept more than two parameters.";;
esac

COMDATEROOT=${COMDATEROOT:-${COMROOT:-}}
ncepdate_file=${COMDATEROOT}/date/$cycle

# Set the PDY var if not already set
if [[ -s "$ncepdate_file" ]] ; then
    # Use ncepdate file if one exists.
    if [ -z "$PDY" ]; then
        cp $ncepdate_file ncepdate
        PDY=$(cut -c7-14 ncepdate)
    else
        sed "s/[0-9]\{8\}/$PDY/" $ncepdate_file > ncepdate
    fi
else
    # If there is no ncepdate file, use the date command:
    if [[ -z "$PDY" ]] ; then
        PDY=$( date +%Y%m%d )
    fi
    echo "DATE  $PDY${cycle:1:2}0000WASHINGTON" > ncepdate
fi

# Use grep to search through date table to find production date-line
# date-line = day minus 1, day minus 2, .. day minus $dates_before_PDY
DATElne_minus=$(finddate.sh $PDY s-${dates_before_PDY})
export err=$?; err_chk
# date-line = day plus 1, day plus 2, .. day plus $dates_after_PDY
DATElne_plus=$(finddate.sh $PDY s+${dates_after_PDY})
export err=$?; err_chk

# Cut date-line to form PDY variables
if [ -f PDY ]; then rm PDY; fi
for d in $(seq $dates_before_PDY -1 1); do
   # cut the date starting at position ($d-1)*9 from date-line
   echo "export PDYm${d}=${DATElne_minus:((($d-1)*9)):8}" >>PDY
done
echo "export PDY=$PDY" >>PDY
for d in $(seq $dates_after_PDY); do
   # cut the date starting at position ($d-1)*9 from date-line
   echo "export PDYp${d}=${DATElne_plus:((($d-1)*9)):8}" >>PDY
done

chmod u+x PDY

echo "Source PDY script to export PDYm${dates_before_PDY}, ..., PDY, ..., PDYp${dates_after_PDY} variables."
