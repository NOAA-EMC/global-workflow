#!/bin/ksh

######################################################################
#
#  Script verfdate.sh takes a user specified PDY, time and forecast
#  hour and computes a valid time.  The valid time is passed to the 
#  program in a local file, verfdate.txt .
#
#     USAGE: ${NWROOT}/util/ush/verfdate.sh   <PDY> <hour/cyc> <fhr>
#
#   EXAMPLE: ${NWROOT}/util/ush/verfdate.sh  20050807   12      48
#
#
#  The result can be accessed by:
#
#  cat verfdate.txt | read fy fm fd valid dayvrf monvrf
#
#    Where      fy = 4-digit year  ( eg. 2005 )
#               fm = 2-digit month  ( eg. 03 )
#               fd = 2-digit day-of-month  ( eg. 27 )
#            valid = 2-digit time of day  ( eg. 06 )
#           dayvrf = name of valid day  ( eg. FRI )
#           monvrf = name of valid month  ( eg. JUN )
#
#  NOTE:  This routine uses script ${NWROOT}/util/ush/finddate.sh
#
#
#  History:    29 AUG 2005  Ralph Jones  -  Original version
#
#
#######################################################################


  typeset -Z2 hr

# Collect arguments

  PDY=$1
  hh=$2
  fhour=$3


# Parse PDY

  yyyy=`echo $PDY | cut -c1-4`
    mm=`echo $PDY | cut -c5-6` 
    dd=`echo $PDY | cut -c7-8`


# Check for invalid day

  case $mm in
    01|03|05|07|08|10|12) ndays=31  ;;
    02) ndays=28
        let mod4=yyyy%4
        if [ $mod4 -eq 0 ]; then
          ndays=29
        fi        ;;
    04|06|09|11) ndays=30  ;;
  esac

  if [ $dd -gt $ndays ]; then
    echo "\n\n The specified day, $dd, exceeds the number of days in month $mm \n"
    echo " EXITING verfdate.sh \n"
    exit
  fi


# Compute number of days and verifying hour
 
  let term=fhour+hh

  let days=term/24
  if [ $term -lt 0 ]; then
    let "days=-1+(term+1)/24"
  fi


  if [ $days -eq 0 ]; then
    verfdate=$PDY
    hour=$term
  elif [ $days -gt 0 ]; then
    verfdate=`finddate.sh $PDY d+$days`
    let "hour=term-(days*24)"
  else
    let negdays=-1*days
    verfdate=`finddate.sh $PDY d-$negdays`
    let "hour=(negdays*24)+term"
  fi

  hr=$hour



  fy=`echo $verfdate | cut -c1-4`
  fm=`echo $verfdate | cut -c5-6`
  fd=`echo $verfdate | cut -c7-8` 
  fday=$fd
  if [ $fday -lt 10 ]; then
    fday=`echo $fday | cut -c2`
  fi



# Find name of verification month

  case $fm in
   01) monvrf=JAN
       pdays=0    ;;
   02) monvrf=FEB
       pdays=31   ;;
   03) monvrf=MAR
       pdays=59   ;;
   04) monvrf=APR
       pdays=90   ;;
   05) monvrf=MAY
       pdays=120  ;;
   06) monvrf=JUN
       pdays=151  ;;
   07) monvrf=JUL
       pdays=181  ;;
   08) monvrf=AUG
       pdays=212  ;;
   09) monvrf=SEP
       pdays=243  ;;
   10) monvrf=OCT
       pdays=273  ;;
   11) monvrf=NOV
       pdays=304  ;;
   12) monvrf=DEC
       pdays=334  ;;
    *) monvrf=UND
       pdays=999
       echo "\n\n UNDEFINED MONTH-OF-YEAR:  fm = $fm     ABORTING \n\n" 
       exit       ;;
  esac

  let mod4=fy%4
  if [ $mod4 -eq 0 -a $fm -gt 2 ]; then
    let pdays=pdays+1
  fi

  
  case $fy in
    2006|2012|2017|2023|2034|2040|2045) day1=0  ;;
    2007|2018|2024|2029|2035|2046)      day1=1  ;;
    2008|2013|2019|2030|2036|2041|2047) day1=2  ;;
    2014|2020|2025|2031|2042|2048)      day1=3  ;;
    2009|2015|2026|2032|2037|2043)      day1=4  ;;
    2010|2016|2021|2027|2038|2044|2049) day1=5  ;;
    2005|2011|2022|2028|2033|2039|2050) day1=6  ;;
  esac


# Find day-of-week and day name of verification date

  let doy=pdays+fday-1
  let "dow=(doy+day1)%7"


  case $dow in
    0) dayvrf=SUN ;;
    1) dayvrf=MON ;;
    2) dayvrf=TUE ;;
    3) dayvrf=WED ;;
    4) dayvrf=THU ;;
    5) dayvrf=FRI ;;
    6) dayvrf=SAT ;;
  esac


# Create valid time label

  echo "$fy $fm $fd $hr $dayvrf $monvrf" | tee  verfdate.txt

  echo "VALID  ${hr}Z $dayvrf $fd $monvrf $fy" >> verfdate.txt


  exit

