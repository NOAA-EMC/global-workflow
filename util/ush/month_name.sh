#!/bin/ksh

####################################################################
#
#   SCRIPT:  month_name.sh
#
#   This script returns the name/abreviation of a month
#   in a small text file, month_name.txt.  It also echos the 
#   name/abreviation to stdout.  The form of the returned
#   name/abreviation is specified by the script arguments.
#
#     USAGE:  ./month_name.sh < month > < monthspec>
#
#   EXAMPLE:  ./month_name.sh     5          MON
#
#        month         spec      contents of month_name.txt
#     -----------     ------    ----------------------------
#
#        6/06          Mon                Jun
#        8/08          Month              August               
#        9/09          MON                SEP
#         11           MONTH              NOVEMBER
#
#
#   Note:  Variables may be assigned the value of the returned name
#          by either of the following methods:
#
#          MM=`cat month_name.txt`  after executing month_name.sh
#                            - OR - 
#          MM=`month_name.sh 5 MON`  (for example)
#
#
#
#   HISTORY:   07/08/2005 -  Original script
#
#
####################################################################


  typeset -Z2 month_num


  month_num=$1
  month_spec=$2

  case ${month_num} in

    01) Mon=Jan 
        Month=January   ;;

    02) Mon=Feb
        Month=February  ;;

    03) Mon=Mar
        Month=March     ;;

    04) Mon=Apr
        Month=April     ;;

    05) Mon=May
        Month=May       ;;

    06) Mon=Jun
        Month=June      ;;

    07) Mon=Jul
        Month=July      ;;

    08) Mon=Aug
        Month=August    ;;

    09) Mon=Sep
        Month=September ;;

    10) Mon=Oct
        Month=October   ;;

    11) Mon=Nov
        Month=November  ;;

    12) Mon=Dec
        Month=December  ;;

  esac


  if [ ${month_spec} = Mon ]; then

    echo ${Mon}
    echo ${Mon} > month_name.txt

  elif [ ${month_spec} = Month ]; then

    echo ${Month}
    echo ${Month} > month_name.txt

  elif [ ${month_spec} = MON ]; then

    MON=`echo ${Mon} | tr [a-z] [A-Z]`
    echo ${MON}
    echo ${MON} > month_name.txt

  elif [ ${month_spec} = MONTH ]; then

    MONTH=`echo ${Month} | tr [a-z] [A-Z]`
    echo ${MONTH}
    echo ${MONTH} > month_name.txt

  fi



