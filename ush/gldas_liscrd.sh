#! /usr/bin/env bash

source "${HOMEgfs:?}/ush/preamble.sh"

if [ $# -lt 3 ]; then
  echo usage $0 yyyymmddhh1 yyyymmddhh2 126/382/574/1534
  exit $?
fi

date1=$1
date2=$2
grid=$3

export yyyy1=$(echo $date1 | cut -c 1-4)
export   mm1=$(echo $date1 | cut -c 5-6)
export   dd1=$(echo $date1 | cut -c 7-8)
export   hh1=$(echo $date1 | cut -c 9-10)
export yyyy2=$(echo $date2 | cut -c 1-4)
export   mm2=$(echo $date2 | cut -c 5-6)
export   dd2=$(echo $date2 | cut -c 7-8)
export   hh2=$(echo $date2 | cut -c 9-10)
export  grid=$grid
export PARM_LM=${PARMgldas}
export LISCARD=lis.crd

rm -f $LISCARD
touch $LISCARD
cat $PARM_LM/lis.crd.T${grid}.tmp.1 >> $LISCARD
echo "LIS%t%SSS        = 0     "      >> $LISCARD
echo "LIS%t%SMN        = 00    "      >> $LISCARD
echo "LIS%t%SHR        = $hh1  "      >> $LISCARD
echo "LIS%t%SDA        = $dd1  "      >> $LISCARD
echo "LIS%t%SMO        = $mm1  "      >> $LISCARD
echo "LIS%t%SYR        = $yyyy1"      >> $LISCARD
echo "LIS%t%ENDCODE    = 1     "      >> $LISCARD
echo "LIS%t%ESS        = 0     "      >> $LISCARD
echo "LIS%t%EMN        = 00    "      >> $LISCARD
echo "LIS%t%EHR        = $hh2  "      >> $LISCARD
echo "LIS%t%EDA        = $dd2  "      >> $LISCARD
echo "LIS%t%EMO        = $mm2  "      >> $LISCARD
echo "LIS%t%EYR        = $yyyy2"      >> $LISCARD
cat $PARM_LM/lis.crd.T${grid}.tmp.2   >> $LISCARD

exit 0
