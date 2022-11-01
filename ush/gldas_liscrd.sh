#! /usr/bin/env bash

source "${HOMEgfs:?}/ush/preamble.sh"

if [[ $# -lt 3 ]]; then
  echo usage "$0" yyyymmddhh1 yyyymmddhh2 126/382/574/1534
  exit $?
fi

date1=$1
date2=$2
grid=$3

yyyy1=$(echo "${date1}" | cut -c 1-4)
mm1=$(echo "${date1}" | cut -c 5-6)
dd1=$(echo "${date1}" | cut -c 7-8)
hh1=$(echo "${date1}" | cut -c 9-10)
yyyy2=$(echo "${date2}" | cut -c 1-4)
mm2=$(echo "${date2}" | cut -c 5-6)
dd2=$(echo "${date2}" | cut -c 7-8)
hh2=$(echo "${date2}" | cut -c 9-10)

PARM_LM=${PARMgldas:?}
LISCARD=lis.crd

rm -f "${LISCARD}"
touch "${LISCARD}"
{
  cat "${PARM_LM}/lis.crd.T${grid}.tmp.1"
  echo "LIS%t%SSS        = 0     "
  echo "LIS%t%SMN        = 00    "
  echo "LIS%t%SHR        = ${hh1}  "
  echo "LIS%t%SDA        = ${dd1}  "
  echo "LIS%t%SMO        = ${mm1}  "
  echo "LIS%t%SYR        = ${yyyy1}"
  echo "LIS%t%ENDCODE    = 1     "
  echo "LIS%t%ESS        = 0     "
  echo "LIS%t%EMN        = 00    "
  echo "LIS%t%EHR        = ${hh2}  "
  echo "LIS%t%EDA        = ${dd2}  "
  echo "LIS%t%EMO        = ${mm2}  "
  echo "LIS%t%EYR        = ${yyyy2}"
  cat "${PARM_LM}/lis.crd.T${grid}.tmp.2"
} >> "${LISCARD}"

exit 0
