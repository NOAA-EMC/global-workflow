#! /usr/bin/env bash
# create global_convinfo file for a given date
#date=$1
cd "${FIXgfs}/gsi/build_gsinfo/convinfo" || exit
# currently using a single convinfo
cat merged_convinfo.txt
#usedate=""
#for datex in [1-2]*; do
#   if [[ ${date} -ge ${datex} ]]; then
#      usedate=$datex
#   fi
#done
## cat matching date file, or quit if date not found
#if [[ ${usedate} != "" ]]; then
#    cat ${usedate}
#else
#    echo "date not found"
#    exit 1
#fi
