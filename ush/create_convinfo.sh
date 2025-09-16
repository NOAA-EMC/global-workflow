#! /usr/bin/env bash
# create global_convinfo file for a given date
date=$1
# 2m ob use file (DO_GSISOILDA)
if [ -z ${2} ]; then
   use2mobs="NO" # NO if not present
else
   use2mobs=$2
fi
cd "${BUILD_GSINFO_DIR}/convinfo" || exit
usedate=""
for datex in [1-2]*; do
   if [[ ${date} -ge ${datex} ]]; then
      usedate=$datex
   fi
done
# cat matching date file, or quit if date not found
if [[ ${usedate} != "" ]]; then
    if [[ ${use2mobs} == "YES" ]]; then
        # turn on 2m t,q obs over land
        sed -e "s/t        181    0   -1/t        181    0    1/g" \
            -e "s/t        183    0   -1/t        183    0    1/g" \
            -e "s/t        187    0   -1/t        187    0    1/g" \
            -e "s/q        181    0   -1/q        181    0    1/g" \
            -e "s/q        183    0   -1/q        183    0    1/g" \
            -e "s/q        187    0   -1/q        187    0    1/g" ${usedate}
    else
        cat ${usedate}
    fi
else
    echo "date not found"
    exit 1
fi
