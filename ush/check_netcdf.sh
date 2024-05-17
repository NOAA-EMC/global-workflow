#! /usr/bin/env bash

# shellcheck disable=SC2155,SC2312
HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
declare -rx HOMEgfs

source "${HOMEgfs}/ush/load_fv3gfs_modules.sh" 1>/dev/null 2>&1

ncfile=${1?}

# shellcheck disable=SC2086
(( $(ncdump ${file} 2> /dev/null | grep -Po '(?<=time = UNLIMITED ; // \()\d+(?= currently)' || echo 0) > 0 )) # redirect stdout and stderr to /dev/null to suppress output in cron
rc=$?
# If there is no error, rc=0, else rc!=0

exit "${rc}"
