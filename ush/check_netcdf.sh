#! /usr/bin/env bash

readonly HOMEgfs=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)
declare -rx HOMEgfs

source "${HOMEgfs}/ush/load_fv3gfs_modules.sh" 1>/dev/null 2>&1

ncfile=${1?}

ncdump -h "${ncfile}"
rc=$?
# If there is no error, rc=0, else rc!=0

exit "${rc}"
