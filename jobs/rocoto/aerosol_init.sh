#!/bin/bash

set -x

###############################################################
# Source FV3GFS workflow modules
. $USHgfs/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base aerosol_init"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
. $BASE_ENV/${machine}.env aerosol_init
status=$?
[[ $status -ne 0 ]] && exit $status

$HOMEgfs/scripts/exgfs_aero_init_aerosol.py

status=$?
if [[ $status -ne 0 ]]; then
    echo "FATAL ERROR: exgfs_chem_init_aerosol.py failed with error code $status"
    exit $status
fi

##############################################################
# Exit cleanly

set +x
exit 0
