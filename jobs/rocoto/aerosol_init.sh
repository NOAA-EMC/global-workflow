#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
source $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base aerosol_init"
for config in $configs; do
    source $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

###############################################################
# Source machine runtime environment
source $BASE_ENV/${machine}.env aerosol_init
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


exit 0
