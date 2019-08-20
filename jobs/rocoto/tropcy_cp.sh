#!/bin/bash

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# Source relevant configs
configs="base prep prepbufr relocate"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

#Sourcing of relocation config will copy tcvitals file into COMOUT

################################################################################
# Exit out cleanly
exit 0
