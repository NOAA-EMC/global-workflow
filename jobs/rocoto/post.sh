#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## NCEP post driver script
## FHRLST : forecast hourlist to be post-process (e.g. anl, f000, f000_f001_f002, ...)
###############################################################

# Source FV3GFS workflow modules
# . ${HOMEgfs}/ush/load_fv3gfs_modules.sh
# status=$?
# [[ ${status} -ne 0 ]] && exit ${status}
# Temporarily load modules from UPP
source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/ush/module-setup.sh"
module use "${HOMEgfs}/sorc/ufs_model.fd/FV3/upp/modulefiles"
module load orion
if [[ "${MACHINE_ID}" = "wcoss2" ]]; then
    module load prod_util
    module load cray-pals
    module load cfp
else
    module load prod-util
    export UTILROOT=${prod_util_ROOT}
fi
module load grib-util
module load wgrib2
export WGRIB2=wgrib2
# End hack

export job="post"
export jobid="${job}.$$"

fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

#---------------------------------------------------------------
for fhr in ${fhrlst}; do
    export post_times=${fhr}
    ${HOMEgfs}/jobs/JGLOBAL_ATMOS_POST
    status=$?
    [[ ${status} -ne 0 ]] && exit ${status}
done

exit 0
