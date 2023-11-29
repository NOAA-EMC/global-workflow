#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
## Abstract:
## Inline awips driver script
## HOMEgfs   : /full/path/to/workflow
## EXPDIR : /full/path/to/config/files
## CDATE  : current analysis date (YYYYMMDDHH)
## CDUMP  : cycle name (gdas / gfs)
## PDY    : current date (YYYYMMDD)
## cyc    : current cycle (HH)
###############################################################

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
(( status != 0 )) && exit "${status}"

export job="awips_20sh.sh"
export jobid="${job}.$$"

# TODO (#1228) - This script is doing more than just calling a j-job
#   Also, this forces us to call the config files here instead of the j-job
source "${HOMEgfs}/ush/jjob_header.sh" -e "awips" -c "base awips"

fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

###############################################################

################################################################################
echo
echo "=============== BEGIN AWIPS ==============="

for fhr3 in ${fhrlst}; do
    fhr=$(( 10#${fhr3} ))
    if (( fhr > FHMAX_GFS )); then
        echo "Nothing to process for FHR = ${fhr3}, cycle"
        continue
    fi

    fhmin=0
    fhmax=84
    if (( fhr >= fhmin && fhr <= fhmax )); then
        if ((fhr % 3 == 0)); then
            export fcsthrs=${fhr3}
            ${AWIPS20SH}
        fi
    fi

    fhmin=90
    fhmax=240
    if (( fhr >= fhmin && fhr <= fhmax )); then
        if ((fhr % 6 == 0)); then
            export fcsthrs=${fhr3}
            ${AWIPS20SH}
        fi
    fi
done


###############################################################
# Force Exit out cleanly
if [[ ${KEEPDATA:-"NO"} == "NO" ]] ; then rm -rf "${DATA}" ; fi

exit 0
