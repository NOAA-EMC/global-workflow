#! /usr/bin/env bash

# CSD - THIS IS THE REGRIDDING

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_sfcanl.sh
# Script description:  Makes global model surface analysis files
#
# Author: Russ Treadon      Org: NCEP/EMC     Date: 2021-12-13
#
# Abstract: This script makes global model surface analysis files
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

#  Set environment.

# Derived base variables

# Dependent Scripts and Executables
REGRIDSH=${REGRIDSH:-"${USHgfs}/regrid_gsiSfcIncr_to_tile.sh"}

ntiles=6

# Collect the dates in the window to update surface restarts
# CSD - this code is repeated in the re-gridding?
soilinc_fhrs=("${assim_freq}") # increment file at middle of window
LFHR="${assim_freq}"
if [[ "${DOIAU:-}" == "YES" ]]; then # Update surface restarts at beginning of window
    half_window=$((assim_freq / 2))
    soilinc_fhrs+=("${half_window}")
    LFHR=-1
fi

# if doing GSI soil anaysis, copy increment file and re-grid it to native model resolution
if [[ "${DO_GSISOILDA}" == "YES" ]]; then

    export COMIN_SOIL_ANALYSIS_MEM="${COMIN_ATMOS_ENKF_ANALYSIS_STAT}"
    export COMOUT_ATMOS_ANALYSIS_MEM="${COMIN_ATMOS_ANALYSIS}"
    export CASE_IN="${CASE_ENS}"
    export CASE_OUT="${CASE}"
    export OCNRES_OUT="${OCNRES}"
    export LFHR

    "${REGRIDSH}"
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "Soil increment file was not regridded correctly!"
    fi

fi

################################################################################

exit "${err}"

################################################################################
