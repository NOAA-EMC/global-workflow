#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_enkf_sfc_regrid.sh
# Script description:  regrid soil increments for ensemble surface analyses
#
# Author:        Clara Draper       Org: NCEP/EMC     Date: 2024-12-01
#
# Abstract: This script regrids soil increments for ensemble surface analyses
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

# Directories.
pwd=$(pwd)

# Base variables
DO_GSISOILDA=${DO_GSISOILDA:-"NO"}
ntiles=${ntiles:-6}

# Scripts
REGRIDSH=${REGRIDSH:-"${USHglobal}/regrid_gsiSfcIncr_to_tile.sh"}

# Executables.

# Files.

# Variables
if [[ "${RUN}" == "enkfgfs" ]]; then
    NMEM_ENS=${NMEM_ENS_GFS:-30}
else
    NMEM_ENS=${NMEM_ENS:-80}
fi
DOIAU=${DOIAU_ENKF:-"NO"}

# regrid the surface increment files
if [[ "${DO_GSISOILDA}" == "YES" ]]; then

    export CASE_IN=${CASE_ENS}
    export CASE_OUT=${CASE_ENS}
    export OCNRES_OUT=${OCNRES}
    export NMEM_REGRID=${NMEM_ENS}
    if [[ "${DOIAU}" == "YES" ]]; then
        export LFHR=3 # match BDATE
    else
        export LFHR=6 # PDYcyc
    fi

    "${REGRIDSH}" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "Failed to regrid the surface inrement file!"
    fi

fi

################################################################################

################################################################################
# Postprocessing
cd "${pwd}" || exit 1

exit "${err}"
