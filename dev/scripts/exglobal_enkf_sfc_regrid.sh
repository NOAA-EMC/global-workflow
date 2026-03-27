#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_sfc.sh
# Script description:  generate ensemble surface analyses on tiles
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script generates ensemble surface analyses on tiles
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
REGRIDSH=${REGRIDSH:-"${USHgfs}/regrid_gsiSfcIncr_to_tile.sh"}

# Executables.

# Files.

# Variables
NMEM_ENS_MAX=${NMEM_ENS:-80}
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
