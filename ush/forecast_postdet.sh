#! /usr/bin/env bash

#===============================================================================
#
#   FILE: forecast_postdet.sh
#
#   DESCRIPTION: Sources component-specific forecast post-determination scripts.
#                Each script defines handler functions for a specific UFS component:
#                FV3 (Atmosphere), WW3 (Waves), MOM6 (Ocean), CICE (Sea Ice),
#                GOCART (Aerosols), and CMEPS (Coupler/Mediator)

# shellcheck disable=SC1091
source "${USHglobal}/forecast_postdet_fv3.sh"
source "${USHglobal}/forecast_postdet_ww3.sh"
source "${USHglobal}/forecast_postdet_mom6.sh"
source "${USHglobal}/forecast_postdet_cice.sh"
source "${USHglobal}/forecast_postdet_gocart.sh"
source "${USHglobal}/forecast_postdet_cmeps.sh"
