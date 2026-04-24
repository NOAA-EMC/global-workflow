#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         fcst_mgr_component.sh
# Script description:  Single-component wrapper for forecast manager MPMD runs
#
# Abstract: Called by JGLOBAL_FORECAST_MGR via "srun --multi-prog" (MPMD mode)
#           with one SLURM task allocated per active model component.  Sources
#           forecast_mgr.sh and calls fcst_mgr_wait_and_copy for the component
#           assigned to this rank.
#
# Usage:    fcst_mgr_component.sh <component> <table_file>
#             component  - short name used for logging, e.g. "atm", "ww3",
#                          "ocn", or "ice"
#             table_file - absolute path to the 4-column product table file
################################################################################

component="${1:?Usage: fcst_mgr_component.sh <component> <table_file>}"
table_file="${2:?Usage: fcst_mgr_component.sh <component> <table_file>}"

source "${USHglobal}/forecast_mgr.sh"
fcst_mgr_wait_and_copy "${table_file}" "${component}"
