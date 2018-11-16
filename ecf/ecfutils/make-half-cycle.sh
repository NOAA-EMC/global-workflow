#! /bin/sh

if [[ "$#" != 1 && "$#" != 2 ]] ; then
    echo "Syntax: make-half-cycle.sh half-cycle [prior-cycle]"
fi

set -xu

if [[ "$#" -gt 1 ]] ; then
    ecflow_client --force=complete recursive $2/gfs
    ecflow_client --force=complete recursive $2/gdas
fi

ecflow_client --force=complete recursive $1/gfs
ecflow_client --force=complete recursive $1/gdas/prep
ecflow_client --force=complete recursive $1/gdas/analysis
ecflow_client --force=complete recursive $1/gdas/post_processing
ecflow_client --force=complete recursive $1/gdas/gempak
ecflow_client --force=complete recursive $1/gdas/enkf/jgdas_enkf_select_obs
ecflow_client --force=complete recursive $1/gdas/enkf/innovate
ecflow_client --force=complete recursive $1/gdas/enkf/jgdas_enkf_update
ecflow_client --force=complete recursive $1/gdas/enkf/jgdas_enkf_inflate_recenter
ecflow_client --force=complete recursive $1/gdas/jgdas_emc_dump_waiter
ecflow_client --force=complete recursive $1/gdas/prep
