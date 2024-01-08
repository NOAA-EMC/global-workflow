#! /usr/bin/env bash

#####
## This script writes ufs.configure file
## first, select a "*.IN" templates based on
## $confignamevarforufs and parse values based on
## $cpl** switches.
##
## This is a child script of modular
## forecast script. This script is definition only (Is it? There is nothing defined here being used outside this script.)
#####

# Disable variable not used warnings
# shellcheck disable=SC2034
writing_ufs_configure() {

echo "SUB ${FUNCNAME[0]}: ufs.configure.sh begins"

# Setup ufs.configure
local DumpFields=${NEMSDumpFields:-false}
local cap_dbug_flag=${cap_dbug_flag:-0}
# Determine "cmeps_run_type" based on the availability of the mediator restart file
# If it is a warm_start, we already copied the mediator restart to DATA, if it was present
# If the mediator restart was not present, despite being a "warm_start", we put out a WARNING
# in forecast_postdet.sh
if [[ -f "${DATA}/ufs.cpld.cpl.r.nc" ]]; then
  local cmeps_run_type='continue'
else
  local cmeps_run_type='startup'
fi

local esmf_logkind=${esmf_logkind:-"ESMF_LOGKIND_MULTI"} #options: ESMF_LOGKIND_MULTI_ON_ERROR, ESMF_LOGKIND_MULTI, ESMF_LOGKIND_NONE

# Atm-related
local atm_model="fv3"
local atm_petlist_bounds="0 $(( ATMPETS-1 ))"
local atm_omp_num_threads="${ATMTHREADS}"

local med_model="cmeps"
local med_petlist_bounds="0 $(( MEDPETS-1 ))"
local med_omp_num_threads="${MEDTHREADS}"

if [[ "${cpl}" = ".true." ]]; then
  local coupling_interval_slow_sec="${CPL_SLOW}"
fi

if [[ "${cplflx}" = ".true." ]]; then

  local use_coldstart=${use_coldstart:-".false."}
  local use_mommesh=${USE_MOMMESH:-"true"}

  local ocn_model="mom6"
  local ocn_petlist_bounds="${ATMPETS} $(( ATMPETS+OCNPETS-1 ))"
  local ocn_omp_num_threads="${OCNTHREADS}"
  local RUNTYPE="${cmeps_run_type}"
  local CPLMODE="${cplmode}"
  local coupling_interval_fast_sec="${CPL_FAST}"
  local RESTART_N="${restart_interval}"
  local ocean_albedo_limit=0.06
  local ATMTILESIZE="${CASE:1}"
  local ocean_albedo_limit=0.06
fi

if [[ "${cplice}" = ".true." ]]; then

  local ice_model="cice6"
  local ice_petlist_bounds="$(( ATMPETS+OCNPETS )) $(( ATMPETS+OCNPETS+ICEPETS-1 ))"
  local ice_omp_num_threads="${ICETHREADS}"
  local MESH_OCN_ICE=${MESH_OCN_ICE:-"mesh.mx${ICERES}.nc"}
  local FHMAX="${FHMAX_GFS}"  # TODO:  How did this get in here hard-wired to FHMAX_GFS?
fi

if [[ "${cplwav}" = ".true." ]]; then

  local wav_model="ww3"
  local wav_petlist_bounds="$(( ATMPETS+OCNPETS+ICEPETS )) $(( ATMPETS+OCNPETS+ICEPETS+WAVPETS-1 ))"
  local wav_omp_num_threads="${WAVTHREADS}"
  local MULTIGRID="${waveMULTIGRID}"

fi

if [[ "${cplchm}" = ".true." ]]; then

  local chm_model="gocart"
  local chm_petlist_bounds="0 $(( CHMPETS-1 ))"
  local chm_omp_num_threads="${CHMTHREADS}"
  local coupling_interval_fast_sec="${CPL_FAST}"

fi

# Ensure the template exists
if [[ ! -r "${ufs_configure_template}" ]]; then
  echo "FATAL ERROR: template '${ufs_configure_template}' does not exist, ABORT!"
  exit 1
fi

source "${HOMEgfs}/ush/atparse.bash"
rm -f "${DATA}/ufs.configure"
atparse < "${ufs_configure_template}" >> "${DATA}/ufs.configure"
echo "Rendered ufs.configure:"
cat ufs.configure

${NCP} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/fd_ufs.yaml" fd_ufs.yaml

echo "SUB ${FUNCNAME[0]}: ufs.configure.sh ends for ${ufs_configure_template}"

}
