#! /usr/bin/env bash

#####
## This script writes ufs.configure file based on a template defined in
## ${ufs_configure_template}
#####

# Disable variable not used warnings
# shellcheck disable=SC2034
UFS_configure() {

echo "SUB ${FUNCNAME[0]}: ufs.configure begins"

# Setup ufs.configure
local esmf_logkind=${esmf_logkind:-"ESMF_LOGKIND_MULTI"} #options: ESMF_LOGKIND_MULTI_ON_ERROR, ESMF_LOGKIND_MULTI, ESMF_LOGKIND_NONE
local DumpFields=${DumpFields:-false}
local cap_dbug_flag=${cap_dbug_flag:-0}

# Determine "cmeps_run_type" based on the availability of the mediator restart file
# If it is a warm_start, we already copied the mediator restart to DATA, if it was present
# If the mediator restart was not present, despite being a "warm_start", we put out a WARNING
# in forecast_postdet.sh function CMEPS_postdet
if [[ -f "${DATA}/ufs.cpld.cpl.r.nc" ]]; then
  local cmeps_run_type='continue'
else
  local cmeps_run_type='startup'
fi

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

local WRITE_ENDOFRUN_RESTART=.false.

if [[ "${cplflx}" = ".true." ]]; then

  local use_coldstart=${use_coldstart:-".false."}
  local use_mommesh=${USE_MOMMESH:-"true"}

  local ocn_model="mom6"
  local ocn_petlist_bounds="${ATMPETS} $(( ATMPETS+OCNPETS-1 ))"
  local ocn_omp_num_threads="${OCNTHREADS}"
  local RUNTYPE="${cmeps_run_type}"
  local CMEPS_RESTART_DIR="CMEPS_RESTART/"
  local CPLMODE="${cplmode}"
  local coupling_interval_fast_sec="${CPL_FAST}"
  local RESTART_N=999999
  local ocean_albedo_limit=0.06
  local ATMTILESIZE="${CASE:1}"
  local ocean_albedo_limit=0.06
  local pio_rearranger=${pio_rearranger:-"box"}
  local MED_history_n=1000000 

  local histaux_enabled=".false."
fi

if [[ "${cplice}" = ".true." ]]; then

  local ice_model="cice6"
  local ice_petlist_bounds="$(( ATMPETS+OCNPETS )) $(( ATMPETS+OCNPETS+ICEPETS-1 ))"
  local ice_omp_num_threads="${ICETHREADS}"
  local FHMAX="${FHMAX_GFS}"  # TODO:  How did this get in here hard-wired to FHMAX_GFS?
fi

if [[ "${cplwav}" = ".true." ]]; then

  local wav_model="ww3"
  local wav_petlist_bounds="$(( ATMPETS+OCNPETS+ICEPETS )) $(( ATMPETS+OCNPETS+ICEPETS+WAVPETS-1 ))"
  local wav_omp_num_threads="${WAVTHREADS}"

  local WW3_user_histname="false"
  local WW3_historync="false"
  local WW3_restartnc="true"
  local WW3_PIO_FORMAT="pnetcdf"
  local WW3_PIO_IOTASKS=-99
  local WW3_PIO_STRIDE=4
  local WW3_PIO_REARR="box"
  local WW3_PIO_ROOT=-99

fi

if [[ "${cplchm}" = ".true." ]]; then

  local chm_model="gocart"
  local chm_petlist_bounds="0 $(( CHMPETS-1 ))"
  local chm_omp_num_threads="${CHMTHREADS}"
  local coupling_interval_sec="${CPL_FAST}"

fi

#Set ESMF_THREADING variable for ufs configure 
if [[ "${USE_ESMF_THREADING}" = "YES" ]]; then 
  local ESMF_THREADING="true" 
else
  local ESMF_THREADING="false" 
fi

# Ensure the template exists
if [[ ! -r "${ufs_configure_template}" ]]; then
  echo "FATAL ERROR: template '${ufs_configure_template}' does not exist, ABORT!"
  exit 1
else
  echo "INFO: using ufs.configure template: '${ufs_configure_template}'"
fi

rm -f "${DATA}/ufs.configure"
atparse < "${ufs_configure_template}" >> "${DATA}/ufs.configure"
echo "Rendered ufs.configure:"
cat ufs.configure

${NCP} "${HOMEgfs}/sorc/ufs_model.fd/tests/parm/fd_ufs.yaml" fd_ufs.yaml

echo "SUB ${FUNCNAME[0]}: ufs.configure ends"

}
