#! /usr/bin/env bash

# parsing namelist of CICE

# Disable variable not used warnings
# shellcheck disable=SC2034
CICE_namelists(){

# "warm_start" here refers to whether CICE model is warm starting or not.
# Per JM, in the case of the Prototypes, the sea-ice ICs were obtained from CPC.
# CPC sea-ice initial conditions are created from SIS2 sea-ice model.
# Hence, the prototypes always set this to "initial"
# in order for the CICE model to _initialize_ from the SIS2 ICs.
# However, in the SOCA cycled system, if starting from a previously cycled SOCA run,
# the CICE ICs are obtained from the previous cycle of the UFS S2S,
# so the CICE namelist should be set to "continue"
# TODO: Is there a way to interrogate the restart file to know if this is a
# SIS2 restart or a CICE restart, instead of relying on "${warm_start}"
if [[ "${warm_start}" = ".true." ]]; then
   local runtype="continue"
   local use_restart_time=".true."
else
   local runtype="initial"
   local use_restart_time=".false."
fi

# Get correct MPI options for NPROC and grid
local processor_shape=${cice6_processor_shape:-'slenderX2'}
local shape=${processor_shape#${processor_shape%?}}
local NPX=$(( ntasks_cice6 / shape )) #number of processors in x direction
local NPY=$(( ntasks_cice6 / NPX ))   #number of processors in y direction
if (( $(( NX_GLB % NPX )) == 0 )); then
  local block_size_x=$(( NX_GLB / NPX ))
else
  local block_size_x=$(( (NX_GLB / NPX) + 1 ))
fi
if (( $(( NY_GLB % NPY )) == 0 )); then
  local block_size_y=$(( NY_GLB / NPY ))
else
  local block_size_y=$(( (NY_GLB / NPY) + 1 ))
fi

local sec stepsperhr npt
sec=$(to_seconds "${current_cycle:8:2}0000")
stepsperhr=$((3600/ICETIM))
npt=$((FHMAX*stepsperhr)) # Need this in order for dump_last to work

# Prepare local variables for use in ice_in_template from UFSWM
# The ones already defined are left commented as a reminder
# setup_nml section
local SYEAR=${current_cycle:0:4}
local SMONTH=${current_cycle:4:2}
local SDAY=${current_cycle:6:2}
local SECS=${sec}
local DT_CICE=${ICETIM}
local CICE_NPT=${npt}
local CICE_RUNTYPE=${runtype}
local CICE_RUNID="unknown"
local CICE_USE_RESTART_TIME=${use_restart_time}
local CICE_RESTART_DIR="./CICE_RESTART/"
local CICE_RESTART_FILE="cice_model.res"
local CICE_ICE_IC='cice_model.res.nc'
local CICE_RESTART_DEFLATE=0
local CICE_RESTART_CHUNK=0,0
local CICE_RESTART_STRIDE=-99
local CICE_RESTART_ROOT=-99
local CICE_RESTART_REARR="box"
local CICE_RESTART_IOTASKS=-99
local CICE_RESTART_FORMAT="pnetcdf2"
local CICE_DUMPFREQ="y"  # "h","d","m" or "y" for restarts at intervals of "hours", "days", "months" or "years"
local CICE_DUMPFREQ_N=10000  # Set this to a really large value, as cice, mom6 and cmeps restart interval is controlled by ufs.configure
local CICE_DIAGFREQ=$(( 86400 / DT_CICE ))  # frequency of diagnostic output in timesteps, recommended for 1x per day
local CICE_HISTFREQ_N="0, 0, ${FHOUT_ICE}, 0, 1"
local CICE_hist_suffix="'x','x','x','x','x'"
if [[ "${RUN}" =~ "gdas" ]]; then
  local CICE_HIST_AVG=".false., .false., .false., .false., .false."   # DA needs instantaneous
else
  local CICE_HIST_AVG=".true., .true., .true., .true., .true."    # GFS long forecaset wants averaged over CICE_HISTFREQ_N
fi
local CICE_HISTORY_FORMAT="pnetcdf2"
local CICE_HISTORY_DIR="./CICE_OUTPUT/"
local CICE_INCOND_DIR="./CICE_OUTPUT/"
local CICE_HISTORY_IOTASKS=-99
local CICE_HISTORY_REARR="box"
local CICE_HISTORY_ROOT=-99
local CICE_HISTORY_STRIDE=-99
local CICE_HISTORY_CHUNK=0,0
local CICE_HISTORY_DEFLATE=0
local CICE_HISTORY_PREC=4
# grid_nml section
# CICE_GRID
# CICE_MASK
local CICE_GRIDATM="A"  # A-grid for atmosphere (FV3)
local CICE_GRIDOCN="A"  # A-grid for ocean (MOM6)
local CICE_GRIDICE="B"  # B-grid for seaice (CICE6)
# tracer_nml section
local CICE_TR_POND_LVL=".true."  # Use level melt ponds
# (if CICE_TR_POND_LVL=true):
  #   -- if true, initialize the level ponds from restart (if runtype=continue)
  #   -- if false, re-initialize level ponds to zero (if runtype=initial or continue)
local CICE_RESTART_POND_LVL=".false."  # Restart level ponds from restart file (if runtype=continue)
# thermo_nml section
local CICE_KTHERM=2  # 0=zero-layer thermodynamics, 1=fixed-salinity profile, 2=mushy thermodynamics
# dynamics_nml section
# NONE
# shortwave_nml section
# NONE
# ponds_nml section
# NONE
# snow_nml section
# NONE
# forcing_nml section
local CICE_FRAZIL_FWSALT=${FRAZIL_FWSALT:-".true."}
local CICE_TFREEZE_OPTION=${tfrz_option:-"mushy"}
# domain_nml section
local CICE_NPROC=${ntasks_cice6}
# NX_GLB
# NY_GLB
local CICE_BLCKX=${block_size_x}
local CICE_BLCKY=${block_size_y}
local CICE_DECOMP=${processor_shape}
# ice_prescribed_nml section 
local CICE_PRESCRIBED="false"
local MESH_DICE="none"
local stream_files_dice="none"



# Ensure the template exists
local template=${CICE_TEMPLATE:-"${PARMgfs}/ufs/ice_in.IN"}
if [[ ! -f "${template}" ]]; then
  echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
  exit 1
fi
rm -f "${DATA}/ice_in"
atparse < "${template}" >> "${DATA}/ice_in"
echo "Rendered ice_in:"
cat "${DATA}/ice_in"

# Create a ice.restart_file when runtype is "continue"
# This file is not needed when runtype is "initial"
rm -f "${DATA}/ice.restart_file"
if [[ "${runtype}" == "continue" ]]; then
  echo "${DATA}/cice_model.res.nc" > "${DATA}/ice.restart_file"
fi

}
