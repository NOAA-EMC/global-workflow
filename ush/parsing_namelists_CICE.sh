#! /usr/bin/env bash

# parsing namelist of CICE

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
# setup_nml section
local SYEAR=${current_cycle:0:4}
local SMONTH=${current_cycle:4:2}
local SDAY=${current_cycle:6:2}
local SECS=${sec}
local DT_ICE=${ICETIM}
local NPT=${npt}  # TBT [To be templated in ice_in_template; UFSWM sets to 999]
local CICERUNTYPE=${runtype}
local RUNID="unknown"
local USE_RESTART_TIME=${use_restart_time}
local RESTART_DIR="./CICE_RESTART/"  # TBT [UFSWM sets to './RESTART/']
local RESTART_FILE="cice_model.res"  # TBT [UFSWM sets to 'iced']
local DUMPFREQ=${dumpfreq}
local DUMPFREQ_N=${dumpfreq_n}
local DIAG_FREQ=6  # The template variable should be DIAGFREQ, not DIAG_FREQ (it goes against other variable conventions used)
local HISTFREQ_N="0, 0, ${FHOUT}, 1, 1"  # TBT [UFSWM sets to '0, 0, 6, 1, 1']
local CICE_HIST_AVG=${cice_hist_avg}
local HISTORY_DIR="./CICE_OUTPUT/"  # TBT [UFSWM sets to './history/']
local INCOND_DIR="./CICE_OUTPUT/"  # TBT [UFSWM sets to './history/']
# grid_nml section
local CICEGRID="${ice_grid_file}"
local CICEMASK="${ice_kmt_file}"
local GRIDATM="???"  # What are the global-workflow definitions for these? What are CICE defaults
local GRIDOCN="???"  # What are the global-workflow definitions for these? What are CICE defaults
local GRIDICE="???"  # What are the global-workflow definitions for these? What are CICE defaults
# tracer_nml section
local TR_POND_LVL=${tr_pond_lvl}  # TBT [UFSWM sets to '.true.']
local RESTART_POND_LVL=${restart_pond_lvl}  # TBT [UFSWM sets to '.false.']
# thermo_nml section
local KTHERM=${ktherm}
# dynamics_nml section
# NONE
# shortwave_nml section
# NONE
# ponds_nml section
# NONE
# snow_nml section
# NONE
# forcing_nml section
local FRAZIL_FWSALT=${FRAZIL_FWSALT}
local TFREEZE_OPTION=${tfrz_option}
# domain_nml section
local NPROC_ICE=${ntasks_cice6}
#local NX_GLB=${NX_GLB}
#ocal NY_GLB=${NY_GLB}
local BLCKX=${block_size_x}
local BLCKY=${block_size_y}
local CICE_DECOMP=${processor_shape}

# Ensure the template exists
template="${HOMEgfs}/parm/ufs/ice_in_template"
if [[ ! -f ${template} ]]; then
  echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
  exit 1
fi
source "${HOMEgfs}/ush/atparse.bash"
rm -f "${DATA}/ice_in"
atparse < "${template}" >> "${DATA}/ice_in"
echo "Rendered ice_in:"
cat ice_in

}
