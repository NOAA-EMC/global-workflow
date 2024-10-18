#! /usr/bin/env bash

# Disable variable not used warnings
# shellcheck disable=SC2034
MOM6_namelists(){

# MOM6 namelists generation

# ================================================================
# input.nml
# ---------
cat >> input.nml <<EOF

&MOM_input_nml
  output_directory = 'MOM6_OUTPUT/',
  input_filename = '${MOM6_RESTART_SETTING}'
  restart_input_dir = 'INPUT/',
  restart_output_dir = 'MOM6_RESTART/',
  parameter_filename = 'INPUT/MOM_input',
                       'INPUT/MOM_override'
/
EOF
#temporarily commented out until a long term solution can be found
#&nam_stochy
#  new_lscale=.true.
#EOF

#cat >> input.nml <<EOF
#/
#
#&nam_sfcperts
#/
#
#EOF

echo "Rendered input.nml:"
cat input.nml

# ================================================================
# MOM_input
# ---------
# Prepare local variables for use in MOM_input.IN from UFSWM
# The ones already defined are left commented as a reminder
# == MOM options to start from coarsed grained restarts, set to off by default
#       options only available for 05 and 1 degree grids
#       as restarts are coarsed grained/interpolated from the 0.25 degrees grid
local MOM6_INIT_FROM_Z=${MOM6_INIT_FROM_Z:-True}
local MOM6_WARMSTART_FILE=${MOM6_WARMSTART_FILE:-"none"}
local MOM6_INIT_UV=${MOM6_INIT_UV:-"zero"}
# == MOM_domains section ==
# NX_GLB
# NY_GLB
# == MOM section ==
# DT_DYNAM_MOM6
# DT_THERM_MOM6
# MOM6_THERMO_SPAN
# == MOM_grid_init section ==
local MOM6_TOPOEDITS=${TOPOEDITS}
# MOM6_ALLOW_LANDMASK_CHANGES
# == MOM_diag_mediator section ==
# MOM6_DIAG_COORD_DEF_Z_FILE
# MOM6_DIAG_MISVAL
# == MOM_diabatic_aux section ==
local MOM6_CHLCLIM=${CHLCLIM}
# == MOM_energetic_PBL section ==
# MOM6_USE_LI2016
if [[ "${cplwav}" == ".true." ]] ; then
  local MOM6_USE_WAVES="True"
else
  local MOM6_USE_WAVES="False"
fi
# == MOM_oda_incupd section ==
local ODA_TEMPINC_VAR=${ODA_TEMPINC_VAR:-"Temp"}
local ODA_SALTINC_VAR=${ODA_SALTINC_VAR:-"Salt"}
local ODA_THK_VAR=${ODA_THK_VAR:-"h"}
local ODA_INCUPD_UV="True"
local ODA_UINC_VAR=${ODA_UINC_VAR:-"u"}
local ODA_VINC_VAR=${ODA_VINC_VAR:-"v"}
# ODA_INCUPD
# ODA_INCUPD_NHOURS
# == MOM_surface_forcing section ==
# MOM6_RIVER_RUNOFF
# == ocean_stochastics section ==
if [[ "${DO_OCN_SPPT}" == "YES" ]]; then
  local DO_OCN_SPPT="True"  # TODO: This is problematic if DO_OCN_SPPT is going to be used elsewhere
else
  local DO_OCN_SPPT="False"
fi
if [[ "${DO_OCN_PERT_EPBL}" == "YES" ]]; then
  local PERT_EPBL="True"
else
  local PERT_EPBL="False"
fi
# Ensure the template exists
local template=${MOM6_INPUT_TEMPLATE:-"${PARMgfs}/ufs/MOM_input_${OCNRES}.IN"}
if [[ ! -f "${template}" ]]; then
  echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
  exit 1
fi
rm -f "${DATA}/INPUT/MOM_input"
atparse < "${template}" >> "${DATA}/INPUT/MOM_input"
echo "Rendered MOM_input:"
cat "${DATA}/INPUT/MOM_input"

# ================================================================
# data_table
# ----------
# Prepare local variables for use in MOM6_data_table.IN from UFSWM
local MOM6_FRUNOFF=${FRUNOFF}

# Ensure the template exists
local template=${MOM6_DATA_TABLE_TEMPLATE:-"${PARMgfs}/ufs/MOM6_data_table.IN"}
if [[ ! -f "${template}" ]]; then
  echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
  exit 1
fi
rm -f "${DATA}/data_table"
atparse < "${template}" >> "${DATA}/data_table"
echo "Rendered data_table:"
cat "${DATA}/data_table"

}
