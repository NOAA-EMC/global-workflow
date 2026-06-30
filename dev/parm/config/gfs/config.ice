#! /usr/bin/env bash

echo "BEGIN: config.ice"

# Override atm-only FV3 settings when ice model is on
export min_seaice="1.0e-6"
export use_cice_alb=".true."

export MESH_ICE="mesh.mx${ICERES}.nc"

export CICE_GRID="grid_cice_NEMS_mx${ICERES}.nc"
export CICE_MASK="kmtu_cice_NEMS_mx${ICERES}.nc"

export CICE_FBOT_XFER_TYPE='mushy'          # default constant
export CICE_TFREEZE_OPTION='linear_salt'    # default mushy
export CICE_AHMAX=0.1                       # default 0.3
export CICE_R_ICE=2.8                       # default 0
export CICE_R_PND=2.8                       # default 0
export CICE_R_SNW=2.8
export CICE_EMISSIVITY=0.98
export CICE_TR_POND_TOPO='.true.'
export CICE_TR_POND_LVL='.false.'
export CICE_HS0=0.001
export CICE_HS1=0.005
export CICE_DPSCALE=0.02
export CICE_RFRACMIN=0.1
export CICE_RFRACMAX=0.6
export CICE_PNDASPECT=1.2
export CICE_SNWREDIST='ITDrdg'

echo "END: config.ice"
