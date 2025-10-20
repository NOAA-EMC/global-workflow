#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_grid_moddef.sh
# Script description:  Create grib2 files for the wave component
#
# Author:   J-Henrique Alves    Org: NCEP/EMC      Date: 2011-04-08
# Abstract: Creates model definition files for the wave model WW3
#
# Script history log:
# 2019-11-02  J-Henrique Alves Ported to global-workflow.
# 2020-06-10  J-Henrique Alves Ported to R&D machine Hera
#
# $Id$
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

# 0.a Basic modes of operation
  grdID=${1?Must provide grdID}

  echo "INFO: Generating mod_def file for ${grdID}"

  mkdir -p "moddef_${grdID}"
  cd "moddef_${grdID}" || exit 2

# --------------------------------------------------------------------------- #
# 2.  Create mod_def file


  rm -f "ww3_grid.inp"
  ${NLN} "../ww3_grid.inp.${grdID}" "ww3_grid.inp"

  if [[ -f "../${grdID}.msh" ]]; then
     rm -f "${grdID}.msh"
     ${NLN} "../${grdID}.msh" "${grdID}.msh"
  fi

  export pgm="${NET,,}_ww3_grid.x"

  echo "INFO: Executing ${EXECgfs}/${NET,,}_ww3_grid.x"

  "${EXECgfs}/${pgm}"
  export err=$?

  if [[ "${err}" != '0' ]]; then
    echo "FATAL ERROR: Error in ${pgm}"
    exit "${err}"
  fi

  if [[ -f mod_def.ww3 ]];then
    cpfs "mod_def.ww3" "${COMOUT_WAVE_PREP}/${RUN}.t${cyc}z.mod_def.${grdID}.bin"
    mv "mod_def.ww3" "../mod_def.${grdID}"
  else
    echo "FATAL ERROR: Mod def file not created for ${grdID}"
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up

cd ..
rm -rf "moddef_${grdID}"

# End of ww3_mod_def.sh ------------------------------------------------- #
