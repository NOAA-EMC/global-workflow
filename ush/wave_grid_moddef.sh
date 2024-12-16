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

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  echo "Generating mod_def file"

  mkdir -p moddef_${1}
  cd moddef_${1}

  grdID=$1

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!     Generate moddef file       |'
  echo '+--------------------------------+'
  echo "   Grid            : $1"
  echo ' '
  set_trace

# 0.b Check if grid set

  if [ "$#" -lt '1' ]
  then
    set +x
    echo ' '
    echo '**************************************************'
    echo '*** Grid not identifife in ww3_mod_def.sh ***'
    echo '**************************************************'
    echo ' '
    set_trace
    exit 1
  else
    grdID=$1
  fi

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [ -z "$grdID" ] || [ -z "${EXECgfs}" ]
  then
    set +x
    echo ' '
    echo '*********************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_mod_def.sh NOT SET ***'
    echo '*********************************************************'
    echo ' '
    set_trace
    exit 2
  fi

# --------------------------------------------------------------------------- #
# 2.  Create mod_def file

  set +x
  echo ' '
  echo '   Creating mod_def file ...'
  echo "   Executing ${EXECgfs}/${NET,,}_ww3_grid.x"
  echo ' '
  set_trace

  rm -f ww3_grid.inp
  ${NLN} ../ww3_grid.inp.$grdID ww3_grid.inp

  if [ -f ../${grdID}.msh ]
  then
     rm -f ${grdID}.msh
     ${NLN} ../${grdID}.msh ${grdID}.msh
  fi


  export pgm="${NET,,}_ww3_grid.x"

  "${EXECgfs}/${pgm}" 1> "grid_${grdID}.out" 2>&1
  err=$?

  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************** '
    echo "*** FATAL ERROR : ERROR IN ${pgm} *** "
    echo '******************************************** '
    echo ' '
    set_trace
    exit 3
  fi

  if [[ -f mod_def.ww3 ]]
  then
    cp mod_def.ww3 "${COMOUT_WAVE_PREP}/${RUN}wave.mod_def.${grdID}"
    mv mod_def.ww3 "../mod_def.${grdID}"
  else
    set +x
    echo ' '
    echo '******************************************** '
    echo '*** FATAL ERROR : MOD DEF FILE NOT FOUND *** '
    echo '******************************************** '
    echo ' '
    set_trace
    exit 4
  fi

# --------------------------------------------------------------------------- #
# 3.  Clean up

cd ..
rm -rf "moddef_${grdID}"

# End of ww3_mod_def.sh ------------------------------------------------- #
