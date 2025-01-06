#! /usr/bin/env bash

WW3_namelists(){

# WW3 namelists/input generation

  FHMAX_WAV="${FHMAX_WAV:-384}"

# --------------------------------------------------------------------------- #
# Buoy location file

  if [ -f "${PARMgfs}/wave/wave_${NET}.buoys" ]
  then
    ${NCP} "${PARMgfs}/wave/wave_${NET}.buoys" "${DATA}/ww3_points.list"
  fi

  if [ -f "${DATA}/ww3_points.list" ]
  then
    set +x
    echo "ww3_points.list copied (${PARMgfs}/wave/wave_${NET}.buoys)."
    set_trace
  else
    echo "FATAL ERROR : ww3_points.list (${PARMgfs}/wave/wave_${NET}.buoys) NOT FOUND"
    exit 12 
  fi

  #set coupling to ice/current
  WW3_ICE="F"
  WW3_CUR="F"

  case ${WW3ICEINP} in
    'YES' )
      WW3_ICE="T";;
    'CPL' )
      WW3_ICE="C";;
  esac

  case ${WW3CURINP} in
    'YES' )
      WW3_CUR="T";;
    'CPL' )
      WW3_CUR="C";;
  esac

  # Variables used in atparse of shel template 
  export WW3_IC1="F"
  export WW3_IC5="F"
  export WW3_ICE
  export WW3_CUR
  export WW3_OUTPARS="${OUTPARS_WAV}"
  export WW3_DTFLD="${DTFLD_WAV}"
  export WW3_DTPNT="${DTPNT_WAV}"
  # Ensure the template exists
  local template=${WW3_INPUT_TEMPLATE:-"${PARMgfs}/ufs/ww3_shel.nml.IN"}
  if [[ ! -f "${template}" ]]; then
    echo "FATAL ERROR: template '${template}' does not exist, ABORT!"
    exit 1
  fi
  rm -f "${DATA}/ww3_shel.nml"
  atparse < "${template}" >> "${DATA}/ww3_shel.nml"
  echo "Rendered ww3_shel.nml:"
  cat "${DATA}/ww3_shel.nml"

}
