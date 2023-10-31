#! /usr/bin/env bash

MOM6_namelists(){

# MOM6 namelists generation

if [[ "${cplwav}" == ".true." ]] ; then
  local MOM6_USE_WAVES='True'
else
  local MOM6_USE_WAVES='False'
fi

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

if [[ ${DO_OCN_SPPT} = "YES" ]]; then
  local OCN_SPPT="True"
  cat >> input.nml <<EOF
  OCNSPPT=${OCNSPPT:-1.0}
  OCNSPPT_LSCALE=${OCNSPPT_LSCALE:-500e3}
  OCNSPPT_TAU=${OCNSPPT_TAU:-21600}
  ISEED_OCNSPPT=${ISEED_OCNSPPT:-${ISEED}}
EOF
else
  local OCN_SPPT="False"
fi

if [[ ${DO_OCN_PERT_EPBL} = "YES" ]]; then
  local PERT_EPBL="True"
  cat >> input.nml <<EOF
  EPBL=${EPBL:-1.0}
  EPBL_LSCALE=${EPBL_LSCALE:-500e3}
  EPBL_TAU=${EPBL_TAU:-21600}
  ISEED_EPBL=${ISEED_EPBL:-${ISEED}}
EOF
  else
    local PERT_EPBL="False"
fi

#cat >> input.nml <<EOF
#/
#
#&nam_sfcperts
#/
#
#EOF

echo "$(cat input.nml)"


#Copy MOM_input and edit:
${NCP} -pf "${HOMEgfs}/parm/ufs/mom6/MOM_input_template_${OCNRES}" "${DATA}/INPUT/"
sed -e "s/@\[DT_THERM_MOM6\]/${DT_THERM_MOM6}/g" \
    -e "s/@\[DT_DYNAM_MOM6\]/${DT_DYNAM_MOM6}/g" \
    -e "s/@\[MOM6_RIVER_RUNOFF\]/${MOM6_RIVER_RUNOFF}/g" \
    -e "s/@\[MOM6_THERMO_SPAN\]/${MOM6_THERMO_SPAN}/g" \
    -e "s/@\[MOM6_USE_LI2016\]/${MOM6_USE_LI2016}/g" \
    -e "s/@\[MOM6_USE_WAVES\]/${MOM6_USE_WAVES}/g" \
    -e "s/@\[MOM6_ALLOW_LANDMASK_CHANGES\]/${MOM6_ALLOW_LANDMASK_CHANGES}/g" \
    -e "s/@\[NX_GLB\]/${NX_GLB}/g" \
    -e "s/@\[NY_GLB\]/${NY_GLB}/g" \
    -e "s/@\[CHLCLIM\]/${CHLCLIM}/g" \
    -e "s/@\[DO_OCN_SPPT\]/${OCN_SPPT}/g" \
    -e "s/@\[PERT_EPBL\]/${PERT_EPBL}/g" \
    -e "s/@\[MOM6_DIAG_COORD_DEF_Z_FILE\]/${MOM6_DIAG_COORD_DEF_Z_FILE}/g" \
    -e "s/@\[TOPOEDITS\]/${TOPOEDITS}/g" \
    -e "s/@\[MOM6_DIAG_MISVAL\]/${MOM6_DIAG_MISVAL}/g" \
    -e "s/@\[ODA_INCUPD_NHOURS\]/${ODA_INCUPD_NHOURS}/g" \
    -e "s/@\[ODA_INCUPD\]/${ODA_INCUPD}/g" "${DATA}/INPUT/MOM_input_template_${OCNRES}" > "${DATA}/INPUT/MOM_input"
rm "${DATA}/INPUT/MOM_input_template_${OCNRES}"

#data table for runoff:
DATA_TABLE=${DATA_TABLE:-${HOMEgfs}/parm/ufs/fv3/data_table}
${NCP} "${DATA_TABLE}" "${DATA}/data_table_template"
sed -e "s/@\[FRUNOFF\]/${FRUNOFF}/g" "${DATA}/data_table_template" > "${DATA}/data_table"
rm "${DATA}/data_table_template"

}
