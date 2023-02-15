#! /usr/bin/env bash

MOM6_namelists(){

# MOM6 namelists generation

OCNRES=${OCNRES:-"025"}
MOM_INPUT=MOM_input_template_$OCNRES

#Set to False for restart reproducibility
MOM6_USE_LI2016=${MOM6_USE_LI2016:-'True'}
MOM6_THERMO_SPAN=${MOM6_THERMO_SPAN:-'False'}
MOM6_ALLOW_LANDMASK_CHANGES=${MOM6_ALLOW_LANDMASK_CHANGES:-'False'}

DO_OCN_SPPT=${DO_OCN_SPPT:-'False'}
PERT_EPBL=${PERT_EPBL:-'False'}

ODA_INCUPD=${ODA_INCUPD:-"false"}
ODA_INCUPD_NHOURS=${ODA_INCUPD_NHOURS:-'3.0'}

if [ $cplwav = ".true." ] ; then
  MOM6_USE_WAVES='True'
else
  MOM6_USE_WAVES='False'
fi

case "${OCNRES}" in
  "025")
    NX_GLB=1440
    NY_GLB=1080
    DT_DYNAM_MOM6='900'
    DT_THERM_MOM6='1800'
    CHLCLIM="seawifs-clim-1997-2010.${NX_GLB}x${NY_GLB}.v20180328.nc"
    FRUNOFF="runoff.daitren.clim.${NX_GLB}x${NY_GLB}.v20180328.nc"
    MOM6_RIVER_RUNOFF='True'
    MOM6_RESTART_SETTING="r"
    ;;
  "050")
    NX_GLB=720
    NY_GLB=576
    DT_DYNAM_MOM6='1800'
    DT_THERM_MOM6='3600'
    CHLCLIM="seawifs-clim-1997-2010.${NX_GLB}x${NY_GLB}.v20180328.nc"
    FRUNOFF="runoff.daitren.clim.${NX_GLB}x${NY_GLB}.v20180328.nc"
    MOM6_RESTART_SETTING='n'
    MOM6_RIVER_RUNOFF='True'
    ;;
  "100")
    NX_GLB=360
    NY_GLB=320
    DT_DYNAM_MOM6='1800'
    DT_THERM_MOM6='3600'
    FRUNOFF=""
    CHLCLIM="seawifs_1998-2006_smoothed_2X.nc"
    MOM6_RESTART_SETTING='n'
    MOM6_RIVER_RUNOFF='False'
    ;;
  "500")
    NX_GLB=72
    NY_GLB=35
    DT_DYNAM_MOM6='3600'
    DT_THERM_MOM6='3600'
    FRUNOFF=""
    CHLCLIM="seawifs_1998-2006_smoothed_2X.nc"
    MOM6_RESTART_SETTING='r'
    MOM6_RIVER_RUNOFF='False'
    ;;
  *)
    echo "FATAL ERROR: do not have MOM6 settings defined for desired OCNRES=$OCNRES"
    exit 1
    ;;
esac

cat >> input.nml <<EOF

&MOM_input_nml
  output_directory = 'MOM6_OUTPUT/',
  input_filename = '$MOM6_RESTART_SETTING'
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

OCN_SPPT="False"
if [ $DO_OCN_SPPT = "YES" ]; then
  OCN_SPPT="True"
  cat >> input.nml <<EOF
  OCNSPPT=${OCNSPPT:-1.0}
  OCNSPPT_LSCALE=${OCNSPPT_LSCALE:-500e3}
  OCNSPPT_TAU=${OCNSPPT_TAU:-21600}
  ISEED_OCNSPPT=${ISEED_OCNSPPT:-$ISEED}
EOF
  fi

PERT_EPBL="False"
if [ $DO_OCN_PERT_EPBL = "YES" ]; then
  PERT_EPBL="True"
  cat >> input.nml <<EOF
  EPBL=${EPBL:-1.0}
  EPBL_LSCALE=${EPBL_LSCALE:-500e3}
  EPBL_TAU=${EPBL_TAU:-21600}
  ISEED_EPBL=${ISEED_EPBL:-$ISEED}
EOF
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
$NCP -pf $HOMEgfs/parm/mom6/MOM_input_template_$OCNRES $DATA/INPUT/
sed -e "s/@\[DT_THERM_MOM6\]/$DT_THERM_MOM6/g" \
    -e "s/@\[DT_DYNAM_MOM6\]/$DT_DYNAM_MOM6/g" \
    -e "s/@\[MOM6_RIVER_RUNOFF\]/$MOM6_RIVER_RUNOFF/g" \
    -e "s/@\[MOM6_THERMO_SPAN\]/$MOM6_THERMO_SPAN/g" \
    -e "s/@\[MOM6_USE_LI2016\]/$MOM6_USE_LI2016/g" \
    -e "s/@\[MOM6_USE_WAVES\]/$MOM6_USE_WAVES/g" \
    -e "s/@\[MOM6_ALLOW_LANDMASK_CHANGES\]/$MOM6_ALLOW_LANDMASK_CHANGES/g" \
    -e "s/@\[NX_GLB\]/$NX_GLB/g" \
    -e "s/@\[NY_GLB\]/$NY_GLB/g" \
    -e "s/@\[CHLCLIM\]/$CHLCLIM/g" \
    -e "s/@\[DO_OCN_SPPT\]/$OCN_SPPT/g" \
    -e "s/@\[PERT_EPBL\]/$PERT_EPBL/g" \
    -e "s/@\[ODA_INCUPD_NHOURS\]/$ODA_INCUPD_NHOURS/g" \
    -e "s/@\[ODA_INCUPD\]/$ODA_INCUPD/g" $DATA/INPUT/MOM_input_template_$OCNRES > $DATA/INPUT/MOM_input
rm $DATA/INPUT/MOM_input_template_$OCNRES

#data table for runoff:
DATA_TABLE=${DATA_TABLE:-$PARM_FV3DIAG/data_table}
$NCP $DATA_TABLE $DATA/data_table_template
sed -e "s/@\[FRUNOFF\]/$FRUNOFF/g" $DATA/data_table_template > $DATA/data_table
rm $DATA/data_table_template

}
