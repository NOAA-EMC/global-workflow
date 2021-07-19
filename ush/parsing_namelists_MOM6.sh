
MOM6_namelists(){

# MOM6 namelists generation

OCNRES=${OCNRES:-"025"}
MOM_INPUT=MOM_input_template_$OCNRES

#TODO: Make these variables configurable 

#Set to False for restart reproducibility   
MOM6_REPRO_LA='True'
MOM6_THERMO_SPAN='False'

if [ $cplwav = ".true." ] ; then
  MOM6_USE_WAVES='True'
else 
  MOM6_USE_WAVES='False'
fi

if [ $OCNRES = '025' ]; then
  NX_GLB=1440
  NY_GLB=1080
  DT_DYNAM_MOM6='900'
  DT_THERM_MOM6='1800'
  CHLCLIM="seawifs-clim-1997-2010.${NX_GLB}x${NY_GLB}.v20180328.nc"
  FRUNOFF="runoff.daitren.clim.${NX_GLB}x${NY_GLB}.v20180328.nc"
  MOM6_RIVER_RUNOFF='True'
  MOM6_RESTART_SETTING="r"
elif [ $OCNRES = '050' ]; then
  NX_GLB=720
  NY_GLB=576
  DT_DYNAM_MOM6='1800'
  DT_THERM_MOM6='3600'
  CHLCLIM="seawifs-clim-1997-2010.${NX_GLB}x${NY_GLB}.v20180328.nc"
  FRUNOFF="runoff.daitren.clim.${NX_GLB}x${NY_GLB}.v20180328.nc"
  MOM6_RESTART_SETTING='n'
  MOM6_RIVER_RUNOFF='True'
elif [ $OCNRES = '100' ]; then 
  NX_GLB=360
  NY_GLB=320
  DT_DYNAM_MOM6='1800'
  DT_THERM_MOM6='3600'
  FRUNOFF=""
  CHLCLIM="seawifs_1998-2006_smoothed_2X.nc"
  MOM6_RESTART_SETTING='n'
  MOM6_RIVER_RUNOFF='False'
else 
  echo "FATAL ERROR: do not have MOM6 settings defined for desired OCNRES=$OCNRES"
  exit 1 
fi 


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

echo "$(cat input.nml)"


#Copy MOM_input and edit: 
$NCP -pf $HOMEgfs/parm/mom6/MOM_input_template_$OCNRES $DATA/INPUT/
sed -e "s/DT_THERM_MOM6/$DT_THERM_MOM6/g" \
    -e "s/DT_DYNAM_MOM6/$DT_DYNAM_MOM6/g" \
    -e "s/MOM6_RIVER_RUNOFF/$MOM6_RIVER_RUNOFF/g" \
    -e "s/MOM6_THERMO_SPAN/$MOM6_THERMO_SPAN/g" \
    -e "s/MOM6_REPRO_LA/$MOM6_REPRO_LA/g" \
    -e "s/MOM6_USE_WAVES/$MOM6_USE_WAVES/g" \
    -e "s/NX_GLB/$NX_GLB/g" \
    -e "s/NY_GLB/$NY_GLB/g" \
    -e "s/CHLCLIM/$CHLCLIM/g" $DATA/INPUT/MOM_input_template_$OCNRES > $DATA/INPUT/MOM_input
rm $DATA/INPUT/MOM_input_template_$OCNRES

#data table for runoff: 
DATA_TABLE=${DATA_TABLE:-$PARM_FV3DIAG/data_table}
$NCP $DATA_TABLE $DATA/data_table_template
sed -e "s/FRUNOFF/$FRUNOFF/g" $DATA/data_table_template > $DATA/data_table
rm $DATA/data_table_template

}
