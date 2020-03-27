
MOM6_namelists(){

# MOM6 namelists generation

  cat >> input.nml <<EOF

&MOM_input_nml
  output_directory = 'MOM6_OUTPUT/',
  input_filename = 'r'
  restart_input_dir = 'INPUT/',
  restart_output_dir = 'MOM6_RESTART/',
  parameter_filename = 'INPUT/MOM_input',
                       'INPUT/MOM_override'
/
EOF

echo "$(cat input.nml)"
}
