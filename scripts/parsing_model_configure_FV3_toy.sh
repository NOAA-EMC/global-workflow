#! /bin/sh

## "parsing_namelist_FV3.sh"
## This is the child script of ex-global forecast,
## writing namelist for FV3

cat >> model.configure << EOF
&atmos_model_nml
  fdiag = $cplflx
  fhmax = $cplwav
  fhout = $cplchem
EOF
