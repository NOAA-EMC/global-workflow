# Standalone script used to pass namelist updates to the regression tests.

if [[ `expr substr $exp $((${#exp}-4)) ${#exp}` = "updat" ]]; then
   export SETUP_update=""
else
   export SETUP_update=""
fi
export GRIDOPTS_update=""
export BKGVERR_update=""
export ANBKGERR_update=""
export JCOPTS_update=""
if [[ `expr substr $exp 1 6` = "global" ]]; then
   if [[ `expr substr $exp $((${#exp}-4)) ${#exp}` = "updat" ]]; then
      export STRONGOPTS_update=""
   else
      export STRONGOPTS_update=""
   fi
fi
export OBSQC_update=""
export OBSINPUT_update=""
export SUPERRAD_update=""
export SINGLEOB_update=""

# Include changes to fix files between experiment and control 

if [[ `expr substr $exp 1 6` = "global" ]]; then 
   if [[ `expr substr $exp $((${#exp}-4)) ${#exp}` = "updat" ]]; then 
      export biascr_update="abias_ssmis" 
   else 
      export biascr_update="abias" 
   fi 
fi 
