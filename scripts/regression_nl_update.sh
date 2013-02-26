# Standalone script used to pass namelist updates to the regression tests.

# First, generate new variable to hole the first 6 characters of the experiment.

global_test = cut c1-6 $exp

if [[ $gsiexec = $updat ]]; then
   export SETUP_update="lrun_subdirs=.true."
else
   export SETUP_update=""
fi
export GRIDOPTS_update=""
export BKGVERR_update=""
export ANBKGERR_update=""
export JCOPTS_update=""
if [[ $global_test = "global" ]]; then
   if [[ $gsiexec = $updat ]]; then
      export STRONGOPTS_update="tlnmc_option=1,tlnmc_type=2"
   else
      export STRONGOPTS_update="hybens_inmc_option=1,jcstrong_option=2,jcstrong=.true."
   fi
fi
export OBSQC_update=""
export OBSINPUT_update=""
export SUPERRAD_update=""
export SINGLEOB_update=""
