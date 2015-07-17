# Standalone script used to pass namelist updates to the regression tests.

# First, generate new variable to hole the first 6 characters of the experiment.

#if [[ `expr substr $exp $((${#exp}-4)) ${#exp}` = "updat" ]]; then
if [[ `expr substr $exp 1 6` = "global" ]]; then
   if [[ `expr substr $exp 8 5` = "4dvar" ]]; then
      export SETUP_update=""
      export SETUP_enkf=""
   elif [[ `expr substr $exp 8 7` = "lanczos" ]]; then
      export SETUP_update=""
      export SETUP_enkf=""
   else
      export SETUP_update="newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,diag_precon=.true.,step_start=1.0e-3,emiss_bc=.true.,"
      export SETUP_enkf="univaroz=.true.,adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,"
   fi
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
