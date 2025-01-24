# Standalone script used to pass namelist updates to the regression tests.

# First, generate new variable to hold the first 6 characters of the experiment.

export SETUP_update=""
export SETUP_enkf=""

if [[ `expr substr $exp 1 4` = "rtma" ]]; then
   export OBSQC_update="pvis=0.2,pcldch=0.1,scale_cv=1.0,estvisoe=2.61,estcldchoe=2.3716,vis_thres=16000.,cldch_thres=16000.,"
else
   export OBSQC_update=""
fi
export GRIDOPTS_update=""
export BKGVERR_update=""
export ANBKGERR_update=""
export JCOPTS_update=""
export STRONGOPTS_update=""
export OBSQC_update=""
export OBSINPUT_update=""
export SUPERRAD_update=""
export SINGLEOB_update=""

