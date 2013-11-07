# Standalone script used to pass namelist updates to the regression tests.

# First, generate new variable to hole the first 6 characters of the experiment.

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

# Now, add in location for different convinfo fix files (based on control vs experiment)

if [[ `expr substr $exp $((${#exp}-4)) ${#exp}` = "updat" ]]; then
   if [[ "$arch" = "Linux" ]]; then
      export convinfopath="/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/xsu_sattype/fix"
   elif [[ "$machine" = "WCOSS" ]]; then
      export convinfopath="/da/save/Michael.Lueken/xsu_sattype/fix"
   fi
else
   if [[ "$arch" = "Linux" ]]; then
      export convinfopath="/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/fix_test"
   elif [[ "$machine" = "WCOSS" ]]; then
      export convinfopath="/da/save/Michael.Lueken/fix_test"
   fi
fi
