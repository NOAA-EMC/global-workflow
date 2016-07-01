# Standalone script used to pass namelist updates to the regression tests.

# First, generate new variable to hold the first 6 characters of the experiment.

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

coeffsize=large

if echo $exp | grep updat > /dev/null ; then
  if [[ $coeffsize = "large" ]]; then 
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/airs2378_aqua.SpcCoeff.bin ./airs_aqua.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/airs2378_aqua.TauCoeff.bin ./airs_aqua.TauCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi8461_metop-a.TauCoeff.bin ./iasi_metop-a.TauCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi8461_metop-a.SpcCoeff.bin ./iasi_metop-a.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi8461_metop-b.TauCoeff.bin ./iasi_metop-b.TauCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi8461_metop-b.SpcCoeff.bin ./iasi_metop-b.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/cris1305_npp.SpcCoeff.bin ./cris_npp.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/cris1305_npp.TauCoeff.bin ./cris_npp.TauCoeff.bin
  else
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/airs281SUBSET_aqua.SpcCoeff.bin ./airs_aqua.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/airs281SUBSET_aqua.TauCoeff.bin ./airs_aqua.TauCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi616_metop-a.TauCoeff.bin ./iasi_metop-a.TauCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi616_metop-a.SpcCoeff.bin ./iasi_metop-a.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi616_metop-b.TauCoeff.bin ./iasi_metop-b.TauCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/iasi616_metop-b.SpcCoeff.bin ./iasi_metop-b.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/cris399_npp.SpcCoeff.bin ./cris_npp.SpcCoeff.bin
    /bin/cp /da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix/cris399_npp.TauCoeff.bin ./cris_npp.TauCoeff.bin
  fi
fi

