# It is now possible to run all regression tests (except RTMA) using the hybrid ensemble option with
#  internally generated random ensemble perturbations.  No script changes are required.
#  To run with hybrid ensemble option on, change HYBENS_GLOBAL and/or HYBENS_REGIONAL from "false" to "true".
#  These are located at the end of this script.

# First determine what machine are we on:
if [ -d /da ]; then # WCOSS
   export machine="WCOSS"
elif [ -d /scratch4/NCEPDEV/da ]; then # Theia
   export machine="Theia"
fi

# Name of the branch being tested
updat="EXP-regopt"

#  Handle machine specific paths for:
#  experiment and control executables, fix, ptmp, and CRTM coefficient files.
#  Location of ndate utility, noscrub directory, and account name (accnt = ada by default).
if [[ "$machine" = "Theia" ]]; then

   export basedir="/scratch4/NCEPDEV/global/save/$LOGNAME/gsi"

   export group="global"
   export queue="batch"

   export ptmp="/scratch4/NCEPDEV/stmp3/$LOGNAME"
   export noscrub="/scratch4/NCEPDEV/da/noscrub/$LOGNAME"

   export fixcrtm="/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/crtm/2.2.3/fix"
   export casesdir="/scratch4/NCEPDEV/da/noscrub/Michael.Lueken/CASES"
   export ndate="/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate"

   export check_resource="no"

   export accnt="hybrid"

   #  On Theia, there are no scrubbers to remove old contents from stmp* directories.
   #  After completion of regression tests, will remove the regression test subdirecories
   export clean=".false."

elif [[ "$machine" = "WCOSS" ]]; then

   export basedir="/da/save/$LOGNAME/gsi"

   export group="dev"
   export queue="dev"

   export ptmp="/ptmpp1/$LOGNAME"
   export noscrub="/da/noscrub/$LOGNAME"

   export fixcrtm="/da/save/Michael.Lueken/CRTM_REL-2.2.3/crtm_v2.2.3/fix"
   export casesdir="/da/noscrub/Michael.Lueken/CASES"
   export ndate="/nwprod/util/exec/ndate"

   export check_resource="yes"

   export accnt=""

fi

# GSI paths based on basedir

export gsisrc="$basedir/branches/$updat/src"
export gsiexec_updat="$basedir/branches/$updat/src/global_gsi"
export gsiexec_contrl="$basedir/trunk/src/global_gsi"
export enkfexec_updat="$basedir/branches/$updat/src/enkf/global_enkf"
export enkfexec_contrl="$basedir/trunk/src/enkf/global_enkf"
export fixgsi="$basedir/branches/$updat/fix"
export scripts="$basedir/branches/$updat/scripts"

# Paths to tmpdir and savedir base on ptmp
export tmpdir="$ptmp/REGDIRS/gsi/$updat"
export savdir="$ptmp/REGDIRS/gsi/$updat"

# We are dealing with *which* endian files
export endianness="Big_Endian"

# Variables with the same values are defined below.

# Default resolution
export JCAP="62"

# Case Study analysis dates
export global_T62_adate="2014080400"
export global_4dvar_T62_adate="2014080400"
export global_hybrid_T126_adate="2014092912"
export global_enkf_T62_adate="2014092912"
export global_lanczos_T62_adate="2014080400"
export global_nemsio_T62_adate="2013011400"
export nmmb_nems_adate="2015061000"
export arw_binary_adate="2010072412"
export arw_netcdf_adate="2008051112"
export nmm_binary_adate="2010021600"
export nmm_netcdf_adate="2007122000"
export rtma_adate="2015030712"
export hwrf_nmm_adate="2012102812"

# Paths for canned case data.
export global_T62_obs="$casesdir/global/sigmap/${global_T62_adate}"
export global_T62_ges="$casesdir/global/sigmap/${global_T62_adate}"
export global_4dvar_T62_obs="$casesdir/global/sigmap/${global_4dvar_T62_adate}"
export global_4dvar_T62_ges="$casesdir/global/sigmap/${global_4dvar_T62_adate}"
export global_hybrid_T126_datobs="$casesdir/global/sigmap/${global_hybrid_T126_adate}/obs"
export global_hybrid_T126_datges="$casesdir/global/sigmap/${global_hybrid_T126_adate}/ges"
export global_enkf_T62_datobs="$casesdir/global/sigmap/${global_enkf_T62_adate}/obs"
export global_enkf_T62_datges="$casesdir/global/sigmap/${global_enkf_T62_adate}/ges"
export global_lanczos_T62_obs="$casesdir/global/sigmap/${global_lanczos_T62_adate}"
export global_lanczos_T62_ges="$casesdir/global/sigmap/${global_lanczos_T62_adate}"
export global_nemsio_T62_obs="$casesdir/global/sigmap/${global_nemsio_T62_adate}"
export global_nemsio_T62_ges="$casesdir/global/sigmap_nemsio/$global_nemsio_T62_adate"
export nmmb_nems_4denvar_obs="$casesdir/regional/nmmb_nems/$nmmb_nems_adate"
export nmmb_nems_4denvar_ges="$casesdir/regional/nmmb_nems/$nmmb_nems_adate"
export arw_binary_obs="$casesdir/regional/arw_binary/$arw_binary_adate"
export arw_binary_ges="$casesdir/regional/arw_binary/$arw_binary_adate"
export arw_netcdf_obs="$casesdir/regional/arw_netcdf/$arw_netcdf_adate"
export arw_netcdf_ges="$casesdir/regional/arw_netcdf/$arw_netcdf_adate"
export nmm_binary_obs="$casesdir/regional/nmm_binary/$nmm_binary_adate"
export nmm_binary_ges="$casesdir/regional/nmm_binary/$nmm_binary_adate"
export nmm_netcdf_obs="$casesdir/regional/nmm_netcdf/$nmm_netcdf_adate"
export nmm_netcdf_ges="$casesdir/regional/nmm_netcdf/$nmm_netcdf_adate"
export rtma_obs="$casesdir/regional/rtma/$rtma_adate"
export rtma_ges="$casesdir/regional/rtma/$rtma_adate"
export hwrf_nmm_obs="$casesdir/regional/hwrf_nmm/$hwrf_nmm_adate"
export hwrf_nmm_ges="$casesdir/regional/hwrf_nmm/$hwrf_nmm_adate"

# Define type of GPSRO data to be assimilated (refractivity or bending angle)
export gps_dtype="gps_bnd"

# Regression vfydir
export regression_vfydir="$noscrub/regression/gsi/$updat"

# Define debug variable - If you want to run the debug tests, set this variable to .true.  Default is .false.
export debug=".false."

# Define parameters for global_T62_3d4dvar and global_T62_4dvar
export minimization="lanczos"  # If "lanczos", use sqrtb lanczos minimization algorithm.  Otherwise use "pcgsoi".
export nhr_obsbin="6"          # Time window for observation binning.  Use "6" for 3d4dvar test.  Otherwise use "1"

# Define parameters for hybrid ensemble option test.
# (default is set to false, so no hybrid ensemble option test.)

export HYBENS_GLOBAL=".false."
export ENSEMBLE_SIZE_GLOBAL="10"
export HYBENS_UV_GLOBAL=".true."
export BETA1_INV_GLOBAL="0.5"
export HYBENS_HOR_SCALE_GLOBAL="1500"
export HYBENS_VER_SCALE_GLOBAL="20"
export GENERATE_ENS_GLOBAL=".true."
export HYBENS_ANISO_GLOBAL=".false."

export HYBENS_REGIONAL=".false."
export ENSEMBLE_SIZE_REGIONAL="10"
export HYBENS_UV_REGIONAL=".true."
export BETA1_INV_REGIONAL="0.5"
export HYBENS_HOR_SCALE_REGIONAL="1500"
export HYBENS_VER_SCALE_REGIONAL="20"
export GENERATE_ENS_REGIONAL=".true."
export HYBENS_ANISO_REGIONAL=".false."
export NLON_ENS_REGIONAL="0"
export NLAT_ENS_REGIONAL="0"
export JCAP_ENS_REGIONAL="0"
export JCAP_ENS_TEST_REGIONAL="0"

# Toggle EnKF update code bias correction flag: lupd_satbiasc
# TRUE  =        compute and update radiance bias correction
# FALSE = do NOT compute or  update radiance bias correction
# default is FALSE (as done in NCEP operations)
export lupd_satbiasc=".false."
