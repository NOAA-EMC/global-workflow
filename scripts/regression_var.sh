# It is now possible to run all regression tests (except RTMA) using the hybrid ensemble option with
#  internally generated random ensemble perturbations.  No script changes are required.
#  To run with hybrid ensemble option on, change HYBENS_GLOBAL and/or HYBENS_REGIONAL from "false" to "true".
#  These are located at the end of this script.

# Define global noscrub directory

export noscrub="/global/noscrub/$USER"

# Define path to fix file directory

export fix_file="/global/save/$USER/svn1/fix"
export crtm_coef="/global/save/wx20ml/CRTM_REL-2.0/CRTM_Coefficients"
export scripts="/global/save/$USER/svn1/scripts"
export src="/global/save/$USER/svn1/src"

# Define work directories (location of executables)

export updat="/global/save/$USER/mlueken/src/global_gsi"
export cntrl="/global/save/$USER/svn1/src/global_gsi"

# Define experiment names

export global="62_sigmap"
export global_lanczos="62_sigmap_lanczos"
export arw_binary="arw_binary"
export arw_netcdf="arw_netcdf"
export nmm_binary="nmm_binary"
export nmm_netcdf="nmm_netcdf"
export nems_nmmb="nems_nmmb"
export rtma="rtma"
export compare="compare"

export exp1_global_updat="global.updat.exp1"
export exp1_global_lanczos_updat="global_lanczos.updat.exp1"
export exp1_nmm_binary_updat="nmm_binary.updat.exp1"
export exp1_nmm_netcdf_updat="nmm_netcdf.updat.exp1"
export exp1_arw_binary_updat="arw_binary.updat.exp1"
export exp1_arw_netcdf_updat="arw_netcdf.updat.exp1"
export exp1_rtma_updat="rtma.updat.exp1"
export exp1_nems_nmmb_updat="nems_nmmb.updat.exp1"

export exp2_global_updat="global.updat.exp2"
export exp2_global_lanczos_updat="global_lanczos.updat.exp2"
export exp2_nmm_binary_updat="nmm_binary.updat.exp2"
export exp2_nmm_netcdf_updat="nmm_netcdf.updat.exp2"
export exp2_arw_binary_updat="arw_binary.updat.exp2"
export exp2_arw_netcdf_updat="arw_netcdf.updat.exp2"
export exp2_rtma_updat="rtma.updat.exp2"
export exp2_nems_nmmb_updat="nems_nmmb.updat.exp2"

export exp1_global_cntrl="global.cntrl.exp1"
export exp1_global_lanczos_cntrl="global_lanczos.cntrl.exp1"
export exp1_nmm_binary_cntrl="nmm_binary.cntrl.exp1"
export exp1_nmm_netcdf_cntrl="nmm_netcdf.cntrl.exp1"
export exp1_arw_binary_cntrl="arw_binary.cntrl.exp1"
export exp1_arw_netcdf_cntrl="arw_netcdf.cntrl.exp1"
export exp1_rtma_cntrl="rtma.cntrl.exp1"
export exp1_nems_nmmb_cntrl="nems_nmmb.cntrl.exp1"

export exp2_global_cntrl="global.cntrl.exp2"
export exp2_global_lanczos_cntrl="global_lanczos.cntrl.exp2"
export exp2_nmm_binary_cntrl="nmm_binary.cntrl.exp2"
export exp2_nmm_netcdf_cntrl="nmm_netcdf.cntrl.exp2"
export exp2_arw_binary_cntrl="arw_binary.cntrl.exp2"
export exp2_arw_netcdf_cntrl="arw_netcdf.cntrl.exp2"
export exp2_rtma_cntrl="rtma.cntrl.exp2"
export exp2_nems_nmmb_cntrl="nems_nmmb.cntrl.exp2"

# Define ptmp location

export ptmp_loc="/ptmp/$USER"

# Define analysis date

export adate_global="2010020112"
export adate_regional="2007122000"
export adate_regional_nmm_binary="2010021600"
export adate_regional_nems_nmmb="2009031600"
export adate_regional_arw_netcdf="2008051112"
export adate_regional_arw_binary="2010072412"

# Define machine (added due to almost daily switch between cirrus and stratus and different locations of obs between machines)

#machine="cirrus"
machine="stratus"
#machine="vapor"

# Define obs directory

if [[ $machine = "stratus" ]]; then
   export datobs_global="/global/noscrub/wx20ml/cases/global/sigmap"
   export datobs_global_lanczos="/global/noscrub/wx20ml/cases/global/sigmap"
   export datobs_rtma="/global/noscrub/wx20ml/cases/regional/rtma_binary"
   export datobs_nmm_binary="/global/noscrub/wx20ml/cases/regional/ndas_binary"
   export datobs_nems_nmmb="/global/noscrub/wx20ml/nmmb_regression_case"
   export datobs_nmm_netcdf="/global/noscrub/wx20ml/cases/regional/ndas_binary"
   export datges_nmm_netcdf="/global/noscrub/wx20ml/cases/regional/nmm_netcdf"
   export datobs_arw_binary="/global/noscrub/wx20ml/cases/regional/arw_binary"
   export datobs_arw_netcdf="/global/noscrub/wx20ml/cases/regional/arw_netcdf"
elif [[ $machine = "cirrus" ]]; then
     export datobs_global="/global/noscrub/wx20ml/cases/global/sigmap"
     export datobs_global_lanczos="/global/noscrub/wx20ml/cases/global/sigmap"
     export datobs_rtma="/global/noscrub/wx20ml/cases/regional/rtma_binary"
     export datobs_nmm_binary="/global/noscrub/wx20ml/cases/regional/ndas_binary"
     export datobs_nems_nmmb="/global/noscrub/wx20ml/nmmb_regression_case"
     export datobs_nmm_netcdf="/global/noscrub/wx20ml/cases/regional/ndas_binary"
     export datges_nmm_netcdf="/global/noscrub/wx20ml/cases/regional/nmm_netcdf"
     export datobs_arw_binary="/global/noscrub/wx20ml/cases/regional/arw_binary"
     export datobs_arw_netcdf="/global/noscrub/wx20ml/cases/regional/arw_netcdf"
elif [[ $machine = "vapor" ]]; then
# Update location of coefficient files for vapor, then give obs locations
     export crtm_coef="/jcsda/save/wx20ml/CRTM_REL-2.0/CRTM_Coefficients"
     export datobs_global="/jcsda/noscrub/wx20ml/cases/global/sigmap"
     export datobs_global_lanczos="/jcsda/noscrub/wx20ml/cases/global/sigmap"
     export datobs_rtma="/jcsda/noscrub/wx20ml/cases/regional/rtma_binary"
     export datobs_nmm_binary="/jcsda/noscrub/wx20ml/cases/regional/ndas_binary"
     export datobs_nems_nmmb="/jcsda/noscrub/wx20ml/nmmb_regression_case"
     export datobs_nmm_netcdf="/jcsda/noscrub/wx20ml/cases/regional/ndas_binary"
     export datges_nmm_netcdf="/jcsda/noscrub/wx20ml/cases/regional/nmm_netcdf"
     export datobs_arw_binary="/jcsda/noscrub/wx20ml/cases/regional/arw_binary"
     export datobs_arw_netcdf="/jcsda/noscrub/wx20ml/cases/regional/arw_netcdf"
fi

# Regression output filename

export global_regression="global_regression_results.txt"
export global_lanczos_regression="global_lanczos_regression_results.txt"
export rtma_regression="rtma_regression_results.txt"
export nmm_binary_regression="nmm_binary_regression_results.txt"
export nmm_netcdf_regression="nmm_netcdf_regression_results.txt"
export arw_binary_regression="arw_binary_regression_results.txt"
export arw_netcdf_regression="arw_netcdf_regression_results.txt"
export nems_nmmb_regression="nems_nmmb_regression_results.txt"

# Regression vfydir

export regression_vfydir="$noscrub/regression"

# Control run option

export control="false" # If true, run the extra two control runs for each configuration.  If false, skip the control runs and use data from /noscrub.
export debug="false" # If true, run the extra debug run for each configuration.  If false, skip the debug runs.

# Define location for copying control run data to

export control_RTMA="$noscrub/tmpreg_${rtma}/$exp1_rtma_cntrl"
export control_RTMA2="$noscrub/tmpreg_${rtma}/$exp2_rtma_cntrl"
export control_global_T62="$noscrub/tmp${global}/$exp1_global_cntrl"
export control_global_T622="$noscrub/tmp${global}/$exp2_global_cntrl"
export control_global_lanczos_T62="$noscrub/tmp${global_lanczos}/$exp1_global_lanczos_cntrl"
export control_global_lanczos_T622="$noscrub/tmp${global_lanczos}/$exp2_global_lanczos_cntrl"
export control_nmm_binary="$noscrub/tmpreg_${nmm_binary}/$exp1_nmm_binary_cntrl"
export control_nmm_binary2="$noscrub/tmpreg_${nmm_binary}/$exp2_nmm_binary_cntrl"
export control_nmm_netcdf="$noscrub/tmpreg_${nmm_netcdf}/$exp1_nmm_netcdf_cntrl"
export control_nmm_netcdf2="$noscrub/tmpreg_${nmm_netcdf}/$exp2_nmm_netcdf_cntrl"
export control_arw_binary="$noscrub/tmpreg_${arw_binary}/$exp1_arw_binary_cntrl"
export control_arw_binary2="$noscrub/tmpreg_${arw_binary}/$exp2_arw_binary_cntrl"
export control_arw_netcdf="$noscrub/tmpreg_${arw_netcdf}/$exp1_arw_netcdf_cntrl"
export control_arw_netcdf2="$noscrub/tmpreg_${arw_netcdf}/$exp2_arw_netcdf_cntrl"
export control_nems_nmmb="$noscrub/tmpreg_${nems_nmmb}/$exp1_nems_nmmb_cntrl"
export control_nems_nmmb2="$noscrub/tmpreg_${nems_nmmb}/$exp2_nems_nmmb_cntrl"

# Define parameters for hybrid ensemble option test.
#   (default is set to false, so no hybrid ensemble option test.)

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
