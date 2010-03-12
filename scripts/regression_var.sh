# Define global noscrub directory

export noscrub="/global/noscrub/$USER"

# Define path to fix file directory

export fix_file="/global/save/$USER/mlueken/fix"
export scripts="/global/save/$USER/mlueken/scripts"
export src="/global/save/$USER/mlueken/src"

# Define work directories (location of executables)

export subversion="/global/save/$USER/mlueken/src/global_gsi"
export benchmark="/global/save/$USER/svn1/src/global_gsi"

# Define experiment names

export global="62_sigmap"
export arw_binary="arw_binary"
export arw_netcdf="arw_netcdf"
export nmm_binary="ndas_binary"
export nmm_netcdf="ndas_netcdf"
export nems_nmmb="nems_nmmb"
export rtma="rtma"
export compare="compare"

export exp1_global_sub_1node="global.sub.1node"
export exp1_nmm_binary_sub_2node="nmm_binary.sub.2node"
export exp1_nmm_netcdf_sub_1node="nmm_netcdf.sub.1node"
export exp1_arw_binary_sub_1node="arw_binary.sub.1node"
export exp1_arw_netcdf_sub_1node="arw_netcdf.sub.1node"
export exp1_rtma_sub_1node="rtma.sub.1node"
export exp1_nems_nmmb_sub_2node="nems_nmmb.sub.2node"

export exp2_global_sub_2node="global.sub.2node"
export exp2_nmm_binary_sub_3node="nmm_binary.sub.3node"
export exp2_nmm_netcdf_sub_2node="nmm_netcdf.sub.2node"
export exp2_arw_binary_sub_2node="arw_binary.sub.2node"
export exp2_arw_netcdf_sub_2node="arw_netcdf.sub.2node"
export exp2_rtma_sub_2node="rtma.sub.2node"
export exp2_nems_nmmb_sub_3node="nems_nmmb.sub.3node"

export exp1_global_bench_1node="global.bench.1node"
export exp1_nmm_binary_bench_2node="nmm_binary.bench.2node"
export exp1_nmm_netcdf_bench_1node="nmm_netcdf.bench.1node"
export exp1_arw_binary_bench_1node="arw_binary.bench.1node"
export exp1_arw_netcdf_bench_1node="arw_netcdf.bench.1node"
export exp1_rtma_bench_1node="rtma.bench.1node"
export exp1_nems_nmmb_bench_2node="nems_nmmb.bench.2node"

export exp2_global_bench_2node="global.bench.2node"
export exp2_nmm_binary_bench_3node="nmm_binary.bench.3node"
export exp2_nmm_netcdf_bench_2node="nmm_netcdf.bench.2node"
export exp2_arw_binary_bench_2node="arw_binary.bench.2node"
export exp2_arw_netcdf_bench_2node="arw_netcdf.bench.2node"
export exp2_rtma_bench_2node="rtma.bench.2node"
export exp2_nems_nmmb_bench_3node="nems_nmmb.bench.3node"

# Define ptmp location

export ptmp_loc="/ptmp/$USER"

# Define analysis date

export adate_global="2010020112"
export adate_regional="2007122000"
export adate_regional_nmm_binary="2010021600"
export adate_regional_nems_nmmb="2009031600"

# Define machine (added due to almost daily switch between cirrus and stratus and different locations of obs between machines)

machine="cirrus"
#machine="stratus"
#machine="vapor"

# Define obs directory

if [[ $machine = "stratus" ]]; then
   export datobs_global="/global/noscrub/wx20ml/cases/global/sigmap"
   export datobs_rtma="/global/noscrub/wx23jd/gsi_anl/cases/regional/rtma_binary"
   export datobs_nmm_binary="/global/noscrub/wx20ml/cases/regional/ndas_binary"
   export datobs_nems_nmmb="/global/noscrub/wx20ml/nmmb_regression_case"
   export datobs_nmm_netcdf="/global/noscrub/wx23jd/gsi_anl/cases/regional/ndas_binary"
   export datges_nmm_netcdf="/global/noscrub/wx23jd/gsi_anl/cases/regional/nmm_netcdf"
   export datobs_arw_binary="/global/noscrub/wx23jd/gsi_anl/cases/regional/ndas_binary"
   export datges_arw_binary="/global/noscrub/wx23jd/gsi_anl/cases/regional/arw_binary"
   export datobs_arw_netcdf="/global/noscrub/wx23jd/gsi_anl/cases/regional/ndas_binary"
   export datges_arw_netcdf="/global/noscrub/wx23jd/gsi_anl/cases/regional/arw_netcdf"
elif [[ $machine = "cirrus" ]]; then
     export datobs_global="/global/noscrub/wx20ml/cases/global/sigmap"
     export datobs_rtma="/global/noscrub/wx20rt/gsi_anl/cases/regional/rtma_binary"
     export datobs_nmm_binary="/global/noscrub/wx20ml/cases/regional/ndas_binary"
     export datobs_nems_nmmb="/meso/noscrub/wx23dp/nmmb_regression_case"
     export datobs_nmm_netcdf="/global/noscrub/wx20rt/gsi_anl/cases/regional/ndas_binary"
     export datges_nmm_netcdf="/global/noscrub/wx20rt/gsi_anl/cases/regional/nmm_netcdf"
     export datobs_arw_binary="/global/noscrub/wx20rt/gsi_anl/cases/regional/ndas_binary"
     export datges_arw_binary="/global/noscrub/wx20rt/gsi_anl/cases/regional/arw_binary"
     export datobs_arw_netcdf="/global/noscrub/wx20rt/gsi_anl/cases/regional/ndas_binary"
     export datges_arw_netcdf="/global/noscrub/wx20rt/gsi_anl/cases/regional/arw_netcdf"
elif [[ $machine = "vapor" ]]; then
     export datobs_global="/jcsda/noscrub/wx20ml/cases/global/sigmap"
     export datobs_rtma="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/rtma_binary"
     export datobs_nmm_binary="/jcsda/noscrub/wx20ml/cases/regional/ndas_binary"
     export datobs_nems_nmmb="/jcsda/noscrub/wx20ml/nmmb_regression_case"
     export datobs_nmm_netcdf="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/ndas_binary"
     export datges_nmm_netcdf="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/nmm_netcdf"
     export datobs_arw_binary="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/ndas_binary"
     export datges_arw_binary="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/arw_binary"
     export datobs_arw_netcdf="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/ndas_binary"
     export datges_arw_netcdf="/jcsda/noscrub/wx20rt/gsi_anl/cases/regional/arw_netcdf"
fi

# Regression output filename

export global_regression="global_regression_results.txt"
export rtma_regression="rtma_regression_results.txt"
export nmm_binary_regression="nmm_binary_regression_results.txt"
export nmm_netcdf_regression="nmm_netcdf_regression_results.txt"
export arw_binary_regression="arw_binary_regression_results.txt"
export arw_netcdf_regression="arw_netcdf_regression_results.txt"
export nems_nmmb_regression="nems_nmmb_regression_results.txt"

# Regression vfydir

export regression_vfydir="$noscrub/benchmark"

# Control run option

export control="false" # If true, run the extra two control runs for each configuration.  If false, skip the control runs and use data from /noscrub.
export debug="false" # If true, run the extra debug run for each configuration.  If false, skip the debug runs.

# Define location for copying control run data to

export control_RTMA="$noscrub/tmpreg_${rtma}/$exp1_rtma_bench_1node"
export control_RTMA2="$noscrub/tmpreg_${rtma}/$exp2_rtma_bench_2node"
export control_global_T62="$noscrub/tmp${global}/$exp1_global_bench_1node"
export control_global_T622="$noscrub/tmp${global}/$exp2_global_bench_2node"
export control_nmm_binary="$noscrub/tmpreg_${nmm_binary}/$exp1_nmm_binary_bench_2node"
export control_nmm_binary2="$noscrub/tmpreg_${nmm_binary}/$exp2_nmm_binary_bench_3node"
export control_nmm_netcdf="$noscrub/tmpreg_${nmm_netcdf}/$exp1_nmm_netcdf_bench_1node"
export control_nmm_netcdf2="$noscrub/tmpreg_${nmm_netcdf}/$exp2_nmm_netcdf_bench_2node"
export control_arw_binary="$noscrub/tmpreg_${arw_binary}/$exp1_arw_binary_bench_1node"
export control_arw_binary2="$noscrub/tmpreg_${arw_binary}/$exp2_arw_binary_bench_2node"
export control_arw_netcdf="$noscrub/tmpreg_${arw_netcdf}/$exp1_arw_netcdf_bench_1node"
export control_arw_netcdf2="$noscrub/tmpreg_${arw_netcdf}/$exp2_arw_netcdf_bench_2node"
export control_nems_nmmb="$noscrub/tmpreg_${nems_nmmb}/$exp1_nems_nmmb_bench_2node"
export control_nems_nmmb2="$noscrub/tmpreg_${nems_nmmb}/$exp2_nems_nmmb_bench_3node"
