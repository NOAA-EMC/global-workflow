#!/bin/sh -xe
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
## NCEP EMC GLOBAL MODEL VERIFICATION
##
## CONTRIBUTORS: Mallory Row, mallory.row@noaa.gov, NOAA/NWS/NCEP/EMC-VPPGB
## PURPOSE: Used to run the verif_global package in a stand alone mode.
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------

topdir=`eval "cd ../;pwd"`
export HOMEverif_global=${HOMEverif_global:-$topdir}  # Home base of verif_global

echo "=============== SOURCING CONFIGS ==============="
passed_config=$1
passed_config_strlength=$(echo -n $passed_config | wc -m)
if [ $passed_config_strlength = 0 ]; then
    echo "No config passed, using default: $HOMEverif_global/parm/config/config.vrfy"
    config=$HOMEverif_global/parm/config/config.vrfy
else
    config=$(readlink -f $passed_config)
    if [ ! -e $config ]; then
        echo "The passed config $config does not exist"
        exit 1
    else
        echo "Using passed config: $config"
    fi
fi
. $config
status=$?
[[ $status -ne 0 ]] && exit $status
[[ $status -eq 0 ]] && echo "Succesfully sourced ${config}"
echo

echo "=============== SETTING UP ==============="
. $HOMEverif_global/ush/set_up_verif_global.sh
status=$?
[[ $status -ne 0 ]] && exit $status
[[ $status -eq 0 ]] && echo "Succesfully ran set_up_verif_global.sh"

echo "=============== RUNNING METPLUS ==============="
if [ $RUN_GRID2GRID_STEP1 = YES ] ; then
    echo
    echo "===== RUNNING GRID-TO-GRID STEP 1 VERIFICATION  ====="
    echo "===== creating partial sum data for grid-to-grid verifcation using METplus ====="
    export RUN="grid2grid_step1"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_GRID2GRID_STEP2 = YES ] ; then
    echo
    echo "===== RUNNING GRID-TO-GRID STEP 2 VERIFICATION  ====="
    echo "===== calculating statistics and creating plots for grid-to-grid verifcation using METplus ====="
    export RUN="grid2grid_step2"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_GRID2OBS_STEP1 = YES ] ; then
    echo
    echo "===== RUNNING GRID-TO-OBSERVATIONS STEP 1 VERIFICATION  ====="
    echo "===== creating partial sum data for grid-to-observations verifcation using METplus ====="
    export RUN="grid2obs_step1"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_GRID2OBS_STEP2 = YES ] ; then
    echo
    echo "===== RUNNING GRID-TO-OBSERVATIONS STEP 2 VERIFICATION  ====="
    echo "===== calculating statistics and creating plots for grid-to-observations verifcation using METplus ====="
    export RUN="grid2obs_step2"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_PRECIP_STEP1 = YES ] ; then
    echo
    echo "===== RUNNING PRECIPITATION STEP 1 VERIFICATION  ====="
    echo "===== creating partial sum data for precipitation verifcation using METplus ====="
    export RUN="precip_step1"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_PRECIP_STEP2 = YES ] ; then
    echo
    echo "===== RUNNING PRECIPITATION STEP 2 VERIFICATION  ====="
    echo "===== calculating statistics and creating plots for precipitation verifcation using METplus ====="
    export RUN="precip_step2"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_SATELLITE_STEP1 = YES ] ; then
    echo
    echo "===== RUNNING SATELLITE STEP 1 VERIFICATION  ====="
    echo "===== creating partial sum data for satellite verifcation using METplus ====="
    export RUN="satellite_step1"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_SATELLITE_STEP2 = YES ] ; then
    echo
    echo "===== RUNNING SATELLITE STEP 2 VERIFICATION  ====="
    echo "===== calculating statistics and creating plots for satellite verifcation using METplus ====="
    export RUN="satellite_step2"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_FIT2OBS_PLOTS = YES ] ; then
    echo
    echo "===== RUNNING FIT-TO-OBS PLOTTING  ====="
    echo "===== plotting fit-to-obs data ====="
    export RUN="fit2obs_plots"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_TROPCYC = YES ] ; then
    echo
    echo "===== RUNNING TROPICAL CYCLONE VERIFICATION  ====="
    echo "===== calculating and plotting track and intensity error using METplus ====="
    export RUN="tropcyc"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_MAPS2D = YES ] ; then
    echo
    echo "===== RUNNING MODEL-TO-MODEL COMPARISON AND MODEL-TO-OBSERVATION ERROR VERIFICATION  ====="
    echo "===== calculating and plotting mean errors using METplus ====="
    export RUN="maps2d"
    python $HOMEverif_global/ush/run_batch.py
fi

if [ $RUN_MAPSDA = YES ] ; then
    echo
    echo "===== RUNNING GDAS ANALYSIS AND ENSEMBLE COMAPRISON VERIFICATION  ====="
    export RUN="mapsda"
    python $HOMEverif_global/ush/run_batch.py
fi
