#!/bin/sh

################################################################################
## UNIX Script Documentation Block
## Script name:         exglobal_fcst_nemsfv3gfs.sh
## Script description:  Runs a global FV3GFS model forecast
##
## Author:   Fanglin Yang       Organization: NCEP/EMC       Date: 2016-11-15
## Abstract: This script runs a single GFS forecast with FV3 dynamical core.
##           This script is created based on a C-shell script that GFDL wrote
##           for the NGGPS Phase-II Dycore Comparison Project.
##
## Script history log:
## 2016-11-15  Fanglin Yang   First Version.
## 2017-02-09  Rahul Mahajan  Added warm start and restructured the code.
## 2017-03-10  Fanglin Yang   Updated for running forecast on Cray.
## 2017-03-24  Fanglin Yang   Updated to use NEMS FV3GFS with IPD4
## 2017-05-24  Rahul Mahajan  Updated for cycling with NEMS FV3GFS
## 2017-09-13  Fanglin Yang   Updated for using GFDL MP and Write Component
## 2019-04-02  
##
## Attributes:
##   Language: Portable Operating System Interface (POSIX) Shell
##   Machines: All supported platforms
##
## Usage (Arguments)
##	No command line argument
##
## Data input (location, name)
## 	Warm start files:
## 		1. restart file except sfc_data, $gmemdir/RESTART/$PDY.$cyc.*.nc
##		2. sfcanl_data, $memdir/RESTART/$PDY.$cyc.*.nc
##		3. coupler_res, $gmemdir/RESTART/$PDY.$cyc.coupler.res
##		4. increment file, $memdir/${CDUMP}.t${cyc}z.atminc.nc
##			OR $DATA/INPUT/fv3_increment.nc
##	Cold start files:
##		1. initial condition, $memdir/INPUT/*.nc
##	Restart files:
##
##	Fix files: 
##		1. computing grid, $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc
##		2. orography data, $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc
##		3. mosaic data, $FIXfv3/$CASE/${CASE}_mosaic.nc
##		4. Global O3 data, $FIX_AM/${O3FORC}
##		5. Global H2O data, $FIX_AM/${H2OFORC}
##		6. Global solar constant data, $FIX_AM/global_solarconstant_noaa_an.txt
##		7. Global surface emissivity, $FIX_AM/global_sfc_emissivity_idx.txt
##		8. Global CO2 historical data, $FIX_AM/global_co2historicaldata_glob.txt
##		8. Global CO2 monthly data, $FIX_AM/co2monthlycyc.txt
##		10. Additional global CO2 data, $FIX_AM/fix_co2_proj/global_co2historicaldata
##		11. Climatological aerosol global distribution 
##			$FIX_AM/global_climaeropac_global.txt
## 		12. Monthly volcanic forcing $FIX_AM/global_volcanic_aerosols_YYYY-YYYY.txt
##
## Data output (location, name)
##	If quilting=true and output grid is gaussian grid:
##	   1. atmf data, $memdir/${CDUMP}.t${cyc}z.atmf${FH3}.$OUTPUT_FILE
##	   2. sfcf data, $memdir/${CDUMP}.t${cyc}z.sfcf${FH3}.$OUTPUT_FILE
##	   3. logf data, $memdir/${CDUMP}.t${cyc}z.logf${FH3}.$OUTPUT_FILE
##	If quilting=false and output grid is not gaussian grid:
##           1. NGGPS2D, $memdir/nggps2d.tile${n}.nc
##	   2. NGGPS3D, $memdir/nggps3d.tile${n}.nc
##	   3. grid spec, $memdir/grid_spec.tile${n}.nc
##	   4. atmospheric static tiles, $memdir/atmos_static.tile${n}.nc
##	   5. atmospheric 4x daily tiles, $memdir/atmos_4xdaily.tile${n}.nc
##
## Status output 
##	0: Normal 
##	others: Error
##
## Namelist input, in RUNDIR,
##	1. diag_table
##	2. nems.configure
##	3. model_configure
##	4. input.nml

#######################
# Main body starts here
#######################

VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXECUTING $0 $* >&2
  set -x
fi

## Run in sandbox if no $machine defined
## Current script folder
if [ -z $machine ]; then
	machine='sandbox'
	echo "MAIN: !!!Running in sandbox mode!!!"
	unameOut="$(uname -s)"
	case "${unameOut}" in
    		Linux*)
			SCRIPTDIR=$(dirname $(readlink -f "$0") )/mod_forecast
			echo "MAIN: Linux environment. Current Script locates in $SCRIPTDIR."
			;;
    		Darwin*)
                        SCRIPTDIR=$(pwd)/mod_forecast
                        echo "MAIN: MacOS environment. Current Script locates in $SCRIPTDIR."
			;;
    		CYGWIN*)    echo CYGWIN ;;
    		MINGW*)     echo MinGw ;;
    		*)          echo "UNKNOWN:${unameOut}"
    esac
else
	SCRIPTDIR=$(dirname $(readlink -f "$0") )/mod_forecast
	echo "MAIN: environment loaded for $machine platform,Current Script locates in $SCRIPTDIR."
fi

source $SCRIPTDIR/cplvalidate.sh	# validation of cpl*
source $SCRIPTDIR/forecast_def.sh	# include functions for variable definition
source $SCRIPTDIR/forecast_io.sh	# include functions executing later
source $SCRIPTDIR/nems_configure.sh

confignamevarfornems=${confignamevarfornems:-'atm'}

CPLFLX=${CPLFLX:-False} # default off,import from outside source
CPLWAV=${CPLWAV:-False} # ? how to control 1-way/2-way?
CPLCHEM=${CPLCHEM:-False} #
CPLICE=${CPLICE:-False}
#CPLFLX=TRUE				# Pseudo
#CPLICE=TRUE				# Pseudo
#RUN='gfs'				# debug setting

echo "MAIN: $confignamevarfornems selected"
echo "MAIN: Forecast script started for $confignamevarfornems on $machine"

echo "MAIN: Validating $confignamevarfornems with cpl switches"
cplvalidate
echo "MAIN: $confignamevarfornems validated, continue"


echo "MAIN: Loading variables"
echo $RUN
case $RUN in
	'gfs') FV3_GFS_def;;
	'gdas') FV3_GFS_def;;
	'gefs') FV3_GEFS_def;;
esac
[[ $CPLFLX = TRUE ]] && HYCOM_def
[[ $CPLWAV = TRUE ]] && WW3_def
[[ $CPLICE = TRUE ]] && CICE_def
[[ $CPLCHEM = TRUE ]] && GSD_def

echo "MAIN: Finish loading variables"

echo "MAIN: Linking input data"
echo $RUN
case $RUN in
	'gfs') data_link_GFS;;
	'gdas') data_link_GFS;;
	'gefs') data_link_GEFS;;
esac
[[ $CPLFLX = TRUE ]] && HYCOM_in
[[ $CPLWAV = TRUE ]] && WW3_in
[[ $CPLICE = TRUE ]] && CICE_in
[[ $CPLCHEM = TRUE ]] && GSD_in
echo "MAIN: Input data linked"

echo "MAIN: Writing name lists and model configuration"
FV3_nml
[[ $CPLFLX = TRUE ]] && HYCOM_nml
[[ $CPLWAV = TRUE ]] && WW3_nml
[[ $CPLICE = TRUE ]] && CICE_nml
[[ $CPLCHEM = TRUE ]] && GSD_nml
echo "MAIN: Name lists and model configuration written"

echo "MAIN: Writing NEMS Configure file"
writing_nems_configure
echo "MAIN: NEMS configured"

#------------------------------------------------------------------
# run the executable

if [ $machine != 'sandbox' ]; then
	$NCP $FCSTEXECDIR/$FCSTEXEC $DATA/.
	export OMP_NUM_THREADS=$NTHREADS_FV3
	$APRUN_FV3 $DATA/$FCSTEXEC 1>&1 2>&2
	export ERR=$?
	export err=$ERR
	$ERRSCRIPT || exit $err
	mpirun -np $NPROC executable
else
	echo "MAIN: mpirun launch here"
fi

if [ $machine != 'sandbox' ]; then		
	case $RUN in
		'gfs') data_out_GFS;;
		'gdas') data_out_GFS;;
		'gefs') data_out_GEFS;;
	esac
	[[ $CPLFLX = TRUE ]] && HYCOM_out
	[[ $CPLWAV = TRUE ]] && WW3_out
	[[ $CPLICE = TRUE ]] && CICE_out
	[[ $CPLCHEM = TRUE ]] && GSD_out
else
	echo "MAIN: Running on sandbox mode, no output linking"
fi
echo "MAIN: Output copied to COMROT"

#------------------------------------------------------------------
# Clean up before leaving
if [ $mkdata = "YES" ]; then rm -rf $DATA; fi

#------------------------------------------------------------------
set +x
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXITING $0 with return code $err >&2
fi

echo "MAIN: $confignamevarfornems Forecast completed at normal status"
exit 0
