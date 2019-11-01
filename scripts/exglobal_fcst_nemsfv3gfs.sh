#!/bin/ksh
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
## Locate mod_forecast script folder
if [ -z $machine ]; then
	machine='sandbox'
	echo "MAIN: !!!Running in sandbox mode!!!"
	unameOut="$(uname -s)"
	case "${unameOut}" in
    		Linux*)
			SCRIPTDIR=$(dirname $(readlink -f "$0") )/../ush
			echo "MAIN: Linux environment. Current Script locates in $SCRIPTDIR."
			;;
    		Darwin*)
                        SCRIPTDIR=$(pwd)/../ush
                        echo "MAIN: MacOS environment. Current Script locates in $SCRIPTDIR."
			;;
    		CYGWIN*)    echo CYGWIN ;;
    		MINGW*)     echo MinGw ;;
    		*)          echo "UNKNOWN:${unameOut}"
    esac
else
	SCRIPTDIR=$(dirname $(readlink -f "$0") )/../ush
	echo "MAIN: environment loaded for $machine platform,Current Script locates in $SCRIPTDIR."
fi

# include all subroutines. Executions later.
source $SCRIPTDIR/cplvalidate.sh	# validation of cpl*
source $SCRIPTDIR/forecast_predet.sh	# include functions for variable definition
source $SCRIPTDIR/forecast_det.sh  # include functions for run type determination
source $SCRIPTDIR/forecast_postdet.sh	# include functions for variables after run type determination
source $SCRIPTDIR/nems_configure.sh	# include functions for nems_configure processing
source $SCRIPTDIR/parsing_model_configure_FV3.sh

# Compset string. For nems.configure.* template selection. Default ATM only
confignamevarfornems=${confignamevarfornems:-'atm'}

# Coupling control switches, for coupling purpose, off by default
cpl=${cpl:-.false.}
cplflx=${cplflx:-.false.} # default off,import from outside source
cplwav=${cplwav:-.false.} # ? how to control 1-way/2-way?
cplchem=${cplchem:-.false.} # Chemistry model
cplice=${cplice:-.false.} # ICE model

OCNTIM=${OCNTIM:-3600}
DELTIM=${DELTIM:-450}
ICETIM=${DELTIM}

CPL_SLOW=${OCNTIM}
CPL_FAST=${ICETIM}
# Coupling control switches, for coupling purpose, off by default

[[ $machine = 'sandbox' ]] && RUN=gfs

echo "MAIN: $confignamevarfornems selected"
echo "MAIN: Forecast script started for $confignamevarfornems on $machine"

echo "MAIN: Validating $confignamevarfornems with cpl switches"
cplvalidate
echo "MAIN: $confignamevarfornems validated, continue"
# Validate the consistency between $confignamevarfornems and $CPL switches

echo "MAIN: Loading variables before determination of run type"

common_predet

echo $RUN
case $RUN in
	'data') Data_ATM_setup;;
	'gfs') FV3_GFS_predet;;
	'gdas') FV3_GFS_predet;;
	'gefs') FV3_GEFS_predet;;
esac
[[ $cplflx = .true. ]] && MOM6_predet
[[ $cplwav = .true. ]] && WW3_predet
[[ $cplice = .true. ]] && CICE_predet
[[ $cplchem = .true. ]] && GSD_predet

case $RUN in
        'gfs') FV3_GFS_det;;
        'gdas') FV3_GFS_det;;
        'gefs') FV3_GEFS_det;;
esac				#no run type determination for data atmosphere
[[ $cplflx = .true. ]] && MOM6_det
[[ $cplwav = .true. ]] && WW3_det
[[ $cplice = .true. ]] && CICE_det
[[ $cplchem = .true. ]] && GSD_det

echo "MAIN: RUN Type Determined"

echo "MAIN: Post-determination set up of run type"
echo $RUN
case $RUN in
	'gfs') FV3_GFS_postdet;;
	'gdas') FV3_GFS_postdet;;
	'gefs') FV3_GEFS_postdet;;
esac				#no post determination set up for data atmosphere
[[ $cplflx = .true. ]] && MOM6_postdet
[[ $cplwav = .true. ]] && WW3_postdet
[[ $cplice = .true. ]] && CICE_postdet
[[ $cplchem = .true. ]] && GSD_postdet
echo "MAIN: Post-determination set up of run type finished"

echo "MAIN: Writing name lists and model configuration"
case $RUN in
        'gfs') FV3_GFS_nml;;
        'gdas') FV3_GFS_nml;;
        'gefs') FV3_GEFS_nml;;
esac				#no namelist for data atmosphere
[[ $cplflx = .true. ]] && MOM6_nml
[[ $cplwav = .true. ]] && WW3_nml
[[ $cplice = .true. ]] && CICE_nml
[[ $cplchem = .true. ]] && GSD_nml
Common_model_configure
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
else
	echo "MAIN: mpirun launch here"
fi

if [ $machine != 'sandbox' ]; then		
	case $RUN in
		'data') data_out_Data_ATM;;
		'gfs') data_out_GFS;;
		'gdas') data_out_GFS;;
		'gefs') data_out_GEFS;;
	esac
	[[ $cplflx = .true. ]] && MOM6_out
	[[ $cplwav = .true. ]] && WW3_out
	[[ $cplice = .true. ]] && CICE_out
	[[ $cplchem = .true. ]] && GSD_out
else
	echo "MAIN: Running on sandbox mode, no output linking"
fi
echo "MAIN: Output copied to COMROT"

#------------------------------------------------------------------
# Clean up before leaving
if [ $mkdata = "YES" ]; then rm -rf $DATA; fi

#------------------------------------------------------------------
#set +x
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXITING $0 with return code $err >&2
fi

if [ $err != 0 ]; then
  echo "MAIN: $confignamevarfornems Forecast failed"
  exit $err
else
  echo "MAIN: $confignamevarfornems Forecast completed at normal status"
  exit 0
fi
