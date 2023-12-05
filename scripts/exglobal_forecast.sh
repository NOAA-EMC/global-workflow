#! /usr/bin/env bash

################################################################################
## UNIX Script Documentation Block
## Script name:         exglobal_forecast.sh
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
##	2. ufs.configure
##	3. model_configure
##	4. input.nml
#######################
# Main body starts here
#######################

source "${HOMEgfs}/ush/preamble.sh"

# include all subroutines. Executions later.
source "${HOMEgfs}/ush/forecast_predet.sh"	# include functions for variable definition
source "${HOMEgfs}/ush/forecast_det.sh"  # include functions for run type determination
source "${HOMEgfs}/ush/forecast_postdet.sh"	# include functions for variables after run type determination
source "${HOMEgfs}/ush/ufs_configure.sh"	# include functions for ufs.configure processing
source "${HOMEgfs}/ush/parsing_model_configure_FV3.sh"

# Coupling control switches, for coupling purpose, off by default
cpl=${cpl:-.false.}
cplflx=${cplflx:-.false.} # default off,import from outside source
cplwav=${cplwav:-.false.} # ? how to control 1-way/2-way?
cplchm=${cplchm:-.false.} # Chemistry model
cplice=${cplice:-.false.} # ICE model

OCNTIM=${OCNTIM:-1800}
DELTIM=${DELTIM:-450}
ICETIM=${DELTIM}

CPL_SLOW=${CPL_SLOW:-${OCNTIM}}
CPL_FAST=${CPL_FAST:-${ICETIM}}

echo "MAIN: Loading common variables before determination of run type"
common_predet

echo "MAIN: Loading variables before determination of run type"
FV3_predet
[[ ${cplflx} = .true. ]] && MOM6_predet
[[ ${cplwav} = .true. ]] && WW3_predet
[[ ${cplice} = .true. ]] && CICE_predet
echo "MAIN: Variables before determination of run type loaded"

echo "MAIN: Determining run type"
FV3_det
[[ ${cplflx} = .true. ]] && MOM6_det
[[ ${cplwav} = .true. ]] && WW3_det
[[ ${cplice} = .true. ]] && CICE_det
echo "MAIN: RUN Type Determined"

echo "MAIN: Post-determination set up of run type"
FV3_postdet
[[ ${cplflx} = .true. ]] && MOM6_postdet
[[ ${cplwav} = .true. ]] && WW3_postdet
[[ ${cplice} = .true. ]] && CICE_postdet
[[ ${cplchm} = .true. ]] && GOCART_postdet
echo "MAIN: Post-determination set up of run type finished"

echo "MAIN: Writing namelists and model configuration"
FV3_nml
[[ ${cplflx} = .true. ]] && MOM6_nml
[[ ${cplwav} = .true. ]] && WW3_nml
[[ ${cplice} = .true. ]] && CICE_nml
[[ ${cplchm} = .true. ]] && GOCART_rc
FV3_model_configure
echo "MAIN: Name lists and model configuration written"

echo "MAIN: Writing UFS Configure file"
writing_ufs_configure
echo "MAIN: UFS configured"

#------------------------------------------------------------------
# run the executable

if [[ "${esmf_profile:-}" = ".true." ]]; then
  export ESMF_RUNTIME_PROFILE=ON
  export ESMF_RUNTIME_PROFILE_OUTPUT=SUMMARY
fi

${NCP} "${FCSTEXECDIR}/${FCSTEXEC}" "${DATA}/"
${APRUN_UFS} "${DATA}/${FCSTEXEC}" 1>&1 2>&2
export ERR=$?
export err=${ERR}
${ERRSCRIPT} || exit "${err}"

FV3_out
[[ ${cplflx} = .true. ]] && MOM6_out
[[ ${cplwav} = .true. ]] && WW3_out
[[ ${cplice} = .true. ]] && CICE_out
[[ ${cplchm} = .true. ]] && GOCART_out
[[ ${esmf_profile:-} = .true. ]] && CPL_out
echo "MAIN: Output copied to COMROT"

#------------------------------------------------------------------

exit "${err}"
