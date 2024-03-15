#!/usr/bin/bash
# Change to a directory that you can write to
export ATARDIR=/work/noaa/global/dhuber/para/stmp/test_arch

set -e
module use /work/noaa/epic/role-epic/spack-stack/hercules/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core
module load stack-intel stack-python py-jinja2 py-pyyaml
export HOMEgfs=/work/noaa/global/dhuber/GW/gw_arch
export wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/sorc/wxflow/src"
export PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}"

export ROTDIR=/work2/noaa/global/dhuber/para/com/herc_mon
export RUN=gdas
export PDY=20211111
export cyc=00
export COM_ATMOS_ANALYSIS=$ROTDIR/$RUN.$PDY/$cyc/analysis/atmos
export COM_ATMOS_BUFR=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/bufr
export COM_ATMOS_GEMPAK=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/gempak/
export COM_ATMOS_GENESIS=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/cyclone/genesis_vital
export COM_ATMOS_HISTORY=$ROTDIR/$RUN.$PDY/$cyc/model_data/atmos/history
export COM_ATMOS_INPUT=$ROTDIR/$RUN.$PDY/$cyc/model_data/atmos/input
export COM_ATMOS_MASTER=$ROTDIR/$RUN.$PDY/$cyc/model_data/atmos/master
export COM_ATMOS_RESTART=$ROTDIR/$RUN.$PDY/$cyc/model_data/atmos/restart
export COM_ATMOS_TRACK=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/cyclone/tracks
export COM_ATMOS_WMO=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/wmo
export COM_CHEM_HISTORY=$ROTDIR/$RUN.$PDY/$cyc/model_data/chem/history
export COM_CHEM_ANALYSIS=$ROTDIR/$RUN.$PDY/$cyc/analysis/chem
export COM_MED_RESTART=$ROTDIR/$RUN.$PDY/$cyc/model_data/med/restart
export COM_ICE_HISTORY=$ROTDIR/$RUN.$PDY/$cyc/model_data/ice/history
export COM_ICE_INPUT=$ROTDIR/$RUN.$PDY/$cyc/model_data/ice/input
export COM_ICE_RESTART=$ROTDIR/$RUN.$PDY/$cyc/model_data/ice/restart
export COM_ICE_GRIB=$ROTDIR/$RUN.$PDY/$cyc/products/ice/grib2
export COM_OBS=$ROTDIR/$RUN.$PDY/$cyc/obs
export COM_TOP=$ROTDIR/$RUN.$PDY/$cyc
export COM_OCEAN_HISTORY=$ROTDIR/$RUN.$PDY/$cyc/model_data/ocean/history
export COM_OCEAN_INPUT=$ROTDIR/$RUN.$PDY/$cyc/model_data/ocean/input
export COM_OCEAN_RESTART=$ROTDIR/$RUN.$PDY/$cyc/model_data/ocean/restart
export COM_OCEAN_GRIB=$ROTDIR/$RUN.$PDY/$cyc/products/ocean/grib2
export COM_OCEAN_NETCDF=$ROTDIR/$RUN.$PDY/$cyc/products/ocean/netcdf
export COM_OCEAN_ANALYSIS=$ROTDIR/$RUN.$PDY/$cyc/analysis/ocean
export COM_WAVE_GRID=$ROTDIR/$RUN.$PDY/$cyc/products/wave/gridded
export COM_WAVE_HISTORY=$ROTDIR/$RUN.$PDY/$cyc/model_data/wave/history
export COM_WAVE_STATION=$ROTDIR/$RUN.$PDY/$cyc/products/wave/station
export COM_WAVE_RESTART=$ROTDIR/$RUN.$PDY/$cyc/products/wave/restart
export COM_ATMOS_OZNMON=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/oznmon
export COM_ATMOS_RADMON=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/radmon
export COM_ATMOS_MINMON=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/minmon
export COM_CONF=$ROTDIR/$RUN.$PDY/$cyc/conf
export COM_ATMOS_GRIB_0p25=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/grib2/0p25
export COM_ATMOS_GRIB_0p50=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/grib2/0p50
export COM_ATMOS_GRIB_1p00=$ROTDIR/$RUN.$PDY/$cyc/products/atmos/grib2/1p00

export FHMIN=0
export FHMAX=9
export FHOUT=1
export DO_VERFRAD=YES
export DO_VMINMON=YES
export DO_VERFOZN=YES
export DO_ICE=NO
export WRITE_DOPOST=YES
export ARCHIVE_RUN=$RUN
export PARMgfs=/work/noaa/global/dhuber/GW/gw_arch/parm
# Not needed for task, but required by wxflow
export DATA=""
export CDUMP=""

rm -rf $ATARDIR
mkdir $ATARDIR

cd ${ROTDIR}

/work/noaa/global/dhuber/GW/gw_arch/scripts/call_archive.py
