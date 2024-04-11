#!/usr/bin/bash
# Change to a directory that you can write to
export ATARDIR=/NCEPDEV/emc-global/1year/David.Huber/HERA/test_archive
# Change to the head of your clone
export HOMEgfs=/scratch1/NCEPDEV/global/David.Huber/GW/gw_archive
# Change to the name of an experiment
export PSLOT=minmon
export ROTDIR=/scratch1/NCEPDEV/global/David.Huber/para/comrot/${PSLOT}
# Change to where to archive local forecast products
export ARCDIR="/scratch1/NCEPDEV/global/David.Huber/archive/minmon_test"

set -e
module reset
module use /scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-rocky8/install/modulefiles/Core
module load stack-intel stack-python py-jinja2 py-pyyaml
export PARMgfs=$HOMEgfs/parm
export wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/sorc/wxflow/src"
export PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}"

export RUN=gdas
export PDY=20240302
export cyc=00
export SDATE=2024030118
export assim_freq=6
export NFHRS_PER_GROUP=3
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
export DO_AERO=NO
export DO_OCN=NO
export DO_WAVE=NO
export DO_JEDISNOWDA=NO
export WRITE_DOPOST=.true.
export ARCHIVE_RUN=$RUN
export HPSSARCH="YES"
export LOCALARCH="NO"
export REALTIME="YES"
export ARCH_WARMICFREQ="4"
export ARCH_FCSTICFREQ="1"
export ARCH_CYC="00"
export SAVEFCSTIC="NO"
export assim_freq=6
# Not needed for task, but required by wxflow
export DATA=""
export CDUMP=""

#rm -rf $ATARDIR
#mkdir -p $ATARDIR

cd ${ROTDIR}

${HOMEgfs}/scripts/call_archive.py
