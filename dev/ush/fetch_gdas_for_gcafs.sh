#!/bin/bash
# This script fetches GDAS data for GCAFS from HPSS
# and places it in a new tar file.
set -e
set -u

# Get arguments from command line
if [[ "${#}" -ne 2 ]]; then
    echo "Usage: ${0} <YYYYMMDDHH> <output_directory>"
    exit 1
fi
YYYYMMDDHH=${1}
OUTPUT_DIR=${2}
# Create output directory if it doesn't exist
mkdir -p "${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}/tmp"

# determine GDAS version based on date
if [[ "${YYYYMMDDHH}" -ge "2022112900" ]]; then
    gdas_version="v16.3"
elif [[ "${YYYYMMDDHH}" -ge "2022062700" ]]; then
    gdas_version="v16.2"
else
    gdas_version="prod"
fi

# break date and time into components
cycle_Y=${YYYYMMDDHH:0:4}
cycle_M=${YYYYMMDDHH:4:2}
cycle_D=${YYYYMMDDHH:6:2}
cyc=${YYYYMMDDHH:8:2}
# cycle_YM is YYYYMM
cycle_YM="${cycle_Y}${cycle_M}"
# PDY is YYYYMMDD
PDY="${cycle_Y}${cycle_M}${cycle_D}"

# HPSS path for the two tar files
hpss_path_root="/NCEPPROD/hpssprod/runhistory/rh${cycle_Y}/${cycle_YM}/${PDY}"
hpss_file_nc="com_gfs_${gdas_version}_gdas.${PDY}_${cyc}.gdas_nc.tar"

# get the names of the files to extract
atmanl="./gdas.${PDY}/${cyc}/atmos/gdas.t${cyc}z.atmanl.nc"
sfcanl="./gdas.${PDY}/${cyc}/atmos/gdas.t${cyc}z.sfcanl.nc"

# Fetch the tar files from HPSS
cd "${OUTPUT_DIR}/tmp"

htar -xvf "${hpss_path_root}/${hpss_file_nc}" "${atmanl}" "${sfcanl}"

# create the output tar files
echo "creating output tar files"
tar cvf "${hpss_file_nc}" "${atmanl}" "${sfcanl}"

# Move the tar files to the output directory
echo "moving tar files to ${OUTPUT_DIR}"
mv "${hpss_file_nc}" "${OUTPUT_DIR}/"

# Clean up temporary directory
rm -rf "${OUTPUT_DIR}/tmp"
echo "GDAS data for ${YYYYMMDDHH} has been successfully fetched and stored in ${OUTPUT_DIR}."
# End of script
exit 0
