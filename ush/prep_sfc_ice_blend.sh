#!/usr/bin/env bash

####  UNIX Script Documentation Block ###################################
#
# Script name:  prep_sfc_ice_blend.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls the emcsfc_ice_blend program to create
#    a global ice concentation from a blend of National Ice Center IMS data
#    and EMC/MMAB 5-minute data.
#
# Script History Log:
#    July  2014  Gayno   Initial version
#    Oct   2014  Gayno   The emcsfc_ice_blend program was modified to
#                        use all grib 2 for i/o.  Modify default
#                        filenames to use 'grib2' extension.
#    Aug   2015  Gayno   Convert output blended ice file from
#                        grib 2 to grib 1.  Bring up to NCO standards.
#
# Usage:
#    Parameters:    [no arguments]
#    Input Files:
#      $IMS_FILE               - ims ice cover data (grib 1 or 2)
#      $FIVE_MIN_ICE_FILE      - 5-minute global ice concentration (grib 2)
#      $FIVE_MIN_ICE_MASK_FILE - land/sea mask of $FIVE_MIN_ICE_FILE
#                                (grib 2)
#    Output Files:
#      $BLENDED_ICE_FILE - blended ice concentration file. program
#                          produces grib 2.  script converts to
#                          grib 1 expected by gfs.
#
# Condition codes:
#      0 - normal termination
#  non 0 - input data is missing or emcsfc_ice_blend program
#          terminated abnormally.
#
# Attributes:
#     Language:  RedHat Linux
#     Machine:   NCEP WCOSS
#
#########################################################################

export pgm=emcsfc_ice_blend

#------------------------------------------------------------------------
# Set up script variables
#------------------------------------------------------------------------

# the input data.  ims may be grib1 or grib2.  five_min files are grib 2.
IMS_FILE=${IMS_FILE:-"ims.grib2"}
FIVE_MIN_ICE_FILE=${FIVE_MIN_ICE_FILE:-"seaice.5min.grib2"}
FIVE_MIN_ICE_MASK_FILE=${FIVE_MIN_ICE_MASK_FILE:-"${FIXgfs}/am/emcsfc_gland5min.grib2"}

# the output ice blend data (grib)
BLENDED_ICE_FILE=${BLENDED_ICE_FILE:-"seaice.5min.blend"}

# the program executable
BLENDICEEXEC=${BLENDICEEXEC:-"${EXECgfs}/emcsfc_ice_blend"}

# standard output file
pgmout=${pgmout:-"OUTPUT"}

#------------------------------------------------------------------------
# Interpolate ims data to ncep grid 173 (the grid used by mmab 5-minute ice data).
# If ims is grib1 format, convert to grib2.  If ims data is missing, then
# don't run ice blend program.  Copy old blended data to current directory.
#------------------------------------------------------------------------

if [[ -f "${IMS_FILE}" ]]; then
  cpfs "${IMS_FILE}" ./ims.grib2
  ${WGRIB2} ims.grib2 -match "ICEC" -grib ims.icec.grib2
  grid173="0 0 0 0 0 0 0 0 4320 2160 0 0 89958000 42000 48 -89958000 359958000 83000 83000 0"
  ${COPYGB2} -x -i3 -g "${grid173}" ims.icec.grib2 ims.icec.5min.grib2
else
  echo "WARNING: IMS ${IMS_FILE} ice data missing. Can not run program ${pgm}."
  exit 3
fi

#------------------------------------------------------------------------
# Does 5-minute ice data exist?  If not, don't run ice blend program.
# Copy old blended data to current directory.
#------------------------------------------------------------------------

if [[ ! -f "${FIVE_MIN_ICE_FILE}" ]]
then
  echo "WARNING: ${FIVE_MIN_ICE_FILE} data missing. Can not run program ${pgm}."
  exit 5
fi

#------------------------------------------------------------------------
# Run program to blend data.
#------------------------------------------------------------------------

source prep_step

# These are input files.
export FORT17="${FIVE_MIN_ICE_MASK_FILE}"
export FORT11="ims.icec.5min.grib2"
export FORT15="${FIVE_MIN_ICE_FILE}"

# This is the output blended file
export FORT51="${BLENDED_ICE_FILE}"

${BLENDICEEXEC} >> "${pgmout}" 2> errfile
export err=$?

#------------------------------------------------------------------------
# Did program run without error?
#
# If no, copy old blended ice file to current directory.
#
# If yes, convert file from grib 2 to grib 1 expected by gfs as
# follows:
#    - use wgrib 2 to set corner point longitude to 0.042 degrees
#      to prevent round-off error during the cnvgrib step.
#    - use cnvgrib to convert from grib2 to grib 1.
#    - use copygb to replace bitmap with '1.57' land flag value
#      expected by global cycle program.
#------------------------------------------------------------------------

if [[ "${err}" -ne 0 ]]; then
  echo "WARNING: ${pgm} completed abnormally. The old ice blend file will be used."
  # Exit but do not call err_exit. Calling script will handle use of older file
  exit "${err}"
else
  ${WGRIB2} -set_int 3 51 42000 "${BLENDED_ICE_FILE}" -grib "${BLENDED_ICE_FILE}.corner"
  ${CNVGRIB} -g21 "${BLENDED_ICE_FILE}.corner" "${BLENDED_ICE_FILE}.bitmap"
  rm -f "${BLENDED_ICE_FILE}"
  ${COPYGB} -M "#1.57" -x "${BLENDED_ICE_FILE}.bitmap" "${BLENDED_ICE_FILE}"
  cpfs "${BLENDED_ICE_FILE}" "${COMOUT_OBS}"
  rm -f "${BLENDED_ICE_FILE}.corner" "${BLENDED_ICE_FILE}.bitmap"
fi

exit 0
