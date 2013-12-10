#!/bin/sh

# Link script to link in CRTM release fixfiles into a single directory.
#
# $Id$

script_id()
{
  REVISION='$Revision$'
  LAST_CHANGED_DATE='$LastChangedDate$'
  echo
  echo "${SCRIPT_NAME} ${REVISION} ${LAST_CHANGED_DATE}"
  echo " "`date`
  echo " Support email: NCEP.List.EMC.JCSDA_CRTM.Support@noaa.gov"
}

usage()
{
  echo
  echo " Usage: link_crtm_coeffs.sh [-xhla] source-dir dest-dir"
  echo
  echo "   Link in CRTM release fixfiles into a single directory."
  echo
  echo " Options:"
  echo "   -l"
  echo "         Link in little-endian files. BIG-ENDIAN is the default."
  echo
  echo "   -a"
  echo "         Link in ODAS TauCoeff files. ODPS is the default."
  echo
  echo "         Note: Currently there are no ODPS TauCoeff files"
  echo "               for visible sensors (we're working on it)."
  echo
  echo "   -x"
  echo "         Turn on execution tracing"
  echo
  echo "   -h"
  echo "         Print this message"
  echo
  echo " Arguments:"
  echo "   source-dir"
  echo "         The /fix directory location of the CRTM release."
  echo "         This directory must already exist."
  echo
  echo "   dest-dir"
  echo "         The directory into which the coefficients will be linked."
  echo "         If this directory does not exist, it is created."
  echo
}

error_message()
{
  MESSAGE=$1
  echo >&2
  echo "  **************************" >&2
  echo "  ${SCRIPT_NAME}(ERROR): ${MESSAGE}" >&2
  echo "  **************************" >&2
}

link_coeff_files()
{
  COEFF_DIR=$1
  COEFF_FILES=`ls ${COEFF_DIR}`
  for FILE in ${COEFF_FILES}; do
    ${LINK} ${COEFF_DIR}/${FILE} ${FILE}
    if [ $? -ne 0 ]; then
      error_message "Error linking in ${FILE} from ${COEFF_DIR}. Exiting."
      exit ${FAILURE}
    fi
  done
}


########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

script_id

# Setup
SCRIPT_NAME=`basename $0`

# ...Commands
LINK="ln -sf"

# ...Definitions
SUCCESS=0
FAILURE=1

# ..Define defaults
ENDIAN_TYPE="Big_Endian"
TAUCOEFF_TYPE="ODPS"



# Parse command line options
while getopts :xhla OPTVAL; do

  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options here
  case ${OPTVAL} in
    l)  ENDIAN_TYPE="Little_Endian";;
    a)  TAUCOEFF_TYPE="ODAS";;
    x)  set -x;;
    h)  usage; exit ${SUCCESS};;
    \?) OPTVAL=${OPTARG}; break;;
  esac
done

# ...Remove the options processed
shift $((OPTIND - 1))

# ...Output invalidities based on OPTVAL
case ${OPTVAL} in

  # If OPTVAL contains nothing, then all options
  # have been successfully parsed.
  # So, check for presence of mandatory arguments.
  \?) if [ $# -lt 2 ]; then
        usage
        error_message "Missing source-dir and/or dest-dir arguments"
        exit ${FAILURE}
      fi;;

  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit ${FAILURE};;
esac



# Transfer the arguments
SOURCE_DIR=$1
DEST_DIR=$2



# Check the directory arguments

# ...Ensure the source directory exists
if [ ! -d ${SOURCE_DIR} ]; then
  usage
  error_message "Source directory '${SOURCE_DIR}' does not exist. Exiting."
  exit ${FAILURE}
fi

# ...Get the absolute path to the source directory
CURRENT_DIR=${PWD}
cd ${SOURCE_DIR}; SOURCE_DIR=`pwd -L`; cd ${CURRENT_DIR}

# ...Create the destination directory if necessary
if [ ! -d ${DEST_DIR} ]; then
  mkdir ${DEST_DIR}
  if [ $? -ne ${SUCCESS} ]; then
    error_message "Error creating destination directory '${DEST_DIR}'. Exiting."
    exit ${FAILURE}
  fi
fi



# Begin the linking process

echo
echo "Linking coefficient files from root source directory,"
echo "  ${SOURCE_DIR}"
echo "to destination directory,"
echo "  ${DEST_DIR}"

# ...Go to destination
cd ${DEST_DIR}
if [ $? -ne ${SUCCESS} ]; then
  error_message "Error cd'ing to destination directory '${DEST_DIR}'. Exiting."
  exit ${FAILURE}
fi

# ...List of coefficient directories 
ALL_COEFF_DIRS="\
  AerosolCoeff \
  CloudCoeff \
  EmisCoeff/MW_Water \
  EmisCoeff/IR_Water \
  EmisCoeff/IR_Land/SEcategory \
  EmisCoeff/IR_Snow/SEcategory \
  EmisCoeff/IR_Ice/SEcategory \
  EmisCoeff/VIS_Water/SEcategory \
  EmisCoeff/VIS_Land/SEcategory \
  EmisCoeff/VIS_Snow/SEcategory \
  EmisCoeff/VIS_Ice/SEcategory \
  SpcCoeff \
  TauCoeff/${TAUCOEFF_TYPE}"
  
# ...Visit each directory
for COEFF_DIR in ${ALL_COEFF_DIRS}; do
  echo "...linking ${COEFF_DIR} coefficient files..."
  link_coeff_files "${SOURCE_DIR}/${COEFF_DIR}/${ENDIAN_TYPE}"
  if [ $? -ne 0 ]; then
    error_message "Error linking in ${COEFF_DIR} coefficient files. Exiting."
    exit ${FAILURE}
  fi
done


# Special case renaming of airs281_aqua and cris399_npp 
# SpcCoeff and TauCoeff datafiles.
${LINK} airs281_aqua.SpcCoeff.bin airs281SUBSET_aqua.SpcCoeff.bin
${LINK} airs281_aqua.TauCoeff.bin airs281SUBSET_aqua.TauCoeff.bin
${LINK} cris399_npp.SpcCoeff.bin cris_npp.SpcCoeff.bin
${LINK} cris399_npp.TauCoeff.bin cris_npp.TauCoeff.bin



# Return to original directory
cd ${CURRENT_DIR}
