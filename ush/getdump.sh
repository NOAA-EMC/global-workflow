#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

COMPONENT=${COMPONENT:-atmos}

CDATE=${1:-""}
RUN=${2:-""}
SOURCE_DIR=${3:-$DMPDIR/${RUN}${DUMP_SUFFIX}.${PDY}/${cyc}/${COMPONENT}}
TARGET_DIR=${4:-$ROTDIR/${RUN}.${PDY}/${cyc}/${COMPONENT}}

DUMP_SUFFIX=${DUMP_SUFFIX:-""}

# Exit if SORUCE_DIR does not exist
if [ ! -s $SOURCE_DIR ]; then 
   echo "***ERROR*** DUMP SOURCE_DIR=$SOURCE_DIR does not exist"
   exit 99
fi
   

# Create TARGET_DIR if is does not exist
if [ ! -s $TARGET_DIR ]; then mkdir -p $TARGET_DIR ;fi


# Set file prefix
cyc=$(echo $CDATE |cut -c 9-10)
prefix="$RUN.t${cyc}z."


# Link dump files from SOURCE_DIR to TARGET_DIR
cd $SOURCE_DIR
if [ -s ${prefix}updated.status.tm00.bufr_d ]; then
    for file in $(ls ${prefix}*); do
	${NLN} $SOURCE_DIR/$file $TARGET_DIR/$file
    done
else
    echo "***ERROR*** ${prefix}updated.status.tm00.bufr_d NOT FOUND in $SOURCE_DIR"
    exit 99
fi

exit 0
