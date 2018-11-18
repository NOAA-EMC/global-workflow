#!/bin/ksh
set -x

CDATE=${1:-""}
CDUMP=${2:-""}
SOURCE_DIR=${3:-$DMPDIR/$CDATE/$CDUMP}
TARGET_DIR=${4:-$ROTDIR/${CDUMP}.${PDY}/$cyc}


# Exit if SORUCE_DIR does not exist
if [ ! -s $SOURCE_DIR ]; then 
   echo "***ERROR*** DUMP SOURCE_DIR=$SOURCE_DIR does not exist"
   exit 99
fi
   

# Create TARGET_DIR if is does not exist
if [ ! -s $TARGET_DIR ]; then mkdir -p $TARGET_DIR ;fi


# Set file prefix
cyc=`echo $CDATE |cut -c 9-10`
prefix="$CDUMP.t${cyc}z."


# Link dump files from SOURCE_DIR to TARGET_DIR
cd $SOURCE_DIR
if [ -s ${prefix}updated.status.tm00.bufr_d ]; then
    for file in `ls ${prefix}*`; do
	ln -fs $SOURCE_DIR/$file $TARGET_DIR/$file
    done
else
    echo "***ERROR*** ${prefix}updated.status.tm00.bufr_d NOT FOUND in $SOURCE_DIR"
    exit 99
fi

exit 0



