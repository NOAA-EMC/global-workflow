#!/bin/bash
set -euax

#####################################################################
# User specific parameters

echo "Entered $0"

export EXEC_DIR=$MOM6REGRID/exec
export FIX_DIR=$MOM6REGRID/fix
export COMOUT=$COMOUT
export IDATE=$IDATE
export ENSMEM=$ENSMEM
export FHR=$fhr
export DATA=$DATA
export OCNFIXDIR=$OCNFIXDIR

#export IDATE=2015040100     # From caller/env
#export ENSMEM=01            # From caller/env
#export NCARG_ROOT=$ROOT_DIR/ncl  # From module
#export FHR=$fhr

###### DO NOT MODIFY BELOW UNLESS YOU KNOW WHAT YOU ARE DOING #######
module load ncl/6.5.0
echo $NCARG_ROOT
export NCL=$NCARG_ROOT/bin/ncl

####
ls -alrt

# executed from DATA dir
#$NCL $EXEC_DIR/regrid_MOM6.ncl > regrid_MOM6.log 2>&1
#$NCL $EXEC_DIR/regrid_CICE.ncl > regrid_CICE.log 2>&1
$NCL $FIX_DIR/icepost.ncl > regrid_CICE.log 2>&1
$NCL $FIX_DIR/ocnpost.ncl > regrid_MOM6.log 2>&1
#####################################################################
