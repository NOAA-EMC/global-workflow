#!/bin/bash
set -x

echo "Entered $0"
MOM6REGRID=${MOM6REGRID:-$HOMEgfs}
export EXEC_DIR=$MOM6REGRID/exec
export USH_DIR=$MOM6REGRID/ush
export COMOUT=$COMOUT
export IDATE=$IDATE
export ENSMEM=$ENSMEM
export FHR=$fhr
export DATA=$DATA
export OCNFIXDIR=$OCNFIXDIR

###### DO NOT MODIFY BELOW UNLESS YOU KNOW WHAT YOU ARE DOING #######
#Need NCL module to be loaded:
echo $NCARG_ROOT
export NCL=$NCARG_ROOT/bin/ncl

ls -alrt

$NCL $USH_DIR/icepost.ncl > regrid_CICE.log 2>&1
$NCL $USH_DIR/ocnpost.ncl > regrid_MOM6.log 2>&1
#####################################################################
