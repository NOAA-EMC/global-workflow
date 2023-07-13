#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

MOM6REGRID="${MOM6REGRID:-${HOMEgfs}}"
export EXEC_DIR="${MOM6REGRID}/exec"
export USH_DIR="${MOM6REGRID}/ush"
export COMOUTocean="${COM_OCEAN_HISTORY}"
export COMOUTice="${COM_ICE_HISTORY}"
export IDATE="${IDATE}"
export VDATE="${VDATE}"
export ENSMEM="${ENSMEM}"
export FHR="${fhr}"
export DATA="${DATA}"
export FIXreg2grb2="${FIXreg2grb2}"
export vyyyy=`echo ${VDATE} | cut -c1-4`
export vmm=`echo ${VDATE} | cut -c5-6`
export vdd=`echo ${VDATE} | cut -c7-8`

ls -alrt

python ${USH_DIR}/ocnpost.py ${COMOUTocean}/ocn${VDATE}.${ENSMEM}.${IDATE}.nc ${DATA}/ocnr${VDATE}.${ENSMEM}.${IDATE}_0p25x0p25_MOM6.nc ${FIXreg2grb2} ${PARMgfs}/product/ocnpost.parm  "0p25"
python ${USH_DIR}/ocnpost.py ${COMOUTice}/ice${VDATE}.${ENSMEM}.${IDATE}.nc ${DATA}/icer${VDATE}.${ENSMEM}.${IDATE}_0p25x0p25_CICE.nc ${FIXreg2grb2} ${PARMgfs}/product/icepost.parm  "0p25"

#####################################################################
