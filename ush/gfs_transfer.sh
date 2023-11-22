#! /usr/bin/env bash

#####################################################################
# echo "-----------------------------------------------------"
# echo " Script: gfs_transfer.sh"
# echo " "
# echo " Purpose - Copy GFS Posts to /nwges and /com"
# echo "           Alert posted files to DBNet"
# echo " "
# echo " History - "
# echo "    Cooke   - 04/21/05 - Inital version, based off of"
# echo "                         global_transfer.sh"
# echo "    Meng    - 01/04/18 - Remove writing data file to /nwges."
# echo "    Meng    - 09/14/20 - Update model output format to netcdf for GFS V16"
# echo "-----------------------------------------------------"
#####################################################################

source "${HOMEgfs}/ush/preamble.sh"

if [[ "${SENDDBN}" = 'YES' && "${RUN}" = 'gfs' ]]; then
  fhr3=$(printf "%03d" "${fhr}")
  "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_SF "${job}" "${COMOUT}/${RUN}.t${cyc}z.atmf${fhr3}.nc"

  if (( fhr > 0 && fhr <= 84 )); then
     "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_BF "${job}" "${COMOUT}/${RUN}.t${cyc}z.sfcf${fhr3}.nc"
  fi
  if (( fhr == 120 )); then
     "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_BF "${job}" "${COMOUT}/${RUN}.t${cyc}z.sfcf${fhr3}.nc"
  fi
fi

exit 0
