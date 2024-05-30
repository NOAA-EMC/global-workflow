#! /usr/bin/env bash

#
#  UTILITY SCRIPT NAME :  gfsbufr.sh
#               AUTHOR :  Hua-Lu Pan
#         DATE WRITTEN :  02/03/97
#
#  Abstract:  This utility script produces BUFR file of
#             station forecasts from the GFS suite.
#
#     Input:  none
# Script History Log:
# 2016-10-30  H Chuang: Tranistion to read nems output.
#             Change to read flux file fields in gfs_bufr
#             so remove excution of gfs_flux
# 2018-03-22 Guang Ping Lou: Making it works for either 1 hourly or 3 hourly output
# 2018-05-22 Guang Ping Lou: Making it work for both GFS and FV3GFS 
# 2018-05-30  Guang Ping Lou: Make sure all files are available.
# 2019-10-10  Guang Ping Lou: Read in NetCDF files
# 2024-03-03 Bo Cui: Add options to use different bufr table for different resolution NetCDF files
# 2024_05_15 Bo Cui: Add restart capability
# echo "History: February 2003 - First implementation of this utility script"
#
source "${USHgfs}/preamble.sh"

if [[ "${F00FLAG}" == "YES" ]]; then
   f00flag=".true."
else
   f00flag=".false."
fi

export pgm="gfs_bufr.x"
#. prep_step

if [[ "${MAKEBUFR}" == "YES" ]]; then
   bufrflag=".true."
else
   bufrflag=".false."
fi

##fformat="nc"
##fformat="nemsio"

CLASS="class1fv3"
cat << EOF > gfsparm
 &NAMMET
  levs=${LEVS},makebufr=${bufrflag},
  dird="${COM_ATMOS_BUFR}/bufr",
  nstart=${FSTART},nend=${FEND},nint=${FINT},
  nend1=${NEND1},nint1=${NINT1},nint3=${NINT3},
  nsfc=80,f00=${f00flag},fformat=${fformat},np1=0
/
EOF

for (( hr = 10#${FSTART}; hr <= 10#${FEND}; hr = hr + 10#${FINT} )); do
   hh2=$(printf %02i "${hr}")
   hh3=$(printf %03i "${hr}")

   #---------------------------------------------------------
   # Make sure all files are available:
   ic=0
   while (( ic < 1000 )); do
      if [[ ! -f "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${hh3}.${logfm}" ]]; then
          sleep 10
          ic=$((ic + 1))
      else
          break
      fi

      if (( ic >= 360 )); then
         echo "FATAL: COULD NOT LOCATE logf${hh3} file AFTER 1 HOUR"
         exit 2
      fi
   done
   #------------------------------------------------------------------
   ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atmf${hh3}.${atmfm}" "sigf${hh2}"
   ${NLN} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.sfcf${hh3}.${atmfm}" "flxf${hh2}"
done

#  define input BUFR table file.
${NLN} "${PARMgfs}/product/bufr_gfs_${CLASS}.tbl" fort.1
${NLN} "${STNLIST:-${PARMgfs}/product/bufr_stalist.meteo.gfs}" fort.8

case "${CASE}" in
    "C768")
        ${NLN} "${PARMgfs}/product/bufr_ij13km.txt" fort.7
        ;;
    "C1152")
        ${NLN} "${PARMgfs}/product/bufr_ij9km.txt"  fort.7
        ;;
    *)
        echo "FATAL ERROR: Unrecognized bufr_ij*km.txt For CASE ${CASE}, ABORT!"
        exit 1
        ;;
esac


if [[ ${RESTART_postsnd} == "YES" ]]; then

  if [ -f "${DATA_ATMOS_RESTART}/${RUN}.${cycle}.bufr.logf${FEND}.${logfm}" ]; then

    echo "Copy job postsnd files from restart directory"

    cp -p "${DATA_ATMOS_RESTART}/${RUN}.${cycle}.bufr.logf${FEND}.${logfm}" .
    while IFS= read -r fortname; do
#     echo "Copy job postsnd files from restart directory: $fortname"
      cp -p "${DATA_ATMOS_RESTART}/${RUN}.${cycle}.bufr_${fortname}" ${fortname}
    done < "${RUN}.${cycle}.bufr.logf${FEND}.${logfm}"
    err=0

    if [[ ${FEND} -eq ${ENDHOUR} ]]; then
      ${APRUN_POSTSND} "${EXECgfs}/${pgm}" < gfsparm > "out_gfs_bufr_${FEND}"
      export err=$?
    fi

  else

    echo "No more job postsnd restart file found in '${DATA_ATMOS_RESTART}'"
    export RESTART_postsnd="NO"
    echo "set RESTART_postsnd='${RESTART_postsnd}'"
    ${APRUN_POSTSND} "${EXECgfs}/${pgm}" < gfsparm > "out_gfs_bufr_${FEND}"
    export err=$?
  fi

else

  ${APRUN_POSTSND} "${EXECgfs}/${pgm}" < gfsparm > "out_gfs_bufr_${FEND}"
  export err=$?
fi

if [[ $err -ne 0 ]]; then
   echo "GFS postsnd job error, Please check files "
   echo "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atmf${hh2}.${atmfm}"
   echo "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.sfcf${hh2}.${atmfm}"
   err_chk

else

  # Count the number of restart files
  nrestarts=$(find ./ -maxdepth 1 -type f -name 'fort.*' | wc -l)
  find_exit_code=$?
  if [ $find_exit_code -ne 0 ]; then
    # handle the error, set the number of restart file is 0
    nrestarts=0
  else
    echo "Number of restart fort.* files found: ${nrestarts}"
  fi

  # Check if there are restart files
  if [[ "${nrestarts}" -gt 0 ]]; then
    echo "Copying GFS postsnd files to restart directory..."

    # Exclude specific files and save the rest to a log file
    #ls fort.* | grep -v -e 'fort\.1' -e 'fort\.7' -e 'fort\.8' > "${RUN}.${cycle}.bufr.logf${FEND}.${logfm}"
    
    # Initialize an empty array to store fort file names
    files=()

    # Loop through files in the directory
    for file in fort.*; do
      # Check if the file is not fort.1 or fort.7 or fort.8
      if [[ ${file} != "fort.1" && $file != "fort.7" && $file != "fort.8" ]]; then
        files+=("${file}")
      fi
    done

    # Write the list of fort files to the log file
    printf "%s\n" "${files[@]}" > "${RUN}.${cycle}.bufr.logf${FEND}.${logfm}"

    # Copy each restart file to the restart directory
    while IFS= read -r fortname; do
    # echo "Copying restart file: $fortname"
      cp -p "${fortname}" "${DATA_ATMOS_RESTART}/${RUN}.${cycle}.bufr_${fortname}"
    done < "${RUN}.${cycle}.bufr.logf${FEND}.${logfm}"
  fi

    # Copy the log file to the restart directory
    cp -p "${RUN}.${cycle}.bufr.logf${FEND}.${logfm}" "${DATA_ATMOS_RESTART}/"

fi

exit ${err}
