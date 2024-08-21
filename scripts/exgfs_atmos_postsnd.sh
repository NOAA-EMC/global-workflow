#! /usr/bin/env bash

################################################################
# Script Name:		exgfs_atmos_postsnd.sh.sms
# Script Description:	Generate GFS BUFR sounding files
# Script History Log:
#   1) 2003-03-25       Hualu Pan       First Implementation
#   2) 2010-05-25       V. Krishna Kumar Modified for the GFS 
#                                  resolution upgrade
#   3) 2014-08-01       D. Carlis Updated to vertical structure 
#                                 and T1534 Resolution 
#   4) 2016-11-01       H. Chuang Update to read new model nems output
#   5) 2017-02-21       Guang Ping Lou setup mpmd to speedup the run
#                                 and 1 & 3 hourly output
#   6) 2018-03-22       Guang Ping Lou  Take FV3GFS configuration
#                          parameters as input; make it work for 
#                          both FV3GFS and GFS
#   7) 2018-07-18       Guang Ping Lou Generalize this version to other platforms
#   8) 2019-10-18       Guang Ping Lou Transition to reading in NetCDF model data
#   9) 2019-12-18       Guang Ping Lou generalizing to reading in NetCDF or nemsio
#  10) 2024-08-08       Bo Cui Update to handle one forecast at a time
#                          For GFSv17 bufr, total number of forecast hours is 141(num_hours=141) 
#                          it requires 7 nodes & allocate 21 processes per node(num_ppn=21)
################################################################

source "${USHgfs}/preamble.sh"

runscript=${USHgfs}/gfs_bufr.sh 

cd $DATA

########################################

###################################################
## Run meteogram generator for T574
###################################################
export LEVS=${LEVS:-127}
export STARTHOUR=${STARTHOUR:-00}
export ENDHOUR=${ENDHOUR:-180}
export INCREMENT=12
export MAKEBUFR=NO
export F00FLAG=YES
export fformat=netcdf
export atmfm="nc"
export logfm="txt"
export NINT1=${FHOUT_HF_GFS:-1}
export NEND1=${FHMAX_HF_GFS:-120}
export NINT3=${FHOUT_GFS:-3}

rm -f -r "${COM_ATMOS_BUFR}"
mkdir -p "${COM_ATMOS_BUFR}"
export COM_ATMOS_BUFR="${COM_ATMOS_BUFR}"

GETDIM="${USHgfs}/getncdimlen"
export LEVS=$(${GETDIM} "${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atmf000.${atmfm}" pfull)
declare -x LEVS

### wait for the sigma and surface flux file:
sleep_interval=10
max_tries=360
#

# Initialize an empty list to store the hours
hour_list=()

# Generate hours from 0 to NEND1 with interval NINT1
# Convert ENDHOUR to decimal through $((10#$ENDHOUR)) to avoid it is thought as octal number
for (( hour=0; hour<=$((10#$NEND1)) && hour<=$((10#$ENDHOUR)); hour+=$((10#$NINT1)) )); do
  hour_list+=("$(printf "%03d" $hour)")
done

# Generate hours from NEND1 + NINT3 to ENDHOUR with interval NINT3
for (( hour=$((10#$NEND1))+$((10#$NINT3)); hour<=$((10#$ENDHOUR)); hour+=$((10#$NINT3)) )); do
  hour_list+=("$(printf "%03d" $hour)")
done

# Print the hour list
echo "Hour List:" "${hour_list[@]}"

# Count the number of elements in the hour_list
num_hours="${#hour_list[@]}"

# Print the total number of hours
echo "Total number of hours: $num_hours"

# allocate 21 processes per node
# don't allocate more processes, or it might have memory issue
num_ppn=21
export APRUN="mpiexec -np ${num_hours} -ppn ${num_ppn} --cpu-bind core cfp "

if [ -s "${DATA}/poescript_bufr" ]; then
  rm ${DATA}/poescript_bufr
fi

for fhr in "${hour_list[@]}"; do

  if [ ! -s "${DATA}/${fhr}" ]; then mkdir -p ${DATA}/${fhr}; fi
  export fhr=${fhr}
  export FINT=${NINT1}
  ## 1-hourly output before $NEND1, 3-hourly output after
  if [[ $((10#${fhr})) -gt $((10#${NEND1})) ]]; then
    export FINT=${NINT3}
  fi
  if [[ ${fhr} -eq 000 ]]; then 
     export F00FLAG="YES"
  else
     export F00FLAG="NO"
  fi

  # Convert fhr to integer
  fhr_int=$((10#$fhr))

  # Get previous hour
  if (( fhr_int == STARTHOUR )); then
    fhr_p=${fhr_int}
  else
    fhr_p=$(( fhr_int - FINT ))
  fi

  # Format fhr_p with leading zeros
  fhr_p="$(printf "%03d" $fhr_p)" 

  filename="${COM_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${fhr}.${logfm}"
  if ! wait_for_file "${filename}" "${sleep_interval}" "${max_tries}"; then
    echo "Waiting for the file ${filename} for $((sleep_interval * (max_tries - 1))) seconds..."
    err_exit "FATAL ERROR: logf${fhr} not found after waiting $((sleep_interval * (max_tries - 1))) secs"
  fi
  echo "${runscript} \"${fhr}\" \"${fhr_p}\" \"${FINT}\" \"${F00FLAG}\" \"${DATA}/${fhr}\"" >> "${DATA}/poescript_bufr"
done

chmod +x "${DATA}/poescript_bufr"
startmsg
$APRUN "${DATA}/poescript_bufr"
export err=$?; err_chk

cd "${DATA}"

# Initialize fortnum
fortnum=20

# Loop through each element in the array
for fhr in "${hour_list[@]}"; do
    # Increment fortnum
    fortnum=$((fortnum + 1))
    ${NLN} "${DATA}/${fhr}/fort.${fortnum}" "fort.${fortnum}"
done

export MAKEBUFR=YES
export fhr=${ENDHOUR}
export FINT=${NINT1}
## 1-hourly output before $NEND1, 3-hourly output after
if [[ $((10#${fhr})) -gt $((10#${NEND1})) ]]; then
  export FINT=${NINT3}
fi
if [[ ${fhr} -eq 000 ]]; then 
  export F00FLAG="YES"
else
  export F00FLAG="NO"
fi
${runscript} "${fhr}" "${fhr_p}" "${FINT}" "${F00FLAG}" "${DATA}" 

##############################################################
# Tar and gzip the individual bufr files and send them to /com
##############################################################
cd "${COM_ATMOS_BUFR}" || exit 2
tar -cf - . | /usr/bin/gzip > "${RUN}.${cycle}.bufrsnd.tar.gz"
cd "${DATA}" || exit 2

########################################
# Send the single tar file to OSO
########################################
if [[ "${SENDDBN}" == 'YES' ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL GFS_BUFRSND_TAR "${job}" \
        "${COM_ATMOS_BUFR}/${RUN}.${cycle}.bufrsnd.tar.gz"
fi

########################################
# Create Regional Collectives of BUFR data and 
# add appropriate WMO Headers.
########################################
rm -rf poe_col
for (( m = 1; m <= $((10#$NUM_SND_COLLECTIVES)); m++ )); do
    echo "sh ${USHgfs}/gfs_sndp.sh ${m} " >> poe_col
done

if [[ ${CFP_MP:-"NO"} == "YES" ]]; then
    nl -n ln -v 0 poe_col > cmdfile
else
    mv poe_col cmdfile
fi

cat cmdfile
chmod +x cmdfile

${APRUN_POSTSNDCFP} cmdfile

sh "${USHgfs}/gfs_bfr2gpk.sh"



############## END OF SCRIPT #######################
