#! /usr/bin/env bash

###############################################################################
#                                                                             #
# This script tars the sectral or bulletin files into a single file and       #
# puts it into /com. This is a separate script to enable it to be run in      #
# parallel using poe. It also tars the spectral and bulletin files of the     #
# old grids that are generated for backward compatibility                     #
#                                                                             #
# Remarks :                                                                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory TAR_$type_$ID which is  #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
# Origination: Hendrik Tolman                                March 13, 2007   #
# Update log                                                                  #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
# 2020-06-10  J-Henrique Alves Ported R&D machine Hera   
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

source "${USHgfs}/preamble.sh"

# 0.a Basic modes of operation

  cd "${DATA}"
  echo "Making TAR FILE"

  alertName=$(echo $RUN|tr [a-z] [A-Z])

  set +x
  echo ' '
  echo '+--------------------------------+'
  echo '!         Make tar file          |'
  echo '+--------------------------------+'
  echo "   ID              : $1"
  echo "   Type            : $2"
  echo "   Number of files : $3"
  set_trace


# 0.b Check if type set

  if [[ "$#" -lt '3' ]]
  then
    set +x
    echo ' '
    echo '********************************************'
    echo '*** VARIABLES IN ww3_tar.sh NOT SET ***'
    echo '********************************************'
    echo ' '
    set_trace
    exit 1
  else
    ID=$1
    type=$2
    nb=$3
  fi

  filext=$type
  if [[ "$type" = "ibp" ]]; then filext='spec'; fi
  if [[ "$type" = "ibpbull" ]]; then filext='bull'; fi
  if [[ "$type" = "ibpcbull" ]]; then filext='cbull'; fi


  rm -rf TAR_${filext}_$ID 
  mkdir  TAR_${filext}_$ID
# this directory is used only for error capturing

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

  if [[ -z "${cycle}" ]] || [[ -z "${COMOUT_WAVE_STATION}" ]] || [[ -z "${WAV_MOD_TAG}" ]] ||  \
     [[ -z "${SENDDBN}" ]] || [[ -z "${STA_DIR}" ]]; then
    set +x
    echo ' '
    echo '*****************************************************'
    echo '*** EXPORTED VARIABLES IN ww3_tar.sh NOT SET ***'
    echo '*****************************************************'
    echo ' '
    set_trace
    exit 2
  fi

  cd "${STA_DIR}/${filext}"

# --------------------------------------------------------------------------- #
# 2.  Generate tar file (spectral files are compressed)

  set +x
  echo ' '
  echo '   Making tar file ...'
  set_trace

  countMAX=5
  tardone='no'
  sleep_interval=10
  
  while [[ "${tardone}" = "no" ]]
  do
    
    nf=$(ls | awk '/'$ID.*.$filext'/ {a++} END {print a}')
    nbm2=$(( $nb - 2 ))
    if [[ "${nf}" -ge "${nbm2}" ]]
    then

      tar -cf "${ID}.${cycle}.${type}_tar" ./${ID}.*.${filext}
      exit=$?
      filename="${ID}.${cycle}.${type}_tar" 
      if ! wait_for_file "${filename}" "${sleep_interval}" "${countMAX}" ; then
        echo "FATAL ERROR: File ${filename} not found after waiting $(( sleep_interval * (countMAX + 1) )) secs"
        exit 3
      fi

      if  [[ "${exit}" != '0' ]]
      then
        set +x
        echo ' '
        echo '***************************************** '
        echo '*** FATAL ERROR : TAR CREATION FAILED *** '
        echo '***************************************** '
        echo ' '
        set_trace
        exit 3
      fi
      
      if [[ -f "${ID}.${cycle}.${type}_tar" ]]
      then
        tardone='yes'
      fi
    fi

  done

  if [[ "${tardone}" = 'no' ]]
  then
    set +x
    echo ' '
    echo '***************************************** '
    echo '*** FATAL ERROR : TAR CREATION FAILED *** '
    echo '***************************************** '
    echo ' '
    set_trace
    exit 3
  fi

  if [[ "${type}" = 'spec' ]]
  then
    if [[ -s "${ID}.${cycle}.${type}_tar" ]]
    then
      file_name="${ID}.${cycle}.${type}_tar.gz"
      /usr/bin/gzip -c "${ID}.${cycle}.${type}_tar" > "${file_name}"
      exit=$?

      if  [[ "${exit}" != '0' ]]
      then
        set +x
        echo ' '
        echo '***************************************************** '
        echo '*** FATAL ERROR : SPECTRAL TAR COMPRESSION FAILED *** '
        echo '***************************************************** '
        echo ' '
        set_trace
        exit 4
      fi
    fi
  else
    file_name="${ID}.${cycle}.${type}_tar"
  fi

# --------------------------------------------------------------------------- #
# 3.  Move data to /com

  set +x
  echo ' '
  echo "   Moving tar file ${file_name} to ${COMOUT_WAVE_STATION} ..."
  set_trace

  cp "${file_name}" "${COMOUT_WAVE_STATION}/."

  exit=$?

  if  [[ "${exit}" != '0' ]]
  then
    set +x
    echo ' '
    echo '************************************* '
    echo '*** FATAL ERROR : TAR COPY FAILED *** '
    echo '************************************* '
    echo ' '
    set_trace
    exit 4
  fi

  if [[ "${SENDDBN}" = 'YES' ]]
  then
    set +x
    echo ' '
    echo "   Alerting TAR file as ${COMOUT_WAVE_STATION}/${file_name}"
    echo ' '
    set_trace
    "${DBNROOT}/bin/dbn_alert MODEL" "${alertName}_WAVE_TAR" "${job}" \
      "${COMOUT_WAVE_STATION}/${file_name}"
  fi

# --------------------------------------------------------------------------- #
# 4.  Final clean up

cd "${DATA}"

if [[ ${KEEPDATA:-NO} == "NO" ]]; then
  set -v
  rm -rf  ${STA_DIR}/${type}
  set +v
fi

# End of ww3_tar.sh ----------------------------------------------------- #
