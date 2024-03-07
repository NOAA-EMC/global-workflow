#!/bin/bash
set -x

# Input GSI diagnostic file containing inputs to wdqms.py
CNVSTAT="${CDUMP}.t${cyc}z.cnvstat"

# Input files from CNVSTAT fed to wdqms.py
INPUT_LIST=("diag_conv_ps_ges.${PDY}${cyc}.nc4" \
            "diag_conv_t_ges.${PDY}${cyc}.nc4" \
            "diag_conv_q_ges.${PDY}${cyc}.nc4" \
            "diag_conv_uv_ges.${PDY}${cyc}.nc4")

# Observation types being processed by wdqms.py
OTYPES=(SYNOP TEMP MARINE)

# debugging and verbose options for wdqms.py
[[ ${DEBUG_WDQMS:-} == "YES" ]] && debug="-d"
[[ ${VERBOSE_WDQMS:-} == "YES" ]] && verbose="-v"

################################################################################
echo "Begin job ${job:-}"

#-------------------------------------------------------------------------------
# Enter working directory
cd "${DATA}" || ( echo "FATAL ERROR: Unable to cd ${DATA}, ABORT!"; exit 2 )

#-------------------------------------------------------------------------------
# Copy cnvstat file from COMIN to DATA, untar and gunzip input files for wdqms.py
cp "${COMIN}/${CNVSTAT}" .
rc=$?
(( rc != 0 )) && ( echo "FATAL ERROR: Unable to copy ${diag_file} from ${COMIN}, ABORT!"; exit 2 )
for file in "${INPUT_LIST[@]}"; do
  tar -xzvf "${diag_file}" "${file}.gz"
  rc=$?
  (( rc != 0 )) && ( echo "FATAL ERROR: Unable to extract ${file}.gz from ${diag_file}, ABORT!"; exit 3 )
  gunzip "${file}.gz"
  rc=$?
  (( rc != 0 )) && ( echo "FATAL ERROR: Unable to gunzip ${file}.gz, ABORT!"; exit 3 )
done

#-------------------------------------------------------------------------------
# Loop over observation types and produce csv files
for otype in "${OTYPES[@]}"; do
  echo "Processing ... ${otype}"
  ${WDQMSPY} -i ${INPUT_LIST[@]} -t "${otype}" -o "${DATA}" "${debug:-}" "${verbose:-}"
  rc=$?
  if (( rc != 0 )); then
    echo "FATAL ERROR: wdqms.py failed to process observation type ${otype}; ABORT!"
    exit "${rc}"
  fi
done

#-------------------------------------------------------------------------------
# Copy output from wdqms.py to COMOUT
for otype in "${OTYPES[@]}"; do
  file="NCEP_${otype}_${PDY}_${cyc}.csv"
  cp "./${file}" "${COMOUT}/${file}" || ( echo "FATAL ERROR: Unable to copy ${file} to ${COMOUT}, ABORT!"; exit 2 )
done

#-------------------------------------------------------------------------------
# Send DBN alerts for dataflow to pick up data from COMOUT
for otype in "${OTYPES[@]}"; do
  file="NCEP_${otype}_${PDY}_${cyc}.csv"
  if [[ -f "${COMOUT}/${file}" ]]; then
    echo "Send DBN Alert"  # TODO: Add appropriate DBNALERT call
  else
    echo "WARNING: wdqms.py did not produce '${file}'"  # TODO: Should this be a fatal error?
  fi
done

#-------------------------------------------------------------------------------
echo 'Job completed normally.'
################################################################################

exit 0
