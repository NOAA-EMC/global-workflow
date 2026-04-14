#!/bin/bash
set -x

################################################################################
# UNIX Script Documentation Block
# Name:     exgdas_atmos_analysis_wdqms.sh
# Author:   Rahul Mahajan
# Org:      NCEP/EMC
# Abstract: This script unpacks GSI diagnostic files, runs them through a python
#           script ush/wdqms.py, and creates CSV files.
#           These CSV files contain observation information, residual statistics,
#           and data use in the assimilation for certain observation sub-types
# History Log:
#   2024-04-01  Rahul Mahajan  Initial Version.
# Usage:
# Input Files:
#   ${RUN}.t${cyc}z.cnvstat
# Output Files:
#   ${RUN}.t${cyc}z.${otype}.csv
#   where: otype = synop | temp | marine
# Condition codes:
#   == 0 : success
#   != 0 : non-fatal error encountered while generating output file
################################################################################

# Input GSI diagnostic file containing inputs to wdqms.py
CNVSTAT="${RUN}.t${cyc}z.cnvstat"

# Input files from CNVSTAT fed to wdqms.py
INPUT_LIST=("diag_conv_ps_ges.${PDY}${cyc}.nc4" \
            "diag_conv_t_ges.${PDY}${cyc}.nc4" \
            "diag_conv_q_ges.${PDY}${cyc}.nc4" \
            "diag_conv_uv_ges.${PDY}${cyc}.nc4")

# Observation types being processed by wdqms.py
OTYPES=(SYNOP TEMP MARINE)

################################################################################
echo "Begin job ${job:-}"

#-------------------------------------------------------------------------------
# Enter working directory
cd "${DATA}" || ( echo "FATAL ERROR: Unable to cd into '${DATA}', ABORT!"; exit 2 )

#-------------------------------------------------------------------------------
# Copy cnvstat file from COMIN to DATA, untar and gunzip input files for wdqms.py
# These should always be available
cp "${COMIN}/${CNVSTAT}" .
export err=$?
(( err != 0 )) && ( msg="FATAL ERROR: Unable to copy '${CNVSTAT}' from '${COMIN}', ABORT!"; err_exit "${msg}" )
for diagfile in "${INPUT_LIST[@]}"; do
  tar -xvf "${CNVSTAT}" "${diagfile}.gz"
  export err=$?
  (( err != 0 )) && ( msg="FATAL ERROR: Unable to extract '${diagfile}.gz' from '${CNVSTAT}', ABORT!"; err_exit "${msg}" )
  gunzip "${diagfile}.gz"
  export err=$?
  (( err != 0 )) && ( msg="FATAL ERROR: Unable to gunzip '${diagfile}.gz', ABORT!"; err_exit "${msg}" )
done

#-------------------------------------------------------------------------------
# Loop over observation types, produce CSV files
# Copy CSV files to COMOUT
# Issue DBN alerts
# Issue warnings if wdqms.py fails for any reason
# These do not need to be a FATAL ERROR, but developers should be notified
error=0
for otype in "${OTYPES[@]}"; do

  echo "Processing ... ${otype}"

  #=============================================================================
  # Process with wdqms.py
  ${WDQMSPY} -i ${INPUT_LIST[@]} -t "${otype}" -o "${DATA}"
  rc=$?
  if (( rc != 0 )); then
    echo "WARNING: wdqms.py failed to process observation type '${otype}'"
    error=$((error + 1))
  fi
  #=============================================================================

  #=============================================================================
  # Copy to COMOUT if wdqms.py created the output file
  csvfile="NCEP_${otype}_${PDY}_${cyc}.csv"
  csvfileout="${RUN}.t${cyc}z.${otype,,}.csv"
  if [[ -f "${csvfile}" ]]; then
    cp "./${csvfile}" "${COMOUT}/${csvfileout}" || ( echo "WARNING: Unable to copy '${csvfile}' to '${COMOUT}/${csvfileout}'" )
  else
    echo "WARNING: wdqms.py failed to create csvfile '${csvfile}'"
    error=$((error + 1))
  fi
  #=============================================================================

  #=============================================================================
  # Send DBN alerts
  if [[ "${SENDDBN:-}" == "YES" ]]; then
    if [[ -f "${COMOUT}/${csvfileout}" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_WDQMS" "${job}" "${COMOUT}/${csvfileout}"
    fi
  fi
  #=============================================================================

done  # for otype
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
if (( error == 0 )); then
  echo "Job completed normally."
else
  echo "WARNING: Job completed with non-fatal errors."
fi
################################################################################

exit 0
