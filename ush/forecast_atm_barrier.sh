#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         forecast_atm_barrier.sh
# Script description:  ATM barrier for per-product parallel copy ranks
#
# Abstract: Runs as the fifth MPMD rank alongside the four per-product ATM
#           manager ranks (atm_atmf, atm_sfcf, atm_grib, atm_flux).
#
#           Each of those four ranks copies one class of ATM output files to
#           COM and writes a small per-product sentinel (com_log) in COM
#           when its copy is done.  Downstream jobs cannot wait on four
#           separate sentinels, so this barrier rank provides the single
#           combined sentinel they already expect: gfs.tXXz.log.fHHH.txt.
#
#           The barrier table (written by FV3_postdet in forecast_postdet.sh)
#           has one row per forecast hour:
#
#             final_com_log  com_log_atmf  com_log_sfcf  [com_log_grib  com_log_flux]
#
#           Where:
#             final_com_log  = COMOUT_ATMOS_HISTORY/gfs.tXXz.log.fHHH.txt
#                              (the standard per-hour sentinel downstream jobs poll)
#             com_log_atmf   = COMOUT_ATMOS_HISTORY/gfs.tXXz.log.atm.atmf.fHHH.txt
#                              (written by atm_atmf rank once atmfHHH.nc is in COM)
#             com_log_sfcf   = COMOUT_ATMOS_HISTORY/gfs.tXXz.log.atm.sfcf.fHHH.txt
#                              (written by atm_sfcf rank once sfcfHHH.nc is in COM)
#             com_log_grib   = COMOUT_ATMOS_MASTER/gfs.tXXz.log.atm.grib.fHHH.txt
#                              (written by atm_grib rank; present only if WRITE_DOPOST)
#             com_log_flux   = COMOUT_ATMOS_MASTER/gfs.tXXz.log.atm.flux.fHHH.txt
#                              (written by atm_flux rank; present only if WRITE_DOPOST)
#
#           Once all dep logs in a row are present (meaning all product types
#           for that hour are in COM), this script writes final_com_log and
#           removes the intermediate dep logs (they are internal bookkeeping
#           and not part of the downstream sentinel contract).
#
#           If the model finishes (fcst_done_seg appears) while rows are still
#           pending, a WARN sentinel is written to unblock the job rather than
#           hanging indefinitely.
#
# Usage:    forecast_atm_barrier.sh <barrier_table>
#             barrier_table - absolute path to the barrier table file
################################################################################

barrier_table="${1:?Usage: forecast_atm_barrier.sh <barrier_table>}"

echo "INFO [atm_barrier]: Starting barrier; reading '${barrier_table}'"

if [[ ! -f "${barrier_table}" ]]; then
    echo "FATAL ERROR [atm_barrier]: Barrier table '${barrier_table}' not found" >&2
    exit 1
fi

FCST_POLL_INTERVAL="${FCST_MGR_POLL_INTERVAL:-30}"
FCST_DONE_SENTINEL="${DATAjob}/fcst_done_seg${FCST_SEGMENT:-0}"
# After fcst_done appears, keep polling for this long before forcing WARN sentinels
# if no pending rows are being resolved. Set to 0 to disable forced WARN drain.
FCST_POSTDONE_TIMEOUT="${FCST_MGR_POSTDONE_TIMEOUT:-1800}"

# Track which rows are still pending.
declare -a final_logs all_deps_arr pending_idx
count=0

while read -r line; do
    [[ -z "${line}" || "${line:0:1}" == "#" ]] && continue
    read -r -a fields <<< "${line}"
    final_logs[count]="${fields[0]}"
    # Dependencies are all remaining fields (fields[1], fields[2], ...).
    all_deps_arr[count]="${fields[*]:1}"
    pending_idx[count]="${count}"
    ((count++)) || true
done < "${barrier_table}"

total="${count}"
echo "INFO [atm_barrier]: ${total} forecast hour(s) to confirm"

remaining="${total}"
postdone_elapsed=0
postdone_announced=0
while [[ "${remaining}" -gt 0 ]]; do
    remaining_before="${remaining}"
    new_pending=()

    for idx in "${pending_idx[@]}"; do
        final_log="${final_logs[${idx}]}"
        read -r -a deps <<< "${all_deps_arr[${idx}]}"

        # Check if all per-product dep logs are present.
        all_ready=1
        for dep in "${deps[@]}"; do
            if [[ ! -r "${dep}" ]]; then
                all_ready=0
                break
            fi
        done

        if [[ "${all_ready}" -eq 1 ]]; then
            echo "INFO [atm_barrier]: All deps for '$(basename "${final_log}")' confirmed; writing final sentinel"
            echo "ATM products confirmed in COM at $(date --utc +%Y%m%d%H%M%S)" > "${final_log}"
            # The four per-product dep logs are internal bookkeeping only; remove
            # them now that the combined final_com_log is written so COM stays clean.
            rm -f "${deps[@]}"
            ((remaining--)) || true
        else
            new_pending+=("${idx}")
        fi
    done

    pending_idx=("${new_pending[@]}")

    if [[ "${remaining}" -gt 0 ]]; then
        # If the model is finished, keep polling for a grace period before forcing
        # WARN sentinels. Product-copy ranks can still be actively writing dep
        # sentinels after fcst_done appears.
        if [[ -f "${FCST_DONE_SENTINEL}" ]]; then
            # Reset idle timer whenever at least one row completed this poll cycle.
            if [[ "${remaining}" -lt "${remaining_before}" ]]; then
                postdone_elapsed=0
            else
                ((postdone_elapsed += FCST_POLL_INTERVAL)) || true
            fi

            if [[ "${postdone_announced}" -eq 0 ]]; then
                echo "INFO [atm_barrier]: fcst_done detected with ${remaining} row(s) pending; allowing copy ranks to finish"
                postdone_announced=1
            fi

            if [[ "${FCST_POSTDONE_TIMEOUT}" -gt 0 && "${postdone_elapsed}" -ge "${FCST_POSTDONE_TIMEOUT}" ]]; then
                echo "WARN [atm_barrier]: Model done and no barrier progress for ${postdone_elapsed}s; writing WARN sentinels for ${remaining} pending row(s)"
                for idx in "${pending_idx[@]}"; do
                    final_log="${final_logs[${idx}]}"
                    final_dir=$(dirname "${final_log}")
                    if [[ ! -d "${final_dir}" ]]; then
                        mkdir -p "${final_dir}"
                    fi
                    echo "WARN: ATM barrier timed out at $(date --utc +%Y%m%d%H%M%S)" > "${final_log}"
                    read -r -a warn_deps <<< "${all_deps_arr[${idx}]}"
                    rm -f "${warn_deps[@]}"
                done
                break
            fi
        fi
        sleep "${FCST_POLL_INTERVAL}"
    fi
done

echo "INFO [atm_barrier]: Barrier complete (${total} forecast hour(s) processed)"
