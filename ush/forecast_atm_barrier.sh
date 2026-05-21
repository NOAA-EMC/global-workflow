#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         forecast_atm_barrier.sh
# Script description:  ATM barrier for per-product parallel copy ranks
#
# Abstract: Called by JGLOBAL_FORECAST_MGR in MPMD mode alongside the four
#           per-product ATM manager ranks (atm_atmf, atm_sfcf, atm_grib,
#           atm_flux). Reads a barrier table where each row has the format:
#
#             final_com_log  dep1  dep2  [dep3  dep4 ...]
#
#           Polls until every per-product dep log (dep1..depN) exists in COM,
#           then writes the final_com_log so that downstream jobs see the
#           standard sentinel contract: all ATM products for that forecast
#           hour are fully in COM when final_com_log appears.
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
while [[ "${remaining}" -gt 0 ]]; do
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
            ((remaining--)) || true
        else
            new_pending+=("${idx}")
        fi
    done

    pending_idx=("${new_pending[@]}")

    if [[ "${remaining}" -gt 0 ]]; then
        # If the model is finished, drain remaining rows with a warning rather
        # than blocking the job from completing.
        if [[ -f "${FCST_DONE_SENTINEL}" ]]; then
            echo "WARN [atm_barrier]: Model done but ${remaining} row(s) still pending; writing WARN sentinels"
            for idx in "${pending_idx[@]}"; do
                final_log="${final_logs[${idx}]}"
                echo "WARN: ATM barrier timed out at $(date --utc +%Y%m%d%H%M%S)" > "${final_log}"
            done
            break
        fi
        sleep "${FCST_POLL_INTERVAL}"
    fi
done

echo "INFO [atm_barrier]: Barrier complete (${total} forecast hour(s) processed)"
