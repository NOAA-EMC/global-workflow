#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         forecast_mgr.sh
# Script description:  Forecast manager for a single model component
#
# Abstract: Called by JGLOBAL_FORECAST_MGR via "srun --multi-prog" (MPMD mode)
#           with one SLURM task per active model component. Reads a product
#           table listing local output files and their COM destinations. Monitors
#           for per-file sentinel log files and copies data then log to COM in
#           the correct order (data first, log last), preserving the sentinel
#           contract that downstream jobs rely on.
#
#           Each row in the product table has four space-separated fields:
#             local_data_file  local_log_file  com_dest_file  com_dest_log
#
#           Multiple data files that share the same sentinel log are handled
#           automatically: all data files for a given sentinel are copied before
#           the sentinel log is written to COM.
#
# Usage:    forecast_mgr.sh <component> <table_file>
#             component  - short name used in log output, e.g. "atm", "ww3",
#                          "ocn", or "ice"
#             table_file - absolute path to the 4-column product table file
################################################################################

component="${1:?Usage: forecast_mgr.sh <component> <table_file>}"
table_file="${2:?Usage: forecast_mgr.sh <component> <table_file>}"

echo "INFO [${component}]: Starting manager"
echo "INFO [${component}]: Reading product table from '${table_file}'"

if [[ ! -f "${table_file}" ]]; then
    echo "FATAL ERROR: Product table '${table_file}' not found for component '${component}'" >&2
    exit 1
fi

# Load table into indexed arrays
declare -a local_data local_log com_data com_log done_flag
count=0
while read -r ld ll cd cl; do
    [[ -z "${ld}" || "${ld:0:1}" == "#" ]] && continue
    local_data[count]="${ld}"
    local_log[count]="${ll}"
    com_data[count]="${cd}"
    com_log[count]="${cl}"
    done_flag[count]="NO"
    (( count++ )) || true
done < "${table_file}"

if [[ ${count} -eq 0 ]]; then
    echo "WARN [${component}]: Product table '${table_file}' contains no entries; nothing to do"
    exit 0
fi
echo "INFO [${component}]: Loaded ${count} product entries"

remaining=${count}
start_time=$(date +%s)
fcst_done_idle=0

while [[ ${remaining} -gt 0 ]]; do
    remaining_before=${remaining}
    for (( i = 0; i < count; i++ )); do
        [[ "${done_flag[i]}" == "YES" ]] && continue
        [[ ! -f "${local_log[i]}" ]] && continue

        # Sentinel exists; process all rows that share this sentinel
        this_ll="${local_log[i]}"
        this_cl="${com_log[i]}"

        # RERUN safety: if com_log already in COM, mark all rows for this sentinel done
        if [[ -f "${this_cl}" ]]; then
            for (( j = 0; j < count; j++ )); do
                if [[ "${done_flag[j]}" == "NO" && "${local_log[j]}" == "${this_ll}" ]]; then
                    done_flag[j]="YES"
                    (( remaining-- )) || true
                fi
            done
            continue
        fi

        # Copy all data files that share this sentinel (data first, log last)
        for ((j = 0; j < count; j++)); do
            [[ "${done_flag[j]}" == "YES" ]] && continue
            [[ "${local_log[j]}" != "${this_ll}" ]] && continue
            com_dir=$(dirname "${com_data[j]}")
            if [[ ! -d "${com_dir}" ]]; then mkdir -p "${com_dir}"; fi
            cpfs "${local_data[j]}" "${com_data[j]}"
            copy_err=$?
            if [[ ${copy_err} -ne 0 ]]; then
                echo "FATAL ERROR [${component}]: cpfs '${local_data[j]}' -> '${com_data[j]}' failed (err=${copy_err})" >&2
                exit "${copy_err}"
            fi
        done

        # Copy sentinel log last.
        # Skip if already in COM (self-sentinel pattern: local_log == local_data means
        # com_log == com_data, which was already copied in the data step above).
        if [[ ! -f "${this_cl}" ]]; then
            cl_dir=$(dirname "${this_cl}")
            if [[ ! -d "${cl_dir}" ]]; then mkdir -p "${cl_dir}"; fi
            cpfs "${this_ll}" "${this_cl}"
            log_err=$?
            if [[ ${log_err} -ne 0 ]]; then
                echo "FATAL ERROR [${component}]: cpfs sentinel '${this_ll}' -> '${this_cl}' failed (err=${log_err})" >&2
                exit "${log_err}"
            fi
        fi
        echo "INFO [${component}]: Copied sentinel $(basename "${this_cl}") and its data files to COM"

        # Mark all rows for this sentinel as done
        for ((j = 0; j < count; j++)); do
            if [[ "${done_flag[j]}" == "NO" && "${local_log[j]}" == "${this_ll}" ]]; then
                done_flag[j]="YES"
                ((remaining--)) || true
            fi
        done
    done

    [[ ${remaining} -eq 0 ]] && break

    # Timeout check (fatal — hard wall enforced by batch scheduler walltime).
    elapsed=$(($(date +%s) - start_time))
    if [[ ${FCST_MGR_TIMEOUT:-0} -gt 0 && ${elapsed} -gt ${FCST_MGR_TIMEOUT} ]]; then
        echo "FATAL ERROR [${component}]: Timed out after ${elapsed}s with ${remaining} sentinels still pending" >&2
        exit 1
    fi

    # Graceful exit: once the model has finished (fcst_done sentinel present), count
    # consecutive poll cycles where no new files were processed. After
    # FCST_MGR_DONE_IDLE_MAX idle cycles (default 3) exit with a warning for any
    # entries the model never produced (e.g. optional GOCART output types).
    if [[ -n "${FCST_DONE_SENTINEL:-}" && -f "${FCST_DONE_SENTINEL}" ]]; then
        if [[ ${remaining} -lt ${remaining_before} ]]; then
            fcst_done_idle=0
        else
            ((fcst_done_idle++)) || true
            idle_max=${FCST_MGR_DONE_IDLE_MAX:-3}
            if [[ ${fcst_done_idle} -ge ${idle_max} ]]; then
                echo "WARN [${component}]: Model run complete; no new files for ${fcst_done_idle} consecutive poll cycle(s). ${remaining} of ${count} table entry(s) were not produced by the model — skipping."
                break
            fi
        fi
    fi

    sleep "${FCST_MGR_SLEEP:-30}"
done

if [[ ${remaining} -eq 0 ]]; then
    echo "INFO [${component}]: All ${count} product entries processed"
else
    echo "INFO [${component}]: Manager exiting: $((count - remaining)) of ${count} entries processed; ${remaining} skipped (not produced by model)"
fi
