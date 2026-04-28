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

# Indexed arrays holding all known product entries across all forecast segments.
declare -a local_data local_log com_data com_log done_flag
count=0
remaining=0
table_line_count=0  # physical lines consumed from the table file (tracks position for incremental loads)

# Load any new entries from the table file that have not been read yet.
# Uses table_line_count to skip already-processed lines so this function is
# safe to call repeatedly as subsequent forecast segments append to the file.
_load_new_entries() {
    local lineno=0 raw_line ld ll cd cl
    while IFS= read -r raw_line; do
        (( lineno++ )) || true
        [[ ${lineno} -le ${table_line_count} ]] && continue
        read -r ld ll cd cl <<< "${raw_line}"
        [[ -z "${ld}" || "${ld:0:1}" == "#" ]] && continue
        local_data[count]="${ld}"
        local_log[count]="${ll}"
        com_data[count]="${cd}"
        com_log[count]="${cl}"
        done_flag[count]="NO"
        (( count++ )) || true
        (( remaining++ )) || true
    done < "${table_file}"
    table_line_count=${lineno}
}

# Initial load
_load_new_entries

if [[ ${count} -eq 0 ]]; then
    echo "WARN [${component}]: Product table '${table_file}' contains no entries; nothing to do"
    exit 0
fi
echo "INFO [${component}]: Loaded ${count} product entries"

# seen_end becomes YES when forecast_postdet.sh writes #END to the table,
# indicating that the last forecast segment has published all its entries.
seen_end="NO"
start_time=$(date +%s)

while true; do
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
        for (( j = 0; j < count; j++ )); do
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
        for (( j = 0; j < count; j++ )); do
            if [[ "${done_flag[j]}" == "NO" && "${local_log[j]}" == "${this_ll}" ]]; then
                done_flag[j]="YES"
                (( remaining-- )) || true
            fi
        done
    done

    # Check whether the last forecast segment has signalled completion
    grep -q '^#END' "${table_file}" 2>/dev/null && seen_end="YES"

    # Exit when all known entries are processed and no more will arrive
    if [[ ${remaining} -eq 0 && "${seen_end}" == "YES" ]]; then
        break
    fi

    # Timeout check
    elapsed=$(( $(date +%s) - start_time ))
    if [[ ${FCST_MGR_TIMEOUT:-0} -gt 0 && ${elapsed} -gt ${FCST_MGR_TIMEOUT} ]]; then
        echo "FATAL ERROR [${component}]: Timed out after ${elapsed}s with ${remaining} entries pending (end_marker=${seen_end})" >&2
        exit 1
    fi

    sleep "${FCST_MGR_SLEEP:-30}"

    # Reload table to pick up entries appended by subsequent forecast segments
    prev_count=${count}
    _load_new_entries
    if [[ ${count} -gt ${prev_count} ]]; then
        echo "INFO [${component}]: Loaded $((count - prev_count)) new entries from next forecast segment (total=${count})"
    fi
done

echo "INFO [${component}]: All ${count} product entries processed"
