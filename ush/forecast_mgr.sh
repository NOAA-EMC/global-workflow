#! /usr/bin/env bash

################################################################################
#
# UNIX Script Documentation Block
# Script name:         forecast_mgr.sh
# Script description:  Forecast manager utility functions
#
# Abstract: Provides the fcst_mgr_wait_and_copy function used by
#           JGLOBAL_FORECAST_MGR. For each active model component, reads a
#           product table listing local output files and their corresponding
#           COM destinations. Monitors for per-file sentinel log files and
#           copies data then log to COM in the correct order (data first, log
#           last), preserving the sentinel contract that downstream jobs rely on.
#
#           Each row in the product table has four space-separated fields:
#             local_data_file  local_log_file  com_dest_file  com_dest_log
#
#           Multiple data files that share the same sentinel log are handled
#           automatically: all data files for a given sentinel are copied before
#           the sentinel log is written to COM.
#
################################################################################

fcst_mgr_wait_and_copy() {
    # Wait for each product's sentinel log to appear, then copy all data files
    # sharing that sentinel to COM, and finally copy the log (sentinel last).
    #
    # Parameters
    # ----------
    # $1  Path to the product table file (4-column space-separated)
    # $2  Component name for logging, e.g. "atm" or "ww3"

    local table_file="$1"
    local component="$2"

    echo "SUB ${FUNCNAME[0]}: Starting manager for component '${component}'"
    echo "SUB ${FUNCNAME[0]}: Reading product table from '${table_file}'"

    if [[ ! -f "${table_file}" ]]; then
        echo "FATAL ERROR: Product table '${table_file}' not found for component '${component}'"
        exit 1
    fi

    # Load table into indexed arrays
    local -a local_data local_log com_data com_log done_flag
    local count=0
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
        echo "WARN: Product table '${table_file}' contains no entries; nothing to do"
        return 0
    fi
    echo "SUB ${FUNCNAME[0]}: Loaded ${count} product entries for '${component}'"

    local remaining=${count}
    local start_time
    start_time=$(date +%s)

    while [[ ${remaining} -gt 0 ]]; do
        local i
        for (( i = 0; i < count; i++ )); do
            [[ "${done_flag[i]}" == "YES" ]] && continue
            [[ ! -f "${local_log[i]}" ]] && continue

            # Sentinel exists; process all rows that share this sentinel
            local this_ll="${local_log[i]}"
            local this_cl="${com_log[i]}"
            local j

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
                local com_dir
                com_dir=$(dirname "${com_data[j]}")
                if [[ ! -d "${com_dir}" ]]; then
                    mkdir -p "${com_dir}"
                fi
                cpfs "${local_data[j]}" "${com_data[j]}"
                local copy_err=$?
                if [[ ${copy_err} -ne 0 ]]; then
                    echo "FATAL ERROR: cpfs '${local_data[j]}' -> '${com_data[j]}' failed (err=${copy_err})"
                    exit "${copy_err}"
                fi
            done

            # Copy sentinel log last
            local cl_dir
            cl_dir=$(dirname "${this_cl}")
            if [[ ! -d "${cl_dir}" ]]; then
                mkdir -p "${cl_dir}"
            fi
            cpfs "${this_ll}" "${this_cl}"
            local log_err=$?
            if [[ ${log_err} -ne 0 ]]; then
                echo "FATAL ERROR: cpfs sentinel '${this_ll}' -> '${this_cl}' failed (err=${log_err})"
                exit "${log_err}"
            fi
            echo "INFO: Copied '${component}' sentinel $(basename "${this_cl}") and its data files to COM"

            # Mark all rows for this sentinel as done
            for (( j = 0; j < count; j++ )); do
                if [[ "${done_flag[j]}" == "NO" && "${local_log[j]}" == "${this_ll}" ]]; then
                    done_flag[j]="YES"
                    (( remaining-- )) || true
                fi
            done
        done

        [[ ${remaining} -eq 0 ]] && break

        # Timeout check
        local elapsed
        elapsed=$(( $(date +%s) - start_time ))
        if [[ ${FCST_MGR_TIMEOUT:-0} -gt 0 && ${elapsed} -gt ${FCST_MGR_TIMEOUT} ]]; then
            echo "FATAL ERROR: Manager for '${component}' timed out after ${elapsed}s with ${remaining} sentinels still pending"
            exit 1
        fi

        sleep "${FCST_MGR_SLEEP:-30}"
    done

    echo "SUB ${FUNCNAME[0]}: All ${count} product entries for '${component}' processed"
}
