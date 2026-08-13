#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         forecast_manager.sh
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
# Usage:    forecast_manager.sh <component> <table_file>
#             component  - short name used in log output, e.g. "atm", "ww3",
#                          "ocn", or "ice"
#             table_file - absolute path to the 4-column product table file
################################################################################

# Suppress xtrace that may be inherited from the parent J-job via SHELLOPTS.
# The inner copy loops iterate hundreds of entries; xtrace would generate
# millions of log lines and significantly slow filesystem I/O.
component="${1:?Usage: forecast_manager.sh <component> <table_file>}"
table_file="${2:?Usage: forecast_manager.sh <component> <table_file>}"

echo "INFO [${component}]: Starting manager"
echo "INFO [${component}]: Reading product table from '${table_file}'"

if [[ ! -f "${table_file}" ]]; then
    echo "FATAL ERROR: Product table '${table_file}' not found for component '${component}'" >&2
    exit 1
fi

# Load table into indexed arrays
declare -a local_data local_log com_data com_log done_flag
count=0
while read -r src_data src_log dst_data dst_log; do
    if [[ -z "${src_data}" || "${src_data:0:1}" == "#" ]]; then
        continue
    fi
    local_data[count]="${src_data}"
    local_log[count]="${src_log}"
    com_data[count]="${dst_data}"
    com_log[count]="${dst_log}"
    done_flag[count]="NO"
    ((count++)) || true
done < "${table_file}"

if [[ ${count} -eq 0 ]]; then
    echo "WARNING: [${component}] Product table '${table_file}' contains no entries; nothing to do"
    exit 0
fi
echo "INFO [${component}]: Loaded ${count} product entries"

# Wait for fcst_table_ready (tables complete, model about to start). If the sentinel is
# absent the forecast is mid-rewind; stall until postdet re-writes it.
# Skip entirely when the sentinel path is not configured (backward compat).
if [[ -n "${FCST_TABLE_READY_SENTINEL:-}" && ! -f "${FCST_TABLE_READY_SENTINEL}" ]]; then
    _mgr_wait_max="${FCST_MANAGER_INIT_TIMEOUT:-7200}"
    _mgr_waited=0
    until [[ -f "${FCST_TABLE_READY_SENTINEL}" ]]; do
        sleep 5
        _mgr_waited=$((_mgr_waited + 5))
        if [[ ${_mgr_waited} -ge ${_mgr_wait_max} ]]; then
            echo "FATAL ERROR [${component}]: timed out after ${_mgr_wait_max}s waiting for fcst_table_ready" >&2
            exit 1
        fi
    done
    echo "INFO [${component}]: Sentinel check passed after ${_mgr_waited}s"
fi

remaining=${count}
start_time=$(date +%s)
fcst_history_done_idle=0

while [[ ${remaining} -gt 0 ]]; do
    remaining_before=${remaining}

    # Propagate JGLOBAL_FORECAST failure: if the finalized sentinel exists and its
    # content matches "aborted rc=<rc>", exit with that rc so JGLOBAL_FORECAST_MANAGER
    # also fails instead of masking a broken forecast with a green manager job.
    if [[ -n "${FCST_FINALIZED_SENTINEL:-}" && -f "${FCST_FINALIZED_SENTINEL}" ]]; then
        _fcst_final_content=$(< "${FCST_FINALIZED_SENTINEL}")
        if [[ "${_fcst_final_content}" == *aborted* ]]; then
            _fcst_rc=1
            if [[ "${_fcst_final_content}" =~ rc=([0-9]+) ]]; then
                _fcst_rc="${BASH_REMATCH[1]}"
                if ((_fcst_rc == 0)); then
                    _fcst_rc=1
                fi
            fi
            echo "FATAL ERROR [${component}]: JGLOBAL_FORECAST aborted (rc=${_fcst_rc}); propagating failure" >&2
            exit "${_fcst_rc}"
        fi
    fi

    for ((i = 0; i < count; i++)); do
        if [[ "${done_flag[i]}" == "YES" ]]; then
            continue
        fi

        _fcst_history_done_fallback=0
        _missing_sentinel=0
        _data_file_trigger=0
        _size_check_msgs=""
        if [[ ! -f "${local_log[i]}" ]]; then
            # History-done fallback for ocean and ice.
            # Ocean (MOM6): sentinel may be absent for any output period due
            # to NFS metadata lag (period log written at the start of the
            # next averaging period, so the final window's log is never
            # produced even without lag).
            # Ice (CICE): sentinel may be absent for any forecast hour due to
            # NFS metadata lag or a cice_fhr_offset mismatch.
            # Both cases: warn, copy, and run a post-copy size check.
            if [[ ("${component}" == "ocn" || "${component}" == "ice") &&
                -n "${FCST_HISTORY_DONE_SENTINEL:-}" && -f "${FCST_HISTORY_DONE_SENTINEL}" &&
                -f "${local_data[i]}" ]]; then
                _fcst_history_done_fallback=1
                _missing_sentinel=1
                echo "WARNING: [${component}] sentinel '$(basename "${local_log[i]}")' not found; model history complete and data present -- copying without sentinel"
            else
                continue
            fi
        fi

        # Sentinel exists, or history-done fallback active; process all rows that share this sentinel
        this_ll="${local_log[i]}"
        this_cl="${com_log[i]}"
        _ll_base=$(basename "${this_ll}")
        # Data-file trigger: sentinel column holds the next-hour ice output (*.nc),
        # the history-done sentinel (fcst_history_done_seg*), or the job-finalized
        # sentinel (fcst_finalized_seg*) rather than a text log. The trigger signals
        # current-hour data is ready; the manager writes a synthetic COM log instead
        # of copying the trigger file itself.
        if [[ "${this_ll}" == *.nc || "${_ll_base}" == fcst_history_done_seg* || "${_ll_base}" == fcst_finalized_seg* ]]; then
            _data_file_trigger=1
        fi

        # RERUN safety: if com_log already in COM, mark all rows for this sentinel done
        if [[ -f "${this_cl}" ]]; then
            for ((j = 0; j < count; j++)); do
                if [[ "${done_flag[j]}" == "NO" && "${local_log[j]}" == "${this_ll}" ]]; then
                    done_flag[j]="YES"
                    ((remaining--)) || true
                fi
            done
            continue
        fi

        # Copy all data files that share this sentinel (data first, log last).
        # If a data file is not yet visible on this node (Lustre/NFS metadata
        # latency after the sentinel write), wait FCST_MGR_STABILITY_WAIT
        # seconds then re-check; if still absent, defer the entire sentinel
        # group to the next poll cycle rather than exiting with a fatal error.
        _deferred=0
        for ((j = 0; j < count; j++)); do
            if [[ "${done_flag[j]}" == "YES" ]]; then
                continue
            fi
            if [[ "${local_log[j]}" != "${this_ll}" ]]; then
                continue
            fi
            if [[ ! -f "${local_data[j]}" ]]; then
                if [[ ${FCST_MGR_STABILITY_WAIT:-0} -gt 0 ]]; then
                    sleep "${FCST_MGR_STABILITY_WAIT}"
                fi
                if [[ ! -f "${local_data[j]}" ]]; then
                    echo "INFO [${component}]: '$(basename "${local_data[j]}")' not yet visible; deferring sentinel $(basename "${this_ll}")"
                    _deferred=1
                    break
                fi
            fi
            # File is present; verify it is not still being written.
            # Take two size snapshots FCST_MGR_STABILITY_WAIT seconds apart;
            # if the size changes or is zero the write is still in progress.
            if [[ ${FCST_MGR_STABILITY_WAIT:-0} -gt 0 ]]; then
                _sz_pre=$(stat -c %s "${local_data[j]}" 2> /dev/null || echo -1)
                sleep "${FCST_MGR_STABILITY_WAIT}"
                _sz_post=$(stat -c %s "${local_data[j]}" 2> /dev/null || echo -1)
                if [[ "${_sz_pre}" -ne "${_sz_post}" || "${_sz_post}" -le 0 ]]; then
                    echo "INFO [${component}]: '$(basename "${local_data[j]}")' still flushing (${_sz_pre} to ${_sz_post} B); deferring"
                    _deferred=1
                    break
                fi
            fi
            com_dir=$(dirname "${com_data[j]}")
            if [[ ! -d "${com_dir}" ]]; then
                mkdir -p "${com_dir}"
            fi
            cpfs "${local_data[j]}" "${com_data[j]}"
            copy_err=$?
            if [[ ${copy_err} -ne 0 ]]; then
                echo "FATAL ERROR [${component}]: cpfs '${local_data[j]}' -> '${com_data[j]}' failed (err=${copy_err})" >&2
                exit "${copy_err}"
            fi
            if [[ ${_missing_sentinel} -eq 1 ]]; then
                _sz_new=$(stat -c %s "${com_data[j]}")
                # Find an already-copied COM file of the same file type (same
                # name pattern, different forecast hour) as a size reference.
                # Strip the forecast-hour token (f006, F06, etc.) to derive a
                # normalised pattern, then only accept a match with the same
                # normalised name so we compare apples to apples.
                _ref_size=""
                _ref_name=""
                _cur_norm=$(basename "${com_data[j]}" | sed 's/[Ff][0-9]\{2,3\}/fNNN/g')
                for ((k = 0; k < count; k++)); do
                    if [[ "${done_flag[k]}" == "YES" &&
                        "${local_log[k]}" != "${this_ll}" &&
                        -f "${com_data[k]}" ]]; then
                        _ref_norm=$(basename "${com_data[k]}" | sed 's/[Ff][0-9]\{2,3\}/fNNN/g')
                        if [[ "${_ref_norm}" == "${_cur_norm}" ]]; then
                            _ref_size=$(stat -c %s "${com_data[k]}")
                            _ref_name=$(basename "${com_data[k]}")
                            break
                        fi
                    fi
                done
                if [[ -n "${_ref_size}" ]]; then
                    _sz_diff=$((_sz_new - _ref_size))
                    if [[ ${_sz_diff} -lt 0 ]]; then
                        _sz_diff=$((-_sz_diff))
                    fi
                    if [[ ${_sz_diff} -gt 1048576 ]]; then
                        echo "ERROR: [${component}] size mismatch for '$(basename "${com_data[j]}")': ${_sz_new}B vs reference '${_ref_name}': ${_ref_size}B (diff=${_sz_diff}B > 1 MB) -- possible partial/corrupt output"
                        exit 1
                    else
                        _size_check_msgs+="INFO: size OK for '$(basename "${com_data[j]}")': ${_sz_new}B (ref '${_ref_name}': ${_ref_size}B)"$'\n'
                    fi
                else
                    _size_check_msgs+="INFO: no reference COM file available yet for size check of '$(basename "${com_data[j]}")'"$'\n'
                fi
            fi
        done
        if [[ ${_deferred} -eq 1 ]]; then
            continue
        fi

        # Copy sentinel log last.
        # Skip if already in COM (e.g. RERUN scenario where data was already copied).
        if [[ ! -f "${this_cl}" ]]; then
            cl_dir=$(dirname "${this_cl}")
            if [[ ! -d "${cl_dir}" ]]; then
                mkdir -p "${cl_dir}"
            fi
            if [[ ${_fcst_history_done_fallback} -eq 1 ]]; then
                # history-done fallback: model never wrote the period log; write a synthetic COM marker.
                _cl_base=$(basename "${this_cl}")
                {
                    echo "synthetic sentinel created (model sentinel unavailable): ${_cl_base} at $(date --utc +%Y%m%d%H%M%S)"
                    if [[ ${_missing_sentinel} -eq 1 ]]; then
                        echo "WARNING: model sentinel '${_ll_base}' was not produced"
                        if [[ -n "${_size_check_msgs}" ]]; then
                            printf '%s' "${_size_check_msgs}"
                        fi
                    fi
                } > "${this_cl}"
                log_err=0
            elif [[ ${_data_file_trigger} -eq 1 ]]; then
                # Data-file trigger: sentinel column is the next-hour ice output
                # (*.nc), fcst_history_done_seg, or fcst_finalized_seg -- not a text
                # log to copy to COM. Write a compact synthetic COM sentinel instead.
                _cl_base=$(basename "${this_cl}")
                {
                    echo "sentinel created from data-file trigger '${_ll_base}': ${_cl_base} at $(date --utc +%Y%m%d%H%M%S)"
                    if [[ -n "${_size_check_msgs}" ]]; then
                        printf '%s' "${_size_check_msgs}"
                    fi
                } > "${this_cl}"
                log_err=0
            else
                cpfs "${this_ll}" "${this_cl}"
                log_err=$?
            fi
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

    if [[ ${remaining} -eq 0 ]]; then
        break
    fi

    # Timeout check (fatal: hard wall enforced by batch scheduler walltime).
    elapsed=$(($(date +%s) - start_time))
    if [[ ${FCST_MGR_TIMEOUT:-0} -gt 0 && ${elapsed} -gt ${FCST_MGR_TIMEOUT} ]]; then
        echo "FATAL ERROR [${component}]: Timed out after ${elapsed}s with ${remaining} sentinels still pending" >&2
        exit 1
    fi

    # Graceful exit: once the model has finished writing history (fcst_history_done
    # sentinel present), count consecutive poll cycles where no new files were
    # processed. After FCST_MGR_DONE_IDLE_MAX idle cycles (default 3) exit with a
    # warning for any entries the model never produced (e.g. optional GOCART output
    # types).
    #
    # Suppression: while any remaining row's sentinel is a fcst_finalized_seg* trigger,
    # do NOT count idle cycles. That row is written by exglobal_forecast.sh AFTER every
    # *_out completes (restart copies to COM), which happens after fcst_history_done.
    # Counting idle cycles here would let the manager exit -- releasing downstream jobs
    # that key off the finalized sentinel -- while restart copies are still writing under
    # DATAjob.
    if [[ -n "${FCST_HISTORY_DONE_SENTINEL:-}" && -f "${FCST_HISTORY_DONE_SENTINEL}" ]]; then
        _finalized_pending=0
        for ((k = 0; k < count; k++)); do
            if [[ "${done_flag[k]}" == "NO" ]]; then
                _k_base=$(basename "${local_log[k]}")
                if [[ "${_k_base}" == fcst_finalized_seg* ]]; then
                    _finalized_pending=1
                    break
                fi
            fi
        done
        if [[ ${_finalized_pending} -eq 1 ]]; then
            fcst_history_done_idle=0
        elif [[ ${remaining} -lt ${remaining_before} ]]; then
            fcst_history_done_idle=0
        else
            ((fcst_history_done_idle++)) || true
            idle_max=${FCST_MGR_DONE_IDLE_MAX:-3}
            if [[ ${fcst_history_done_idle} -ge ${idle_max} ]]; then
                echo "WARNING: [${component}] Model history complete; no new files for ${fcst_history_done_idle} consecutive poll cycle(s). ${remaining} of ${count} table entry(s) were not produced by the model; skipping."
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
