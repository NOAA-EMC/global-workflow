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
#           retains the per-product dep logs in COM.
#
#           If the model finishes writing history (fcst_history_done_seg appears)
#           while rows are still pending, a WARN sentinel is written to unblock
#           the job rather than hanging indefinitely.
#
# Usage:    forecast_atm_barrier.sh <barrier_table>
#             barrier_table - absolute path to the barrier table file
################################################################################

# $1 is required: the barrier table written by FV3_postdet. The :? causes bash
# to abort with a usage message if the argument is missing or empty.
barrier_table="${1:?Usage: forecast_atm_barrier.sh <barrier_table>}"

echo "INFO [atm_barrier]: Starting barrier; reading '${barrier_table}'"

if [[ ! -f "${barrier_table}" ]]; then
    echo "FATAL ERROR [atm_barrier]: Barrier table '${barrier_table}' not found" >&2
    exit 1
fi

# How often (in seconds) to re-check COM for new dep logs.
# Sourced from the parent job; default 5s keeps latency low without hammering
# the filesystem.
FCST_POLL_INTERVAL="${FCST_MGR_POLL_INTERVAL:-5}"

# Path to the file that JGLOBAL_FORECAST touches once the model has finished
# writing all history output for this segment.  The barrier uses this as a
# signal that no new dep logs will appear after a grace period, at which point
# it writes WARN sentinels rather than polling forever.
FCST_HISTORY_DONE_SENTINEL="${DATAjob}/fcst_history_done_seg${FCST_SEGMENT:-0}"

# How long (seconds) to keep polling after fcst_history_done appears before
# giving up and forcing WARN sentinels for still-pending rows.  The grace
# period exists because product-copy ranks can still be actively writing dep
# logs after the model exits.  Set to 0 to disable forced WARN drain entirely
# (barrier will poll until the batch walltime kills it).
FCST_POSTDONE_TIMEOUT="${FCST_MGR_POSTDONE_TIMEOUT:-120}"

# ---------------------------------------------------------------------------
# Data structures that track barrier state across poll cycles.
#
#   final_logs[i]   - The combined per-hour COM sentinel this script must
#                     write once all deps for row i are confirmed.
#                     e.g. COMOUT_ATMOS_HISTORY/gfs.t00z.log.f006.txt
#
#   all_deps_arr[i] - Space-separated list of per-product dep log paths that
#                     must all exist before final_logs[i] can be written.
#                     e.g. "...log.atm.atmf.f006.txt ...log.atm.sfcf.f006.txt"
#
#   pending_idx     - Array of row indices that have NOT yet been resolved.
#                     Starts as [0, 1, 2, ...] and shrinks as rows complete.
#                     Iterating only pending rows avoids re-checking rows that
#                     already finished in earlier poll cycles.
#
#   dep_seen        - Associative array (hash set) used as a "have I seen this
#                     dep file before?" cache.  The first time a dep file
#                     appears on disk it is inserted here and progress_made is
#                     set.  This lets the timeout logic distinguish "no new
#                     files at all" (idle → count toward timeout) from "some
#                     deps are trickling in" (active → reset timeout counter).
#
#   count           - Total number of rows parsed from the barrier table.
# ---------------------------------------------------------------------------
declare -a final_logs all_deps_arr pending_idx
declare -A dep_seen
count=0

# Parse the barrier table into the arrays above.
# Lines starting with '#' or empty lines are skipped (comments/blanks).
# Field layout per row:  final_com_log  dep1  dep2  [dep3  dep4]
while read -r line; do
    [[ -z "${line}" || "${line:0:1}" == "#" ]] && continue
    read -r -a fields <<< "${line}"
    final_logs[count]="${fields[0]}"
    # Everything after the first field is a dep log path.
    # "${fields[*]:1}" joins fields[1..N] back into a space-separated string
    # so they can be stored in a single array element and unpacked later.
    all_deps_arr[count]="${fields[*]:1}"
    pending_idx[count]="${count}"
    ((count++)) || true
done < "${barrier_table}"

total="${count}"
echo "INFO [atm_barrier]: ${total} forecast hour(s) to confirm"

# remaining  - number of rows not yet resolved (final sentinel not yet written).
#              Loop exits when this reaches zero.
remaining="${total}"

# postdone_elapsed - seconds elapsed since fcst_history_done appeared with no
#                    barrier progress.  Reset to 0 whenever any progress is
#                    observed (a row completes or a new dep file appears).
#                    When this reaches FCST_POSTDONE_TIMEOUT the barrier writes
#                    WARN sentinels and exits.
postdone_elapsed=0

# postdone_announced - flag (0/1) so we print the "fcst_history_done detected"
#                      message only once rather than every poll cycle.
postdone_announced=0

while [[ "${remaining}" -gt 0 ]]; do
    # Snapshot remaining count at the start of this cycle so we can detect
    # whether any row completed during the cycle (used to reset the idle timer).
    remaining_before="${remaining}"

    # progress_made - set to 1 within a poll cycle if ANY forward progress
    #                 occurred: a row completed OR at least one new dep file
    #                 appeared for the first time.  Used by the timeout logic:
    #                 the idle timer only advances when progress_made stays 0
    #                 for an entire cycle, preventing premature WARN drain while
    #                 copy ranks are still actively writing dep logs.
    progress_made=0

    # Rebuild pending_idx with only the rows still unresolved after this cycle.
    new_pending=()

    for idx in "${pending_idx[@]}"; do
        final_log="${final_logs[${idx}]}"
        # Unpack the space-separated dep string back into an indexed array so
        # we can iterate over individual dep paths with a for loop.
        read -r -a deps <<< "${all_deps_arr[${idx}]}"

        # Fast-path: if the final sentinel already exists (written in a previous
        # run, a previous cycle of this loop, or by the WARN drain below),
        # skip re-processing this row entirely.  This is the restart/RERUN
        # safety valve — the barrier never re-writes a sentinel that is already
        # in COM.
        if [[ -f "${final_log}" ]]; then
            ((remaining--)) || true
            continue
        fi

        # Check whether every dep log for this forecast hour is present on disk.
        #
        # all_ready - boolean flag (1 = all deps present, 0 = at least one
        #             missing).  Starts optimistically at 1 and is cleared as
        #             soon as any dep is found to be absent.  Using a flag
        #             (rather than counting) keeps the logic simple: we break
        #             out of the dep loop on the first miss.
        all_ready=1
        for dep in "${deps[@]}"; do
            if [[ ! -r "${dep}" ]]; then
                # This dep is not yet visible; the hour is not complete.
                all_ready=0
                break
            fi
            # dep_seen tracks which dep files have been observed at least once.
            # progress_made is set only on first observation so the idle timer
            # resets only when genuinely new dep files arrive.
            if [[ -z "${dep_seen[${dep}]+_}" ]]; then
                dep_seen["${dep}"]=1
                progress_made=1
            fi
        done

        if [[ "${all_ready}" -eq 1 ]]; then
            echo "INFO [atm_barrier]: All deps for '$(basename "${final_log}")' confirmed; writing final sentinel"
            # Write the combined per-hour sentinel that downstream jobs poll.
            # Content is a human-readable timestamp; the presence of the file
            # (not its content) is what downstream jobs check.
            echo "ATM products confirmed in COM at $(date --utc +%Y%m%d%H%M%S)" > "${final_log}"
            # Per-product dep logs are retained in COM so the forecast manager
            # can skip already-copied products on segment rewind.
            ((remaining--)) || true
            progress_made=1
        else
            # Row still has missing deps; keep it in the pending list for the
            # next poll cycle.
            new_pending+=("${idx}")
        fi
    done

    # Replace pending_idx with the subset of rows that didn't complete this cycle.
    pending_idx=("${new_pending[@]}")

    if [[ "${remaining}" -gt 0 ]]; then
        # Timeout / WARN-drain logic: only activates after the model signals it
        # has finished writing history.  Before that sentinel appears we poll
        # indefinitely because the model may still be producing output.
        if [[ -f "${FCST_HISTORY_DONE_SENTINEL}" ]]; then
            # Reset the idle timer if any forward progress was made this cycle.
            # "Progress" means either a row completed (remaining dropped) or at
            # least one new dep file appeared for the first time (progress_made=1).
            # Without this reset a slow-but-still-copying rank would be killed
            # by the timeout even though it is making progress.
            if [[ "${progress_made}" -eq 1 || "${remaining}" -lt "${remaining_before}" ]]; then
                postdone_elapsed=0
            else
                # No progress this cycle: advance the idle clock by one poll interval.
                ((postdone_elapsed += FCST_POLL_INTERVAL)) || true
            fi

            # Print the "history done" notice only the first time we enter this block.
            if [[ "${postdone_announced}" -eq 0 ]]; then
                echo "INFO [atm_barrier]: fcst_history_done detected with ${remaining} row(s) pending; allowing copy ranks to finish"
                postdone_announced=1
            fi

            # If the idle clock has reached the timeout threshold, stop waiting
            # and write WARN sentinels for all still-pending rows.  This unblocks
            # downstream jobs (they see the sentinel file) while clearly marking
            # the sentinel content as a WARNING so post/archive can detect the
            # degraded state.
            if [[ "${FCST_POSTDONE_TIMEOUT}" -gt 0 && "${postdone_elapsed}" -ge "${FCST_POSTDONE_TIMEOUT}" ]]; then
                echo "WARN [atm_barrier]: Model history done and no barrier progress for ${postdone_elapsed}s; writing WARN sentinels for ${remaining} pending row(s)"
                for idx in "${pending_idx[@]}"; do
                    final_log="${final_logs[${idx}]}"
                    final_dir=$(dirname "${final_log}")
                    # Create the target directory if it does not yet exist
                    # (unlikely but possible if the hour never started copying).
                    if [[ ! -d "${final_dir}" ]]; then
                        mkdir -p "${final_dir}"
                    fi
                    # Write a WARN sentinel rather than a normal one.  The file's
                    # presence unblocks downstream jobs; the "WARN:" prefix in its
                    # content signals that not all products were confirmed in COM.
                    echo "WARN: ATM barrier timed out at $(date --utc +%Y%m%d%H%M%S)" > "${final_log}"
                    # Any dep logs that arrived before the timeout are retained in
                    # COM for post-mortem: they show which product types completed
                    # successfully before the timeout fired.
                done
                break
            fi
        fi
        # Sleep before the next poll cycle.  The interval is short enough to
        # keep copy-to-COM latency low but long enough to avoid hammering the
        # parallel filesystem with stat calls on every dep path.
        sleep "${FCST_POLL_INTERVAL}"
    fi
done

echo "INFO [atm_barrier]: Barrier complete (${total} forecast hour(s) processed)"
