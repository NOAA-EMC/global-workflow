#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         write_status_sentinel.sh
# Script description:  Write a completion / status sentinel on behalf of a
#                      J-job so any companion job that polls for it (forecast
#                      manager, downstream barrier, etc.) is never left hanging.
#
# Abstract: Designed to be invoked from an EXIT trap installed at the top of a
#           J-job. Caller passes the exit code and the sentinel path. On rc=0
#           the sentinel content is:
#               finalized at <UTC timestamp>
#           On any other rc it is:
#               aborted rc=<rc> at <UTC timestamp>
#
#           Idempotent: if the sentinel already exists the script leaves it
#           alone so a caller that wrote it explicitly is not clobbered.
#
#           Silent-skip design: any missing / unwritable path or empty argument
#           causes a clean exit 0 rather than a noisy crash inside a trap
#           handler. The sentinel is a best-effort safety net; the primary
#           failure signal remains the J-job's own exit code.
#
# Usage:    write_status_sentinel.sh <rc> <sentinel_path>
#             rc            - exit code to record in the sentinel (required)
#             sentinel_path - absolute path to the sentinel file (required)
#
#           Typical install in a J-job (after DATAjob is exported):
#             trap '"${USHglobal}/write_status_sentinel.sh" "$?" \
#                       "${DATAjob}/fcst_finalized_seg${FCST_SEGMENT:-0}"' EXIT
################################################################################

rc="${1:-0}"
sentinel_path="${2:-}"

if [[ -z "${sentinel_path}" ]]; then
    exit 0
fi

if [[ -f "${sentinel_path}" ]]; then
    exit 0
fi

sentinel_dir=$(dirname "${sentinel_path}")
mkdir -p "${sentinel_dir}" 2> /dev/null || true
if [[ ! -d "${sentinel_dir}" ]]; then
    exit 0
fi

if ((rc == 0)); then
    echo "finalized at $(date --utc +%Y%m%d%H%M%S)" > "${sentinel_path}"
else
    echo "aborted rc=${rc} at $(date --utc +%Y%m%d%H%M%S)" > "${sentinel_path}"
fi

exit 0
