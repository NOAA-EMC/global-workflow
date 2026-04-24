#! /usr/bin/env bash

################################################################################
# UNIX Script Documentation Block
# Script name:         wait_for_table.sh
# Script description:  Poll until a product table file appears on disk
#
# Abstract: Waits for a named product table file to be created, polling every
#           30 seconds until the file exists or the timeout is exceeded.
#           Exits 0 when the file is found; exits 1 on timeout.
#
# Usage:    wait_for_table.sh <label> <table_file> <timeout_seconds>
#             label          - human-readable component name used in log output
#             table_file     - absolute path to the product table file to wait for
#             timeout_seconds - maximum number of seconds to wait
################################################################################

label="${1:?Usage: wait_for_table.sh <label> <table_file> <timeout_seconds>}"
table="${2:?Usage: wait_for_table.sh <label> <table_file> <timeout_seconds>}"
timeout="${3:?Usage: wait_for_table.sh <label> <table_file> <timeout_seconds>}"

elapsed=0
echo "INFO: Waiting for ${label} product table at ${table}"
while [[ ! -f "${table}" ]]; do
    if [[ ${elapsed} -ge ${timeout} ]]; then
        echo "FATAL ERROR: Timed out after ${elapsed}s waiting for ${table}" >&2
        exit 1
    fi
    sleep 30
    (( elapsed += 30 )) || true
done
echo "INFO: ${label} product table found after ${elapsed}s"
