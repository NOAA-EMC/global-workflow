#! /usr/bin/env bash

#---------------------------------------------------------
# wait_for_file.sh
# Wait for a file to exist and return the status.
#
# Checks if a file exists periodically up to a maximum number of attempts. When the file
#   exists or the limit is reached, the status is returned (0 if the file exists, 1 if it
#   does not). This allows it to be used as a conditional to handle missing files.
#
# Syntax:
#   wait_for_file file_name [sleep_interval [max_tries]]
#
#     file_name:      File to check the existence of (must be readable)
#     sleep_interval: Time to wait between each check (in seconds) [default: 60]
#     max_tries:      The maximum number of checks to make [default: 100]
#
# Example:
#     ```
#     file_name=/path/to/foo
#     sleep_interval=60
#     max_tries=30
#     if wait_for_file; then
#       echo "FATAL ERROR: ${file_name} still does not exist after waiting one-half hour."
#       exit 1
#     fi
#     # Code that depends on file existing
#     ```
#---------------------------------------------------------

wait_for_file() {
    set +x
    local file_name=${1:?"wait_for_file() requires a file name"}
    local sleep_interval=${2:-60}
    local max_tries=${3:-100}

    for ((iter = 0; iter < max_tries; iter++)); do
        if [[ -r ${file_name} ]]; then
            set -x
            return 0
        fi
        sleep "${sleep_interval}"
    done
    set -x
    return 1
}

declare -xf wait_for_file
