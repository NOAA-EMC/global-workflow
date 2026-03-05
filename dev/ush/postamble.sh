#! /usr/bin/env bash

#######
# Defines the postamble function for use in J-jobs and ex-scripts.
#
# Source this file to load the function into the current shell:
#   source "${USHglobal}/postamble.sh"
#
# Then register with trap:
#   trap "postamble ${start_time}" EXIT
#######

postamble() {
    #
    # Commands to execute when a script ends.
    #
    # Syntax:
    #   postamble start_time [rc]
    #
    #   Arguments:
    #     start_time: start time of script (in seconds since epoch)
    #     rc:         exit code of the script [default: $?]
    #

    set +x
    local start_time="${1}"
    local rc="${2:-$?}"

    # Execute any commands registered in POSTAMBLE_CMD
    #
    # Commands can be added to the postamble by appending them to $POSTAMBLE_CMD:
    #    POSTAMBLE_CMD="new_thing; ${POSTAMBLE_CMD:-}" # (before existing commands)
    #    POSTAMBLE_CMD="${POSTAMBLE_CMD:-}; new_thing" # (after existing commands)
    #
    # These commands will be called when EACH SCRIPT terminates, so be mindful.
    # Please consult with global-workflow CMs about permanent changes to
    # $POSTAMBLE_CMD or this postamble function.
    if [[ -v 'POSTAMBLE_CMD' ]]; then
        ${POSTAMBLE_CMD}
    fi

    # Calculate the elapsed time
    local end_time end_time_human elapsed_sec elapsed
    end_time=$(date +%s)
    end_time_human=$(date -d@"${end_time}" -u +%H:%M:%S)
    elapsed_sec=$((end_time - start_time))
    elapsed=$(date -d@"${elapsed_sec}" -u +%H:%M:%S)

    echo "End ${_calling_script:-script} at ${end_time_human} with error code ${rc} (time elapsed: ${elapsed})"
    exit "${rc}"
}

declare -xf postamble
