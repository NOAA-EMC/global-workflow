#! /usr/bin/env bash

#######
# A postamble for use in J-jobs and ex-scripts.
# This is intended to be used in a trap to execute commands when a script ends,
# regardless of how it ends (normal exit, error, etc.).
#
# To use, register with trap:
#   trap "${USHglobal}/postamble.sh ${start_time}" EXIT
#######

# Commands to execute when a script ends.
#
# Syntax:
#   postamble.sh start_time [rc]
#
#   Arguments:
#     start_time: start time of script (in seconds since epoch)
#     rc:         exit code of the script [default: $?]
#

set +x
start_time="${start_time:-${1}}"
rc="${rc:-${2:-$?}}"

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
_end_time=$(date +%s)
_end_time_human=$(date -d@"${_end_time}" -u +%H:%M:%S)
_elapsed_sec=$((_end_time - start_time))
_elapsed=$(date -d@"${_elapsed_sec}" -u +%H:%M:%S)

echo "End ${_calling_script:-script} at ${_end_time_human} with error code ${rc} (time elapsed: ${_elapsed})"
exit "${rc}"
