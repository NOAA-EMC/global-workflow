#! /usr/bin/env bash

#---------------------------------------------------------
# timer.sh
#
# Script to create profiler timing functions tick and tock.
#
# tick:
#   Starts a timer by storing current time in a stack.
#   Accepts an optional label to identify the timer instance.
#
#   Syntax:
#     tick [label]
#       label: Optional label to identify the timer instance [default: "Timer"]
#
# tock:
#   Stops a timer by calculating elapsed time since last tick and outputs
#   the elapsed time in seconds.
#   Accepts an optional label to check for the timer instance.
#   If the provided label does not match the one stored during tick, a warning is issued.
#
#   Syntax:
#     tock [label]
#       label: Optional label to identify the timer instance [default: "Timer"]
#---------------------------------------------------------

# Initialize stacks for tick-tock profiling (initialize only once)
if [[ -z ${_GW_TIMER_STACK+x} ]]; then
    declare -xa _GW_TIMER_STACK=()
    declare -xa _GW_LABEL_STACK=()
fi

tick() {
    set +x
    local start_time label
    start_time=$(date +%s%N)
    # Use provided label or default to "Timer" if $1 is empty
    label="${1:-Timer}"

    # Push values onto the stacks
    _GW_TIMER_STACK+=("${start_time}")
    _GW_LABEL_STACK+=("${label}")
    set -x
}

tock() {
    set +x
    local end_time
    end_time=$(date +%s%N)

    # Safety check
    if [[ ${#_GW_TIMER_STACK[@]} -eq 0 ]]; then
        echo "WARNING: 'tock' called without a matching 'tick'."
        set -x
        return 1
    fi

    local last_idx
    # Retrieve the last element index
    last_idx=$((${#_GW_TIMER_STACK[@]} - 1))

    # Get the start time and label
    local start_time label
    start_time=${_GW_TIMER_STACK[${last_idx}]}
    label=${_GW_LABEL_STACK[${last_idx}]}

    local label_input
    label_input="${1:-}"
    if [[ -n ${label_input} ]]; then
        if [[ ${label_input} != "${label}" ]]; then
            echo "WARNING: 'tock' label '${label_input}' does not match 'tick' label '${label}'."
        fi
    fi

    # Remove (pop) elements from stacks
    unset "_GW_TIMER_STACK[${last_idx}]"
    unset "_GW_LABEL_STACK[${last_idx}]"

    # Calculate elapsed time
    local elapsed_nanos elapsed_secs
    elapsed_nanos=$((end_time - start_time))
    elapsed_secs=$(echo "scale=3; ${elapsed_nanos} / 1000000000" | bc -l)

    # Output the result with the label
    echo "[${label}] Elapsed: ${elapsed_secs}s"
    set -x
}

declare -xf tick
declare -xf tock

# If the script is invoked directly (not sourced), dispatch to tick or tock
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    if [[ $# -eq 0 ]]; then
        echo "ERROR: timer.sh requires an argument: tick or tock"
        exit 1
    fi
    case "${1}" in
        tick) tick "${@:2}" ;;
        tock) tock "${@:2}" ;;
        *)
            echo "ERROR: Unknown argument '${1}'. Must be 'tick' or 'tock'."
            exit 1
            ;;
    esac
fi
