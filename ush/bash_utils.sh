#! /usr/bin/env bash

function declare_from_tmpl() {
    #
    # Define variables from corresponding templates by substituting in env variables.
    #
    # Each template must already be defined. Any variables in the template are replaced
    #   with their values. Undefined variables are just removed WITHOUT raising an error.
    #
    # Template can be used implicitly, however, all declared COM variables must be
    #   defined as either COMIN or COMOUT, therefore the template should be explicit
    #
    # Accepts as options `-r` and `-x`, which do the same thing as the same options in
    #   `declare`. Variables are automatically marked as `-g` so the variable is visible
    #   in the calling script.
    #
    # Syntax:
    #   declare_from_tmpl [-rx] $var1[:$tmpl1] [$var2[:$tmpl2]] [...]]
    #
    #   options:
    #       -r: Make variable read-only (same as `declare -r`)
    #       -x: Mark variable for export (same as `declare -x`)
    #   var1, var2, etc: Variable names whose values will be generated from a template
    #                    and declared
    #   tmpl1, tmpl2, etc: Specify the template to use (default is "${var}_TMPL")
    #
    #   Examples:
    #       # Current cycle and RUN, implicitly using template COM_ATMOS_ANALYSIS_TMPL
    #       YMD=${PDY} HH=${cyc} declare_from_tmpl -rx COM_ATMOS_ANALYSIS
    #
    #       # Previous cycle and gdas using an explicit template
    #       RUN=${GDUMP} YMD=${gPDY} HH=${gcyc} declare_from_tmpl -rx \
    #           COMIN_ATMOS_HISTORY_PREV:COM_ATMOS_HISTORY_TMPL
    #
    #       # Current cycle and COM for first member using an explicit template
    #       MEMDIR='mem001' YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
    #           COMOUT_ATMOS_HISTORY:COM_ATMOS_HISTORY_TMPL
    #
    if [[ ${DEBUG_WORKFLOW:-"NO"} == "NO" ]]; then
        set +x
    fi
    local opts="-g"
    local OPTIND=1
    while getopts "rx" option; do
        opts="${opts}${option}"
    done
    shift $((OPTIND - 1))

    for input in "$@"; do
        IFS=':' read -ra args <<< "${input}"
        local com_var="${args[0]}"
        local template
        local value
        if ((${#args[@]} > 1)); then
            template="${args[1]}"
        else
            template="${com_var}_TMPL"
        fi
        if [[ ! -v "${template}" ]]; then
            echo "FATAL ERROR in declare_from_tmpl: Requested template ${template} not defined!"
            exit 2
        fi
        value=$(echo "${!template}" | envsubst)
        # shellcheck disable=SC2086
        declare ${opts} "${com_var}"="${value}"
        # shellcheck disable=
        echo "declare_from_tmpl :: ${com_var}=${value}"
    done
    set_trace
}

function wait_for_file() {
    #
    # Wait for a file to exist and return the status.
    #
    # Checks if a file exists periodically up to a maximum number of attempts. When the file
    #   exists or the limit is reached, the status is returned (0 if the file exists,1 if it
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
    #
    set +x
    local file_name=${1:?"wait_for_file() requires a file name"}
    local sleep_interval=${2:-60}
    local max_tries=${3:-100}

    for ((iter = 0; iter < max_tries; iter++)); do
        if [[ -r ${file_name} ]]; then
            set_trace
            return 0
        fi
        sleep "${sleep_interval}"
    done
    set_trace
    return 1
}

# This utility is to be used to create a COM structure in the DATAROOT
# It will replace the root path (up to $COMROOT) with $DATAROOT
# Use realpath --relative-to to get the relative path from $COMROOT to the target file
# and then prepend $DATAROOT to that path to get the new target path
function dataroot_com_path() {
    #
    # Generate a COM path in the DATAROOT based on an existing COM path.
    #
    # This function takes an existing COM path and generates a corresponding
    # path in the DATAROOT by replacing the root directory with DATAROOT.
    #
    # Syntax:
    #   dataroot_com_path original_com_path
    #
    #   original_com_path: The original COM path to be transformed.
    #
    # Example:
    #   # Declare COMOUT_ATMOS_ANALYSIS using template
    #   YMD=${PDY} HH=${cyc} declare_from_tmpl -rx \
    #       COMOUT_ATMOS_ANALYSIS:COM_ATMOS_ANALYSIS_TMPL
    #   # Get the DATAROOT version of the COM path
    #   pCOMOUT_ATMOS_ANALYSIS=$(dataroot_com_path "${COMOUT_ATMOS_ANALYSIS}")
    #   echo "New COM path in DATAROOT: ${pCOMOUT_ATMOS_ANALYSIS}"
    #

    set +x
    if [[ $# -ne 1 ]]; then
        echo "FATAL ERROR in dataroot_com_path: Incorrect number of arguments!"
        echo "Usage: dataroot_com_path original_com_path"
        exit 2
    fi

    local original_com_path=${1}

    if [[ -z "${COMROOT:-}" || -z "${DATAROOT:-}" ]]; then
        echo "FATAL ERROR in dataroot_com_path: COMROOT and DATAROOT must be defined!"
        exit 2
    fi

    local relative_path
    relative_path=$(realpath --relative-to="${COMROOT}" "${original_com_path}")
    local new_com_path="${DATAROOT}/${relative_path}"

    echo "${new_com_path}"
    set_trace
}

# Initialize stacks for tick-tock profiling (initialize only once)
if [[ -z ${_GW_TIMER_STACK+x} ]]; then
    declare -xa _GW_TIMER_STACK=()
    declare -xa _GW_LABEL_STACK=()
fi

# Function: tick [label]
tick() {
    #
    # Start timer for profiling
    #
    # Starts a timer by storing current time in a stack
    #   Accepts an optional label to identify the timer instance
    #
    # Syntax:
    #   tick [label]
    #     label: Optional label to identify the timer instance [default: "Timer"]
    #
    set +x
    local start_time label
    start_time=$(date +%s%N)
    # Use provided label or default to "Timer" if $1 is empty
    label="${1:-Timer}"

    # Push values onto the stacks
    _GW_TIMER_STACK+=("${start_time}")
    _GW_LABEL_STACK+=("${label}")
    set_trace
}

# Function: tock
tock() {
    #
    # Stop timer and print elapsed time in seconds
    #
    # Stops a timer by calculating elapsed time since last tick and outputs the elapsed time in seconds.
    #   Accepts an optional label to check for the timer instance.
    #   If the provided label does not match the one stored during tick, a warning is issued.
    #
    # Syntax:
    #   tock [label]
    #
    set +x
    local end_time
    end_time=$(date +%s%N)

    # Safety check
    if [[ ${#_GW_TIMER_STACK[@]} -eq 0 ]]; then
        echo "WARNING: 'tock' called without a matching 'tick'."
        set_trace
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
    set_trace
}

# shellcheck disable=

declare -xf declare_from_tmpl
declare -xf wait_for_file
declare -xf dataroot_com_path
declare -xf tick
declare -xf tock
