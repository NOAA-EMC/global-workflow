#! /usr/bin/env bash
# shellcheck disable=SC2016

# The purpose of this script is to read a target config.com and apply the ush/bash_utils.sh declare_from_tmpl function to generate the COM variable.
#
# Here are two cases of COM variable generation in the current workflow:
# YMD="${PDY}" HH="${cyc}" declare_from_tmpl -rx COMIN_ATMOS_INPUT:COM_ATMOS_INPUT_TMPL
# YMD="${PDY}" HH="${cyc}" declare_from_tmpl -rx COMOUT_CONF:COM_CONF_TMPL
#
#
# Here is the config.com for these TMPL variables:
# COM_BASE='${ROTDIR}/${RUN}.${YMD}/${HH}/${MEMDIR}'
# declare -rx COM_ATMOS_INPUT_TMPL=${COM_BASE}'/model/atmos/input'
# declare -rx COM_CONF_TMPL=${COM_BASE}'/conf'
#
#
# The output of this script will look like the following for non-member variables:
# COMIN_ATMOS_INPUT=${ROTDIR}/${RUN}.${YMD}/${HH}/model/atmos/input
# For member variables, it will look like this:
# COMIN_ATMOS_INPUT=${ROTDIR}/${RUN}.${YMD}/${HH}/${MEMDIR}/model/atmos/input

if [[ $# -ne 3 ]]; then
    echo "Usage: $0 <path/to/config.com> <path/to/jjob> <path/to/bash_utils.sh>" >&2
    exit 1
fi

set -eu -o pipefail

config_com="${1}"
jjob="${2}"
bash_utils="${3}"

if [[ ! -f "${config_com}" ]]; then
    echo "Error: config.com file '${config_com}' not found!" >&2
    exit 1
fi

if [[ ! -f "${jjob}" ]]; then
    echo "Error: jjob file '${jjob}' not found!" >&2
    exit 1
fi

# Source the config.com to get the TMPL variables
source "${config_com}" > /dev/null
# Source the bash_utils.sh to get the declare_from_tmpl function
source "${bash_utils}" > /dev/null

# Set static variable for use in templates
export ROTDIR='${ROTDIR}'
export DMPDIR='${DMPDIR}'
export IODADIR='${IODADIR}'

# Replace the declare_from_tmpl calls in the jjob with the generated COM variable declarations and replace them in the jjob
line_num=0
while IFS= read -r line; do
    line_num=$((line_num + 1))
    # Intialize RUN. This can be overridden by the prefix assignments.
    export RUN='${RUN}'
    if [[ "${line}" =~ declare_from_tmpl ]]; then
        # Use awk to get the number of leading spaces
        COUNT=$(awk '{print match($0, /[^ ]|$/)-1}' <<< "${line}")
        spaces=""
        for i in $(seq 1 "${COUNT}"); do
            spaces="${spaces} "
        done
        # Extract the prefix assignments to declare_from_tmpl
        prefix=$(echo "${line}" | sed -E 's/^(.*)declare_from_tmpl.*/\1/')
        # Parse the prefix to get all of the variable assignments (e.g. YMD="${PDY}" HH="${cyc}")
        IFS=' ' read -ra prefix_args <<< "${prefix}"
        # Convert these into literal variable assignments (e.g. YMD='${PDY}' HH='${cyc}')
        for i in "${!prefix_args[@]}"; do
            # shellcheck disable=SC2004
            prefix_args[${i}]=$(echo "${prefix_args[${i}]}" | sed -E 's/(.*)="*([a-zA-Z0-9_{}$]+)"*/\1='\''\2'\''/')
        done
        # Extract the arguments to declare_from_tmpl
        args=$(echo "${line}" | sed -E 's/.*declare_from_tmpl (.*)/\1/')
        # Extract the flags to declare_from_tmpl (e.g. -rx)
        flags=$(echo "${args}" | grep -E -o -- '-[a-zA-Z]+')
        # Generate the COM variable declaration using declare_from_tmpl
        # The prefix arguments need to be in the form of VAR='${var}' (i.e. we want the template to render a literal string with the variable names, not their values)
        # Use prefix_args to form these prefix assignments and pass them to declare_from_tmpl
        # Valid prefix args are DUMP, YMD, HH, GRDRESNAME, MEMDIR, RUN, and GRID
        for var in DUMP YMD HH GRDRESNAME MEMDIR RUN GRID; do
            for arg in "${prefix_args[@]}"; do
                if [[ "${arg}" =~ ${var} ]]; then
                    # Get what the variable is assigned to
                    assignment=$(echo "${arg}" | sed -E "s/${var}='(.*)'/\1/")
                    declare -x "${var}"="${assignment}"
                fi
            done
        done

        # Now render the template
        # shellcheck disable=SC2086
        COM=$(declare_from_tmpl -rx ${args} | sed 's/declare_from_tmpl :: \(.*\)=\(.*\)/\1=\2/')
        # Remove duplicate // in the COM path if it exists (e.g. if MEMDIR is empty, we don't want a double slash in the path)
        COM=$(echo "${COM}" | sed 's/\/\//\//g')
        # Prepend with declare ${flags} if flags are present
        if [[ -n "${flags}" ]]; then
            COM="declare ${flags} ${COM}"
        fi
        # And write it to stdout
        echo "${spaces}${COM}"
    else
        # Not a declare_from_tmpl line, just print it as is
        echo "${line}"
    fi

    # Unset any variables that may have been set in the prefix to avoid unintended consequences in later lines of the jjob
    unset DUMP YMD HH GRDRESNAME MEMDIR RUN GRID flags args spaces
done < "${jjob}"
