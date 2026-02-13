#! /usr/bin/env bash

# The purpose of this script is to read a target config.com and apply the ush/bash_utils.sh declare_from_tmpl function to generate the COM variable.
# For reference, here is the declare_from_tmpl function from bash_utils.sh:
# function declare_from_tmpl() {
#
#    local opts="-g"
#    local OPTIND=1
#    while getopts "rx" option; do
#        opts="${opts}${option}"
#    done
#    shift $((OPTIND - 1))

#    for input in "$@"; do
#        IFS=':' read -ra args <<< "${input}"
#        local com_var="${args[0]}"
#        local template
#        local value
#        if ((${#args[@]} > 1)); then
#            template="${args[1]}"
#        else
#            template="${com_var}_TMPL"
#        fi
#        if [[ ! -v "${template}" ]]; then
#            echo "FATAL ERROR in declare_from_tmpl: Requested template ${template} not defined!"
#            exit 2
#        fi
#        value=$(echo "${!template}" | envsubst)
#        # shellcheck disable=SC2086
#        declare ${opts} "${com_var}"="${value}"
#        # shellcheck disable=
#        echo "declare_from_tmpl :: ${com_var}=${value}"
#    done
#
#
# Here are two cases of COM variable generation in the current workflow:
# YMD="${PDY}" HH="${cyc}" declare_from_tmpl -rx \
#    COMIN_ATMOS_INPUT:COM_ATMOS_INPUT_TMPL \
#    COMOUT_CONF:COM_CONF_TMPL
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
    echo "Usage: $0 <config.com> <jjob>"
    exit 1
fi

config_com="${1}"
jjob="${2}"
bash_utils="${3}"

if [[ ! -f "${config_com}" ]]; then
    echo "Error: config.com file '${config_com}' not found!"
    exit 1
fi

if [[ ! -f "${jjob}" ]]; then
    echo "Error: jjob file '${jjob}' not found!"
    exit 1
fi

# Source the config.com to get the TMPL variables
source "${config_com}"
# Source the bash_utils.sh to get the declare_from_tmpl function
source "${bash_utils}"

# Replace the declare_from_tmpl calls in the jjob with the generated COM variable declarations and replace them in the jjob
while IFS= read -r line; do
    if [[ "${line}" =~ declare_from_tmpl ]]; then
        # Extract the arguments from the declare_from_tmpl call
        args=$(echo "${line}" | sed -E 's/.*declare_from_tmpl[[:space:]]+(-[rx]+)?[[:space:]]+(.*)/\2/')
        # Generate the COM variable declarations using declare_from_tmpl
        generated_declarations=$(declare_from_tmpl ${args})
        # Replace the declare_from_tmpl call with the generated declarations in the jjob
        jjob=$(echo "${jjob}" | sed "s|${line}|${generated_declarations}|")
    fi
done < "${jjob}"
