#!/usr/bin/env bash

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../../.." >/dev/null 2>&1 && pwd )"
source "${HOMEgfs}/ush/detect_machine.sh"

utility_function="${1}"

source "${HOMEgfs}/ci/scripts/utils/ci_utils.sh"
${utility_function} "${@:2}"
