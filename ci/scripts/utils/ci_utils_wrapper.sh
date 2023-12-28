#!/usr/bin/env bash

HOMEgfs="$(cd "$(dirname  "${BASH_SOURCE[0]}")/../../.." >/dev/null 2>&1 && pwd )"

utitilty_function="${1}"

source "${HOMEgfs}/ci/scripts/utils/ci_utils.sh"
${utitilty_function} "${@:2}"
