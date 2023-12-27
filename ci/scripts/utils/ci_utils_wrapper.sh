#!/usr/bin/env bash

utitilty_function="${1}"

source "${HOMEgfs}/ci/scripts/utils/ci_utils.sh"
${utitilty_function} "${@:2}"