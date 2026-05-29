#! /usr/bin/env bash

set -x

export job="arch_logs"
export jobid="${job}.$$"

# Source config.base to get ATARDIR
ATARDIR=$(source "${EXPDIR}/config.base" >& /dev/null && echo "${ATARDIR}")

if [[ -z "${ATARDIR:-}" ]]; then
    echo "FATAL ERROR Could not determine the HPSS archive directory (ATARDIR)!"
    exit 1
fi

###############################################################
# Create the tarball of ecflow logs
cd "${ECF_LOG_DIR}" || exit 1

htar -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_logs.tar" "${PDY}${cyc}/${RUN}_"*

status=$?

exit "${status}"
