#! /usr/bin/env bash

set -x

export job="arch_logs"
export jobid="${job}.$$"

source "${HOMEgfs}/ush/jjob_header.sh" -e "arch_logs" -c "base"

###############################################################
# Tar up the ecflow logs
cd "${ECF_LOG_DIR}" || exit 1

htar -cvf "${ATARDIR}/${PDY}${cyc}/${RUN}_logs.tar" "${PDY}${cyc}/${RUN}_"*

status=$?

exit "${status}"
