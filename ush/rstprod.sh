#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

#---------------------------------------------------------
#  rstprod.sh
#
#  Restrict data from select sensors and satellites 
#---------------------------------------------------------

# Restrict select sensors and satellites

export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
rlist="saphir abi_g16"
for rtype in $rlist; do
    if compgen -G "*${rtype}*" > /dev/null; then
        ${CHGRP_CMD} *${rtype}*
    fi
done
