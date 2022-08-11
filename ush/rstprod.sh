#!/bin/bash

#---------------------------------------------------------
#  rstprod.sh
#
#  Restrict data from select sensors and satellites 
#---------------------------------------------------------

# Restrict select sensors and satellites
echo "--> rstprod.sh"

export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
rlist="saphir abi_g16"
for rtype in $rlist; do
    ${CHGRP_CMD} *${rtype}*
done

echo "<-- rstprod.sh"
