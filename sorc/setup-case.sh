#!/bin/bash
#https://global-workflow.readthedocs.io/en/latest/setup.html

#set -x

 GLOBALWORKFLOWTEMP=/contrib/Wei.Huang/run
 GLOBALWORKFLOWHOME=/contrib/Wei.Huang/src/global-workflow-cloud
#ATMOSRES=48
#ATMOSRES=96
 ATMOSRES=192
#ATMOSRES=384
 PSLOT=c${ATMOSRES}atm
 EXPNAME=gfs
 PSLOT=c${ATMOSRES}atm
 IDATE=2024010100
 EDATE=2024010100
 COMROOT=${GLOBALWORKFLOWTEMP}/comroot
 EXPDIR=${GLOBALWORKFLOWTEMP}/expdir
 CONFIGDIR=${GLOBALWORKFLOWHOME}/parm/config
 export BASE_CPLIC=/contrib/Wei.Huang/data/ICs
 export CPL_ATMIC=C${ATMOSRES}

 source ${GLOBALWORKFLOWHOME}/workflow/gw_setup.sh

 ${GLOBALWORKFLOWHOME}/workflow/setup_expt.py ${EXPNAME} forecast-only \
        --app ATM \
        --idate ${IDATE} \
        --edate ${EDATE} \
        --pslot ${PSLOT} \
        --configdir ${CONFIGDIR}/gfs \
        --resdetatmos ${ATMOSRES} \
        --comroot ${COMROOT} \
        --expdir ${EXPDIR}

 ${GLOBALWORKFLOWHOME}/workflow/setup_xml.py ${EXPDIR}/${PSLOT}

