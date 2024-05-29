#!/bin/bash
#https://global-workflow.readthedocs.io/en/latest/setup.html

#set -x

 GLOBALWORKFLOWHOME=/contrib/Wei.Huang/src/global-workflow-cloud
#ATMOSRES=48
 ATMOSRES=96
#ATMOSRES=192
#ATMOSRES=384
 PSLOT=c${ATMOSRES}atm
 EXPNAME=gfs
 PSLOT=c${ATMOSRES}atm
#PSLOT=C48C48mx500
 IDATE=2024010100
 EDATE=2024010100
#IDATE=2021032306
#EDATE=2021032306

 GLOBALWORKFLOWTEMP=/lustre/Wei.Huang/run
 COMROOT=${GLOBALWORKFLOWTEMP}/comroot

 GLOBALWORKFLOWTEMP=/lustre/Wei.Huang/run
 EXPDIR=${GLOBALWORKFLOWTEMP}/expdir

 CONFIGDIR=${GLOBALWORKFLOWHOME}/parm/config
 export BASE_CPLIC=/contrib/Wei.Huang/data/ICs
 export CPL_ATMIC=C${ATMOSRES}
#export BASE_CPLIC=/contrib/Wei.Huang/data/ICs
#export CPL_ATMIC=C48C48mx500
#export IC_PREFIX=gdas
#export IC_TYPE=restart

#workflow/hosts/awspw.yaml
 export STMP=/lustre/${USER}/stmp
 export PTMP=/lustre/${USER}/ptmp
 export NOSCRUB=/s3bucket/${USER}/archive

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

