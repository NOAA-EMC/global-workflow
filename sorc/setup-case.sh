#!/bin/bash
#https://global-workflow.readthedocs.io/en/latest/setup.html

 set -x

 cd ${GLOBALWORKFLOWHOME}/workflow

 GLOBALWORKFLOWTEMP=/contrib/Wei.Huang/run
 GLOBALWORKFLOWHOME=/contrib/Wei.Huang/src/global-workflow-cloud
 PSLOT=c48atm
 CONFIGDIR=${GLOBALWORKFLOWHOME}/parm/config
 IDATE=2024010100
 EDATE=2024010100
 COMROOT=${GLOBALWORKFLOWTEMP}/comroot
 EXPDIR=${GLOBALWORKFLOWTEMP}/expdir

 mkdir -p ${COMROOT} ${EXPDIR}

 source ${GLOBALWORKFLOWHOME}/workflow/gw_setup.sh

 ${GLOBALWORKFLOWHOME}/workflow/setup_expt.py gfs forecast-only \
        --idate ${IDATE} \
        --edate ${EDATE} \
        --app ATM \
        --pslot ${PSLOT} \
        --configdir ${CONFIGDIR}/gfs \
        --resdetatmos 48 \
        --comroot ${COMROOT} \
        --expdir ${EXPDIR}

#        --resdetocean 1.0 \
#${GLOBALWORKFLOWHOME}/workflow/setup_expt.py gfs forecast-only \
#       --idate $IDATE \
#       --edate $EDATE \
#       [--app $APP] \
#       [--start $START] \
#       [--gfs_cyc $GFS_CYC] \
#       [--resdetatmos $RESDETATMOS] \
#       [--resdetocean $RESDETOCEAN] \
#       [--pslot $PSLOT] \
#       [--configdir $CONFIGDIR] \
#       [--comroot $COMROOT] \
#       [--expdir $EXPDIR]

#${GLOBALWORKFLOWHOME}/workflow/setup_xml.py ${EXPDIR}/${PSLOT}

