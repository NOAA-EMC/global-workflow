#!/bin/bash
#https://global-workflow.readthedocs.io/en/latest/setup.html

 set -x

 GLOBALWORKFLOWHOME=/scratch2/NAGAPE/epic/Wei.Huang/src/global-workflow-cloud
 PSLOT=exp_c48
 CONFIGDIR=${GLOBALWORKFLOWHOME}/parm/config
 IDATE=2021032312
 EDATE=2021032312
 COMROOT=${GLOBALWORKFLOWHOME}/comroot
 EXPDIR=${GLOBALWORKFLOWHOME}/expdir

 cd ${GLOBALWORKFLOWHOME}/workflow
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

#usage: setup_expt.py gfs forecast-only [-h] [--pslot PSLOT] [--resdetatmos RESDETATMOS] [--resdetocean RESDETOCEAN]
#                                      [--comroot COMROOT] [--expdir EXPDIR] --idate IDATE --edate EDATE [--overwrite]
#                                      [--start {warm,cold}] [--cdump CDUMP] [--yaml YAML] [--app {ATM,ATMA,ATMW,S2S,S2SA,S2SW,S2SWA}]
#                                      [--gfs_cyc {1,2,4}]

#${GLOBALWORKFLOWHOME}/workflow/setup_xml.py ${EXPDIR}/${PSLOT}

