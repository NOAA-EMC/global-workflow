#!/bin/bash

 set -x

 rundir=/scratch4/NAGAPE/epic/Wei.Huang/run
 mkdir -p ${rundir}

 source ~/.bashrc
 source dev/ush/gw_setup.sh

 HPC_ACCOUNT=epic \
        pslot=c48atm \
        RUNTESTS=${rundir} \
        RUNDIR=/scratch4/NAGAPE/epic/Wei.Huang \
        bin/run_python.sh dev/workflow/create_experiment.py \
        --yaml dev/ci/cases/pr/C48_ATM.yaml

