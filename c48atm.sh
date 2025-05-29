#!/bin/bash

 set -x

 rundir=/lustre/Wei.Huang/run
 mkdir -p ${rundir}

 source ~/.bashrc
 source dev/ush/gw_setup.sh
 source ~/prod_util.env

 HPC_ACCOUNT=Wei.Huang \
        pslot=c48atm \
        RUNTESTS=${rundir} \
        RUNDIR=/lustre/Wei.Huang/run/RUNDIR \
        HOMEDIR=/lustre/Wei.Huang \
        bin/run_python.sh dev/workflow/create_experiment.py \
        --yaml dev/ci/cases/pr/C48_ATM.yaml

