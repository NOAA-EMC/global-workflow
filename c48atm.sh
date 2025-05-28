#!/bin/bash

 set -x

 rundir=/gpfs/f6/scratch/Wei.Huang/run
 mkdir -p ${rundir}

 source ~/.bashrc
 source dev/ush/gw_setup.sh
 source ~/prod_util.env

 HPC_ACCOUNT=bil-fire8 \
        pslot=c48atm \
        RUNTESTS=${rundir} \
        RUNDIR=/gpfs/f6/scratch/Wei.Huang/run \
        bin/run_python.sh dev/workflow/create_experiment.py \
        --yaml dev/ci/cases/pr/C48_ATM.yaml

