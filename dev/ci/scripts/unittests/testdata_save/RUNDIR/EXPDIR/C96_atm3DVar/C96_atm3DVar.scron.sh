#!/usr/bin/env bash
set -x
source /home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/dev/ush/gw_setup.sh
/home/tmcguinness/GITHUB/COPILOT/rocoto/bin/rocotorun -d /home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/dev/ci/scripts/unittests/testdata/RUNDIR/EXPDIR/C96_atm3DVar/C96_atm3DVar.db -w /home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/dev/ci/scripts/unittests/testdata/RUNDIR/EXPDIR/C96_atm3DVar/C96_atm3DVar.xml
