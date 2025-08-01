#!/usr/bin/env bash
set -x
source /home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/dev/ush/gw_setup.sh
/home/tmcguinness/GITHUB/COPILOT/rocoto_forked/bin/rocotorun -d /home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/dev/ci/scripts/unittests/testdata/RUNTESTS/EXPDIR/C48mx500_3DVarAOWCDA/C48mx500_3DVarAOWCDA.db -w /home/tmcguinness/GITHUB/COPILOT/global-workflow_forked/dev/ci/scripts/unittests/testdata/RUNTESTS/EXPDIR/C48mx500_3DVarAOWCDA/C48mx500_3DVarAOWCDA.xml
