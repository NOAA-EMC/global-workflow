#!/bin/bash
################################################################################
# run executable
export pgm=$JEDIVAREXE
. prep_step
$APRUN_AEROANL $DATA/fv3jedi_var.x $DATA/${CDUMP}aeroanl_${CDATE}.yaml 1>&1 2>&2
export err=$?; err_chk
