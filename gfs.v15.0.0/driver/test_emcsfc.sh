#!/bin/sh

#--------------------------------------------------------------
# Run the JGFS_EMCSFC_SFC_PREP j-job on wcoss cray
#
# Invoke as follows:
# 'cat $script | bsub'
#--------------------------------------------------------------

#BSUB -oo emcsfc.log
#BSUB -eo emcsfc.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -J emcsfc
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

export cyc="00"
export job=emcsfc_sfc_prep_${cyc}
export KEEPDATA="YES"
export SENDECF="NO"
export SENDCOM="YES"
export RUN_ENVIR="nco"

export DATA="/gpfs/hps/stmp/$LOGNAME/tmpnwprd/${job}"
export jlogfile="/gpfs/hps/stmp/$LOGNAME/jlogfile"

module load prod_envir/1.0.1

export envir="prod"
export COMROOT="/gpfs/hps/stmp/${LOGNAME}"${COMROOT}

export NWROOT="/gpfs/hps/emc/global/noscrub/George.Gayno/q3fy17_final"
export global_shared_ver="v14.1.0"

module load grib_util/1.0.3
module load prod_util/1.0.5

export jobid="LLgfs_emcsfc_sfc_prep"
export gfs_ver="v14.1.0"
$NWROOT/gfs.${gfs_ver}/jobs/JGFS_EMCSFC_SFC_PREP

exit 0
