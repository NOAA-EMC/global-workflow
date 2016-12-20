#!/bin/sh

#BSUB -oo emcsfc.log
#BSUB -eo emcsfc.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -R affinity[core]
#BSUB -J emcsfc
#BSUB -P GFS-T2O
#BSUB -W 0:03

set -x


export cyc="00"
export job=emcsfc_sfc_prep_${cyc}
export KEEPDATA="YES"
export SENDECF="NO"
export SENDCOM="YES"
export RUN_ENVIR="nco"

export DATA="/stmpp1/$LOGNAME/tmpnwprd/${job}"
export jlogfile="/stmpp1/$LOGNAME/jlogfile"
export COMROOT="/com"
export COMOUT="/stmpp1/$LOGNAME/com"
export COMIN_m6hrs="/stmpp1/$LOGNAME/com_old"

export envir="prod"

export NWROOT=${LS_SUBCWD}/../..
export global_shared_ver="v13.0.0"
export gfs_ver="v13.0.0"
export gdas_ver="v13.0.0"

module load grib_util
module load prod_util

export jobid="LLgfs_emcsfc_sfc_prep"
$NWROOT/gfs.${gfs_ver}/jobs/JGFS_EMCSFC_SFC_PREP

exit 0
