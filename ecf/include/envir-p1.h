# envir-p1.h
export job=${job:-$PBS_JOBNAME}
export jobid=${jobid:-$job.$PBS_JOBID}

export RUN_ENVIR=nco
export envir=%ENVIR%
export MACHINE_SITE=%MACHINE_SITE%
export RUN=%RUN%

if [ -n "%SENDCANNEDDBN:%" ]; then export SENDCANNEDDBN=${SENDCANNEDDBN:-%SENDCANNEDDBN:%}; fi
export SENDCANNEDDBN=${SENDCANNEDDBN:-"NO"}

if [[ "$envir" == prod && "$SENDDBN" == YES ]]; then
    export eval=%EVAL:NO%
    if [ $eval == YES ]; then export SIPHONROOT=${UTILROOT}/para_dbn
    else export SIPHONROOT=/lfs/h1/ops/prod/dbnet_siphon
    fi
    if [ "$PARATEST" == YES ]; then export SIPHONROOT=${UTILROOT}/fakedbn; export NODBNFCHK=YES; fi
else
    export SIPHONROOT=${UTILROOT}/fakedbn
fi
export SIPHONROOT=${UTILROOT}/fakedbn
export DBNROOT=$SIPHONROOT

if [[ ! " prod para test " =~ " ${envir} " && " ops.prod ops.para " =~ " $(whoami) " ]]; then err_exit "ENVIR must be prod, para, or test [envir-p1.h]"; fi

# Developer configuration
PTMP=/lfs/h3/emc/eib/noscrub/ptmp
model=gfs
PSLOT=ecflow_gfs
export COMROOT=${PTMP}/${USER}/${PSLOT}/para/com
export COMPATH=${COMROOT}/${model}
#### export COMgfs=$(compath.py gfs/${gfs_ver})
export COMgfs=/lfs/h3/emc/eib/noscrub/ptmp/${USER}/ecflow_gfs/para/com
export DATAROOT=/lfs/h3/emc/eib/noscrub/stmp/${USER}/${model}/${PSLOT}/${RUN}.${CDATE}
export DBNLOG=${DATAROOT}/DBNLOG
mkdir -p ${DATAROOT}/emc_ecflow_header_work ${DBNLOG}
cd ${DATAROOT}/emc_ecflow_header_work
