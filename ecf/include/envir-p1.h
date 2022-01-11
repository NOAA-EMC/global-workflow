# envir-p1.h
export job=${job:-$PBS_JOBNAME}
export jobid=${jobid:-$job.$PBS_JOBID}

export RUN_ENVIR=emc
export envir=%ENVIR%
export MACHINE_SITE=%MACHINE_SITE%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}
if [[ "$envir" == prod && "$SENDDBN" == YES ]]; then
    export eval=%EVAL:NO%
    if [ $eval == YES ]; then 
      export SIPHONROOT=${UTILROOT}/para_dbn
    else 
      export SIPHONROOT=/lfs/h1/ops/prod/dbnet_siphon
    fi
    export SIPHONROOT=${UTILROOT}/fakedbn
else
    export SIPHONROOT=${UTILROOT}/fakedbn
fi

export DBNROOT=$SIPHONROOT

if [[ ! " prod para test " =~ " ${envir} " && " ops.prod ops.para " =~ " $(whoami) " ]]; then err_exit "ENVIR must be prod, para, or test [envir-p1.h]"; fi
export ECF_PORT=34326
export DATAROOT=/lfs/h2/emc/stmp/Lin.Gan/RUNDIRS/ecfops
export COMROOT=/lfs/h2/emc/ptmp/Lin.Gan/ecfops/com
export COREROOT=/lfs/h2/emc/ptmp/production.core/$jobid
export NWROOT=/lfs/h1/ops/prod
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}
export TMPDIR=${TMPDIR:-${DATAROOT:?}}
if [ -n "%PDY:%" ]; then 
  export PDY=${PDY:-%PDY:%}
  export CDATE=${PDY}%CYC:%
fi
if [ -n "%COMPATH:%" ]; then export COMPATH=${COMPATH:-%COMPATH:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi


