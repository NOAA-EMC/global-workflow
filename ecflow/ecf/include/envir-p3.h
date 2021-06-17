# envir-p3.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-$job.$LSB_JOBID}

#### export RUN_ENVIR=${RUN_ENVIR:-nco}
export RUN_ENVIR=emc

export envir=%ENVIR%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}

#### FILESYSTEMROOT=/gpfs/%FILESYSTEM:dell1%
FILESYSTEMROOT=PTMP_ECFR

module load prod_envir/1.1.0 prod_util/1.1.4 EnvVars/1.0.3

case $envir in
  prod)
    export DATAROOT=${DATAROOT:-${FILESYSTEMROOT}/nco/ops/tmpnwprd}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=${UTILROOT}/fakedbn
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  eval)
    export envir=para
    export DATAROOT=${DATAROOT:-${FILESYSTEMROOT}/nco/ops/tmpnwprd}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=${UTILROOT}/para_dbn
       SENDDBN_NTC=NO
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  para|test)
    export DATAROOT=${DATAROOT:-${FILESYSTEMROOT}/nco/ops/tmpnwprd}
    export DBNROOT=${UTILROOT}/fakedbn
    ;;
  *)
    ecflow_client --abort="ENVIR must be prod, para, eval, or test [envir.h]"
    exit
    ;;
esac

export ECF_PORT=31867
export COMROOT=${FILESYSTEMROOT}/com/gfs/prod
export GESROOT=${FILESYSTEMROOT}/nco/ops/nwges
export COREROOT=${FILESYSTEMROOT}/ptmp/production.core/$jobid
export NWROOT=/gpfs/dell1/nco/ops/nw${envir}
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}
export TMPDIR=${TMPDIR:-${DATAROOT:?}}

if [ -n "%PARATEST:%" ]; then export PARATEST=${PARATEST:-%PARATEST:%}; fi
if [ -n "%PDY:%" ]; then export PDY=${PDY:-%PDY:%}; fi
if [ -n "%PDY:%" ]; then export CDATE=${PDY}%CYC%; fi
if [ -n "%COMPATH:%" ]; then export COMPATH=${COMPATH:-%COMPATH:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi
