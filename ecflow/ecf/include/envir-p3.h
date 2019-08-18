# envir-p3.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-$job.$LSB_JOBID}

export RUN_ENVIR=${RUN_ENVIR:-nco}
export envir=%ENVIR%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}
FILESYSTEMROOT=/gpfs/%FILESYSTEM:dell1%

module load prod_envir/%prod_envir_ver% prod_util/%prod_util_ver%

case $envir in
  prod)
    export DATAROOT=${DATAROOT:-${FILESYSTEMROOT}/nco/ops/tmpnwprd}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/iodprod_dell/dbnet_siphon
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

export COMROOT=${FILESYSTEMROOT}/nco/ops/com
export COREROOT=${FILESYSTEMROOT}/ptmp/production.core/$jobid
export NWROOT=/gpfs/dell1/nco/ops/nw${envir}
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}

if [ -n "%PDY:%" ]; then export PDY=${PDY:-%PDY:%}; fi
if [ -n "%COMPATH:%" ]; then export COMPATH=${COMPATH:-%COMPATH:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi
