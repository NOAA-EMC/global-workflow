# envir-p1.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-${job}.$LSB_JOBID}

export RUN_ENVIR=${RUN_ENVIR:-nco}
export envir=%ENVIR%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}

export COMROOT=${COMROOT:-/com}
export GESROOT=${GESROOT:-/nwges}
export UTILROOT=${UTILROOT:-/nwprod/util}
export DCOMROOT=/dcom/us007003  # previously set to /dcom in .bash_profile

case $envir in
  prod)
    export jlogfile=${jlogfile:-${COMROOT}/logs/jlogfiles/jlogfile.${jobid}}
    export DATAROOT=${DATAROOT:-/tmpnwprd1}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/iodprod/dbnet_siphon  # previously set in .bash_profile
    else
       export DBNROOT=/nwprod/spa_util/fakedbn
    fi
    ;;
  eval)
    export envir=para
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd2}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/nwprod/spa_util/para_dbn
       SENDDBN_NTC=NO
    else
       export DBNROOT=/nwprod/spa_util/fakedbn
    fi
    ;;
  para|test)
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd2}
    export DBNROOT=/nwprod/spa_util/fakedbn
    ;;
  *)
    ecflow_client --abort="ENVIR must be prod, para, eval, or test [envir.h]"
    exit
    ;;
esac

export NWROOT=${NWROOT:-/nw${envir}}
export PCOMROOT=${PCOMROOT:-/pcom/${envir}}
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}
