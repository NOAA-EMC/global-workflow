# envir-p1.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-$job.$LSB_JOBID}

export RUN_ENVIR=${RUN_ENVIR:-nco}
export envir=%ENVIR%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}

module load prod_envir prod_util

case $envir in
  prod)
    export jlogfile=${jlogfile:-${COMROOT}/logs/jlogfiles/jlogfile.${jobid}}
    export DATAROOT=${DATAROOT:-/tmpnwprd1}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/iodprod/dbnet_siphon  # previously set in .bash_profile
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  eval)
    export envir=para
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd2}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=${UTILROOT}/para_dbn
       SENDDBN_NTC=NO
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  para|test)
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd2}
    export DBNROOT=${UTILROOT}/fakedbn
    ;;
  *)
    ecflow_client --abort="ENVIR must be prod, para, eval, or test [envir.h]"
    exit
    ;;
esac

export NWROOT=/nw${envir}
export COMROOT=/com
export PCOMROOT=/pcom/${envir}
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}

# The following two lines are deprecated (only in the "old" header)
export DCOMROOT=/dcom/us007003  # previously set to /dcom in .bash_profile
export UTILROOT=/nwprod/util

if [ -n "%PDY:%" ]; then export PDY=${PDY:-%PDY:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi
