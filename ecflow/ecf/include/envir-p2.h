# envir-p2.h
export job=${job:-$LSB_JOBNAME} #Can't use $job in filenames!
export jobid=${jobid:-$job.$LSB_JOBID}

export RUN_ENVIR=${RUN_ENVIR:-nco}
export envir=%ENVIR%
export SENDDBN=${SENDDBN:-%SENDDBN:YES%}
export SENDDBN_NTC=${SENDDBN_NTC:-%SENDDBN_NTC:YES%}

export COMROOTp1=${COMROOTp1:-/com}
export GESROOTp1=${GESROOTp1:-/nwges}
export COMROOT=${COMROOT:-/com2}
export GESROOT=${GESROOT:-/nwges2}
export DCOMROOT=${DCOMROOT:-/dcom}
export ECF_HOSTFILE=/nwprod/ecflow/hostfile
#export subhost=`echo ${LSB_SUB_HOST}| cut -c1`

module load prod_util

case $envir in
  prod)
    export jlogfile=${jlogfile:-${COMROOT}/logs/jlogfiles/jlogfile.${jobid}}
    export DATAROOT=${DATAROOT:-/tmpnwprd_p2}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=/iodprod/dbnet_siphon  # previously set in .bash_profile
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  eval)
    export envir=para
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd_p2}
    if [ "$SENDDBN" == "YES" ]; then
       export DBNROOT=${UTILROOT}/para_dbn
       SENDDBN_NTC=NO
    else
       export DBNROOT=${UTILROOT}/fakedbn
    fi
    ;;
  para|test)
    export jlogfile=${jlogfile:-${COMROOT}/logs/${envir}/jlogfile}
    export DATAROOT=${DATAROOT:-/tmpnwprd_p2}
    export DBNROOT=${UTILROOT}/fakedbn
    ;;
  *)
    ecflow_client --abort="ENVIR must be prod, para, eval, or test [envir.h]"
    exit
    ;;
esac

export NWROOT=/nw${envir}2
export NWROOTprod=/nwprod2
export NWROOTp1=/nwprod
export PCOMROOT=${PCOMROOT:-/pcom2/${envir}}
export SENDECF=${SENDECF:-YES}
export SENDCOM=${SENDCOM:-YES}
export KEEPDATA=${KEEPDATA:-%KEEPDATA:NO%}

if [ -n "%PDY:%" ]; then export PDY=${PDY:-%PDY:%}; fi
if [ -n "%COMPATH:%" ]; then export COMPATH=${COMPATH:-%COMPATH:%}; fi
if [ -n "%MAILTO:%" ]; then export MAILTO=${MAILTO:-%MAILTO:%}; fi
if [ -n "%DBNLOG:%" ]; then export DBNLOG=${DBNLOG:-%DBNLOG:%}; fi
