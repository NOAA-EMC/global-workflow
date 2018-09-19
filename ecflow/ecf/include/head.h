set -xe  # print commands as they are executed and enable signal trapping

# Variables needed for communication with ecFlow version %ECF_VERSION%
export ECF_NAME=%ECF_NAME%
#export ECF_HOST=%ECF_HOST%
export ECF_HOST=%ECF_LOGHOST%
export ECF_PORT=%ECF_PORT%
export ECF_PASS=%ECF_PASS%
export ECF_TRYNO=%ECF_TRYNO%
export ECF_RID=${LSB_JOBID:-$$}
export SAVED_ECF_RID="$ECF_RID"

# Tell ecFlow we have started
# POST_OUT variable enables LSF to communicate with ecFlow
if [ -d /opt/modules ]; then
    # WCOSS TO4 (Cray XC40)
    . /opt/modules/default/init/sh
    POST_OUT=${POST_OUT:-/gpfs/hps/tmpfs/ecflow/ecflow_post_in.$LSB_BATCH_JID}
    module load ecflow
elif [ -d /usrx/local/Modules ]; then
    # WCOSS Phase 1 & 2 (IBM iDataPlex)
    . /usrx/local/Modules/default/init/sh
    POST_OUT=/var/lsf/ecflow_post_in.$LSB_BATCH_JID
    module load ecflow
else
    # WCOSS Phase 3 (Dell PowerEdge)
    # The next line is temporary - it should be made default on Mars and Venus in the near future!
    . /usrx/local/prod/lmod/lmod/init/sh
    module load ips/18.0.1.163 ecflow/4.7.1
    POST_OUT=${POST_OUT:-/var/lsf/ecflow_post_in.$LSB_BATCH_JID}
fi

# On Dell P3, the dev ecflow module clobbers the ECF_* variables so we
# have to set them again here:

export ECF_NAME=%ECF_NAME%
#export ECF_HOST=%ECF_HOST%
export ECF_HOST=%ECF_LOGHOST%
export ECF_PORT=%ECF_PORT%
export ECF_PASS=%ECF_PASS%
export ECF_TRYNO=%ECF_TRYNO%
export ECF_RID=${LSB_JOBID:-$$}

ecflow_client --init=${ECF_RID}

cat > $POST_OUT <<ENDFILE
ECF_NAME=${ECF_NAME}
ECF_HOST=${ECF_HOST}
ECF_PORT=${ECF_PORT}
ECF_PASS=${ECF_PASS}
ECF_TRYNO=${ECF_TRYNO}
ECF_RID=${ECF_RID}
ENDFILE

# Define error handler
ERROR() {
  set +exu
  echo Reached head.h error handler with \$1=$1 1>&2
  if [ "$1" -lt 0 ]; then
     msg="Killed by signal $((-$1))"
  else
     msg="Abort: exited with status $1"
  fi
  export ECF_NAME=%ECF_NAME%
  #export ECF_HOST=%ECF_HOST%
  export ECF_HOST=%ECF_LOGHOST%
  export ECF_PORT=%ECF_PORT%
  export ECF_PASS=%ECF_PASS%
  export ECF_TRYNO=%ECF_TRYNO%
  export ECF_RID=$SAVED_ECF_RID
  ecflow_client --abort="$msg"
  echo $msg
  echo "Trap Caught" >>$POST_OUT
  trap 0 # restore original EXIT handler
  exit $1
}
# Trap all error and exit signals
trap 'ERROR $?' ERR   # trap non-zero exit status of all commands
trap 'ERROR $?' EXIT  # trap exit of this shell process
trap 'ERROR -3' 3     # SIGQUIT
trap 'ERROR -4' 4     # SIGINT
trap 'ERROR -15' 15   # SIGTERM
trap 'ERROR -16' 16   # SIGUSR1
trap 'ERROR -17' 17   # SIGUSR2

