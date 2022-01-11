set -xe  # print commands as they are executed and enable signal trapping

export PS4='+ $SECONDS + ' 

# Variables needed for communication with ecFlow
export ECF_NAME=%ECF_NAME%
export ECF_HOST=%ECF_LOGHOST%
export ECF_PORT=%ECF_PORT%
export ECF_PASS=%ECF_PASS%
export ECF_TRYNO=%ECF_TRYNO%
export ECF_RID=${ECF_RID:-${PBS_JOBID:-$$}}
export ECF_JOB=%ECF_JOB%
export ECF_JOBOUT=%ECF_JOBOUT%
export ecflow_ver=%ecflow_ver%

if [ -d /apps/ops/prod ]; then # On WCOSS2
  echo "Running 'module reset'"
  module reset
  module load envvar/1.0
  module load PrgEnv-intel/8.1.0
  module load craype/2.7.8
  module load intel/19.1.3.304
fi

export HOMEgfs=/lfs/h2/emc/global/noscrub/Lin.Gan/git/feature-ops-wcoss2
. ${HOMEgfs}/versions/run.ver
export gfs_ver=v16.2

if [ -d /apps/ops/prod ]; then # On WCOSS2
  export ECF_ROOT=/apps/ops/prod/nco/core/ecflow.v5.6.0.7
  . ${ECF_ROOT}/versions/run.ver
  module load prod_util/${prod_util_ver}
  module load prod_envir/${prod_envir_ver}

  echo "Running module load ecflow/$ecflow_ver"
  module load ecflow/$ecflow_ver
  echo "ecflow module location: $(module display ecflow |& head -2 | tail -1 | sed 's/:$//')"
  export ECF_ROOT=/apps/ops/prod/nco/core/ecflow.v5.6.0.7
  export ECF_PORT=34326
  export ECF_HOST=ddecflow02
  export ECF_INCLUDE=/lfs/h2/emc/global/noscrub/Lin.Gan/git/feature-ops-wcoss2/ecf/include
  export ECF_HOME=/lfs/h2/emc/global/noscrub/Lin.Gan/ecflow/submit
  export ECF_DATA_ROOT=/lfs/h2/emc/global/noscrub/Lin.Gan/ecflow
  export ECF_OUTPUTDIR=/lfs/h2/emc/global/noscrub/Lin.Gan/ecflow/output
  export ECF_COMDIR=/lfs/h2/emc/global/noscrub/Lin.Gan/ecflow/submit
  export ECF_COMDIR=/lfs/h2/emc/ptmp/Lin.Gan/ecflow/submit
  ecflow_client --alter change variable ECF_INCLUDE /lfs/h2/emc/global/noscrub/Lin.Gan/git/feature-ops-wcoss2/ecf/include /

  echo "Listing modules from head.h:"
  module list
fi

timeout 300 ecflow_client --init=${ECF_RID}

POST_OUT=/lfs/h2/emc/stmp/Lin.Gan/RUNDIRS/ecfops/tmp/posts/ecflow_post_in.$USER.${PBS_JOBID}
mkdir -p /lfs/h2/emc/stmp/Lin.Gan/RUNDIRS/ecfops/tmp/posts
echo 'export ECF_NAME=${ECF_NAME}' > $POST_OUT
echo 'export ECF_HOST=${ECF_HOST}' >> $POST_OUT
echo 'export ECF_PORT=${ECF_PORT}' >> $POST_OUT
echo 'export ECF_PASS=${ECF_PASS}' >> $POST_OUT
echo 'export ECF_TRYNO=${ECF_TRYNO}' >> $POST_OUT
echo 'export ECF_RID=${ECF_RID}' >> $POST_OUT

# Define error handler
ERROR() {
  set +ex
  if [ "$1" -eq 0 ]; then
     msg="Killed by signal (likely via qdel)"
  else
     msg="Killed by signal $1"
  fi
  ecflow_client --abort="$msg"
  echo $msg
  echo "Trap Caught" >>$POST_OUT
  trap $1; exit $1
}
# Trap all error and exit signals
trap 'ERROR $?' ERR EXIT

