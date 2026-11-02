module load ecflow 2> /dev/null || true
export ECF_HOST="${_ECF_HOST_SAVED}"
export ECF_PORT="${_ECF_PORT_SAVED}"
timeout 300 ecflow_client --complete
trap 0
exit 0
