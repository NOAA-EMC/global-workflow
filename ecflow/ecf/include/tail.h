export ECF_NAME=%ECF_NAME%
#export ECF_HOST=%ECF_HOST%
export ECF_HOST=%ECF_LOGHOST%
export ECF_PORT=%ECF_PORT%
export ECF_PASS=%ECF_PASS%
export ECF_TRYNO=%ECF_TRYNO%
export ECF_RID=$SAVED_ECF_RID
ecflow_client --complete  # Notify ecFlow of a normal end
trap 0                    # Remove exit trap
exit 0                    # End the shell
