#### ecflow module work around
#module load ecflow/4.7.1
export ECF_NAME=%ECF_NAME%
export ECF_PORT=%ECF_PORT%
#export ECF_PORT=31867
export ECF_PASS=%ECF_PASS%
export ECF_TRYNO=%ECF_TRYNO%
export ECF_RID=$LSB_JOBID
/gpfs/dell1/nco/ops/nwprod/ecflow.v4.7.1.0/intel/bin/ecflow_client --complete  # Notify ecFlow of a normal end
#ecflow_client --complete  # Notify ecFlow of a normal end
trap 0                    # Remove all traps
exit 0                    # End the shell
