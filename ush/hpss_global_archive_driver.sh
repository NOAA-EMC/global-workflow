set -x

pslot=${PSLOT:-""}
export JOB_LOG_DIR=${ARCH_LIST:-""}
export SOURCE_DIR=${ROTDIR:-""}
export ARCH_LIST=${ARCH_LIST:-""}
export HPSS_TARGET_DIR=${ATARDIR:-""}

echo "Dynamically create an archive job to archive files in $TRANSFER_TARGET_FILE"

python $HOMEgfs/ush/hpss_global_archive.py
