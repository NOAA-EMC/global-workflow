#!/bin/bash

verbose=false
bindings="-B /scratch3 -B /scratch4"

while [ "$#" -gt 0 ]; do
  case "$1" in
    -H|--HOMEgfs)
      HOMEgfs="$2"
      shift 2
      ;;
    -b|--bindings)
      bindings="$2"
      shift 2
      ;;
    -c|--container)
      container="$2"
      shift 2
      ;;
    -m|--model)
      model="$2"
      shift 2
      ;;
    -v|--verbose)
      verbose=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

if [[ ! -v HOMEgfs || ! -v container || ! -v model ]]; then
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image \\"
   echo "                     -m/--model name_model  -b/--bindings -B dirname [-B dirname1 [...]] [-v]"
   exit -1
fi

#echo "HOMEgfs: $HOMEgfs"
#echo "model: $model"
#echo "Verbose: $verbose"

if [[ "$verbose" == "true" ]]; then
   set -x
fi

run_model_script=${HOMEgfs}/ush/container/run_${model}.sh
rm -f ${run_model_script}

cat > $run_model_script << EOF_MODEL
#!/bin/bash

source "${HOMEgfs}/dev/ush/load_gw_run_modules.sh"

arg="\$@"
${HOMEgfs}/sorc/ufs_model.fd/tests/${model}.x \$arg
EOF_MODEL

chmod 755 $run_model_script

link_model_script=${HOMEgfs}/exec/${model}.x
rm -f ${link_model_script}

cat > $link_model_script << EOF_LINK
#!/bin/bash

# --- MPI and Fabric Configuration ---
# 1. Force Intel MPI to use Slurm's PMI2 library for job startup
# for Ursa
export I_MPI_PMI_LIBRARY=/apps/slurm/default/lib/libpmi2.so

# 2. Set the OFI provider to Mellanox InfiniBand
export FI_PROVIDER=mlx

# 3. Disable problematic shared memory transports in UCX
export UCX_TLS=^sm,cma
# --- End of Configuration ---

 export LD_LIBRARY_PATH=$(dirname ${container})
 set +x
 arg="\$@"
 singularity exec \\
 ${bindings} \\
 ${container} \\
 ${run_model_script} \$arg
EOF_LINK

chmod 755 $link_model_script

