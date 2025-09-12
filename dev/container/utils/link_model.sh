#!/bin/bash

verbose=false
bindings="-B /scratch3 -B /scratch4"
machineid="ursa"

while [[ "$#" -gt 0 ]]; do
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
    -M|--MACHINE_ID)
      machineid="$2"
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

if [[ ! -v HOMEgfs || ! -v container || ! -v model || ! -v MACHINE_ID ]]; then
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image \\"
   echo "                     -m/--model name_model -M/MACHINE_ID MACHINE_ID -b/--bindings [...]] [-v]"
   exit 11
fi

#echo "HOMEgfs: $HOMEgfs"
#echo "model: $model"
#echo "Verbose: $verbose"
#echo "machineid: $machineid"

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

link_model_script=${HOMEgfs}/exec/${model}.x
rm -f ${link_model_script}

case "${machineid}" in
  ursa)
cat > $link_model_script << EOF_URSA
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

HOST_SLURM_PATH=/apps/slurm/default
HOST_MPI_PATH=/apps/spack-2024-12/linux-rocky9-x86_64/gcc-11.4.1/intel-oneapi-compilers-2024.2.1-oqhstbmawnrsdw472p4pjsopj547o6xs/compiler/2024.2/opt/compiler

 export LD_LIBRARY_PATH=$(dirname ${container})
 set +x
 arg="\$@"
 singularity exec \\
    --bind \${HOST_SLURM_PATH}:\${HOST_SLURM_PATH} \\
    --bind \${HOST_MPI_PATH}:\${HOST_MPI_PATH} \\
    ${bindings} \\
    ${container} \\
    ${run_model_script} \$arg
EOF_URSA
    ;;

  gaea*)
cat > $link_model_script << EOF_GAEA
#!/bin/bash
#export SINGULARITY_ENABLE_OVERLAY=try
#export SINGULARITY_DISABLE_OVERLAY=yes
#export SINGULARITY_DEBUG=10
#export SINGULARITY_DEBUG=0
#unset SINGULARITY_DEBUG

 export LD_LIBRARY_PATH=$(dirname ${container})
 set +x
 arg="\$@"
 singularity exec \\
    ${bindings} \\
    ${container} \\
    ${run_model_script} \$arg
EOF_GAEA
    ;;

  noaacloud)
cat > $link_model_script << EOF_NOAACLOUD
#!/bin/bash

#Need these lines on AWS to run more than one node.
#export I_MPI_DEBUG=10
 export I_MPI_FABRICS=shm:ofi
 export I_MPI_OFI_PROVIDER=tcp
 export FI_PROVIDER=tcp
 export FI_TCP_IFACE=eth0

 export LD_LIBRARY_PATH=$(dirname ${container})
 set +x
 arg="\$@"
 singularity exec \\
    ${bindings} \\
    ${container} \\
    ${run_model_script} \$arg
EOF_NOAACLOUD
    ;;

  *)
cat > $link_model_script << EOF_LINK
#!/bin/bash
 export LD_LIBRARY_PATH=$(dirname ${container})
 set +x
 arg="\$@"
 singularity exec \\
    ${bindings} \\
    ${container} \\
    ${run_model_script} \$arg
EOF_LINK
    ;;

esac

chmod 755 $run_model_script
chmod 755 $link_model_script

