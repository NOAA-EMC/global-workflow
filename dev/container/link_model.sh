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

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1

#source /opt/spack-stack/spack-stack-1.6.0/envs/unified-env/install/intel/2021.10.0/intel-oneapi-mpi-2021.9.0-6bnjcwc/setvars.sh --force
#export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2023.2.3/linux/compiler/lib/intel64_lin
#export LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2024.0/lib:$LD_LIBRARY_PATH

source /usr/lmod/lmod/init/bash
module purge
module use ${HOMEgfs}/sorc/ufs_model.fd/modulefiles
module load ufs_container.intel

arg="\$@"
${HOMEgfs}/sorc/ufs_model.fd/tests/${model}.x \$arg
EOF_MODEL

chmod 755 $run_model_script

link_model_script=${HOMEgfs}/exec/${model}.x
rm -f ${link_model_script}

cat > $link_model_script << EOF_LINK
#!/bin/bash

 export LD_LIBRARY_PATH=$(dirname ${container})
 arg="\$@"
 singularity exec \\
 ${bindings} \\
 ${container} \\
 ${run_model_script} \$arg
EOF_LINK

chmod 755 $link_model_script

