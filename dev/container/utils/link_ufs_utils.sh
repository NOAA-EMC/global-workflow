#!/bin/bash

verbose=false

while [[ "$#" -gt 0 ]]; do
  case "$1" in
    -H|--HOMEgfs)
      HOMEgfs="$2"
      shift 2
      ;;
    -b|--binding)
      binding="$2"
      shift 2
      ;;
    -c|--container)
      container="$2"
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

if [[ ! -v HOMEgfs || ! -v container ]]; then
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image -b/--bindings -B dirname [-B dirname1 [...]] [-v]"
   exit 11
fi

if [[ "$verbose" == "true" ]]; then
   set -x
fi

for nm in emcsfc_ice_blend emcsfc_snow2mdl fregrid global_cycle regridStates.x
do
   model=${nm}

   run_model_script=${HOMEgfs}/ush/container/run_${model}.sh
   rm -f ${run_model_script}

   cat > $run_model_script << EOF_MODEL
#!/bin/bash

source /usr/lmod/lmod/init/bash
module purge
module use ${HOMEgfs}/sorc/ufs_utils.fd/modulefiles
module load build.container.intel

arg="\$@"
${HOMEgfs}/sorc/ufs_utils.fd/exec/${model} \$arg
EOF_MODEL

   chmod 755 $run_model_script

  #link_model_script=${HOMEgfs}/exec/${model}
  #rm -f ${link_model_script}

   link_model_script=${HOMEgfs}/exec/${model}
   rm -f ${link_model_script}

   cat > $link_model_script << EOF_LINK
#!/bin/bash
 export LD_LIBRARY_PATH=$(dirname $container)
 arg="\$@"
 singularity exec ${bindings} ${container} ${run_model_script} \$arg
EOF_LINK

   chmod 755 $link_model_script
done

