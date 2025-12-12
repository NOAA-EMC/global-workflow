#!/bin/bash

verbose=false

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
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image -b/--bindings [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   set -x
fi

for nm in emcsfc_ice_blend emcsfc_snow2mdl fregrid global_cycle regridStates.x
do
   model=${nm}

   link_model_script=${HOMEgfs}/exec/${model}
   rm -f "${link_model_script}"

   cat > "${link_model_script}" << EOF_LINK
#!/bin/bash
 LD_LIBRARY_PATH=$(dirname "${container}")
 export LD_LIBRARY_PATH
 singularity exec ${bindings} ${container} ${run_model_script} "\$@"
             ${HOMEgfs}/dev/container/env/ufsutils-env.sh \\
             ${HOMEgfs}/sorc/ufs_utils.fd/exec/${model} "\$@"
EOF_LINK

   chmod 755 "${link_model_script}"
done

