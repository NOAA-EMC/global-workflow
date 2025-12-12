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
   echo "Usage: link_gfs_utils.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image -b/--bindings -B dirname [-B dirname1 [...]] [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   set -x
fi

for nm in enkf_chgres_recenter_nc ensadd ensppf ensstat fbwndgfs \
          gaussian_sfcanl gefs_6h_ave_1mem gfs_bufr \
          mkgfsawps ocnicepost overgridid reg2grb2 supvit \
          syndat_getjtbul syndat_maksynrc syndat_qctropcy \
          tave tocsbufr vint wave_stat webtitle rdbfmsua
do
   model=${nm}
   # echo "model: $model"

   run_gfs_script="${HOMEgfs}/exec/${model}.x"
   rm -f "${run_gfs_script}"

   cat > "${run_gfs_script}" << EOF_LINK
#!/bin/bash
 LD_LIBRARY_PATH=$(dirname "${container}")
 export LD_LIBRARY_PATH
 singularity exec "${bindings}" "${container}" "${run_model_script}" \\
             ${HOMEgfs}/dev/container/env/gfsutils-env.sh \\
             ${HOMEgfs}/sorc/gfs_utils.fd/install/bin/${model}.x "\$@"
EOF_LINK

   chmod 755 "${run_gfs_script}"
done

ocnicepost_script=${HOMEgfs}/exec/ocnicepost.x
rm -f "${ocnicepost_script}"

cat > "${ocnicepost_script}" << EOF_DIRECT
#!/bin/bash

${HOMEgfs}/dev/container/env/gfsutils-env.sh \\
${HOMEgfs}/sorc/gfs_utils.fd/install/bin/ocnicepost.x "\$@"
EOF_DIRECT

chmod 755 "${ocnicepost_script}"

