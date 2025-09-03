#!/bin/bash

verbose=false

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
   exit -1
fi

#echo "HOMEgfs: $HOMEgfs"
#echo "container: $container"
#echo "bindings: $bindings"
#echo "Verbose: $verbose"

if [[ "$verbose" == "true" ]]; then
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

   run_model_script=${HOMEgfs}/ush/container/run_${model}.sh
   rm -f ${run_model_script}

   cat > $run_model_script << EOF_MODEL
#!/bin/bash

#source /usr/lmod/lmod/init/bash
#module purge
#module use ${HOMEgfs}/sorc/gfs_utils.fd/modulefiles
#module load gfsutils_container.intel

source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
module load wgrib2/3.6.0

arg="\$@"
${HOMEgfs}/sorc/gfs_utils.fd/install/bin/${model}.x \$arg
EOF_MODEL

   chmod 755 $run_model_script

  #link_model_script=${HOMEgfs}/exec/${model}
  #rm -f ${link_model_script}

   link_model_script=${HOMEgfs}/exec/${model}.x
   rm -f ${link_model_script}

   cat > $link_model_script << EOF_LINK
#!/bin/bash
 export LD_LIBRARY_PATH=$(dirname $container)
 arg="\$@"
 singularity exec ${bindings} ${container} ${run_model_script} \$arg
EOF_LINK

   chmod 755 $link_model_script
done

for nm in ocnicepost
do
   direct_model_script=${HOMEgfs}/exec/${nm}.x
   rm -f ${direct_model_script}

   cat > $direct_model_script << EOF_DIRECT
#!/bin/bash

source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
module load wgrib2/3.6.0

arg="\$@"
${HOMEgfs}/sorc/gfs_utils.fd/install/bin/${nm}.x \$arg
EOF_DIRECT

   chmod 755 $direct_model_script
done

