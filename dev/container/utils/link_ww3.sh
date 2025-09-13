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
    -t|--type)
      type="$2"
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

if [[ ! -v HOMEgfs || ! -v container || ! -v type ]]; then
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image \\"
        "                     -b/--bindings -B dirname [-B dirname1 [...]] -t/--type [gfs|sfs|gefs] [-v]"
   exit -1
fi

if [[ "$verbose" == "true" ]]; then
   set -x
fi

#if [[ "$type" == "gfs" ]]; then
   pdlib=pdlib_ON
#else
#   pdlib=pdlib_OFF
#fi

for nm in gint grib grid ounf ounp outf outp prep prnc
do
   model=ww3_${nm}
  #echo "model: $model"

   run_model_script=${HOMEgfs}/ush/container/run_${type}_${model}.sh
   rm -f ${run_model_script}

   cat > $run_model_script << EOF_MODEL
#!/bin/bash

# Set OMP_NUM_THREADS to 1 to avoid oversubscription when doing MPMD
export OMP_NUM_THREADS=1

source /usr/lmod/lmod/init/bash
module purge
module use ${HOMEgfs}/sorc/gfs_utils.fd/modulefiles
module load gfsutils_container.intel

arg="\$@"
${HOMEgfs}/sorc/ufs_model.fd/WW3/install/${pdlib}/bin/${model} \$arg
EOF_MODEL

   chmod 755 $run_model_script

   link_model_script=${HOMEgfs}/exec/${type}_${model}.x
   rm -f ${link_model_script}

   cat > $link_model_script << EOF_LINK
#!/bin/bash
 export LD_LIBRARY_PATH=$(dirname $container)
 arg="\$@"
 singularity exec ${bindings} ${container} ${run_model_script} \$arg
EOF_LINK

   chmod 755 $link_model_script
done

