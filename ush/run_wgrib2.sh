#!/bin/bash
#img=/scratch2/NAGAPE/epic/Wei.Huang/src/gw-container-spack-stack-1.6.0/gw-container.sif
#img=/scratch2/NAGAPE/epic/Wei.Huang/src/gw-container-spack-stack-1.6.0/gw-container
 img=/contrib/Wei.Huang/src/gw-container-spack-stack-1.6.0/wei-gw-container
 cmd=/opt/global-workflow-cloud/ush/run_wgrib2.sh
 arg="$@"
#echo running: singularity exec "${img}" $cmd $arg
 singularity exec ${img} $cmd $arg

#module reset
#unset MACHINE_ID
#export HOMEgfs=/scratch2/NAGAPE/epic/Wei.Huang/src/global-workflow-cloud
# Find module command and purge:
#source "${HOMEgfs}/ush/detect_machine.sh"
#source "${HOMEgfs}/ush/module-setup.sh"

# Source versions file for runtime
#source "${HOMEgfs}/versions/run.ver"

# Load our modules:
#module use "${HOMEgfs}/modulefiles"

#case "${MACHINE_ID}" in
#  "wcoss2" | "hera" | "orion" | "hercules" | "gaeac5" | "gaeac6" | "jet" | "s4" | "noaacloud" | "container")
#    module load "module_base.${MACHINE_ID}"
#    ;;
#  *)
#    echo "WARNING: UNKNOWN PLATFORM"
#    ;;
#esac

#module load prod_util
