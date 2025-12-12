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

if [[ ! -v HOMEgfs || ! -v container || ! -v MACHINE_ID ]]; then
   echo "Usage: link_gsi.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image \\"
   echo "                   -M/MACHINE_ID MACHINE_ID -b/--bindings [...]] [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   set -x
fi

for model in oznmon_horiz oznmon_time radmon_angle radmon_bcoef radmon_bcor radmon_time
do
  gsi_exec_script=${HOMEgfs}/exec/${model}.x
  rm -f "${gsi_exec_script}"

case "${machineid}" in
  ursa)
cat > "${gsi_exec_script}" << EOF_URSA
#!/bin/bash

# --- MPI and Fabric Configuration ---
# 1. Force Intel MPI to use Slurm's PMI2 library for job startup
# for Ursa

 export I_MPI_PMI_LIBRARY=/apps/slurm/default/lib/libpmi2.so
 export I_MPI_FABRICS=shm:ofi
 export I_MPI_OFI_PROVIDER=tcp
 export FI_PROVIDER=tcp
 export FI_TCP_IFACE=eth0

 HOST_SLURM_PATH=/apps/slurm/default
 HOST_MPI_PATH=/apps/spack-2024-12/linux-rocky9-x86_64/gcc-11.4.1/intel-oneapi-compilers-2024.2.1-oqhstbmawnrsdw472p4pjsopj547o6xs/compiler/2024.2/opt/compiler

 LD_LIBRARY_PATH=$(dirname "${container}")
 export LD_LIBRARY_PATH
 singularity exec \\
    --bind \${HOST_SLURM_PATH}:\${HOST_SLURM_PATH} \\
    --bind \${HOST_MPI_PATH}:\${HOST_MPI_PATH} \\
    -B /apps/slurm/default/lib/libpmi2.so \\
    ${bindings} \\
    ${container} \\
    ${HOMEgfs}/dev/container/env/gsimonitor-env.sh \\
    ${HOMEgfs}/sorc/gsi_monitor.fd/install/bin/${model}.x \\
    "\$@"
EOF_URSA
    ;;

  gaea*)
cat > "${gsi_exec_script}" << EOF_GAEA
#!/bin/bash
#export SINGULARITY_ENABLE_OVERLAY=try
#export SINGULARITY_DISABLE_OVERLAY=yes
#export SINGULARITY_DEBUG=10
#export SINGULARITY_DEBUG=0
#unset SINGULARITY_DEBUG

 LD_LIBRARY_PATH=$(dirname "${container}")
 export LD_LIBRARY_PATH
 set +x
 singularity exec \\
    ${bindings} \\
    ${container} \\
    ${HOMEgfs}/dev/container/env/gsimonitor-env.sh \\
    ${HOMEgfs}/sorc/gsi_monitor.fd/install/bin/${model}.x \\
    "\$@"
EOF_GAEA
    ;;

  noaacloud)
cat > "${gsi_exec_script}" << EOF_NOAACLOUD
#!/bin/bash

#Need these lines on AWS to run more than one node.
#export I_MPI_DEBUG=10
 export I_MPI_FABRICS=shm:ofi
 export I_MPI_OFI_PROVIDER=tcp
 export FI_PROVIDER=tcp
 export FI_TCP_IFACE=eth0

 LD_LIBRARY_PATH=$(dirname "${container}")
 export LD_LIBRARY_PATH
 set +x
 singularity exec \\
    ${bindings} \\
    ${container} \\
    ${HOMEgfs}/dev/container/env/gsimonitor-env.sh \\
    ${HOMEgfs}/sorc/gsi_monitor.fd/install/bin/${model}.x \\
    "\$@"
EOF_NOAACLOUD
    ;;

  *)
cat > "${gsi_exec_script}" << EOF_LINK
#!/bin/bash
 LD_LIBRARY_PATH=$(dirname "${container}")
 export LD_LIBRARY_PATH
 set +x
 singularity exec \\
    ${bindings} \\
    ${container} \\
    ${HOMEgfs}/dev/container/env/gsimonitor-env.sh \\
    ${HOMEgfs}/sorc/gsi_monitor.fd/install/bin/${model}.x \\
    "\$@"
EOF_LINK
    ;;

esac

  chmod 755 "${gsi_exec_script}"

done

