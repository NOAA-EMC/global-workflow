#! /usr/bin/env bash
set -x
set -e

###############################################################
## atmosphere post (daily and monthly mean) driver script
###############################################################

# Source FV3GFS workflow modules
if ((status != 0)); then exit "${status}"; fi
source "${HOMEgfs}/ush/detect_machine.sh" || exit 1
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?

if [[ "${MACHINE_ID}" == "ursa" ]]; then
    module unload stack-oneapi
    module use /scratch4/NCEPDEV/nems/Richard.Grubin/spack-stack/envs/ue-oneapi-2025.2.1-wgrib2-3.8.0/modules/Core
    module load stack-intel-oneapi-compilers/2025.2.1
    module load stack-intel-oneapi-mpi/2021.13
    module load wgrib2/3.8.0
    export GMERGE=gmerge
    export WGRIB2=wgrib2
elif [[ "${MACHINE_ID}" == "gaeac6" ]]; then
    source "${HOMEgfs}/dev/ush/load_modules.sh" run || exit 1
    export GMERGE=/gpfs/f6/sfs-emc/scratch/Karina.Asmar/wgrib2/aux_progs/gmerge
    export WGRIB2=wgrib2
else
    export GMERGE=gmerge
    export WGRIB2=wgrib2
fi

export job="atmos_post"
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/dev/jobs/JGLOBAL_ATMOS_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
