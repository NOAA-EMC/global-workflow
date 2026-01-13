#! /usr/bin/env bash
set -x
set -e

###############################################################
## atmosphere post (daily and monthly mean) driver script
###############################################################

source "${HOMEgfs}/ush/detect_machine.sh" || exit 1
if [[ "${MACHINE_ID}" == "wcoss2" ]]; then

    module load craype-x86-rome
    module load libfabric/1.20.1
    module load craype-network-ofi
    module load envvar/1.0

    module load PrgEnv-intel/8.3.3
    module load intel/19.1.3.304
    module load craype/2.7.17
    module load cray-mpich/8.1.19

    module use /apps/ops/para/libs/modulefiles/compiler/intel/19.1.3.304/
    module use /apps/ops/para/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.19

    module load jasper
    module load zlib
    module load libpng

    module load libjpeg-turbo/2.1.0

    module load libaec
    module load g2c/2.3.0
    module load netcdf-D/4.9.2
    module load sp
    module load ip/5.2.0
    module load wgrib2/3.8.0

    module list

    export GMERGE=gmerge
    export WGRIB2=wgrib2

elif [[ "${MACHINE_ID}" == "ursa" ]]; then
# use wgrib2/3.6.0 + exectuable gmerge
#    source "${HOMEgfs}/dev/ush/load_modules.sh" run || exit 1
#    export GMERGE=/scratch4/NCEPDEV/ovp/Karina.Asmar/wgrib2/aux_progs/gmerge
#    export WGRIB2=wgrib2

# use wgrib2/3.8.0 (which includes gmerge already)
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
    source "${HOMEgfs}/dev/ush/load_modules.sh" run || exit 1
    export GMERGE=gmerge
    export WGRIB2=wgrib2
fi

status=$?
if [[ ${status} -ne 0 ]]; then
   exit "${status}";
fi

export job="atmos_post"

# shellcheck disable=SC2153
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/jobs/JGLOBAL_ATMOS_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
