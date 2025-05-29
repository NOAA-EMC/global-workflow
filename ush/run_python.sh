#!/usr/bin/env bash

export FPATH=/usr/lmod/lmod/libexec

export HOMEgfs=/opt/global-workflow-cloud
source /usr/lmod/lmod/init/bash
module purge
source ${HOMEgfs}/versions/run.ver
module use ${HOMEgfs}/modulefiles
module load module_base.container

#module load python/3.10.13
#module load py-f90nml/1.4.3
#module load py-netcdf4/1.5.8
#module load py-pyyaml/6.0
#module load py-jinja2/3.1.2
#module load py-pandas/1.5.3
#module load py-numpy/1.22.3
#module load py-xarray/2023.7.0
#module load py-python-dateutil/2.8.2

wxflowPATH="${HOMEgfs}/ush/python"
export PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush:${wxflowPATH}"

arg="$@"

/opt/spack-stack/spack-stack-1.6.0/envs/unified-env/install/intel/2021.10.0/python-3.10.13-h3oyipv/bin/python $arg

