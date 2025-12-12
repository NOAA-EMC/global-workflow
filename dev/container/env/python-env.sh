#!/bin/bash

source /usr/lmod/lmod/init/bash
module purge
module use "${HOMEgfs}"/sorc/gfs_utils.fd/modulefiles
module load gfsutils_container.intel
module load python
module load py-netcdf4
module load py-xarray
module load py-f90nml
module load py-numpy
module load py-jinja2
module load py-pyyaml

xflowPATH=${HOMEgfs}/ush:${HOMEgfs}/ush/python:${HOMEgfs}/sorc/wxflow/src
export PYTHONPATH=\${PYTHONPATH:+\${PYTHONPATH}:}${wxflowPATH}

python "$@"
