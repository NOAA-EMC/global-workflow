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
   echo "Usage: link_model.sh -H/-HOMEgfs gw-home-dir -c/--container full-path-container-image -b/--bindings -B dirname [-B dirname1 [...]] [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   set -x
fi

exec_python_script="${HOMEgfs}"/exec/run_python.sh 

cat > "${exec_python_script}" << EOF_EXEC_PYTHON
#!/bin/bash
 LD_LIBRARY_PATH=\$(dirname "${container}")
 export LD_LIBRARY_PATH

 singularity exec \\
        ${bindings} \\
        ${container} \\
        ${HOMEgfs}/ush/container/run_python.sh "\$@"
EOF_EXEC_PYTHON

run_python_script="${HOMEgfs}"/ush/container/run_python.sh

cat > "${run_python_script}" << EOF_RUN_PYTHON
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

wxflowPATH=${HOMEgfs}/ush/python:${HOMEgfs}/sorc/wxflow/src
export PYTHONPATH=\${PYTHONPATH:+\${PYTHONPATH}:}${HOMEgfs}/ush:\${wxflowPATH}

python "\$@"
EOF_RUN_PYTHON

sed -i 's/RUN_WITH_CONTAINER=NO/RUN_WITH_CONTAINER=YES/g' "${HOMEgfs}/ush/preamble.sh"
chmod +x "${exec_python_script}"
chmod +x "${run_python_script}"
