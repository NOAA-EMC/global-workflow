help([[
Load environment to run GFS workflow setup scripts on container
]])

-- load("rocoto")

prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.9.2/envs/unified-env/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.0"
local stack_mpi_ver=os.getenv("stack_mpi_ver") or "2021.13" 
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_mpi_ver))
load(pathJoin("cmake", cmake_ver))

load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
