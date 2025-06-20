help([[
Load environment to run GFS workflow setup scripts in container
]])

--load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
local stack_mpi_ver=os.getenv("stack_mpi_ver") or "2021.9.0"

load("gnu")
load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_mpi_ver))
unload("gnu")

load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
