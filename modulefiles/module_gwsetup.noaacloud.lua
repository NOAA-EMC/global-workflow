help([[
Load environment to run GFS workflow setup scripts on noaacloud
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.6.0/envs/ue-intel/install/modulefiles/Core")

load("gnu")
local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
local stack_mpi_ver=os.getenv("stack_mpi_ver") or "2021.10.0"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_mpi_ver))
unload("gnu")

load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
