help([[
Load environment to run GFS workflow setup scripts on Hera
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/Alexander.Richert/spack-stack-1.4.1-gw/envs/gw/install/modulefiles/Core")
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/jcsda/jedipara/spack-stack/modulefiles")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-python", stack_python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
