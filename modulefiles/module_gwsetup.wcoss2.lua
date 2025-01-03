help([[
Load environment to run GFS workflow ci scripts on WCOSS2
]])

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles/core")
prepend_path("MODULEPATH", "/apps/ops/test/spack-stack-1.6.0-nco/envs/nco-intel-19.1.3.304/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "19.1.3.304"
local stack_python_ver=os.getenv("stack_python_ver") or "3.10.13"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

load(pathJoin("rocoto","1.3.5"))

whatis("Description: GFS run setup environment")
