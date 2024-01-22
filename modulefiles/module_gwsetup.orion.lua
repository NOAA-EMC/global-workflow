help([[
Load environment to run GFS workflow ci scripts on Orion
]])

prepend_path("MODULEPATH", "/apps/modulefiles/core")
load(pathJoin("contrib","0.1"))
load(pathJoin("rocoto","1.3.3"))
load(pathJoin("git","2.28.0"))

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2022.0.2"
local python_ver=os.getenv("python_ver") or "3.11.6"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
