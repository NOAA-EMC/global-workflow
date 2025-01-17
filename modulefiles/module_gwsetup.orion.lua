help([[
Load environment to run GFS workflow ci scripts on Orion
]])

prepend_path("MODULEPATH", "/apps/modulefiles/core")
load(pathJoin("contrib","0.1"))
load(pathJoin("rocoto","1.3.7"))

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/gsi-addon-env-rocky9/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.9.0"
local python_ver=os.getenv("python_ver") or "3.11.6"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
load(pathJoin("cmake", cmake_ver))

whatis("Description: GFS run setup environment")
