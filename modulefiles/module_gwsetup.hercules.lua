help([[
Load environment to run GFS workflow ci scripts on Hercules
]])

load(pathJoin("contrib","0.1"))
load(pathJoin("rocoto","1.3.5"))

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/hercules/spack-stack-1.5.1/envs/gsi-addon/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.9.0"
local python_ver=os.getenv("python_ver") or "3.10.8"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
