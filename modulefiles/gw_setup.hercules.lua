help([[
Load environment to run GFS workflow ci scripts on Hercules
]])

load(pathJoin("contrib","0.1"))
load(pathJoin("rocoto","1.3.7"))

prepend_path("MODULEPATH", "/apps/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.1.0/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local python_ver=os.getenv("python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
try_load("globus-cli")
load("gh")

whatis("Description: GFS run setup environment")
