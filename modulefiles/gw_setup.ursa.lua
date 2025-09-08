help([[
Load environment to run GFS workflow setup scripts on Ursa
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local python_ver=os.getenv("python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-python", python_ver))
load(pathJoin("cmake", cmake_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
load("gh")

whatis("Description: GFS run setup environment")
