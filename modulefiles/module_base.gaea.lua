help([[
Load environment to run GFS workflow setup scripts on Hera
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/modulefiles")
prepend_path("MODULEPATH", "/ncrc/proj/epic/rocoto/modulefiles")
load(pathJoin("rocoto","1.3.6"))

local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.1.0"
local python_ver=os.getenv("python_ver") or "3.9.12"

load(pathJoin("stack-intel", stack_intel_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

whatis("Description: GFS run setup environment")
