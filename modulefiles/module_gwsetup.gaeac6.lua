help([[
Load environment to run GFS workflow setup scripts on Gaea C6
]])

prepend_path("MODULEPATH", "/ncrc/proj/epic/rocoto/modulefiles")
load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
local python_ver=os.getenv("python_ver") or "3.10.13"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
load("git-lfs")

whatis("Description: GFS run setup environment")
