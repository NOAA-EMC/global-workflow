help([[
Load environment to run GFS workflow setup scripts on Hera
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.5.1/envs/gsi-addon/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local python_ver=os.getenv("python_ver") or "3.10.8"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
local git_ver=os.getenv("git_ver") or "2.40.0"
load(pathJoin("git", git_ver))

whatis("Description: GFS run setup environment")
