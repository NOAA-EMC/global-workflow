help([[
Load environment to run GFS workflow setup scripts on Hera
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-rocky8/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local python_ver=os.getenv("python_ver") or "3.11.6"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
local git_ver=os.getenv("git_ver") or "2.18.0"
load(pathJoin("git", git_ver))

whatis("Description: GFS run setup environment")
