help([[
Load environment to run GFS workflow setup scripts on noaacloud
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.3.0"
local python_ver=os.getenv("python_ver") or "3.10.3"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
local git_ver=os.getenv("git_ver") or "1.8.3.1"
load(pathJoin("git", git_ver))

whatis("Description: GFS run setup environment")
