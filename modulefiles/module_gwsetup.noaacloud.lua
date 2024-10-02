help([[
Load environment to run GFS workflow setup scripts on noaacloud
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.6.0/envs/ue-intel/install/modulefiles/Core")
prepend_path("MODULEPATH", "/apps/modules/modulefiles")
load("gnu")
load("stack-intel")
load("stack-intel-oneapi-mpi")
unload("gnu")

load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
local git_ver=os.getenv("git_ver") or "1.8.3.1"
load(pathJoin("git", git_ver))

whatis("Description: GFS run setup environment")
