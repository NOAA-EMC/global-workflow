help([[
Load environment to run GFS workflow setup scripts on Gaea C6
]])

prepend_path("MODULEPATH", "/autofs/ncrc-svm1_proj/hurr1/hafs/shared/modulefiles")
load(pathJoin("rocoto", "1.3.7_fix"))

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-1.9.2/envs/ue-intel-2023.2.0/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
local python_ver=os.getenv("python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("cmake", cmake_ver))
load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
load("git-lfs")

unload("cray-libsci")

whatis("Description: GFS run setup environment")
