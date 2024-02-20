help([[
Load environment to run GFS workflow setup scripts on Gaea C5
]])

whatis([===[Loads libraries needed for building the UFS Weather Model on Gaea C5 ]===])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/modulefiles")
load(pathJoin("rocoto","1.3.5"))

local intel_classic_ver=os.getenv("intel_classic_ver") or "2023.1.0"
load(pathJoin("intel-classic", intel_classic_ver))

local cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.25"
load(pathJoin("cray-mpich", cray_mpich_ver))

local python_ver=os.getenv("python_ver") or "3.9.12" 
load(pathJoin("python", python_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

local ecflow_ver=os.getenv("ecflow_ver") or "5.8.4"
load(pathJoin("ecflow", ecflow_ver))

local mysql_ver=os.getenv("mysql_ver") or "8.0.31"
load(pathJoin("mysql", mysql_ver))

local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.1.0"
load(pathJoin("stack-intel", stack_intel_ver))

local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.25"
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))

local stack_python_ver=os.getenv("stack_python_ver") or "3.10.13" 
load(pathJoin("stack-python", stack_python_ver))

local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
load(pathJoin("cmake", cmake_ver))

load("ufs_common")
load("nccmp/1.9.0.1")

unload("darshan-runtime")
unload("cray-libsci")

setenv("CC","cc")
setenv("CXX","CC")
setenv("FC","ftn")
setenv("CMAKE_Platform","gaea-c5.intel")
