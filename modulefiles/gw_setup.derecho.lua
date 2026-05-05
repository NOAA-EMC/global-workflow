help([[
Load environment to run GFS workflow setup scripts on Derecho
]])

setenv("LMOD_TMOD_FIND_FIRST","yes")
prepend_path("MODULEPATH", "/lustre/desc1/scratch/epicufsrt/contrib/modulefiles_extra")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/cray-mpich/8.1.29-3sepg3g/gcc/12.4.0")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/derecho/modulefiles")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local stack_impi_ver=os.getenv("stack_cray_mpich_ver") or "8.1.29"
local python_ver=os.getenv("python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"
local rocoto_ver=os.getenv("rocoto_ver") or "1.3.7"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("stack-python", python_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("rocoto", rocoto_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")
load("py-netcdf4")

whatis("Description: GFS run setup environment")
