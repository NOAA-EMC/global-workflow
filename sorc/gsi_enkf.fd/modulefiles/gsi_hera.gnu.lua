help([[
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev/install/modulefiles/Core")
--Needed for openmpi build
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/jcsda/jedipara/spack-stack/modulefiles")

local python_ver=os.getenv("python_ver") or "3.11.6"
local stack_gnu_ver=os.getenv("stack_gnu_ver") or "9.2.0"
local stack_openmpi_ver=os.getenv("stack_openmpi_ver") or "4.1.5"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"
local openblas_ver=os.getenv("openblas_ver") or "0.3.24"

load(pathJoin("stack-gcc", stack_gnu_ver))
load(pathJoin("stack-openmpi", stack_openmpi_ver))
load(pathJoin("python", python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

load(pathJoin("prod_util", prod_util_ver))
load(pathJoin("openblas", openblas_ver))

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch1/NCEPDEV/global/glopara/fix/gsi/20241022")

whatis("Description: GSI environment on Hera with GNU Compilers")
