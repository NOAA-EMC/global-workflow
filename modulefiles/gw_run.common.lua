help([[
Load common modules for all systems
]])

local stack_compiler=(os.getenv("stack_compiler") or "None")
local stack_compiler_ver=(os.getenv("stack_compiler_ver") or "None")
local stack_mpi=(os.getenv("stack_mpi") or "None")
local stack_mpi_ver=(os.getenv("stack_mpi_ver") or "None")
local stack_core_path=(os.getenv("stack_core_path") or "None")
if stack_compiler == "None" or stack_compiler_ver == "None" or
   stack_mpi == "None" or stack_mpi_ver == "None" or
   stack_core_path == "None" then
    LmodError("FATAL ERROR One or more spack-stack variables is undefined!")
end

prepend_path("MODULEPATH", stack_core_path)

local stack_compiler_module=pathJoin("stack-" .. stack_compiler, stack_compiler_ver)
local stack_mpi_module=pathJoin("stack-" .. stack_mpi, stack_mpi_ver)

load(stack_compiler_module)
load(stack_mpi_module)

local common_modules = {
  "stack-python",
  "cmake",
  "jasper",
  "libpng",
  "cdo",
  "hdf5",
  "netcdf-c",
  "netcdf-fortran",
  "esmf",
  "nco",
  "prod_util",
  "grib-util",
  "g2tmpl",
  "gsi-ncdiag",
  "crtm",
  "bufr",
  --"wgrib2",  temporarily disable wgrib2 until it is installed with ipolates
  "py-f90nml",
  "py-netcdf4",
  "py-pyyaml",
  "py-jinja2",
  "py-pandas",
  "py-python-dateutil",
  "py-xarray",
  -- TODO: Reenable when MET/METplus and verif-global are at compatible versions
  -- "met",
  -- "metplus",
}

for _, name in pairs(common_modules) do
  -- Version variables have "_"s where the module name has "-"s
  local ver_name = string.gsub(name, "-", "_") .. "_ver"
  ver = os.getenv(ver_name) or "None"
  if ver == "None" then
    LmodError("FATAL ERROR version variable " .. ver_name .. " is not set!  Unable to load module!")
  end
  load(pathJoin(name, ver))
end

setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))
