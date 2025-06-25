help([[
Load environment to run GFS workflow setup scripts on Ursa
]])

load(pathJoin("rocoto"))

-- prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-rocky8/install/modulefiles/Core")

-- local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"

-- load(pathJoin("stack-intel", stack_intel_ver))

whatis("Description: GFS run setup environment")
