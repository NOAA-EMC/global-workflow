help([[
Load environment to run GFS workflow ci scripts on Orion
]])

prepend_path("MODULEPATH", "/apps/modulefiles/core")

-- prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.5.1/envs/gsi-addon/install/modulefiles/Core")
prepend_path("MODULEPATH", "/shared/spack/share/spack/modules/linux-amzn2-skylake")

load ("py-numpy-1.22.4-gcc-7.3.1-ylp7efj")
load ("py-pyyaml-6.0-gcc-7.3.1-z36arc4")
load ("py-jinja2-3.0.3-gcc-7.3.1-oegomip")

whatis("Description: GFS run setup environment")
