#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

#
# Clean lib directory (libraries, modulefiles and incmod)
#
cd $cwd/lib
rm -rf libpng* libjasper*  lib*.a incmod/* modulefiles/*

#
# Optional install, first independent libraries:
#

# --- first choose BUFR version (different on different machine)
# --- hopefully this will lead to single version in the future
if [ $target = wcoss_cray ]; then
  bufr_ver=v11.0.1
elif [ $target = "theia" ]; then
  bufr_ver=v10.2.5
fi
# --------------------------------------------------------------

for lib in \
    bacio_v2.0.2         \
    bufr_${bufr_ver}     \
    crtm_v2.0.6          \
    g2tmpl_v1.3.0        \
    gfsio_v1.1.0         \
    ip_v2.0.0            \
    ip_v3.0.0            \
    jasper-1.900.1       \
    landsfcutil_v2.1.0   \
    nemsio_v2.2.3        \
    png-1.2.44           \
    sfcio_v1.0.0         \
    sigio_v2.0.1         \
    sp_v2.0.2            \
    w3nco_v2.0.6         \
    z-1.2.6
do
 cd $cwd/lib/sorc/$lib
 ./build.sh
 cd $cwd
done

#
# Optional install, now libraries depending on previously installed libs:
#
for lib in \
    w3emc_v2.2.0         \
    nemsiogfs_v2.0.1     \
    g2c_v1.5.0           \
    g2_v3.1.0
do
 cd $cwd/lib/sorc/$lib
 ./build.sh
 cd $cwd
done

exit
