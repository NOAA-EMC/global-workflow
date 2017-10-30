#!/bin/sh

if [[ "$1" != cray  &&  "$1" != theia ]] ; then
    echo 'Syntax: build_all.sh ( cray | theia )'
    exit 1
fi
target="$1"
fv3target="$target"

set -eux

pwd=$(pwd -P)

#--link large fixed fields for respective machines
$pwd/../ush/link_fv3gfs_fix.sh $target $pwd/../
rc=$?
[[ $rc != 0 ]] && exit $rc

[[ "$target" == cray ]] && fv3target=wcoss_cray

# Initialize environment for module command and purge modules:
setup=$pwd/../modulefiles/module-setup.sh.inc
test -s $setup
source $setup

# Add our modulefiles:
module use $( pwd -P )/../modulefiles
module load module_base.$target

for script in build_nems_util.sh build_cycle.sh build_chgres.sh build_chgres_GSM.sh build_orog.sh build_radmon.sh; do
 cd $pwd
 sh $script $target
done

#sh build_emcsfc.sh
sh build_tropcy_NEMS.sh $target
#sh build_ncep_post.sh
cd $pwd/fre-nctools.fd
./BUILD_TOOLS.csh $target

for util in fv3nc2nemsio.fd regrid_nemsio.fd; do
 cd $pwd/$util
 ./makefile.sh $target
done

cd $pwd

rc=0
for x in fv3gfs.fd gsi.fd ; do
    if [[ ! -d $x ]] ; then
        echo "$x: missing.  Did you run checkout.sh?"
        rc=$((rc+1))
    fi
done
[[ $rc -ne 0 ]] && exit $rc

nems=fv3gfs.fd/NEMS
cd $pwd
sh build_fv3.sh $fv3target

cd $pwd
sh build_gsi.sh $target

exit 0
