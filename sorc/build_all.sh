#!/bin/sh

if [[ "$1" != cray  &&  "$1" != theia ]] ; then
    echo 'Syntax: build_all.sh ( cray | theia )'
    exit 1
fi
target="$1"
fv3target="$target"

set -eux

pwd=$(pwd -P)
[[ "$target" == cray ]] && fv3target=wcoss_cray

# Initialize environment for module command and purge modules:
setup=$pwd/../modulefiles/module-setup.sh.inc
test -s $setup
source $setup

# Add our modulefiles:
module use $( pwd -P )/../modulefiles
module load module_base.$target

# Build scripts, execute one-by-one
for script in build_nems_util.sh build_cycle.sh build_chgres.sh build_enkf_chgres_recenter.sh \
              build_gaussian_sfcanl.sh build_orog.sh build_radmon.sh build_oznmon.sh \
              build_tropcy_NEMS.sh \
              build_gdas_gridbull.sh build_gdas_navybull.sh build_gdas_trpsfcmv.sh \
              build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh; do
  cd $pwd
  sh $script $target
done

#sh build_emcsfc.sh

cd $pwd/fre-nctools.fd
./BUILD_TOOLS.csh $target

for util in fv3nc2nemsio.fd regrid_nemsio.fd; do
  cd $pwd/$util
  ./makefile.sh $target
done

cd $pwd
if [ $target = theia ]; then
#for script in build_tocsbufr_bufr_flux_theia.sh build_gfs_cnvgrib21_gfs.sh_theia; do
#  sh $script
#done
  sh build_gfs_cnvgrib21_gfs.sh_theia
elif [ $target = cray ]; then
  sh build_gfs_cnvgrib21_gfs.sh $target
  sh build_gfs_bufrsnd.sh
fi

# Now build external components:
cd $pwd
rc=0
for x in fv3gfs.fd gfs_post.fd gsi.fd; do
    if [[ ! -d $x ]] ; then
        echo "$x: missing.  Did you run checkout.sh?"
        rc=$((rc+1))
    fi
done
[[ $rc -ne 0 ]] && exit $rc

cd $pwd
sh build_fv3.sh $fv3target

cd $pwd/gfs_post.fd/sorc
sh build_ncep_post.sh

cd $pwd
sh build_gsi.sh $target


exit 0
