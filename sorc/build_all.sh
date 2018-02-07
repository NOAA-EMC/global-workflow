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

for script in build_nems_util.sh build_cycle.sh build_chgres.sh build_enkf_chgres_recenter.sh build_orog.sh build_radmon.sh build_oznmon.sh ;do
 cd $pwd
 sh $script $target
done

#sh build_emcsfc.sh
sh build_tropcy_NEMS.sh $target

cd $pwd/fre-nctools.fd
./BUILD_TOOLS.csh $target

for util in fv3nc2nemsio.fd regrid_nemsio.fd; do
 cd $pwd/$util
 ./makefile.sh $target
done

for script in build_gdas_gridbull.sh build_gdas_navybull.sh build_gdas_trpsfcmv.sh; do
 cd $pwd
 sh $script $target
done


cd $pwd
rc=0
for x in fv3gfs.fd gsi.fd gfs_post.fd; do
    if [[ ! -d $x ]] ; then
        echo "$x: missing.  Did you run checkout.sh?"
        rc=$((rc+1))
    fi
done
[[ $rc -ne 0 ]] && exit $rc

nems=fv3gfs.fd/NEMS
sh build_fv3.sh $fv3target
cd $pwd
sh build_gsi.sh $target

cd $pwd/gfs_post.fd/sorc
sh build_ncep_post.sh


cd $pwd/../exec
[[ -s fv3_gfs_nh.prod.32bit.x ]] && rm -f fv3_gfs_nh.prod.32bit.x
cp -p ../sorc/fv3gfs.fd/NEMS/exe/fv3_gfs_nh.prod.32bit.x .
for gsiexe in  adderrspec_nmcmeth_spec.x  calc_increment_ens.x  getsfcensmeanp.x  getsigensmeanp_smooth.x  global_enkf  gribmean.x calc_increment.x  getnstensmeanp.x  getsfcnstensupdp.x  getsigensstatp.x  global_gsi  recentersigp.x ;do
    [[ -s $gsiexe ]] && rm -f $gsiexe
    cp -p ../sorc/gsi.fd/exec/$gsiexe .
done
[[ -s gfs_ncep_post ]] && rm -f gfs_ncep_post
cp -p ../sorc/gfs_post.fd/exec/ncep_post gfs_ncep_post


cd $pwd
if [ $target = theia ]; then
 for script in build_tocsbufr_bufr_flux_theia.sh build_gfs_cnvgrib21_gfs.sh_theia; do
  sh $script
 done
 for script in build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh; do
  sh $script $target
 done
elif [ $target = cray ]; then
 for script in build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh build_gfs_cnvgrib21_gfs.sh; do
  sh $script $target
 done
 for script in build_gfs_bufrsnd.sh; do
  sh $script
 done
fi




exit 0
