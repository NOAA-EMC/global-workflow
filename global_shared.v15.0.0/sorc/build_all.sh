#!/bin/sh
set -eux

target=${1:-cray}    ;#wcoss, cray or theia

#--link large fixed fields for respective machines
if [ -d ../fix ]; then rm -f ../fix; fi
if [ $target = cray -o $target = wcoss ]; then
 ln -fs /gpfs/hps/emc/global/noscrub/emc.glopara/svn/fv3gfs/fix ../fix
elif [ $target = theia ]; then
 ln -fs /scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/fix    ../fix
else
 echo " machine $target is not supported. exit"
 exit
fi

pwd=`pwd`
#for script in build_gsi.sh build_gsm.sh build_radmon.sh build_nems_util.sh build_chgres.sh build_orog.sh; do
for script in  build_nems_util.sh build_chgres.sh build_orog.sh; do
 cd $pwd
 sh $script $target
done

#sh build_emcsfc.sh
sh build_tropcy_NEMS_$target.sh
#sh build_ncep_post.sh

cd $pwd/fre-nctools.fd
./BUILD_TOOLS.csh $target

for util in fv3nc2nemsio.fd regrid_nemsio.fd; do
 cd $pwd/$util
 ./makefile.sh $target
done

exit
