#!/bin/sh

if [[ "$1" != cray  &&  "$1" != theia ]] ; then
    echo 'Syntax: build_all.sh ( cray | theia )'
    exit 1
fi
target="$1"
fv3target="$target"

set -eux

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

if [[ "$target" == cray ]] ; then
    fv3target=wcoss_cray
fi

rc=0
for x in fv3gfs.fd gsi.fd ; do
    if [[ ! -d $x ]] ; then
        echo "$x: missing.  Did you run checkout.sh?"
        rc=$((rc+1))
    fi
done
[[ $rc -ne 0 ]] && exit $rc

nems=fv3gfs.fd/NEMS

# Initialize environment for module command and purge modules:
setup=$( pwd -P )/../modulefiles/module-setup.sh.inc
test -s $setup
source $setup

# Add our modulefiles:
module use $( pwd -P )/../modulefiles
module load module_base.$target

pwd=`pwd`

sh build_fv3.sh $fv3target

#for script in build_gsi.sh build_radmon.sh build_nems_util.sh build_chgres.sh build_orog.sh; do
for script in build_gsi.sh build_nems_util.sh build_chgres.sh build_chgres_GSM.sh build_orog.sh; do
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
