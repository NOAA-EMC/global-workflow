SHELL=/bin/sh

####################################################################################################
#
# post using module compile standard
#
# 10/15 Lin Gan:        Create module load version
# 01/16 Lin Gan:        Update to use GFS Vertical Structure
# 07/16 J. Carley:      Generalize for other machines using modules
#
#####################################################################################################
#####################################################################################################


# Lin Gan Module Load
module purge

set -x
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)
if [ $mac2 = tf ] ; then                       # For Theia
 machine=theia
 . /etc/profile
 . /etc/profile.d/modules.sh
elif [ $mac2 = t1 -o  $mac2 = g1 ] ; then # For WCOSS
 machine=wcoss
 . /usrx/local/Modules/default/init/bash
elif [ $mac = l -o $mac = s ] ; then             #    wcoss_c (i.e. luna and surge)
 export machine=cray-intel
elif [ $mac2 = ga ] ; then
export machine=gaea

    unset _LMFILES_
    unset _LMFILES_000
    unset _LMFILES_001
    unset LOADEDMODULES
    module use -a /opt/cray/ari/modulefiles
    module use -a /opt/cray/pe/ari/modulefiles
    module use -a /opt/cray/pe/craype/default/modulefiles
    source /etc/opt/cray/pe/admin-pe/site-config

export NCEPLIBS=/lustre/f1/pdata/ncep_shared/NCEPLIBS/lib
elif [ $mac = f  ] ; then             # Jet
export NCEPLIBS=/mnt/lfs3/projects/hwrfv3/gwv/lj/lib
#export NCEPLIBS=/lfs3/projects/hfv3gfs/gwv/lj/lib/
 export machine=jet
fi

# Lin Gan modifiy to use NCO vertical structure prefix for NCO deployment - 20160131
moduledir=`dirname $(readlink -f ../modulefiles/post)`
module use ${moduledir}
module load post/v7.0.0-${machine}
module list

cd ncep_post.fd
module list
make -f makefile_module clean
make -f makefile_module 

if [ ! -d "../../exec" ] ; then
  mkdir -p ../../exec
fi
cp ncep_post ../../exec/
