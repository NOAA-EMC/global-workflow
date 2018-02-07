SHELL=/bin/sh

####################################################################################################
#
# post using module compile standard
#
# 10/15 Lin Gan:        Create module load version
# 01/16 Lin Gan:	Update to use GFS Vertical Structure
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
elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
 machine=wcoss
 . /usrx/local/Modules/default/init/bash
elif [ $mac = l -o $mac = s ] ; then             #    wcoss_c (i.e. luna and surge)
 export machine=cray-intel
fi

# Lin Gan modifiy to use NCO vertical structure prefix for NCO deployment - 20160131
moduledir=`dirname $(readlink -f ../modulefiles/post)`
module use ${moduledir}
module load post/v7.0.0-${machine}
module list

cd ncep_post.fd
make -f makefile_module clean
make -f makefile_module 

cp ncep_post ../../exec/
