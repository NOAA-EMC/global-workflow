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

# Create a test function for sh vs. bash detection.  The name is
# randomly generated to reduce the chances of name collision.
__ms_function_name="setup__test_function__$$"
eval "$__ms_function_name() { /bin/true ; }"

# Determine which shell we are using
__ms_ksh_test=$( eval '__text="text" ; if [[ $__text =~ ^(t).* ]] ; then printf "%s" ${.sh.match[1]} ; fi' 2> /dev/null | cat )
__ms_bash_test=$( eval 'if ( set | grep '$__ms_function_name' | grep -v name > /dev/null 2>&1 ) ; then echo t ; fi ' 2> /dev/null | cat )

if [[ ! -z "$__ms_ksh_test" ]] ; then
    __ms_shell=ksh
elif [[ ! -z "$__ms_bash_test" ]] ; then
    __ms_shell=bash
else
    # Not bash or ksh, so assume sh.
    __ms_shell=sh
fi

target=""
USERNAME=`echo $LOGNAME | awk '{ print tolower($0)'}`

if [[ -d /lfs3 ]] ; then
    # We are on NOAA Jet
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    target=jet
    machine=$target
    module purge
elif [[ -d /lustre && -d /ncrc ]] ; then # for GAEA. 
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        # We cannot simply load the module command.  The GAEA
        # /etc/profile modifies a number of module-related variables
        # before loading the module command.  Without those variables,
        # the module command fails.  Hence we actually have to source
        # /etc/profile here.
	echo load the module command 1>&2
        source /etc/profile
    fi

    machine=gaea
    echo "machine set to: $machine"

    module purge
    unset _LMFILES_
    unset _LMFILES_000
    unset _LMFILES_001
    unset LOADEDMODULES
    module use -a /opt/cray/ari/modulefiles
    module use -a /opt/cray/pe/ari/modulefiles
    module use -a /opt/cray/pe/craype/default/modulefiles
    source /etc/opt/cray/pe/admin-pe/site-config
    export NCEPLIBS=/lustre/f1/pdata/ncep_shared/NCEPLIBS/lib
    echo NCEPLIBS HARD SET to  $NCEPLIBS in `pwd`/module_setup.sh.inc
    module use $NCEPLIBS/modulefiles

elif [ $mac2 = tf ] ; then                       # For Theia
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

if [ ! -d "../../exec" ] ; then
  mkdir -p ../../exec
fi
cp ncep_post ../../exec/

unset __ms_shell
unset __ms_ksh_test
unset __ms_bash_test
unset $__ms_function_name
unset __ms_function_name
