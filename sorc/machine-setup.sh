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
    module purge
     export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/gwv/ljtjet/lib
     echo NCEPLIBS HARD SET to  $NCEPLIBS in `pwd`/module_setup.sh.inc
     module use $NCEPLIBS/modulefiles
elif [[ -d /scratch3 ]] ; then
    # We are on NOAA Theia
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    target=theia
    module purge
    module use /scratch3/NCEPDEV/nwprod/modulefiles/
    module use /scratch3/NCEPDEV/nwprod/lib/modulefiles
elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    # We are on NOAA Luna or Surge
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
	source /opt/modules/default/init/$__ms_shell
    fi
    target=wcoss_cray

    # Silence the "module purge" to avoid the expected error messages
    # related to modules that load modules.
    module purge > /dev/null 2>&1
    module use /usrx/local/prod/modulefiles
    module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
    module use /gpfs/hps/nco/ops/nwprod/modulefiles
    module use /opt/cray/alt-modulefiles
    module use /opt/cray/craype/default/alt-modulefiles
    module use /opt/cray/ari/modulefiles
    module use /opt/modulefiles
    module purge > /dev/null 2>&1

    # Workaround until module issues are fixed:
    #unset _LMFILES_
    #unset LOADEDMODULES
    echo y 2> /dev/null | module clear > /dev/null 2>&1

    module use /usrx/local/prod/modulefiles
    module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
    module use /gpfs/hps/nco/ops/nwprod/modulefiles
    module use /opt/cray/alt-modulefiles
    module use /opt/cray/craype/default/alt-modulefiles
    module use /opt/cray/ari/modulefiles
    module use /opt/modulefiles
    module load modules

elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then
    # We are on NOAA Venus or Mars
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
	source /usrx/local/prod/lmod/lmod/init/$__ms_shell
    fi
    target=wcoss_dell_p3
    module purge 

elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        source /usrx/local/Modules/default/init/$__ms_shell
    fi
    target=wcoss
    module purge
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        . /usr/share/Modules/init/$__ms_shell
    fi
    target=yellowstone
    module purge
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA. 
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        # We cannot simply load the module command.  The GAEA
        # /etc/profile modifies a number of module-related variables
        # before loading the module command.  Without those variables,
        # the module command fails.  Hence we actually have to source
        # /etc/profile here.
	echo load the module command 1>&2
        source /etc/profile
    fi
    target=gaea
    module purge
else
    echo WARNING: UNKNOWN PLATFORM 1>&2
fi

unset __ms_shell
unset __ms_ksh_test
unset __ms_bash_test
unset $__ms_function_name
unset __ms_function_name
