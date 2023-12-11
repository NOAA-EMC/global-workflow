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

if [[ -d /lfs/f1 ]] ; then
    # We are on NOAA Cactus or Dogwood
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /usr/share/lmod/lmod/init/$__ms_shell
    fi
    module reset
elif [[ -d /mnt/lfs1 ]] ; then
    # We are on NOAA Jet
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    module purge
elif [[ -d /scratch1 ]] ; then
    # We are on NOAA Hera
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    module purge
elif [[ -d /work ]] ; then
    # We are on MSU Orion or Hercules
    if [[ -d /apps/other ]] ; then
        # Hercules
        init_path="/apps/other/lmod/lmod/init/$__ms_shell"
    else
        # Orion
        init_path="/apps/lmod/lmod/init/$__ms_shell"
    fi
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source "${init_path}"
    fi
    module purge
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        . /usr/share/Modules/init/$__ms_shell
    fi
    module purge
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA.
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        # We cannot simply load the module command.  The GAEA
        # /etc/profile modifies a number of module-related variables
        # before loading the module command.  Without those variables,
        # the module command fails.  Hence we actually have to source
        # /etc/profile here.
        source /etc/profile
        __ms_source_etc_profile=yes
    else
        __ms_source_etc_profile=no
    fi
    module purge
    # clean up after purge
    unset _LMFILES_
    unset _LMFILES_000
    unset _LMFILES_001
    unset LOADEDMODULES
    module load modules
    if [[ -d /opt/cray/ari/modulefiles ]] ; then
        module use -a /opt/cray/ari/modulefiles
    fi
    if [[ -d /opt/cray/pe/ari/modulefiles ]] ; then
        module use -a /opt/cray/pe/ari/modulefiles
    fi
    if [[ -d /opt/cray/pe/craype/default/modulefiles ]] ; then
        module use -a /opt/cray/pe/craype/default/modulefiles
    fi
    if [[ -s /etc/opt/cray/pe/admin-pe/site-config ]] ; then
        source /etc/opt/cray/pe/admin-pe/site-config
    fi
    export NCEPLIBS=/lustre/f1/pdata/ncep_shared/NCEPLIBS/lib
    if [[ -d "$NCEPLIBS" ]] ; then
        module use $NCEPLIBS/modulefiles
    fi
    if [[ "$__ms_source_etc_profile" == yes ]] ; then
      source /etc/profile
      unset __ms_source_etc_profile
    fi
elif [[ -d /data/prod ]] ; then
    # We are on SSEC's S4
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /usr/share/lmod/lmod/init/$__ms_shell
    fi
    module purge
else
    echo WARNING: UNKNOWN PLATFORM 1>&2
fi

unset __ms_shell
unset __ms_ksh_test
unset __ms_bash_test
unset $__ms_function_name
unset __ms_function_name
