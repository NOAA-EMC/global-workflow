set __ms_shell=csh

eval "if ( -d / ) set __ms_shell=tcsh"

if ( { test -d /lfs/f1 } ) then
    # We are on NOAA Cactus or Dogwood
    if ( ! { module help >& /dev/null } ) then
        source /usr/share/lmod/lmod/init/$__ms_shell
    fi
    module reset
else if ( { test -d /lfs3 } ) then
    if ( ! { module help >& /dev/null } ) then
            source /apps/lmod/lmod/init/$__ms_shell
    endif
    module purge
else if ( { test -d /scratch1 } ) then
    # We are on NOAA Hera
    if ( ! { module help >& /dev/null } ) then
        source /apps/lmod/lmod/init/$__ms_shell
    endif
    module purge
else if ( { test -d /work } ) then
    # We are on MSU Orion
    if ( ! { module help >& /dev/null } ) then
        source /apps/lmod/init/$__ms_shell
    endif
    module purge
else if ( { test -d /data/prod } ) then
    # We are on SSEC S4
    if ( ! { module help >& /dev/null } ) then
        source /usr/share/lmod/lmod/init/$__ms_shell
    endif
    source /etc/profile
    module purge
else if ( { test -d /glade } ) then
    # We are on NCAR Yellowstone
    if ( ! { module help >& /dev/null } ) then
        source /usr/share/Modules/init/$__ms_shell
    endif
    module purge
else if ( { test -d /lustre -a -d /ncrc } ) then
    # We are on GAEA.
    if ( ! { module help >& /dev/null } ) then
        # We cannot simply load the module command.  The GAEA
        # /etc/csh.login modifies a number of module-related variables
        # before loading the module command.  Without those variables,
        # the module command fails.  Hence we actually have to source
        # /etc/csh.login here.
        source /etc/csh.login
        set __ms_source_etc_csh_login=yes
    else
        set __ms_source_etc_csh_login=no
    endif
    module purge
    unsetenv _LMFILES_
    unsetenv _LMFILES_000
    unsetenv _LMFILES_001
    unsetenv LOADEDMODULES
    module load modules
    if ( { test -d /opt/cray/ari/modulefiles } ) then
        module use -a /opt/cray/ari/modulefiles
    endif
    if ( { test -d /opt/cray/pe/ari/modulefiles } ) then
        module use -a /opt/cray/pe/ari/modulefiles
    endif
    if ( { test -d /opt/cray/pe/craype/default/modulefiles } ) then
        module use -a /opt/cray/pe/craype/default/modulefiles
    endif
    setenv NCEPLIBS /lustre/f1/pdata/ncep_shared/NCEPLIBS/lib
    if ( { test -d /lustre/f1/pdata/ncep_shared/NCEPLIBS/lib } ) then
      module use $NCEPLIBS/modulefiles
    endif
    if ( "$__ms_source_etc_csh_login" == yes ) then
      source /etc/csh.login
      unset __ms_source_etc_csh_login
    endif
else
    # Workaround for csh limitation.  Use sh to print to stderr.
    sh -c 'echo WARNING: UNKNOWN PLATFORM 1>&2'
endif
