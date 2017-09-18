set __ms_shell=csh

eval "if ( -d / ) set __ms_shell=tcsh"

if ( { test -d /lfs3 } ) then 
    if ( ! { module help >& /dev/null } ) then
            source /apps/lmod/lmod/init/$__ms_shell
    endif
    module purge
else if ( { test -d /scratch3 } ) then
    # We are on NOAA Theia
    if ( ! { module help >& /dev/null } ) then
        source /apps/lmod/lmod/init/$__ms_shell
    endif
    module purge
else if ( { test -d /gpfs/hps -a -e /etc/SuSE-release } ) then
    # We are on NOAA Luna or Surge
    if ( ! { module help >& /dev/null } ) then
	source /opt/modules/default/init/$__ms_shell
    endif
    module purge
    module purge
    unset _LMFILES_
    unset LOADEDMODULES
    module use /opt/modulefiles
    module use /opt/cray/ari/modulefiles
    module use /opt/cray/craype/default/alt-modulefiles
    module use /opt/cray/alt-modulefiles
    module use /gpfs/hps/nco/ops/nwprod/modulefiles
    module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
    module use /usrx/local/prod/modulefiles
else if ( { test -d /dcom -a -d /hwrf } ) then
    # We are on NOAA Tide or Gyre
    if ( ! { module help >& /dev/null } ) then
        source /usrx/local/Modules/default/init/$__ms_shell
    endif
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
    endif
    module purge
else
    # Workaround for csh limitation.  Use sh to print to stderr.
    sh -c 'echo WARNING: UNKNOWN PLATFORM 1>&2'
endif
