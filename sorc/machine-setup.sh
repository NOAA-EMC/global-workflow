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
##---------------------------------------------------------------------------
export hname=`hostname | cut -c 1,1`
if [[ -d /scratch1 ]] ; then
if [  $hname == 'h'  ] ; then
    # We are on NOAA Hera
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    target=hera
    module purge
    module load intel
    module load impi
    export NCEPLIBS=/scratch2/NCEPDEV/nwprod/NCEPLIBS
    module use $NCEPLIBS/modulefiles
    #export WRFPATH=$NCEPLIBS/wrf.shared.new/v1.1.1/src
    export myFC=mpiifort
    export FCOMP=mpiifort

##---------------------------------------------------------------------------
elif [  $hname == 't'  ] ; then
    # We are on NOAA Theia
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    target=theia
    module purge
    module use /scratch3/NCEPDEV/nwprod/modulefiles/
    module use /scratch3/NCEPDEV/nwprod/lib/modulefiles
fi #scratch1

##---------------------------------------------------------------------------
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

##---------------------------------------------------------------------------
elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then
    # We are on NOAA Venus or Mars
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
	source /usrx/local/prod/lmod/lmod/init/$__ms_shell
    fi
    target=wcoss_dell_p3
    module purge 

##---------------------------------------------------------------------------

elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        source /usrx/local/Modules/default/init/$__ms_shell
    fi
    target=wcoss
    module purge

##---------------------------------------------------------------------------
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
    if ( ! eval module help > /dev/null 2>&1 ) ; then
	echo load the module command 1>&2
        . /usr/share/Modules/init/$__ms_shell
    fi
    target=yellowstone
    module purge

##---------------------------------------------------------------------------
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA. 
   # We are on GAEA.
    echo gaea
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

target=gaea

# GWV ADD
module load craype
module load intel
export NCEPLIBS=/lustre/f2/dev/ncep/George.Vandenberghe/NEWCOPY/l508/lib/
module use $NCEPLIBS/modulefiles
export myFC=ftn      
export WRFPATH=$NCEPLIBS/wrf.shared.new/v1.1.1/src                                                    
export FCOMP=ftn
# END GWV ADD

##---------------------------------------------------------------------------
elif [[ -d /lfs3 ]] ; then
    # We are on NOAA Jet
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        echo load the module command 1>&2
        source /apps/lmod/lmod/init/$__ms_shell
    fi
    target=jet
    module purge
module load intel/15.0.3.187
module load  impi
#export  NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/gwv/ljtjet/lib
     export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/gwv/ljtjet/lib
export NCEPLIBS=/mnt/lfs3/projects/hfv3gfs/gwv/NCEPLIBS.15X
     module use $NCEPLIBS/modulefiles
export WRFPATH=$NCEPLIBS/wrf.shared.new/v1.1.1/src
export myFC=mpiifort

##---------------------------------------------------------------------------
elif [[ -d /apps/prod ]]; then

	# We are on WCOSS2/Acorn
    target=acorn

    module purge
    source /apps/prod/lmodules/startLmod
    #module use -a /apps/prod/modules
    # For gefs_nemsio2nc
    #module use /lfs/h1/ops/prod/libs/modulefiles/stack
    #module load hpc
    #module load hpc-intel
    #module load hpc-cray-mpich
    module load envvar/1.0

else
    echo WARNING: UNKNOWN PLATFORM 1>&2
fi

unset __ms_shell
unset __ms_ksh_test
unset __ms_bash_test
unset $__ms_function_name
unset __ms_function_name
