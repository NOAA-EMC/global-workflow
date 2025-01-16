#!/bin/bash
set -u

source "${HOMEgfs}/ush/detect_machine.sh"

if [[ ${MACHINE_ID} = jet* ]] ; then
    # We are on NOAA Jet
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/bash
    fi
    export LMOD_SYSTEM_DEFAULT_MODULES=contrib
    module reset

elif [[ ${MACHINE_ID} = hera* ]] ; then
    # We are on NOAA Hera
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/bash
    fi
    export LMOD_SYSTEM_DEFAULT_MODULES=contrib
    set +u
    module reset
    set -u

elif [[ ${MACHINE_ID} = hercules* ]] ; then
    # We are on Hercules
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/other/lmod/lmod/init/bash
    fi
    export LMOD_SYSTEM_DEFAULT_MODULES=contrib
    set +u
    module reset
    set -u

elif [[ ${MACHINE_ID} = orion* ]] ; then
    # We are on Orion
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /apps/lmod/lmod/init/bash
    fi
    #export LMOD_SYSTEM_DEFAULT_MODULES=git/2.28.0  # contrib has a lot of stuff we shouldn't put in MODULEPATH
    #set +u
    module purge # reset causes issues on Orion sometimes.
    #set -u

elif [[ ${MACHINE_ID} = s4* ]] ; then
    # We are on SSEC Wisconsin S4
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /usr/share/lmod/lmod/init/bash
    fi
    export LMOD_SYSTEM_DEFAULT_MODULES=license_intel
    module reset

elif [[ ${MACHINE_ID} = wcoss2 ]]; then
    # We are on WCOSS2
    # Ignore default modules of the same version lower in the search path (req'd by spack-stack)
    #export LMOD_TMOD_FIND_FIRST=yes #TODO: Uncomment this when using spack-stack
    module reset

elif [[ ${MACHINE_ID} = cheyenne* ]] ; then
    # We are on NCAR Cheyenne
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /glade/u/apps/ch/modulefiles/default/localinit/localinit.sh
    fi
    module purge

elif [[ ${MACHINE_ID} = stampede* ]] ; then
    # We are on TACC Stampede
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /opt/apps/lmod/lmod/init/bash
    fi
    module purge

elif [[ ${MACHINE_ID} = gaea* ]] ; then
    # We are on GAEA.
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /usr/share/lmod/lmod/init/bash
        source /etc/profile
    fi
    module reset

elif [[ ${MACHINE_ID} = expanse* ]]; then
    # We are on SDSC Expanse
    if ( ! eval module help > /dev/null 2>&1 ) ; then
        source /etc/profile.d/modules.sh
    fi
    module purge
    module load slurm/expanse/20.02.3

elif [[ ${MACHINE_ID} = discover* ]]; then
    # We are on NCCS discover
    export SPACK_ROOT=/discover/nobackup/mapotts1/spack
    export PATH=${PATH}:${SPACK_ROOT}/bin
    . "${SPACK_ROOT}"/share/spack/setup-env.sh

# TODO: This can likely be made more general once other cloud
# platforms come online.
elif [[ ${MACHINE_ID} = "noaacloud" ]]; then
    # We are on NOAA Cloud
    module purge

else
    echo WARNING: UNKNOWN PLATFORM 1>&2
fi

# If this function exists in the environment, run it; else do not
ftype=$(type -t set_strict || echo "")
if [[ "${ftype}" == "function" ]]; then
  set_strict
else
  set +u
fi
