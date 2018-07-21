check_ecf_variables() {
    if ( ! which ecflow_client > /dev/null 2>&1 ) ; then
        echo "ERROR: There is no ecflow_client in your \$PATH.  Load the ecflow module."
        exit 1
    fi
    
    if [[ "${ECF_ROOT:-Q}" == Q ]] ; then
        echo "ERROR: You need to set \$ECF_ROOT"
        exit 1
    fi
    
    if [[ "${ECF_HOME:-Q}" == Q ]] ; then
        echo "ERROR: You need to set \$ECF_HOME.  I suggest \$ECF_ROOT/submit"
        exit 1
    fi
    
    if [[ "${check_ecf_host:-YES}" == YES ]] ; then
        if [[ "${ECF_HOST:-Q}" == Q ]] ; then
	    echo "ERROR: You need to set \$ECF_HOST."
	    exit 1
        fi
    fi
    
    if [[ "${ECF_PORT:-Q}" == Q ]] ; then
        echo "ERROR: You need to set \$ECF_PORT.  See /usrx/local/sys/ecflow/assigned_ports.txt"
        exit 1
    fi
    
    export ECF_HOME="${ECF_HOME:-$ECF_ROOT/submit}"
}

maybe_verbose_source() {
    if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
	echo "$1: source"
	source "$1"
    else
	source "$1" > /dev/null 2>&1
    fi
}

find_python36() {
    if ( ! which python3 > /dev/null 2>&1 || \
	     ! python3 -c 'import yaml ; f"{1+1}"' > /dev/null 2>&1 ) ; then
	python36="/dev/null/python"
	if [[ ! -x "$python36" && -d /usrx ]] ; then
	    if ( readlink /usrx | grep dell > /dev/null ) ; then
		module use /gpfs/dell2/emc/modeling/noscrub/emc.nemspara/soft/modulefiles
		module load python/intel-3.6.2-emc
		python36="$( which python3.6 )"
	    fi
	fi
	if [[ ! -x "$python36" && -s /etc/SuSE-release ]] ; then
	    python36=/gpfs/hps3/emc/nems/noscrub/Samuel.Trahan/python/3.6.1-emc/bin/python3.6
	fi
        if [[ ! -x "$python36" && -d /scratch4 ]] ; then
            module load intelpython
            python36="$( which python3.6 )"
        fi
        if [[ ! -x "$python36" && -d /lfs3 ]] ; then
            module use /lfs3/projects/hwrf-vd/soft/modulefiles/
            module load python/intel-3.6.2-emc
            python36="$( which python3.6 )"
        fi
        if [[ ! -x "$python36" && -d /lustre/f1 ]] ; then
            module use /lustre/f1/unswept/ncep/Samuel.Trahan/python/modulefiles/
            module load python/intel-3.6.2-emc
            python36="$( which python3.6 )"
        fi
    else
	python36="$( which python3 )"
    fi
    if [[ ! -x "$python36" ]] ; then
	echo "ERROR: cannot find python 3.6" 1>&2
	exit 1
    fi
}
