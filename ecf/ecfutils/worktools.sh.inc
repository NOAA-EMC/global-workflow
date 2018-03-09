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
	     ! python3 -c 'import yaml ; f{"1+1"}' > /dev/null 2>&1 ) ; then
	python36=/gpfs/hps3/emc/nems/noscrub/Samuel.Trahan/python/3.6.1-emc/bin/python3.6
        if [[ ! -x "$python36" ]] ; then
            module load intelpython/3.6.1.0
            python36="$( which python3 )"
        fi
    else
	python36="$( which python3 )"
    fi
}

make_yaml_files_in_expdir() {
    if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
	set -x
    fi

    # NOTE: Sourcing config.base clobbers the ecflow variables, so we
    # must do it in a subshell.
    set +ue
    maybe_verbose_source "$CONFIGDIR"/config.base 
    set -ue

    if [[ "${WORKTOOLS_VERBOSE:-NO}" == YES ]] ; then
	set -x
    fi

    echo "EXPDIR=\"$EXPDIR\"" > "$tmpfile"

    mkdir -p "$EXPDIR"/logs
    
    set +ue
    ( maybe_verbose_source "$CONFIGDIR"/config.earc ;
      echo "export NMEM_EARCGRP=\"$NMEM_EARCGRP\"" >> "$tmpfile" )
    ( maybe_verbose_source "$CONFIGDIR"/config.efcs ;
      echo "export NMEM_EFCSGRP=\"$NMEM_EFCSGRP\"" >> "$tmpfile" ;
      echo "export ENKF_layout_x=\"$layout_x\"" >> "$tmpfile" ;
      echo "export ENKF_layout_y=\"$layout_y\"" >> "$tmpfile" ;
      echo "export ENKF_WRITE_GROUP=\"$WRITE_GROUP\"" >> "$tmpfile" ;
      echo "export ENKF_WRTTASK_PER_GROUP=\"$WRTTASK_PER_GROUP\"" >> "$tmpfile" )
    ( maybe_verbose_source "$CONFIGDIR"/config.eobs ;
      echo "export NMEM_EOMGGRP=\"$NMEM_EOMGGRP\"" >> "$tmpfile" )
    ( maybe_verbose_source "$CONFIGDIR"/config.fcst ;
      echo "export layout_x=\"$layout_x\"" >> "$tmpfile" ;
      echo "export layout_y=\"$layout_y\"" >> "$tmpfile" ;
      echo "export WRITE_GROUP=\"$WRITE_GROUP\"" >> "$tmpfile" ;
      echo "export WRTTASK_PER_GROUP=\"$WRTTASK_PER_GROUP\"" >> "$tmpfile" )
    set -ue

    source "$tmpfile"

    $python36 -c "import worktools ; worktools.make_yaml_files_in_expdir('$here','$EXPDIR')"
}
