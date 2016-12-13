regtest=$1

case $regtest in

    global_T62)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:30:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="12/9/" ; ropts[2]="/2"
           sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="1:45:00" ; popts[1]="20/4" ; ropts[1]="/1"
           topts[2]="1:45:00" ; popts[2]="40/2" ; ropts[2]="/2"
           sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_T62_ozonly)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="12/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="12/3/" ; ropts[2]="/2"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/2"
            sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_4dvar_T62)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="12/5/" ; ropts[2]="/2"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:35:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
            sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:55:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:45:00" ; popts[2]="16/4/" ; ropts[2]="/2"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=5; scaling[2]=8; scaling[3]=2

    ;;

    global_hybrid_T126)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="12/5/" ; ropts[2]="/2"
           sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="20/4/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="40/4/" ; ropts[2]="/2"
           sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_lanczos_T62)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:20:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="12/5/" ; ropts[2]="/2"
           sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:20:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:30:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    global_nemsio_T62)

        if [[ "$machine" = "Theia" ]]; then
           topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="12/9/" ; ropts[2]="/2"
           sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
           topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
           sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    arw_binary | arw_netcdf)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="4/4/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="6/6/"  ; ropts[2]="/1"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/1/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/2/" ; ropts[2]="/1"
            sub_cmd="sub_wcoss -a RDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
           topts[1]="0:25:00" ; popts[1]="16/1/" ; ropts[1]="/1"
           topts[2]="0:25:00" ; popts[2]="16/2/" ; ropts[2]="/1"
           sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=4; scaling[2]=10; scaling[3]=4

    ;;

    nmm_binary )

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:30:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/12/" ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/12/" ; ropts[2]="/2"
            sub_cmd="sub_wcoss -a RDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:50:00" ; popts[1]="7/12/" ; ropts[1]="/1"
            topts[2]="0:50:00" ; popts[2]="9/12/" ; ropts[2]="/2"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=8; scaling[2]=10; scaling[3]=8

    ;;

    nmm_netcdf)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="4/2/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="4/4/"  ; ropts[2]="/1"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/1/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/1/" ; ropts[2]="/2"
            sub_cmd="sub_wcoss -a RDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:25:00" ; popts[1]="8/1/"  ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="16/1/" ; ropts[2]="/2"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=5; scaling[2]=10; scaling[3]=2

;;

    nmmb_nems_4denvar)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/10/"  ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/10/"  ; ropts[2]="/1"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:30:00" ; popts[1]="7/10/" ; ropts[1]="/1"
            topts[2]="0:30:00" ; popts[2]="9/10/" ; ropts[2]="/2"
            sub_cmd="sub_wcoss -a RDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:50:00" ; popts[1]="7/10/" ; ropts[1]="/1"
            topts[2]="0:50:00" ; popts[2]="9/10/" ; ropts[2]="/2"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=8; scaling[2]=10; scaling[3]=8

;;

    rtma)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/6/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="8/6/"  ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_wcoss -a RTMA-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:25:00" ; popts[1]="8/6/"  ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=10; scaling[2]=10; scaling[3]=2

    ;;

    hwrf_nmm_d2 | hwrf_nmm_d3)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:20:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:20:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:20:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:20:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_wcoss -a HWRF-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:40:00" ; popts[1]="6/6/"  ; ropts[1]="/1"
            topts[2]="0:40:00" ; popts[2]="8/8/"  ; ropts[2]="/1"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=5; scaling[2]=10; scaling[3]=2

    ;;

    global_enkf_T62)

        if [[ "$machine" = "Theia" ]]; then
            topts[1]="0:15:00" ; popts[1]="12/3/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="12/5/" ; ropts[2]="/2"
            sub_cmd="sub_zeus"
        elif [[ "$machine" = "WCOSS" ]]; then
            topts[1]="0:15:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:15:00" ; popts[2]="16/4/" ; ropts[2]="/2"
            sub_cmd="sub_wcoss -a GDAS-T2O"
        elif [[ "$machine" = "s4" ]]; then
            topts[1]="0:25:00" ; popts[1]="16/2/" ; ropts[1]="/1"
            topts[2]="0:25:00" ; popts[2]="16/4/" ; ropts[2]="/2"
            sub_cmd="sub_s4"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:00:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=2

    ;;

    *) # EXIT out for unresolved regtest

        echo "unknown $regtest"
        exit 1

esac

job[1]=${regtest}_loproc_updat
job[2]=${regtest}_hiproc_updat
job[3]=${regtest}_loproc_contrl
job[4]=${regtest}_hiproc_contrl

topts[3]=${topts[1]} ; popts[3]=${popts[1]} ; ropts[3]=${ropts[1]}
topts[4]=${topts[2]} ; popts[4]=${popts[2]} ; ropts[4]=${ropts[2]}

tmpregdir="tmpreg_$regtest"
rcname="return_code_${regtest}.out"
result="${regtest}_regression_results.txt"

export sub_cmd
export job
export topts
export popts
export ropts
export rcname
#export ptmp 
#export tmpdir
export tmpregdir
export result
export scaling
echo "HEYYYY this is ptmp $ptmp"
#export machine
#export basedir
#export builddir
#export gsisrc
#export gsiexec_updat
#export enkfexec_updat
#export gsiexec_contrl
#export enkfexec_contrl

if [[ "$machine" = "Theia" ]]; then
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export APRUN="mpirun -v -np \$PBS_NP"
elif [[ "$machine" = "WCOSS" ]]; then
   export MP_USE_BULK_XFER=yes
   export MP_BULK_MIN_MSG_SIZE=64k
   export APRUN="mpirun.lsf"
elif [[ "$machine" = "s4" ]]; then
   export APRUN="srun"
   export MPI_BUFS_PER_PROC=2048
   export MPI_BUFS_PER_HOST=2048
   export MPI_GROUP_MAX=256
   export MPI_MEMMAP_OFF=1
   export MP_STDOUTMODE=ORDERED
   export KMP_STACKSIZE=512MB 
   export KMP_AFFINITY=scatter
fi
