set -x

regtest=$1

case $machine in

    Hera)
           sub_cmd="sub_hera"
           memnode=96
           numcore=40
    ;;
    Orion)
           sub_cmd="sub_orion"
           memnode=192
           numcore=40
    ;;
    Hercules)
           sub_cmd="sub_hercules"
           memnode=512
           numcore=40
    ;;
    Jet)
           sub_cmd="sub_jet"
           memnode=96
           numcore=40
    ;;
    gaeac5)
	   sub_cmd="sub_gaeac5"
           memnode=251
           numcore=128
    ;;
    gaeac6)
	   sub_cmd="sub_gaeac6"
           memnode=384
           numcore=192
    ;;
    wcoss2)
           sub_cmd="sub_wcoss2"
           memnode=512
           numcore=128
    ;;
    acorn)
           sub_cmd="sub_acorn"
           memnode=512
           numcore=128
    ;;
    Discover)
       sub_cmd="sub_discover"
    ;;
    *) # EXIT out for unresolved machine
        echo "unknown $machine"
        exit 1

esac

# Maximum memory per task for above machines
# Select minimim memory per core for regression tests
export memnode=${memnode:-64}
export numcore=${numcore:-24}
export maxmem=$((($memnode*1024*1024)/$numcore))  # Kb / core

case $regtest in

    global_4denvar)

        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/12/" ; ropts[2]="/2"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/12/" ; ropts[2]="/2"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "Discover" ]]; then
           topts[1]="0:30:00" ; popts[1]="48/2"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="60/3"  ; ropts[2]="/2"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/10/" ; ropts[2]="/2"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/8/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/10/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="1:30:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    rrfs_3denvar_rdasens)

        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:05:00" ; popts[1]="40/3/"  ; ropts[1]="/1"
           topts[2]="0:05:00" ; popts[2]="40/5/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:15:00" ; popts[1]="20/6/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="20/12/" ; ropts[2]="/2"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:05:00" ; popts[1]="40/3/" ; ropts[1]="/1"
           topts[2]="0:05:00" ; popts[2]="40/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:15:00" ; popts[1]="40/3/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="40/5/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:15:00" ; popts[1]="40/3/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="40/5/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:15:00" ; popts[1]="40/3/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="40/5/"  ; ropts[2]="/1"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="128/2/" ; ropts[2]="/1"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=2; scaling[2]=10; scaling[3]=4

    ;;

    hafs_3denvar_hybens)

        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:15:00" ; popts[1]="5/4/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="10/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:15:00" ; popts[1]="5/4/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="10/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:15:00" ; popts[1]="20/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="5/8/" ; ropts[2]="/2"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:15:00" ; popts[1]="5/4/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="10/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:15:00" ; popts[1]="32/2/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="64/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="128/2/"  ; ropts[2]="/1"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="128/2/" ; ropts[2]="/1"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=2; scaling[2]=10; scaling[3]=4

    ;;

    hafs_4denvar_glbens)
        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:15:00" ; popts[1]="5/4/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="10/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:20:00" ; popts[1]="5/4/"  ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="20/2/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:20:00" ; popts[1]="5/4/"  ; ropts[1]="/1"
           topts[2]="0:20:00" ; popts[2]="10/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:15:00" ; popts[1]="5/4/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="10/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:15:00" ; popts[1]="32/2/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="64/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="128/2/"  ; ropts[2]="/1"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="128/2/" ; ropts[2]="/1"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:45:00"
        fi

        scaling[1]=10; scaling[2]=8; scaling[3]=4

    ;;

    rrfs_enkf_conv)

        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:05:00" ; popts[1]="40/2/"  ; ropts[1]="/1"
           topts[2]="0:05:00" ; popts[2]="40/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:15:00" ; popts[1]="4/4/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="6/6/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:05:00" ; popts[1]="40/2/"  ; ropts[1]="/1"
           topts[2]="0:05:00" ; popts[2]="40/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:15:00" ; popts[1]="4/4/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="6/6/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:15:00" ; popts[1]="40/2/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="40/4/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/"  ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="64/2/"  ; ropts[2]="/1"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:15:00" ; popts[1]="64/1/" ; ropts[1]="/1"
           topts[2]="0:15:00" ; popts[2]="64/2/" ; ropts[2]="/1"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="0:30:00"
        fi

        scaling[1]=4; scaling[2]=10; scaling[3]=4

    ;;

    rtma)

        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:30:00" ; popts[1]="6/12/"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="8/12/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:30:00" ; popts[1]="6/12/"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="8/12/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:30:00" ; popts[1]="6/12/"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="8/12/"  ; ropts[2]="/1"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:30:00" ; popts[1]="6/12/"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="8/12/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:30:00" ; popts[1]="14/8/"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="14/14/"  ; ropts[2]="/1"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:30:00" ; popts[1]="14/8/"  ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="14/14/"  ; ropts[2]="/1"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:30:00" ; popts[1]="14/8/" ; ropts[1]="/1"
           topts[2]="0:30:00" ; popts[2]="14/14/" ; ropts[2]="/2"
        fi

        if [ "$debug" = ".true." ] ; then
           topts[1]="3:00:00"
        fi

        scaling[1]=10; scaling[2]=10; scaling[3]=2

    ;;

    global_enkf)

        if [[ "$machine" = "Hera" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "Orion" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "Hercules" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "Jet" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "gaeac5" ]]; then
           topts[1]="0:10:00" ; popts[1]="12/3/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="12/5/" ; ropts[2]="/2"
        elif [[ "$machine" = "gaeac6" ]]; then
           topts[1]="0:10:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="16/4/" ; ropts[2]="/2"
        elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
           topts[1]="0:10:00" ; popts[1]="16/2/" ; ropts[1]="/1"
           topts[2]="0:10:00" ; popts[2]="16/4/" ; ropts[2]="/2"
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
export tmpregdir
export result
export scaling

if [[ "$machine" = "Hera" ]]; then
   export OMP_STACKSIZE=1024M
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export APRUN="srun"
elif [[ "$machine" = "Orion" ]]; then
   export OMP_STACKSIZE=2048M
   export APRUN="srun -n \$ntasks --mem=0  --cpus-per-task=\$threads"
elif [[ "$machine" = "Hercules" ]]; then
   export OMP_STACKSIZE=2048M
   export APRUN="srun  -n \$ntasks --mem=0  --cpus-per-task=\$threads"
elif [[ "$machine" = "Jet" ]]; then
   export OMP_STACKSIZE=1024M
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export APRUN="srun -n \$ntasks --cpus-per-task=\$threads"
elif [[ "$machine" = "gaeac5" ]]; then
   export OMP_STACKSIZE=1024M
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export FI_VERBS_PREFER_XRC=0
   export APRUN="srun --export=ALL -n \$ntasks"
elif [[ "$machine" = "gaeac6" ]]; then
   export OMP_STACKSIZE=1024M
   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export APRUN="srun --export=ALL -n \$ntasks"
elif [[ "$machine" = "wcoss2" || "$machine" = "acorn" ]]; then
   export OMP_PLACES=cores
   export OMP_STACKSIZE=2G
   export FORT_BUFFERED=true
   export FI_OFI_RXM_SAR_LIMIT=3145728
   export APRUN="mpiexec -n \$ntasks -ppn \$ppn --cpu-bind core --depth \$threads"
elif [[ "$machine" = "Discover" ]]; then
   export APRUN="mpiexec_mpt -np \$SLURM_NTASKS"
fi
