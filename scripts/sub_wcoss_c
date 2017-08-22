#!/bin/ksh
set -x
#
# May 28, 2013 - Shrinivas Moorthi :now updated for lsf9.1.1 - should handle coupled case also
#
usage="\
Usage:  $0 [options] executable [args]
      where the options are:
      -a account        account (default: none)
      -b binding        run smt binding or not (default:NO)
      -d dirin          initial directory (default: cwd)
      -e envars         copy comma-separated environment variables
      -g group          group name
      -i                append standard input to command file
      -j jobname        specify jobname (default: executable basename)
#     -m machine        machine on which to run (default: current)
      -m mpiver         mpi version (poe or intelmpi) (default: poe)
      -n                write command file to stdout rather than submitting it
      -o output         specify output file (default: jobname.out)
      -p procs[/nodes[/ppreq]
                        number of MPI tasks and optional nodes or Bblocking and
                        ppreq option (N or S) (defaults: serial, Bunlimited, S)
      -q queue[/qpreq]  queue name and optional requirement, e.g. dev/P
                        (defaults: 1 if serial or dev if parallel and none)
                        (queue 3 or 4 is dev or prod with twice tasks over ip)
                        (options: P=parallel, B=bigmem, b=batch)
      -r rmem[/rcpu[/pe_node]  resources memory and cpus/task and cores per node                       (default: '1024 mb', 1, and 16)
      -t timew          wall time limit in [[hh:]mm:]ss format (default: 900)
      -u userid         userid to run under (default: self)
      -v                verbose mode
      -w when           when to run, in yyyymmddhh[mm], +hh[mm], thh[mm], or
                        Thh[mm] (full, incremental, today or tomorrow) format
                        (default: now)

       You can also export variables and
        \"INHERIT_ENV\" (default:-\"YES\") - Set this variable to \"NO\" and
        export it if you do not want the next job to inherit current job
        environment.

        Other environmental variables which can be exported from outside are:

          \"KMP_STACKSIZE\"           (default:-\"1024m\")
          \"MP_EUIDEVELOP\"           (default:-\"NULL\")
          \"F_UFMTENDIAN\"            (default:-\"NULL\")
          \"MPICH_ALLTOALL_THROTTLE\" (default:-\"NULL\")
          \"MP_SINGLE_THREAD\"        (default:-\"NULL\")
          \"MP_EAGER_LIMIT\"          (default:-\"NULL\")
          \"MP_USE_BULK_XFER\"        (default:-\"NULL\")
          \"MP_COLLECTIVE_OFFLOAD\"   (default:-\"NULL\")
          \"MP_SHARED_MEMORY\"        (default:-\"NULL\")
          \"MP_MPILIB\"               (default:-\"NULL\")
          \"MP_LABELIO\"              (default:-\"NULL\")
          \"MP_STDOUTMODE\"           (default:-\"NULL\")
          \"DATA\"                    (default:-\"/stmp/$LOGNAME/sub\"
                                       - deleted at the end if created)

Function:  This command submits a job to the batch queue."
subcmd="$*"
stdin=NO
nosub=NO
account=""
binding="NO"
dirin=""
envars=""
group=""
jobname=""
machine=""
mpiver=""
output=""
procs=0
nodes=""
ppreq="NONE"
queue=""
qpreq=""
rmem="1024"
rcpu="1"
pe_node=${pe_node:-24}
timew="900"
userid=""
verbose=NO
when=""
while getopts a:b:d:e:g:ij:m:no:p:q:r:t:u:vw: opt;do
  case $opt in
    a) account="$OPTARG";;
    b) binding="$OPTARG";;
    d) dirin="$OPTARG";;
    e) envars="$OPTARG";;
    g) group="$OPTARG";;
    i) stdin=YES;;
    j) jobname=$OPTARG;;
    m) machine="$OPTARG";;
    m) mpiver="$OPTARG";;
    n) nosub=YES;;
    o) output=$OPTARG;;
    p) procs=$(echo $OPTARG/|cut -d/ -f1);nodes=$(echo $OPTARG/|cut -d/ -f2);ppreq=$(echo $OPTARG/|cut -d/ -f3);;
    q) queue=$(echo $OPTARG/|cut -d/ -f1);qpreq=$(echo $OPTARG/|cut -d/ -f2);;
    r) rmem=$(echo $OPTARG/|cut -d/ -f1);rcpu=$(echo $OPTARG/|cut -d/ -f2);pe_node=$(echo $OPTARG/|cut -d/ -f3);;
    t) timew=$OPTARG;;
    u) userid=$OPTARG;;
    v) verbose=YES;;
    w) when=$OPTARG;;
    \?) echo $0: invalid option >&2;echo "$usage" >&2;exit 1;;
  esac
done
shift $(($OPTIND-1))
if [[ $# -eq 0 ]];then
  echo $0: missing executable name >&2;echo "$usage" >&2;exit 1
fi
exec=$1
if [[ ! -s $exec ]]&&which $exec >/dev/null 2>&1;then
  exec=$(which $exec)
fi
shift
args="$*"
bn=$(basename $exec)
export jobname=${jobname:-$bn}
machine=${machine:-""}
output=${output:-$jobname.out}
myuser=$LOGNAME
myhost=$(hostname)
#mpiver=${mpiver:-poe}
envars=$envars

#DATA=/lustre/fs/scratch/$LOGNAME/stmp
pext=${pext:-""}
PTMP=${PTMP:-/gpfs/hps/stmp}
DATA=${DATA:-$PTMP/$LOGNAME/sub}
if [ -s $DATA ] ; then
  MKDATA=NO
else
  mkdir -p $DATA
  MKDATA=YES
fi
dirin=${dirin:-$(pwd)}

queue=${queue:-dev}
timew=${timew:-01:20}
timew=$(echo $timew |cut -d: -f1):$(echo $timew |cut -d: -f2)
threads=${rcpu:-1}
nthreads=$threads


max_core=${max_core:-24}
ptile=$((procs/nodes))
pe_node=${pe_ndoe:-$ptile}
task_node=${pe_node:-${task_node:-$max_core}}

export INHERIT_ENV=${INHERIT_ENV:-YES}
if [ $nodes -eq 1 ] ; then
  task_node=$procs
fi
tot_size=$procs
max_tasks=$((max_core*nodes))
#tot_size=$procs
if [ $((task_node*threads)) -gt $max_core ]; then
   core_typ=cpu
   nthreads=$threads
   threads=$((2*max_core/task_node))
   echo "Hyper-threading is used - setting core_typ=$corei_typ"
fi
export core_typ=${core_typ:-core}

if [ $pe_node -eq $max_tasks -a $nthreads -gt 1 ] ; then
  pe_node=$((npe_node/nthreads))
fi


export OMP_STACKSIZE=${OMP_STACKSIZE:-1024m}
export KMP_AFFINITY=${KMP_AFFINITY:-disabled}
export OMP_NUM_THREADS=$nthreads

export TZ=GMT
cfile=$DATA/sub$$
> $cfile

if [ $INHERIT_ENV = YES ] ; then
 echo "#!/bin/ksh"                                                >> $cfile
else
 echo "#!/bin/sh --login"                                         >> $cfile
 echo "#BSUB -L /bin/sh"                                          >> $cfile
fi
echo "#BSUB -P $account"                                          >> $cfile
echo "#BSUB -e $output"                                           >> $cfile
echo "#BSUB -o $output"                                           >> $cfile
echo "#BSUB -J $jobname"                                          >> $cfile
#echo "#BSUB -network type=sn_all:mode=US"                         >> $cfile
echo "#BSUB -q $queue"                                            >> $cfile
#echo "#BSUB -n $tot_size"                                         >> $cfile
if [ $queue = dev_transfer ] ; then
   echo "#BSUB -R rusage[mem=$rmem]"                                 >> $cfile
fi
if [ $queue != dev_transfer ] ; then
   echo "#BSUB -M $rmem"                                                >> $cfile
   echo "#BSUB -extsched 'CRAYLINUX[]' -R '1*{select[craylinux && !vnode]} + $max_tasks*{select[craylinux && vnode]span[ptile=24] cu[type=cabinet]}'" >> $cfile
fi
echo "#BSUB -W $timew"                                            >> $cfile
echo "#BSUB -cwd $dirin"                                          >> $cfile

if [[ -n $when ]];then
  whena=$when
  if [[ $when = +* ]];then
    hr=$(echo $when|cut -c2-3)
    mn=$(echo $when|cut -c4-5)
    [[ -n $mn ]] || mn=00
    now=$(date -u +"%Y%m%d%H%M")
    ((mn+=$(echo $now|cut -c11-12)))
    [[ $mn -ge 60 ]] && ((hr+=1)) && ((mn-=60))
    [[ $mn -lt 10 ]] && mn=0$mn
    whena=$($NDATE +$hr $(echo $now|cut -c1-10))$mn
  elif [[ $when = t* ]];then
    hr=$(echo $when|cut -c2-3)
    mn=$(echo $when|cut -c4-5)
    [[ -n $mn ]] || mn=00
    now=$(date -u +"%Y%m%d")
    whena=$now$hr$mn
  elif [[ $when = T* ]];then
    hr=$(echo $when|cut -c2-3)
    mn=$(echo $when|cut -c4-5)
    [[ -n $mn ]] || mn=00
    now=$(date -u +"%Y%m%d%H")
    whena=$($NDATE +24 $now|cut -c1-8)$hr$mn
  fi
  yr=$(echo $whena|cut -c1-4)
  mo=$(echo $whena|cut -c5-6)
  dy=$(echo $whena|cut -c7-8)
  hr=$(echo $whena|cut -c9-10)
  mn=$(echo $whena|cut -c11-12)
  [[ -n $mn ]] || mn=00
  echo "#BSUB -b $yr:$mo:$dy:$hr:$mn"                            >> $cfile
fi

#echo "source ~${LOGNAME}/.profile"          >> $cfile
#echo "ulimit -s unlimited"                  >> $cfile
#if [ ${MP_EUIDEVICE:-NULL} = sn_all ] ; then
#echo "#BSUB -network \"type=sn_all:mode=US\" "                 >> $cfile
#fi
#if [ ${MP_EULIB:-NULL} != NULL ] ; then
#echo "export MP_EUILIB=$MP_EUILIB"                             >> $cfile
#fi

if [ ${KMP_AFFINITY:-NULL} != NULL ] ; then
 echo "export KMP_AFFINITY=$KMP_AFFINITY"                       >> $cfile
fi

echo "export OMP_STACKSIZE=$OMP_STACKSIZE"                      >> $cfile
echo "export OMP_NUM_THREADS=$OMP_NUM_THREADS"                  >> $cfile
echo "export FORT_BUFFERED=true"                                >> $cfile

if [ ${MP_LABELIO:-NULL} != NULL ] ; then
 echo "export MP_LABELIO=$MP_LABELIO"                           >> $cfile
fi
if [ ${MP_STDOUTMODE:-NULL} != NULL ] ; then
 echo "export MP_STDOUTMODE=$MP_STDOUTMODE "                    >> $cfile
fi
for var in $(eval echo $envars | tr , ' ') ; do
 echo "export $var"                                             >> $cfile
done

APRUN="aprun -j1 -n $procs -N $pe_node -d $nthreads -cc depth"
echo "export APRUN='$APRUN'"                                    >> $cfile

echo "$exec"                                                    >> $cfile

if [[ $stdin = YES ]];then
  cat
fi >>$cfile
if [[ $nosub = YES ]];then
  cat $cfile
  exit
elif [[ $verbose = YES ]];then
  set -x
  cat $cfile
fi
bsub=${bsub:-$LSF_BINDIR/bsub}

ofile=$DATA/subout$$
>$ofile
chmod 777 $ofile
$bsub < $cfile
rc=$?
cat $ofile
if [[ -w $SUBLOG ]];then
  jobn=$(grep -i submitted $ofile|head -n1|cut -d\" -f2)
  date +"%Y%m%d%H%M%S : $subcmd : $jobn" >>$SUBLOG
fi
#rm $cfile $ofile
#[[ $MKDATA = YES ]] && rmdir $DATA
exit $rc
