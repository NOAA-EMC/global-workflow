#!/bin/ksh
set -x

export machine=WCOSS_C
curdir=`pwd`

export CASE=C768             # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072
export CDATE=2016100300


if [ $machine = WCOSS_C ]; then
 . $MODULESHOME/init/sh
 export PTMP=/gpfs/hps/ptmp
 export BASEDIR=/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS
 export SUB=/gpfs/hps/emc/global/noscrub/emc.glopara/svn/gfs/q3fy17/gfs_workflow.v14.1.0/para/bin/sub_wcoss_c
 export ACCOUNT=GFS-T2O
 export CUE2RUN=dev

 export MKL_CBWR=AVX
 export MPICH_GNI_COLL_OPT_OFF=MPI_Alltoallv
elif [ $machine = WCOSS ]; then
 export PTMP=/ptmpd3
 export BASEDIR=/global/save/$LOGNAME/NGGPS
 export SUB=/global/save/emc.glopara/svn/gfs/q3fy17/gfs_workflow.v14.1.0/para/bin/sub_wcoss
 export ACCOUNT=GFS-T2O
 export CUE2RUN=dev2
fi

export layout_x=12
export layout_y=24
#export layout_x=8
#export layout_y=16
#export layout_x=4
#export layout_y=8

export MODE=64bit      # choices:  32bit, 64bit
export TYPE=nh         # choices:  nh, hydro
export HYPT=off        # choices:  on, off  (controls hyperthreading)

export npes=$(( ${layout_x} * ${layout_y} * 6 ))
if [ $HYPT = on ]; then
 export max_core=48
 export nthreads=4
 export j_opt="-j 2"
else
 export max_core=24
 export nthreads=2
 export j_opt="-j 1"
fi
export npe_node=$(( ${max_core} / ${nthreads} ))
export nodes=$((npes/$npe_node))
export memory_node=3072
export me=$((memory_node/npe_node))

export APRUN=""
if [ $machine = WCOSS_C ]; then
 export APRUN="aprun -n $npes -N $npe_node -d $nthreads $j_opt -cc depth"
fi

export OMP_NUM_THREADS=$nthreads
export MP_STDOUTMODE=ordered
export MP_LABELIO=yes
export MP_SHARED_MEMORY=yes
export MP_COREFILE_FORMAT=lite
export MEMORY_AFFINITY=MCM


export JOBNAME=${CASE}_$CDATE        
export JOB_TEMPLATE=$BASEDIR/jobs/RUN_fv3_gfs.sh

export WORKDIR=$PTMP/$LOGNAME/fv3/${CASE}_${CDATE}.$$
export logfile=$WORKDIR/$JOBNAME.out
mkdir -p $WORKDIR
cd $WORKDIR ||exit 8
rm -rf *

#---------------------------------------------------------------------------------
cat >jobhead.sh<<EOF
#!/bin/ksh
set -x
export machine=$machine
export BASEDIR=$BASEDIR
export CDATE=$CDATE
export PTMP=$PTMP
export WORKDIR=$WORKDIR
export layout_x=$layout_x
export layout_y=$layout_y
export nthreads=$nthreads
export max_core=$max_core
export APRUN="$APRUN"
export MODE=$MODE
export HYPT=$HYPT 
export TYPE=$TYPE 
if [ $machine = WCOSS_C ]; then
 . $MODULESHOME/init/sh
 module load craype-hugepages4M
fi
EOF

cat jobhead.sh $JOB_TEMPLATE >$WORKDIR/job.sh
chmod a+x $WORKDIR/job.sh
$SUB -a $ACCOUNT -j $JOBNAME -o $logfile -p $npes/$nodes/N -q $CUE2RUN  -r $me/$nthreads/$npe_node -t 06:00 $WORKDIR/job.sh


