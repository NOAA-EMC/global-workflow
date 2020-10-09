#!/bin/ksh
set -eaux

export DATAVSDB=$RUNDIR/$CDATE/$CDUMP/vsdb
mkdir -p $DATAVSDB
cd $DATAVSDB

export VSDBSH=${1:-$VSDBSH}
export xdate=${2:-$xdate}
export vlength=${3:-$vlength}
export cyc=${4:-$cyc}
export PSLOT=${5:-$PSLOT}
export CDATE=${6:-$CDATE}
export CDUMP=${7:-$CDUMP}
export gfs_cyc=${8:-$gfs_cyc}
export rain_bucket=${9:-$rain_bucket}
export machine=${10:-$machine}

PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)

export ACCOUNT=${ACCOUNT:-GFS-DEV}
export CUE2RUN=${CUE2RUN:-dev}
export KEEPDATA=${KEEPDATA:-NO}

# Submit VSDBSH as separate job on WCOSS_DELL_P3
if [ $machine = "WCOSS_DELL_P3" ]; then

    rm -rf submit.sh

cat <<EOF > submit.sh
#!/bin/ksh
#BSUB -o $ROTDIR/logs/$CDATE/vsdbjob.log
#BSUB -J vsdbjob.$PSLOT.$CDATE
#BSUB -P $ACCOUNT
#BSUB -n 28
#BSUB -R span[ptile=28]
#BSUB -R affinity[core(1)]
#BSUB -W ${TIMELIM:-06:00}
#BSUB -q $QUEUE

set -euax

export OMP_NUM_THREADS=1
printenv

$VSDBSH $xdate $xdate $vlength $cyc $PSLOT $CDATE $CDUMP $gfs_cyc $rain_bucket

EOF

   chmod 755 submit.sh
   bsub < submit.sh


# Directly execute VSDBSH on other machines
else

    $VSDBSH $xdate $xdate $vlength $cyc $PSLOT $CDATE $CDUMP $gfs_cyc $rain_bucket

fi

exit

