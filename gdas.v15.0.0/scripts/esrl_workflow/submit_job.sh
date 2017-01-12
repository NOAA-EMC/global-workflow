machine=$1
cat ${machine}_preamble config.sh > job.sh
if [ $machine == 'wcoss' ]; then
    bsub < job.sh
else
    qsub job.sh
fi
