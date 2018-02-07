#!/bin/sh

# Checkout, build, setup and execute the workflow

set -ex

fv3gfs_tag="https://svnemc.ncep.noaa.gov/projects/fv3gfs/trunk"

pslot="fv3test"
expdir="/path/to/expdir"
comrot="/path/to/comrot"
fv3gfs="/path/to/fv3gfs_tag/checkout"
idate="2017073118"
edate="2017080112"

######################################
# USER NEED NOT MODIFY BELOW THIS LINE
######################################

if [ -d /scratch4/NCEPDEV ]; then
    machine="theia"
    icsdir="/scratch4/NCEPDEV/global/noscrub/glopara/ICS/FV3GFS"
elif [ -d /gpfs/hps3 ]; then
    machine="cray"
    icsdir="/gpfs/hps3/emc/global/noscrub/emc.glopara/ICS"
else
    echo "Unknown machine $machine, ABORT!"
    exit -1
fi

[[ -d $expdir/$pslot ]] && rm -rf $expdir/$pslot
[[ -d $comrot/$pslot ]] && rm -rf $comrot/$pslot
[[ -d $fv3gfs/$pslot ]] && rm -rf $fv3gfs/$pslot

gfs_ver=v15.0.0
mkdir -p $fv3gfs
cd $fv3gfs
git clone --recursive gerrit:fv3gfs gfs.${gfs_ver}

cd $fv3gfs/gfs.${gfs_ver}/sorc
sh checkout.sh
sh build_all.sh $machine
sh link_fv3gfs.sh emc $machine

cd $fv3gfs/gfs.${gfs_ver}/ush/rocoto
python setup_expt.py --pslot $pslot --comrot $comrot --expdir $expdir --idate $idate --edate $edate --icsdir $icsdir --configdir ../parm/config
python setup_workflow.py --expdir $expdir/$pslot

cd $expdir/$pslot
crontab $pslot.crontab

exit
