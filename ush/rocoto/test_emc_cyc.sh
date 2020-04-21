USER=Judy.K.Henderson
PTMP=/scratch1/BMC/gsd-fv3-dev/NCEPDEV/stmp3/${USER}                 ## default PTMP directory
STMP=/scratch1/BMC/gsd-fv3-dev/NCEPDEV/stmp4/${USER}                 ## default STMP directory
GITDIR=/scratch1/BMC/gsd-fv3-dev/${USER}/test/gw_ccpp_v16b           ## where your git checkout is located
COMROT=/scratch1/BMC/gsd-fv3-dev/${USER}/test/comrot                 ## default COMROT directory
EXPDIR=/scratch1/BMC/gsd-fv3-dev/${USER}/test/expdir                 ## default EXPDIR directory

PSLOT=emccyc_v16b
IDATE=2020020800
EDATE=2020021000
RESDET=192
RESENS=96
NENS=20
HPSS_PROJECT=fim

### note default RESDET=384 RESENS=192 NENS=20  CCPP_SUITE=FV3_GFS_v16beta
###./setup_expt.py --pslot $PSLOT --configdir $CONFIGDIR --idate $IDATE --edate $EDATE --comrot $COMROT --expdir $EXPDIR [ --icsdir $ICSDIR --resdet $RESDET --resens $RESENS --nens $NENS --gfs_cyc $GFS_CYC ]

./setup_expt_gsd.py --pslot $PSLOT  \
       --idate $IDATE --edate $EDATE \
       --configdir $GITDIR/parm/config \
       --hpss_project $HPSS_PROJECT \
       --resdet=$RESDET --resens $RESENS \
       --comrot $COMROT --expdir $EXPDIR

#for running chgres, forecast, and post 
./setup_workflow.py --expdir $EXPDIR/$PSLOT

