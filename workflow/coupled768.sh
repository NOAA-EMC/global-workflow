APP=S2SW
PSLOT=u20kmt03
BASEDIR=/lfs/h2/emc/couple/noscrub/jessica.meixner/HR3/unstruc01
CDUMP=gfs #gfs or gefs
CONFIGDIR=$BASEDIR/global-workflow/parm/config/${CDUMP}
IDATE=2019120300
EDATE=${IDATE}
RES=768
GFS_CYC=1
COMROT=$BASEDIR/$PSLOT/COMROOT
EXPDIR=$BASEDIR/$PSLOT/EXPDIR

echo "Set Up Modules:"
echo "source gw_setup.sh"
echo "set +u" 

echo " "
echo "Set up script"
echo ./setup_expt.py ${CDUMP} forecast-only --app $APP --pslot $PSLOT --configdir $CONFIGDIR --idate $IDATE --edate $EDATE --res $RES --gfs_cyc $GFS_CYC --comrot $COMROT --expdir $EXPDIR 

echo " "
echo "setup workflow after any changes:"
echo "./setup_xml.py $EXPDIR/$PSLOT"

echo " " 
echo "cat $EXPDIR/$PSLOT/$PSLOT.crontab"
