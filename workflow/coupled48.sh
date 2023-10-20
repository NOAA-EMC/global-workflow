APP=S2SWA
PSLOT=s2swc48t01
BASEDIR=/work2/noaa/marine/jmeixner/HR3/updatemodel
CDUMP=gfs #gfs or gefs
CONFIGDIR=$BASEDIR/global-workflow/parm/config/${CDUMP}
IDATE=2021032312
EDATE=2021032312
RES=48
GFS_CYC=1
COMROT=$BASEDIR/$PSLOT/COMROOT
EXPDIR=$BASEDIR/$PSLOT/EXPDIR

echo "source module setup:"
echo "source gw_setup.sh"

echo " "
echo "Set up script"
echo ./setup_expt.py ${CDUMP} forecast-only --app $APP --pslot $PSLOT --configdir $CONFIGDIR --idate $IDATE --edate $EDATE --res $RES --gfs_cyc $GFS_CYC --comrot $COMROT --expdir $EXPDIR 

echo " "
echo "setup workflow after any changes:"
echo "./setup_xml.py $EXPDIR/$PSLOT"

echo " " 
echo "cat $EXPDIR/$PSLOT/$PSLOT.crontab"
