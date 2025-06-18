
PSLOT=test_wave_stat_gefs
icsdir=/lfs/h2/emc/couple/noscrub/saeideh.banihashemi/test/REPLAY_ICs
basedir=/lfs/h2/emc/couple/noscrub/saeideh.banihashemi/Dev_Work/GEFS/INFRST/GW-DEV
path_to_clone=${basedir}
COMROOT=${basedir}/${PSLOT}/COMROOT
EXPDIR=${basedir}/${PSLOT}/EXPDIR
IDATE=2019020400
EDATE=$IDATE
COMROOT=${basedir}/${PSLOT}/COMROOT
EXPDIR=${basedir}/${PSLOT}/EXPDIR
NENS="3"
RESDETATMOS="384"
RESENSATMOS="384"
START="warm"
ACCOUNT="GFS-DEV"


cd ${basedir}/global-workflow/workflow
source ${basedir}/global-workflow/workflow/gw_setup.sh

./setup_expt.py gefs forecast-only --app S2SW --pslot ${PSLOT} --icsdir ${icsdir} --configdir ${path_to_clone}/global-workflow/parm/config/gefs --idate ${IDATE} --edate ${EDATE}  --nens ${NENS} --start ${START}  --resdetatmos ${RESDETATMOS} --resensatmos ${RESENSATMOS} --resdetocean 0.25 --interval 24 --comroot ${COMROOT} --expdir ${EXPDIR}
./setup_xml.py ${EXPDIR}/${PSLOT}


cat ${EXPDIR}/${PSLOT}/${PSLOT}.crontab

