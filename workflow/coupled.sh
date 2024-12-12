PSLOT=t02tth
icsdir=/work2/noaa/marine/jmeixner/hercules/wavesforhr5/ICDIR/Opt1
basedir=/work2/noaa/marine/jmeixner/hercules/TestNewGridNewThreads
path_to_clone=${basedir}
IDATE=2020091300
EDATE=$IDATE
RESDETATMOS=1152
COMROOT=${basedir}/${PSLOT}/COMROOT
EXPDIR=${basedir}/${PSLOT}/EXPDIR


./setup_expt.py gfs forecast-only --app S2SW --pslot ${PSLOT} --icsdir ${icsdir} --configdir ${path_to_clone}/global-workflow/parm/config/gfs --idate ${IDATE} --edate ${EDATE} --resdetatmos ${RESDETATMOS} --resdetocean 0.25 --interval 24 --comroot ${COMROOT} --expdir ${EXPDIR}
./setup_xml.py ${EXPDIR}/${PSLOT}


cat ${EXPDIR}/${PSLOT}/${PSLOT}.crontab

