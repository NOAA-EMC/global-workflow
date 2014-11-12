#!/bin/tcsh

## PASS IN ANALDATE and ARCH-HOUR VIA PSUB
echo "IN ARCHIVE AND CLEANUP SCRIPT"

# import parameters
source ${CONFIG}

# current analysis time.
echo "analdate = $analdate"

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`

# previous analysis time.
setenv analdatem1 `${incdate} $analdate -$ANALINC`
# next analysis time.
setenv analdatep1 `${incdate} $analdate $ANALINC`
setenv datapathprev "${datapath}/${analdatem1}/"
setenv hrp1 `echo $analdatep1 | cut -c9-10`
setenv hrm1 `echo $analdatem1 | cut -c9-10`
setenv datapathp1 "${datapath}/${analdatep1}/"
setenv datapathm1 "${datapath}/${analdatem1}/"
# make log dir for analdate
setenv current_logdir "${logdir}/ensda_out_${analdate}"
echo "Current LogDir: ${current_logdir}"
mkdir -p ${current_logdir}

echo "SET MORE PARMS"
setenv datapath2 "${datapath}/${analdate}/"
setenv hpsstar "/nwprod/util/ush/hpsstar"

setenv archdate `${incdate} $analdate -$ARCHLAG`
setenv hrar `echo $archdate | cut -c9-10`

echo "ARCHDATE = $archdate"
echo "hrar = $hrar"

if ($do_cleanup == 'true') then
      echo "PERFORM CLEANUP OF DIRECTORY"
      echo "make tar files, then clean up files `date`"
      cd $datapath2
      tar -cvf sanl_ens_${analdate}.tar sanl*${analdate}*mem*
      tar -cvf sfhr06_ens_${analdate}.tar sfg*${analdate}*fhr06*mem*
      tar -cvf bfhr06_ens_${analdate}.tar bfg*${analdate}*fhr06*mem*
      tar -cvf sfcanl_ens_${analdate}.tar sfcanl*${analdate}*mem*

      /bin/rm -f diag*a*${analdate}*mem*
      /bin/rm -f diag*conv*${analdate}*mem*
      /bin/rm -f diag*s*${analdate}*mem*
      /bin/rm -f diag*${analdate}*mem*

      /bin/rm -f sfg*${analdate}*mem*
      /bin/rm -f gfg_${analdate}*mem*
      /bin/rm -f sanl*${analdate}*mem*
      /bin/rm -f ganl*${analdate}*mem*
      /bin/rm -f sfcanl*${analdate}*mem*
      /bin/rm -f sflxgrb*${analdate}*mem*
      /bin/rm -f bfg*${analdate}*mem*
    
      /bin/rm -f *${yr}${mon}${day}_mem*.t${hr}z*
      /bin/rm -f *.sigr1 *.sigr2 *.sfcr *.log*
      /bin/rm -rf gsitmp*
      /bin/rm -rf gfs.*
      /bin/rm -f *.nml
      /bin/rm -f hxprime*

      cd $current_logdir
      tar -cvf run_fg_logs.tgz run_fg_mem*
      tar -cvf run_obs_logs.tgz run_obs_mem*
      tar -cvf run_cycle_logs.tgz run_cycle_mem*
      tar -cvf chgres_logs.tgz chgres_mem*
      /bin/rm -f *mem*
      echo "${analdate} done cleanup `date`"

###    exit 0
endif

echo "hpssfile = ${hpssdir}/${archdate}enkfanl.tar"
echo "from directory : ${datapath}/${archdate}/" 

mkdir -p ${archdiskdir}
/bin/rcp ${datapath}/${archdate}/*.tar ${archdiskdir}/
/bin/rcp ${datapath}/${archdate}/enkf.t${hrar}z.sf06 ${archdiskdir}/sf06_${archdate}_control
/bin/rcp ${datapath}/${archdate}/sanl_*_control ${archdiskdir}/
/bin/rcp ${datapath}/${archdate}/sfcanl_*_control ${archdiskdir}/


cd ${datapath}
${hpsstar} put ${hpssdir}/${archdate}enkfanl.tar ${archdate}/* 

rm -rf ${datapath}/${archdate}

echo "DONE WITH CLEANUP AND ARCHIVING"
exit 0
