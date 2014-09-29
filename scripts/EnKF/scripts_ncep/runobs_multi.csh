#!/bin/tcsh

# import parameters
source /global/save/${LOGNAME}/enkf/work/scripts/ncep/current.enkfparms

setenv startupenv "${datapath}/analdate.csh"
source $startupenv

# current analysis time.
setenv analdate $analdate

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

setenv DIAGCONV .true.
setenv HXONLY .true.
setenv SAVEHX .true.

setenv datapath2 "${datapath}/${analdate}/"

set nanal=$NSTART
@ nanalsp1 = $nanals + 1
echo "nanalsp1=",$nanalsp1

while ($nanal <= $NEND)

if ($nanal == $nanalsp1) then
   setenv charnanal "ensmean"
else
   setenv charnanal "mem"`printf %03i $nanal`
endif

setenv SFCG03a ${datapath2}/bfg_${analdate}_fhr03_${charnanal}
setenv SFCG04a ${datapath2}/bfg_${analdate}_fhr04_${charnanal}
setenv SFCG05a ${datapath2}/bfg_${analdate}_fhr05_${charnanal}
setenv SFCG06a ${datapath2}/bfg_${analdate}_fhr06_${charnanal}
setenv SFCG07a ${datapath2}/bfg_${analdate}_fhr07_${charnanal}
setenv SFCG08a ${datapath2}/bfg_${analdate}_fhr08_${charnanal}
setenv SFCG09a ${datapath2}/bfg_${analdate}_fhr09_${charnanal}
setenv SFCG03 ${datapath2}/bfg_${analdate}_fhr03_${charnanal}
setenv SFCG04 ${datapath2}/bfg_${analdate}_fhr04_${charnanal}
setenv SFCG05 ${datapath2}/bfg_${analdate}_fhr05_${charnanal}
setenv SFCG06 ${datapath2}/bfg_${analdate}_fhr06_${charnanal}
setenv SFCG07 ${datapath2}/bfg_${analdate}_fhr07_${charnanal}
setenv SFCG08 ${datapath2}/bfg_${analdate}_fhr08_${charnanal}
setenv SFCG09 ${datapath2}/bfg_${analdate}_fhr09_${charnanal}
setenv SFCGES $SFCG06a
setenv SFCANL ${datapath2}/sfcanl_${analdate}_${charnanal}
# use ensemble mean surface files for forward operator
# (since ob thinning routines use surface information, won't
#  get same obs for each ensemble member if different surface
#  files specified for each member).
setenv SFCG03Ma ${datapath2}/bfg_${analdate}_fhr03_ensmean
setenv SFCG04Ma ${datapath2}/bfg_${analdate}_fhr04_ensmean
setenv SFCG05Ma ${datapath2}/bfg_${analdate}_fhr05_ensmean
setenv SFCG06Ma ${datapath2}/bfg_${analdate}_fhr05_ensmean
setenv SFCG07Ma ${datapath2}/bfg_${analdate}_fhr07_ensmean
setenv SFCG08Ma ${datapath2}/bfg_${analdate}_fhr08_ensmean
setenv SFCG09Ma ${datapath2}/bfg_${analdate}_fhr09_ensmean
setenv SFCG03M ${datapath2}/bfg_${analdate}_fhr03_ensmean
setenv SFCG04M ${datapath2}/bfg_${analdate}_fhr04_ensmean
setenv SFCG05M ${datapath2}/bfg_${analdate}_fhr05_ensmean
setenv SFCG06M ${datapath2}/bfg_${analdate}_fhr06_ensmean
setenv SFCG07M ${datapath2}/bfg_${analdate}_fhr07_ensmean
setenv SFCG08M ${datapath2}/bfg_${analdate}_fhr08_ensmean
setenv SFCG09M ${datapath2}/bfg_${analdate}_fhr09_ensmean
setenv SFCGESM $SFCG06Ma


# just use ensemble mean surface file for all members
setenv SIGG03 ${datapath2}/sfg_${analdate}_fhr03_${charnanal}
setenv SIGG04 ${datapath2}/sfg_${analdate}_fhr04_${charnanal}
setenv SIGG05 ${datapath2}/sfg_${analdate}_fhr05_${charnanal}
setenv SIGGES ${datapath2}/sfg_${analdate}_fhr06_${charnanal}
setenv SIGG07 ${datapath2}/sfg_${analdate}_fhr07_${charnanal}
setenv SIGG08 ${datapath2}/sfg_${analdate}_fhr08_${charnanal}
setenv SIGG09 ${datapath2}/sfg_${analdate}_fhr09_${charnanal}

##setenv GBIAS ${datapath}/${analdatem1}/abias
setenv GSATANG ${datapath}/${analdatem1}/satang

# obs bufr files to use in assimilation.
setenv PREINP "${RUN}.t${hr}z."
setenv PREINP1 "${RUN}.t${hrp1}z."
setenv COMIN "${obs_datapath}/${analdate}/${datdump}"
setenv COMINGES "/global/noscrub/${LOGNAME}/bias_prd09q1o"
setenv GPSBF "${COMIN}/gpsro.${datdump}.${analdate}"
setenv PREPQC "${PREPDATA}/prepqc.${datdump}.${analdate}"
setenv TCVIT  "${COMIN}/tcvitl.${datdump}.${analdate}"
setenv B1AMUA "${COMIN}/1bamua.${datdump}.${analdate}"
setenv B1AMUB "${COMIN}/1bamub.${datdump}.${analdate}"
setenv FNTSFA "${COMIN}/sstgrb.${datdump}.${analdate}"
setenv FNACNA "${COMIN}/icegrb.${datdump}.${analdate}"
setenv FNSNOA "${COMIN}/snogrb.${datdump}.${analdate}"
setenv SSMITBF "${COMIN}/ssmit.${datdump}.${analdate}"
setenv SBUVBF "${COMIN}/osbuv8.${datdump}.${analdate}"
setenv AIRSBF "${COMIN}/airsev.${datdump}.${analdate}"
setenv IASIBF "${COMIN}/mtiasi.${datdump}.${analdate}"
setenv B1HRS3 "${COMIN}/1bhrs3.${datdump}.${analdate}"
setenv B1HRS4 "${COMIN}/1bhrs4.${datdump}.${analdate}"
setenv AMSREBF "${COMIN}/amsre.${datdump}.${analdate}"
setenv B1MHS "${COMIN}/1bmhs.${datdump}.${analdate}"
setenv GSNDBF1 "${COMIN}/goesfv.${datdump}.${analdate}"
setenv ABIAS "${COMINGES}/biascr.${datdump}.${analdate}"
setenv GBIAS "${COMINGES}/biascr.${datdump}.${analdate}"

# check to see if output files already created.
set obsfiles = "${datapath2}/diag_conv_ges.${analdate}_${charnanal} ${datapath2}/sfcanl_${analdate}_${charnanal}"
set filemissing='no'
echo $obsfiles
foreach obsfile ($obsfiles) 
  if { /bin/test ! -s $obsfile } set filemissing='yes'
end
if ($filemissing == 'yes') then
   echo "nanal = ${nanal}"
   setenv PGMOUT  ${current_logdir}/run_cycle_${charnanal}.out
   time sh ${enkfscripts}/drive_gsi >&! ${current_logdir}/run_obs_${charnanal}.out
else
   echo "skipping nanal = ${nanal}, output files already created"
endif

@ nanal = $nanal + 1
end
echo "waiting at nanal = ${nanal} `date`"
wait
echo "all done `date`"

if ($charnanal == 'ensmean') then
  echo "now that ensmean done, submit ensemble member obs processing"
  /bin/tcsh ${enkfscripts}/obsproc_multi.sh  
endif

exit 0
