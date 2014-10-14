#!/bin/tcsh

# import parameters
setenv CONFIG /global/save/${LOGNAME}/enkf/work/scripts_ncep/current.enkfparms
source ${CONFIG}

setenv startupenv "${datapath}/analdate.csh"
source $startupenv

# current analysis time.
setenv analdate $analdate

echo "analdate = $analdate"

# substringing to get yr, mon, day, hr info
setenv yr `echo $analdate | cut -c1-4`
setenv mon `echo $analdate | cut -c5-6`
setenv day `echo $analdate | cut -c7-8`
setenv hr `echo $analdate | cut -c9-10`

echo "SET DATE PARAMETERS"
setenv SUB "/u/wx23sm/bin/sub_vapor"
echo "SUB=  ${SUB}"


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

setenv DATOUT "${datapath}${analdatep1}"
setenv datapath2 "${datapath}/${analdate}/"

echo "set DATOUT"
@ nanalsp1 = $nanals + 1
echo "nanalsp1=",$nanalsp1

# Check every minute, for a maximum of 90 minutes
set ncheck=1
set ncheckmax=90

while ($ncheck <= $ncheckmax)
  set nanal2=1
  set anyfilemissing='no'
  while ($nanal2 <= $nanals)
    setenv charnanal  "mem`printf %03i $nanal2`"
    set fhr=$FHMIN
    set outfiles=""
    while ($fhr <= $FHMAX)
      set charhr="fhr`printf %02i $fhr`"
      set outfiles = "${outfiles} ${datapath}${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}${analdatep1}/bfg_${analdatep1}_${charhr}_${charnanal}"
      @ fhr = $fhr + $FHOUT
    end
    set filemissing='no'
    foreach outfile ($outfiles) 
##      ls -l $outfile
      if (-e $outfile) then 
##        echo "${outfile} is OK"
      else
        echo "${outfile} is missing"
        set filemissing='yes'
        set anyfilemissing='yes'
      endif
    end
    @ nanal2 = $nanal2 + 1
  end

  if ($anyfilemissing == 'yes') then
    echo 'there are output files missing...wait another minute'
    sleep 60
    @ ncheck = $ncheck + 1
    if ($ncheck == $ncheckmax) then
      echo 'SOMETHING FAILED IN FG STEP, JOB STOPPING'
      exit 1
    endif
  else
    echo "all output files seem OK"
## CALL ARCHIVE SCRIPT HERE
    echo "analdate=$analdate"
    echo "CONFIG=$CONFIG"
    $SUB -a GDAS-T2O -e analdate,CONFIG -g devonprod -j enkf_archive -o ${current_logdir}/enkf_archive.out -p 1 -q 1 -r 1000 -t 00:45:00 -u ${LOGNAME} -w +0000 ${enkfscripts}/arch.sh
## EXIT OUT OF CHECK LOOP
    @ ncheck = $ncheckmax + 1
  endif

end
## end check < checkmax

# CHECK SURFACE FILE
set filemissing='no'
set sfcffile="${datapath2}/enkf.t${hr}z.bf06"
if { /bin/test ! -s $sfcffile } set filemissing='yes'

#if($filemissing == 'yes') then
#  echo "SURFACE F06 FILE MISSING, WAIT 5 MINUTES"
#  sleep 300
#else
#  echo "SURFACE F06 FILE IS READY, CONTINUE ON TO NEXT CYCLE"
#endif


## next analdate: increment by $ANALINC
    setenv analdate `${incdate} $analdate $ANALINC`
    echo "setenv analdate ${analdate}" >! $startupenv

## SUBMIT NEXT CYCLE
    echo "submit cycle for ${analdate}"
    ${enkfscripts}/enkfdriver.sh

    exit 0

