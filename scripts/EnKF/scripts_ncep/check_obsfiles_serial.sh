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
  set nanal=0
  set anyfilemissing='no'
  set filemissing='no'
  while ($nanal <= $nanals)
      if ($nanal == '0') then
         setenv charnanal2  "ensmean"
      else
         setenv charnanal2  "mem"`printf %03i $nanal`
      endif

      echo $nanal
      echo $charnanal2
      set obsfiles = "${datapath2}/diag_conv_ges.${analdate}_${charnanal2} ${datapath2}/sfcanl_${analdate}_${charnanal2}"

      foreach obsfile ($obsfiles)
        ls -l $obsfile
          if { /bin/test ! -s $obsfile } then
          set filemissing='yes'
          set anyfilemissing='yes'
        endif
      end
      if ($filemissing == 'yes') then
        echo "file missing .."
      else
        echo "files ok .."
      endif
      @ nanal = $nanal + 1
  end

  if ($anyfilemissing == 'yes') then
    echo 'there are output files missing...wait another minute'
    sleep 120
    @ ncheck = $ncheck + 1
    if ($ncheck == $ncheckmax) then
      echo 'SOMETHING FAILED IN OBS STEP, JOB STOPPING'
      exit 1
    endif
  else
    echo "all output files seem OK"
    echo "Submit EnKF update step now"
    llsubmit ${enkfscripts}/analupdate.sh
    exit 0
  endif

end

## end check < checkmax
