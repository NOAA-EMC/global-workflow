#!/bin/tcsh

# import parameters
setenv CONFIG /global/save/${LOGNAME}/enkf/work/scripts_ncep/current.enkfparms
source ${CONFIG}

echo "IN RUN_FG SCRIPT"

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


vv# previous analysis time.
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

echo "SET MORE PARMETERS"

# run model
setenv DATOUT "${datapath}${analdatep1}"
setenv datapath2 "${datapath}/${analdate}/"

echo "set DATOUT"

set nanal=$NSTART
@ nanalsp1 = $nanals + 1
echo "nanalsp1=",$nanalsp1

while ($nanal <= $NEND)
 setenv charnanal "mem`printf %03i $nanal`"
 setenv SFCI "${datapath}/${analdate}/sfcanl_${analdate}_${charnanal}"
 setenv SIGI "${datapath}/${analdate}/sanl_${analdate}_${charnanal}"

# check to see if output files already created.
 set fhr=$FHMIN
 set outfiles=""
 while ($fhr <= $FHMAX)
    set charhr="fhr`printf %02i $fhr`"
    set outfiles = "${outfiles} ${datapath}${analdatep1}/sfg_${analdatep1}_${charhr}_${charnanal} ${datapath}${analdatep1}/bfg_${analdatep1}_${charhr}_${charnanal}"
    @ fhr = $fhr + $FHOUT
 end
 echo "run_fg_fgs outfiles = " $outfiles

 set filemissing='no'
 foreach outfile ($outfiles) 
   if (-e "${outfile}" ) then
     echo "${outfile} is OK"
   else
     echo "${outfile} is missing"
     set filemissing='yes'
   endif
 end

 if ($filemissing == 'yes') then
   echo "nanal = ${nanal}"
   time sh ${enkfscripts}/drive_gfs >&! ${current_logdir}/run_fg_${charnanal}.out 
 else
   echo "skipping nanal = ${nanal}, output files already created"
 endif

@ nanal = $nanal + 1
end

echo "all done `date`"

exit 0
