#!/bin/tcsh

source /global/save/${LOGNAME}/enkf/work/scripts_ncep/current.enkfparms
setenv startupenv "${datapath}/analdate.csh"
source $startupenv

if ($analdate >= $enddate) then
  echo "***********"
  echo "end of experiment reached"
  echo "***********"
else
  if ($fg_only == "false") then
    tcsh ${enkfscripts}/obsproc_ensmean.sh
  else
    tcsh ${enkfscripts}/firstges_multi.sh
  endif
endif
