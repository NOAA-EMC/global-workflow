#! /usr/bin/env bash

#####
## This script validates $confignamevarfornems
## against cpl** switches to check consistency
##
## This is a child script of modular
## forecast script. This script is a direct execution
#####

cplvalidate(){
echo "SUB cplvalidate: validating cpl** switches for ${confignamevarfornems}"
return  # TODO: Why are we returning right here?
case ${confignamevarfornems} in
  'atm') combination=.false..false..false..false..false.;;
  'datm') combination=.true..true..false..false..false.;;
  'atm_aero') combination=.true..false..false..false..true.;;
  'med_atm_ocn_ice') combination=.true..true..true..false..false.;;
  'cpld') combination=.true..true..true..false..false.;;
  'blocked_atm_wav') combination=.true..false..false..true..false.;;
  'leapfrog_atm_wav')combination=.true..false..false..true..false.;;
  'med_atm_ocn_ice_wav') combination=.true..true..true..true..false.;;
  'cpld_wave') combination=.true..true..true..true..false.;;
  'cpld_aero_wave') combination=.true..true..true..true..true.;;
  'medcold_atm_ocn_ice_wav') combination=.true..true..true..true..false.;;
  'med_atm_ocn_ice_wav1way') combination=.true..true..true..true..false.;;
  'med_atm_ocn_ice_wav1waywcurr') combination=.true..true..true..true..false.;;
  'medcold_atm_ocn_ice') combination=.true..true..true..false..false.;;
  *) echo "SUB cplvalidate: Combination not supported"
  exit 1 ;;
esac
control="${cpl}${cplflx}${cplice}${cplwav}${cplchm}"
if [[ "${control}" != "${combination}" ]]; then
  echo "SUB cplvalidate: inconsistent cpl setting!"
  exit 2
else
  echo "SUB cplvalidate: cpl settings validated!"
fi
}
