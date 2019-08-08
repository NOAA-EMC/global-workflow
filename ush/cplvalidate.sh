#!/bin/sh

#####
## This script validates $confignamevarfornems
## against cpl** switches to check consistency
##
## This is a child script of modular
## forecast script. This script is a direct execution
#####

cplvalidate(){
echo "SUB cplvalidate: validating cpl** switches for $confignamevarfornems"
case $confignamevarfornems in
	'atm') combination=.false.F.F.F.F;;
	'med_atm_ocn_ice') combination=.true.T.T.F.F;;
	'blocked_atm_wav') combination=.true.F.F.T.F;;
	'leapfrog_atm_wav')combination=.true.F.F.T.F;;
	'med_atm_ocn_ice_wav') combination=.true.T.T.T.F;;
	'med_atm_ocn_ice_wav1way') combination=.true.T.T.T.F;;
	'med_atm_ocn_ice_wav1waywcurr') combination=.true.T.T.T.F;;
	'medcold_atm_ocn_ice') combination=.true.T.T.F.F;;
	*) echo "SUB cplvalidate: Combination not supported" 
		exit ;;
esac
control=$cpl$cplflx$cplice$cplwav$cplchem
#echo $control
if [ $control != $combination ]; then
	echo "SUB cplvalidate: inconsistent cpl setting!"
	exit 1
else
	echo "SUB cplvalidate: cpl settings validated!"
fi
}
