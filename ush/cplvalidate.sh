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
	'atm') combination=.false.false.false.false;;
	'med_atm_ocn_ice') combination=.true.false.true.false;;
	'atm_chm_between') combination=.false.false.false.true;;
	'blocked_atm_wav') combination=.false.true.false.false;;
	'leapfrog_atm_wav')combination=.false.true.false.false;;
	'med_atm_ocn_ice_wav') combination=.true.true.true.false;;
	'med_atm_ocn_ice_wav1way') combination=.true.true.true.false;;
	'med_atm_ocn_ice_wav1waywcurr') combination=.true.true.true.false;;
	'med_atm_ocn_ice') combination=.true.false.true.false;;
	'medcold_atm_ocn_ice') combination=.true.false.true.false;;
	*) echo "SUB cplvalidate: Combination not supported" 
		exit ;;
esac
control=$CPLFLX$CPLWAV$CPLICE$CPLCHEM
#echo $control
if [ $control != $combination ]; then
	echo "SUB cplvalidate: inconsistent cpl setting!"
	exit 1
else
	echo "SUB cplvalidate: cpl settings validated!"
fi
}
