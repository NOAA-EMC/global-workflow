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
	'atm') combination=FalseFalseFalseFalse;;
	'med_atm_ocn_ice') combination=TRUEFalseTRUEFalse;;
	'atm_chm_between') combination=FalseFalseFalseTrue;;
	'blocked_atm_wav') combination=FalseTRUEFalseFalse;;
	'leapfrog_atm_wav')combination=FalseTRUEFalseFalse;;
	'med_atm_ocn_ice_wav') combination=TRUETRUETRUEFalse;;
	'med_atm_ocn_ice_wav1way') combination=TRUETRUETRUEFalse;;
	'med_atm_ocn_ice_wav1waywcurr') combination=TRUETRUETRUEFalse;;
	'med_atm_ocn_ice') combination=TRUEFalseTRUEFalse;;
	'medcold_atm_ocn_ice') combination=TRUEFalseTRUEFalse;;
	*) echo "SUB cplvalidate: Combination not supported" 
		exit ;;
esac
control=$CPLFLX$CPLWAV$CPLICE$CPLCHEM
#echo $control
if [ $control != $combination ]; then
	echo "SUB cplvalidate: inconsistent cpl setting!"
	exit
else
	echo "SUB cplvalidate: cpl settings validated!"
fi
}
