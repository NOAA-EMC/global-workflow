#!/bin/sh

###################################################
# Simple code for modularized forecast script     #
# This is a real shell script, not a pseudo code  #
#												  #
# two argument is needed: 						  #
# Machine: The computing platform				  #
# Compset: Group of components that could 		  #
#          currently run using this script		  #
###################################################

#######################
# External variable	  #
#######################
# These are flags of the status of each component

# 1. change all variable names;
# 2, selection logics for nems_configure

export cplflx=${CPLFLX:-FALSE} # default off,import from outside source
export cplwav=${CPLWAV:-FALSE} # ? how to control 1-way/2-way?
export cplchem=${CPLCHEM:-FALSE} #

#######################
# Function definition #
#######################

set_environment(){		# Do we really need this block?
case "$machine" in
	'sandbox')
  		# environment commands here
  		echo "SUB: environment loaded for $machine platform"
  		;;
  	'WCOSS_C')
  		echo "SUB: environment loaded for $machine platform"
  		;;
  	'WCOSS_DELL_P3')
  		echo "SUB: environment loaded for $machine platform"
  		;;
	'theia')
  		echo "SUB: environment loaded for $machine platform"
  		;;
esac
# More platforms here
}

select_combination(){
	if [ $cplflx = FALSE -a $cplwav = FALSE -a $cplchem = FALSE ]; then
		combination='ATM'
	elif [ $cplflx = FALSE -a $cplwav = TRUE -a $cplchem = FALSE ]; then
		combination='ATM_WAVE'
	elif [ $cplflx = FALSE -a $cplwav = TRUE -a $cplchem = FALSE ]; then
		combination='ATM_WAVE_CHEM'
	else
		echo "SUB: Combination currently not supported. Exit now!"
		exit
	fi
}

data_link(){
# data in take for all active components
# Arguments: None 
# 
echo 'SUB: Linking input data for FV3'
# soft link commands insert here
if [ $cplwav = TRUE ]
then
	  echo 'SUB: Linking input data for WW3'
	  # soft link commands insert here
fi
if [ $cplflx = TRUE ]	#cplflx
then
	  echo 'SUB: Linking input data for HYCOM'
	  # soft link commands insert here
fi
if [ $cplchem = TRUE ]
then
	  echo 'SUB: Linking input data for GSD'
	  # soft link commands insert here
fi
# More components
}

namelist_and_diagtable()
{
# namelist output for a certain component
echo 'SUB: Creating name lists and model configure file for FV3'

# Call child scripts in current script directory
source $(dirname "$0")/parsing_namelists_FV3_toy.sh
source $(dirname "$0")/parsing_model_configure_FV3_toy.sh

echo 'SUB: FV3 name lists and model configure file created'

if [ $cplwav = TRUE ]
then
	  echo 'SUB: Creating name list for WW3'
	  sh parsing_namelist_WW3.sh
	  # name list insert here
fi
if [ $cplflx = TRUE ]
then
	  echo 'SUB: Creating name list for HYCOM'
	  sh parsing_namelist_HYCOM.sh
	  # name list insert here
fi
if [ $cplchem = TRUE ]
then
	  echo 'SUB: Creating name list for GSD'
	  sh parsing_namelist_GSD.sh
	  # name list insert here
fi
# More components
}

nems_config_writing()
{
# selection logic including combination of components
# Argument: 
#   Compset: directly from second argument of main script
# echo 'This section writes nems configuration file for a compset'
case "$1" in
'ATM')
rm -f nems.configure
source $(dirname "$0")/nems.configure_temp_fv3.sh
;;
'ATM_WAVE')
echo "SUB: Writing nems_config for FV3-WW3"
# nems_config for FV#-WW3 here
;;
'ATM_WAVE_CHEM')
echo "SUB: Writing nems_config for FV3-WW3-GSD"
# nems_config for FV#-WW3 here
;;
*)
echo "SUB: Component set not supported, exiting"
exit 1
esac
}

execution()
{
# launcher for given app based on the compset
# Argument:
# 
echo "SUB: !!Only output the command instead of actually executing it."
case "$1" in
  	'ATM')
  		echo "SUB: mpirun -np XX executable_FV3\n"
  		;;
  	'ATM_WAVE')
  		echo "SUB: mpirun -np XX executable_FV3-WW3\n"
  		;;
    'ATM_WAVE_CHEM')
        echo "SUB: mpirun -np XX executable_FV3-WW3-GSD\n"
        ;;
    *)
        echo "SUB: Component set $1 not supported, exiting\n"
        exit 1
esac
}

data_out()
{
# data in take for all active components
# Arguments: None 
# 
echo "SUB: copying output data for FV3"
# copy commands insert here

if [ $cplwav = TRUE ]
then
	  echo "SUB: copying output data for WW3"
	  # copy commands insert here
fi
if [ $cplflx = TRUE ]
then
	  echo "SUB: copying output data for HYCOM"
	  # copy commands insert here
fi
if [ $cplchem = TRUE ]
then
	  echo "SUB: copying output data for GSD"
	  # copy commands insert here
fi

# More components
}

#######################
# Main body starts here
#######################

if [ -z $machine ]; then
	machine=sandbox
fi

select_combination
echo "MAIN: $combination selected\n"

echo "MAIN: Forecast script started for $combination on $machine\n"
set_environment $1
echo "MAIN: ENV Configured to run\n"

echo "MAIN: Loading variables"
source forecast_def_toy.sh
echo "MAIN: Finish loading variables\n"

echo "MAIN: Linking input data\n"
data_link
echo "MAIN: Input data linked\n"

echo "MAIN: Writing name lists and model configuration\n"
namelist_and_diagtable
echo "MAIN: Namelist written\n"

echo "MAIN: Configuring NEMS\n"
nems_config_writing $combination
echo "MAIN: NEMS configured\n"

execution $combination
sleep 1s

data_out
sleep 1s
echo "MAIN: Output copied to COMROT\n"
echo "MAIN: $combination Forecast completed at normal status\n"
#return final()
