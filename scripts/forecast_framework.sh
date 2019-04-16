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

ATMOSPHERE_ON=FALSE
OCEAN_ON=FALSE
WAVE_ON=FALSE
CHEMISTRY_ON=FALSE

#######################
# Function definition #
#######################

set_environment(){
if [ $1 = 'sandbox' ] ; then
  # environment commands here
  echo "SUB: environment loaded for $1 platform"
fi
# More platforms here
}

components(){
# manipulate component switches (external) based
# on compset selected by users 
# Argument:
#   Compset: directly from second argument of main script
case "$2" in
	'ATM')
	    ATMOSPHERE_ON=TRUE
        WAVE_ON=FALSE
        OCEAN_ON=FALSE
        CHEMISTRY_ON=FALSE
            case "$1" in
        	'theia')
        		echo "SUB: Component set $2 is not supported on $1, exiting"
        		exit 1
        	esac
		;;
	'ATM_WAVE')
	    ATMOSPHERE_ON=TRUE
        WAVE_ON=TRUE
        OCEAN_ON=FALSE
        CHEMISTRY_ON=FALSE
		;;
    'ATM_WAVE_CHEM')
        ATMOSPHERE_ON=TRUE
        WAVE_ON=TRUE
        OCEAN_ON=FALSE
        CHEMISTRY_ON=TRUE
		;;
	*)
        echo "SUB: Component set $2 is not supported, exiting"
        exit 1
esac
}

data_link(){
# data in take for all active components
# Arguments: None 
# 
if [ $ATMOSPHERE_ON = TRUE ]
then
	  echo 'linking input data for FV3'
	  # soft link commands insert here
fi
if [ $WAVE_ON = TRUE ]
then
	  echo 'linking input data for WW3'
	  # soft link commands insert here
fi
if [ $OCEAN_ON = TRUE ]
then
	  echo 'linking input data for HYCOM'
	  # soft link commands insert here
fi
if [ $CHEMISTRY_ON = TRUE ]
then
	  echo 'linking input data for GSD'
	  # soft link commands insert here
fi
# More components
}

namelist_and_diagtable()
{
# namelist output for a certain component
if [ $ATMOSPHERE_ON = TRUE ]
then
	  echo 'creating name list for FV3'
	  # name list insert here
fi
if [ $WAVE_ON = TRUE ]
then
	  echo 'creating name list for WW3'
	  # name list insert here
fi
if [ $OCEAN_ON = TRUE ]
then
	  echo 'creating name list for HYCOM'
	  # name list insert here
fi
if [ $CHEMISTRY_ON = TRUE ]
then
	  echo 'creating name list for GSD'
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
		echo "SUB: No nems_config needed"
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
case "$2" in
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
        echo "SUB: Component set $2 not supported, exiting\n"
        exit 1
esac
}

data_out()
{
# data in take for all active components
# Arguments: None 
# 
if [ $ATMOSPHERE_ON = TRUE ]
then
	  echo "SUB: copying output data for FV3"
	  # copy commands insert here
fi
if [ $WAVE_ON = TRUE ]
then
	  echo "SUB: copying output data for WW3"
	  # copy commands insert here
fi
if [ $OCEAN_ON = TRUE ]
then
	  echo "SUB: copying output data for HYCOM"
	  # copy commands insert here
fi
if [ $CHEMISTRY_ON = TRUE ]
then
	  echo "SUB: copying output data for GSD"
	  # copy commands insert here
fi

# More components
}

final()
{
# report finish status
  return 0
}

#######################
# Main body starts here
#######################

echo "MAIN: Forecast script started for $2 on $1\n"
set_environment $1
echo "MAIN: ENV Configured to run\n"
sleep 1s
components $1 $2
echo "MAIN: Active components set\n"

echo "MAIN: Loading variables"
X=3
Y=4
if [ $ATMOSPHERE_ON = TRUE ]
then
	Z=5
fi
if [ $WAVE_ON = TRUE ]
then
	A=6
fi
echo "MAIN: Finish loading variables\n"

data_link
echo "MAIN: Input data linked\n"
namelist_and_diagtable
echo "MAIN: Namelist written\n"
nems_config_writing $2
echo "MAIN: NEMS configured\n"
execution $1 $2
sleep 1s

data_out
sleep 1s
echo "MAIN: Output copied to COMROT\n"
echo "MAIN: $2 Forecast completed at normal status\n"
#return final()
