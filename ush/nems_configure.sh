#!/bin/sh

#####
## This script writes nems.configure file 
## first, select a "*.IN" templates based on 
## $confignamevarfornems and parse values based on 
## $cpl** switches.
##
## This is a child script of modular
## forecast script. This script is definition only
#####
writing_nems_configure()
{
echo "SUB ${FUNCNAME[0]}: parsing_nems_configure begins"
if [ -e $SCRIPTDIR/nems.configure ]; then
	rm -f $SCRIPTDIR/nems.configure
fi

# Setup nems.configure
DumpFields=${NEMSDumpFields:-false}
if [[ $inistep = "cold" ]]; then
  restart_interval=0
  coldstart=true     # this is the correct setting
else
  restart_interval=${restart_interval:-1296000}    # Interval in seconds to write restarts
  coldstart=false
fi

rm -f $DATA/nems.configure

if [ $CASE = "C96" ] ; then
  MED_petlist_bounds=${MED_petlist_bounds:-'0 149'}
  ATM_petlist_bounds=${ATM_petlist_bounds:-'0 149'}
  OCN_petlist_bounds=${OCN_petlist_bounds:-'150 389'}
  ICE_petlist_bounds=${ICE_petlist_bounds:-'390 509'}
elif [ $CASE = "C384" ] ; then
  # This is 4x8 layout * 6 - worked
  #MED_petlist_bounds=${MED_petlist_bounds:-'0 263'}
  #ATM_petlist_bounds=${ATM_petlist_bounds:-'0 263'}    #192+wrtgrps(72)
  #OCN_petlist_bounds=${OCN_petlist_bounds:-'264 503'}  #240
  #ICE_petlist_bounds=${ICE_petlist_bounds:-'504 623'}  #120

  MED_petlist_bounds=${MED_petlist_bounds:-'0 311'}
  ATM_petlist_bounds=${ATM_petlist_bounds:-'0 311'}    #6*8*6+wrtgrps(24)
  OCN_petlist_bounds=${OCN_petlist_bounds:-'312 431'}  #120
  ICE_petlist_bounds=${ICE_petlist_bounds:-'432 479'}  #48

  # This is 6x12 layout * 6 = 432 + 72 # didn't work
  #MED_petlist_bounds=${MED_petlist_bounds:-'0 503'}
  #ATM_petlist_bounds=${ATM_petlist_bounds:-'0 503'}    #432+wrtgrps(72)
  #OCN_petlist_bounds=${OCN_petlist_bounds:-'504 743'}  #240
  #ICE_petlist_bounds=${ICE_petlist_bounds:-'744 863'}  #120
else
  echo "$CASE not supported for coupled yet"
  # $CASE can only run standalone model
fi

# Copy the selected template into run directory
cp $SCRIPTDIR/nems.configure.$confignamevarfornems.IN tmp1
sed "s/atm_model/FV3/g" tmp1>tmp0

if [ $CPLFLX = TRUE ]; then
	sed "s/@MED_petlist_bounds/$MED_petlist_bounds/g" tmp0>tmp1
	sed "s/@ATM_petlist_bounds/$ATM_petlist_bounds/g" tmp1>tmp0
	sed "s/@OCN_petlist_bounds/$OCN_petlist_bounds/g" tmp0>tmp1
	sed "s/@DumpFields/$DumpFields/g" tmp1>tmp0
	sed "s/@coldstart/$coldstart/g" tmp0>tmp1
	sed "s/@restart_interval/$restart_interval/g" tmp1>tmp0
	sed "s/@CPL_SLOW/$CPL_SLOW/g" tmp0>tmp1
	sed "s/@CPL_FAST/$CPL_FAST/g" tmp1>tmp0
#	mv tmp1 tmp0
fi
if [ $CPLWAV = TRUE ]; then
	sed "s/wav_model/WW3/g" tmp0>tmp1
	mv tmp1 tmp0
fi
if [ $CPLICE = TRUE ]; then
	sed "s/ice_model/CICE/g" tmp0>tmp1
	sed "s/@ICE_petlist_bounds/$ICE_petlist_bounds/g" tmp0>tmp1
	mv tmp1 tmp0
fi
if [ $CPLCHEM = TRUE ]; then
	sed "s/chem_model/GSD/g" tmp0>tmp1
	mv tmp1 tmp0
fi
mv tmp0 nems.configure

echo "SUB ${FUNCNAME[0]}: Nems configured for $confignamevarfornems"
}
