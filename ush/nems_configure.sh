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
  restart_interval=${restart_interval:-3024000}    # Interval in seconds to write restarts
  coldstart=false
fi

ATM_model=${ATM_model:-'fv3'}
OCN_model=${OCN_model:-'mom6'}
ICE_model=${ICE_model:-'cice'}
WAV_model=${WAV_model:-'ww3'}

ATMPETS=${ATMPETS:-8}
OCNPETS=${OCNPETS:-8}
ICEPETS=${ICEPETS:-8}
WAVPETS=${WAVPETS:-8}

rm -f $DATA/nems.configure

med_petlist_bounds=${med_petlist_bounds:-"0 $(( $ATMPETS-1 ))"}
atm_petlist_bounds=${atm_petlist_bounds:-"0 $(( $ATMPETS-1 ))"}    #6*8*6+wrtgrps(24)
ocn_petlist_bounds=${ocn_petlist_bounds:-"$ATMPETS $(( $ATMPETS+$OCNPETS-1 ))"}  #120
ice_petlist_bounds=${ice_petlist_bounds:-"$(( $ATMPETS+$OCNPETS )) $(( $ATMPETS+$OCNPETS+$ICEPETS-1 ))"}  #48
wav_petlist_bounds=${wav_petlist_bounds:-"$(( $ATMPETS+$OCNPETS+$ICEPETS )) $(( $ATMPETS+$OCNPETS+$ICEPETS+$WAVPETS-1 ))"}  #48


# Copy the selected template into run directory
cp $SCRIPTDIR/nems.configure.$confignamevarfornems.IN tmp1
sed -i -e "s;@\[med_model\];cmeps;g" tmp1
sed -i -e "s;@\[atm_model\];$ATM_model;g" tmp1
sed -i -e "s;@\[med_petlist_bounds\];$med_petlist_bounds;g" tmp1
sed -i -e "s;@\[atm_petlist_bounds\];$atm_petlist_bounds;g" tmp1

if [ $cplflx = .true. ]; then
        sed -i -e "s;@\[ocn_model\];$OCN_model;g" tmp1
	sed -i -e "s;@\[ocn_petlist_bounds\];$ocn_petlist_bounds;g" tmp1
	sed -i -e "s;@\[DumpFields\];$DumpFields;g" tmp1
	sed -i -e "s;@\[coldstart\];$coldstart;g" tmp1
	sed -i -e "s;@\[restart_interval\];$restart_interval;g" tmp1
	sed -i -e "s;@\[CPL_SLOW\];$CPL_SLOW;g" tmp1
	sed -i -e "s;@\[CPL_FAST\];$CPL_FAST;g" tmp1
        sed -i -e "s;@\[FV3_RESTART_INTERVAL\];$restart_interval;g" tmp1
fi
if [ $cplwav = .true. ]; then
	sed -i -e "s;@\[wav_model\];ww3;g" tmp1
        sed -i -e "s;@\[wav_petlist_bounds\];$wav_petlist_bounds;g" tmp1
fi
if [ $cplice = .true. ]; then
	sed -i -e "s;@\[ice_model\];$ICE_model;g" tmp1
	sed -i -e "s;@\[ice_petlist_bounds\];$ice_petlist_bounds;g" tmp1
fi
if [ $cplchem = .true. ]; then
	sed -i -e "s;@\[chem_model\];gsd;g" tmp1
fi
mv tmp1 nems.configure

echo "$(cat nems.configure)"

echo "SUB ${FUNCNAME[0]}: Nems configured for $confignamevarfornems"
}
