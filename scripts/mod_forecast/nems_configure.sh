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
#cat >> nems.configure <<EOF
#EARTH_component_list: ATM
#ATM_model:            fv3
#runSeq::
#	 ATM
#::
#EOF

cp $SCRIPTDIR/nems.configure.$confignamevarfornems.IN $SCRIPTDIR/tmp0
sed "s/atm_model/FV3/g" $SCRIPTDIR/tmp0>$SCRIPTDIR/tmp1
sed "s/atm_model/FV3/g" $SCRIPTDIR/tmp1>$SCRIPTDIR/tmp0
if [ $CPLFLX = TRUE ]; then
	sed "s/ocn_model/HYCOM/g" $SCRIPTDIR/tmp0>$SCRIPTDIR/tmp1
	mv $SCRIPTDIR/tmp1 $SCRIPTDIR/tmp0
fi
if [ $CPLWAV = TRUE ]; then
	sed "s/wav_model/WW3/g" $SCRIPTDIR/tmp0>$SCRIPTDIR/tmp1
	mv $SCRIPTDIR/tmp1 $SCRIPTDIR/tmp0
fi
if [ $CPLICE = TRUE ]; then
	sed "s/ice_model/CICE/g" $SCRIPTDIR/tmp0>$SCRIPTDIR/tmp1
	mv $SCRIPTDIR/tmp1 $SCRIPTDIR/tmp0
fi
if [ $CPLCHEM = TRUE ]; then
	sed "s/chem_model/GSD/g" $SCRIPTDIR/tmp0>$SCRIPTDIR/tmp1
	mv $SCRIPTDIR/tmp1 $SCRIPTDIR/tmp0
fi
mv $SCRIPTDIR/tmp0 nems.configure

echo "SUB ${FUNCNAME[0]}: Selecting nems.configure.$confignamevarfornems.IN"
}
