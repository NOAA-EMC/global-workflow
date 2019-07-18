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

# Copy the selected template into run directory
cp $SCRIPTDIR/nems.configure.$confignamevarfornems.IN tmp1
sed "s/atm_model/FV3/g" tmp1>tmp0

if [ $CPLFLX = TRUE ]; then
	sed "s/ocn_model/MOM6/g" tmp0>tmp1
	mv tmp1 tmp0
fi
if [ $CPLWAV = TRUE ]; then
	sed "s/wav_model/WW3/g" tmp0>tmp1
	mv tmp1 tmp0
fi
if [ $CPLICE = TRUE ]; then
	sed "s/ice_model/CICE/g" tmp0>tmp1
	mv tmp1 tmp0
fi
if [ $CPLCHEM = TRUE ]; then
	sed "s/chem_model/GSD/g" tmp0>tmp1
	mv tmp1 tmp0
fi
mv tmp0 nems.configure

echo "SUB ${FUNCNAME[0]}: Nems configured for $confignamevarfornems"
}
