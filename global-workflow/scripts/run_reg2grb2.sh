#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

#requires grib_util module 

MOM6REGRID=${MOM6REGRID:-${HOMEgfs}}
export mask_file="${MOM6REGRID}/fix/reg2grb2/mask.0p25x0p25.grb2"

# offline testing:
#export DATA=
#export icefile=$DATA/DATA0p5/icer2012010106.01.2012010100_0p5x0p5.nc
#export ocnfile=$DATA/DATA0p5/ocnr2012010106.01.2012010100_0p5x0p5.nc
#export outfile=$DATA/DATA0p5/out/ocnh2012010106.01.2012010100.grb2
#
# workflow testing:
export icefile="icer${VDATE}.${ENSMEM}.${IDATE}_0p25x0p25_CICE.nc"
export ocnfile="ocnr${VDATE}.${ENSMEM}.${IDATE}_0p25x0p25_MOM6.nc"
export outfile="ocn_ice${VDATE}.${ENSMEM}.${IDATE}_0p25x0p25.grb2"
export outfile0p5="ocn_ice${VDATE}.${ENSMEM}.${IDATE}_0p5x0p5.grb2"

export mfcstcpl=${mfcstcpl:-1}
export IGEN_OCNP=${IGEN_OCNP:-197}

# PT This is the forecast date
export year=${VDATE:0:4}
export month=${VDATE:4:2}
export day=${VDATE:6:2}
export hour=${VDATE:8:2}

# PT This is the initialization date
export syear=${IDATE:0:4}
export smonth=${IDATE:4:2}
export sday=${IDATE:6:2}
export shour=${IDATE:8:2}

# PT Need to get this from above - could be 6 or 1 hour
export hh_inc_ocn=6
#
# set for 1p0 lat-lon
#export im=360
#export jm=181
# export km=40
#export imo=360
#export jmo=181
#
# set for 0p5 lat-lon
#export im=720
#export jm=361
#export km=40
#export imo=720
#export jmo=361
#
# set for 0p25 lat-lon
export im=1440
export jm=721
export imo=1440
export jmo=721
export km=40

export flats=-90.
export flatn=90.
export flonw=0.0
export flone=359.75

ln -sf "${mask_file}" ./iceocnpost.g2
${executable} > "reg2grb2.${VDATE}.${IDATE}.out"

# interpolated from 0p25 to 0p5 grid
grid2p05="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000  500000  0"
${COPYGB2} -g "${grid2p05}" -i0 -x "${outfile}" "${outfile0p5}"

