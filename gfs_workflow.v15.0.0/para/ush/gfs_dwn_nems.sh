#!/bin/ksh
set -x

# this script generates 0.25/0.5/1/2.5 deg pgb files for each small Grib file
# Hui-Ya Chuang 01/2014: First Version
# Fanglin Yang  09/2015: Modified to use WGRIB2 instead of COPYGB2 for interpolation
# Fanglin Yang  02/2016: remove 0.5-deg and 2.5deg output to speed up post            

export tmpfile=$1
export fhr3=$2
export iproc=$3
export nset=$4

export CNVGRIB=${CNVGRIB:-$${NWPROD:-/nwprod}/util/exec/cnvgrib21}
export COPYGB2=${COPYGB2:-$${NWPROD:-/nwprod}/util/exec/copygb2}
export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}

export option1=' -set_grib_type same -new_grid_winds earth '
export option2=' -new_grid_interpolation bilinear  -if ":(VGTYP|SOTYP):" -new_grid_interpolation neighbor -fi '
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"

if [ $nset = 1 ]; then
 $WGRIB2 $tmpfile $option1 $option2 -new_grid $grid0p25 pgb2file_${fhr3}_${iproc}_0p25 \
                                    -new_grid $grid1p0  pgb2file_${fhr3}_${iproc}_1p0  \
                                    -new_grid $grid0p5  pgb2file_${fhr3}_${iproc}_0p5   
 $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_0p25 pgbfile_${fhr3}_${iproc}_0p25          
 $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_1p0 pgbfile_${fhr3}_${iproc}_1p0          
elif [ $nset = 2 ]; then
 $WGRIB2 $tmpfile $option1 $option2 -new_grid $grid0p25 pgb2bfile_${fhr3}_${iproc}_0p25 \
                                    -new_grid $grid1p0  pgb2bfile_${fhr3}_${iproc}_1p0  \
                                    -new_grid $grid0p5  pgb2bfile_${fhr3}_${iproc}_0p5  
fi

#----------------------------------------------------------------------------------------------
#--Hui-Ya Chuang
# export grid1p0="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
# $COPYGB2 -g "${grid1p0}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_1p0
# export grid0p5="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
# $COPYGB2 -g "${grid0p5}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_0p5
# export grid2p5="0 6 0 0 0 0 0 0 144 73 0 0 90000000 0 48 -90000000 357500000 2500000 2500000 0"
# $COPYGB2 -g "${grid2p5}" -i0 -x tmpfile_${FH}_${iproc} pgb2file_${FH}_${iproc}_2p5
# $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_1p0 pgbfile_${fhr3}_${iproc}_1p0          
# $CNVGRIB -g21 pgb2file_${fhr3}_${iproc}_2p5 pgbfile_${fhr3}_${iproc}_2p5 
#----------------------------------------------------------------------------------------------

exit
