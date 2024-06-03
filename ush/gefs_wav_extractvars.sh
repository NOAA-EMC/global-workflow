#! /usr/bin/env bash                                                                                                                                                                          
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

ensname=$1
DATA=$2
cd $DATA

outfreq=6

varlist=$varlist_wav

dointerp=1
option1=' -set_grib_type same -new_grid_winds earth '
option21=' -new_grid_interpolation bilinear'
grid1p00="latlon 0:360:1.0 90:181:-1.0"

nh=6
while [[ $nh -le $fhmax ]];do
  fnh=`printf "%3.3d" ${nh}`
  echo "extracting f${fnh}"

  infile=$COM_WAVE_GRID/gefswave.${cycle}.global.${wavinres}.f${fnh}.grib2
  oufile1=$DATA/gefswave.${cycle}.global.${wavinres}.f${fnh}.mem${ensname}.rfcst.grib2
  rm -f $oufile1 #remove outfile if it already exists before extraction
            
  if [ -f $infile ]; then #check if input file exists before extraction
    if [[ $dointerp -eq 1 ]];then
      oufile2=$DATA/gefswave.${cycle}.global.${wavoures}.f${fnh}.rfcst.grib2
      $WGRIB2 $infile $option1 $option21 -new_grid $grid1p00 $oufile2>/dev/null      
      infile=$oufile2
    fi 
      $WGRIB2 $infile | grep -F -f $varlist | $WGRIB2 -i $infile -append -grib $oufile1>/dev/null
  else
    echo "WARNING: $infile does not exist."
  fi 

    nh=$(($nh + $outfreq))

done #fhr



exit 0                                                                                                                                                                                        
