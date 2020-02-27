#!/bin/sh

set -x

ymdh_rtofs=$1
curfile=$2

# Timing has to be made relative to the single 00z RTOFS cycle for that PDY

mkdir -p rtofs_${ymdh_rtofs}
cd rtofs_${ymdh_rtofs}

ncks -x -v sst,sss,layer_density  $curfile cur_uv_${PDY}_${fext}${fhr}.nc
ncks -O -a -h -x -v Layer cur_uv_${PDY}_${fext}${fhr}.nc cur_temp1.nc
ncwa -h -O -a Layer cur_temp1.nc cur_temp2.nc
ncrename -h -O -v MT,time cur_temp2.nc
ncrename -h -O -d MT,time cur_temp2.nc
ncks -v u_velocity,v_velocity cur_temp2.nc cur_temp3.nc
mv -f cur_temp3.nc cur_uv_${PDY}_${fext}${fhr}_flat.nc

# Convert to regular lat lon file

cp ${FIXwave}/weights_rtofs_to_r4320x2160.nc ./weights.nc
  
# Interpolate to regular 5 min grid
$CDO remap,r4320x2160,weights.nc cur_uv_${PDY}_${fext}${fhr}_flat.nc cur_5min_01.nc

# Perform 9-point smoothing twice to make RTOFS data less noisy when
# interpolating from 1/12 deg RTOFS grid to 1/6 deg wave grid 
if [ "WAV_CUR_CDO_SMOOTH" = "YES" ]; then
  $CDO -f nc -smooth9 cur_5min_01.nc cur_5min_02.nc
  $CDO -f nc -smooth9 cur_5min_02.nc cur_glo_uv_${PDY}_${fext}${fhr}_5min.nc
else
  mv cur_5min_01.nc cur_glo_uv_${PDY}_${fext}${fhr}_5min.nc
fi

# Cleanup
rm -f cur_temp[123].nc cur_5min_??.nc cur_glo_uv_${PDY}_${fext}${fhr}.nc weights.nc

if [ ${fhr_wave} -gt ${WAVHINDH} ] 
then
  sed -e "s/HDRFL/F/g" ${FIXwave}/ww3_prnc.cur.${WAVECUR_FID}.inp.tmpl > ww3_prnc.inp
else
  sed -e "s/HDRFL/T/g" ${FIXwave}/ww3_prnc.cur.${WAVECUR_FID}.inp.tmpl > ww3_prnc.inp
fi

rm -f cur.nc
ln -s cur_glo_uv_${PDY}_${fext}${fhr}_5min.nc cur.nc
ln -s ${DATA}/mod_def.rtofs_5m ./mod_def.ww3

$EXECcode/ww3_prnc

mv -f current.ww3 ${DATA}/${WAVECUR_FID}.${ymdh_rtofs}

cd ${DATA}

