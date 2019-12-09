#!/bin/sh

set -x

ymdh_rtofs=$1

# Timing has to be made relative to the single 00z RTOFS cycle for that PDY

fhr_wave=`${NHOUR} ${ymdh_rtofs} ${YMDH}`
fhr=`${NHOUR} ${ymdh_rtofs} ${PDY}00`
fext='f'

if [ ${fhr} -le 0 ]
then
# Data from nowcast phase
  fhr=`expr 48 + ${fhr}`
  fext='n'
fi 

fhr=`printf "%03d\n" ${fhr}`

#module unload EnvVars NetCDF
#module load EnvVars/1.0.0 NetCDF/3.6.3 CDO/v1.5.0 
#module load nco

#CDO=/nwprod2/rtofs_shared/rtofs_cdo.v1.5.0/bin/cdo

#FIXwave=/marine/noscrub/wavepa/MULTI_1_CURR_Q1FY18/RTOFS_REALTIME/fix
#EXECcode=/marine/save/wavepa/svn/wave_code_git.v6/st4nc/exec

curfile=rtofs_glo_2ds_${fext}${fhr}_3hrly_prog.nc

if [ -s ${COMINcur}/rtofs.${PDY}/${curfile} ]
then

  mkdir -p rtofs_${fext}${fhr}
  cd rtofs_${fext}${fhr}

  ncks -x -v sst,sss,layer_density  ${COMINcur}/rtofs.${PDY}/rtofs_glo_2ds_${fext}${fhr}_3hrly_prog.nc rtofs_glo_uv_${PDY}_${fext}${fhr}.nc
  if [ -s rtofs_glo_uv_${PDY}_${fext}${fhr}.nc ]; then
    rm -f rtofs_glo_2ds_${fext}${fhr}_3hrly_prog.nc
  fi

  ncks -O -a -h -x -v Layer rtofs_glo_uv_${PDY}_${fext}${fhr}.nc rtofs_temp1.nc
  ncwa -h -O -a Layer rtofs_temp1.nc rtofs_temp2.nc
  ncrename -h -O -v MT,time rtofs_temp2.nc
  #ncdump -h rtofs_temp2.nc
  ncrename -h -O -d MT,time rtofs_temp2.nc
  ncks -v u_velocity,v_velocity rtofs_temp2.nc rtofs_temp3.nc
  mv -f rtofs_temp3.nc rtofs_glo_uv_${PDY}_${fext}${fhr}_flat.nc

# Convert to regular lat lon file

  cp ${FIXwave}/weights_rtofs_to_r4320x2160.nc ./weights.nc
  
# Interpolate to regular 5 min grid
  $CDO remap,r4320x2160,weights.nc rtofs_glo_uv_${PDY}_${fext}${fhr}_flat.nc rtofs_5min_01.nc

# Perform 9-point smoothing twice to make RTOFS data less noisy when
# interpolating from 1/12 deg RTOFS grid to 1/6 deg wave grid 
  $CDO -f nc -smooth9 rtofs_5min_01.nc rtofs_5min_02.nc
  $CDO -f nc -smooth9 rtofs_5min_02.nc rtofs_glo_uv_${PDY}_${fext}${fhr}_5min.nc

# Cleanup
  rm -f rtofs_temp[123].nc rtofs_5min_??.nc rtofs_glo_uv_${PDY}_${fext}${fhr}.nc weights.nc

  if [ ${fhr_wave} -gt ${HINDH} ] 
  then
    sed -e "s/HDRFL/F/g" ${FIXwave}/ww3_prnc.cur.rtofs_5m.inp.tmpl > ww3_prnc.inp
  else
    sed -e "s/HDRFL/T/g" ${FIXwave}/ww3_prnc.cur.rtofs_5m.inp.tmpl > ww3_prnc.inp
  fi

  rm -f cur.nc
  ln -s rtofs_glo_uv_${PDY}_${fext}${fhr}_5min.nc cur.nc
  ln -s ${DATA}/mod_def.rtofs_5m ./mod_def.ww3

  $EXECcode/ww3_prnc

  mv -f current.ww3 ${DATA}/rtofs.${ymdh_rtofs}

  cd ${DATA}

else

  echo ' '
  set $setoff
  echo ' '
  echo '************************************** '
  echo "*** FATAL ERROR: NO CUR FILE $curfile ***  "
  echo '************************************** '
  echo ' '
  set $seton
  postmsg "$jlogfile" "FATAL ERROR - NO CURRENT FILE (RTOFS)"
  exit 0
  echo ' '

fi
