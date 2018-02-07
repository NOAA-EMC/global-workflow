#!/bin/ksh
set -x

export fhr=$1
export filetype=$2
export res=$3

export 'PS4=${fhr}-${filetype}-${res}:$SECONDS + '

# Set type of Interpolation for WRGIB2
export opt1=' -set_grib_type same -new_grid_winds earth '
export opt21='  -new_grid_interpolation bilinear -if '
export opt22=":(LAND|CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export opt23=' -new_grid_interpolation neighbor -fi '

cd $DATA/${filetype}_${res}

# Set local var fhm3 to YES if it is 000 or 3 hourly 
fhm3=NO		# initialize
if [ $fhr != anl -a $fhr -eq 00 ]; then
  fhm3=YES	# Generate it
else
  if [ $fhr != anl -a $fhr%3 -eq 0 ]; then
    fhm3=YES
  fi
fi

# Generate anl file on all resolution
if [ $fhr = anl ]; then
  fhm3=YES
fi

if [ $fhr != anl -a $fhr -lt 100 ]; then
  fhr3=0$fhr
else
  fhr3=$fhr
fi

#export FILET=`echo $filetype |tr [a-z] [A-Z]`

if [ $filetype = pgrb2 ]; then
  export FILET=PGB2
elif [ $filetype = pgrb2b ]; then
  export FILET=PGB2B
fi
   
case $res in 
  0p25) RES=0P25
        #  grid="0 6 0 0 0 0 0 0 1440 721 0 -1 90000000 0 48 -90000000 359750000 250000 250000 0";;
        export grid="latlon 0:1440:0.25 90:721:-0.25";;
  0p50) RES=0P5
        #  grid="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0";;
        export grid="latlon 0:720:0.5 90:361:-0.5";;
  1p00) RES=1P0
        # grid="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0";;
        export grid="latlon 0:360:1.0 90:181:-1.0";;
  2p50) RES=2P5
        # grid="0 6 0 0 0 0 0 0 144 73 0 0 90000000 0 48 -90000000 357500000 2500000 2500000 0";;
        export grid="latlon 0:144:2.5 90:73:-2.5";;
esac

if [ $filetype = pgrb2 ]; then

# Only generate 0P25 for hourly output
  if [ $res = 0P25 -o $fhm3 = YES ]; then
#    $COPYGB2 -g "$grid" -i0 -x $DATA/maser_pgrb2_${fhr} ${filetype}file_${fhr3}_${res}

    $WGRIB2  $DATA/master_pgrb2_${fhr} $opt1 $opt21 $opt22 $opt23 -new_grid $grid ${filetype}file_${fhr3}_${res}
#generate second land mask using bi-linear interpolation and append to the end
    rm -f land.grb newland.grb newnewland.grb
    $WGRIB2 $DATA/master_pgrb2_${fhr} -match "LAND:surface" -grib land.grb
    $WGRIB2 land.grb -set_grib_type same -new_grid_interpolation bilinear -new_grid $grid newland.grb
    $WGRIB2 newland.grb -set_byte 4 11 218 -grib newnewland.grb
    cat ./newnewland.grb >> ${filetype}file_${fhr3}_${res} 
#
     
    $WGRIB2 ${filetype}file_${fhr3}_${res} -s > ${filetype}ifile_${fhr3}_${res}
  fi

# Only if it is 3 hourly
  if [ $res = 1p00 -a $fhm3 = YES ]; then
    $EXECgfs/cnvgrib21_gfs -g21 ${filetype}file_${fhr3}_${res} pgbfile_${fhr}_1p0
    $GRBINDEX pgbfile_${fhr}_1p0 pgbifile_${fhr}_1p0
  elif [ $res = 2p50  -a $fhr -le 192 -a $fhm3 = YES ]; then
    $EXECgfs/cnvgrib21_gfs -g21 ${filetype}file_${fhr3}_${res} pgbfile_${fhr}_2p5
    $GRBINDEX pgbfile_${fhr}_2p5 pgbifile_${fhr}_2p5
  fi
elif [ $filetype = pgrb2b ]; then

# Only generate 0P25 for hourly output
  if [ $res = 0P25 -o $fhm3 = YES ]; then
#    $COPYGB2 -g "$grid" -i0 -x $DATA/master_pgrb2b_${fhr} ${filetype}file_${fhr3}_${res}
    $WGRIB2  $DATA/master_pgrb2b_${fhr} $opt1 $opt21 $opt22 $opt23 -new_grid $grid ${filetype}file_${fhr3}_${res}
    $WGRIB2 ${filetype}file_${fhr3}_${res} -s > ${filetype}ifile_${fhr3}_${res}
  fi
fi

# Only if it is 3 hourly
#if [ $res = 0p50 -a $filetype = pgrb2 -a $fhr3 -le 192 -a $fhm3 = YES ]; then
#  GRIB_LIST="UGRD:10 m above ground|VGRD:10 m above ground"
#  $WGRIB2 -s ${filetype}file_${fhr3}_${res} | egrep "$GRIB_LIST" | $WGRIB2 ${filetype}file_${fhr3}_${res} -s -i -grib $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2
#  $WGRIB2 $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2 -s > \
#  $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2.idx
#fi

if [ $SENDCOM = YES ]; then
  if [ $fhr = anl ]; then
    cpfs ${filetype}file_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}
    cpfs ${filetype}ifile_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}.idx
    if [ $filetype = pgrb2 -a $res = 1p00 ]; then
      cpfs pgbfile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrb$fhr
      cpfs pgbifile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrbi$fhr
    fi
  else
# Only generate 0P25 for hourly output
    if [ $res = 0P25 -o $fhm3 = YES ]; then
      cpfs ${filetype}file_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.f${fhr3}
      cpfs ${filetype}ifile_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.f${fhr3}.idx
      if [ $filetype = pgrb2 -a $res = 1p00 ]; then
        cpfs pgbfile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrbf$fhr
        cpfs pgbifile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrbif$fhr
      elif [ $filetype = pgrb2 -a $res = 2p50 -a $fhr -le 192 ]; then
#        if [ $fhr -le 192 ]; then
          cpfs pgbfile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbf$fhr.2p5deg
          cpfs pgbifile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbif$fhr.2p5deg
#        elif [ $fhr -gt 192 -a `expr $fhr % 12` -eq 0 ]; then
#          cpfs pgbfile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbf$fhr
#          cpfs  pgbifile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbif$fhr
#        fi
      fi
    fi

  fi
fi

if [ $SENDDBN = YES ]; then
  if [ $fhr = anl ]; then
    $DBNROOT/bin/dbn_alert MODEL GFS_${FILET}_${RES} $job $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}
    $DBNROOT/bin/dbn_alert MODEL GFS_${FILET}_${RES}_WIDX $job $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}.idx
    if [ $filetype = pgrb2 -a $res = 1p00 ]; then
      $DBNROOT/bin/dbn_alert MODEL GFS_PGB $job $COMOUT/${RUN}.${cycle}.pgrb$fhr
      $DBNROOT/bin/dbn_alert MODEL GFS_PGBI $job $COMOUT/${RUN}.${cycle}.pgrbi$fhr
    fi
  else
    if [ $res = 0P25 -o $fhm3 = YES ]; then
      $DBNROOT/bin/dbn_alert MODEL GFS_${FILET}_${RES} $job $COMOUT/${RUN}.${cycle}.${filetype}.${res}.f${fhr3}
      $DBNROOT/bin/dbn_alert MODEL GFS_${FILET}_${RES}_WIDX $job $COMOUT/${RUN}.${cycle}.${filetype}.${res}.f${fhr3}.idx
      if [ $filetype = pgrb2 -a $res = 1p00 -a $fhr -le 192 ]; then
        $DBNROOT/bin/dbn_alert MODEL GFS_PGB $job $COMOUT/${RUN}.${cycle}.pgrbf$fhr
        $DBNROOT/bin/dbn_alert MODEL GFS_PGBI $job $COMOUT/${RUN}.${cycle}.pgrbif$fhr
      elif [ $filetype = pgrb2 -a $res = 2p50 -a $fhr -gt 192 -a `expr $fhr % 12` -eq 0 ]; then
        $DBNROOT/bin/dbn_alert MODEL GFS_PGB2P5 $job $COMOUT/${RUN}.${cycle}.pgrbf$fhr
        $DBNROOT/bin/dbn_alert MODEL GFS_PGBI2P5 $job $COMOUT/${RUN}.${cycle}.pgrbif$fhr
      fi
    fi
  fi

# Only if it is 3 hourly
#  if [ $filetype = pgrb2 -a $res = 0p50 -a $fhr -le 192 -a $fhm3 = YES ]; then
#    $DBNROOT/bin/dbn_alert MODEL GFS_PGB_USCG_GB2 $job $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2
#    $DBNROOT/bin/dbn_alert MODEL GFS_PGB_USCG_GB2_IDX $job $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2.idx
#  fi
fi

exit
