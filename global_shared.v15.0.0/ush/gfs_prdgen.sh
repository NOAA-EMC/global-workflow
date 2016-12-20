#!/bin/ksh
set -x

export fhr=$1
export filetype=$2
export res=$3

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
  0p25) RES=0P25;;
  0p50) RES=0P5
        grid="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0";;
  1p00) RES=1P0
        grid="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0";;
  2p50) RES=2P5
        grid="0 6 0 0 0 0 0 0 144 73 0 0 90000000 0 48 -90000000 357500000 2500000 2500000 0";;
esac

if [ $filetype = pgrb2 ]; then

# Only generate 0P25 for hourly output
  if [ $res = 0P25 -o $fhm3 = YES ]; then
    $COPYGB2 -g "$grid" -i0 -x $DATA/tmpfile_${fhr} ${filetype}file_${fhr3}_${res}
    $WGRIB2 ${filetype}file_${fhr3}_${res} -s > ${filetype}ifile_${fhr3}_${res}
  fi

# Only if it is 3 hourly
  if [ $res = 1p00 -a $fhr -le 192 -a $fhm3 = YES ]; then
    $CNVGRIB21_GFS -g21 ${filetype}file_${fhr3}_${res} pgbfile_${fhr}_1p0
    $GRBINDEX pgbfile_${fhr}_1p0 pgbifile_${fhr}_1p0
  elif [ $res = 2p50 -a $fhm3 = YES ]; then
    $CNVGRIB21_GFS -g21 ${filetype}file_${fhr3}_${res} pgbfile_${fhr}_2p5
    $GRBINDEX pgbfile_${fhr}_2p5 pgbifile_${fhr}_2p5
  fi
elif [ $filetype = pgrb2b ]; then

# Only generate 0P25 for hourly output
  if [ $res = 0P25 -o $fhm3 = YES ]; then
    $COPYGB2 -g "$grid" -i0 -x $DATA/tmpfile3_${fhr} ${filetype}file_${fhr3}_${res}
    $WGRIB2 ${filetype}file_${fhr3}_${res} -s > ${filetype}ifile_${fhr3}_${res}
  fi
fi

# Only if it is 3 hourly
if [ $res = 0p50 -a $filetype = pgrb2 -a $fhr3 -le 192 -a $fhm3 = YES ]; then
  GRIB_LIST="UGRD:10 m above ground|VGRD:10 m above ground"
  $WGRIB2 -s ${filetype}file_${fhr3}_${res} | egrep "$GRIB_LIST" | $WGRIB2 ${filetype}file_${fhr3}_${res} -s -i -grib $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2
  $WGRIB2 $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2 -s > \
  $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2.idx
fi

if [ $SENDCOM = YES ]; then
  if [ $fhr = anl ]; then
    cp ${filetype}file_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}
    cp ${filetype}ifile_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}.idx
    if [ $filetype = pgrb2 -a $res = 1p00 ]; then
      cp pgbfile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrb$fhr
      cp pgbifile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrbi$fhr
    fi
  else
# Only generate 0P25 for hourly output
    if [ $res = 0P25 -o $fhm3 = YES ]; then
      cp ${filetype}file_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.f${fhr3}
      cp ${filetype}ifile_${fhr3}_${res} $COMOUT/${RUN}.${cycle}.${filetype}.${res}.f${fhr3}.idx
      if [ $filetype = pgrb2 -a $res = 1p00 -a fhr -le 192 ]; then
        cp pgbfile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrbf$fhr
        cp pgbifile_${fhr}_1p0 $COMOUT/${RUN}.${cycle}.pgrbif$fhr
      elif [ $filetype = pgrb2 -a $res = 2p50 ]; then
        if [ $fhr -le 192 ]; then
          cp pgbfile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbf$fhr.2p5deg
          cp pgbifile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbif$fhr.2p5deg
        elif [ $fhr -gt 192 -a `expr $fhr % 12` -eq 0 ]; then
          cp pgbfile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbf$fhr
          cp pgbifile_${fhr}_2p5 $COMOUT/${RUN}.${cycle}.pgrbif$fhr
        fi
      fi
    fi

  fi
fi

if [ $SENDDBN = YES ]; then
  if [ $fhr = anl ]; then
    $DBNROOT/bin/dbn_alert MODEL GFS_${FILET}_${RES} $job $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}
    $DBNROOT/bin/dbn_alert MODEL GFS_${FILET}_${RES}_WIDX $job $COMOUT/${RUN}.${cycle}.${filetype}.${res}.${fhr3}.idx
    if [ $res = 1p00 ]; then
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
  if [ $filetype = pgrb2 -a $res = 0p50 -a $fhr -le 192 -a $fhm3 = YES ]; then
    $DBNROOT/bin/dbn_alert MODEL GFS_PGB_USCG_GB2 $job $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2
    $DBNROOT/bin/dbn_alert MODEL GFS_PGB_USCG_GB2_IDX $job $COMOUT/${RUN}.${cycle}.master.grbf${fhr3}.10m.uv.grib2.idx
  fi
fi

exit
