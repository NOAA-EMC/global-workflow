#!/bin/ksh
if [ $machine = WCOSS ]; then
set -xa
echo "!!!!!CREATING GFS DOWNSTREAM PRODUCTS FOR FH = $FH!!!!!!"
if [ $FH -eq -1 ] ; then
  unset paramlist
#export paramlist=/global/save/Hui-Ya.Chuang/gfs_trunk/sib/fix/global_1x1_paramlist_g2.anl
  export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2.anl}
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile_anl $PGBOUT2
  export grid1p0="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
  $COPYGB2 -g "${grid1p0}" -i0 -x tmpfile_anl pgb2file_anl_1p0

  export grid0p5="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
  $COPYGB2 -g "${grid0p5}" -i0 -x tmpfile_anl pgb2file_anl_0p5 
  grid2p5="0 6 0 0 0 0 0 0 144 73 0 0 90000000 0 48 -90000000 357500000 2500000 2500000 0"
  $COPYGB2 -g "${grid2p5}" -i0 -x tmpfile_anl pgb2file_anl_2p5

# make 0.25 deg Grib1 master file based on Sib's script
  unset paramlist
#export paramlist=/global/save/Hui-Ya.Chuang/gfs_trunk/sib/fix/global_master-catchup_parmlist_g2
  export paramlist=${paramlist:-$PARM_SIB/global_master-catchup_parmlist_g2}
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib tmpfile3_anl $PGBOUT2
  $COPYGB2 -g "${grid0p5}" -i0 -x tmpfile3_anl pgb2bfile_anl_0p5
  $COPYGB2 -g "${grid1p0}" -i0 -x tmpfile3_anl pgb2bfile_anl_1p0

  $WGRIB2 -s pgb2file_anl_1p0  > pgbi2file_anl_1p0
  $WGRIB2 -s pgb2file_anl_0p5  > pgbi2file_anl_0p5
  $WGRIB2 -s pgb2file_anl_2p5  > pgbi2file_anl_2p5
  $WGRIB2 -s tmpfile_anl  > tmpi2file_anl
  $WGRIB2 -s pgb2bfile_anl_0p5 > pgbi2bfile_anl_0p5
  $WGRIB2 -s pgb2bfile_anl_1p0 > pgbi2bfile_anl_1p0
  $WGRIB2 -s tmpfile3_anl > tmpfile3i_anl
  export fhr3=anl
  cp pgb2file_anl_1p0  $COMOUT/pgrb${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_anl_1p0 $COMOUT/pgrb${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_anl_0p5  $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_anl_0p5 $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_anl_2p5  $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_anl_2p5 $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp tmpfile_anl $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2
  cp tmpi2file_anl $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2bfile_anl_0p5 $COMOUT/pgrbbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2bfile_anl_0p5 $COMOUT/pgrbbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2bfile_anl_1p0 $COMOUT/pgrbbf${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2bfile_anl_1p0 $COMOUT/pgrbbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp tmpfile3_anl $COMOUT/pgrbbq${fhr3}.${CDUMP}.${CDATE}.grib2
  cp tmpfile3i_anl $COMOUT/pgrbbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
# convert 0.25/1 deg files back to Grib1 for tracker and verification respectively 
  $CNVGRIB -g21 pgb2file_anl_1p0 $COMOUT/pgbanl.${CDUMP}.${CDATE}
  $CNVGRIB -g21 tmpfile_anl $COMOUT/pgbqnl.${CDUMP}.${CDATE}
# no operational Grib1 2.5 deg files at anl
#$CNVGRIB -g21 pgb2file_anl_2p5 $COMOUT/pgrblanl.${CDUMP}.${CDATE}
elif [ $FH -ge 0 ] ; then
  if test $FH -eq 0
  then
    unset paramlist
    export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2.f000}
  else
    unset paramlist
    export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2}
  fi
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile_$FH $PGBOUT2
# split of Grib files to run downstream jobs using MPMD
  export count=`$WGRIB2 tmpfile_${FH} |wc`
  export ncount=`echo $count|cut -c1-3`
  export tasks_post=$(eval echo \$tasksp_$nknd)
  export nproc=$tasks_post
  export inv=`expr $ncount / $nproc`
  rm -f $DATA/poescript
  export iproc=1
  export end=0
  while [ $iproc -le $nproc ] ; do
    export start=`expr ${end} + 1`
    export end=`expr ${start} + ${inv} - 1`
    if [[ $end -ge $ncount ]] ;then
      export end=$ncount
    fi
# if final record of each piece is ugrd, add vgrd
# copygb will only interpolate u and v together
    $WGRIB2 -d $end tmpfile_${FH}|grep -i ugrd
    export rc=$?
    if [[ $rc -eq 0 ]] ; then
      export end=`expr ${end} + 1`
    fi
    if [ $iproc -eq $nproc ]; then
      export end=$ncount
    fi
    $WGRIB2 tmpfile_${FH} -for ${start}:${end} -grib tmpfile_${FH}_${iproc}
    echo "${GFSDWNSH:-$USHDIR/gfs_dwn.sh} $FH $iproc" >> $DATA/poescript
# if at final record and have not reached the final processor then write echo's to
# poescript for remaining processors
    if [[ $end -eq $ncount ]] ;then
      while [[ $iproc -lt $nproc ]];do
        export iproc=`expr $iproc + 1`
	echo "/bin/echo $iproc" >> $DATA/poescript
      done
      break
    fi
    export iproc=`expr $iproc + 1`
  done

  chmod 775 $DATA/poescript
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=$DATA/poescript
  mpirun.lsf
  export err=$?
  export iproc=1
  while [ $iproc -le $nproc ]; do
    cat pgbf${FH}_${iproc}.${CDUMP}.${CDATE} >> $DATA/pgbf${FH}.${CDUMP}.${CDATE}
    cat pgb2file_${FH}_${iproc}_1p0 >> pgb2file_${FH}_1p0
    cat pgb2file_${FH}_${iproc}_0p5 >> pgb2file_${FH}_0p5
    cat pgb2file_${FH}_${iproc}_2p5 >> pgb2file_${FH}_2p5
    cat pgbfile_${FH}_${iproc}_2p5 >> pgbfile_${FH}_2p5
    export iproc=`expr $iproc + 1` 
  done

# make 0.25 ,0.5, and 1.0 deg special Grib2 pgrb2b files based on Sib's script
  rm -f tmpfile3_${FH}*
  unset paramlist
  export paramlist=${paramlist:-$PARM_SIB/global_master-catchup_parmlist_g2}
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile3_$FH $PGBOUT2
# split of Grib files to run downstream jobs using MPMD
  export count=`$WGRIB2 tmpfile3_${FH} |wc`
  export ncount=`echo $count|cut -c1-3`
  export tasks_post=$(eval echo \$tasksp_$nknd)
  export nproc=$tasks_post
  export inv=`expr $ncount / $nproc`
  rm -f $DATA/poescript
  export iproc=1
  export end=0
  while [ $iproc -le $nproc ]; do
    export start=`expr ${end} + 1`
    export end=`expr ${start} + ${inv} - 1`
    if [[ $end -ge $ncount ]] ;then
      export end=$ncount
    fi
# if final record of each piece is ugrd, add vgrd
# copygb will only interpolate u and v together
    $WGRIB2 -d $end tmpfile3_${FH}|grep -i ugrd
    export rc=$?
    if [[ $rc -eq 0 ]] ; then
      export end=`expr ${end} + 1`
    fi
    if [ $iproc -eq $nproc ]; then
      export end=$ncount
    fi
    $WGRIB2 tmpfile3_${FH} -for ${start}:${end} -grib tmpfile3_${FH}_${iproc}
    echo "${GFSDWNSHB:-$USHDIR/gfs_dwnb.sh} $FH $iproc" >> $DATA/poescript
# if at final record and have not reached the final processor then write echo's to
# poescript for remaining processors
    if [[ $end -eq $ncount ]] ;then
      while [[ $iproc -lt $nproc ]];do
        export iproc=`expr $iproc + 1`
        echo "/bin/echo $iproc" >> $DATA/poescript
      done
      break
    fi
    export iproc=`expr $iproc + 1`
  done

  chmod 775 $DATA/poescript
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=$DATA/poescript
  mpirun.lsf
  export err=$?
  export iproc=1
  while [ $iproc -le $nproc ]; do
    cat pgb2bfile_${FH}_${iproc}_0p5 >> pgb2bfile_${FH}_0p5
    cat pgb2bfile_${FH}_${iproc}_1p0 >> pgb2bfile_${FH}_1p0
    export iproc=`expr $iproc + 1`
  done

# make 0.25 deg Grib1 master file based on Sib's script

  if test $FH -eq 0
  then
    unset paramlist
    export paramlist=${paramlist:-$PARM_SIB/gfs_masterg1__paramlist.f00}
    $WGRIB2 $PGBOUT2 | grep "CSNOW:surface::"  > gfs_master.csnow
    $WGRIB2 $PGBOUT2 | grep "CICEP:surface::"  > gfs_master.cicep
    $WGRIB2 $PGBOUT2 | grep "CFRZR:surface::"  > gfs_master.cfrzr
    $WGRIB2 $PGBOUT2 | grep "CRAIN:surface::"  > gfs_master.crain
  else
    unset paramlist
    export paramlist=${paramlist:-$PARM_SIB/gfs_masterg1__paramlist}
    $WGRIB2 $PGBOUT2 | grep "CSNOW:surface:.* hour ave fcst:"  >  gfs_master.csnow
    $WGRIB2 $PGBOUT2 | grep "CICEP:surface:.* hour ave fcst:"  >  gfs_master.cicep
    $WGRIB2 $PGBOUT2 | grep "CFRZR:surface:.* hour ave fcst:"  >  gfs_master.cfrzr
    $WGRIB2 $PGBOUT2 | grep "CRAIN:surface:.* hour ave fcst:"  >  gfs_master.crain
  fi
  $WGRIB2 $PGBOUT2 |grep  -F -f $paramlist > tmpfile1
  cat gfs_master.csnow gfs_master.cicep  gfs_master.cfrzr  gfs_master.crain tmpfile1 > paramlist2
  $WGRIB2 $PGBOUT2 | grep -F -f paramlist2 | $WGRIB2 -i -grib  tmpfile2_${FH} $PGBOUT2
  export count=`$WGRIB2 tmpfile2_${FH} |wc`
  export ncount=`echo $count|cut -c1-3`
  export nproc=$tasks_post
  export inv=`expr $ncount / $nproc`

  export iproc=1
  export end=0
  rm -f $DATA/poescript2
  while [ $iproc -le $nproc ]; do
    export start=`expr ${end} + 1`
    export end=`expr ${start} + ${inv} - 1`
    if [[ $end -ge $ncount ]] ;then
      export end=$ncount
    fi
# if final record of each piece is ugrd, add vgrd
    $WGRIB2 -d $end tmpfile2_${FH}|grep -i ugrd
    export rc=$?
    if [[ $rc -eq 0 ]] ; then
      export end=`expr ${end} + 1`
    fi
    if [ $iproc -eq $nproc ]; then
      export end=$ncount
    fi
    $WGRIB2 tmpfile2_${FH} -for ${start}:${end} -grib tmpfile2_${FH}_${iproc}

    echo "$CNVGRIB -g21 tmpfile2_${FH}_${iproc} pgbq${FH}_${iproc}.${CDUMP}.${CDATE}" >> $DATA/poescript2
# if at final record and have not reached the final processor then write echo's to
# poescript for remaining processors
    if [[ $end -eq $ncount ]] ;then
      while [[ $iproc -lt $nproc ]];do
        export iproc=`expr $iproc + 1`
        echo "/bin/echo $iproc" >> $DATA/poescript
      done
      break
    fi
    export iproc=`expr $iproc + 1`
  done

  chmod 775 $DATA/poescript2
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=$DATA/poescript2

  mpirun.lsf
  export err=$?
  export iproc=1
  while [ $iproc -le $nproc ]; do
    cat pgbq${FH}_${iproc}.${CDUMP}.${CDATE} >> $DATA/pgbq${FH}.${CDUMP}.${CDATE}
    export iproc=`expr $iproc + 1`
  done

  $WGRIB2 -s pgb2file_${FH}_1p0  > pgbi2file_${FH}_1p0
  $WGRIB2 -s pgb2file_${FH}_0p5  > pgbi2file_${FH}_0p5
  $WGRIB2 -s pgb2file_${FH}_2p5  > pgbi2file_${FH}_2p5
  $WGRIB2 -s tmpfile_${FH}  > tmpi2file_${FH}
  $WGRIB2 -s pgb2bfile_${FH}_0p5  > pgbi2bfile_${FH}_0p5
  $WGRIB2 -s pgb2bfile_${FH}_1p0  > pgbi2bfile_${FH}_1p0
  $WGRIB2 -s tmpfile3_${FH} > tmpfile3i_${FH}
  export fhr3=$FH
  if test $fhr3 -lt 100
  then
    export fhr3="0$fhr3"
  fi
  cp pgb2file_${FH}_1p0  $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_${FH}_1p0 $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_${FH}_0p5  $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_${FH}_0p5 $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_${FH}_2p5  $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_${FH}_2p5 $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp tmpfile_${FH}       $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2
  cp tmpi2file_${FH}     $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp $DATA/pgbq${FH}.${CDUMP}.${CDATE} $COMOUT/
  cp $DATA/pgbf${FH}.${CDUMP}.${CDATE} $COMOUT/
  cp pgbfile_${FH}_2p5 $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}
  cp pgb2bfile_${FH}_0p5  $COMOUT/pgrbbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2bfile_${FH}_0p5 $COMOUT/pgrbbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2bfile_${FH}_1p0  $COMOUT/pgrbbf${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2bfile_${FH}_1p0 $COMOUT/pgrbbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp tmpfile3_${FH} $COMOUT/pgrbbq${fhr3}.${CDUMP}.${CDATE}.grib2
  cp tmpfile3i_${FH} $COMOUT/pgrbbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
fi

echo "!!!!!!CREATION OF GFS DOWNSTREAM PRODUCTS COMPLETED FOR FHR = $FH !!!!!!!"

# generate 0.25 and 1 deg files only for R&D machine
else
set -x
echo "!!!!!CREATING SELECT GFS DOWNSTREAM PRODUCTS FOR FH = $FH for non WCOSS machine!!!!!!"
if [ $FH -eq -1 ] ; then
  unset paramlist
  export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2.anl}
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile_anl $PGBOUT2
  export grid1p0="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
  $COPYGB2 -g "${grid1p0}" -i0 -x tmpfile_anl pgb2file_anl_1p0

  export grid0p5="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
  $COPYGB2 -g "${grid0p5}" -i0 -x tmpfile_anl pgb2file_anl_0p5

  $WGRIB2 -s pgb2file_anl_1p0  > pgbi2file_anl_1p0
  $WGRIB2 -s pgb2file_anl_0p5  > pgbi2file_anl_0p5
  $WGRIB2 -s tmpfile_anl  > tmpi2file_anl
  export fhr3=anl
  cp pgb2file_anl_1p0  $COMOUT/pgrb${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_anl_1p0 $COMOUT/pgrb${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_anl_0p5  $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_anl_0p5 $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp tmpfile_anl $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2
  cp tmpi2file_anl $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
# convert 1 deg files back to Grib1 for verification 
  $CNVGRIB -g21 pgb2file_anl_1p0 $COMOUT/pgbanl.${CDUMP}.${CDATE}
#  $CNVGRIB -g21 tmpfile_anl $COMOUT/pgbqnl.${CDUMP}.${CDATE}

elif [ $FH -ge 0 ] ; then
  if test $FH -eq 0
  then
    unset paramlist
    export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2.f000}
  else
    unset paramlist
    export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2}
  fi
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile_$FH $PGBOUT2
  export grid1p0="0 6 0 0 0 0 0 0 360 181 0 0 90000000 0 48 -90000000 359000000 1000000 1000000 0"
  $COPYGB2 -g "${grid1p0}" -i0 -x tmpfile_$FH pgb2file_${FH}_1p0

  export grid0p5="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
  $COPYGB2 -g "${grid0p5}" -i0 -x tmpfile_$FH pgb2file_${FH}_0p5

  $WGRIB2 -s pgb2file_${FH}_1p0  > pgbi2file_${FH}_1p0
  $WGRIB2 -s pgb2file_${FH}_0p5  > pgbi2file_${FH}_0p5
  $WGRIB2 -s tmpfile_${FH}  > tmpi2file_$FH

  export fhr3=$FH
  if test $fhr3 -lt 100
  then
    export fhr3="0$fhr3"
  fi

  cp pgb2file_${FH}_1p0  $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_${FH}_1p0 $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_${FH}_0p5  $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgbi2file_${FH}_0p5 $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp tmpfile_${FH}       $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2
  cp tmpi2file_${FH}     $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
# convert 1 deg files back to Grib1 for verification  
  $CNVGRIB -g21 pgb2file_${FH}_1p0 $COMOUT/pgbf${FH}.${CDUMP}.${CDATE}

fi

echo "!!!!!!CREATION OF SELECT GFS DOWNSTREAM PRODUCTS COMPLETED FOR FHR = $FH !!!!!!!"

fi 
