#!/bin/ksh
set -x

#-----------------------------------------------------------------------
#-Hui-Ya Chuang, January 2014:  First version.
#  This script was written to include all new Grib2 GFS downstream post processing in
#  EMC's parallel so that EMC can reproduce all operational pgb files as in operations.
#  Due to EMC's limited resources, MPMD is used to speed up downstream post processing.
#-Hui-Ya Chuang, September 2015: 
#  modification to generate select grids on non-WCOSS machines without using MPMD.  
#  NCEP R&D machines do not have MPMD.
#-Fanglin Yang, September 2015
#  1. restructured and simplified the script.
#  2. use wgrib2 instead of copygb2 for interpolation. copygb2 is slow and is not 
#     working for converting grib2 files that are produced by nceppost using nemsio files. 
#  3. use wgrib2 to convert pgbm on quarter-degree (no matter gaussian or lat-lon) grid 
#     to pgbq on quarter-degree lat-lon grid.
#  4. Between using COPYGB2 and WGRIB2, about 50% of the fields are bit-wise identical. 
#     Others are different at the noise level at spotted points.
#-Fanglin Yang, February 2016
#  1. For NEMSIO, gfspost1 exceends 6-hour CPU limits on WCOSS. Remove pgrbh and pgrbl
#-----------------------------------------------------------------------


echo "!!!!!CREATING GFS DOWNSTREAM PRODUCTS FOR FH = $FH !!!!!!"

export downset=${downset:-1}
export DATA=${DATA:-/ptmpd2/$LOGNAME/test}
export CNVGRIB=${CNVGRIB:-${NWPROD:-/nwprod}/util/exec/cnvgrib21}
export COPYGB2=${COPYGB2:-${NWPROD:-/nwprod}/util/exec/copygb2}
export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}
if [ $machine = WCOSS_C ]; then
  . $MODULESHOME/init/sh 2>>/dev/null
  export IOBUF_PARAMS="*:size=32M:count=4"
  module load iobuf 2>>/dev/null
fi


#--wgrib2 regrid parameters
export option1=' -set_grib_type same -new_grid_winds earth '    
export option21=' -new_grid_interpolation bilinear  -if '
export option22=":(LAND|CSNOW|CRAIN|CFRZR|CICEP|ICSEV):"
export option23=' -new_grid_interpolation neighbor -fi '
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"


unset paramlist paramlistb
if [ $FH -eq -1 ] ; then
  #export paramlist=/global/save/Hui-Ya.Chuang/gfs_trunk/sib/fix/global_1x1_paramlist_g2.anl
  export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2.anl}
  export paramlistb=${paramlistb:-$PARM_SIB/global_master-catchup_parmlist_g2}
  export fhr3=anl
elif [ $FH -eq 0 ] ; then
  export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2.f000}
  export paramlistb=${paramlistb:-$PARM_SIB/global_master-catchup_parmlist_g2}
  export fhr3=000
else
  export paramlist=${paramlist:-$PARM_SIB/global_1x1_paramlist_g2}
  export paramlistb=${paramlistb:-$PARM_SIB/global_master-catchup_parmlist_g2}
  export fhr3=`expr $FH + 0 `
  if [ $fhr3 -lt 100 ]; then export fhr3="0$fhr3"; fi
  if [ $fhr3 -lt 10 ];  then export fhr3="0$fhr3"; fi
fi


$WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile1_$fhr3 $PGBOUT2
#if [ $machine = WCOSS -o $machine = WCOSS_C -a $downset = 2 ]; then
if [ $downset = 2 ]; then
   $WGRIB2 $PGBOUT2 | grep -F -f $paramlistb | $WGRIB2 -i -grib  tmpfile2_$fhr3 $PGBOUT2
fi

#-----------------------------------------------------
#-----------------------------------------------------
if [ $machine = WCOSS -o $machine = WCOSS_C ]; then
#-----------------------------------------------------
#-----------------------------------------------------
export nset=1
export totalset=2
if [ $downset = 1 ]; then totalset=1 ; fi

#..............................................
while [ $nset -le $totalset ]; do
#..............................................
  export tmpfile=$(eval echo tmpfile${nset}_${fhr3})    

# split of Grib files to run downstream jobs using MPMD
  export ncount=`$WGRIB2 $tmpfile |wc -l`
# export tasks_post=$(eval echo \$tasksp_$nknd)
  export nproc=${nproc:-${npe_dwn:-32}}
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
    $WGRIB2 -d $end $tmpfile |grep -i ugrd
    export rc=$?
    if [[ $rc -eq 0 ]] ; then
      export end=`expr ${end} + 1`
    fi
    if [ $iproc -eq $nproc ]; then
      export end=$ncount
    fi

    $WGRIB2 $tmpfile -for ${start}:${end} -grib ${tmpfile}_${iproc}
    echo "${GFSDWNSH:-$USHDIR/gfs_dwn_new.sh} ${tmpfile}_${iproc} $fhr3 $iproc $nset" >> $DATA/poescript

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

date
  chmod 775 $DATA/poescript
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=$DATA/poescript
  if [ $machine = WCOSS_C ] ; then
     launcher=${launcher:-$APRUN_DWN}
     $launcher $MP_CMDFILE
  else
     launcher=${launcher:-mpirun.lsf}
     $launcher
  fi
  export err=$?
  if [ $err -ne 0 ]; then sh +x $DATA/poescript ; fi

date
  export iproc=1
  while [ $iproc -le $nproc ]; do
    if [ $nset = 1 ]; then
     cat pgb2file_${fhr3}_${iproc}_0p25 >> pgb2file_${fhr3}_0p25
     cat pgb2file_${fhr3}_${iproc}_0p5  >> pgb2file_${fhr3}_0p5
     cat pgb2file_${fhr3}_${iproc}_1p0  >> pgb2file_${fhr3}_1p0
#    cat pgb2file_${fhr3}_${iproc}_2p5  >> pgb2file_${fhr3}_2p5
     cat pgbfile_${fhr3}_${iproc}_0p25  >> pgbfile_${fhr3}_0p25
     cat pgbfile_${fhr3}_${iproc}_1p0   >> pgbfile_${fhr3}_1p0
    elif [ $nset = 2 ]; then
     cat pgb2bfile_${fhr3}_${iproc}_0p25 >> pgb2bfile_${fhr3}_0p25
     cat pgb2bfile_${fhr3}_${iproc}_0p5 >> pgb2bfile_${fhr3}_0p5
     cat pgb2bfile_${fhr3}_${iproc}_1p0 >> pgb2bfile_${fhr3}_1p0
    fi
    export iproc=`expr $iproc + 1` 
  done
date

  if [ $nset = 1 ]; then
   $WGRIB2 -s pgb2file_${fhr3}_0p25 > $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
   $WGRIB2 -s pgb2file_${fhr3}_0p5  > $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
#  $WGRIB2 -s pgb2file_${fhr3}_2p5  > $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}.grib2.idx
   cp pgb2file_${fhr3}_0p25  $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2
   cp pgb2file_${fhr3}_0p5   $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2
#  cp pgb2file_${fhr3}_2p5   $COMOUT/pgrbl${fhr3}.${CDUMP}.${CDATE}.grib2

   if [ $fhr3 = anl ]; then
    $WGRIB2 -s pgb2file_${fhr3}_1p0  > $COMOUT/pgrbanl.${CDUMP}.${CDATE}.grib2.idx
    cp pgb2file_${fhr3}_1p0   $COMOUT/pgrbanl.${CDUMP}.${CDATE}.grib2
    cp pgbfile_${fhr3}_1p0    $COMOUT/pgbanl.${CDUMP}.${CDATE}
    cp pgbfile_${fhr3}_0p25   $COMOUT/pgbqnl.${CDUMP}.${CDATE}
   else
    $WGRIB2 -s pgb2file_${fhr3}_1p0  > $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
    cp pgb2file_${fhr3}_1p0   $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2
    cp pgbfile_${fhr3}_1p0    $COMOUT/pgbf${FH}.${CDUMP}.${CDATE}
    cp pgbfile_${fhr3}_0p25   $COMOUT/pgbq${FH}.${CDUMP}.${CDATE}
   fi


  elif [ $nset = 2 ]; then
   $WGRIB2 -s pgb2bfile_${fhr3}_0p25 > $COMOUT/pgrbbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
   $WGRIB2 -s pgb2bfile_${fhr3}_0p5  > $COMOUT/pgrbbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
   $WGRIB2 -s pgb2bfile_${fhr3}_1p0  > $COMOUT/pgrbbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
   cp pgb2bfile_${fhr3}_0p25  $COMOUT/pgrbbq${fhr3}.${CDUMP}.${CDATE}.grib2
   cp pgb2bfile_${fhr3}_0p5   $COMOUT/pgrbbh${fhr3}.${CDUMP}.${CDATE}.grib2
   cp pgb2bfile_${fhr3}_1p0   $COMOUT/pgrbbf${fhr3}.${CDUMP}.${CDATE}.grib2
  fi

#..............................................
 export nset=`expr $nset + 1 `
 done
#..............................................


#---------------------------------------------------------------
#---------------------------------------------------------------
# R&D machine has no MPDP. Only generate 0.25 and 1 deg files 
else
#---------------------------------------------------------------
#---------------------------------------------------------------
  $WGRIB2 tmpfile1_$fhr3  $option1 $option21 $option22 $option23 \
                                           -new_grid $grid0p25 pgb2file_${fhr3}_0p25 \
                                           -new_grid $grid0p5  pgb2file_${fhr3}_0p5 \
                                           -new_grid $grid1p0  pgb2file_${fhr3}_1p0

# convert 1 deg files back to Grib1 for verification 
  if [ $fhr3 = anl ]; then
   $CNVGRIB -g21 pgb2file_${fhr3}_1p0  $COMOUT/pgbanl.${CDUMP}.${CDATE}
  else
   $CNVGRIB -g21 pgb2file_${fhr3}_1p0  $COMOUT/pgbf${FH}.${CDUMP}.${CDATE}
  fi

  $WGRIB2 -s pgb2file_${fhr3}_0p25 > $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2.idx
# $WGRIB2 -s pgb2file_${fhr3}_0p5  > $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  $WGRIB2 -s pgb2file_${fhr3}_1p0  > $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2.idx
  cp pgb2file_${fhr3}_0p25 $COMOUT/pgrbq${fhr3}.${CDUMP}.${CDATE}.grib2
# cp pgb2file_${fhr3}_0p5  $COMOUT/pgrbh${fhr3}.${CDUMP}.${CDATE}.grib2
  cp pgb2file_${fhr3}_1p0  $COMOUT/pgrbf${fhr3}.${CDUMP}.${CDATE}.grib2

#---------------------------------------------------------------
fi 
echo "!!!!!!CREATION OF SELECT GFS DOWNSTREAM PRODUCTS COMPLETED FOR FHR = $FH !!!!!!!"
#---------------------------------------------------------------


exit
