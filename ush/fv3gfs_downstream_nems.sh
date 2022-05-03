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
#-Fanglin Yang, March 2017
#  1. Modified for FV3GFS, using NCEP-NCO standard output name convention
#  2. Add option24 to turn on bitmap in grib2 file (from Wen Meng)
#-Wen Meng, January 2018, add flag PGB1F for turning on/ogg grib1 pgb data at 1.00 deg. generation.
#-Wen Meng, Feburary 2018
#  1. Add flag PGBS for turning on/off pgb data at 1.0 and 0.5 deg. generation frequency of FHOUT_PGB defined.
#-Wen Meng, October 2019
#  1. Use bilinear interpolation for LAND. It can trancate land-sea mask as 1 or 0.
#-Wen Meng, November 2019
#  1. Modify sea icea cover via land-sea mask.
#-----------------------------------------------------------------------


echo "!!!!!CREATING $RUN DOWNSTREAM PRODUCTS FOR FH = $FH !!!!!!"

export downset=${downset:-1}
export DATA=${DATA:-/ptmpd2/$LOGNAME/test}
export CNVGRIB=${CNVGRIB:-${NWPROD:-/nwprod}/util/exec/cnvgrib21}
export COPYGB2=${COPYGB2:-${NWPROD:-/nwprod}/util/exec/copygb2}
export WGRIB2=${WGRIB2:-${NWPROD:-/nwprod}/util/exec/wgrib2}
export GRBINDEX=${GRBINDEX:-${NWPROD:-nwprod}/util/exec/grbindex}
export RUN=${RUN:-"gfs"}
export cycn=$(echo $CDATE |cut -c 9-10)
export TCYC=${TCYC:-".t${cycn}z."}
export PREFIX=${PREFIX:-${RUN}${TCYC}}
export PGB1F=${PGB1F:-"NO"}
export FHOUT_PGB=${FHOUT_PGB:-3}
export PGBS=${PGBS:-"NO"} #YES-- generate 1.00 and 0.50 deg pgb data
export MODICEC=${MODICEC:-$USHgfs/mod_icec.sh}

#--wgrib2 regrid parameters
export option1=' -set_grib_type same -new_grid_winds earth '
export option21=' -new_grid_interpolation bilinear  -if '
export option22=":(CRAIN|CICEP|CFRZR|CSNOW|ICSEV):"
export option23=' -new_grid_interpolation neighbor -fi '
export option24=' -set_bitmap 1 -set_grib_max_bits 16 -if '
export option25=":(APCP|ACPCP|PRATE|CPRAT):"
export option26=' -set_grib_max_bits 25 -fi -if '
export option27=":(APCP|ACPCP|PRATE|CPRAT|DZDT):"
export option28=' -new_grid_interpolation budget -fi '
export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export grid0p5="latlon 0:720:0.5 90:361:-0.5"
export grid1p0="latlon 0:360:1.0 90:181:-1.0"
export grid2p5="latlon 0:144:2.5 90:73:-2.5"

unset paramlist paramlistb
if [ $FH -eq -1 ] ; then
  #export paramlist=/global/save/Hui-Ya.Chuang/gfs_trunk/sib/fix/global_1x1_paramlist_g2.anl
  export paramlist=${paramlist:-$PARMpost/global_1x1_paramlist_g2.anl}
  export paramlistb=${paramlistb:-$PARMpost/global_master-catchup_parmlist_g2}
  export fhr3=anl
  export PGBS=YES
elif [ $FH -eq 0 ] ; then
  export paramlist=${paramlist:-$PARMpost/global_1x1_paramlist_g2.f000}
  export paramlistb=${paramlistb:-$PARMpost/global_master-catchup_parmlist_g2}
  export fhr3=000
  export PGBS=YES
else
  export paramlist=${paramlist:-$PARMpost/global_1x1_paramlist_g2}
  export paramlistb=${paramlistb:-$PARMpost/global_master-catchup_parmlist_g2}
  export fhr3=$(expr $FH + 0 )
  if [ $fhr3 -lt 100 ]; then export fhr3="0$fhr3"; fi
  if [ $fhr3 -lt 10 ];  then export fhr3="0$fhr3"; fi
  if [ $fhr3%${FHOUT_PGB} -eq 0 ]; then
    export PGBS=YES
  fi
fi


$WGRIB2 $PGBOUT2 | grep -F -f $paramlist | $WGRIB2 -i -grib  tmpfile1_$fhr3 $PGBOUT2
export err=$?; err_chk
#if [ $machine = WCOSS -o $machine = WCOSS_C -a $downset = 2 ]; then
if [ $downset = 2 ]; then
  $WGRIB2 $PGBOUT2 | grep -F -f $paramlistb | $WGRIB2 -i -grib  tmpfile2_$fhr3 $PGBOUT2
  export err=$?; err_chk
fi

#-----------------------------------------------------
#-----------------------------------------------------
#if [ $machine = WCOSS -o $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
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
  export ncount=$($WGRIB2 $tmpfile |wc -l)
  # export tasks_post=$(eval echo \$tasksp_$nknd)
  export nproc=${nproc:-${npe_dwn:-24}}
  if [ $nproc -gt $ncount ]; then
    echo " *** FATA ERROR: Total number of records in $tmpfile is not right"
    export err=8
    err_chk
  fi
  export inv=$(expr $ncount / $nproc)
  rm -f $DATA/poescript
  export iproc=1
  export end=0

  while [ $iproc -le $nproc ] ; do
    export start=$(expr ${end} + 1)
    export end=$(expr ${start} + ${inv} - 1)
    if [[ $end -ge $ncount ]] ;then
      export end=$ncount
    fi

    # if final record of each piece is ugrd, add vgrd
    # copygb will only interpolate u and v together
    #$WGRIB2 -d $end $tmpfile |grep -i ugrd
    $WGRIB2 -d $end $tmpfile |egrep -i "ugrd|ustm|uflx|u-gwd"
    export rc=$?
    if [[ $rc -eq 0 ]] ; then
      export end=$(expr ${end} + 1)
    fi
    # if final record is land, add next record icec 
    $WGRIB2 -d $end $tmpfile |egrep -i "land"
    export rc=$?
    if [[ $rc -eq 0 ]] ; then
      export end=$(expr ${end} + 1)
    fi
    if [ $iproc -eq $nproc ]; then
      export end=$ncount
    fi

    $WGRIB2 $tmpfile -for ${start}:${end} -grib ${tmpfile}_${iproc}
    export err=$?; err_chk
    echo "${GFSDWNSH:-$USHgfs/fv3gfs_dwn_nems.sh} ${tmpfile}_${iproc} $fhr3 $iproc $nset" >> $DATA/poescript

    # if at final record and have not reached the final processor then write echo's to
    # poescript for remaining processors
    if [[ $end -eq $ncount ]] ;then
      while [[ $iproc -lt $nproc ]];do
        export iproc=$(expr $iproc + 1)
        echo "/bin/echo $iproc" >> $DATA/poescript
      done
      break
    fi
    export iproc=$(expr $iproc + 1)
  done

  date
  chmod 775 $DATA/poescript
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=$DATA/poescript
  launcher=${APRUN_DWN:-"aprun -j 1 -n 24 -N 24 -d 1 cfp"}
  if [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 -o $machine = WCOSS2 ] ; then
    $launcher $MP_CMDFILE
  elif [ $machine = HERA -o $machine = ORION -o $machine = JET -o $machine = S4 ] ; then
    if [ -s $DATA/poescript_srun ]; then rm -f $DATA/poescript_srun; fi
    touch $DATA/poescript_srun
    nm=0
    cat $DATA/poescript | while read line; do
      echo "$nm $line" >> $DATA/poescript_srun 
      nm=$((nm+1))
    done
    ${launcher:-"srun --export=ALL"} -n $nm --multi-prog $DATA/poescript_srun
  else
    $launcher
  fi
  export err=$?
  if [ $err -ne 0 ]; then sh +x $DATA/poescript ; fi

  date
  export iproc=1
  while [ $iproc -le $nproc ]; do
    if [ $nset = 1 ]; then
      cat pgb2file_${fhr3}_${iproc}_0p25 >> pgb2file_${fhr3}_0p25
      if [ "$PGBS" = "YES" ]; then
        # cat pgb2file_${fhr3}_${iproc}_0p5  >> pgb2file_${fhr3}_0p5
        cat pgb2file_${fhr3}_${iproc}_1p0  >> pgb2file_${fhr3}_1p0
        if [ "$PGB1F" = 'YES' ]; then
          cat pgbfile_${fhr3}_${iproc}_1p0   >> pgbfile_${fhr3}_1p0
        fi
      fi
    elif [ $nset = 2 ]; then
      cat pgb2bfile_${fhr3}_${iproc}_0p25 >> pgb2bfile_${fhr3}_0p25
      if [ "$PGBS" = "YES" ]; then
        # cat pgb2bfile_${fhr3}_${iproc}_0p5 >> pgb2bfile_${fhr3}_0p5
        cat pgb2bfile_${fhr3}_${iproc}_1p0 >> pgb2bfile_${fhr3}_1p0
      fi
    fi
    export iproc=$(expr $iproc + 1)
  done
  date

  #Chuang: generate second land mask using bi-linear interpolation and append to the end
  # if [ $nset = 1 ]; then
  # rm -f land.grb newland.grb newnewland.grb newnewland.grb1
  # $WGRIB2 $tmpfile -match "LAND:surface" -grib land.grb
  ##0p25 degree
  # $WGRIB2 land.grb -set_grib_type same -new_grid_interpolation bilinear -new_grid_winds earth -new_grid $grid0p25 newland.grb
  # $WGRIB2 newland.grb -set_byte 4 11 218 -grib newnewland.grb
  # cat ./newnewland.grb >> pgb2file_${fhr3}_0p25
  # $CNVGRIB -g21 newnewland.grb newnewland.grb1 
  # cat ./newnewland.grb1 >> pgbfile_${fhr3}_0p25 
  ##0p5 degree
  # rm -f newland.grb newnewland.grb newnewland.grb1
  # $WGRIB2 land.grb -set_grib_type same -new_grid_interpolation bilinear -new_grid_winds earth -new_grid $grid0p5 newland.grb
  # $WGRIB2 newland.grb -set_byte 4 11 218 -grib newnewland.grb
  # cat ./newnewland.grb >> pgb2file_${fhr3}_0p5
  #1p0
  # rm -f newland.grb newnewland.grb newnewland.grb1
  # $WGRIB2 land.grb -set_grib_type same -new_grid_interpolation bilinear -new_grid_winds earth -new_grid $grid1p0 newland.grb
  # $WGRIB2 newland.grb -set_byte 4 11 218 -grib newnewland.grb
  # cat ./newnewland.grb >> pgb2file_${fhr3}_1p0
  # $CNVGRIB -g21 newnewland.grb newnewland.grb1
  # cat ./newnewland.grb1 >> pgbfile_${fhr3}_1p0
  # fi

  if [ $nset = 1 ]; then
    if [ $fhr3 = anl ]; then
      cp pgb2file_${fhr3}_0p25  $COMOUT/${PREFIX}pgrb2.0p25.anl
      $WGRIB2 -s pgb2file_${fhr3}_0p25 > $COMOUT/${PREFIX}pgrb2.0p25.anl.idx
      if [ "$PGBS" = "YES" ]; then
        cp pgb2file_${fhr3}_0p5   $COMOUT/${PREFIX}pgrb2.0p50.anl
        cp pgb2file_${fhr3}_1p0   $COMOUT/${PREFIX}pgrb2.1p00.anl
        $WGRIB2 -s pgb2file_${fhr3}_0p5  > $COMOUT/${PREFIX}pgrb2.0p50.anl.idx
        $WGRIB2 -s pgb2file_${fhr3}_1p0  > $COMOUT/${PREFIX}pgrb2.1p00.anl.idx
        if [ "$PGB1F" = 'YES' ]; then 
          cp pgbfile_${fhr3}_1p0    $COMOUT/${PREFIX}pgrb.1p00.anl
          $GRBINDEX $COMOUT/${PREFIX}pgrb.1p00.anl $COMOUT/${PREFIX}pgrb.1p00.anl.idx
        fi
      fi
    else
      cp pgb2file_${fhr3}_0p25  $COMOUT/${PREFIX}pgrb2.0p25.f${fhr3}
      $WGRIB2 -s pgb2file_${fhr3}_0p25 > $COMOUT/${PREFIX}pgrb2.0p25.f${fhr3}.idx
      if [ "$PGBS" = "YES" ]; then
        cp pgb2file_${fhr3}_0p5   $COMOUT/${PREFIX}pgrb2.0p50.f${fhr3}
        cp pgb2file_${fhr3}_1p0   $COMOUT/${PREFIX}pgrb2.1p00.f${fhr3}
        $WGRIB2 -s pgb2file_${fhr3}_0p5  > $COMOUT/${PREFIX}pgrb2.0p50.f${fhr3}.idx
        $WGRIB2 -s pgb2file_${fhr3}_1p0  > $COMOUT/${PREFIX}pgrb2.1p00.f${fhr3}.idx
        if [ "$PGB1F" = 'YES' ]; then
          cp pgbfile_${fhr3}_1p0    $COMOUT/${PREFIX}pgrb.1p00.f${fhr3}
          $GRBINDEX $COMOUT/${PREFIX}pgrb.1p00.f${fhr3} $COMOUT/${PREFIX}pgrb.1p00.f${fhr3}.idx
        fi
      fi
    fi
  elif [ $nset = 2 ]; then
    if [ $fhr3 = anl ]; then
      cp pgb2bfile_${fhr3}_0p25  $COMOUT/${PREFIX}pgrb2b.0p25.anl
      $WGRIB2 -s pgb2bfile_${fhr3}_0p25 > $COMOUT/${PREFIX}pgrb2b.0p25.anl.idx
      if [ "$PGBS" = "YES" ]; then
        cp pgb2bfile_${fhr3}_0p5   $COMOUT/${PREFIX}pgrb2b.0p50.anl
        cp pgb2bfile_${fhr3}_1p0   $COMOUT/${PREFIX}pgrb2b.1p00.anl
        $WGRIB2 -s pgb2bfile_${fhr3}_0p5  > $COMOUT/${PREFIX}pgrb2b.0p50.anl.idx
        $WGRIB2 -s pgb2bfile_${fhr3}_1p0  > $COMOUT/${PREFIX}pgrb2b.1p00.anl.idx
      fi
    else
      cp pgb2bfile_${fhr3}_0p25  $COMOUT/${PREFIX}pgrb2b.0p25.f${fhr3}
      $WGRIB2 -s pgb2bfile_${fhr3}_0p25 > $COMOUT/${PREFIX}pgrb2b.0p25.f${fhr3}.idx
      if [ "$PGBS" = "YES" ]; then
        cp pgb2bfile_${fhr3}_0p5   $COMOUT/${PREFIX}pgrb2b.0p50.f${fhr3}
        cp pgb2bfile_${fhr3}_1p0   $COMOUT/${PREFIX}pgrb2b.1p00.f${fhr3}
        $WGRIB2 -s pgb2bfile_${fhr3}_0p5  > $COMOUT/${PREFIX}pgrb2b.0p50.f${fhr3}.idx
        $WGRIB2 -s pgb2bfile_${fhr3}_1p0  > $COMOUT/${PREFIX}pgrb2b.1p00.f${fhr3}.idx
      fi
    fi
  fi

  export nset=$(expr $nset + 1 )
done

echo "!!!!!!CREATION OF SELECT $RUN DOWNSTREAM PRODUCTS COMPLETED FOR FHR = $FH !!!!!!!"
#---------------------------------------------------------------


exit 0
