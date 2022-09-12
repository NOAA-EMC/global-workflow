#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_atmos_gldas.sh
# Script description:  Runs the global land analysis                    
#
################################################################################

source "${HOMEgfs:?}/ush/preamble.sh"

#################################
# Set up UTILITIES
#################################
export FINDDATE=${FINDDATE:-/apps/ops/prod/nco/core/prod_util.v2.0.13/ush/finddate.sh}
export utilexec=${utilexec:-/apps/ops/prod/libs/intel/19.1.3.304/grib_util/1.2.3/bin}
export CNVGRIB=${CNVGRIB:-$utilexec/cnvgrib}
export WGRIB=${WGRIB:-$utilexec/wgrib}
export WGRIB2=${WGRIB2:-/apps/ops/prod/libs/intel/19.1.3.304/wgrib2/2.0.7/bin/wgrib2}
export COPYGB=${COPYGB:-$utilexec/copygb}
export NDATE=${NDATE:-/apps/ops/prod/nco/core/prod_util.v2.0.13/exec/ndate}
export DCOMIN=${DCOMIN:-${DCOMROOT:-"/lfs/h1/ops/prod/dcom"}}
export CPCGAUGE=${CPCGAUGE:-/lfs/h2/emc/global/noscrub/emc.global/dump}
export COMINgdas=${COMINgdas:-$ROTDIR}
export OFFLINE_GLDAS=${OFFLINE_GLDAS:-"NO"}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}


#################################
# Set up the running environment
#################################
export USE_CFP=${USE_CFP:-"NO"}
export COMPONENT=${COMPONENT:-atmos}
export assim_freq=${assim_freq:-6}
export gldas_spinup_hours=${gldas_spinup_hours:-72}
export gldas_cdate=$CDATE
export gldas_eymd=$(echo $gldas_cdate |cut -c 1-8)                 
export gldas_ecyc=$(echo $gldas_cdate |cut -c 9-10)                 
export gldas_sdate=$($NDATE -$gldas_spinup_hours $CDATE)
export gldas_symd=$(echo $gldas_sdate |cut -c 1-8)                 
export gldas_scyc=$(echo $gldas_sdate |cut -c 9-10)                 

export iau_cdate=$CDATE
if [ "$DOIAU" = "YES" ]; then
 IAU_OFFSET=${IAU_OFFSET:-0}
 IAUHALH=$((IAU_OFFSET/2))
 export iau_cdate=$($NDATE -$IAUHALH $CDATE)
fi
export iau_eymd=$(echo $iau_cdate |cut -c 1-8)
export iau_ecyc=$(echo $iau_cdate |cut -c 9-10)
echo "GLDAS runs from $gldas_sdate to $iau_cdate"

export CASE=${CASE:-C768}
export res=$(echo $CASE |cut -c2-5)
export JCAP=$((2*res-2))               
export nlat=$((2*res))
export nlon=$((4*res))

export FIXgldas=${FIXgldas:-$HOMEgfs/fix}
export topodir=${topodir:-$HOMEgfs/fix/fix_fv3_gmted2010/$CASE/}

DATA=${DATA:-$pwd/gldastmp$$}    
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 1
export RUNDIR=$DATA


#################################
GDAS=${RUNDIR}/force
mkdir -p $GDAS

input1=$COMINgdas/gdas.$gldas_symd/$gldas_scyc/$COMPONENT/RESTART
input2=$COMINgdas/gdas.$gldas_eymd/$gldas_ecyc/$COMPONENT/RESTART
[[ -d $RUNDIR ]]       && rm -fr $RUNDIR/FIX
[[ -f $RUNDIR/LIS ]]   && rm -fr $RUNDIR/LIS
[[ -d $RUNDIR/input ]] && rm -fr $RUNDIR/input
mkdir -p $RUNDIR/input
ln -fs $GDAS $RUNDIR/input/GDAS
ln -fs $FIXgldas/FIX_T${JCAP} $RUNDIR/FIX
ln -fs $EXECgldas/gldas_model $RUNDIR/LIS


#---------------------------------------------------------------
### 1) Get gdas 6-tile netcdf restart file and gdas forcing data
#---------------------------------------------------------------

${USHgldas}/gldas_get_data.sh $gldas_sdate $gldas_cdate
export err=$?
$ERRSCRIPT || exit 2

#---------------------------------------------------------------
### 2) Get CPC daily precip and temporally disaggreated 
#---------------------------------------------------------------

${USHgldas}/gldas_forcing.sh $gldas_symd $gldas_eymd
export err=$?
$ERRSCRIPT || exit 3

# spatially disaggregated

if   [ $JCAP -eq 1534 ]; then 
  gds='255 4 3072 1536 89909 0 128 -89909 -117 117 768 0 0 0 0 0 0 0 0 0 255 0 0 0 0 0'
elif [ $JCAP -eq  766 ]; then
  gds='255 4 1536  768 89821 0 128 -89821 -234 234 384 0 0 0 0 0 0 0 0 0 255 0 0 0 0 0'
elif [ $JCAP -eq  382 ]; then
  gds='255 4  768  384 89641 0 128 -89641 -469 469 192 0 0 0 0 0 0 0 0 0 255 0 0 0 0 0'
elif [ $JCAP -eq  190 ]; then
  gds='255 4  384  192 89284 0 128 -89284 -938 938  96 0 0 0 0 0 0 0 0 0 255 0 0 0 0 0'
else
 echo "JCAP=$JCAP not supported, exit"
 export err=4
 $ERRSCRIPT || exit 4
fi

echo $JCAP
echo $gds
ymdpre=$(sh $FINDDATE $gldas_symd d-1) 
ymdend=$(sh $FINDDATE $gldas_eymd d-2)
ymd=$ymdpre

if [ $USE_CFP = "YES" ] ; then
  rm -f ./cfile
  touch ./cfile
fi

while [ $ymd -le $ymdend ]; do
  if [ $ymd -ne $ymdpre ]; then
    if [ $USE_CFP = "YES" ] ; then
      echo "$COPYGB -i3 '-g"$gds"' -x $GDAS/cpc.$ymd/precip.gldas.${ymd}00 $RUNDIR/cmap.gdas.${ymd}00" >> ./cfile
      echo "$COPYGB -i3 '-g"$gds"' -x $GDAS/cpc.$ymd/precip.gldas.${ymd}06 $RUNDIR/cmap.gdas.${ymd}06" >> ./cfile
    else
      $COPYGB -i3 -g"$gds" -x $GDAS/cpc.$ymd/precip.gldas.${ymd}00 $RUNDIR/cmap.gdas.${ymd}00
      $COPYGB -i3 -g"$gds" -x $GDAS/cpc.$ymd/precip.gldas.${ymd}06 $RUNDIR/cmap.gdas.${ymd}06
    fi
  fi
  if [ $ymd -ne $ymdend ]; then 
    if [ $USE_CFP = "YES" ] ; then
      echo "$COPYGB -i3 '-g"$gds"' -x $GDAS/cpc.$ymd/precip.gldas.${ymd}12 $RUNDIR/cmap.gdas.${ymd}12" >> ./cfile
      echo "$COPYGB -i3 '-g"$gds"' -x $GDAS/cpc.$ymd/precip.gldas.${ymd}18 $RUNDIR/cmap.gdas.${ymd}18" >> ./cfile
    else
      $COPYGB -i3 -g"$gds" -x $GDAS/cpc.$ymd/precip.gldas.${ymd}12 $RUNDIR/cmap.gdas.${ymd}12
      $COPYGB -i3 -g"$gds" -x $GDAS/cpc.$ymd/precip.gldas.${ymd}18 $RUNDIR/cmap.gdas.${ymd}18
    fi
  fi
  ymd=$(sh $FINDDATE $ymd d+1)
done

if [ $USE_CFP = "YES" ] ; then
  $APRUN_GLDAS_DATA_PROC ./cfile
fi

# create configure file
${USHgldas}/gldas_liscrd.sh $gldas_sdate $iau_cdate ${JCAP}
export err=$?
$ERRSCRIPT || exit 4


#---------------------------------------------------------------
### 3) Produce initials noah.rst from 6-tile gdas restart files 
#---------------------------------------------------------------
rm -f fort.41 fort.141 fort.11 fort.12

# 3a) create gdas2gldas input file 

cat >> fort.141 << EOF
 &config
   data_dir_input_grid="$input1"
   sfc_files_input_grid="${gldas_symd}.${gldas_scyc}0000.sfcanl_data.tile1.nc","${gldas_symd}.${gldas_scyc}0000.sfcanl_data.tile2.nc","${gldas_symd}.${gldas_scyc}0000.sfcanl_data.tile3.nc","${gldas_symd}.${gldas_scyc}0000.sfcanl_data.tile4.nc","${gldas_symd}.${gldas_scyc}0000.sfcanl_data.tile5.nc","${gldas_symd}.${gldas_scyc}0000.sfcanl_data.tile6.nc"
   mosaic_file_input_grid="${CASE}_mosaic.nc"
   orog_dir_input_grid="$topodir"
   orog_files_input_grid="${CASE}_oro_data.tile1.nc","${CASE}_oro_data.tile2.nc","${CASE}_oro_data.tile3.nc","${CASE}_oro_data.tile4.nc","${CASE}_oro_data.tile5.nc","${CASE}_oro_data.tile6.nc"
   i_target=$nlon
   j_target=$nlat
   model="$model"
 /
EOF
cp fort.141 fort.41


# 3b) Use gdas2gldas to generate nemsio file 

export OMP_NUM_THREADS=1
export pgm=gdas2gldas
. prep_step
$APRUN_GAUSSIAN ${EXECgldas}/gdas2gldas      1>&1 2>&2
export err=$?
$ERRSCRIPT || exit 5


# 3c)gldas_rst to generate noah.rst 

sfcanl=sfc.gaussian.nemsio
ln -fs FIX/lmask_gfs_T${JCAP}.bfsa fort.11
ln -fs $sfcanl fort.12
export pgm=gldas_rst
. prep_step
${EXECgldas}/gldas_rst     1>&1 2>&2
export err=$?
$ERRSCRIPT || exit 6

mv $sfcanl ${sfcanl}.$gldas_symd


#---------------------------------------------------------------
### 4) run noah/noahmp model
#---------------------------------------------------------------
export pgm=LIS
. prep_step
$APRUN_GLDAS ./LIS      1>&1 2>&2
export err=$?
$ERRSCRIPT || exit 7


#---------------------------------------------------------------
### 5) using gdas2gldas to generate nemsio file for gldas_eymd
###    use gldas_post to replace soil moisture and temperature
###    use gldas2gdas to produce 6-tile restart file
#---------------------------------------------------------------
rm -f fort.41 fort.241 fort.42

# 5a) create input file for gdas2gldas

cat >> fort.241 << EOF
 &config
   data_dir_input_grid="$input2"
   sfc_files_input_grid="${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile1.nc","${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile2.nc","${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile3.nc","${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile4.nc","${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile5.nc","${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile6.nc"
   mosaic_file_input_grid="${CASE}_mosaic.nc"
   orog_dir_input_grid="$topodir"
   orog_files_input_grid="${CASE}_oro_data.tile1.nc","${CASE}_oro_data.tile2.nc","${CASE}_oro_data.tile3.nc","${CASE}_oro_data.tile4.nc","${CASE}_oro_data.tile5.nc","${CASE}_oro_data.tile6.nc"
   i_target=$nlon
   j_target=$nlat
   model="$model"
 /
EOF
cp fort.241 fort.41

# 5b) use gdas2gldas to produce nemsio file

export OMP_NUM_THREADS=1
export pgm=gdas2gldas
. prep_step
$APRUN_GAUSSIAN ${EXECgldas}/gdas2gldas     1>&1 2>&2
export err=$?
$ERRSCRIPT || exit 8


# 5c) use gldas_post to replace soil moisture and temperature

yyyy=$(echo $iau_eymd | cut -c1-4)
gbin=$RUNDIR/EXP901/NOAH/$yyyy/$iau_eymd/LIS.E901.${iau_eymd}${iau_ecyc}.NOAHgbin
sfcanl=sfc.gaussian.nemsio
rm -rf fort.11 fort.12
ln -fs $gbin   fort.11
ln -fs $sfcanl fort.12

export pgm=gldas_post
. prep_step
${EXECgldas}/gldas_post      1>&1 2>&2
export err=$?
$ERRSCRIPT || exit 9

cp  fort.22 ./gldas.nemsio
mv  fort.22 ${sfcanl}.gldas


# 5d) use gldas2gdas to create 6-tile restart tiles

cat >> fort.42 << EOF
 &config
   orog_dir_gdas_grid="$topodir"
   mosaic_file_gdas_grid="${CASE}_mosaic.nc"
 /
EOF

# copy/link gdas netcdf tiles
k=1; while [ $k -le 6 ]; do
 cp $input2/${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile${k}.nc  ./sfc_data.tile${k}.nc
 k=$((k+1))
done

# copy soil type
ln -fs FIX/stype_gfs_T${JCAP}.bfsa  stype_gfs_T${JCAP}.bfsa

export OMP_NUM_THREADS=1
export pgm=gldas2gdas
. prep_step
$APRUN_GAUSSIAN $EXECgldas/gldas2gdas      1>&1 2>&2
export err=$?
$ERRSCRIPT || exit 10


# 5e) archive gldas results

if [ $OFFLINE_GLDAS = "YES" ]; then
 ${USHgldas}/gldas_archive.sh $gldas_symd $gldas_eymd 
 export err=$?
 $ERRSCRIPT || exit 11
else
 k=1; while [ $k -le 6 ]; do
  mv $input2/${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile${k}.nc  $input2/${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile${k}.nc_bfgldas
  cp sfc_data.tile${k}.nc  $input2/${iau_eymd}.${iau_ecyc}0000.sfcanl_data.tile${k}.nc  
  k=$((k+1))
 done
fi


#------------------------------------------------------------------
# Clean up before leaving
if [ $mkdata = "YES" ]; then rm -rf $DATA; fi

exit $err

