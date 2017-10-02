#!/bin/ksh
set -ax
#-------------------------------------------------------------------------------------------------
# Makes ICs on fv3 globally uniform cubed-sphere grid using operational GFS initial conditions.
# Fanglin Yang, 09/30/2016
#  This script is created based on the C-shell scripts fv3_gfs_preproc/IC_scripts/DRIVER_CHGRES.csh
#  and submit_chgres.csh provided by GFDL.  APRUN and environment variables are added to run on
#  WCOSS CRAY.  Directory and file names are standaridized to follow NCEP global model convention.
#  This script calls fv3gfs_chgres.sh.
# Fanglin Yang and George Gayno, 02/08/2017
#  Modified to use the new CHGRES George Gayno developed.
# Fanglin Yang 03/08/2017
#  Generalized and streamlined the script and enabled to run on multiple platforms.
# Fanglin Yang 03/20/2017
#  Added option to process NEMS GFS initial condition which contains new land datasets.
#  Switch to use ush/global_chgres.sh.
#-------------------------------------------------------------------------------------------------

export machine=${machine:-WCOSS_C}
export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS_CH:-24}
export APRUNC=${APRUNC:-"time"}

export CASE=${CASE:-C96}                     # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072
export CRES=`echo $CASE | cut -c 2-`
export CDATE=${CDATE:-${cdate:-2017031900}}  # format yyyymmddhh yyyymmddhh ...
export CDUMP=${CDUMP:-gfs}                   # gfs or gdas
export LEVS=${LEVS:-65}
export LSOIL=${LSOIL:-4}

export VERBOSE=YES
pwd=$(pwd)
export NWPROD=${NWPROD:-$pwd}
export BASE_GSM=${BASE_GSM:-$NWPROD/global_shared}
export FIXgsm=${FIXgsm:-$BASE_GSM/fix/fix_am}
export FIXfv3=${FIXfv3:-$BASE_GSM/fix/fix_fv3}
export CHGRESEXEC=$BASE_GSM/exec/global_chgres
export CHGRESSH=$BASE_GSM/ush/global_chgres.sh

# Location of initial conditions for GFS (before chgres) and FV3 (after chgres)
export INIDIR=${INIDIR:-$pwd}
export OUTDIR=${OUTDIR:-$pwd/INPUT}
mkdir -p $OUTDIR

#---------------------------------------------------------
export gtype=${gtype:-uniform}	          # grid type = uniform, stretch, or nest

if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
  export name=${CASE}
  export ntiles=6
elif [ $gtype = stretch ]; then
  export stetch_fac=       	                 # Stretching factor for the grid
  export rn=`expr $stetch_fac \* 10 `
  export name=${CASE}r${rn}       		 # identifier based on refined location (same as grid)
  export ntiles=6
  echo "creating stretched ICs"
elif [ $gtype = nest ]; then
  export stetch_fac=1.5  	                         # Stretching factor for the grid
  export rn=`expr $stetch_fac \* 10 `
  export refine_ratio=3   	                 # Specify the refinement ratio for nest grid
  export name=${CASE}r${rn}n${refine_ratio}      # identifier based on nest location (same as grid)
  export ntiles=7
  echo "creating nested ICs"
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, or nest"
fi

#---------------------------------------------------------------

# Temporary rundirectory
export DATA=${DATA:-${RUNDIR:-$pwd/rundir$$}}
if [ ! -s $DATA ]; then mkdir -p $DATA; fi
cd $DATA || exit 8

export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=2
export SIGLEVEL=${FIXgsm}/global_hyblev.l${LEVS}.txt
if [ $LEVS = 128 ]; then export SIGLEVEL=${FIXgsm}/global_hyblev.l${LEVS}B.txt; fi
export FNGLAC=${FIXgsm}/global_glacier.2x2.grb
export FNMXIC=${FIXgsm}/global_maxice.2x2.grb
export FNTSFC=${FIXgsm}/cfs_oi2sst1x1monclim19822001.grb
export FNSNOC=${FIXgsm}/global_snoclim.1.875.grb
export FNALBC=${FIXgsm}/global_albedo4.1x1.grb
export FNALBC2=${FIXgsm}/global_albedo4.1x1.grb
export FNAISC=${FIXgsm}/cfs_ice1x1monclim19822001.grb
export FNTG3C=${FIXgsm}/global_tg3clim.2.6x1.5.grb
export FNVEGC=${FIXgsm}/global_vegfrac.0.144.decpercent.grb
export FNVETC=${FIXgsm}/global_vegtype.1x1.grb
export FNSOTC=${FIXgsm}/global_soiltype.1x1.grb
export FNSMCC=${FIXgsm}/global_soilmcpc.1x1.grb
export FNVMNC=${FIXgsm}/global_shdmin.0.144x0.144.grb
export FNVMXC=${FIXgsm}/global_shdmax.0.144x0.144.grb
export FNSLPC=${FIXgsm}/global_slope.1x1.grb
export FNABSC=${FIXgsm}/global_snoalb.1x1.grb
export FNMSKH=${FIXgsm}/seaice_newland.grb


export ymd=`echo $CDATE | cut -c 1-8`
export cyc=`echo $CDATE | cut -c 9-10`

# Determine if we are current operations with NSST or the one before that
if [ ${ATMANL:-"NULL"} = "NULL" ]; then
 if [ -s ${INIDIR}/nsnanl.${CDUMP}.$CDATE -o -s ${INIDIR}/${CDUMP}.t${cyc}z.nstanl.nemsio ]; then
  ictype='opsgfs'
 else
  ictype='oldgfs'
 fi
else
 if [ ${NSTANL:-"NULL"} = "NULL" ]; then
  ictype='oldgfs'
 else
  ictype='opsgfs'
 fi
fi

if [ $ictype = oldgfs ]; then
 if [ ${ATMANL:-"NULL"} = "NULL" ]; then
  if [ -s ${INIDIR}/siganl.${CDUMP}.$CDATE ]; then
   export ATMANL=$INIDIR/siganl.${CDUMP}.$CDATE
   export SFCANL=$INIDIR/sfcanl.${CDUMP}.$CDATE
  else
   export ATMANL=$INIDIR/${CDUMP}.t${cyc}z.sanl
   export SFCANL=$INIDIR/${CDUMP}.t${cyc}z.sfcanl
  fi
 fi
 export NSTANL="NULL"
 export SOILTYPE_INP=zobler
 export SOILTYPE_OUT=zobler
 export VEGTYPE_INP=sib
 export VEGTYPE_OUT=sib
 export FNZORC=sib
 export nopdpvv=.false.

 #--sigio to user defined lat-lon gaussian grid
 export LONB_SFC=$CRES
 export LATB_SFC=$CRES
 export LONB_ATM=$((CRES*4))
 export LATB_ATM=$((CRES*2))
 if [ $CRES -gt 768 -o $gtype = stretch -o $gtype = nest ]; then
  export LONB_ATM=3072
  export LATB_ATM=1536
 fi

elif [ $ictype = opsgfs ]; then
 if [ ${ATMANL:-"NULL"} = "NULL" ]; then
  if [ -s ${INIDIR}/gfnanl.${CDUMP}.$CDATE ]; then
   export ATMANL=$INIDIR/gfnanl.${CDUMP}.$CDATE
   export SFCANL=$INIDIR/sfnanl.${CDUMP}.$CDATE
   export NSTANL=$INIDIR/nsnanl.${CDUMP}.$CDATE
  else
   export ATMANL=$INIDIR/${CDUMP}.t${cyc}z.atmanl.nemsio
   export SFCANL=$INIDIR/${CDUMP}.t${cyc}z.sfcanl.nemsio
   export NSTANL=$INIDIR/${CDUMP}.t${cyc}z.nstanl.nemsio
  fi
 fi
 # to use new albedo, soil/veg type
 export IALB=1
 export FNSMCC=$FIXgsm/global_soilmgldas.t1534.3072.1536.grb
 export FNSOTC=$FIXgsm/global_soiltype.statsgo.t1534.3072.1536.rg.grb
 export SOILTYPE_INP=statsgo
 export SOILTYPE_OUT=statsgo
 export FNVETC=$FIXgsm/global_vegtype.igbp.t1534.3072.1536.rg.grb
 export VEGTYPE_INP=igbp
 export VEGTYPE_OUT=igbp
 export FNABSC=$FIXgsm/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb
 export FNALBC=$FIXgsm/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb
 # needed for facsf and facwf
 export FNALBC2=$FIXgsm/global_albedo4.1x1.grb
 export FNZORC=igbp
 export nopdpvv=.true.
fi


#------------------------------------------------
# Convert atmospheric file.
#------------------------------------------------
export CHGRESVARS="use_ufo=.false.,idvc=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
export SIGINP=$ATMANL
export SFCINP=NULL
export NSTINP=NULL
export LATB=$LATB_ATM
export LONB=$LONB_ATM

$CHGRESSH
rc=$?
if [[ $rc -ne 0 ]] ; then
 echo "***ERROR*** rc= $rc"
 exit $rc
fi

mv ${DATA}/gfs_data.tile*.nc  $OUTDIR/.
mv ${DATA}/gfs_ctrl.nc        $OUTDIR/.

#---------------------------------------------------
# Convert surface and nst files one tile at a time.
#---------------------------------------------------
export CHGRESVARS="use_ufo=.true.,idvc=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
export SIGINP=NULL
export SFCINP=$SFCANL
export NSTINP=$NSTANL
export LATB=$LATB_SFC
export LONB=$LONB_SFC

tile=1
while [ $tile -le $ntiles ]; do
 export TILE_NUM=$tile
 $CHGRESSH
 rc=$?
 if [[ $rc -ne 0 ]] ; then
  echo "***ERROR*** rc= $rc"
  exit $rc
 fi
 mv ${DATA}/out.sfc.tile${tile}.nc $OUTDIR/sfc_data.tile${tile}.nc
 tile=`expr $tile + 1 `
done

exit 0
