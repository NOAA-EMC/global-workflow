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

export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS_CH:-24}
export APRUNC=${APRUNC:-"time"}

export CASE=${CASE:-C96}                     # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072
export CRES=`echo $CASE | cut -c 2-`
export CDATE=${CDATE:-${cdate:-2017031900}}  # format yyyymmddhh yyyymmddhh ...
export CDUMP=${CDUMP:-gfs}                   # gfs or gdas
export LEVS=${LEVS:-65}
export LSOIL=${LSOIL:-4}
export REGIONAL=${REGIONAL:-0}               # default is to assume uniform grid, which is REGIONAL=0
                                             # REGIONAL=1 - generate data and boundary (for regional case)
                                             # REGIONAL=2 - generate boundaries only (for regional case)

export VERBOSE=YES
pwd=$(pwd)
export NWPROD=${NWPROD:-$pwd}
export HOMEgfs=${HOMEgfs:-$NWPROD/gfs.v15.0.0}
export FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
export FIXam=${FIXam:-$HOMEgfs/fix/fix_am}
export CHGRESEXEC=$HOMEgfs/exec/global_chgres
export CHGRESSH=$HOMEgfs/ush/global_chgres.sh

# Location of initial conditions for GFS (before chgres) and FV3 (after chgres)
export INIDIR=${INIDIR:-$pwd}
export OUTDIR=${OUTDIR:-$pwd/INPUT}
mkdir -p $OUTDIR

export gtype=${gtype:-uniform}	    # grid type = uniform, stretch, nest or stand alone regional

if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
  export ntiles=6
elif [ $gtype = stretch ]; then
  echo "creating stretched ICs"
  export ntiles=6
elif [ $gtype = nest ]; then
  echo "creating nested ICs"
  export ntiles=7
elif [ $gtype = regional ]; then
  echo "creating standalone regional ICs"
  export ntiles=1
  export TILE_NUM=7
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi

#---------------------------------------------------------------

# Temporary rundirectory
export DATA=${DATA:-${RUNDIR:-$pwd/rundir$$}}
if [ ! -s $DATA ]; then mkdir -p $DATA; fi
cd $DATA || exit 8

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

if [ $ictype = oldgfs ]; then   # input data is old spectral sigio format.

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
 export VEGTYPE_INP=sib
 export nopdpvv=.false.

 #--sigio to user defined lat-lon gaussian grid
 JCAP_CASE=$((CRES*2-2))
 LONB_ATM=$((CRES*4))
 LATB_ATM=$((CRES*2))
 LONB_SFC=$((CRES*4))
 LATB_SFC=$((CRES*2))
 if [ $CRES -gt 768 -o $gtype = stretch -o $gtype = nest ]; then
   JCAP_CASE=1534
   LONB_ATM=3072
   LATB_ATM=1536
   LONB_SFC=3072
   LATB_SFC=1536
 fi

elif [ $ictype = opsgfs ]; then   # input data is nemsio format.

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

 export SOILTYPE_INP=statsgo
 export VEGTYPE_INP=igbp
 export nopdpvv=.true.

 LONB_ATM=0   # not used for
 LATB_ATM=0   # ops files
 JCAP_CASE=$((CRES*2-2))
 LONB_SFC=$((CRES*4))
 LATB_SFC=$((CRES*2))
 if [ $CRES -gt 768 -o $gtype = stretch -o $gtype = nest ]; then
   JCAP_CASE=1534
   LONB_SFC=3072
   LATB_SFC=1536
 fi
fi  # is input data old or new format?


# to use new albedo, soil/veg type
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=${LANDICE_OPT:-2}
export IALB=1
export SOILTYPE_OUT=statsgo
export VEGTYPE_OUT=igbp
export FNZORC=igbp

export SIGLEVEL=${FIXam}/global_hyblev.l${LEVS}.txt
if [ $LEVS = 128 ]; then export SIGLEVEL=${FIXam}/global_hyblev.l${LEVS}B.txt; fi
export FNGLAC=${FIXam}/global_glacier.2x2.grb
export FNMXIC=${FIXam}/global_maxice.2x2.grb
export FNTSFC=${FIXam}/cfs_oi2sst1x1monclim19822001.grb
export FNSNOC=${FIXam}/global_snoclim.1.875.grb
export FNALBC2=${FIXam}/global_albedo4.1x1.grb
export FNAISC=${FIXam}/cfs_ice1x1monclim19822001.grb
export FNTG3C=${FIXam}/global_tg3clim.2.6x1.5.grb
export FNVEGC=${FIXam}/global_vegfrac.0.144.decpercent.grb
export FNVMNC=${FIXam}/global_shdmin.0.144x0.144.grb
export FNVMXC=${FIXam}/global_shdmax.0.144x0.144.grb
export FNSLPC=${FIXam}/global_slope.1x1.grb
export FNMSKH=${FIXam}/seaice_newland.grb
export FNSMCC=$FIXam/global_soilmgldas.statsgo.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.grb
export FNSOTC=$FIXam/global_soiltype.statsgo.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNVETC=$FIXam/global_vegtype.igbp.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNABSC=$FIXam/global_mxsnoalb.uariz.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb
export FNALBC=$FIXam/global_snowfree_albedo.bosu.t${JCAP_CASE}.${LONB_SFC}.${LATB_SFC}.rg.grb

#
# For a regional run, if REGIONAL=2 (generate boundary data only) this script is called multiple times
# so that each boundary time other than hour 0 will be done individually. This allows multiple instances
# of chgres to execute simultaneously
#

if [ $REGIONAL -ne 2 ]; then           # REGIONAL -ne 2 is for uniform and regional hour 0

#------------------------------------------------
# Convert atmospheric file.
#------------------------------------------------

  export CHGRESVARS="use_ufo=.false.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export SIGINP=$ATMANL
  export SFCINP=NULL
  export NSTINP=NULL
  export JCAP=$JCAP_CASE
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
  if [ $gtype = regional ]; then
    mv ${DATA}/gfs_bndy.tile7.nc $OUTDIR/gfs_bndy.tile7.000.nc
  fi

#---------------------------------------------------
# Convert surface and nst files one tile at a time.
#---------------------------------------------------

  export CHGRESVARS="use_ufo=.true.,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export SIGINP=NULL
  export SFCINP=$SFCANL
  export NSTINP=$NSTANL
  export JCAP=$JCAP_CASE
  export LATB=$LATB_SFC
  export LONB=$LONB_SFC

  if [ $gtype = regional ]; then
    $CHGRESSH
    mv ${DATA}/out.sfc.tile${TILE_NUM}.nc $OUTDIR/sfc_data.tile${TILE_NUM}.nc
    rc=$?
    if [[ $rc -ne 0 ]] ; then
      echo "***ERROR*** rc= $rc"
      exit $rc
    fi
  else
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
  fi

else # REGIONAL = 2, just generate boundary data

  export CHGRESVARS="use_ufo=.false.,nst_anl=$nst_anl,idvc=2,nvcoord=2,idvt=21,idsl=1,IDVM=0,nopdpvv=$nopdpvv"
  export ATMANL=$INIDIR/${CDUMP}.t${cyc}z.atmf${bchour}.nemsio
  export SIGINP=$ATMANL
  export SFCINP=NULL
  export NSTINP=NULL
  export LATB=$LATB_ATM
  export LONB=$LONB_ATM

#
# run chgres and check rc to make sure it was successful
#

  $CHGRESSH
  rc=$?
  if [[ $rc -ne 0 ]] ; then
    echo "***ERROR*** rc= $rc"
    exit $rc
  fi
 
  mv ${DATA}/gfs_bndy.tile7.nc $OUTDIR/gfs_bndy.tile7.${bchour}.nc

fi
 
exit 0
