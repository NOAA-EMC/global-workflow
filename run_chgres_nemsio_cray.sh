#!/bin/sh

#-----------------------------------------------------------------
# Run chgres using input data in nemsio format.
#-----------------------------------------------------------------

#BSUB -L /bin/sh
#BSUB -P FV3GFS-T2O
#BSUB -oo log.chgres
#BSUB -eo log.chgres
#BSUB -J chgres_nemsio
#BSUB -q debug
#BSUB -M 2000
#BSUB -W 00:05
#BSUB -extsched 'CRAYLINUX[]'

set -ax

 . $MODULESHOME/init/sh
module load PrgEnv-intel

export NODES=1

export OMP_NUM_THREADS_CH=24
export KMP_AFFINITY=disabled
export APRUNC="aprun -n 1 -N 1 -j 1 -d $OMP_NUM_THREADS_CH -cc depth"

# input data nemsio format
export CDATE=2017041200
export COMROT=/gpfs/hps/ptmp/emc.glopara/prnemsrn

export BASEDIR=/gpfs/hps/emc/global/noscrub/George.Gayno/fv3gfs/branches/chgres_nsst_netcdf/global_shared.v15.0.0

# work directories
export SAVDIR=/gpfs/hps/stmp/$LOGNAME/chgres_save
rm -fr $SAVDIR
mkdir -p $SAVDIR
export DATA=/gpfs/hps/stmp/$LOGNAME/chgres_work
rm -fr $DATA
mkdir -p $DATA

# Set chgres executable and script

export CHGRESEXEC=$BASEDIR/exec/global_chgres
export CHGRESSH=$BASEDIR/ush/global_chgres.sh

export VERBOSE=YES

export CRES=768
export LEVS=64
export LSOIL=4
export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=2

# fixed fields describing fv3 grid
export FIXfv3=$BASEDIR/fix/C${CRES}
export FV3GRID_TILE1=$FIXfv3/C${CRES}_grid.tile1.nc
export FV3GRID_TILE2=$FIXfv3/C${CRES}_grid.tile2.nc
export FV3GRID_TILE3=$FIXfv3/C${CRES}_grid.tile3.nc
export FV3GRID_TILE4=$FIXfv3/C${CRES}_grid.tile4.nc
export FV3GRID_TILE5=$FIXfv3/C${CRES}_grid.tile5.nc
export FV3GRID_TILE6=$FIXfv3/C${CRES}_grid.tile6.nc
export FV3OROG_TILE1=$FIXfv3/C${CRES}_oro_data.tile1.nc
export FV3OROG_TILE2=$FIXfv3/C${CRES}_oro_data.tile2.nc
export FV3OROG_TILE3=$FIXfv3/C${CRES}_oro_data.tile3.nc
export FV3OROG_TILE4=$FIXfv3/C${CRES}_oro_data.tile4.nc
export FV3OROG_TILE5=$FIXfv3/C${CRES}_oro_data.tile5.nc
export FV3OROG_TILE6=$FIXfv3/C${CRES}_oro_data.tile6.nc

# to use new albedo, soil/veg type
export IALB=1
export FIXgsm=/gpfs/hps/emc/global/noscrub/George.Gayno/global_shared.v14.1.0_backup/fix/fix_am
export FNSMCC=$FIXgsm/global_soilmgldas.t1534.3072.1536.grb
export FNSOTC=$FIXgsm/global_soiltype.statsgo.t1534.3072.1536.rg.grb
export SOILTYPE_INP=statsgo
export SOILTYPE_OUT=statsgo
export FNVETC=$FIXgsm/global_vegtype.igbp.t1534.3072.1536.rg.grb
export VEGTYPE_OUT=igbp
export VEGTYPE_INP=igbp
export FNABSC=$FIXgsm/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb
export FNALBC=$FIXgsm/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb
# needed for facsf and facwf
export FNALBC2=$FIXgsm/global_albedo4.1x1.grb
export FNZORC=igbp

#------------------------------------------------
# Convert atmospheric file.
#------------------------------------------------

#export CHGRESVARS="use_ufo=.false.,nst_anl=.false.,idvc=2,idvt=21,idsl=1,IDVM=0,nopdpvv=.true."
#export SIGINP=$COMROT/gfnanl.gfs.$CDATE
#export SFCINP=NULL
#export NSTINP=NULL

#$CHGRESSH
#rc=$?
#if [[ $rc -ne 0 ]] ; then
#  echo "***ERROR*** rc= $rc"
#  exit
#fi

#mv ${DATA}/gfs*.nc $SAVDIR

#---------------------------------------------------
# Convert surface and nst files one tile at a time.
#---------------------------------------------------

export CHGRESVARS="use_ufo=.false.,nst_anl=.false.,idvc=2,idvt=21,idsl=1,IDVM=0,nopdpvv=.true."
export SIGINP=NULL
export SFCINP=$COMROT/sfnanl.gfs.$CDATE
export NSTINP=$COMROT/nsnanl.gfs.$CDATE

for tile in '1' '2' '3' '4' '5' '6'
do

 export TILE_NUM=$tile

 $CHGRESSH
 rc=$?
 if [[ $rc -ne 0 ]] ; then
   echo "***ERROR*** rc= $rc"
   exit
 fi

 mv ${DATA}/*.nc $SAVDIR

done
