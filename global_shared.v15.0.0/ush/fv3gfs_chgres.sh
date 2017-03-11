#!/bin/ksh

################################################################################
# UNIX Script Documentation Block
# Script name:         fv3gfs_chgres.sh
# Script description:  Remap FV3 forecasts on six tile in NetCDF to global Gaussian
#                      grid with NEMSIO output
#
# Author:   Fanglin Yang       Org: NCEP/EMC       Date: 2017-01-01
# Abstract:
#
# Script history log:
# 2017-01-01  Fanglin Yang  - First version built upon GFDL tcsh script.
# 2017-02-13  Rahul Mahajan - Port o Theia, standardize, generalize, remove hard-wired stuff
#
# Attributes:
#   Language: Portable Operating System Interface (POSIX) Shell
#   Machine: WCOSS-CRAY, Theia
################################################################################

#-------------------------------------------------------
#  Set environment.
export VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = YES ] ; then
  echo $(date) EXECUTING $0 $* >&2
  set -x
fi

#-------------------------------------------------------
# Directories and paths
export CASE=${CASE:-C768}
export COMOUT=${COMOUT:-$(pwd)}
export ATMANL=${ATMANL:-atmanl}
export SFCANL=${SFCANL:-sfcanl}

export BASE_GSM=${BASE_GSM:-/nwprod}
export FIX_FV3=${FIX_FV3:-$BASE_GSM/fix}
export FIX_AM=${FIX_AM:-$BASE_GSM/fix/fix_am}
export STMP=${STMP:-"/stmp"}
export GLOBAL_CHGRES_EXEC=${GLOBAL_CHGRES_EXEC:-$BASE_GSM/exec/global_chgres}

#-------------------------------------------------------
# IO specific parameters and error traps
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}

#-------------------------------------------------------
# Namelist and other options
export ntiles=${ntiles:-6}
export LEVS=${LEVS:-64}
export IDVC=${IDVC:-2}
export IDRT=${IDRT:-4}
export LANDICE_OPT=${LANDICE_OPT:-2}
export CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-3}
export NTRAC=${NTRAC:-3}
export IDSL=${IDSL:-1}
export LSOIL=${LSOIL:-4}
export IVSSFC=0
export CHGRESVARS="use_ufo=.true.,IALB=0,ntrac=3,idvc=2,idvt=21,idsl=1,IDVM=1"

res=`echo $CASE |cut -c 2-`
export LONB_SFC=$res
export LATB_SFC=$res
export LONB_ATM=$((res*4))
export LATB_ATM=$((res*2))
if [ $res -gt 768 -o $gtype = stretch ]; then
 export LONB_ATM=3072
 export LATB_ATM=1536
fi
echo "LONB_SFC=$LONB_SFC, LATB_SFC=$LATB_SFC"
echo "LONB_ATM=$LONB_ATM, LATB_ATM=$LATB_ATM"

#-------------------------------------------------------
# Working directory
export DATA=${DATA:-$STMP/fv3_chgres}
if [ ! -d $DATA ]; then mkdir -p $DATA; fi
cd $DATA || exit 8

#-------------------------------------------------------
# Fix files
$NLN ${SIGLEVEL:-$FIX_AM/global_hyblev.l${LEVS}.txt} chgres.inp.siglevel
$NLN $FIX_AM/global_o3clim.txt                       chgres.inp.o3clim

export FNGLAC=$FIX_AM/global_glacier.2x2.grb
export FNMXIC=$FIX_AM/global_maxice.2x2.grb
export FNTSFC=$FIX_AM/cfs_oi2sst1x1monclim19822001.grb
export FNSNOC=$FIX_AM/global_snoclim.1.875.grb
export FNZORC=${FNZORC:-sib}
export FNALBC=$FIX_AM/global_albedo4.1x1.grb
export FNAISC=$FIX_AM/cfs_ice1x1monclim19822001.grb
export FNTG3C=$FIX_AM/global_tg3clim.2.6x1.5.grb
export FNVEGC=$FIX_AM/global_vegfrac.0.144.decpercent.grb
export FNVETC=$FIX_AM/global_vegtype.1x1.grb
export FNSOTC=$FIX_AM/global_soiltype.1x1.grb
export FNSMCC=$FIX_AM/global_soilmcpc.1x1.grb
export FNVMNC=$FIX_AM/global_shdmin.0.144x0.144.grb
export FNVMXC=$FIX_AM/global_shdmax.0.144x0.144.grb
export FNSLPC=$FIX_AM/global_slope.1x1.grb
export FNABSC=$FIX_AM/global_snoalb.1x1.grb
export FNMSKH=$FIX_AM/seaice_newland.grb

if [ $LANDICE_OPT -eq 3 -o $LANDICE_OPT -eq 4 ]; then
  export LANDICE=.false.
else
  export LANDICE=.true.
fi

#-------------------------------------------------------
# Namelists and input files
rm -f fort.35
cat << EOF > fort.35
&NAMSFC
  FNGLAC='${FNGLAC}'
  FNMXIC='${FNMXIC}'
  FNTSFC='${FNTSFC}'
  FNSNOC='${FNSNOC}'
  FNZORC='${FNZORC}'
  FNALBC='${FNALBC}'
  FNAISC='${FNAISC}'
  FNTG3C='${FNTG3C}'
  FNVEGC='${FNVEGC}'
  FNVETC='${FNVETC}'
  FNSOTC='${FNSOTC}'
  FNSMCC='${FNSMCC}'
  FNVMNC='${FNVMNC}'
  FNVMXC='${FNVMXC}'
  FNSLPC='${FNSLPC}'
  FNABSC='${FNABSC}'
  FNMSKH='${FNMSKH}'
  FNTSFA=''
  FNACNA=''
  FNSNOA=''
  LDEBUG=.false.
  LANDICE=$LANDICE
/
EOF

rm -f fort.81
cat << EOF > fort.81
&soil_parameters
  soil_src_input = "zobler"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                0.404, 0.439, 0.421
  beta_input  =   4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_input  =   0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_input = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
  soil_src_output = "zobler"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                 0.404, 0.439, 0.421
  beta_output  =  4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_output  =  0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_output = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                 0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
/
&veg_parameters
  veg_src_input = "sib"
  veg_src_output = "sib"
  salp_output= -999.
  snup_output= -999.
/
&options
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT}
  LANDICE_OPT=${LANDICE_OPT}
/
EOF

#-------------------------------------------------------
# Ensure global_chgres is running with threads and has adequate stacksize
export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-24}
export OMP_STACKSIZE=${OMP_STACKSIZE_CH:-${OMP_STACKSIZE:-2048000}}

#-------------------------------------------------------
# First CHGRES the 3D atmospheric tiles

# Initial conditions to CHGRES
$NLN $ATMANL chgres.inp.sig
$NLN NULL    chgres.inp.sfc

for tile in `seq 1 $ntiles`; do
  $NLN $FIX_FV3/$CASE/${CASE}_grid.tile${tile}.nc     chgres.fv3.grd.t$tile
  $NLN $FIX_FV3/$CASE/${CASE}_oro_data.tile${tile}.nc chgres.fv3.orog.t$tile
done

$APRUN $GLOBAL_CHGRES_EXEC << EOF '1>&1' '2>&2'
&NAMCHG
  LEVS=$LEVS, LONB=$LONB_ATM, LATB=$LATB_ATM,
  NTRAC=$NTRAC, IDVC=$IDVC, IDSL=$IDSL,
  LSOIL=$LSOIL, IVSSFC=$IVSSFC, IDRT=$IDRT,
  ntiles=$ntiles, $CHGRESVARS
/
EOF

export ERR=$?
export err=$ERR
$ERRSCRIPT || exit $err

rm -f chgres.fv3.grd.t*

#-------------------------------------------------------
# Next CHGRES the 2D surface tiles

# Initial conditions to CHGRES
$NLN NULL    chgres.inp.sig
$NLN $SFCANL chgres.inp.sfc

for tile in `seq 1 $ntiles`; do

  $NLN $FIX_FV3/$CASE/${CASE}_grid.tile$tile.nc     chgres$tile
  $NLN $FIX_FV3/$CASE/${CASE}_oro_data.tile$tile.nc chgres.fv3.orog.t$tile

  $APRUN $GLOBAL_CHGRES_EXEC << EOF '1>&1' '2>&2'
&NAMCHG
  LEVS=$LEVS, LONB=$LONB_SFC, LATB=$LATB_SFC,
  NTRAC=$NTRAC, IDVC=$IDVC, IDSL=$IDSL,
  LSOIL=$LSOIL, IVSSFC=$IVSSFC, IDRT=$IDRT,
  ntiles=$ntiles, tile_num=$tile, $CHGRESVARS
/
EOF

  export ERR=$?
  export err=$ERR
  $ERRSCRIPT || exit $err

done

#-------------------------------------------------------
# copy data to output directory
$NMV gfs_ctrl.nc $COMOUT/gfs_ctrl.nc
for tile in `seq 1 $ntiles`; do
  $NMV gfs_data.tile${tile}.nc $COMOUT/gfs_data.tile${tile}.nc
  $NMV out.sfc.tile${tile}.nc  $COMOUT/sfc_data.tile${tile}.nc
done

#------------------------------------------------------------------
# Clean up before leaving
cd $COMOUT
if [ ${KEEPDATA:-NO} = "NO" ]; then rm -rf $DATA; fi

#------------------------------------------------------------------
set +x
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
