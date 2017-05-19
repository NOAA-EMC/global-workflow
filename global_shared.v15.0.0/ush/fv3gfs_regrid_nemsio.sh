#!/bin/ksh

################################################################################
# UNIX Script Documentation Block
# Script name:         fv3gfs_regrid_nemsio.sh
# Script description:  Remap FV3 forecasts on six tile in NetCDF to global Gaussian
#                      grid with NEMSIO output
#
# $Id$
#
# Author:   Fanglin Yang       Org: NCEP/EMC       Date: 2016-12-01
# Abstract: regrid_nemsio.fd provided by Jeffrey.S.Whitaker OAR/ESRL
#
# Script history log:
# 2016-12-01  Fanglin Yang
# 2017-02-13  Rahul Mahajan
#
# Attributes:
#   Language: Portable Operating System Interface (POSIX) Shell
#   Machine: WCOSS-CRAY, Theia
################################################################################

#  Set environment.
export VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = YES ] ; then
  echo $(date) EXECUTING $0 $* >&2
  set -x
fi

#-------------------------------------------------------
# Directories and paths
pwd=$(pwd)
export DATA=${DATA:-$pwd}
export NWPROD=${NWPROD:-$pwd}
export BASE_GSM=${BASE_GSM:-$NWPROD}
export FIX_DIR=${FIX_DIR:-$BASE_GSM/fix}
export FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}
export FIX_FV3=${FIX_FV3:-$FIX_DIR/fix_fv3}
export REGRID_NEMSIO_EXEC=${REGRID_NEMSIO_EXEC:-$BASE_GSM/exec/regrid_nemsio}
export REGRID_NEMSIO_TBL=${REGRID_NEMSIO_TBL:-$BASE_GSM/parm/parm_fv3diag/variable_table.txt}

export CDATE=${CDATE:-2017011500}
export CDUMP=${CDUMP:-"gdas"}
export CASE=${CASE:-C768}
export LEVS=${LEVS:-64}
export GG=${GG:-gaussian}              # gaussian or regular lat-lon
export JCAP=${JCAP:-$((`echo $CASE | cut -c 2-`*2-2))}
export NLAT=${NLAT:-$((`echo $CASE | cut -c 2-`*2))}
export NLON=${NLON:-$((`echo $CASE | cut -c 2-`*4))}

export NEMSIO_OUT2DNAME=${NEMSIO_OUT2DNAME:-sfc.$CDATE}
export NEMSIO_OUT3DNAME=${NEMSIO_OUT3DNAME:-atm.$CDATE}
export DEBUG=${REGRID_NEMSIO_DEBUG:-".true."}

export APRUN_REGRID_NEMSIO=${APRUN_REGRID_NEMSIO:-${APRUN:-""}}
export NTHREADS_REGRID_NEMSIO=${NTHREADS_REGRID_NEMSIO:-${NTHREADS:-1}}

export NCO_NAMING_CONV=${NCO_NAMING_CONV:-"NO"}
export NMV=${NMV:-"/bin/mv"}

#-------------------------------------------------------
# IO specific parameters and error traps
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}

#--------------------------------------------------
# ESMF regrid weights and output variable table
export weight_bilinear=${weight_bilinear:-$FIX_FV3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${NLON}_lat${NLAT}.${GG}.bilinear.nc}
export weight_neareststod=${weight_neareststod:-$FIX_FV3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${NLON}_lat${NLAT}.${GG}.neareststod.nc}

#-------------------------------------------------------
# Go to the directory where the history files are
cd $DATA || exit 8

#-------------------------------------------------------
if [ $NCO_NAMING_CONV = "YES" ]; then
  export NEMSIO_OUT2DNAME=sfc.$CDATE
  export NEMSIO_OUT3DNAME=atm.$CDATE
fi

#-------------------------------------------------------
# Create namelist
rm -f regrid-nemsio.input

cat > regrid-nemsio.input << EOF
&share
  debug=$DEBUG,
  ntrunc=$JCAP,
  nlons=$NLON,
  nlats=$NLAT,
  datapathout2d='$NEMSIO_OUT2DNAME',
  datapathout3d='$NEMSIO_OUT3DNAME',
  analysis_filename='fv3_history.tile1.nc','fv3_history.tile2.nc','fv3_history.tile3.nc','fv3_history.tile4.nc','fv3_history.tile5.nc','fv3_history.tile6.nc',
  analysis_filename2d='fv3_history2d.tile1.nc','fv3_history2d.tile2.nc','fv3_history2d.tile3.nc','fv3_history2d.tile4.nc','fv3_history2d.tile5.nc','fv3_history2d.tile6.nc',
  forecast_timestamp='${CDATE}',
  variable_table='$REGRID_NEMSIO_TBL',
  nemsio_opt3d='bin4',
  nemsio_opt2d='bin4'
/

&interpio
  esmf_bilinear_filename='$weight_bilinear',
  esmf_neareststod_filename='$weight_neareststod',
  gfs_hyblevs_filename='$FIX_AM/global_hyblev.l$LEVS.txt'
/
EOF

#------------------------------------------------------------------
export OMP_NUM_THREADS=$NTHREADS_REGRID_NEMSIO
$APRUN_REGRID_NEMSIO $REGRID_NEMSIO_EXEC

export ERR=$?
export err=$ERR
$ERRSCRIPT || exit $err

rm -f regrid-nemsio.input

#------------------------------------------------------------------
if [ $NCO_NAMING_CONV = "YES" ]; then
  cymd=`echo $CDATE | cut -c1-8`
  chh=`echo  $CDATE | cut -c9-10`
  export PREFIX=${PREFIX:-"${CDUMP}.t${chh}z."}
  export SUFFIX=${SUFFIX:-".nemsio"}
  for ftype in atm sfc; do
    for file in `ls -1 ${ftype}.${CDATE}.fhr*`; do
      fhrchar=`echo $file | cut -d. -f3 | cut -c4-`
      $NMV $file ${PREFIX}${ftype}f${fhrchar}${SUFFIX}
    done
  done
fi

#------------------------------------------------------------------
set +x
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
