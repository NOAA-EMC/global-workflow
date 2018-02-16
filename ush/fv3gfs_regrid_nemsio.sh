#!/bin/sh

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
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = YES ] ; then
    echo $(date) EXECUTING $0 $* >&2
    set -x
fi

#-------------------------------------------------------
# Directories and paths
pwd=$(pwd)
DATA=${DATA:-$pwd}
NWPROD=${NWPROD:-$pwd}
HOMEgfs=${HOMEgfs:-$NWPROD}
FIX_DIR=${FIX_DIR:-$HOMEgfs/fix}
FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}
FIXfv3=${FIXfv3:-$FIX_DIR/fix_fv3_gmted2010}
REGRID_NEMSIO_EXEC=${REGRID_NEMSIO_EXEC:-$HOMEgfs/exec/regrid_nemsio}
REGRID_NEMSIO_TBL=${REGRID_NEMSIO_TBL:-$HOMEgfs/parm/parm_fv3diag/variable_table.txt}

CDATE=${CDATE:-2017011500}
CDUMP=${CDUMP:-"gdas"}
CASE=${CASE:-C768}
LEVS=${LEVS:-65}
GG=${GG:-gaussian}              # gaussian or regular lat-lon
res=$(echo $CASE | cut -c2-)
JCAP=${JCAP:-$((res*2-2))}
LATB=${LATB:-$((res*2))}
LONB=${LONB:-$((res*4))}

NEMSIO_OUT2DNAME=${NEMSIO_OUT2DNAME:-sfc.$CDATE}
NEMSIO_OUT3DNAME=${NEMSIO_OUT3DNAME:-atm.$CDATE}
DEBUG=${REGRID_NEMSIO_DEBUG:-".true."}

APRUN_REGRID_NEMSIO=${APRUN_REGRID_NEMSIO:-${APRUN:-""}}
NTHREADS_REGRID_NEMSIO=${NTHREADS_REGRID_NEMSIO:-${NTHREADS:-1}}

NMV=${NMV:-"/bin/mv"}

#-------------------------------------------------------
# IO specific parameters and error traps
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}

#--------------------------------------------------
# ESMF regrid weights and output variable table
weight_bilinear=${weight_bilinear:-$FIXfv3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB}_lat${LATB}.${GG}.bilinear.nc}
weight_neareststod=${weight_neareststod:-$FIXfv3/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB}_lat${LATB}.${GG}.neareststod.nc}

#-------------------------------------------------------
# Go to the directory where the history files are
cd $DATA || exit 8

#-------------------------------------------------------
# Create namelist
rm -f regrid-nemsio.input

cat > regrid-nemsio.input << EOF
&share
  debug=$DEBUG,
  ntrunc=$JCAP,
  nlons=$LONB,
  nlats=$LATB,
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
PDY=`echo $CDATE | cut -c1-8`
cyc=`echo  $CDATE | cut -c9-10`
PREFIX=${PREFIX:-"${CDUMP}.t${cyc}z."}
SUFFIX=${SUFFIX:-".nemsio"}
for ftype in atm sfc; do
    for file in `ls -1 ${ftype}.${CDATE}.fhr*`; do
        fhrchar=`echo $file | cut -d. -f3 | cut -c4-`
        $NMV $file ${PREFIX}${ftype}f${fhrchar}${SUFFIX}
    done
done

#------------------------------------------------------------------
set +x
if [ $VERBOSE = "YES" ] ; then
    echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
