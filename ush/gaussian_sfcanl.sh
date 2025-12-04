#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         gaussian_sfcanl.sh
# Script description:  Makes a global gaussian grid surface analysis file
#
# Author:        George Gayno       Org: NP23         Date: 2018-01-30
#
# Abstract: This script makes a global gaussian grid surface analysis from
#           fv3gfs surface analysis tiles.  The gaussian grid resolution is
#           the gaussian equivalent of the history file resolution (may be
#           different than restart resolution).
#
# Script history log:
# 2018-01-30  Gayno  initial script
# 2019-1030   Gayno  updates to output analysis file in netcdf or nemsio
#
# Usage:  gaussian_sfcanl.sh
#
#   Imported Shell Variables:
#     CASE          Forecast model and restart resolution.  Defaults to C768.
#     CASE_HIST     History file output resolution.  Defaults to $CASE.
#     DONST         Process NST fields when 'yes'.  Default is 'no'.
#     FIXWGTS       Weight file to use for interpolation
#     COMOUT        Output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     GAUSFCANLEXE  Program executable.
#                   Defaults to $EXECgfs/gaussian_sfcanl.x
#     gfs_ver       Version number of gfs directory.  Default is
#                   v15.0.0.
#     OMP_NUM_
#     THREADS_SFC   Number of omp threads to use.  Default is 1.
#     APRUNSFC      Machine specific command to invoke the executable.
#                   Default is none.
#
#   Exported Shell Variables:
#     pgm
#     err
#
#   Modules and files referenced:
#     scripts    :
#
#     programs   : $GAUSFCANLEXE
#
#     fixed data : ${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile*.nc
#                  ${FIXWGTS}
#                  ${FIXgfs}/am/global_hyblev.l65.txt
#
#     input data : ${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile*.nc
#                  ${COMIN_ATMOS_ANALYSIS}/increment.sfc.i006.tile${i}.nc"
#
#     output data: $COMOUT/${APREFIX}analysis.sfc.a006.nc
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#    2 Environment variable.
#    3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

CASE=${CASE:-C768}
CASE_HIST=${CASE_HIST:-${CASE}}
resh=${CASE_HIST:1}
LONB_CASE=$((resh * 4))
LATB_CASE=$((resh * 2))
LONB_SFC=${LONB_SFC:-${LONB_CASE}}
LATB_SFC=${LATB_SFC:-${LATB_CASE}}
DONST=${DONST:-"NO"}
LEVS=${LEVS:-64}
LEVSP1=$((LEVS + 1))
FIXWGTS=${FIXWGTS:-${FIXorog}/${CASE}/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB_SFC}_lat${LATB_SFC}.gaussian.neareststod.nc}

#  Filenames.
GAUSFCANLEXE=${GAUSFCANLEXE:-${EXECgfs}/gaussian_sfcanl.x}
SIGLEVEL=${SIGLEVEL:-${FIXgfs}/am/global_hyblev.l${LEVSP1}.txt}

#  Other variables.

# Set defaults
################################################################################
#  Preprocessing

################################################################################
#  Make surface analysis

# input interpolation weights
cpreq "${FIXWGTS}" "./weights.nc"

# input analysis tiles (with nst records)
cpreq "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile1.nc" "./anal.tile1.nc"
cpreq "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile2.nc" "./anal.tile2.nc"
cpreq "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile3.nc" "./anal.tile3.nc"
cpreq "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile4.nc" "./anal.tile4.nc"
cpreq "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile5.nc" "./anal.tile5.nc"
cpreq "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile6.nc" "./anal.tile6.nc"

# input orography tiles
cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile1.nc" "./orog.tile1.nc"
cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile2.nc" "./orog.tile2.nc"
cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile3.nc" "./orog.tile3.nc"
cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile4.nc" "./orog.tile4.nc"
cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile5.nc" "./orog.tile5.nc"
cpreq "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile6.nc" "./orog.tile6.nc"

cpreq "${SIGLEVEL}" "./vcoord.txt"

# Namelist uses booleans now
if [[ "${DONST}" == "YES" ]]; then
    do_nst=".true."
else
    do_nst=".false."
fi

# Add soil increments to gdas gaussian sfcanal if they are not added by gcycle (i.e., when landiau=true)
LSOIL_INCR=${LSOIL_INCR:-2}
if [[ "${DO_LAND_IAU:-.false.}" == ".true." ]]; then
    for i in $(seq 1 6); do
        sfc_inc="${COMIN_ATMOS_ANALYSIS}/increment.sfc.i006.tile${i}.nc"
        cpreq "${sfc_inc}" "./sfc_inc.tile${i}.nc"
    done
fi

# Executable namelist
cat << EOF > fort.41
&setup
  yy=${PDY:0:4},
  mm=${PDY:4:2},
  dd=${PDY:6:2},
  hh=${cyc},
  igaus=${LONB_SFC},
  jgaus=${LATB_SFC},
  donst=${do_nst},
  imp_physics=${imp_physics:-8},
  landsfcmdl=${landsfcmdl:-2},
  add_soil_inc=${DO_LAND_IAU},
  lsoil_incr=${LSOIL_INCR},
  sfc_inc_file="./sfc_inc",
/
EOF
cat fort.41

export pgm="${GAUSFCANLEXE}"
export OMP_NUM_THREADS=${OMP_NUM_THREADS_SFC:-1}
${APRUNSFC} "${GAUSFCANLEXE}"
export err=$?
if [[ ${err} -ne 0 ]]; then
    echo "FATAL ERROR: ${GAUSFCANLEXE} returned non-zero exit status!"
    exit "${err}"
fi

# output gaussian global surface analysis files
if [[ -f "sfc.gaussian.analysis.file" ]]; then
    cpfs "./sfc.gaussian.analysis.file" "${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.sfc.a006.nc"
fi

################################################################################
#  Postprocessing

exit 0
