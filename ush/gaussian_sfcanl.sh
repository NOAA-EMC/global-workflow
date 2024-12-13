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
#           fv3gfs surface analysis tiles
#
# Script history log:
# 2018-01-30  Gayno  initial script
# 2019-1030   Gayno  updates to output analysis file in netcdf or nemsio
#
# Usage:  gaussian_sfcanl.sh
#
#   Imported Shell Variables:
#     CASE          Model resolution.  Defaults to C768.
#     DONST         Process NST fields when 'yes'.  Default is 'no'.
#     OUTPUT_FILE   Output gaussian analysis file format.  Default is "nemsio"
#                   Set to "netcdf" for netcdf output file
#                   Otherwise, output in nemsio.
#     FIXWGTS       Weight file to use for interpolation
#     DATA          Working directory
#                   (if nonexistent will be made, used and deleted)
#                   Defaults to current working directory
#     COMOUT        Output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     XC            Suffix to add to executables. Defaults to none.
#     GAUSFCANLEXE  Program executable.
#                   Defaults to $EXECgfs/gaussian_sfcanl.x
#     INISCRIPT     Preprocessing script.  Defaults to none.
#     LOGSCRIPT     Log posting script.  Defaults to none.
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     CDATE         Output analysis date in yyyymmddhh format. Required.
#     PGMOUT        Executable standard output
#                   defaults to $pgmout, then to '&1'
#     PGMERR        Executable standard error
#                   defaults to $pgmerr, then to '&1'
#     pgmout        Executable standard output default
#     pgmerr        Executable standard error default
#     REDOUT        standard output redirect ('1>' or '1>>')
#                   defaults to '1>', or to '1>>' to append if $PGMOUT is a file
#     REDERR        standard error redirect ('2>' or '2>>')
#                   defaults to '2>', or to '2>>' to append if $PGMERR is a file
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#     gfs_ver       Version number of gfs directory.  Default is
#                   v15.0.0.
#     OMP_NUM_
#     THREADS_SFC   Number of omp threads to use.  Default is 1.
#     APRUNSFC      Machine specific command to invoke the executable.
#                   Default is none.
#
#   Exported Shell Variables:
#     PGM           Current program name
#     pgm
#     ERR           Last return code
#     err
#
#   Modules and files referenced:
#     scripts    : $INISCRIPT
#                  $LOGSCRIPT
#                  $ERRSCRIPT
#                  $ENDSCRIPT
#
#     programs   : $GAUSFCANLEXE
#
#     fixed data : ${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile*.nc
#                  ${FIXWGTS}
#                  ${FIXgfs}/am/global_hyblev.l65.txt
#
#     input data : ${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile*.nc
#
#     output data: $PGMOUT
#                  $PGMERR
#                  $COMOUT/${APREFIX}sfcanl.nc
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
#   Machine: IBM SP
#
################################################################################

source "${USHgfs}/preamble.sh"

CASE=${CASE:-C768}
res=$(echo $CASE | cut -c2-)
LONB_CASE=$((res*4))
LATB_CASE=$((res*2))
LONB_SFC=${LONB_SFC:-$LONB_CASE}
LATB_SFC=${LATB_SFC:-$LATB_CASE}
DONST=${DONST:-"NO"}
LEVS=${LEVS:-64}
LEVSP1=$(($LEVS+1))
FIXWGTS=${FIXWGTS:-${FIXorog}/${CASE}/fv3_SCRIP_${CASE}_GRIDSPEC_lon${LONB_SFC}_lat${LATB_SFC}.gaussian.neareststod.nc}
DATA=${DATA:-$(pwd)}

#  Filenames.
XC=${XC:-}
GAUSFCANLEXE=${GAUSFCANLEXE:-$EXECgfs/gaussian_sfcanl.x}
SIGLEVEL=${SIGLEVEL:-${FIXgfs}/am/global_hyblev.l${LEVSP1}.txt}

CDATE=${CDATE:?}

#  Other variables.
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}

# Set defaults
################################################################################
#  Preprocessing
${INISCRIPT:-}
pwd=$(pwd)
cd "${DATA}" || exit 99
[[ -d "${COMOUT_ATMOS_ANALYSIS}" ]] || mkdir -p "${COMOUT_ATMOS_ANALYSIS}"

################################################################################
#  Make surface analysis
export PGM=$GAUSFCANLEXE
export pgm=$PGM
$LOGSCRIPT

iy=${PDY:0:4}
im=${PDY:4:2}
id=${PDY:6:2}
ih=${cyc}

export OMP_NUM_THREADS=${OMP_NUM_THREADS_SFC:-1}

# input interpolation weights
${NLN} "${FIXWGTS}" "./weights.nc"

# input analysis tiles (with nst records)
${NLN} "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile1.nc" "./anal.tile1.nc"
${NLN} "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile2.nc" "./anal.tile2.nc"
${NLN} "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile3.nc" "./anal.tile3.nc"
${NLN} "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile4.nc" "./anal.tile4.nc"
${NLN} "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile5.nc" "./anal.tile5.nc"
${NLN} "${COMIN_ATMOS_RESTART}/${PDY}.${cyc}0000.sfcanl_data.tile6.nc" "./anal.tile6.nc"

# input orography tiles
${NLN} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile1.nc" "./orog.tile1.nc"
${NLN} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile2.nc" "./orog.tile2.nc"
${NLN} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile3.nc" "./orog.tile3.nc"
${NLN} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile4.nc" "./orog.tile4.nc"
${NLN} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile5.nc" "./orog.tile5.nc"
${NLN} "${FIXorog}/${CASE}/${CASE}.mx${OCNRES}_oro_data.tile6.nc" "./orog.tile6.nc"

${NLN} "${SIGLEVEL}" "./vcoord.txt"

# output gaussian global surface analysis files
${NLN} "${COMOUT_ATMOS_ANALYSIS}/${APREFIX}sfcanl.nc" "./sfc.gaussian.analysis.file"

# Namelist uses booleans now
if [[ ${DONST} == "YES" ]]; then do_nst='.true.'; else do_nst='.false.'; fi

# Executable namelist
cat <<EOF > fort.41
 &setup
  yy=${iy},
  mm=${im},
  dd=${id},
  hh=${ih},
  igaus=${LONB_SFC},
  jgaus=${LATB_SFC},
  donst=${do_nst},
 /
EOF

$APRUNSFC $GAUSFCANLEXE

export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

################################################################################
#  Postprocessing
cd $pwd

exit ${err}
