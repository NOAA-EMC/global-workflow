#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_cycle.sh
# Script description:  Makes a global spectral model surface analysis
#
# Author:        Mark Iredell       Org: NP23         Date: 2005-02-03
#
# Abstract: This script makes a global spectral model surface analysis.
#
# Script history log:
# 2005-02-03  Iredell  extracted from global_analysis.sh
# 2014-11-30  xuli  add NST_ANL
# 2017-08-19  Gayno  updates for FV3GFS.
# 2025-07-08  Friedman  pull script into global-workflow
#
# Usage:  global_cycle.sh
#
#   Imported Shell Variables:
#     CASE          Model resolution.  Defaults to C768.
#     JCAP_CASE     Spectral truncation of the global fixed climatology files
#                   (such as albedo), which are on the old GFS gaussian grid.
#                   Computed from CASE by default.
#     LATB_CASE     i-dimension of the global climatology files.  NOT the
#                   i-dimension of the model grid. Computed from CASE by default.
#     LONB_CASE     j-dimension of the global climatology files. NOT the
#                   j-dimension of the model grid. Computed from CASE by default.
#     OCNRES        Ocean grid resolution. '100' is one degree.
#     HOMEgfs       Directory for gfs.  Default is
#                   PACKAGEROOT/gfs.v15.0.0.
#     PACKAGEROOT   Location of gfs package.
#     FIXgfs        Directory for fixed data. Default is $HOMEgfs/fix.
#     FIXorog       Directory for fixed orography data. Default is $FIXgfs/orog
#     EXECgfs       Directory of the program executable.  Defaults to
#                   $HOMEgfs/exec
#     DATA          Working directory
#                   (if nonexistent will be made, used and deleted)
#                   Defaults to current working directory
#     COMIN_OBS     Directory containing the input analysis data
#                   (such as sea ice).  Defaults to current
#                   working directory.
#     XC            Suffix to add to executables. Defaults to none.
#     PREINP        Prefix to add to input analysis files.
#                   Defaults to none.
#     SUFINP        Suffix to add to input analysis files.
#                   Defaults to none.
#     CYCLEXEC      Program executable.
#                   Defaults to ${EXECgfs}/global_cycle$XC
#     FNGLAC        Input glacier climatology GRIB file.
#                   Defaults to ${FIXgfs}/am/global_glacier.2x2.grb
#     FNMXIC        Input maximum sea ice climatology GRIB file.
#                   Defaults to ${FIXgfs}/am/global_maxice.2x2.grb
#     FNTSFC        Input SST climatology GRIB file.
#                   Defaults to ${FIXgfs}/am/RTGSST.1982.2012.monthly.clim.grb
#     FNSALC        Input Salinity climatology netcdf file.
#                   Defaults to ${FIXgfs}/am/global_salclm.t1534.3072.1536.nc
#     FNSNOC        Input snow climatology GRIB file.
#                   Defaults to ${FIXgfs}/am/global_snoclim.1.875.grb
#     FNZORC        Input roughness climatology.
#                   Defaults to igbp vegetation type-based lookup table
#                   FNVETC must be set to igbp file:
#                   ${FIXgfs}/am/global_vegtype.igbp.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb
#     FNALBC        Input 4-component albedo climatology GRIB file.
#                   defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tileX.nc
#     FNALBC2       Input 'facsf' and 'facwf' albedo climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.facsf.tileX.nc
#     FNAISC        Input sea ice climatology GRIB file.
#                   Defaults to ${FIXgfs}/am/IMS-NIC.blended.ice.monthly.clim.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tileX.nc
#     FNVEGC        Input vegetation fraction climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc
#     FNVETC        Input vegetation type climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tileX.nc
#     FNSOTC        Input soil type climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_type.tileX.nc
#     FNSMCC        Input soil moisture climatology GRIB file.
#                   Defaults to ${FIXgfs}/am/global_soilmgldas.statsgo.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.grb
#     FNVMNC        Input min veg frac climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc
#     FNVMXC        Input max veg frac climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc
#     FNSLPC        Input slope type climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.slope_type.tileX.nc
#     FNABSC        Input max snow albedo climatology GRIB file.
#                   Defaults to ${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tileX.nc
#     FNMSKH        Input high resolution land mask GRIB file.  Use to set mask for
#                   some of the input climatology fields.  This is NOT the model mask.
#                   Defaults to ${FIXgfs}/am/global_slmask.t1534.3072.1536.grb
#     NST_FILE      GSI file on the gaussian grid containing NST increments.
#                   Defaults to NULL (no file).
#     FNTSFA        Input SST analysis GRIB file.
#     FNACNA        Input sea ice analysis GRIB file.
#     FNSNOA        Input snow analysis GRIB file.
#     PDYcyc        Output analysis date in yyyymmddhh format. Required.
#     FHOUR         Output forecast hour.  Defaults to 00hr.
#     LSOIL         Number of soil layers. Defaults to 4.
#     FSMCL2        Scale in days to relax to soil moisture climatology.
#                   Defaults to 60.
#     FSLPL         Scale in days to relax to slope type climatology.
#                   Defaults to 99999 (use first guess)
#     FSOTL         Scale in days to relax to soil type climatology.
#                   Defaults to 99999 (use first guess)
#     FVETL         Scale in days to relax to veg type climatology.
#                   Defaults to 99999 (use first guess)
#     DELTSFC       Cycling frequency in hours. Defaults to 0.
#     IALB          Integer flag for Albedo - 0 for Brigleb and 1 for Modis
#                   based albedo - defaults to 1
#     ISOT          Integer flag for soil type - 0 for zobler, 1 for statsgo
#                   Defaults to 1.
#     IVEGSRC       Integer flag for veg type - 1 for igbp, 2 for sib
#                   Defaults to 1.
#     CYCLVARS      Other namelist inputs to the cycle executable
#                   defaults to none set
#     PGMOUT        Executable standard output
#                   defaults to $pgmout, then to 'out'
#     PGMERR        Executable standard error
#                   defaults to $pgmerr, then to 'err'
#     pgmout        Executable standard output default
#     pgmerr        Executable standard error default
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#     use_ufo       Adjust sst and soil substrate temperature for differences
#                   between the filtered and unfiltered terrain.  Default is true.
#     DONST         Process NST records when using NST model.  Default is 'no'.
#     DO_SFCCYCLE   Call sfcsub routine
#     GCYCLE_DO_SOILINCR   Call routine to add soil increments
#     GCYCLE_DO_SNOWINCR   Call routine to add snow inrcements
#     GCYCLE_INTERP_LANDINCR  Flag to regrid input land increment from Gaus to native model
#                   grid inside gcycle
#
#     zsea1/zsea2   When running with NST model, this is the lower/upper bound
#                   of depth of sea temperature.  In whole mm.
#     MAX_TASKS_CY  Normally, program should be run with a number of mpi tasks
#                   equal to the number of cubed-sphere tiles being processed.
#                   However, the current parallel scripts may over-specify the
#                   number of tasks.  Set this variable to not process
#                   any ranks greater than max_tasks-1.  Default is '99999',
#                   which means to process using all tasks.
#     OMP_NUM_
#     THREADS_CY    Number of omp threads to use.  Default is 1.
#     APRUNC        Machine specific command to invoke the executable.
#                   Default is none.
#
#   Exported Shell Variables:
#     PGM           Current program name
#     pgm
#     ERR           Last return code
#     err
#
#   Modules and files referenced:
#     programs   : $CYCLEXEC
#
#     fixed data : $FNGLAC
#                  $FNMXIC
#                  $FNTSFC
#                  $FNSALC
#                  $FNSNOC
#                  $FNZORC
#                  $FNALBC
#                  $FNALBC2
#                  $FNAISC
#                  $FNTG3C
#                  $FNVEGC
#                  $FNVETC
#                  $FNSOTC
#                  $FNSMCC
#                  $FNVMNC
#                  $FNVMXC
#                  $FNSLPC
#                  $FNABSC
#                  $FNMSKH
#
#     input data : $FNTSFA
#                  $FNACNA
#                  $FNSNOA
#
#     output data: $PGMOUT
#                  $PGMERR
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

#  Filenames.
XC=${XC:-" "}
PREINP=${PREINP:-" "}
SUFINP=${SUFINP:-" "}
CYCLEXEC=${CYCLEXEC:-${EXECgfs}/global_cycle${XC}}

FHOUR=${FHOUR:-00}

CRES=${CASE:1}
JCAP_CASE=$((2*CRES-2))
LONB_CASE=$((4*CRES))
LATB_CASE=$((2*CRES))
DELTSFC=${DELTSFC:-0}

LSOIL=${LSOIL:-4}
LSOIL_INCR=${LSOIL_INCR:-2}
FSMCL2=${FSMCL2:-60}
FSLPL=${FSLPL:-99999.}
FSOTL=${FSOTL:-99999.}
FVETL=${FVETL:-99999.}
IALB=${IALB:-1}
ISOT=${ISOT:-1}
IVEGSRC=${IVEGSRC:-1}
CYCLVARS=${CYCLVARS:-""}
use_ufo=${use_ufo:-.true.}
DONST=${DONST:-"NO"}
DO_SFCCYCLE=${DO_SFCCYCLE:-.true.}
GCYCLE_DO_SOILINCR=${GCYCLE_DO_SOILINCR:-.false.}
GCYCLE_DO_SNOWINCR=${GCYCLE_DO_SNOWINCR:-.false.}
if [[ "${GCYCLE_DO_SOILINCR}" == ".true." ]] || [[ "${GCYCLE_DO_SNOWINCR}" == ".true." ]] ; then
        DO_LANDINCR=".true."
else
        DO_LANDINCR=".false."
fi
GCYCLE_INTERP_LANDINCR=${GCYCLE_INTERP_LANDINCR:-.false.}
zsea1=${zsea1:-0}
zsea2=${zsea2:-0}
MAX_TASKS_CY=${MAX_TASKS_CY:-99999}
FRAC_GRID=${FRAC_GRID:-.false.}

FNGLAC=${FNGLAC:-${FIXgfs}/am/global_glacier.2x2.grb}
FNMXIC=${FNMXIC:-${FIXgfs}/am/global_maxice.2x2.grb}
FNTSFC=${FNTSFC:-${FIXgfs}/am/RTGSST.1982.2012.monthly.clim.grb}
FNSALC=${FNSALC:-${FIXgfs}/am/global_salclm.t1534.3072.1536.nc}
FNSNOC=${FNSNOC:-${FIXgfs}/am/global_snoclim.1.875.grb}
FNZORC=${FNZORC:-igbp}
FNAISC=${FNAISC:-${FIXgfs}/am/IMS-NIC.blended.ice.monthly.clim.grb}
FNSMCC=${FNSMCC:-${FIXgfs}/am/global_soilmgldas.statsgo.t${JCAP_CASE}.${LONB_CASE}.${LATB_CASE}.grb}
FNALBC2=${FNALBC2:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.facsf.tileX.nc}
FNTG3C=${FNTG3C:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.substrate_temperature.tileX.nc}
FNVEGC=${FNVEGC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc}
FNALBC=${FNALBC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.snowfree_albedo.tileX.nc}
FNVETC=${FNVETC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_type.tileX.nc}
FNSOTC=${FNSOTC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.soil_type.tileX.nc}
FNABSC=${FNABSC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.maximum_snow_albedo.tileX.nc}
FNVMNC=${FNVMNC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc}
FNVMXC=${FNVMXC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.vegetation_greenness.tileX.nc}
FNSLPC=${FNSLPC:-${FIXorog}/${CASE}/sfc/${CASE}.mx${OCNRES}.slope_type.tileX.nc}
FNMSKH=${FNMSKH:-${FIXgfs}/am/global_slmask.t1534.3072.1536.grb}
NST_FILE=${NST_FILE:-"NULL"}
FNTSFA=${FNTSFA:-${COMIN_OBS}/${PREINP}sstgrb${SUFINP}}
FNACNA=${FNACNA:-${COMIN_OBS}/${PREINP}engicegrb${SUFINP}}
FNSNOA=${FNSNOA:-${COMIN_OBS}/${PREINP}snogrb${SUFINP}}
#  Other variables.
PGMOUT=${PGMOUT:-${pgmout:-'out'}}
PGMERR=${PGMERR:-${pgmerr:-'err'}}
# Set defaults
################################################################################
#  Preprocessing

ln -fs "${FNTSFC}" sstclm
ln -fs "${FNSALC}" salclm

# If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
[[ ! -f ${FNSMCC} ]] && FNSMCC="${FIXgfs}/am/global_soilmgldas.statsgo.t1534.3072.1536.grb"

################################################################################
#  Make surface analysis
export PGM=${CYCLEXEC}
export pgm=${PGM}

iy=${PDY:0:4}
im=${PDY:4:2}
id=${PDY:6:2}
ih=${cyc}

export OMP_NUM_THREADS=${OMP_NUM_THREADS_CY:-${CYCLETHREAD:-1}}

cat << EOF > fort.35
&NAMSFC
  FNGLAC="${FNGLAC}",
  FNMXIC="${FNMXIC}",
  FNTSFC="${FNTSFC}",
  FNSNOC="${FNSNOC}",
  FNZORC="${FNZORC}",
  FNALBC="${FNALBC}",
  FNALBC2="${FNALBC2}",
  FNAISC="${FNAISC}",
  FNTG3C="${FNTG3C}",
  FNVEGC="${FNVEGC}",
  FNVETC="${FNVETC}",
  FNSOTC="${FNSOTC}",
  FNSMCC="${FNSMCC}",
  FNVMNC="${FNVMNC}",
  FNVMXC="${FNVMXC}",
  FNSLPC="${FNSLPC}",
  FNABSC="${FNABSC}",
  FNMSKH="${FNMSKH}",
  FNTSFA="${FNTSFA}",
  FNACNA="${FNACNA}",
  FNSNOA="${FNSNOA}",
  LDEBUG=.false.,
  FSLPL=${FSLPL},
  FSOTL=${FSOTL},
  FVETL=${FVETL},
  FSMCL(2)=${FSMCL2},
  FSMCL(3)=${FSMCL2},
  FSMCL(4)=${FSMCL2},
  ${CYCLVARS}
 /
EOF

cat << EOF > fort.36
 &NAMCYC
  idim=${CRES}, jdim=${CRES}, lsoil=${LSOIL},
  iy=${iy}, im=${im}, id=${id}, ih=${ih}, fh=${FHOUR},
  deltsfc=${DELTSFC},ialb=${IALB},use_ufo=${use_ufo},donst="${DONST}",
  do_sfccycle=${DO_SFCCYCLE},do_landincr=${DO_LANDINCR},isot=${ISOT},ivegsrc=${IVEGSRC},
  zsea1_mm=${zsea1},zsea2_mm=${zsea2},MAX_TASKS=${MAX_TASKS_CY},
  frac_grid=${FRAC_GRID}
 /
EOF


cat << EOF > fort.37
 &NAMSFCD
  NST_FILE="${NST_FILE}",
  DO_SOILINCR=${GCYCLE_DO_SOILINCR},
  DO_SNOWINCR=${GCYCLE_DO_SNOWINCR},
  INTERP_LANDINCR=${GCYCLE_INTERP_LANDINCR},
  lsoil_incr=${LSOIL_INCR},
 /
EOF

${APRUNCY} "${CYCLEXEC}" 1>"${PGMOUT}" 2>"${PGMERR}"

export err=$?

exit "${err}"
