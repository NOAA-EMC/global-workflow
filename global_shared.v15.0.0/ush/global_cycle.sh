#!/bin/ksh
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
#
# Usage:  global_cycle.sh SFCGES SFCANL
#
#   Input script positional parameters:
#     1             Input surface guess
#                   defaults to $SFCGES; required
#     2             Output surface analysis
#                   defaults to $SFCANL, then to ${COMOUT}/sfcanl
#
#   Imported Shell Variables:
#     SFCGES        Input surface guess file
#                   overridden by $1; required
#     SFCANL        Output surface analysis file
#                   overridden by $2; defaults to ${COMOUT}/sfcanl
#     CASE          Model resolution.  Defaults to C768.
#     TILE_NUM      The number of the cubed-sphere tile to convert surface
#     FIXgsm        Directory for the global fixed climatology files.
#                   Defaults to $HOMEglobal/fix
#     FIXfv3        Directory for the model grid and orography netcdf
#                   files.  Defaults to $HOMEglobal/fix/fix_fv3/${CASE}
#     EXECgsm       Directory of the program executable.  Defaults to
#                   $HOMEglobal/exec
#     DATA          Working directory
#                   (if nonexistent will be made, used and deleted)
#                   Defaults to current working directory
#     COMIN         Directory containing the input analysis data
#                   (such as sea ice).  Defaults to current
#                   working directory.
#     COMOUT        Output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     XC            Suffix to add to executables. Defaults to none.
#     PREINP        Prefix to add to input analysis files.
#                   Defaults to none.
#     SUFINP        Suffix to add to input analysis files.
#                   Defaults to none.
#     CYCLEXEC      Program executable.
#                   Defaults to ${EXECgsm}/global_cycle$XC
#     FNGLAC        Input glacier climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_glacier.2x2.grb
#     FNMXIC        Input maximum sea ice climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_maxice.2x2.grb
#     FNTSFC        Input SST climatology GRIB file.
#                   Defaults to ${FIXgsm}/RTGSST.1982.2012.monthly.clim.grb
#     FNSNOC        Input snow climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_snoclim.1.875.grb
#     FNZORC        Input roughness climatology.
#                   Defaults to igbp vegetation type-based lookup table
#                   FNVETC must be set to igbp file:
#                   ${FIXgsm}/global_vegtype.igbp.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb
#     FNALBC        Input 4-component albedo climatology GRIB file.
#                   defaults to ${FIXgsm}/global_snowfree_albedo.bosu.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb
#     FNALBC2       Input 'facsf' and 'facwf' albedo climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_albedo4.1x1.grb
#     FNAISC        Input sea ice climatology GRIB file.
#                   Defaults to ${FIXgsm}/CFSR.SEAICE.1982.2012.monthly.clim.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_tg3clim.2.6x1.5.grb
#     FNVEGC        Input vegetation fraction climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_vegfrac.0.144.decpercent.grb
#     FNVETC        Input vegetation type climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_vegtype.igbp.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb
#     FNSOTC        Input soil type climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_soiltype.statsgo.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb
#     FNSMCC        Input soil moisture climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_soilmgldas.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.grb
#     FNVMNC        Input min veg frac climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_shdmin.0.144x0.144.grb
#     FNVMXC        Input max veg frac climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_shdmax.0.144x0.144.grb
#     FNSLPC        Input slope type climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_slope.1x1.grb
#     FNABSC        Input max snow albedo climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_mxsnoalb.uariz.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb
#     FNMSKH        Input high resolution land mask GRIB file.  Use to set mask for
#                   some of the input climatology fields.  This is NOT the model mask.
#                   Defaults to ${FIXgsm}/seaice_newland.grb
#     FNOROG        Model orography file (netcdf format)
#                   Defaults to {FIXfv3}/${CASE}_oro_data.tile${TILE_NUM}.nc
#     FNGRID        Model grid file (netcdf format)
#                   Defaults to ${FIXfv3}/${CASE}_grid.tile${TILE_NUM}.nc
#     GSI_FILE      GSI file on the gaussian grid containing NST increments.
#                   Defaults to empty string.
#     FNTSFA        Input SST analysis GRIB file.
#                   Defaults to ${COMIN}/${PREINP}sstgrb${SUFINP}
#     FNACNA        Input sea ice analysis GRIB file.
#                   Defaults to ${COMIN}/${PREINP}engicegrb${SUFINP}
#     FNSNOA        Input snow analysis GRIB file.
#                   Defaults to ${COMIN}/${PREINP}snogrb${SUFINP}
#     INISCRIPT     Preprocessing script.  Defaults to none.
#     LOGSCRIPT     Log posting script.  Defaults to none.
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     CDATE         Output analysis date in yyyymmddhh format. Required.
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
#     use_ufo       Adjust sst and soil substrate temperature for differences
#                   between the filtered and unfiltered terrain.  Default is true.
#     NST_ANL       Process NST records and perform SST terrain adjustments required
#                   when using NST model.  Default is false.
#     zsea1/zsea2   When running with NST model, this is the lower/upper bound
#                   of depth of sea temperature.
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
#     programs   : $CYCLEXEC
#
#     fixed data : $FNGLAC
#                  $FNMXIC
#                  $FNTSFC
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
#                  $FNOROG
#
#     input data : $SFCGES
#                  $FNTSFA
#                  $FNACNA
#                  $FNSNOA
#
#     output data: $SFCANL
#                  $PGMOUT
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

#  Set environment.
VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

#  Command line arguments.
SFCGES=${1:-${SFCGES:?}}
SFCANL=${2:-${SFCANL:?}}
TILE_NUM=${3:-${TILE_NUM:-?}}

CASE=${CASE:-C768}

#  Directories.
global_shared_ver=${global_shared_ver:-v15.0.0}
BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
HOMEglobal=${HOMEglobal:-$BASEDIR/global_shared.${global_shared_ver}}
EXECgsm=${EXECgsm:-$HOMEglobal/exec}
FIXSUBDA=${FIXSUBDA:-fix/fix_am}
FIXgsm=${FIXgsm:-$HOMEglobal/$FIXSUBDA}
FIXfv3=${FIXfv3:-$HOMEglobal/fix/fix_fv3/$CASE}
DATA=${DATA:-$(pwd)}
COMIN=${COMIN:-$(pwd)}
COMOUT=${COMOUT:-$(pwd)}

#  Filenames.
XC=${XC}
PREINP=${PREINP}
SUFINP=${SUFINP}
CYCLEXEC=${CYCLEXEC:-$EXECgsm/global_cycle$XC}

CDATE=${CDATE:?}
FHOUR=${FHOUR:-00}

CRES=$(echo $CASE | cut -c2-)
JCAP_CASE=$((2*CRES-2))
LONB_CASE=$((4*CRES))
LATB_CASE=$((2*CRES))
DELTSFC=${DELTSFC:-0}

LSOIL=${LSOIL:-4}
FSMCL2=${FSMCL2:-60}
FSLPL=${FSLPL:-99999.}
FSOTL=${FSOTL:-99999.}
FVETL=${FVETL:-99999.}
IALB=${IALB:-1}
ISOT=${ISOT:-1}
IVEGSRC=${IVEGSRC:-1}
CYCLVARS=${CYCLVARS:-""}
use_ufo=${use_ufo:-.true.}
NST_ANL=${NST_ANL:-.false.}
zsea1=${zsea1:-0}
zsea2=${zsea2:-0}

FNGLAC=${FNGLAC:-${FIXgsm}/global_glacier.2x2.grb}
FNMXIC=${FNMXIC:-${FIXgsm}/global_maxice.2x2.grb}
FNTSFC=${FNTSFC:-${FIXgsm}/RTGSST.1982.2012.monthly.clim.grb}
FNSNOC=${FNSNOC:-${FIXgsm}/global_snoclim.1.875.grb}
FNZORC=${FNZORC:-igbp}
FNALBC2=${FNALBC2:-${FIXgsm}/global_albedo4.1x1.grb}
FNAISC=${FNAISC:-${FIXgsm}/CFSR.SEAICE.1982.2012.monthly.clim.grb}
FNTG3C=${FNTG3C:-${FIXgsm}/global_tg3clim.2.6x1.5.grb}
FNVEGC=${FNVEGC:-${FIXgsm}/global_vegfrac.0.144.decpercent.grb}
FNALBC=${FNALBC:-${FIXgsm}/global_snowfree_albedo.bosu.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb}
FNVETC=${FNVETC:-${FIXgsm}/global_vegtype.igbp.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb}
FNSOTC=${FNSOTC:-${FIXgsm}/global_soiltype.statsgo.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb}
FNSMCC=${FNSMCC:-${FIXgsm}/global_soilmgldas.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.grb}
FNABSC=${FNABSC:-${FIXgsm}/global_mxsnoalb.uariz.t$JCAP_CASE.$LONB_CASE.$LATB_CASE.rg.grb}
FNVMNC=${FNVMNC:-${FIXgsm}/global_shdmin.0.144x0.144.grb}
FNVMXC=${FNVMXC:-${FIXgsm}/global_shdmax.0.144x0.144.grb}
FNSLPC=${FNSLPC:-${FIXgsm}/global_slope.1x1.grb}
FNMSKH=${FNMSKH:-${FIXgsm}/seaice_newland.grb}
FNOROG=${FNOROG:-${FIXfv3}/${CASE}/${CASE}_oro_data.tile${TILE_NUM}.nc}
FNGRID=${FNGRID:-${FIXfv3}/${CASE}/${CASE}_grid.tile${TILE_NUM}.nc}
GSI_FILE=${GSI_FILE:-" "}
FNTSFA=${FNTSFA:-${COMIN}/${PREINP}sstgrb${SUFINP}}
FNACNA=${FNACNA:-${COMIN}/${PREINP}engicegrb${SUFINP}}
FNSNOA=${FNSNOA:-${COMIN}/${PREINP}snogrb${SUFINP}}
SFCANL=${SFCANL:-${COMIN}/${PREINP}sfcanl}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}
# Set defaults
################################################################################
#  Preprocessing
$INISCRIPT
pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
[[ -d $COMOUT ]]||mkdir -p $COMOUT

# If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
[[ ! -f $FNALBC ]] && FNALBC="$FIXgsm/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb"
[[ ! -f $FNVETC ]] && FNVETC="$FIXgsm/global_vegtype.igbp.t1534.3072.1536.rg.grb"
[[ ! -f $FNSOTC ]] && FNSOTC="$FIXgsm/global_soiltype.statsgo.t1534.3072.1536.rg.grb"
[[ ! -f $FNABSC ]] && FNABSC="$FIXgsm/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb"
[[ ! -f $FNSMCC ]] && FNSMCC="$FIXgsm/global_soilmgldas.t1534.3072.1536.grb"

################################################################################
#  Make surface analysis
export PGM=$CYCLEXEC
export pgm=$PGM
$LOGSCRIPT

rm -f $SFCANL
iy=$(echo $CDATE|cut -c1-4)
im=$(echo $CDATE|cut -c5-6)
id=$(echo $CDATE|cut -c7-8)
ih=$(echo $CDATE|cut -c9-10)

export OMP_NUM_THREADS=${OMP_NUM_THREADS_CY:-${CYCLETHREAD:-1}}

cat << EOF > fort.35
&NAMSFC
  FNGLAC="$FNGLAC",
  FNMXIC="$FNMXIC",
  FNTSFC="$FNTSFC",
  FNSNOC="$FNSNOC",
  FNZORC="$FNZORC",
  FNALBC="$FNALBC",
  FNALBC2="$FNALBC2",
  FNAISC="$FNAISC",
  FNTG3C="$FNTG3C",
  FNVEGC="$FNVEGC",
  FNVETC="$FNVETC",
  FNSOTC="$FNSOTC",
  FNSMCC="$FNSMCC",
  FNVMNC="$FNVMNC",
  FNVMXC="$FNVMXC",
  FNSLPC="$FNSLPC",
  FNABSC="$FNABSC",
  FNMSKH="$FNMSKH",
  FNTSFA="$FNTSFA",
  FNACNA="$FNACNA",
  FNSNOA="$FNSNOA",
  LDEBUG=.false.,
  FSLPL=$FSLPL,
  FSOTL=$FSOTL,
  FVETL=$FVETL,
  FSMCL(2)=$FSMCL2,
  FSMCL(3)=$FSMCL2,
  FSMCL(4)=$FSMCL2,
  $CYCLVARS
 /
EOF

eval $APRUNCY $CYCLEXEC <<EOF $REDOUT$PGMOUT $REDERR$PGMERR
 &NAMCYC
  idim=$CRES, jdim=$CRES, lsoil=$LSOIL,
  iy=$iy, im=$im, id=$id, ih=$ih, fh=$FHOUR,
  DELTSFC=$DELTSFC,ialb=$IALB,use_ufo=$use_ufo,NST_ANL=$NST_ANL,
  isot=$ISOT,ivegsrc=$IVEGSRC,zsea1=$zsea1,zsea2=$zsea2
 /
 &NAMSFCD
  FNBGSI="$SFCGES",
  FNBGSO="$SFCANL",
  FNOROG="$FNOROG",
  FNGRID="$FNGRID",
  GSI_FILE="$GSI_FILE",
 /
EOF

export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
$ENDSCRIPT
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
