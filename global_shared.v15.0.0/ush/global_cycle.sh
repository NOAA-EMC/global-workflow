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
#     CRES          Model resolution.  Defaults to 768.
#     JCAP          Spectral truncation of the global fixed climatology files
#                   (such as albedo), which are on the old GFS gaussian grid.
#                   Defaults to 1534
#     LATB          i-dimension of the global climatology files.  NOT the
#                   i-dimension of the model grid. Defaults to 1536.
#     LONB          j-dimension of the global climatology files. NOT the
#                   j-dimension of the model grid. Defaults to 3072.
#     FIXgsm        Directory for the global fixed climatology files.
#                   Defaults to $HOMEglobal/fix
#     FIXfv3        Directory for the model grid and orography netcdf
#                   files.  Defaults to $HOMEglobal/fix/fix_fv3/C${CRES}
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
#                   ${FIXgsm}/global_vegtype.igbp.t$JCAP.$LONB.$LATB.rg.grb
#     FNALBC        Input 4-component albedo climatology GRIB file.
#                   defaults to ${FIXgsm}/global_snowfree_albedo.bosu.t$JCAP.$LONB.$LATB.rg.grb
#     FNALBC2       Input 'facsf' and 'facwf' albedo climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_albedo4.1x1.grb
#     FNAISC        Input sea ice climatology GRIB file.
#                   Defaults to ${FIXgsm}/CFSR.SEAICE.1982.2012.monthly.clim.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_tg3clim.2.6x1.5.grb
#     FNVEGC        Input vegetation fraction climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_vegfrac.0.144.decpercent.grb
#     FNVETC        Input vegetation type climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_vegtype.igbp.t$JCAP.$LONB.$LATB.rg.grb
#     FNSOTC        Input soil type climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_soiltype.statsgo.t$JCAP.$LONB.$LATB.rg.grb
#     FNSMCC        Input soil moisture climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_soilmgldas.t${JCAP}.${LONB}.${LATB}.grb
#     FNVMNC        Input min veg frac climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_shdmin.0.144x0.144.grb
#     FNVMXC        Input max veg frac climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_shdmax.0.144x0.144.grb
#     FNSLPC        Input slope type climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_slope.1x1.grb
#     FNABSC        Input max snow albedo climatology GRIB file.
#                   Defaults to ${FIXgsm}/global_mxsnoalb.uariz.t$JCAP.$LONB.$LATB.rg.grb
#     FNMSKH        Input high resolution land mask GRIB file.  Use to set mask for
#                   some of the input climatology fields.  This is NOT the model mask.
#                   Defaults to ${FIXgsm}/seaice_newland.grb
#     FNOROG        Model orography file (netcdf format)
#                   Defaults to {FIXfv3}/C${CRES}_oro_data.tile1.nc
#     FNGRID        Model grid file (netcdf format)
#                   Defaults to ${FIXfv3}/C${CRES}_grid.tile1.nc
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
#     Z1/Z2         When running with NST model, this is the lower/upper bound
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
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

#  Command line arguments.
export SFCGES=${1:-${SFCGES:?}}
export SFCANL=${2:-${SFCANL}}

export CRES=${CRES:-768}

#  Directories.
export global_shared_ver=${global_shared_ver:-v15.0.0}
export BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
export HOMEglobal=${HOMEglobal:-$BASEDIR/global_shared.${global_shared_ver}}
export EXECgsm=${EXECgsm:-$HOMEglobal/exec}
export FIXSUBDA=${FIXSUBDA:-fix/fix_am}
export FIXgsm=${FIXgsm:-$HOMEglobal/$FIXSUBDA}
export FIXfv3=${FIXfv3:-$HOMEglobal/fix/fix_fv3/C${CRES}}
export DATA=${DATA:-$(pwd)}
export COMIN=${COMIN:-$(pwd)}
export COMOUT=${COMOUT:-$(pwd)}

#  Filenames.
export XC=${XC}
export PREINP=${PREINP}
export SUFINP=${SUFINP}
export JCAP=${JCAP:-1534}
export CYCLEXEC=${CYCLEXEC:-$EXECgsm/global_cycle$XC}

export CDATE=${CDATE:?}
export FHOUR=${FHOUR:-00}
export LONB=${LONB:-3072}
export LATB=${LATB:-1536}
export DELTSFC=${DELTSFC:-0}

export LSOIL=${LSOIL:-4}
export FSMCL2=${FSMCL2:-60}
export FSLPL=${FSLPL:-99999.}
export FSOTL=${FSOTL:-99999.}
export FVETL=${FVETL:-99999.}
export IALB=${IALB:-1}
export ISOT=${ISOT:-1}
export IVEGSRC=${IVEGSRC:-1}
export CYCLVARS=${CYCLVARS}
export use_ufo=${use_ufo:-.true.}
export NST_ANL=${NST_ANL:-.false.}
export Z1=${Z1:-0}
export Z2=${Z2:-0}

export FNGLAC=${FNGLAC:-${FIXgsm}/global_glacier.2x2.grb}
export FNMXIC=${FNMXIC:-${FIXgsm}/global_maxice.2x2.grb}
export FNTSFC=${FNTSFC:-${FIXgsm}/RTGSST.1982.2012.monthly.clim.grb}
export FNSNOC=${FNSNOC:-${FIXgsm}/global_snoclim.1.875.grb}
export FNZORC=${FNZORC:-igbp}
export FNALBC=${FNALBC:-${FIXgsm}/global_snowfree_albedo.bosu.t$JCAP.$LONB.$LATB.rg.grb}
export FNALBC2=${FNALBC2:-${FIXgsm}/global_albedo4.1x1.grb}
export FNAISC=${FNAISC:-${FIXgsm}/CFSR.SEAICE.1982.2012.monthly.clim.grb}
export FNTG3C=${FNTG3C:-${FIXgsm}/global_tg3clim.2.6x1.5.grb}
export FNVEGC=${FNVEGC:-${FIXgsm}/global_vegfrac.0.144.decpercent.grb}
export FNVETC=${FNVETC:-${FIXgsm}/global_vegtype.igbp.t$JCAP.$LONB.$LATB.rg.grb}
export FNSOTC=${FNSOTC:-${FIXgsm}/global_soiltype.statsgo.t$JCAP.$LONB.$LATB.rg.grb}
export FNSMCC=${FNSMCC:-${FIXgsm}/global_soilmgldas.t${JCAP}.${LONB}.${LATB}.grb}
export FNVMNC=${FNVMNC:-${FIXgsm}/global_shdmin.0.144x0.144.grb}
export FNVMXC=${FNVMXC:-${FIXgsm}/global_shdmax.0.144x0.144.grb}
export FNSLPC=${FNSLPC:-${FIXgsm}/global_slope.1x1.grb}
export FNABSC=${FNABSC:-${FIXgsm}/global_mxsnoalb.uariz.t$JCAP.$LONB.$LATB.rg.grb}
export FNMSKH=${FNMSKH:-${FIXgsm}/seaice_newland.grb}
export FNOROG=${FNOROG:-${FIXfv3}/C${CRES}_oro_data.tile1.nc}
export FNGRID=${FNGRID:-${FIXfv3}/C${CRES}_grid.tile1.nc}
export GSI_FILE=${GSI_FILE:-" "}
export FNTSFA=${FNTSFA:-${COMIN}/${PREINP}sstgrb${SUFINP}}
export FNACNA=${FNACNA:-${COMIN}/${PREINP}engicegrb${SUFINP}}
export FNSNOA=${FNSNOA:-${COMIN}/${PREINP}snogrb${SUFINP}}
export SFCANL=${SFCANL:-${COMIN}/${PREINP}sfcanl}
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
  isot=$ISOT,ivegsrc=$IVEGSRC,z1=$Z1,z2=$Z2
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
