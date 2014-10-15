#!/bin/sh
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
#     SFCGES        Input surface guess
#                   overridden by $1; required
#     SFCANL        Output surface analysis
#                   overridden by $5; defaults to ${COMOUT}/sfcanl
#     FIXGLOBAL     Directory for global fixed files
#                   defaults to /nwprod/fix
#     EXECGLOBAL    Directory for global executables
#                   defaults to /nwprod/exec
#     USHGLOBAL     Directory for global scripts
#                   defaults to /nwprod/ush
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     COMIN         input directory
#                   defaults to current working directory
#     COMOUT        output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     XC            Suffix to add to executables
#                   defaults to none
#     PREINP        Prefix to add to input observation files
#                   defaults to none
#     SUFINP        Suffix to add to input observation files
#                   defaults to none
#     NCP           Copy command
#                   defaults to cp
#     SFCHDR        Command to read surface header
#                   defaults to ${EXECGLOBAL}/global_sfchdr$XC
#     CYCLEXEC      Surface cycle executable
#                   defaults to ${EXECGLOBAL}/global_cycle$XC
#     FNGLAC        Input glacier climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_glacier.2x2.grb
#     FNMXIC        Input maximum sea ice climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_maxice.2x2.grb
#     FNTSFC        Input SST climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_sstclim.2x2.grb
#     FNSNOC        Input snow climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_snoclim.1.875.grb
#     FNZORC        Input roughness climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_zorclim.1x1.grb
#     FNALBC        Input albedo climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_albedo4.1x1.grb
#     FNAISC        Input sea ice climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_iceclim.2x2.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb
#     FNVEGC        Input vegetation fraction climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_vegfrac.1x1.grb
#     FNVETC        Input vegetation type climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_vegtype.1x1.grb
#     FNSOTC        Input soil type climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_soiltype.1x1.grb
#     FNSMCC        Input soil moisture climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_soilmcpc.1x1.grb
#     FNVMNC        Input min veg frac climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_shdmin.0.144x0.144.grb
#     FNVMXC        Input max veg frac climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_shdmax.0.144x0.144.grb
#     FNSLPC        Input slope type climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_slope.1x1.grb
#     FNABSC        Input max snow albedo climatology GRIB file
#                   defaults to ${FIXGLOBAL}/global_snoalb.1x1.grb
#     FNMSKH        Input high resolution land mask GRIB file
#                   defaults to ${FIXGLOBAL}/seaice_newland.grb
#     FNOROG        Input orography GRIB file (horiz resolution dependent)
#                   defaults to ${FIXGLOBAL}/global_orography.t$JCAP.grb
#     FNMASK        Input land mask GRIB file (horiz resolution dependent)
#                   defaults to ${FIXGLOBAL}/global_slmask.t$JCAP.grb
#     FNTSFA        Input SST analysis GRIB file
#                   defaults to ${COMIN}/${PREINP}sstgrb${SUFINP}
#     FNACNA        Input sea ice analysis GRIB file
#                   defaults to ${COMIN}/${PREINP}engicegrb${SUFINP}
#     FNSNOA        Input snow analysis GRIB file
#                   defaults to ${COMIN}/${PREINP}snogrb${SUFINP}
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     KEEPFH        Flag to keep fhour the same (YES or NO)
#                   defaults to NO
#     JCAP          Spectral truncation
#                   defaults to 382
#     CDATE         Output analysis date in yyyymmddhh format
#                   defaults to the value in the input surface file header
#     FHOUR         Output forecast hour
#                   defaults to the value in the input surface file header
#     LATB          Number of latitudes in surface cycling
#                   defaults to the value in the input surface file header
#     LONB          Number of longitudes in surface cycling
#                   defaults to the value in the input surface file header
#     LSOIL         Number of soil layers
#                   defaults to 4
#     FSMCL2        Scale in days to relax to soil moisture climatology
#                   defaults to 60
#     DELTSFC       Cycling frequency in hours
#                   defaults to forecast hour of $SFCGES
#     CYCLVARS      Other namelist inputs to the cycle executable
#                   defaults to none set
#     NTHREADS      Number of threads
#                   defaults to 1
#     NTHSTACK      Size of stack per thread
#                   defaults to 64000000
#     FILESTYLE     File management style flag
#                   ('C' to copy to/from $DATA, 'L' for symbolic links in $DATA,
#                    'X' to use XLFUNIT or symbolic links where appropriate)
#                   defaults to 'X'
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
#                  $FNMASK
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
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
#  Command line arguments.
export SFCGES=${1:-${SFCGES:?}}
export SFCANL=${2:-${SFCANL}}
#  Directories.
export FIXGLOBAL=${FIXGLOBAL:-/nwprod/fix}
export EXECGLOBAL=${EXECGLOBAL:-/nwprod/exec}
export USHGLOBAL=${USHGLOBAL:-/nwprod/ush}
export DATA=${DATA:-$(pwd)}
export COMIN=${COMIN:-$(pwd)}
export COMOUT=${COMOUT:-$(pwd)}
#  Filenames.
export XC=${XC}
export PREINP=${PREINP}
export SUFINP=${SUFINP}
export KEEPFH=${KEEPFH:-NO}
export JCAP=${JCAP:-382}
export SFCHDR=${SFCHDR:-${EXECGLOBAL}/global_sfchdr$XC}
export CYCLEXEC=${CYCLEXEC:-${EXECGLOBAL}/global_cycle$XC}
export FNGLAC=${FNGLAC:-${FIXGLOBAL}/global_glacier.2x2.grb}
export FNMXIC=${FNMXIC:-${FIXGLOBAL}/global_maxice.2x2.grb}
export FNTSFC=${FNTSFC:-${FIXGLOBAL}/cfs_oi2sst1x1monclim19822001.grb}
export FNSNOC=${FNSNOC:-${FIXGLOBAL}/global_snoclim.1.875.grb}
export FNZORC=${FNZORC:-${FIXGLOBAL}/global_zorclim.1x1.grb}
export FNALBC=${FNALBC:-${FIXGLOBAL}/global_albedo4.1x1.grb}
export FNAISC=${FNAISC:-${FIXGLOBAL}/cfs_ice1x1monclim19822001.grb}
export FNTG3C=${FNTG3C:-${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb}
export FNVEGC=${FNVEGC:-${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb}
export FNVETC=${FNVETC:-${FIXGLOBAL}/global_vegtype.1x1.grb}
export FNSOTC=${FNSOTC:-${FIXGLOBAL}/global_soiltype.1x1.grb}
export FNSMCC=${FNSMCC:-${FIXGLOBAL}/global_soilmcpc.1x1.grb}
export FNVMNC=${FNVMNC:-${FIXGLOBAL}/global_shdmin.0.144x0.144.grb}
export FNVMXC=${FNVMXC:-${FIXGLOBAL}/global_shdmax.0.144x0.144.grb}
export FNSLPC=${FNSLPC:-${FIXGLOBAL}/global_slope.1x1.grb}
export FNABSC=${FNABSC:-${FIXGLOBAL}/global_snoalb.1x1.grb}
export FNMSKH=${FNMSKH:-${FIXGLOBAL}/seaice_newland.grb}
export FNOROG=${FNOROG:-${FIXGLOBAL}/global_orography.t$JCAP.grb}
export FNMASK=${FNMASK:-${FIXGLOBAL}/global_slmask.t$JCAP.grb}
export FNTSFA=${FNTSFA:-${COMIN}/${PREINP}sstgrb${SUFINP}}
export FNACNA=${FNACNA:-${COMIN}/${PREINP}engicegrb${SUFINP}}
export FNSNOA=${FNSNOA:-${COMIN}/${PREINP}snogrb${SUFINP}}
export SFCANL=${SFCANL:-${COMIN}/${PREINP}sfcanl}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
if [[ $KEEPFH = YES ]];then
export CDATE=${CDATE:-$($SFCHDR $SFCGES IDATE||echo 0)}
export FHOUR=${FHOUR:-$($SFCHDR $SFCGES FHOUR||echo 0)}
else
export CDATE=${CDATE:-$($SFCHDR $SFCGES VDATE||echo 0)}
export FHOUR=${FHOUR:-0}
fi
export LATB=${LATB:-$($SFCHDR $SFCGES LATB||echo 0)}
export LONB=${LONB:-$($SFCHDR $SFCGES LONB||echo 0)}
export LSOIL=${LSOIL:-4}
export FSMCL2=${FSMCL2:-60}
export DELTSFC=${DELTSFC:-$($SFCHDR $SFCGES FHOUR||echo 0)}
export CYCLVARS=${CYCLVARS}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-64000000}
export FILESTYLE=${FILESTYLE:-'X'}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
#typeset -L1 l=$PGMOUT
[[ $l = '&' ]]&&a=''||a='>'
export REDOUT=${REDOUT:-'1>'$a}
#typeset -L1 l=$PGMERR
[[ $l = '&' ]]&&a=''||a='>'
export REDERR=${REDERR:-'2>'$a}
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
export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"
export PGM=$CYCLEXEC
export pgm=$PGM
#$LOGSCRIPT

rm $SFCANL
iy=$(echo $CDATE|cut -c1-4)
im=$(echo $CDATE|cut -c5-6)
id=$(echo $CDATE|cut -c7-8)
ih=$(echo $CDATE|cut -c9-10)

cat $HOSTFILE
#echo "$MPICH/bin/mpirun_rsh -hostfile $HOSTFILE -np 1 $CYCLEXEC"
#eval $CYCLEXEC <<EOF 
cat << EOF > fort.912
 &NAMCYC 
  idim=$LONB, jdim=$LATB, lsoil=$LSOIL,
  iy=$iy, im=$im, id=$id, ih=$ih, fh=$FHOUR,
  DELTSFC=$DELTSFC,
 /
 &NAMSFCD
  FNBGSI="$SFCGES",
  FNBGSO="$SFCANL",
  FNOROG="$FNOROG",
  FNMASK="$FNMASK",
  LDEBUG=.false.,
 /
 &NAMSFC
  FNGLAC="$FNGLAC",
  FNMXIC="$FNMXIC",
  FNTSFC="$FNTSFC",
  FNSNOC="$FNSNOC",
  FNZORC="$FNZORC",
  FNALBC="$FNALBC",
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
  FSMCL(2)=$FSMCL2,
  FSMCL(3)=$FSMCL2,
  FSMCL(4)=$FSMCL2,
  $CYCLVARS
 /
EOF
cat fort.912
$MPICH/bin/mpirun_rsh -hostfile $HOSTFILE -np 1 $CYCLEXEC 
#$MPICH/bin/mpiexec -hostfile $HOSTFILE -n 1 $CYCLEXEC 

export ERR=$?
export err=$ERR
#$ERRSCRIPT||exit 2

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
#$ENDSCRIPT
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
