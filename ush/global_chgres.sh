#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_chgres.sh
# Script description:  Convert GFS restart files to the FV3 cubed-sphere grid.
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-03-01
#
# Abstract: This script converts the GFS restart files, namely the sigma
#   file, surface file, nst file or all 3, to the cubed-sphere grid.  When
#   converting an nst file, you must also convert a surface file.
#   All input files are specified by the first three arguments.
#   The horizontal/vertical resolution of the output files is given by
#   the CASE/LEVS arguments.  When the input sigma file is sigio format,
#   the conversion is done in two steps.  First, the spectral coefficients
#   are converted to grid point space.  By default, this intermediate
#   data are on a gaussian grid with i/j dimension as described by the
#   input file header.  These defaults may be overridden by the IDRT,
#   LONB and LATB environment variables.  The sigma file is converted
#   to all six sides of the cube.  The surface and nst files are only
#   converted to one side of the cube (as specified by the TILE_NUM
#   environment variable).  I.e., you must run this script six times
#   to fully convert a surface or nst file.
#
# Script history log:
# 1999-03-01  Mark Iredell
# 2011-08-05  Added logic for nst restart files.  G. Gayno
# 2011-10-10  Updated for gaea  - S. Moorthi
# 2016-12-27  Updated for FV3 core.  G. Gayno
# 2017-04-12  Remove references to output nsst file.  nsst data
#             now written to surface restart file.
# 2018-02-09  Updated for regional grids.
#
# Usage:  global_chgres.sh SIGINP SFCINP NSTINP CASE LEVS
#
#   Input script positional parameters:
#     1             Input sigma file (SIGINP)
#     2             Input surface file (SFCINP)
#     3             Input nst file (NSTINP)
#     4             Output cubed-sphere resolution (CASE)
#     5             New number of vertical sigma levels (LEVS)
#
#   Imported Shell Variables:
#     SIGINP        Input sigma file
#                   overridden by $1; skip sigma conversion if missing
#     SFCINP        Input surface file
#                   overridden by $2; skip surface conversion if missing
#     NSTINP        Input nst file
#                   overridden by $3; skip surface conversion if missing
#     CASE          Output cubed-sphere resolution.
#                   overridden by $4.
#     LEVS          New number of sigma levels
#                   overridden by $5; one or the other is required
#     OUTTYP        Output file type.  Not used yet.  The sigma/atms and
#                   surface/nsst files are output in netcdf.
#     IDRT          When converting an atmospheric file in sigio format,
#                   this is the grid type after spectral conversion.
#                   4: guassian(default); 0: lat-lon
#     LONB          When converting an atmospheric file in sigio format,
#                   this is the number of longitudes of the intermediate grid
#                   after spectral conversion.  When converting a surface
#                   file, this is the number of longitudes of the input
#                   climatological soil moisture data file.
#     LATB          When converting an atmospheric file in sigio format,
#                   this is the number of latitudes of the intermediate grid
#                   after spectral conversion.  When converting a surface
#                   file, this is the number of latitudes of the input
#                   climatological soil moisture data file.
#     NTRAC         New number of tracers
#                   defaults to input sigma file value
#     REGIONAL      Process stand-alone regional grid.  When '1', remove halo
#                   from grids and create an atmospheric boundary file.
#                   When '2', create boundary file only.  When '0', 
#                   do neither (process as normal for a global grid).
#                   Default is '0'.
#     HALO          When processing a stand-alone regional grid, this
#                   specifies the number of rows/cols for the halo.
#                   Default is '0'.
#     IDVC          New vertical coordinate id (1 for sigma, 2 for hybrid)
#                   defaults to input sigma file value
#     IDSL          New midlayer pressure id (1 for phillips, 2 for mean)
#                   defaults to input sigma file value
#     TILE_NUM      The number of the cubed-sphere tile to convert surface
#                   and nst data.
#     NWROOT        A string that defaults to /nwprod
#     gfs_ver       Version number.  Defaults to v15.0.0.
#     BASEDIR       Base directory.  Defaults to /nwprod2
#     HOMEgfs       GFS home directory.  Defaults to $BASEDIR/gfs.${gfs_ver}
#     FIXam         Directory for global climo files
#                   Defaults to $HOMEgfs/fix/fix_am
#     FIXfv3        Directory for model 'grid' and 'orography' files.
#                   Defaults to HOMEgfs/fix/fix_fv3_gmted2010
#     EXECgfs       Directory for global executables.
#                   Defaults to $HOMEgfs/exec
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     XC            Suffix to add to executables
#                   defaults to none
#     SIGLEVEL      New sigma levels ("NULL" to use from input sigma file)
#                   defaults to ${FIXam}/global_siglevel.l${LEVS}.txt
#     FNGLAC        Input glacier climatology GRIB file
#                   defaults to ${FIXam}/global_glacier.2x2.grb
#     FNMXIC        Input maximum sea ice climatology GRIB file
#                   defaults to ${FIXam}/global_maxice.2x2.grb
#     FNTSFC        Input SST climatology GRIB file
#                   defaults to ${FIXam}/global_sstclim.2x2.grb
#     FNSNOC        Input snow climatology GRIB file
#                   defaults to ${FIXam}/global_snoclim.1.875.grb
#     FNZORC        Input roughness climatology
#                   defaults to sib vegtetation type-based lookup table
#                   FNVETC must be set to ${FIXam}/global_vegtype.1x1.grb
#     FNALBC        Input 4-component albedo climatology GRIB file
#                   defaults to ${FIXam}/global_albedo4.1x1.grb
#     FNALBC2       Input 'facsf' and 'facwf' albedo climatology GRIB file
#                   defaults to ${FIXam}/global_albedo4.1x1.grb
#     FNAISC        Input sea ice climatology GRIB file
#                   defaults to ${FIXam}/global_iceclim.2x2.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file
#                   defaults to ${FIXam}/global_tg3clim.2.6x1.5.grb
#     FNVEGC        Input vegetation fraction climatology GRIB file
#                   defaults to ${FIXam}/global_vegfrac.1x1.grb
#     FNVETC        Input vegetation type climatology GRIB file
#                   defaults to ${FIXam}/global_vegtype.1x1.grb
#     FNSOTC        Input soil type climatology GRIB file
#                   defaults to ${FIXam}/global_soiltype.1x1.grb
#     FNSMCC        Input soil moisture climatology GRIB file
#                   defaults to ${FIXam}/global_soilmgldas.statsgo.t$JCAP.$LONB.$LATB.grb
#     FNVMNC        Input min veg frac climatology GRIB file
#                   defaults to ${FIXam}/global_shdmin.0.144x0.144.grb
#     FNVMXC        Input max veg frac climatology GRIB file
#                   defaults to ${FIXam}/global_shdmax.0.144x0.144.grb
#     FNSLPC        Input slope type climatology GRIB file
#                   defaults to ${FIXam}/global_slope.1x1.grb
#     FNABSC        Input max snow albedo climatology GRIB file
#                   defaults to ${FIXam}/global_snoalb.1x1.grb
#     FNMSKH        Input high resolution land mask GRIB file
#                   defaults to ${FIXam}/seaice_newland.grb
#     CLIMO_
#     FIELDS_OPT    1-Climo/static fields (albedo, soil type, greenness, etc.)
#                     interpolated from input grid.
#                   2-Vegetation, slope and soil type interpolated from input
#                     grid.  All other climo/static fields from sfccycle.
#                   3-All climo/static fields from sfccycle.
#                   defaults to '3'
#     LANDICE_OPT   1-Input no landice => output landice
#                   2-Input landice => output landice.
#                   3-Input no landice => output no landice
#                   4-Input landice => output no landice
#                   5-Output landice regardless of input
#     LSOIL         2-Output file with 2 soil layers
#                   4-Output file with 4 soil layers
#                   0-Default, number of soil layers same as input file
#     IVSSFC        Version number of surface restart file
#                   0-Default, same as input file.
#     LONSPERLAT    New lonsperlat ("NULL" to use from input surface file)
#     CHGRESEXEC    Change resolution executable
#                   defaults to ${EXECgfs}/global_chgres
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     CHGRESVARS    Other namelist inputs to the change resolution executable
#                   such as IGEN,MQUICK.  Defaults to none set.
#     NTHREADS      Number of threads
#                   defaults to 1
#     NTHSTACK      Size of stack per thread
#                   defaults to 64000000
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
#     FV3GRID_TILE# Contains grid information (lat/lon) for the cubed-sphere grid.
#                   One file for each of the six tiles.
#     FV3OROG_TILE# Contains mask and orography for the cubed-sphere grid.
#                   One file for each of the six tiles.
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
#     programs   : $CHGRESEXEC
#
#     input data : $1 or $SIGINP
#                  $2 or $SFCINP
#                  $3 or $NSTINP
#                  $SIGLEVEL
#                  $LONSPERLAT
#                  $FVGRID_TILE[1-6]
#                  $FVOROG_TILE[1-6]
#
#     output data:
#                  $PGMOUT
#                  $PGMERR
#
#     scratch    : ${DATA}/chgres.inp.sig
#                  ${DATA}/chgres.inp.siglevel
#                  ${DATA}/chgres.inp.sfc
#                  ${DATA}/chgres.inp.nst
#                  ${DATA}/chgres.inp.lpl3
#                  ${DATA}/chgres.fv3.grd.t[1-6]
#                  ${DATA}/chgres.fv3.orog.t[1-6]
#                  ${DATA}/fort.35
#                  ${DATA}/fort.81
#                  ${DATA}/NULL
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
####
################################################################################
#  Set environment.
VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
#  Command line arguments.
APRUNC=${APRUNC:-""}
SIGINP=${1:-${SIGINP:-NULL}}
SFCINP=${2:-${SFCINP:-NULL}}
NSTINP=${3:-${NSTINP:-NULL}}
CASE=${4:-${CASE:?}}
LEVS=${5:-${LEVS:?}}
#  Directories.
gfs_ver=${gfs_ver:-v15.0.0}
BASEDIR=${BASEDIR:-${NWROOT:-/nwprod2}}
HOMEgfs=${HOMEgfs:-$BASEDIR/gfs.${gfs_ver}}
EXECgfs=${EXECgfs:-$HOMEgfs/exec}
FIXfv3=${FIXfv3:-$HOMEgfs/fix/fix_fv3_gmted2010}
FIXam=${FIXam:-$HOMEgfs/fix/fix_am}

DATA=${DATA:-$(pwd)}
#  Filenames.
CHGRESEXEC=${CHGRESEXEC:-${EXECgfs}/global_chgres$XC}
#

CRES=$(echo $CASE | cut -c2-)
JCAP_CASE=$((CRES*2-2))
LATB_CASE=$((CRES*2))
LONB_CASE=$((CRES*4))

JCAP=${JCAP:-$JCAP_CASE}
LONB=${LONB:-$LONB_CASE}
LATB=${LATB:-$LATB_CASE}

SIGLEVEL=${SIGLEVEL:-${FIXam}/global_hyblev.l${LEVS}.txt}
if [ $LEVS = 128 ]; then
  SIGLEVEL=${SIGLEVEL:-${FIXam}/global_hyblev.l${LEVS}B.txt}
fi
FNGLAC=${FNGLAC:-${FIXam}/global_glacier.2x2.grb}
FNMXIC=${FNMXIC:-${FIXam}/global_maxice.2x2.grb}
FNTSFC=${FNTSFC:-${FIXam}/cfs_oi2sst1x1monclim19822001.grb}
FNSNOC=${FNSNOC:-${FIXam}/global_snoclim.1.875.grb}
FNZORC=${FNZORC:-sib}
FNALBC=${FNALBC:-${FIXam}/global_albedo4.1x1.grb}
FNALBC2=${FNALBC2:-${FIXam}/global_albedo4.1x1.grb}
FNAISC=${FNAISC:-${FIXam}/cfs_ice1x1monclim19822001.grb}
FNTG3C=${FNTG3C:-${FIXam}/global_tg3clim.2.6x1.5.grb}
FNVEGC=${FNVEGC:-${FIXam}/global_vegfrac.0.144.decpercent.grb}
FNVETC=${FNVETC:-${FIXam}/global_vegtype.1x1.grb}
FNSOTC=${FNSOTC:-${FIXam}/global_soiltype.1x1.grb}
FNSMCC=${FNSMCC:-${FIXam}/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb}
FNVMNC=${FNVMNC:-${FIXam}/global_shdmin.0.144x0.144.grb}
FNVMXC=${FNVMXC:-${FIXam}/global_shdmax.0.144x0.144.grb}
FNSLPC=${FNSLPC:-${FIXam}/global_slope.1x1.grb}
FNABSC=${FNABSC:-${FIXam}/global_snoalb.1x1.grb}
FNMSKH=${FNMSKH:-${FIXam}/seaice_newland.grb}
LANDICE_OPT=${LANDICE_OPT:-2}
CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-3}
SOILTYPE_INP=${SOILTYPE_INP:-"zobler"}
SOILTYPE_OUT=${SOILTYPE_OUT:-"zobler"}
VEGTYPE_INP=${VEGTYPE_INP:-"sib"}
VEGTYPE_OUT=${VEGTYPE_OUT:-"sib"}
LONSPERLAT=${LONSPERLAT:-NULL}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
TILE_NUM=${TILE_NUM:-1}
IDRT=${IDRT:-4}
OUTTYP=${OUTTYP:-999}
NTRAC=${NTRAC:-3}
REGIONAL=${REGIONAL:-0}
HALO=${HALO:-0}
gtype=${gtype:-uniform}
IALB=${IALB:-0}
IDVC=${IDVC:-2}
IDVT=${IDVT:-21}
IDVM=${IDVM:-0}
IDSL=${IDSL:-1}
LSOIL=${LSOIL:-0}
IVSSFC=${IVSSFC:-0}
use_ufo=${use_ufo:-.true.}
rdgrid=${rdgrid:-.false.}
NTHREADS=${NTHREADS:-1}
NTHSTACK=${NTHSTACK:-1024000000}
XLSMPOPTS=${XLSMPOPTS:-"parthds=$NTHREADS:stack=$NTHSTACK"}
export KMP_STACKSIZE=${KMP_STACKSIZE:-$NTHSTACK}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}
CHGRESVARS=${CHGRESVARS}
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
################################################################################
#  Change resolution
#export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"
export PGM=$CHGRESEXEC
export pgm=$PGM
$LOGSCRIPT
rm -f NULL
ln -sf $SIGINP        chgres.inp.sig
ln -sf $SIGLEVEL      chgres.inp.siglevel
ln -sf $SFCINP        chgres.inp.sfc
ln -sf $NSTINP        chgres.inp.nst
ln -sf $LONSPERLAT    chgres.inp.lpl3

if [ $gtype = regional ]; then
 tile=7
 ln -sf ${FIXfv3}/${CASE}/${CASE}_grid.tile${tile}.halo${HALO}.nc chgres.fv3.grd.t${tile}
 ln -sf ${FIXfv3}/${CASE}/${CASE}_oro_data.tile${tile}.halo${HALO}.nc chgres.fv3.orog.t${tile}
else
 tile=1
 while [ $tile -le $ntiles ]; do
  ln -sf ${FIXfv3}/C${CRES}/C${CRES}_grid.tile${tile}.nc chgres.fv3.grd.t${tile}
  ln -sf ${FIXfv3}/C${CRES}/C${CRES}_oro_data.tile${tile}.nc chgres.fv3.orog.t${tile}
  tile=`expr $tile + 1 `
 done
fi

if [[ $LANDICE_OPT = 3 || $LANDICE_OPT = 4 ]]
then
 LANDICE=.false.
else
 LANDICE=.true.
fi

if [[ $VEGTYPE_OUT = "sib" ]]; then
 IVEGSRC=2
elif [[ $VEGTYPE_OUT = "igbp" ]]; then
 IVEGSRC=1
fi

if [[ $SOILTYPE_OUT = "zobler" ]]; then
 ISOT=0
elif [[ $SOILTYPE_OUT = "statsgo" ]]; then
 ISOT=1
fi

# If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
[[ ! -f $FNSMCC ]] && FNSMCC="$FIXam/global_soilmgldas.statsgo.t1534.3072.1536.grb"

cat << EOF > fort.35
 &NAMSFC
  FNGLAC='${FNGLAC}'
  FNMXIC='${FNMXIC}'
  FNTSFC='${FNTSFC}'
  FNSNOC='${FNSNOC}'
  FNZORC='${FNZORC}'
  FNALBC='${FNALBC}'
  FNALBC2='${FNALBC2}'
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

if [[ $SOILTYPE_INP = "zobler" ]]; then
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
EOF
elif [[ $SOILTYPE_INP = "statsgo" ]]; then
cat << EOF > fort.81
 &soil_parameters
  soil_src_input = "statsgo"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                0.464, -9.99, 0.200, 0.421
  beta_input  = 4.05, 4.26, 4.74, 5.33, 5.33, 5.25,
                6.77, 8.72, 8.17, 10.73, 10.39, 11.55,
                5.25, -9.99, 4.05, 4.26
  psis_input  = 0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                0.3548, -9.99,  0.0350, 0.0363
  satdk_input = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                1.4078e-5
EOF
fi

if [[ $SOILTYPE_OUT = "zobler" ]]; then
cat << EOF >> fort.81
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
EOF
elif [[ $SOILTYPE_OUT = "statsgo" ]]; then
cat << EOF >> fort.81
  soil_src_output = "statsgo"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                 0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                 0.464, -9.99, 0.200, 0.421
  beta_output  = 4.05, 4.26, 4.74, 5.33, 5.33, 5.25,
                 6.77, 8.72, 8.17, 10.73, 10.39, 11.55,
                 5.25, -9.99, 4.05, 4.26
  psis_output  = 0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                 0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                 0.3548, -9.99,  0.0350, 0.0363
  satdk_output = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                 3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                 1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                 1.4078e-5
/
EOF
fi

cat << EOF >> fort.81
 &veg_parameters
  veg_src_input = "${VEGTYPE_INP}"
  veg_src_output = "${VEGTYPE_OUT}"
  salp_output= -999.
  snup_output= -999.
/
 &options
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT}
  LANDICE_OPT=${LANDICE_OPT}
 /
EOF

export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-${CHGRESTHREAD:-1}}

 eval $APRUNC $CHGRESEXEC <<EOF $REDOUT$PGMOUT $REDERR$PGMERR
  &NAMCHG  LEVS=$LEVS, LONB=$LONB, LATB=$LATB,
           NTRAC=$NTRAC, IDVC=$IDVC, IDSL=$IDSL,
           LSOIL=$LSOIL, IVSSFC=$IVSSFC, OUTTYP=$OUTTYP,
           IDRT=$IDRT, IALB=$IALB, ISOT=$ISOT,
           IVEGSRC=$IVEGSRC, TILE_NUM=$TILE_NUM, REGIONAL=$REGIONAL,
           HALO=$HALO, NTILES=$ntiles, $CHGRESVARS
 /
EOF

export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2

rm -f NULL
rm -f chgres.inp.sig chgres.inp.siglevel
rm -f chgres.inp.sfc chgres.inp.nst chgres.inp.lpl3
rm -f fort.35 fort.81
rm -f chgres.fv3.grd.t? chgres.fv3.orog.t?
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
