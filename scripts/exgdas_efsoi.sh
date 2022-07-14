#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_efsoi.sh
# Script description:  Runs efsoi to make observation sensitivities
#
# Author:        Liaofan Lin/Andrew Eichmann          Date: 2020-12-03
#
# Abstract: This script runs the efsoi executable
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#   Machine: Hera
#
################################################################################

# Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

# Directories.
pwd=$(pwd)

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
NEMSIOGET=${NEMSIOGET:-$NWPROD/utils/exec/nemsio_get}
NCLEN=${NCLEN:-$HOMEgfs/ush/getncdimlen}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""
if [ $CFP_MP = "YES" ]; then
    nm=0
fi
APRUNCFP=${APRUNCFP:-""}
APRUN_ENKF=${APRUN_ENKF:-${APRUN:-""}}
NTHREADS_ENKF=${NTHREADS_ENKF:-${NTHREADS:-1}}

# Executables
EFSOIEXEC=${EFSOIEXEC:-$HOMEgfs/exec/global_efsoi.x}

# Cycling and forecast hour specific parameters
CDATE=${CDATE:-"2001010100"}

# Filenames.
GPREFIX=${GPREFIX:-""}
GSUFFIX=${GSUFFIX:-$SUFFIX}
APREFIX=${APREFIX:-""}
ASUFFIX=${ASUFFIX:-$SUFFIX}
VPREFIX=${VPREFIX:-""}
VSUFFIX=${VSUFFIX:-$SUFFIX}

SMOOTH_ENKF=${SMOOTH_ENKF:-"YES"}

EFSOISTAT=${EFSOISTAT:-${APREFIX}efsoistat}

VERFANL=${VERFANL:-${VPREFIX}atmanl.ensres.nc}
INITANL=${INITANL:-${APREFIX}atmanl.ensres.nc}
FCSTLONG=${GPREFIX}atmf030.ensmean.nc
FCSTSHORT=${APREFIX}atmf024.ensmean.nc
OSENSEIN=osense_${CDATE}_init.dat
OSENSEOUT=osense_${CDATE}_final.dat

# this needs to be set manually because params in enkf will default to fhr03
fgfileprefixes=sfg_${CDATE}_fhr06_

#analysise Namelst parameters
NMEM_ENKF=${NMEM_ENKF:-80}
NAM_ENKF=${NAM_ENKF:-""}
corrlength=${corrlength:-1250}
lnsigcutoff=${lnsigcutoff:-2.5}
reducedgrid=${reducedgrid:-".true."}
letkf_flag=${letkf_flag:-".false."}
getkf=${getkf:-".false."}
denkf=${denkf:-".false."}
nobsl_max=${nobsl_max:-10000}
lobsdiag_forenkf=${lobsdiag_forenkf:-".false."}
write_spread_diag=${write_spread_diag:-".false."}
cnvw_option=${cnvw_option:-".false."}
modelspace_vloc=${modelspace_vloc:-".false."} # if true, 'vlocal_eig.dat' is needed

################################################################################
ATMGES_ENSMEAN=$COMIN_ANL/$VERFANL
if [ $SUFFIX = ".nc" ]; then
   LONB_ENKF=${LONB_ENKF:-$($NCLEN $ATMGES_ENSMEAN grid_xt)} # get LONB_ENKF
   LATB_ENKF=${LATB_ENKF:-$($NCLEN $ATMGES_ENSMEAN grid_yt)} # get LATB_ENFK
   LEVS_ENKF=${LEVS_ENKF:-$($NCLEN $ATMGES_ENSMEAN pfull)} # get LEVS_ENFK
   use_gfs_ncio=".true."
   use_gfs_nemsio=".false."
   paranc=${paranc:-".true."}
else
   LEVS_ENKF=${LEVS_ENKF:-$($NEMSIOGET $ATMGES_ENSMEAN dimz | awk '{print $2}')}
   LATB_ENKF=${LATB_ENKF:-$($NEMSIOGET $ATMGES_ENSMEAN dimy | awk '{print $2}')}
   LONB_ENKF=${LONB_ENKF:-$($NEMSIOGET $ATMGES_ENSMEAN dimx | awk '{print $2}')}
   use_gfs_ncio=".false."
   use_gfs_nemsio=".true."
   paranc=${paranc:-".false."}
fi
LATA_ENKF=${LATA_ENKF:-$LATB_ENKF}
LONA_ENKF=${LONA_ENKF:-$LONB_ENKF}

SATANGL=${SATANGL:-${FIXgsi}/global_satangbias.txt}
SATINFO=${SATINFO:-${FIXgsi}/global_satinfo.txt}
CONVINFO=${CONVINFO:-${FIXgsi}/global_convinfo.txt}
OZINFO=${OZINFO:-${FIXgsi}/global_ozinfo.txt}
SCANINFO=${SCANINFO:-${FIXgsi}/global_scaninfo.txt}
HYBENSINFO=${HYBENSINFO:-${FIXgsi}/global_hybens_info.l${LEVS_ENKF}.txt}
ANAVINFO=${ANAVINFO:-${FIXgsi}/global_anavinfo.l${LEVS_ENKF}.txt}
VLOCALEIG=${VLOCALEIG:-${FIXgsi}/vlocal_eig_l${LEVS_ENKF}.dat}

ENKF_SUFFIX="s"
[[ $SMOOTH_ENKF = "NO" ]] && ENKF_SUFFIX=""

################################################################################
# Preprocessing
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 99

################################################################################
# Fixed files
$NLN $SATANGL    satbias_angle
$NLN $SATINFO    satinfo
$NLN $SCANINFO   scaninfo
$NLN $CONVINFO   convinfo
$NLN $OZINFO     ozinfo
$NLN $HYBENSINFO hybens_info
$NLN $ANAVINFO   anavinfo
$NLN $VLOCALEIG  vlocal_eig.dat

################################################################################
# Ensemble guess, observational data and analyses/increments

nfhrs=`echo $IAUFHRS_ENKF | sed 's/,/ /g'`
for imem in $(seq 1 $NMEM_ENKF); do
   memchar="mem"$(printf %03i $imem)
   mkdir ${memchar}
   $NLN $COMOUT_ANL_ENSFSOI/$memchar/${APREFIX}atmf024.nc ${memchar}
done

$NLN $COMIN_GES_ENS/${GPREFIX}atmf006.ensmean${GSUFFIX} sfg_${CDATE}_fhr06_ensmean
$NLN $COMIN_GES_ENS/${GPREFIX}atmf006.ensmean${GSUFFIX} sfg_${CDATE}_fhr03_ensmean

# The following deals with different files with the same local name (assuming
# a 24hr EFSOI forecast):
# both are hybrid analyses from gdas - one from CDATE saved during the
# corresponding GDAS cycle in the efsoigdas tree to be used in 
# the localization advection in EFSOI, the other from VDATE to be used
# for verification.

# saved analysis to be used for localization advection
$NLN $COMOUT_ANL_ENSFSOI/${INITANL} ${APREFIX}atmanl.ensmean.nc

$NLN $ATMGES_ENSMEAN .

# forecasts
$NLN $COMIN_GES_ENS/$FCSTLONG .
$NLN $COMOUT_ANL_ENSFSOI/$FCSTSHORT .

# inital osense file
# efsoi.x will read then clobber this
$NCP $COMOUT_ANL_ENSFSOI/$OSENSEIN osense_${CDATE}.dat




if [ $USE_CFP = "YES" ]; then
   chmod 755 $DATA/mp_untar.sh
   ncmd=$(cat $DATA/mp_untar.sh | wc -l)
   if [ $ncmd -gt 0 ]; then
      ncmd_max=$((ncmd < npe_node_max ? ncmd : npe_node_max))
      APRUNCFP=$(eval echo $APRUNCFP)
      $APRUNCFP $DATA/mp_untar.sh
      export ERR=$?
      export err=$ERR
      $ERRSCRIPT || exit 3
   fi
fi

################################################################################
# Create global_enkf namelist
# This is trimmed down to the entries relevant to the EFSOI code
# AFE changed from original:
# gdatehr, datehr, andataname added
# analpertwt and lnsigcutoff changed upstream
# numiter from 0 to 1
#   fso_cycling=.true.,
#   efsoi_flag=.true.,
#   wmoist=1.0,adrate=0.75



cat > enkf.nml << EOFnml
&nam_enkf
   datestring="$CDATE",datapath="$DATA/",
   gdatehr=$gcyc,
   datehr=$cyc,
   fgfileprefixes=$fgfileprefixes
   andataname="$VERFANL",
   corrlengthnh=${corrlength},corrlengthsh=${corrlength},corrlengthtr=${corrlength},
   lnsigcutoffnh=${lnsigcutoff},lnsigcutoffsh=${lnsigcutoff},lnsigcutofftr=${lnsigcutoff},
   lnsigcutoffpsnh=${lnsigcutoff},lnsigcutoffpssh=${lnsigcutoff},lnsigcutoffpstr=${lnsigcutoff},
   lnsigcutoffsatnh=${lnsigcutoff},lnsigcutoffsatsh=${lnsigcutoff},lnsigcutoffsattr=${lnsigcutoff},
   obtimelnh=1.e30,obtimelsh=1.e30,obtimeltr=1.e30,
   nlons=$LONA_ENKF,nlats=$LATA_ENKF,nlevs=$LEVS_ENKF,nanals=$NMEM_ENKF,
   reducedgrid=${reducedgrid},
   use_gfs_nemsio=${use_gfs_nemsio},use_gfs_ncio=${use_gfs_ncio},
   univaroz=.false.,
   adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,
   letkf_flag=${letkf_flag},nobsl_max=${nobsl_max},denkf=${denkf},getkf=${getkf}.,
   lobsdiag_forenkf=$lobsdiag_forenkf,
   write_spread_diag=$write_spread_diag,
   modelspace_vloc=$modelspace_vloc,
   efsoi_cycling=.true.,
   efsoi_flag=.true.,
   wmoist=1.0,
   adrate=0.75
/
&satobs_enkf
/
&ozobs_enkf
/
EOFnml

################################################################################
# Run enkf update

export OMP_NUM_THREADS=$NTHREADS_ENKF
export pgm=$EFSOIEXEC
. prep_step

$NCP $EFSOIEXEC $DATA
$APRUN_ENKF ${DATA}/$(basename $EFSOIEXEC) 1>stdout 2>stderr
rc=$?

export ERR=$rc
export err=$ERR
$ERRSCRIPT || exit 2

# Cat runtime output files.
cat stdout stderr > $COMOUT_ANL_ENSFSOI/$EFSOISTAT

$NCP osense_${CDATE}.dat $COMOUT_ANL_ENSFSOI/$OSENSEOUT
$NCP osense_${CDATE}.dat $OSENSE_SAVE_DIR/$OSENSEOUT

################################################################################
#  Postprocessing

cd $pwd
[[ $mkdata = "YES" ]] && rm -rf $DATA
set +x
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
