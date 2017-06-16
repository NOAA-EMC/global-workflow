#!/bin/ksh

#
# Script history log:
# 1999-05-01  Mark Iredell
# 2000-02-14  S    Moorthi
# 2001-12-14  J    Alpert  (*j*)
# 2004-05-12  J    Alpert  (*j*) fix for E-W gaussian grid pt shift
# 2004-12-06  J    Alpert  (*j*)  script input settings for spect filter
# 2005-03-21  J    Alpert  (*j*)  Added GrumbineICE to orog/slm...
# 2011-12-xx  S    Moorthi Added unfiltered orography and linear grids
# 2014-05-20  F    YANG    reorganized to include in GFS util directory   
#
# W/Lott & Miller terrain principal coord. (*j*)
#
#Usage: ml7_slm30g.sh slmgb orogb mtnvar14 nlon nlat jcap filter1 filter2 mtnres
# Normally: filter1~1/3 ((jcap/3)-1))
# Normally: filter2~jcap+2))
# Normally: mtnres=8 minute only (do not use =4, =2 except at own risk)
#           now =1 is 30" others are turned off.  see below
# New run mtnlm7 for mtnres=1 set for 30"
#   script changed like ml4b for spect filter input, otherwise same as ml2b
#   Input script fortran  positional parameters:
#     1             output sea-land mask GRIB file
#     2             output orography GRIB file
#     3             output 14-field mountain variance file
#     4             number of Gaussian longitudes
#     5             number of Gaussian latitudes
#     6             spectral triangular truncation
#     7             Envelope orography factor
#     8             Begining latitude (used only for nongaussian grid -
#                                      used only for switching north/south)
#     9             Mountain data resolution
#
#   Imported Shell Variables:
#     WRKDIR        working directory
#                   defaults to a directory that is made, used and deleted
#     FIXDIR        fix directory
#                   defaults to /gloptmp/fix
#     TERRAINSORC   terrain source file
#                   defaults  
#                   now this defaults to the local dir
#     LONSPERLAT    input lonsperlat text file (if it exists)
#                   defaults to $FIXDIR/global_lonsperlat.t$6.txt
#     VERBOSE       verbose flag (YES or NO)
#                   defaults to NO
#
#   Modules and files referenced:
#     scripts    : /global/save/wx23ja/bin/mkwrkdir
#
#     source     : ${TERRAINSORC} or 
#                  ops(20060822)w/GICE 
#
#     input data : /ptmp/wx23ja/terr05/markr/gtopo30_gg.fine output array
#                  /global/noscrub/wx23ja/terr05/markr/gtopo30_gg.fine
#                    about 2GB fort.235
#                  /gloptmp/fix/global_lonsperlat.t$6.txt
#
#     output data: $1
#                  $2
#                  $3
#
#     scratch    : ${WRKDIR}/terrain00.xd
#                  ${WRKDIR}/fort.11
#                  ${WRKDIR}/fort.12
#                  ${WRKDIR}/fort.13
#                  ${WRKDIR}/fort.14
#                  ${WRKDIR}/fort.20
#                  ${WRKDIR}/fort.51
#                  ${WRKDIR}/fort.52
#                  ${WRKDIR}/fort.53
#                  ${WRKDIR}/fort.54
#                  ${WRKDIR}/fort.55
#                  ${WRKDIR}/fort.56
#                  ${WRKDIR}/fort.57
#                  ${WRKDIR}/fort.71
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####

################################################################################
# Check arguments
if [[ $# -ne 13 ]] ; then
 echo Usage: $0 slmgb orogb mtnvar14 IM JM NM filter1 filter2 MTNRES orogb_uf oro_bin oro_bin oro_bin_uf slm_bin >&2
 exit 1
fi
#
# VERBOSE = YES means debug mode
#
# export VERBOSE=${VERBOSE:-"NO"}
export VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" = "YES" ]];then
 echo $(date) EXECUTING $0 $* >&2
 set -x
fi
pwd=$(pwd)
echo $pwd
typeset -L1 l1
slmgb=$1
l1=$slmgb    ; [[ $l1 = / || $l1 = ~ ]] || slmgb=$pwd/$slmgb
orogb=$2
l1=$orogb    ; [[ $l1 = / || $l1 = ~ ]] || orogb=$pwd/$orogb
mtnvar14=$3
l1=$mtnvar14 ; [[ $l1 = / || $l1 = ~ ]] || mtnvar14=$pwd/$mtnvar14
nlon=$4
nlat=$5
jcap=$6
#### efac=$7
#### blat=$8
efac=0
blat=0
#### export NF1=${NF1:-$(($jcap+1))}
#### export NF2=${NF2:-$(($jcap+2))}
export NF1=${7:-$(($jcap+1))}
export NF2=${8:-$(($jcap+2))}
export mtnres=${9:-"8"}
orogb_uf=${10:-""}
oro_bin=${11:-""}
oro_bin_uf=${12:-""}
slm_bin=${13:-""}
l1=$orogb_uf   ; [[ $l1 = / || $l1 = ~ ]] || orogb_uf=$pwd/$orogb_uf
l1=$oro_bin    ; [[ $l1 = / || $l1 = ~ ]] || oro_bin=$pwd/$oro_bin
l1=$oro_bin_uf ; [[ $l1 = / || $l1 = ~ ]] || oro_bin_uf=$pwd/$oro_bin_uf
l1=$slm_bin    ; [[ $l1 = / || $l1 = ~ ]] || slm_bin=$pwd/$slm_bin
NR=0

echo "Usage: $0 $slmgb $orogb $mtnvar14 $nlon $nlat $jcap $NF1 $NF2  $MTNRES $orogb_uf"
echo "   efac=$efac  blat=$blat  NF1=$NF1   NF2=$NF2 "
echo " _______________________________________________  "

#
#  file names for Prin Coord dataset grib output
#
thetagb=thetagb
l1=$thetagb ; [[ $l1 = / || $l1 = ~ ]] || thetagb=$pwd/$thetagb
gammagb=gammagb
l1=$gammagb ; [[ $l1 = / || $l1 = ~ ]] || gammagb=$pwd/$gammagb
sigmagb=sigmagb
l1=$sigmagb ; [[ $l1 = / || $l1 = ~ ]] || sigmagb=$pwd/$sigmagb
vargb=vargb
l1=$vargb   ; [[ $l1 = / || $l1 = ~ ]] || vargb=$pwd/$vargb
elvmaxgb=elvmaxgb
l1=$elvmaxgb;[[ $l1  = / || $l1 = ~ ]] || elvmaxgb=$pwd/$elvmaxgb

#
export WRKDIR=${WRKDIR:-${PTMP:-/ptmpp2}/$LOGNAME/terr_wrkdir$$}

export lin=${lin:-""}
export BASEDIR=${UTILDIR:-/global/save/emc.glopara/svn/gfs/trunk/para/util}
export NWPROD=${NWPROD:-/nwprod}
export FIXDIR=${FIXDIR:-$NWPROD/fix}
export LONSPERLAT=${LONSPERLAT:-$FIXDIR/global_lonsperlat.t${jcap}$lin.txt}
export TERRAINEXEC=${TERRAINEXEC:-$BASEDIR/exec/terrain.x}
################################################################################

if [ ! -d $WRKDIR ] ; then
  export MKWRKDIR=YES
  mkdir -p $WRKDIR
fi
cd $WRKDIR

FIX_TERR=${FIX_TERR:-$BASEDIR/fix}
MTNDIR=${MTNDIR:-$FIX_TERR}
MTN_SLM=${MTN_SLM:-TOP8M_slm.80I1.asc}
HIRES_TERR=${HIRES_TERR:-$FIX_TERR/thirty.second.antarctic.new.bin}
FINE_TERR=${FINE_TERR:-$FIX_TERR/gtopo30_gg.fine}
LANDCOVER30=${LANDCOVER30:-$FIX_TERR/landcover30.fixed}
slm_bin=${slm_bin:-SLM.T$jcap}
oro_bin=${oro_bin:-ORO.T$jcap}
oro_bin_uf=${oro_bin_uf:-ORU.T$jcap}

ln -fs $MTNDIR/$MTN_SLM       fort.14
ln -fs $HIRES_TERR            fort.15
ln -fs $LONSPERLAT            fort.20
ln -fs $slm_bin               fort.51
ln -fs $oro_bin               fort.52
ln -sf $mtnvar14              fort.53
ln -fs ORS.T$jcap             fort.54
ln -fs $oro_bin_uf            fort.55
ln -sf $slmgb                 fort.56
ln -sf $orogb                 fort.57
ln -sf $thetagb               fort.58
ln -sf $gammagb               fort.59
ln -sf $sigmagb               fort.60
ln -sf $vargb                 fort.61
ln -sf $elvmaxgb              fort.62
ln -sf THETA.T$jcap           fort.66
ln -sf GAMMA.T$jcap           fort.67
ln -sf SIGMA.T$jcap           fort.68
ln -sf mtn.T$jcap.ieee        fort.71
ln -fs $orogb_uf              fort.72
ln -fs $FINE_TERR             fort.235
ln -fs $LANDCOVER30           landcover30.fixed
ln -fs $a_ocean_mask          fort.25

#export OMP_NUM_THREADS=1
export MP_COREFILE_FORMAT=lite

echo " mtnres nlon nlat jcap NR NF1 NF2 efac blat"
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat 
echo " exec located:  $x "
echo " EXECUTION BEGINS "
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat | $TERRAINEXEC

ret=$?
if [[ "$VERBOSE" = "YES" ]] ; then 
  echo ret=$ret
fi

# copy files from working dir to present
#cp -p $WRKDIR/m* $local_dir/.

# this will get the mtnvar_14 and mtn.ieee file for grads use
# the ...gb files are present directly - from starting local dir.
# the other files working files are left to be copied by the user.
# Remove working directory

if [[ "$VERBOSE" = "YES" ]] ; then
     echo $pwd
     ls -l
     echo "  ml7_slm30g.sh: setting MKWRKDIR = NO  " 
     echo " - not deleting working dir $WRKDIR "
     MKWRKDIR=NO
fi
# 
cd $pwd
[[ $MKWRKDIR = YES ]] && rm -rf $WRKDIR
set +x
if [[ "$VERBOSE" = "YES" ]];then
 echo " $(date) EXITING $0 with return code $ret >&2 "
fi
exit $ret
