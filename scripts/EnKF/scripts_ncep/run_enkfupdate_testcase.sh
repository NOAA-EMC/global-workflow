#!/bin/sh
## Below are LoadLeveler (IBM queueing system) commands
#@ job_name=enkf_update
#@ error=enkf_update.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node_usage = not_shared
#@ tasks_per_node = 32
#@ node = 10
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ parallel_threads = 2
#@ class= devhigh  
#@ group= devonprod
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 1:30:00
#@ startdate = 07/06/09 10:15
#@ notification=error
#@ queue

set -x


## NOT SURE WHICH OF THESE TO KEEP ?!?!?!

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes
# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Environment variables from Carolyn
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_RFIFO_SIZE=16777216
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
#RDMA specific tunables:
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
adate=2008080112
exp=test

# Set path/file for enkf executable
basedir=/global/save
enkfpath=$basedir/wx20kd/enkf/work/src
enkfexec=$enkfpath/global_enkf_gfs

# directories for case
dirges=/global/noscrub/wx20kd/CASES/$adate/ensges
dirobs=/global/noscrub/wx20kd/CASES/$adate/obs

# fixed files
FIXGSI=/global/save/wx20kd/ensda/ersl/fixgsi
CONVINFO=${FIXGSI}/global_convinfo.txt
SATINFO=${FIXGSI}/global_satinfo.txt.n19_off.r2008
OZINFO=${FIXGSI}/global_ozinfo.txt.r2008

# temporary directory
tmpdir=/ptmp/wx20kd/enkf/${exp}/

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

ncp=/bin/cp

# Set up paramters, options and namelist here
# nanals = # of ensemble members
nanals=60
JCAP=190
LEVS=64
LONA=576
LATA=288
SMOOTHINF=24
npts=`expr \( $LONA \) \* \( $LATA \)`
LSOIL=4
# Number of tracers & dynamic variables
NTRAC=3
nvars=3

cat << EOF > enkf.nml
 &nam_enkf
  datestring="$adate",datapath="$tmpdir",random_partition=.false.,
  analpertwtnh=0.9,analpertwtsh=0.9,analpertwttr=0.9,
  simple_partition=.false.,ntrac_update=2,
  covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.true.,
  corrlengthnh=1500,corrlengthsh=1500,corrlengthtr=1500,
  obtimelnh=15,obtimelsh=15,obtimeltr=15,iassim_order=0,
  lnsigcutoffnh=1.5,lnsigcutoffsh=1.5,lnsigcutofftr=1.5,
  lnsigcutoffsatnh=3.3,lnsigcutoffsatsh=3.3,lnsigcutoffsattr=3.3,
  lnsigcutoffpsnh=2.2,lnsigcutoffpssh=2.2,lnsigcutoffpstr=2.2,
  use_height=.false.,saterrfact=1.0,numiter=3,
  sprd_tol=1.e30,paoverpb_thresh=1.0,
  npts=$npts,nlevs=$LEVS,nanals=$nanals,ntrac=$NTRAC,nvars=$nvars,
  deterministic=.true.,sortinc=.true.,
 /
 &END
EOF

cat enkf.nml

$ncp $enkfexec        ./enkf.x
$ncp $CONVINFO        ./convinfo
$ncp $SATINFO         ./satinfo
$ncp $OZINFO          ./ozinfo
$ncp $dirobs/abias    ./abias
$ncp $dirobs/satang   ./satang

ln -fs ./abias        ./satbias_in
ln -fs ./satang       ./satbias_angle
ln -fs $dirges/sfg*   ./
ln -fs $dirobs/diag*  ./

poe hpmcount $tmpdir/enkf.x < enkf.nml > stdout

rm $tmpdir/diag*
rm $tmpdir/sfg*

exit


