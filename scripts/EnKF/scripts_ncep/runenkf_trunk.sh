#!/bin/sh
## Below are LoadLeveler (IBM queueing system) commands
#@ job_name=enkf_update
#@ error=enkf_update.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node_usage = not_shared
#@ tasks_per_node = 32
#@ node = 8
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ parallel_threads = 2
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:40:00
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
export XLSMPOPTS="parthds=2:stack=128000000"

# Environment variables from Carolyn
export LAPI_DEBUG_ENABLE_AFFINITY=YES
##export MP_FIFO_MTU=4K
##export MP_SYNC_QP=YES
##export MP_RFIFO_SIZE=16777216
##export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
##export MP_EUIDEVELOP=min

#RDMA specific tunables:
##export MP_USE_BULK_XFER=yes
##export MP_BULK_MIN_MSG_SIZE=64k
##export MP_RC_MAX_QP=8192
##export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
##export LAPI_DEBUG_QP_NOTIFICATION=no
##export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
adate=2010101312
exp=test46trunk

# Set path/file for enkf executable
basedir=/global/save
enkfpath=$basedir/wx23jd/trunk/src
enkfexec=$enkfpath/global_enkf_gfs

# directories for case
dirges=/gpfs/t2c/global/noscrub/wx20kd/CASES/$adate/ensges
dirobs=/gpfs/t2c/global/noscrub/wx20kd/CASES/$adate/ensobs

# fixed files
FIXGSI=/global/save/wx20kd/ensda/ersl/fixgsi
CONVINFO=${FIXGSI}/global_convinfo.txt
SATINFO=${FIXGSI}/global_satinfo.txt.n19_off.r2008
OZINFO=${FIXGSI}/global_ozinfo.txt.r2008

# temporary directory
tmpdir=/ptmp/wx23jd/enkf/${exp}/

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

ncp=/bin/cp

# Set up paramters, options and namelist here
# nanals = # of ensemble members
nanals=80
JCAP=254
LEVS=64
LONA=512
LATA=256
SMOOTHINF=24
npts=`expr \( $LONA \) \* \( $LATA \)`
LSOIL=4
# Number of tracers & dynamic variables
NTRAC=3
nvars=3

cat << EOF > enkf.nml
 &nam_enkf
  datestring="$adate",datapath="$tmpdir",
  analpertwtnh=0.9,analpertwtsh=0.9,analpertwttr=0.9,
  ntrac_update=2,
  covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.true.,
  corrlengthnh=1500,corrlengthsh=1500,corrlengthtr=1500,
  obtimelnh=15,obtimelsh=15,obtimeltr=15,iassim_order=0,
  lnsigcutoffnh=1.5,lnsigcutoffsh=1.5,lnsigcutofftr=1.5,
  lnsigcutoffsatnh=3.3,lnsigcutoffsatsh=3.3,lnsigcutoffsattr=3.3,
  lnsigcutoffpsnh=2.2,lnsigcutoffpssh=2.2,lnsigcutoffpstr=2.2,
  saterrfact=1.0,numiter=3,
  sprd_tol=1.e30,paoverpb_thresh=0.975,
  npts=$npts,nlevs=$LEVS,nanals=$nanals,ntrac=$NTRAC,nvars=$nvars,
  deterministic=.true.,sortinc=.true.,lupd_satbiasc=.false.,
  nlats=$LATA,nlons=$LONA,
 /
 &END
 &satobs_enkf
  sattypes_rad(1) = 'amsua_n15',     dsis(1) = 'amsua_n15',
  sattypes_rad(2) = 'amsua_n18',     dsis(2) = 'amsua_n18',
  sattypes_rad(3) = 'amsua_n19',     dsis(3) = 'amsua_n19',
  sattypes_rad(4) = 'amsub_n16',     dsis(4) = 'amsub_n16',
  sattypes_rad(5) = 'amsub_n17',     dsis(5) = 'amsub_n17',
  sattypes_rad(6) = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
  sattypes_rad(7) = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
  sattypes_rad(8) = 'airs_aqua',     dsis(8) = 'airs281SUBSET_aqua',
  sattypes_rad(9) = 'hirs3_n17',     dsis(9) = 'hirs3_n17',
  sattypes_rad(10)= 'hirs4_n19',     dsis(10)= 'hirs4_n19',
  sattypes_rad(11)= 'hirs4_metop-a', dsis(11)= 'hirs4_metop-a',
  sattypes_rad(12)= 'mhs_n18',       dsis(12)= 'mhs_n18',
  sattypes_rad(13)= 'mhs_n19',       dsis(13)= 'mhs_n19',
  sattypes_rad(14)= 'mhs_metop-a',   dsis(14)= 'mhs_metop-a',
  sattypes_rad(15)= 'goes_img_g11',  dsis(15)= 'imgr_g11',
  sattypes_rad(16)= 'goes_img_g12',  dsis(16)= 'imgr_g12',
  sattypes_rad(17)= 'goes_img_g13',  dsis(17)= 'imgr_g13',
  sattypes_rad(18)= 'goes_img_g14',  dsis(18)= 'imgr_g14',
  sattypes_rad(19)= 'goes_img_g15',  dsis(19)= 'imgr_g15',
  sattypes_rad(20)= 'avhrr3_n16',    dsis(20)= 'avhrr3_n16',
  sattypes_rad(21)= 'avhrr3_n17',    dsis(21)= 'avhrr3_n17',
  sattypes_rad(22)= 'avhrr3_n18',    dsis(22)= 'avhrr3_n18',
  sattypes_rad(23)= 'amsre_aqua',    dsis(23)= 'amsre_aqua',
  sattypes_rad(24)= 'ssmis_f16',     dsis(24)= 'ssmis_f16',
  sattypes_rad(25)= 'ssmis_f17',     dsis(25)= 'ssmis_f17',
  sattypes_rad(26)= 'ssmis_f18',     dsis(26)= 'ssmis_f18',
  sattypes_rad(27)= 'ssmis_f19',     dsis(27)= 'ssmis_f19',
  sattypes_rad(28)= 'ssmis_f20',     dsis(28)= 'ssmis_f20',
  sattypes_rad(29)= 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
  sattypes_rad(30)= 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
  sattypes_rad(31)= 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
  sattypes_rad(32)= 'sndrd4_g11',    dsis(32)= 'sndrD4_g12',
  sattypes_rad(33)= 'sndrd1_g12',    dsis(33)= 'sndrD1_g12',
  sattypes_rad(34)= 'sndrd2_g12',    dsis(34)= 'sndrD2_g12',
  sattypes_rad(35)= 'sndrd3_g12',    dsis(35)= 'sndrD3_g12',
  sattypes_rad(36)= 'sndrd4_g12',    dsis(36)= 'sndrD4_g12',
  sattypes_rad(37)= 'sndrd1_g13',    dsis(37)= 'sndrD1_g13',
  sattypes_rad(38)= 'sndrd2_g13',    dsis(38)= 'sndrD2_g13',
  sattypes_rad(39)= 'sndrd3_g13',    dsis(39)= 'sndrD3_g13',
  sattypes_rad(40)= 'sndrd4_g13',    dsis(40)= 'sndrD4_g13',
  sattypes_rad(41)= 'sndrd1_g14',    dsis(41)= 'sndrD1_g14',
  sattypes_rad(42)= 'sndrd2_g14',    dsis(42)= 'sndrD2_g14',
  sattypes_rad(43)= 'sndrd3_g14',    dsis(43)= 'sndrD3_g14',
  sattypes_rad(44)= 'sndrd4_g14',    dsis(44)= 'sndrD4_g14',
  sattypes_rad(45)= 'sndrd1_g15',    dsis(45)= 'sndrD1_g15',
  sattypes_rad(46)= 'sndrd2_g15',    dsis(46)= 'sndrD2_g15',
  sattypes_rad(47)= 'sndrd3_g15',    dsis(47)= 'sndrD3_g15',
  sattypes_rad(48)= 'sndrd4_g15',    dsis(48)= 'sndrD4_g15',
  sattypes_rad(49)= 'iasi_metop-a',  dsis(49)= 'iasi616_metop-a',
 /
 &END
 &ozobs_enkf
  sattypes_oz(1) = 'sbuv2_n16',     
  sattypes_oz(2) = 'sbuv2_n17',    
  sattypes_oz(3) = 'sbuv2_n18',   
  sattypes_oz(4) = 'sbuv2_n19',
  sattypes_oz(5) = 'omi_aura',  
  sattypes_oz(6) = 'gome_metop-a',  
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

##/global/save/wx23jd/enkf/work/src/getsigensmean.x $tmpdir/ sanl_2008080112_ensmean sanl_2008080112 60 anal

##rm $tmpdir/diag*
##rm $tmpdir/sfg*

exit


