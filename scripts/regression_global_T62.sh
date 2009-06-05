#!/bin/sh

. regression_var.sh

#@ job_name=regression_test
#@ step_name=gsi_global_update
#@ error=gsi_global_update.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ queue

#@ step_name=gsi_global_update2
#@ error=gsi_global_update2.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_update==0)
#@ queue

#@ step_name=gsi_global_benchmark
#@ error=gsi_global_benchmark.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_update2==0)
#@ queue

#@ step_name=gsi_global_benchmark2
#@ error=gsi_global_benchmark2.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:15:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_benchmark==0)
#@ queue

#@ step_name=global_regression
#@ error=global_regression.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class= dev
#@ group= dev
#@ wall_clock_limit = 00:10:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ dependency=(gsi_global_benchmark2==0)
#@ queue

case $LOADL_STEP_NAME in
  gsi_global_update)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
adate=$adate_global
exp=$exp1_global_sub_1node
##exp=gmao_gsi7.t62.subversion.1node

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""

GRIDOPTS="JCAP_B=$JCAP_B"

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=500,niter_no_qc(2)=500,
   write_diag(1)=.false.,write_diag(2)=.false.,write_diag(3)=.false.,
   diag_conv=.false.,diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   gencode=82,qoption=2,
   factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=62,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   tsfc_sdv(1)=3.0,tsfc_sdv(2)=3.0,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,dmesh(6)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',
        dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',
        dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',
        dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',
        dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',
        dval(05)=1.0,  dthin(05)=0,
   dfile(06)='radarbufr', dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',
        dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',
        dval(07)=1.0,  dthin(07)=0,
   dfile(08)='prepbufr',  dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',
        dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',
        dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',
        dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',
        dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',
        dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',
        dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',
        dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',
        dval(15)=1.0,  dthin(15)=0,
   dfile(16)='omi',       dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',
        dval(16)=1.0,  dthin(16)=6,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',
        dval(17)=6.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',
        dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',
        dval(19)=6.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',
        dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=6.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',
        dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',
        dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',
        dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',
        dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=20.0, dthin(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',
        dval(27)=2.0,  dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',
        dval(28)=10.0, dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',
        dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',
        dval(30)=0.0,  dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',
        dval(31)=10.0, dthin(31)=2,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=10.0, dthin(32)=2,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',
        dval(33)=5.0,  dthin(33)=2,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',
        dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',
        dval(35)=3.0,  dthin(35)=3,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',
        dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',
        dval(37)=3.0,  dthin(37)=3,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',
        dval(38)=3.0,  dthin(38)=3,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',
        dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',
        dval(40)=0.0,  dthin(40)=4,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',
        dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',
        dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',
        dval(43)=0.0,  dthin(43)=4,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',
        dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_las', dplat(45)='f16',       dsis(45)='ssmis_f16',
        dval(45)=0.0,  dthin(45)=4,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_uas', dplat(46)='f16',       dsis(46)='ssmis_f16',
        dval(46)=0.0,  dthin(46)=4,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_img', dplat(47)='f16',       dsis(47)='ssmis_f16',
        dval(47)=0.0,  dthin(47)=4,
   dfile(48)='ssmisbufr', dtype(48)='ssmis_env', dplat(48)='f16',       dsis(48)='ssmis_f16',
        dval(48)=0.0,  dthin(48)=4,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd1',    dplat(49)='g12',       dsis(49)='sndrD1_g12',
        dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd2',    dplat(50)='g12',       dsis(50)='sndrD2_g12',
        dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd3',    dplat(51)='g12',       dsis(51)='sndrD3_g12',
        dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
  dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd1',    dplat(53)='g11',       dsis(53)='sndrD1_g11',
        dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd2',    dplat(54)='g11',       dsis(54)='sndrD2_g11',
        dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd3',    dplat(55)='g11',       dsis(55)='sndrD3_g11',
        dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd4',    dplat(56)='g11',       dsis(56)='sndrD4_g11',
        dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd1',    dplat(57)='g13',       dsis(57)='sndrD1_g13',
        dval(57)=1.5,  dthin(57)=5,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd2',    dplat(58)='g13',       dsis(58)='sndrD2_g13',
        dval(58)=1.5,  dthin(58)=5,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd3',    dplat(59)='g13',       dsis(59)='sndrD3_g13',
        dval(59)=1.5,  dthin(59)=5,
   dfile(60)='gsnd1bufr', dtype(60)='sndrd4',    dplat(60)='g13',       dsis(60)='sndrD4_g13',
        dval(60)=1.5,  dthin(60)=5,
   dfile(61)='iasibufr',  dtype(61)='iasi',      dplat(61)='metop-a',   dsis(61)='iasi616_metop-a',     dval(61)=20.0, dthin(61)=1,
   dfile(62)='gomebufr',  dtype(62)='gome',      dplat(62)='metop-a',   dsis(62)='gome_metop-a',        dval(62)=1.0,  dthin(62)=6,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /

EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  gsi_global_update2)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
adate=$adate_global
exp=$exp2_global_sub_2node
##exp=gmao_gsi7.t62.subversion.2node

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""

GRIDOPTS="JCAP_B=$JCAP_B"

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=500,niter_no_qc(2)=500,
   write_diag(1)=.false.,write_diag(2)=.false.,write_diag(3)=.false.,
   diag_conv=.false.,diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   gencode=82,qoption=2,
   factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=62,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   tsfc_sdv(1)=3.0,tsfc_sdv(2)=3.0,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,dmesh(6)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',
        dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',
        dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',
        dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',
        dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',
        dval(05)=1.0,  dthin(05)=0,
   dfile(06)='radarbufr', dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',
        dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',
        dval(07)=1.0,  dthin(07)=0,
   dfile(08)='prepbufr',  dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',
        dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',
        dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',
        dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',
        dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',
        dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',
        dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',
        dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',
        dval(15)=1.0,  dthin(15)=0,
   dfile(16)='omi',       dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',
        dval(16)=1.0,  dthin(16)=6,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',
        dval(17)=6.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',
        dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',
        dval(19)=6.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',
        dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=6.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',
        dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',
        dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',
        dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',
        dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=20.0, dthin(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',
        dval(27)=2.0,  dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',
        dval(28)=10.0, dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',
        dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',
        dval(30)=0.0,  dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',
        dval(31)=10.0, dthin(31)=2,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=10.0, dthin(32)=2,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',
        dval(33)=5.0,  dthin(33)=2,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',
        dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',
        dval(35)=3.0,  dthin(35)=3,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',
        dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',
        dval(37)=3.0,  dthin(37)=3,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',
        dval(38)=3.0,  dthin(38)=3,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',
        dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',
        dval(40)=0.0,  dthin(40)=4,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',
        dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',
        dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',
        dval(43)=0.0,  dthin(43)=4,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',
        dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_las', dplat(45)='f16',       dsis(45)='ssmis_f16',
        dval(45)=0.0,  dthin(45)=4,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_uas', dplat(46)='f16',       dsis(46)='ssmis_f16',
        dval(46)=0.0,  dthin(46)=4,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_img', dplat(47)='f16',       dsis(47)='ssmis_f16',
        dval(47)=0.0,  dthin(47)=4,
   dfile(48)='ssmisbufr', dtype(48)='ssmis_env', dplat(48)='f16',       dsis(48)='ssmis_f16',
        dval(48)=0.0,  dthin(48)=4,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd1',    dplat(49)='g12',       dsis(49)='sndrD1_g12',
        dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd2',    dplat(50)='g12',       dsis(50)='sndrD2_g12',
        dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd3',    dplat(51)='g12',       dsis(51)='sndrD3_g12',
        dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
  dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd1',    dplat(53)='g11',       dsis(53)='sndrD1_g11',
        dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd2',    dplat(54)='g11',       dsis(54)='sndrD2_g11',
        dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd3',    dplat(55)='g11',       dsis(55)='sndrD3_g11',
        dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd4',    dplat(56)='g11',       dsis(56)='sndrD4_g11',
        dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd1',    dplat(57)='g13',       dsis(57)='sndrD1_g13',
        dval(57)=1.5,  dthin(57)=5,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd2',    dplat(58)='g13',       dsis(58)='sndrD2_g13',
        dval(58)=1.5,  dthin(58)=5,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd3',    dplat(59)='g13',       dsis(59)='sndrD3_g13',
        dval(59)=1.5,  dthin(59)=5,
   dfile(60)='gsnd1bufr', dtype(60)='sndrd4',    dplat(60)='g13',       dsis(60)='sndrD4_g13',
        dval(60)=1.5,  dthin(60)=5,
   dfile(61)='iasibufr',  dtype(61)='iasi',      dplat(61)='metop-a',   dsis(61)='iasi616_metop-a',     dval(61)=20.0, dthin(61)=1,
   dfile(62)='gomebufr',  dtype(62)='gome',      dplat(62)='metop-a',   dsis(62)='gome_metop-a',        dval(62)=1.0,  dthin(62)=6,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /

EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  gsi_global_benchmark)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
adate=$adate_global
exp=$exp1_global_bench_1node
##exp=q1fy10.t62.2node.orig_mpi_io

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$benchmark
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/versions/global_gsi.fd.old_mpio.May07_0819/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/global_gsi.fd/global_gsi
##gsiexec=/global/save/wx20ml/q1fy10_new/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""

GRIDOPTS="JCAP_B=$JCAP_B"

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   write_diag(1)=.false.,write_diag(2)=.false.,write_diag(3)=.false.,
   diag_conv=.false.,diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   gencode=82,qoption=2,
   factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=62,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   tsfc_sdv(1)=3.0,tsfc_sdv(2)=3.0,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,dmesh(6)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',
        dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',
        dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',
        dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',
        dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',
        dval(05)=1.0,  dthin(05)=0,
   dfile(06)='radarbufr', dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',
        dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',
        dval(07)=1.0,  dthin(07)=0,
   dfile(08)='prepbufr',  dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',
        dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',
        dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',
        dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',
        dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',
        dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',
        dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',
        dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',
        dval(15)=1.0,  dthin(15)=0,
   dfile(16)='omi',       dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',
        dval(16)=1.0,  dthin(16)=6,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',
        dval(17)=6.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',
        dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',
        dval(19)=6.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',
        dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=6.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',
        dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',
        dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',
        dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',
        dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=20.0, dthin(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',
        dval(27)=2.0,  dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',
        dval(28)=10.0, dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',
        dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',
        dval(30)=0.0,  dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',
        dval(31)=10.0, dthin(31)=2,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=10.0, dthin(32)=2,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',
        dval(33)=5.0,  dthin(33)=2,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',
        dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',
        dval(35)=3.0,  dthin(35)=3,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',
        dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',
        dval(37)=3.0,  dthin(37)=3,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',
        dval(38)=3.0,  dthin(38)=3,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',
        dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',
        dval(40)=0.0,  dthin(40)=4,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',
        dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',
        dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',
        dval(43)=0.0,  dthin(43)=4,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',
        dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_las', dplat(45)='f16',       dsis(45)='ssmis_f16',
        dval(45)=0.0,  dthin(45)=4,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_uas', dplat(46)='f16',       dsis(46)='ssmis_f16',
        dval(46)=0.0,  dthin(46)=4,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_img', dplat(47)='f16',       dsis(47)='ssmis_f16',
        dval(47)=0.0,  dthin(47)=4,
   dfile(48)='ssmisbufr', dtype(48)='ssmis_env', dplat(48)='f16',       dsis(48)='ssmis_f16',
        dval(48)=0.0,  dthin(48)=4,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd1',    dplat(49)='g12',       dsis(49)='sndrD1_g12',
        dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd2',    dplat(50)='g12',       dsis(50)='sndrD2_g12',
        dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd3',    dplat(51)='g12',       dsis(51)='sndrD3_g12',
        dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
  dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd1',    dplat(53)='g11',       dsis(53)='sndrD1_g11',
        dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd2',    dplat(54)='g11',       dsis(54)='sndrD2_g11',
        dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd3',    dplat(55)='g11',       dsis(55)='sndrD3_g11',
        dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd4',    dplat(56)='g11',       dsis(56)='sndrD4_g11',
        dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd1',    dplat(57)='g13',       dsis(57)='sndrD1_g13',
        dval(57)=1.5,  dthin(57)=5,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd2',    dplat(58)='g13',       dsis(58)='sndrD2_g13',
        dval(58)=1.5,  dthin(58)=5,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd3',    dplat(59)='g13',       dsis(59)='sndrD3_g13',
        dval(59)=1.5,  dthin(59)=5,
   dfile(60)='gsnd1bufr', dtype(60)='sndrd4',    dplat(60)='g13',       dsis(60)='sndrD4_g13',
        dval(60)=1.5,  dthin(60)=5,
   dfile(61)='iasibufr',  dtype(61)='iasi',      dplat(61)='metop-a',   dsis(61)='iasi616_metop-a',     dval(61)=20.0, dthin(61)=1,
   dfile(62)='gomebufr',  dtype(62)='gome',      dplat(62)='metop-a',   dsis(62)='gome_metop-a',        dval(62)=1.0,  dthin(62)=6,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /

EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  gsi_global_benchmark2)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
adate=$adate_global
exp=$exp2_global_bench_2node
##exp=q1fy10.t62.2node.modified_mpi_io

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$benchmark
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/versions/global_gsi.fd.old_mpio.May07_0819/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/global_gsi.fd/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""

GRIDOPTS="JCAP_B=$JCAP_B"

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   write_diag(1)=.false.,write_diag(2)=.false.,write_diag(3)=.false.,
   diag_conv=.false.,diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,
   gencode=82,qoption=2,
   factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=62,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=0.6,0.6,0.75,0.75,0.75,0.75,1.0,1.0
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   tsfc_sdv(1)=3.0,tsfc_sdv(2)=3.0,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=180.0,dmesh(2)=145.0,dmesh(3)=240.0,dmesh(4)=160.0,dmesh(5)=180.0,dmesh(6)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',
        dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',
        dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',
        dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',
        dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',
        dval(05)=1.0,  dthin(05)=0,
   dfile(06)='radarbufr', dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',
        dval(06)=1.0,  dthin(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',
        dval(07)=1.0,  dthin(07)=0,
   dfile(08)='prepbufr',  dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',
        dval(08)=1.0,  dthin(08)=0,
   dfile(09)='prepbufr',  dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',
        dval(09)=1.0,  dthin(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',
        dval(10)=1.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',
        dval(11)=1.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',
        dval(12)=1.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',
        dval(13)=1.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',
        dval(14)=1.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',
        dval(15)=1.0,  dthin(15)=0,
   dfile(16)='omi',       dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',
        dval(16)=1.0,  dthin(16)=6,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',
        dval(17)=6.0,  dthin(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',
        dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',
        dval(19)=6.0,  dthin(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',
        dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=6.0,  dthin(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',
        dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',
        dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',
        dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',
        dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=20.0, dthin(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',
        dval(27)=2.0,  dthin(27)=2,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',
        dval(28)=10.0, dthin(28)=2,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',
        dval(29)=0.0,  dthin(29)=2,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',
        dval(30)=0.0,  dthin(30)=2,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',
        dval(31)=10.0, dthin(31)=2,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=10.0, dthin(32)=2,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',
        dval(33)=5.0,  dthin(33)=2,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',
        dval(34)=3.0,  dthin(34)=3,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',
        dval(35)=3.0,  dthin(35)=3,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',
        dval(36)=3.0,  dthin(36)=3,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',
        dval(37)=3.0,  dthin(37)=3,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',
        dval(38)=3.0,  dthin(38)=3,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',
        dval(39)=0.0,  dthin(39)=4,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',
        dval(40)=0.0,  dthin(40)=4,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',
        dval(41)=0.0,  dthin(41)=4,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',
        dval(42)=0.0,  dthin(42)=4,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',
        dval(43)=0.0,  dthin(43)=4,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',
        dval(44)=0.0,  dthin(44)=4,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_las', dplat(45)='f16',       dsis(45)='ssmis_f16',
        dval(45)=0.0,  dthin(45)=4,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_uas', dplat(46)='f16',       dsis(46)='ssmis_f16',
        dval(46)=0.0,  dthin(46)=4,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_img', dplat(47)='f16',       dsis(47)='ssmis_f16',
        dval(47)=0.0,  dthin(47)=4,
   dfile(48)='ssmisbufr', dtype(48)='ssmis_env', dplat(48)='f16',       dsis(48)='ssmis_f16',
        dval(48)=0.0,  dthin(48)=4,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd1',    dplat(49)='g12',       dsis(49)='sndrD1_g12',
        dval(49)=1.5,  dthin(49)=5,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd2',    dplat(50)='g12',       dsis(50)='sndrD2_g12',
        dval(50)=1.5,  dthin(50)=5,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd3',    dplat(51)='g12',       dsis(51)='sndrD3_g12',
        dval(51)=1.5,  dthin(51)=5,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
  dfile(52)='gsnd1bufr', dtype(52)='sndrd4',    dplat(52)='g12',       dsis(52)='sndrD4_g12',
        dval(52)=1.5,  dthin(52)=5,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd1',    dplat(53)='g11',       dsis(53)='sndrD1_g11',
        dval(53)=1.5,  dthin(53)=5,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd2',    dplat(54)='g11',       dsis(54)='sndrD2_g11',
        dval(54)=1.5,  dthin(54)=5,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd3',    dplat(55)='g11',       dsis(55)='sndrD3_g11',
        dval(55)=1.5,  dthin(55)=5,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd4',    dplat(56)='g11',       dsis(56)='sndrD4_g11',
        dval(56)=1.5,  dthin(56)=5,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd1',    dplat(57)='g13',       dsis(57)='sndrD1_g13',
        dval(57)=1.5,  dthin(57)=5,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd2',    dplat(58)='g13',       dsis(58)='sndrD2_g13',
        dval(58)=1.5,  dthin(58)=5,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd3',    dplat(59)='g13',       dsis(59)='sndrD3_g13',
        dval(59)=1.5,  dthin(59)=5,
   dfile(60)='gsnd1bufr', dtype(60)='sndrd4',    dplat(60)='g13',       dsis(60)='sndrD4_g13',
        dval(60)=1.5,  dthin(60)=5,
   dfile(61)='iasibufr',  dtype(61)='iasi',      dplat(61)='metop-a',   dsis(61)='iasi616_metop-a',     dval(61)=20.0, dthin(61)=1,
   dfile(62)='gomebufr',  dtype(62)='gome',      dplat(62)='metop-a',   dsis(62)='gome_metop-a',        dval(62)=1.0,  dthin(62)=6,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /

EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit ;;

  global_regression)

set -ax

JCAP=62

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the benchmark run

exp1=$exp1_global_sub_1node
exp2=$exp1_global_bench_1node
exp3=$exp2_global_sub_2node
##exp1=gmao_gsi7.t62.subversion.1node
##exp2=q1fy10.t62.1node
##exp3=gmao_gsi7.t62.subversion.2node

# Choose global, regional, or RTMA
input=tmp${global}

# Name output file
output=$global_regression

# Give location of analysis results, and choose location for regression output
savdir=$ptmp_loc/$input
vfydir=$regression_vfydir

ncp=/bin/cp

# Name and create temporary directory
tmpdir=$ptmp_loc/$compare/$input/${exp1}_vs_${exp2}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Other required constants for regression testing
maxtime=1200
# Dew/Mist=26 GB/16 tasks per node
##maxmem=$((1500000*1))
# Vapor=110 GB/48 tasks per node
##maxmem=$((2300000*1))
# Cirrus=110 GB/32 tasks per node
maxmem=$((3400000*1))

# Copy stdout and fort.220 files 
# from $savdir to $tmpdir
list="$exp1 $exp2 $exp3"
for exp in $list; do
   $ncp $savdir/$exp/stdout ./stdout.$exp
   $ncp $savdir/$exp/fort.220 ./fort.220.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep 'a,b' fort.220.$exp > penalty.$exp.txt
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.1node.txt with penalty.10node.txt)
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt

# Give location of additional output files for scalability testing
# (i.e., output from increased number of nodes)

exp1_scale=$exp2_global_sub_2node
exp2_scale=$exp2_global_bench_2node
##exp1_scale=gmao_gsi7.t62.subversion.2node
##exp2_scale=q1fy10.t62.2node

# Copy stdout for additional scalability testing
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   $ncp $savdir/$exp_scale/stdout ./stdout.$exp_scale
done

# Grep out run time from stdout file
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   grep 'The total amount of wall time' stdout.$exp_scale > runtime.$exp_scale.txt
done

# Important values used to calculate timethresh and memthresh below
# Values below can be fine tuned to make the regression more or less aggressive
# Currently using a value of 10%

timedif=10
memdiff=8
scaledif=10

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from benchmark as avg values here

time2=$(awk '{ print $8 }' runtime.$exp2.txt)
time1=$(awk '{ print $8 }' runtime.$exp1.txt)
mem=$(awk '{ print $8 }' memory.$exp2.txt)

timethresh=$((time2 / timedif + time2))
memthresh=$((mem / memdiff + mem))

# Fill time variables with scalability data

if [[ -n $exp2_scale ]]; then

time_scale1=$(awk '{ print $8 }' runtime.$exp1_scale.txt)
time_scale2=$(awk '{ print $8 }' runtime.$exp2_scale.txt)

# Now, figure out difference in time between two runs

scale1=$((time1 / time_scale1))
scale2=$((time2 / time_scale2))

# Calculate maximum allowable deviation for scalability

scalability=$((scale2 / scaledif + scale2))

fi

# Begin applying threshold tests
# First, wall time (both maximum allowable time and max/min allowable deviation)

{

# This part is for the maximum allowable time (operationally)

  if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $maxtime ]]; then
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable operational time of '$maxtime' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the maximum allowable operational time of '$maxtime' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# This part is for deviation of wall time

{

  if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $timethresh ]]; then
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has
exceeded maximum allowable threshold time of '$timethresh' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the allowable threshold time of '$timethresh' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, maximum residence set size (both harware limitation and percent difference)
# First, hardware limitation

{

  if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $maxmem ]]; then
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable hardware memory limit of '$maxmem' KBs,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable hardware memory limit of '$maxmem' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, maximum residence set size

{

  if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $memthresh ]]; then
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable memory of '$memthresh' KBs,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable memory of '$memthresh' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, reproducibility

{

if [[ $(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are nonreproducible,'
   echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
fi

} >> $output

# Finally, scalability

{

if [[ -z $exp1_scale ]]; then
   echo 'No scalability test will be run due to no additional cases selected'
elif [[ $scale1 -gt $scalability ]]; then
   echo 'The case has failed the scalability regression test.'
   echo 'Please make sure that the same number of nodes/tasks were used.'
else
   echo 'The case has successfully passed the scalability test.'
fi

} >> $output

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit
