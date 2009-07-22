#!/bin/sh
#
#  test nems nmmb regional option for gsi
#
#@ job_name=regression_test
#@ error= gsi_nems_nmmb_update.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity = core(1)
#@ node_resources = ConsumableMemory(110GB)
#@ class=dev
#@ group=dev
#@ account_no = RDAS-MTN
#@ wall_clock_limit = 0:15:00
#@ notification=error
#@ queue

set -x

#  some of parameters to eventually be put in regression_var.sh

#export adate_regional=??????
export datobs_nmm_binary=/meso/noscrub/wx23dp/nmmb_regression_case
export exp1_nems_nmmb_sub_2node=nmmb_regression_test
export subversion=/meso/save/wx23dp/svn/workspace/dparrish2/src/global_gsi
export nems_nmmb=nems_nmmb
export ptmp_loc=/ptmp/wx23dp
export fix_file=/meso/save/wx23dp/svn/workspace/dparrish2/fix

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_PULSE=0
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for threads
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
##export MP_COREFILE_FORMAT=lite

# Set analysis date
#adate=$adate_regional

# Set experiment name
exp=$exp1_nems_nmmb_sub_2node

# Set path/file for gsi executable
gsiexec=$subversion

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export LEVS=60
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${nems_nmmb}/${exp}
savdir=$ptmp_loc/outreg/${nems_nmmb}/${exp}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

## Given the analysis date, compute the date from which the
## first guess comes.  Extract cycle and set prefix and suffix
## for guess and observation data files
#sdate=`echo $adate |cut -c1-8`
#odate=`$ndate +12 $adate`
#hha=`echo $adate | cut -c9-10`
#hho=`echo $odate | cut -c9-10`
#prefixo=ndas.t${hho}z
#prefixa=ndas.t${hha}z
###suffix=tm00.bufr_d
#suffix=tm12.bufr_d

datobs=$datobs_nmm_binary
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=67,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,preserve_restart_date=.true.,
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,nems_nmmb_regional=.true.,diagnostic_reg=.true.,
   nmmb_reference_grid='H',grid_ratio_nmmb=1.412,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   as=0.28,0.28,0.3,0.7,0.1,0.5,1.0,1.0,
   hzscl=0.373,0.746,1.50,
   vs=0.6,bw=0.,fstat=.false.,
 /
 &ANBKGERR
   anisotropic=.false.,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=0,nvmodes_keep=8,period_max=3.,
    baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,
   vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',              dval(01)=1.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',               dval(02)=1.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',               dval(03)=1.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',              dval(04)=1.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',              dval(05)=1.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',       dsis(06)='spd',             dval(06)=1.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',       dsis(07)='dw',              dval(07)=1.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='radarbufr', dtype(08)='rw',        dplat(08)=' ',       dsis(08)='rw',              dval(08)=1.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',       dsis(09)='sst',             dval(09)=1.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',       dsis(10)='gps_ref',         dval(10)=1.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',    dsis(11)='pcp_ssmi',        dval(11)=1.0,  dthin(11)=-1, dsfcalc(11)=0,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',    dsis(12)='pcp_tmi',         dval(12)=1.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',       dval(13)=1.0,  dthin(13)=0,  dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',       dval(14)=1.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',       dval(15)=1.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='hirs2bufr', dtype(16)='hirs2',     dplat(16)='n14',     dsis(16)='hirs2_n14',       dval(16)=6.0,  dthin(16)=1,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n16',     dsis(17)='hirs3_n16',       dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=0,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n17',     dsis(18)='hirs3_n17',       dval(18)=6.0,  dthin(18)=1,  dsfcalc(18)=0,
   dfile(19)='hirs4bufr', dtype(19)='hirs4',     dplat(19)='n18',     dsis(19)='hirs4_n18',       dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='metop-a', dsis(20)='hirs4_metop-a',   dval(20)=6.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='gsndrbufr', dtype(21)='sndr',      dplat(21)='g11',     dsis(21)='sndr_g11',        dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=0,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g12',     dsis(22)='sndr_g12',        dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=0,
   dfile(23)='gimgrbufr', dtype(23)='goes_img',  dplat(23)='g11',     dsis(23)='imgr_g11',        dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=0,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g12',     dsis(24)='imgr_g12',        dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=0,
   dfile(25)='airsbufr',  dtype(25)='airs',      dplat(25)='aqua',    dsis(25)='airs281SUBSET_aqua',    dval(25)=20.0, dthin(25)=1,  dsfcalc(25)=0,
   dfile(26)='msubufr',   dtype(26)='msu',       dplat(26)='n14',     dsis(26)='msu_n14',         dval(26)=2.0,  dthin(26)=2,  dsfcalc(26)=0,
   dfile(27)='amsuabufr', dtype(27)='amsua',     dplat(27)='n15',     dsis(27)='amsua_n15',       dval(27)=10.0, dthin(27)=2,  dsfcalc(27)=0,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n16',     dsis(28)='amsua_n16',       dval(28)=0.0,  dthin(28)=2,  dsfcalc(28)=0,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n17',     dsis(29)='amsua_n17',       dval(29)=0.0,  dthin(29)=2,  dsfcalc(29)=0,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n18',     dsis(30)='amsua_n18',       dval(30)=10.0, dthin(30)=2,  dsfcalc(30)=0,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='metop-a', dsis(31)='amsua_metop-a',   dval(31)=10.0, dthin(31)=2,  dsfcalc(31)=0,
   dfile(32)='airsbufr',  dtype(32)='amsua',     dplat(32)='aqua',    dsis(32)='amsua_aqua',      dval(32)=10.0, dthin(32)=2,  dsfcalc(32)=0,
   dfile(33)='amsubbufr', dtype(33)='amsub',     dplat(33)='n15',     dsis(33)='amsub_n15',       dval(33)=3.0,  dthin(33)=3,  dsfcalc(33)=0,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n16',     dsis(34)='amsub_n16',       dval(34)=3.0,  dthin(34)=3,  dsfcalc(34)=0,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n17',     dsis(35)='amsub_n17',       dval(35)=3.0,  dthin(35)=3,  dsfcalc(35)=0,
   dfile(36)='mhsbufr',   dtype(36)='mhs',       dplat(36)='n18',     dsis(36)='mhs_n18',         dval(36)=3.0,  dthin(36)=3,  dsfcalc(36)=0,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='metop-a', dsis(37)='mhs_metop-a',     dval(37)=3.0,  dthin(37)=3,  dsfcalc(37)=0,
   dfile(38)='ssmitbufr', dtype(38)='ssmi',      dplat(38)='f13',     dsis(38)='ssmi_f13',        dval(38)=0.0,  dthin(38)=4,  dsfcalc(38)=0,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f14',     dsis(39)='ssmi_f14',        dval(39)=0.0,  dthin(39)=4,  dsfcalc(39)=0,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f15',     dsis(40)='ssmi_f15',        dval(40)=0.0,  dthin(40)=4,  dsfcalc(40)=0,
   dfile(41)='amsrebufr', dtype(41)='amsre_low', dplat(41)='aqua',    dsis(41)='amsre_aqua',      dval(41)=0.0,  dthin(41)=4,  dsfcalc(41)=0,
   dfile(42)='amsrebufr', dtype(42)='amsre_mid', dplat(42)='aqua',    dsis(42)='amsre_aqua',      dval(42)=0.0,  dthin(42)=4,  dsfcalc(42)=0,
   dfile(43)='amsrebufr', dtype(43)='amsre_hig', dplat(43)='aqua',    dsis(43)='amsre_aqua',      dval(43)=0.0,  dthin(43)=4,  dsfcalc(43)=0,
   dfile(44)='ssmisbufr', dtype(44)='ssmis_las', dplat(44)='f16',     dsis(44)='ssmis_f16',       dval(44)=0.0,  dthin(44)=4,  dsfcalc(44)=0,
   dfile(45)='ssmisbufr', dtype(45)='ssmis_uas', dplat(45)='f16',     dsis(45)='ssmis_f16',       dval(45)=0.0,  dthin(45)=4,  dsfcalc(45)=0,
   dfile(46)='ssmisbufr', dtype(46)='ssmis_img', dplat(46)='f16',     dsis(46)='ssmis_f16',       dval(46)=0.0,  dthin(46)=4,  dsfcalc(46)=0,
   dfile(47)='ssmisbufr', dtype(47)='ssmis_env', dplat(47)='f16',     dsis(47)='ssmis_f16',       dval(47)=0.0,  dthin(47)=4,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd1',    dplat(48)='g12',     dsis(48)='sndrD1_g12',      dval(48)=1.5,  dthin(48)=5,  dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd2',    dplat(49)='g12',     dsis(49)='sndrD2_g12',      dval(49)=1.5,  dthin(49)=5,  dsfcalc(49)=0,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd3',    dplat(50)='g12',     dsis(50)='sndrD3_g12',      dval(50)=1.5,  dthin(50)=5,  dsfcalc(50)=0,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd4',    dplat(51)='g12',     dsis(51)='sndrD4_g12',      dval(51)=1.5,  dthin(51)=5,  dsfcalc(51)=0,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd1',    dplat(52)='g11',     dsis(52)='sndrD1_g11',      dval(52)=1.5,  dthin(52)=5,  dsfcalc(52)=0,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd2',    dplat(53)='g11',     dsis(53)='sndrD2_g11',      dval(53)=1.5,  dthin(53)=5,  dsfcalc(53)=0,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd3',    dplat(54)='g11',     dsis(54)='sndrD3_g11',      dval(54)=1.5,  dthin(54)=5,  dsfcalc(54)=0,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd4',    dplat(55)='g11',     dsis(55)='sndrD4_g11',      dval(55)=1.5,  dthin(55)=5,  dsfcalc(55)=0,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd1',    dplat(56)='g13',     dsis(56)='sndrD1_g13',      dval(56)=1.5,  dthin(56)=5,  dsfcalc(56)=0,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd2',    dplat(57)='g13',     dsis(57)='sndrD2_g13',      dval(57)=1.5,  dthin(57)=5,  dsfcalc(57)=0,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd3',    dplat(58)='g13',     dsis(58)='sndrD3_g13',      dval(58)=1.5,  dthin(58)=5,  dsfcalc(58)=0,
   dfile(59)='gsnd1bufr', dtype(59)='sndrd4',    dplat(59)='g13',     dsis(59)='sndrD4_g13',      dval(59)=1.5,  dthin(59)=5,  dsfcalc(59)=0,
   dfile(60)='iasibufr',  dtype(60)='iasi',      dplat(60)='metop-a', dsis(60)='iasi616_metop-a', dval(60)=20.0, dthin(60)=1,  dsfcalc(60)=0,
   dfile(61)='gomebufr',  dtype(61)='gome',      dplat(61)='metop-a', dsis(61)='gome_metop-a',    dval(61)=1.0,  dthin(61)=6,  dsfcalc(61)=0,
   dfile(62)='omibufr',   dtype(62)='omi',       dplat(62)='aura',    dsis(62)='omi_aura',        dval(62)=1.0,  dthin(62)=6,  dsfcalc(62)=0,
   dfile(63)='sbuvbufr',  dtype(63)='sbuv2',     dplat(63)='n19',     dsis(63)='sbuv8_n19',       dval(63)=1.0,  dthin(63)=0,  dsfcalc(63)=0,
   dfile(64)='hirs4bufr', dtype(64)='hirs4',     dplat(64)='n19',     dsis(64)='hirs4_n19',       dval(64)=6.0,  dthin(64)=1,  dsfcalc(64)=0,
   dfile(65)='amsuabufr', dtype(65)='amsua',     dplat(65)='n19',     dsis(65)='amsua_n19',       dval(65)=10.0, dthin(65)=2,  dsfcalc(65)=0,
   dfile(66)='mhsbufr',   dtype(66)='mhs',       dplat(66)='n19',     dsis(66)='mhs_n19',         dval(66)=3.0,  dthin(66)=3,  dsfcalc(66)=0,
   dfile(67)='tcvitl'     dtype(67)='tcp',       dplat(67)=' ',       dsis(67)='tcp',             dval(67)=1.0,  dthin(67)=0,  dsfcalc(67)=0,
 /
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /
EOF

berror=$fix_file/nam_glb_berror.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
errtable=$fix_file/nam_errtable.r3dv
convinfo=$fix_file/global_convinfo.txt
mesonetuselist=$fix_file/nam_mesonet_uselist.txt

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
$ncp $mesonetuselist ./mesonetuselist

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
$ncp $datobs/ndas.t12z.prepbufr.tm12      ./prepbufr
$ncp $datobs/ndas.t12z.1bhrs3.tm12.bufr_d ./hirs3bufr
$ncp $datobs/ndas.t12z.1bhrs4.tm12.bufr_d ./hirs4bufr
$ncp $datobs/ndas.t12z.1bamua.tm12.bufr_d ./amsuabufr
$ncp $datobs/ndas.t12z.1bamub.tm12.bufr_d ./amsubbufr
$ncp $datobs/ndas.t12z.1bmhs.tm12.bufr_d  ./mhsbufr
$ncp $datobs/ndas.t12z.goesfv.tm12.bufr_d ./gsnd1bufr
$ncp $datobs/ndas.t12z.airsev.tm12.bufr_d ./airsbufr
$ncp $datobs/ndas.t12z.radwnd.tm12.bufr_d ./radarbufr

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datges/ndas.t06z.satang.tm09 ./satbias_angle
$ncp $datges/ndas.t06z.satbias.tm09 ./satbias_in

$ncp $datges/nmm_b_history_nemsio.012 wrf_inout
$ncp $datges/nmm_b_history_nemsio.012.ctl wrf_inout.ctl
$ncp wrf_inout wrf_ges
$ncp wrf_inout.ctl wrf_ges.ctl

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?
