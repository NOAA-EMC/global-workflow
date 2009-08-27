#!/bin/sh

## Below are LoadLeveler (IBM queueing system) commands
#@ job_name=gsi_global
#@ error=gsi_global.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node_usage = not_shared
#@ tasks_per_node = 32
#@ node = 3
#@ node_resources = ConsumableMemory(110 GB)
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ class= devhigh
#@ group= devonprod
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 1:00:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ queue

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=YES


# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="PARTHDS=1:STACK=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="NLWIDTH=80"
export MP_LABELIO=YES


# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
adate=2009042012
exp=test2


# Set path/file for gsi executable
basedir=/global/noscrub
gsipath=$basedir/wx20rt/gsi_anl
##gsiexec=$gsipath/sorc/q1fy10/global_gsi
gsiexec=/global/save/wx****/gmao/gsi9/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/exec/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/global_gsi.fd/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/global_gsi.fd.bufr/global_gsi






# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=382
export LEVS=64
export JCAP_B=382


# Set runtime and save directories
tmpdir=/ptmp/wx****/tmp${JCAP}_sigmap/${exp}
savdir=/ptmp/wx****/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
fixgsi=/nwprod/fix
##fixcrtm=/nwprod/fix/crtm_gfsgsi

fixcrtm10=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi.rev1855.AAPP_AC
fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

dumpobs=gdas
dumpges=gdas

datobs=/global/shared/dump/$adate/$dumpobs
datobx=/global/shared/dump/$adate/${dumpobs}x
datoby=/global/shared/dump/$adate/${dumpobs}y

##datges=/global/shared/glopara/prd09q1oz
##datges=/global/noscrub/wx20rt/prd10q1g
datges=/global/noscrub/wx20rt/gsi_anl/cases/global/sigmap/$adate







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
yyg=`echo $gdate | cut -c1-8`
hhg=`echo $gdate | cut -c9-10`
yya=`echo $adate | cut -c1-8`
hha=`echo $adate | cut -c9-10`




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

# N19 crtm coefficients full in REL-1.2 crtm.  Partially in operational crtm_gfsgsi
##   dfile(65)='hirs4bufr', dtype(65)='hirs4',     dplat(65)='n19',     dsis(65)='hirs4_n19',       dval(65)=6.0,  dthin(65)=1,  dsfcalc(65)=0,
##   dfile(66)='amsuabufr', dtype(66)='amsua',     dplat(66)='n19',     dsis(66)='amsua_n19',       dval(66)=10.0, dthin(66)=2,  dsfcalc(66)=0,
##   dfile(67)='mhsbufr',   dtype(67)='mhs',       dplat(67)='n19',     dsis(67)='mhs_n19',         dval(67)=3.0,  dthin(67)=3,  dsfcalc(67)=0,
##  jcterm=.false.,jcdivt=.false.,bamp_ext1=2.5e12,bamp_ext2=5.0e11,
##  bamp_int1=2.5e13,bamp_int2=2.5e12, 

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=$IGEN,factqmin=0.000,factqmax=0.000,deltim=$DELTIM,
   ndat=67,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,print_diag_pcg=.true.,
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
   dfile(13)='sbuvbufrx',  dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',       dval(13)=1.0,  dthin(13)=0,  dsfcalc(13)=0,
   dfile(14)='sbuvbufrx',  dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',       dval(14)=1.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufrx',  dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',       dval(15)=1.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='hirs2bufr', dtype(16)='hirs2',     dplat(16)='n14',     dsis(16)='hirs2_n14',       dval(16)=6.0,  dthin(16)=1,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr', dtype(17)='hirs3',     dplat(17)='n16',     dsis(17)='hirs3_n16',       dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=0,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n17',     dsis(18)='hirs3_n17',       dval(18)=6.0,  dthin(18)=1,  dsfcalc(18)=0,
   dfile(19)='hirs4bufr', dtype(19)='hirs4',     dplat(19)='n18',     dsis(19)='hirs4_n18',       dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='metop-a', dsis(20)='hirs4_metop-a',   dval(20)=6.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='gsndrbufr', dtype(21)='sndr',      dplat(21)='g11',     dsis(21)='sndr_g11',        dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=0,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g12',     dsis(22)='sndr_g12',        dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=0,
   dfile(23)='gimgrbufr', dtype(23)='goes_img',  dplat(23)='g11',     dsis(23)='imgr_g11',        dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=0,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g12',     dsis(24)='imgr_g12',        dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=0,
   dfile(25)='airsbufr',  dtype(25)='airs',      dplat(25)='aqua',    dsis(25)='airs281SUBSET_aqua',dval(25)=20.0,dthin(25)=1, dsfcalc(25)=0,
   dfile(26)='msubufr',   dtype(26)='msu',       dplat(26)='n14',     dsis(26)='msu_n14',         dval(26)=2.0,  dthin(26)=2,  dsfcalc(26)=0,
   dfile(27)='amsuabufr', dtype(27)='amsua',     dplat(27)='n15',     dsis(27)='amsua_n15',       dval(27)=10.0, dthin(27)=2,  dsfcalc(27)=0,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n16',     dsis(28)='amsua_n16',       dval(28)=0.0,  dthin(28)=2,  dsfcalc(28)=0,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n17',     dsis(29)='amsua_n17',       dval(29)=0.0,  dthin(29)=2,  dsfcalc(29)=0,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n18',     dsis(30)='amsua_n18',       dval(30)=10.0, dthin(30)=2,  dsfcalc(30)=0,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='metop-a', dsis(31)='amsua_metop-a',   dval(31)=10.0, dthin(31)=2,  dsfcalc(31)=0,
   dfile(32)='airsbufr',  dtype(32)='amsua',     dplat(32)='aqua',    dsis(32)='amsua_aqua',      dval(32)=5.0,  dthin(32)=2,  dsfcalc(32)=0,
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
   dfile(61)='gomebufrx',  dtype(61)='gome',      dplat(61)='metop-a', dsis(61)='gome_metop-a',    dval(61)=1.0,  dthin(61)=6,  dsfcalc(61)=0,
   dfile(62)='omibufrx',   dtype(62)='omi',       dplat(62)='aura',    dsis(62)='omi_aura',        dval(62)=1.0,  dthin(62)=6,  dsfcalc(62)=0,
   dfile(63)='sbuvbufrx',  dtype(63)='sbuv2',     dplat(63)='n19',     dsis(63)='sbuv8_n19',       dval(63)=1.0,  dthin(63)=0,  dsfcalc(63)=0,
   dfile(64)='hirs4bufr', dtype(64)='hirs4',     dplat(64)='n19',     dsis(64)='hirs4_n19',       dval(64)=6.0,  dthin(64)=1,  dsfcalc(64)=0,
   dfile(65)='amsuabufr', dtype(65)='amsua',     dplat(65)='n19',     dsis(65)='amsua_n19',       dval(65)=10.0, dthin(65)=2,  dsfcalc(65)=0,
   dfile(66)='mhsbufr',   dtype(66)='mhs',       dplat(66)='n19',     dsis(66)='mhs_n19',         dval(66)=3.0,  dthin(66)=3,  dsfcalc(66)=0,
   dfile(67)='tcvitl'     dtype(67)='tcp',       dplat(67)=' ',       dsis(67)='tcp',             dval(67)=1.0,  dthin(67)=0,  dsfcalc(67)=0,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${CDATE},
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

berror=$fixgsi/global_berror.l${LEVS}y${NLAT}.f77

emiscoef=$fixcrtm10/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm10/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm10/CloudCoeff/Big_Endian/CloudCoeff.bin

# Use fixjif10 for REL-1.2 crtm
satinfo=$fixjif10/global_satinfo.txt.airs281SUBSET
##satinfo=$fixjif10/global_satinfo.txt.airs281SUBSET.metopa_amsua7
satangl=$fixjif10/global_satangbias.txt.airs281SUBSET

# Use fixgsi for operational crtm_gfsgsi
##satinfo=$fixgsi/global_satinfo.txt
##satangl=$fixgsi/global_satangbias.txt

pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixjif10/global_ozinfo.txt.no_n16
##ozinfo=$fixjif10/global_ozinfo.txt.n16_on
convinfo=$fixjif10/global_convinfo.txt
errtable=/nwprod/fix/prepobs_errtable.global


# Only need this file for single obs test
bufrtable=/nwprod/fix/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=/nwprod/fix/bufrtab.012

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
         $ncp $fixcrtm10/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm10/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done


# Copy observational data to $tmpdir
$ncp $datges/prepqc.$dumpobs.$adate   ./prepbufr
$ncp $datobs/gpsro.$dumpobs.$adate    ./gpsrobufr
$ncp $datobs/spssmi.$dumpobs.$adate   ./ssmirrbufr
$ncp $datobs/sptrmm.$dumpobs.$adate   ./tmirrbufr
##$ncp $datobs/osbuv8.$dumpobs.$adate   ./sbuvbufr
##$ncp $datobs/gome.$dumpobs.$adate     ./gomebufr
$ncp $datobs/goesfv.$dumpobs.$adate   ./gsnd1bufr
##$ncp $datobs/1bamua.$dumpobs.$adate   ./amsuabufr
$ncp $datobs/1bamub.$dumpobs.$adate   ./amsubbufr
$ncp $datobs/1bhrs2.$dumpobs.$adate   ./hirs2bufr
$ncp $datobs/1bhrs3.$dumpobs.$adate   ./hirs3bufr
##$ncp $datobs/1bhrs4.$dumpobs.$adate   ./hirs4bufr
##$ncp $datobs/1bmhs.$dumpobs.$adate    ./mhsbufr
$ncp $datobs/1bmsu.$dumpobs.$adate    ./msubufr
$ncp $datobs/airsev.$dumpobs.$adate   ./airsbufr
$ncp $datobs/mtiasi.$dumpobs.$adate   ./iasibufr
# $ncp $datobs/tcvitl.$dumpobs.$adate   ./tcvitl


# $ncp $datoby/osbuv8.$dumpobs.$adate   ./sbuvbufr
$ncp $datoby/1bhrs4.$dumpobs.$adate   ./hirs4bufr
$ncp $datoby/1bmhs.$dumpobs.$adate    ./mhsbufr
$ncp $datoby/1bamua.$dumpobs.$adate   ./amsuabufr


#Special data
##$ncp $datobx/gpsro.$dumpobs.$adate    ./gpsrobufr
##$ncp $datobx/mtiasi.$dumpobs.$adate   ./iasibufr
# $ncp $datobx/gome.$dumpobs.$adate     ./gomebufr
# $ncp $datobx/omi.$dumpobs.$adate      ./omibufr
# $ncp $datobx/esamua.$dumpobs.$adate   ./amusabufrears
# $ncp $datobx/esamub.$dumpobs.$adate   ./amusbbufrears
# $ncp $datobx/eshrs3.$dumpobs.$adate   ./hirs3bufrears



##$ncp $datobs/ssmit.$dumpobs.$adate    ./ssmitbufr
##$ncp $datobs/amsre.$dumpobs.$adate    ./amsrebufr
##$ncp $datobs/ssmis.$dumpobs.$adate    ./ssmisbufr



# Copy bias correction, atmospheric and surface files
$ncp $datges/biascr.$dumpges.$gdate   ./satbias_in
$ncp $datges/satang.$dumpges.$gdate   ./satbias_angle



$ncp $datges/sfcf03.$dumpges.$gdate   ./sfcf03
$ncp $datges/sfcf06.$dumpges.$gdate   ./sfcf06
$ncp $datges/sfcf09.$dumpges.$gdate   ./sfcf09

$ncp $datges/siggm3.$dumpges.$adate   ./sigf03
$ncp $datges/sigges.$dumpges.$adate   ./sigf06
$ncp $datges/siggp3.$dumpges.$adate   ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?


# Save output
mkdir -p $savdir

##cat stdout fort.2* > $savdir/stdout.anl.${adate}
##$ncp siganl          $savdir/siganl.${adate}
##$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
##$ncp satbias_out     $savdir/biascr.${adate}
##$ncp sfcf06          $savdir/sfcf06.${gdate}
##$ncp sigf06          $savdir/sigf06.${gdate}

ss2gg=$gsipath/util/ss2gg.fd/ss2gg
$ss2gg siganl siganl.bin siganl.ctl 4 768 384
$ss2gg sigf06 sigges.bin sigges.ctl 4 768 384

exit

##sfc2gg=$gsipath/util/sfc2gg.fd/sfc2gg
##$sfc2gg sfcanl.gsi sfcanl.bin sfcanl.ctl
##$sfc2gg sfcf06     sfcges.bin sfcges.ctl

##$ncp s*anl.bin $savdir/
##$ncp s*anl.ctl $savdir/
##$ncp s*ges.bin $savdir/
##$ncp s*ges.ctl $savdir/

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#


echo "Time before diagnostic loop is `date` "
cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "



# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $tmpdir
      cd $tmpdir
      cd ../
      rmdir $tmpdir
   fi
fi


# End of script
exit
