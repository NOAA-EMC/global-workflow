#/bin/sh
#
#@ job_name=gsi_nmm
#@ error=gsi_nmm.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ total_tasks = 32
#@ node = 4
#@ node_usage = not_shared
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ class=dev
#@ group=dev
#@ account_no = NAM-T2O
#@ wall_clock_limit = 0:30:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ queue


set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for threading and stacksize
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
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
##export MP_COREFILE_FORMAT=lite

# What is the endianness of the machine? (default is Big_Endian)
export endianness=Big_Endian

# Choose type of GPSRO data to assimilate (default is gps_ref)
export gps_dtype="gps_ref"

# Set experiment name and analysis date
ndate=/nwprod/util/exec/ndate

adate=2010112900

exp=${USER}_nmm.$adate.$gps_dtype

TM=00
TM2=03


# Set resoltion and other dependent parameters
export JCAP=62
export NLAT=180
export NLON=360
export LEVS=60
export DELTIM=1200

tmpdir=/ptmp/$USER/nam_gsi_oper/${exp}
savdir=/ptmp/$USER/outreg/nam_gsi_oper/${exp}


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
#FIXnam=/global/save/$USER/mlueken/fix
FIXnam=/u/wx20xs/home/gsi/xsu/fix
CRTMnam=/global/save/wx20ml/CRTM_REL-2.1.3/Big_Endian

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$FIXnam}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./global_co2_data.txt
        while [ $yyyy -ge 1957 ] ;do
                co2=$co2dir/global_co2historicaldata_$yyyy.txt
                if [ -s $co2 ] ; then
                        $ncp $co2 ./global_co2_data.txt
                break
                fi
                ((yyyy-=1))
        done
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=60,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   hzscl=0.373,0.746,1.50,
   vs=1.0,bw=0.,fstat=.true.,
 /
 &ANBKGERR
   anisotropic=.false.,an_vs=1.0,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=4.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,  
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=1.0,  dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='satwndbufr',    dtype(05)='uv',        dplat(05)=' ',         dsis(05)='uv',                  dval(05)=1.0,  dthin(05)=0, dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',         dsis(06)='spd',                 dval(06)=1.0,  dthin(06)=0, dsfcalc(06)=0,
   dfile(07)='radarbufr', dtype(07)='rw',        dplat(07)=' ',         dsis(07)='rw',                  dval(07)=1.0,  dthin(07)=0, dsfcalc(07)=0,
   dfile(08)='prepbufr',  dtype(08)='dw',        dplat(08)=' ',         dsis(08)='dw',                  dval(08)=1.0,  dthin(08)=0, dsfcalc(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',         dsis(09)='sst',                 dval(09)=1.0,  dthin(09)=0, dsfcalc(09)=0,
   dfile(10)='prepbufr',  dtype(10)='pw',        dplat(10)=' ',         dsis(10)='pw',                  dval(10)=1.0,  dthin(10)=0, dsfcalc(10)=0,
   dfile(11)='gpsrobufr', dtype(11)='$gps_dtype',   dplat(11)=' ',         dsis(11)='gps',             dval(11)=1.0,  dthin(11)=0, dsfcalc(11)=0,
   dfile(12)='ssmirrbufr',dtype(12)='pcp_ssmi',  dplat(12)='dmsp',      dsis(12)='pcp_ssmi',            dval(12)=1.0,  dthin(12)=-1,dsfcalc(12)=0,
   dfile(13)='tmirrbufr', dtype(13)='pcp_tmi',   dplat(13)='trmm',      dsis(13)='pcp_tmi',             dval(13)=1.0,  dthin(13)=-1,dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n16',       dsis(14)='sbuv8_n16',           dval(14)=1.0,  dthin(14)=0, dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n17',       dsis(15)='sbuv8_n17',           dval(15)=1.0,  dthin(15)=0, dsfcalc(15)=0,
   dfile(16)='sbuvbufr',  dtype(16)='sbuv2',     dplat(16)='n18',       dsis(16)='sbuv8_n18',           dval(16)=1.0,  dthin(16)=0, dsfcalc(16)=0,
   dfile(17)='omi',       dtype(17)='omi',       dplat(17)='aura',      dsis(17)='omi_aura',            dval(17)=1.0,  dthin(17)=6, dsfcalc(17)=0,
   dfile(18)='hirs2bufr', dtype(18)='hirs2',     dplat(18)='n14',       dsis(18)='hirs2_n14',           dval(18)=6.0,  dthin(18)=1, dsfcalc(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n16',       dsis(19)='hirs3_n16',           dval(19)=0.0,  dthin(19)=1, dsfcalc(19)=1,
   dfile(20)='hirs3bufr', dtype(20)='hirs3',     dplat(20)='n17',       dsis(20)='hirs3_n17',           dval(20)=6.0,  dthin(20)=1, dsfcalc(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='n18',       dsis(21)='hirs4_n18',           dval(21)=0.0,  dthin(21)=1, dsfcalc(21)=1,
   dfile(22)='hirs4bufr', dtype(22)='hirs4',     dplat(22)='metop-a',   dsis(22)='hirs4_metop-a',       dval(22)=6.0,  dthin(22)=1, dsfcalc(22)=1,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g11',       dsis(23)='sndr_g11',            dval(23)=0.0,  dthin(23)=1, dsfcalc(23)=0,
   dfile(24)='gsndrbufr', dtype(24)='sndr',      dplat(24)='g12',       dsis(24)='sndr_g12',            dval(24)=0.0,  dthin(24)=1, dsfcalc(24)=0,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g11',       dsis(25)='imgr_g11',            dval(25)=0.0,  dthin(25)=1, dsfcalc(25)=0,
   dfile(26)='gimgrbufr', dtype(26)='goes_img',  dplat(26)='g12',       dsis(26)='imgr_g12',            dval(26)=0.0,  dthin(26)=1, dsfcalc(26)=0,
   dfile(27)='airsbufr',  dtype(27)='airs',      dplat(27)='aqua',      dsis(27)='airs281SUBSET_aqua',  dval(27)=20.0, dthin(27)=1, dsfcalc(27)=1,
   dfile(28)='msubufr',   dtype(28)='msu',       dplat(28)='n14',       dsis(28)='msu_n14',             dval(28)=2.0,  dthin(28)=2, dsfcalc(28)=1,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n15',       dsis(29)='amsua_n15',           dval(29)=10.0, dthin(29)=2, dsfcalc(29)=1,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n16',       dsis(30)='amsua_n16',           dval(30)=0.0,  dthin(30)=2, dsfcalc(30)=1,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n17',       dsis(31)='amsua_n17',           dval(31)=0.0,  dthin(31)=2, dsfcalc(31)=1,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='n18',       dsis(32)='amsua_n18',           dval(32)=10.0, dthin(32)=2, dsfcalc(32)=1,
   dfile(33)='amsuabufr', dtype(33)='amsua',     dplat(33)='metop-a',   dsis(33)='amsua_metop-a',       dval(33)=10.0, dthin(33)=2, dsfcalc(33)=1,
   dfile(34)='airsbufr',  dtype(34)='amsua',     dplat(34)='aqua',      dsis(34)='amsua_aqua',          dval(34)=5.0,  dthin(34)=2, dsfcalc(34)=1,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n15',       dsis(35)='amsub_n15',           dval(35)=3.0,  dthin(35)=3, dsfcalc(35)=1,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n16',       dsis(36)='amsub_n16',           dval(36)=3.0,  dthin(36)=3, dsfcalc(36)=1,
   dfile(37)='amsubbufr', dtype(37)='amsub',     dplat(37)='n17',       dsis(37)='amsub_n17',           dval(37)=3.0,  dthin(37)=3, dsfcalc(37)=1,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='n18',       dsis(38)='mhs_n18',             dval(38)=3.0,  dthin(38)=3, dsfcalc(38)=1,
   dfile(39)='mhsbufr',   dtype(39)='mhs',       dplat(39)='metop-a',   dsis(39)='mhs_metop-a',         dval(39)=3.0,  dthin(39)=3, dsfcalc(39)=1,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f13',       dsis(40)='ssmi_f13',            dval(40)=0.0,  dthin(40)=4, dsfcalc(40)=0,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f14',       dsis(41)='ssmi_f14',            dval(41)=0.0,  dthin(41)=4, dsfcalc(41)=0,
   dfile(42)='ssmitbufr', dtype(42)='ssmi',      dplat(42)='f15',       dsis(42)='ssmi_f15',            dval(42)=0.0,  dthin(42)=4, dsfcalc(42)=0,
   dfile(43)='amsrebufr', dtype(43)='amsre_low', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=4, dsfcalc(43)=1,
   dfile(44)='amsrebufr', dtype(44)='amsre_mid', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=4, dsfcalc(44)=1,
   dfile(45)='amsrebufr', dtype(45)='amsre_hig', dplat(45)='aqua',      dsis(45)='amsre_aqua',          dval(45)=0.0,  dthin(45)=4, dsfcalc(45)=1,
   dfile(46)='ssmisbufr', dtype(46)='ssmis',     dplat(46)='f16',       dsis(46)='ssmis_f16',           dval(46)=0.0,  dthin(46)=4, dsfcalc(46)=1,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd1',    dplat(47)='g12',       dsis(47)='sndrD1_g12',          dval(47)=1.5,  dthin(47)=5, dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd2',    dplat(48)='g12',       dsis(48)='sndrD2_g12',          dval(48)=1.5,  dthin(48)=5, dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd3',    dplat(49)='g12',       dsis(49)='sndrD3_g12',          dval(49)=1.5,  dthin(49)=5, dsfcalc(49)=0,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd4',    dplat(50)='g12',       dsis(50)='sndrD4_g12',          dval(50)=1.5,  dthin(50)=5, dsfcalc(50)=0,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd1',    dplat(51)='g11',       dsis(51)='sndrD1_g11',          dval(51)=1.5,  dthin(51)=5, dsfcalc(51)=0,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd2',    dplat(52)='g11',       dsis(52)='sndrD2_g11',          dval(52)=1.5,  dthin(52)=5, dsfcalc(52)=0,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd3',    dplat(53)='g11',       dsis(53)='sndrD3_g11',          dval(53)=1.5,  dthin(53)=5, dsfcalc(53)=0,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd4',    dplat(54)='g11',       dsis(54)='sndrD4_g11',          dval(54)=1.5,  dthin(54)=5, dsfcalc(54)=0,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd1',    dplat(55)='g13',       dsis(55)='sndrD1_g13',          dval(55)=1.5,  dthin(55)=5, dsfcalc(55)=0,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd2',    dplat(56)='g13',       dsis(56)='sndrD2_g13',          dval(56)=1.5,  dthin(56)=5, dsfcalc(56)=0,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd3',    dplat(57)='g13',       dsis(57)='sndrD3_g13',          dval(57)=1.5,  dthin(57)=5, dsfcalc(57)=0,
   dfile(58)='gsnd1bufr', dtype(58)='sndrd4',    dplat(58)='g13',       dsis(58)='sndrD4_g13',          dval(58)=1.5,  dthin(58)=5, dsfcalc(58)=0,
   dfile(59)='iasibufr',  dtype(59)='iasi',      dplat(59)='metop-a',   dsis(59)='iasi616_metop-a',     dval(59)=20.0, dthin(59)=1, dsfcalc(59)=1,
   dfile(60)='gomebufr',  dtype(60)='gome',      dplat(60)='metop-a',   dsis(60)='gome_metop-a',        dval(60)=1.0,  dthin(60)=6, dsfcalc(60)=0,
 /
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /
EOF

anavinfo=$FIXnam/anavinfo_ndas_binary
berror=$FIXnam/$endianness/nam_nmmstat_na.gcv

emiscoef_IRwater=$CRTMnam/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$CRTMnam/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$CRTMnam/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$CRTMnam/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$CRTMnam/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$CRTMnam/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$CRTMnam/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$CRTMnam/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$CRTMnam/FASTEM5.MWwater.EmisCoeff.bin
aercoef=$CRTMnam/AerosolCoeff.bin
cldcoef=$CRTMnam/CloudCoeff.bin
satinfo=$FIXnam/nam_regional_satinfo.txt
scaninfo=$FIXnam/global_scaninfo.txt
satangl=$FIXnam/nam_global_satangbias.txt
atmsbeamdat=$fix_file/atms_beamwidth.txt
pcpinfo=$FIXnam/nam_global_pcpinfo.txt
ozinfo=$FIXnam/nam_global_ozinfo.txt
errtable=$FIXnam/nam_errtable.r3dv
convinfo=$FIXnam/nam_regional_convinfo.txt
mesonetuselist=$FIXnam/nam_mesonet_uselist.txt

cp $anavinfo ./anavinfo
cp $berror   ./berror_stats
cp $errtable ./errtable
cp $emiscoef ./EmisCoeff.bin
cp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
cp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
cp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
cp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
cp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
cp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
cp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
cp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
cp $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
cp $aercoef  ./AerosolCoeff.bin
cp $cldcoef  ./CloudCoeff.bin
cp $satangl  ./satbias_angle
cp $satinfo  ./satinfo
cp $scaninfo ./scaninfo
cp $pcpinfo  ./pcpinfo
cp $ozinfo   ./ozinfo
cp $convinfo ./convinfo
cp $mesonetuselist ./mesonetuselist


# Copy executable and fixed files to $tmpdir
gsiexec=/u/wx20xs/home/gsi/xsu/src/global_gsi
cp $gsiexec  ./gsi.x

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $CRTMnam/${file}.SpcCoeff.bin ./
   $ncp $CRTMnam/${file}.TauCoeff.bin ./
done
set -x

CDATE=$adate
PDY=`echo $CDATE | cut -c1-8`
CYC=`echo $CDATE | cut -c9-10`

if [ $TM = 00 ] ; then
   datdir=/com/nam/prod/nam.$PDY
   cp $datdir/nam.t${CYC}z.prepbufr.tm${TM}       ./prepbufr
   cp $datdir/nam.t${CYC}z.satwnd.tm${TM}.bufr_d       ./satwndbufr
   cp $datdir/nam.t${CYC}z.1bhrs3.tm${TM}.bufr_d  ./hirs3bufr
   cp $datdir/nam.t${CYC}z.1bamub.tm${TM}.bufr_d  ./amsubbufr
   cp $datdir/nam.t${CYC}z.1bamua.tm${TM}.bufr_d  ./amsuabufr
   cp $datdir/nam.t${CYC}z.1bmhs.tm${TM}.bufr_d   ./mhsbufr
   cp $datdir/nam.t${CYC}z.1bhrs4.tm${TM}.bufr_d  ./hirs4bufr
   cp $datdir/nam.t${CYC}z.radwnd.tm${TM}.bufr_d  ./radarbufr
   cp $datdir/nam.t${CYC}z.nexrad.tm${TM}.bufr_d  ./l2rwbufr
   cp $datdir/nam.t${CYC}z.goesfv.tm${TM}.bufr_d  ./gsnd1bufr
   cp $datdir/nam.t${CYC}z.airsev.tm${TM}.bufr_d  ./airsbufr
else
   datdir=/com/nam/prod/ndas.$PDY
   cp $datdir/ndas.t${CYC}z.prepbufr.tm${TM}       ./prepbufr
   cp $datdir/ndas.t${CYC}z.satwnd.tm${TM}.bufr_d  ./satwndbufr
   cp $datdir/ndas.t${CYC}z.1bhrs3.tm${TM}.bufr_d  ./hirs3bufr
   cp $datdir/ndas.t${CYC}z.1bamub.tm${TM}.bufr_d  ./amsubbufr
   cp $datdir/ndas.t${CYC}z.1bamua.tm${TM}.bufr_d  ./amsuabufr
   cp $datdir/ndas.t${CYC}z.1bmhs.tm${TM}.bufr_d   ./mhsbufr
   cp $datdir/ndas.t${CYC}z.1bhrs4.tm${TM}.bufr_d  ./hirs4bufr
   cp $datdir/ndas.t${CYC}z.radwnd.tm${TM}.bufr_d  ./radarbufr
   cp $datdir/ndas.t${CYC}z.nexrad.tm${TM}.bufr_d  ./l2rwbufr
   cp $datdir/ndas.t${CYC}z.goesfv.tm${TM}.bufr_d  ./gsnd1bufr
   cp $datdir/ndas.t${CYC}z.airsev.tm${TM}.bufr_d  ./airsbufr
fi

cp /com/nam/prod/ndas.${PDY}/ndas.t${CYC}z.satang.tm${TM2}  ./satbias_angle
cp /com/nam/prod/ndas.${PDY}/ndas.t${CYC}z.satbias.tm${TM2} ./satbias_in
cp /com/nam/prod/nam.${PDY}/nam.t${CYC}z.wrfrst_d01.ges     ./wrf_inout



# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

exit
