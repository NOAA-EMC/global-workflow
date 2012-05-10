
set -x

# Set analysis date
#adate=$adate_regional_nems_nmmb

# Set experiment name

if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#gsiexec=$cntrl

# Set resoltion and other dependent parameters
#export JCAP=62
export LEVS=60
export JCAP_B=$JCAP
export LEVS=60
export DELTIM=1200

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_nems_nmmb/${exp}
savdir=$savdir/outreg_nems_nmmb/${exp}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
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
#suffix=tm12.bufr_d

#datobs=$datobs
#datges=$datges

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$nmmb_nems_adate}|cut -c1-4)
        rm ./global_co2_data.txt
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
                if [ -s $co2 ] ; then
                        $ncp $co2 ./global_co2_data.txt
                fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
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
LAGDATA=""
HYBRID_ENSEMBLE=""

cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=66,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,preserve_restart_date=.true.,
   use_gfs_ozone=.true.,check_gfs_ozone_date=.true.,regional_ozone=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,nems_nmmb_regional=.true.,diagnostic_reg=.false.,
   nmmb_reference_grid='H',grid_ratio_nmmb=1.412,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
   $GRIDOPTS
 /
 &BKGERR
   hzscl=0.373,0.746,1.50,
   vs=0.6,bw=0.,fstat=.false.,
   $BKGERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=0,nvmodes_keep=8,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,
   vadfile='prepbufr',
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                 dval(01)=1.0, dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                  dval(02)=1.0, dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                  dval(03)=1.0, dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                 dval(04)=1.0, dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='satwnd',    dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                 dval(05)=1.0, dthin(05)=0, dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='uv',        dplat(06)=' ',       dsis(06)='uv',                 dval(06)=1.0, dthin(06)=0, dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='spd',       dplat(07)=' ',       dsis(07)='spd',                dval(07)=1.0, dthin(07)=0, dsfcalc(07)=0,
   dfile(08)='prepbufr',  dtype(08)='dw',        dplat(08)=' ',       dsis(08)='dw',                 dval(08)=1.0, dthin(08)=0, dsfcalc(08)=0,
   dfile(09)='radarbufr', dtype(09)='rw',        dplat(09)=' ',       dsis(09)='rw',                 dval(09)=1.0, dthin(09)=0, dsfcalc(09)=0,
   dfile(10)='prepbufr',  dtype(10)='sst',       dplat(10)=' ',       dsis(10)='sst',                dval(10)=1.0, dthin(10)=0, dsfcalc(10)=0,
   dfile(11)='gpsrobufr', dtype(11)='gps_bnd',   dplat(11)=' ',       dsis(11)='gps',                dval(11)=1.0, dthin(11)=0, dsfcalc(11)=0,
   dfile(12)='ssmirrbufr',dtype(12)='pcp_ssmi',  dplat(12)='dmsp',    dsis(12)='pcp_ssmi',           dval(12)=1.0, dthin(12)=-1,dsfcalc(12)=0,
   dfile(13)='tmirrbufr', dtype(13)='pcp_tmi',   dplat(13)='trmm',    dsis(13)='pcp_tmi',            dval(13)=1.0, dthin(13)=-1,dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n16',     dsis(14)='sbuv8_n16',          dval(14)=1.0, dthin(14)=0, dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n17',     dsis(15)='sbuv8_n17',          dval(15)=1.0, dthin(15)=0, dsfcalc(15)=0,
   dfile(16)='sbuvbufr',  dtype(16)='sbuv2',     dplat(16)='n18',     dsis(16)='sbuv8_n18',          dval(16)=1.0, dthin(16)=0, dsfcalc(16)=0,
   dfile(17)='hirs2bufr', dtype(17)='hirs2',     dplat(17)='n14',     dsis(17)='hirs2_n14',          dval(17)=6.0, dthin(17)=1, dsfcalc(17)=1,
   dfile(18)='hirs3bufr', dtype(18)='hirs3',     dplat(18)='n16',     dsis(18)='hirs3_n16',          dval(18)=0.0, dthin(18)=1, dsfcalc(18)=1,
   dfile(19)='hirs3bufr', dtype(19)='hirs3',     dplat(19)='n17',     dsis(19)='hirs3_n17',          dval(19)=6.0, dthin(19)=1, dsfcalc(19)=1,
   dfile(20)='hirs4bufr', dtype(20)='hirs4',     dplat(20)='n18',     dsis(20)='hirs4_n18',          dval(20)=0.0, dthin(20)=1, dsfcalc(20)=1,
   dfile(21)='hirs4bufr', dtype(21)='hirs4',     dplat(21)='metop-a', dsis(21)='hirs4_metop-a',      dval(21)=6.0, dthin(21)=1, dsfcalc(21)=1,
   dfile(22)='gsndrbufr', dtype(22)='sndr',      dplat(22)='g11',     dsis(22)='sndr_g11',           dval(22)=0.0, dthin(22)=1, dsfcalc(22)=0,
   dfile(23)='gsndrbufr', dtype(23)='sndr',      dplat(23)='g12',     dsis(23)='sndr_g12',           dval(23)=0.0, dthin(23)=1, dsfcalc(23)=0,
   dfile(24)='gimgrbufr', dtype(24)='goes_img',  dplat(24)='g11',     dsis(24)='imgr_g11',           dval(24)=0.0, dthin(24)=1, dsfcalc(24)=0,
   dfile(25)='gimgrbufr', dtype(25)='goes_img',  dplat(25)='g12',     dsis(25)='imgr_g12',           dval(25)=0.0, dthin(25)=1, dsfcalc(25)=0,
   dfile(26)='airsbufr',  dtype(26)='airs',      dplat(26)='aqua',    dsis(26)='airs281SUBSET_aqua', dval(26)=20.0,dthin(26)=1, dsfcalc(26)=1,
   dfile(27)='msubufr',   dtype(27)='msu',       dplat(27)='n14',     dsis(27)='msu_n14',            dval(27)=2.0, dthin(27)=2, dsfcalc(27)=1,
   dfile(28)='amsuabufr', dtype(28)='amsua',     dplat(28)='n15',     dsis(28)='amsua_n15',          dval(28)=10.0,dthin(28)=2, dsfcalc(28)=1,
   dfile(29)='amsuabufr', dtype(29)='amsua',     dplat(29)='n16',     dsis(29)='amsua_n16',          dval(29)=0.0, dthin(29)=2, dsfcalc(29)=1,
   dfile(30)='amsuabufr', dtype(30)='amsua',     dplat(30)='n17',     dsis(30)='amsua_n17',          dval(30)=0.0, dthin(30)=2, dsfcalc(30)=1,
   dfile(31)='amsuabufr', dtype(31)='amsua',     dplat(31)='n18',     dsis(31)='amsua_n18',          dval(31)=10.0,dthin(31)=2, dsfcalc(31)=1,
   dfile(32)='amsuabufr', dtype(32)='amsua',     dplat(32)='metop-a', dsis(32)='amsua_metop-a',      dval(32)=10.0,dthin(32)=2, dsfcalc(32)=1,
   dfile(33)='airsbufr',  dtype(33)='amsua',     dplat(33)='aqua',    dsis(33)='amsua_aqua',         dval(33)=5.0, dthin(33)=2, dsfcalc(33)=1,
   dfile(34)='amsubbufr', dtype(34)='amsub',     dplat(34)='n15',     dsis(34)='amsub_n15',          dval(34)=3.0, dthin(34)=3, dsfcalc(34)=1,
   dfile(35)='amsubbufr', dtype(35)='amsub',     dplat(35)='n16',     dsis(35)='amsub_n16',          dval(35)=3.0, dthin(35)=3, dsfcalc(35)=1,
   dfile(36)='amsubbufr', dtype(36)='amsub',     dplat(36)='n17',     dsis(36)='amsub_n17',          dval(36)=3.0, dthin(36)=3, dsfcalc(36)=1,
   dfile(37)='mhsbufr',   dtype(37)='mhs',       dplat(37)='n18',     dsis(37)='mhs_n18',            dval(37)=3.0, dthin(37)=3, dsfcalc(37)=1,
   dfile(38)='mhsbufr',   dtype(38)='mhs',       dplat(38)='metop-a', dsis(38)='mhs_metop-a',        dval(38)=3.0, dthin(38)=3, dsfcalc(38)=1,
   dfile(39)='ssmitbufr', dtype(39)='ssmi',      dplat(39)='f13',     dsis(39)='ssmi_f13',           dval(39)=0.0, dthin(39)=4, dsfcalc(39)=0,
   dfile(40)='ssmitbufr', dtype(40)='ssmi',      dplat(40)='f14',     dsis(40)='ssmi_f14',           dval(40)=0.0, dthin(40)=4, dsfcalc(40)=0,
   dfile(41)='ssmitbufr', dtype(41)='ssmi',      dplat(41)='f15',     dsis(41)='ssmi_f15',           dval(41)=0.0, dthin(41)=4, dsfcalc(41)=0,
   dfile(42)='amsrebufr', dtype(42)='amsre_low', dplat(42)='aqua',    dsis(42)='amsre_aqua',         dval(42)=0.0, dthin(42)=4, dsfcalc(42)=1,
   dfile(43)='amsrebufr', dtype(43)='amsre_mid', dplat(43)='aqua',    dsis(43)='amsre_aqua',         dval(43)=0.0, dthin(43)=4, dsfcalc(43)=1,
   dfile(44)='amsrebufr', dtype(44)='amsre_hig', dplat(44)='aqua',    dsis(44)='amsre_aqua',         dval(44)=0.0, dthin(44)=4, dsfcalc(44)=1,
   dfile(45)='ssmisbufr', dtype(45)='ssmis',     dplat(45)='f16',     dsis(45)='ssmis_f16',          dval(45)=0.0, dthin(45)=4, dsfcalc(45)=1,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd1',    dplat(46)='g12',     dsis(46)='sndrD1_g12',         dval(46)=1.5, dthin(46)=5, dsfcalc(46)=0,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd2',    dplat(47)='g12',     dsis(47)='sndrD2_g12',         dval(47)=1.5, dthin(47)=5, dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd3',    dplat(48)='g12',     dsis(48)='sndrD3_g12',         dval(48)=1.5, dthin(48)=5, dsfcalc(48)=0,
   dfile(49)='gsnd1bufr', dtype(49)='sndrd4',    dplat(49)='g12',     dsis(49)='sndrD4_g12',         dval(49)=1.5, dthin(49)=5, dsfcalc(49)=0,
   dfile(50)='gsnd1bufr', dtype(50)='sndrd1',    dplat(50)='g11',     dsis(50)='sndrD1_g11',         dval(50)=1.5, dthin(50)=5, dsfcalc(50)=0,
   dfile(51)='gsnd1bufr', dtype(51)='sndrd2',    dplat(51)='g11',     dsis(51)='sndrD2_g11',         dval(51)=1.5, dthin(51)=5, dsfcalc(51)=0,
   dfile(52)='gsnd1bufr', dtype(52)='sndrd3',    dplat(52)='g11',     dsis(52)='sndrD3_g11',         dval(52)=1.5, dthin(52)=5, dsfcalc(52)=0,
   dfile(53)='gsnd1bufr', dtype(53)='sndrd4',    dplat(53)='g11',     dsis(53)='sndrD4_g11',         dval(53)=1.5, dthin(53)=5, dsfcalc(53)=0,
   dfile(54)='gsnd1bufr', dtype(54)='sndrd1',    dplat(54)='g13',     dsis(54)='sndrD1_g13',         dval(54)=1.5, dthin(54)=5, dsfcalc(54)=0,
   dfile(55)='gsnd1bufr', dtype(55)='sndrd2',    dplat(55)='g13',     dsis(55)='sndrD2_g13',         dval(55)=1.5, dthin(55)=5, dsfcalc(55)=0,
   dfile(56)='gsnd1bufr', dtype(56)='sndrd3',    dplat(56)='g13',     dsis(56)='sndrD3_g13',         dval(56)=1.5, dthin(56)=5, dsfcalc(56)=0,
   dfile(57)='gsnd1bufr', dtype(57)='sndrd4',    dplat(57)='g13',     dsis(57)='sndrD4_g13',         dval(57)=1.5, dthin(57)=5, dsfcalc(57)=0,
   dfile(58)='iasibufr',  dtype(58)='iasi',      dplat(58)='metop-a', dsis(58)='iasi616_metop-a',    dval(58)=20.0,dthin(58)=1, dsfcalc(58)=1,
   dfile(59)='gomebufr',  dtype(59)='gome',      dplat(59)='metop-a', dsis(59)='gome_metop-a',       dval(59)=1.0, dthin(59)=6, dsfcalc(59)=0,
   dfile(60)='omibufr',   dtype(60)='omi',       dplat(60)='aura',    dsis(60)='omi_aura',           dval(60)=1.0, dthin(60)=6, dsfcalc(60)=0,
   dfile(61)='sbuvbufr',  dtype(61)='sbuv2',     dplat(61)='n19',     dsis(61)='sbuv8_n19',          dval(61)=1.0, dthin(61)=0, dsfcalc(61)=0,
   dfile(62)='hirs4bufr', dtype(62)='hirs4',     dplat(62)='n19',     dsis(62)='hirs4_n19',          dval(62)=6.0, dthin(62)=1, dsfcalc(62)=1,
   dfile(63)='amsuabufr', dtype(63)='amsua',     dplat(63)='n19',     dsis(63)='amsua_n19',          dval(63)=10.0,dthin(63)=2, dsfcalc(63)=1,
   dfile(64)='mhsbufr',   dtype(64)='mhs',       dplat(64)='n19',     dsis(64)='mhs_n19',            dval(64)=3.0, dthin(64)=3, dsfcalc(64)=1,
   dfile(65)='tcvitl'     dtype(65)='tcp',       dplat(65)=' ',       dsis(65)='tcp',                dval(65)=1.0, dthin(65)=0, dsfcalc(65)=0,
   dfile(66)='mlsbufr',   dtype(66)='mls',       dplat(66)='aura',    dsis(66)='mls_aura',           dval(66)=1.0, dthin(66)=0, dsfcalc(66)=0,
   $OBSINPUT
/
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
   $SUPERRAD
 /
 &LAG_DATA
   $LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${HYBENS_REGIONAL},
   n_ens=${ENSEMBLE_SIZE_REGIONAL},
   uv_hyb_ens=${HYBENS_UV_REGIONAL},
   beta1_inv=${BETA1_INV_REGIONAL},
   s_ens_h=${HYBENS_HOR_SCALE_REGIONAL},
   s_ens_v=${HYBENS_VER_SCALE_REGIONAL},
   generate_ens=${GENERATE_ENS_REGIONAL},
   aniso_a_en=${HYBENS_ANISO_REGIONAL},
   nlon_ens=${NLON_ENS_REGIONAL},
   nlat_ens=${NLAT_ENS_REGIONAL},
   jcap_ens=${JCAP_ENS_REGIONAL},
   jcap_ens_test=${JCAP_ENS_TEST_REGIONAL},
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${nmmb_nems_adate},
   obhourset=0.,
   $SINGLEOB_TEST
 /
EOF

anavinfo=$fixgsi/anavinfo_nems_nmmb
berror=$fixgsi/nam_glb_berror.f77.gcv
emiscoef=$fixcrtm/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fixgsi/nam_regional_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/nam_global_satangbias.txt
atmsbeamdat=$fixgsi/atms_beamwidth.txt
pcpinfo=$fixgsi/nam_global_pcpinfo.txt
ozinfo=$fixgsi/nam_global_ozinfo.txt
errtable=$fixgsi/nam_errtable.r3dv
convinfo=$fixgsi/nam_regional_convinfo.txt
mesonetuselist=$fixgsi/nam_mesonet_uselist.txt
stnuselist=$fixgsi/nam_mesonet_stnuselist.txt

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $atmsbeamdat  ./atms_beamwidth.txt
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist
$ncp $stnuselist ./mesonet_stnuselist   

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $nmmb_nems_obs/ndas.t12z.prepbufr.tm12      ./prepbufr
$ncp $nmmb_nems_obs/ndas.t12z.satwnd.tm12.bufr_d ./satwnd
$ncp $nmmb_nems_obs/ndas.t12z.1bhrs3.tm12.bufr_d ./hirs3bufr
$ncp $nmmb_nems_obs/ndas.t12z.1bhrs4.tm12.bufr_d ./hirs4bufr
$ncp $nmmb_nems_obs/ndas.t12z.1bamua.tm12.bufr_d ./amsuabufr
$ncp $nmmb_nems_obs/ndas.t12z.1bamub.tm12.bufr_d ./amsubbufr
$ncp $nmmb_nems_obs/ndas.t12z.1bmhs.tm12.bufr_d  ./mhsbufr
$ncp $nmmb_nems_obs/ndas.t12z.goesfv.tm12.bufr_d ./gsnd1bufr
$ncp $nmmb_nems_obs/ndas.t12z.airsev.tm12.bufr_d ./airsbufr
$ncp $nmmb_nems_obs/ndas.t12z.radwnd.tm12.bufr_d ./radarbufr
$ncp $nmmb_nems_obs/gdas1.t00z.osbuv8.tm00.bufr_d ./sbuvbufr
$ncp $nmmb_nems_obs/gdas1.t00z.mls.tm00.bufr_d ./mlsbufr

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $nmmb_nems_ges/ndas.t06z.satang.tm09 ./satbias_angle
$ncp $nmmb_nems_ges/ndas.t06z.satbias.tm09 ./satbias_in

$ncp $nmmb_nems_ges/nmm_b_history_nemsio.012 ./wrf_inout
$ncp $nmmb_nems_ges/nmm_b_history_nemsio.012.ctl ./wrf_inout.ctl
$ncp $nmmb_nems_ges/gdas1.t00z.sgesprep  ./gfs_sigf03
$ncp wrf_inout wrf_ges
$ncp wrf_inout.ctl wrf_ges.ctl

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$arch" = "Linux" ]]; then
   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   #export module="/usr/bin/modulecmd sh"

   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"

elif [[ "$arch" = "AIX" ]]; then

   poe $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${nmmb_nems_adate}
$ncp wrf_inout       $savdir/wrfanl.${nmmb_nems_adate}
$ncp satbias_out     $savdir/biascr.${nmmb_nems_adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${nmmb_nems_adate}

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

cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

# Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g10 imgr_g12 pcp_ssmi_dmsp
pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${nmmb_nems_adate}
         compress diag_${type}_${string}.${nmmb_nems_adate}
         $ncp diag_${type}_${string}.${nmmb_nems_adate}.Z $savdir/
      fi
   done
done

exit
