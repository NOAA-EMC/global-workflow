
set -x

# Set analysis date
#adate=$adate_regional

# Set guess/analysis (i/o) file format.  Two
# option are available:  binary or netcdf
##io_format=binary
io_format=netcdf

if [[ "$io_format" = "binary" ]]; then
   NETCDF=.false.
   FORMAT=binary
elif [[ "$io_format" = "netcdf" ]]; then
   NETCDF=.true.
   FORMAT=netcdf
else
   echo "***ERRROR*** INVALID io_format = $io_format"
   exit
fi

# Set experiment name

if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#gsiexec=$updat

# Set resoltion and other dependent parameters
#export JCAP=62
export LEVS=60
export JCAP_B=$JCAP
if [[ "$io_format" = "binary" ]]; then
   export LEVS=50
elif [[ "$io_format" = "netcdf" ]]; then
   export LEVS=30
fi
export DELTIM=1200

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_arw_netcdf/${exp}
savdir=$savdir/outreg/arw_netcdf/${exp}

# Specify GSI fixed field and data directories.


# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
sdate=`echo $arw_netcdf_adate |cut -c1-8`
odate=`$ndate +6 $arw_netcdf_adate`
hha=`echo $arw_netcdf_adate | cut -c9-10`
hho=`echo $odate | cut -c9-10`
prefixo=ndas.t${hho}z
prefixa=ndas.t${hha}z
suffix=tm06.bufr_d

#datobs=$datobs_arw_netcdf/$adate_regional_arw_netcdf
#datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
	# Copy co2 files to $tmpdir
	co2dir=${CO2DIR:-$fixgsi}
	yyyy=$(echo ${CDATE:-$arw_netcdf_adate}|cut -c1-4)
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
cat << EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=61,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.true.,diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=$NETCDF,
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
   jcstrong=.false.,jcstrong_option=3,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,dmesh(5)=120,time_window_max=1.5,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=1.0,  dthin(01)=0, dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=1.0,  dthin(02)=0, dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=1.0,  dthin(03)=0, dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=1.0,  dthin(04)=0, dsfcalc(04)=0,
   dfile(05)='satwnd',    dtype(05)='uv',        dplat(05)=' ',         dsis(05)='uv',                  dval(05)=1.0,  dthin(05)=0, dsfcalc(05)=0,
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
   dfile(61)='mlsbufr',   dtype(61)='mls',       dplat(61)='aura',      dsis(61)='mls_aura',            dval(61)=1.0,  dthin(61)=0, dsfcalc(61)=0,
 /
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
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
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${arw_netcdf_adate},
   obhourset=0.,
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
#   atmsbeamdat  =  data required for atms spatial averaging
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fixgsi/anavinfo_arw_netcdf
if [[ "$io_format" = "binary" ]]; then
   berror=$fixgsi/$endianness/nam_glb_berror.f77.gcv
elif [[ "$io_format" = "netcdf" ]]; then
   berror=$fixgsi/$endianness/nam_glb_berror.f77.gcv
fi
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

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ "$exp" = $arw_netcdf_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $arw_netcdf_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $arw_netcdf_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ "$exp" = $arw_netcdf_contrl_exp2 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

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
         $ncp $fixcrtm/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fixcrtm/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $arw_netcdf_obs/${prefixo}.prepbufr.tm06   ./prepbufr
$ncp $arw_netcdf_obs/${prefixo}.satwnd.$suffix  ./satwnd
$ncp $arw_netcdf_obs/${prefixo}.1bhrs3.$suffix  ./hirs3bufr
$ncp $arw_netcdf_obs/${prefixo}.1bhrs4.$suffix  ./hirs4bufr
$ncp $arw_netcdf_obs/${prefixo}.1bamua.$suffix  ./amsuabufr
$ncp $arw_netcdf_obs/${prefixo}.1bamub.$suffix  ./amsubbufr
$ncp $arw_netcdf_obs/${prefixo}.1bmhs.$suffix   ./mhsbufr
$ncp $arw_netcdf_obs/${prefixo}.goesfv.$suffix  ./gsnd1bufr
$ncp $arw_netcdf_obs/${prefixo}.airsev.$suffix  ./airsbufr
$ncp $arw_netcdf_obs/${prefixo}.radwnd.$suffix  ./radarbufr
if [[ "$io_format" = "netcdf" ]]; then
   $ncp $arw_netcdf_obs/${prefixo}.nexrad.$suffix  ./l2rwbufr
fi

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $arw_netcdf_obs/${prefixo}.satbias.tm06      ./satbias_in
$ncp $arw_netcdf_obs/${prefixo}.satang.tm06        ./satbias_angle
if [[ "$io_format" = "binary" ]]; then
   $ncp $arw_netcdf_ges/wrfinput_d01_arw_binary        ./wrf_inout
elif [[ "$io_format" = "netcdf" ]]; then
   $ncp $arw_netcdf_ges/wrfout_d01_2008-05-11_12:00:00 ./wrf_inout
fi
cp wrf_inout wrf_ges

if [[ "$arch" = "Linux" ]]; then

   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"

elif [[ "$arch" = "AIX" ]]; then

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
   poe $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${arw_netcdf_adate}
$ncp wrf_inout       $savdir/wrfanl.${arw_netcdf_adate}
$ncp satbias_out     $savdir/biascr.${arw_netcdf_adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${arw_netcdf_adate}

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
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g10 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${arw_netcdf_adate}
         compress diag_${type}_${string}.${arw_netcdf_adate}
         $ncp diag_${type}_${string}.${arw_netcdf_adate}.Z $savdir/
      fi
   done
done

exit
