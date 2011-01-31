# Define namelist for global run (pcgsoi minimization)

export global_T62_namelist=" 

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,
   niter_no_qc(1)=1,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=82,qoption=2,
   factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=66,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcpdry=.true.,bamp_jcpdry=2.5e7,
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
   dmesh(1)=1450.0,dmesh(2)=1500.0,dmesh(3)=5000.0,dmesh(4)=10000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                dval(01)=0.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                 dval(02)=0.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                 dval(03)=0.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',       dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                dval(04)=0.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',       dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                dval(05)=0.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='prepbufr',       dtype(06)='spd',       dplat(06)=' ',       dsis(06)='spd',               dval(06)=0.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',       dsis(07)='dw',                dval(07)=0.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='radarbufr',      dtype(08)='rw',        dplat(08)=' ',       dsis(08)='rw',                dval(08)=0.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='prepbufr',       dtype(09)='sst',       dplat(09)=' ',       dsis(09)='sst',               dval(09)=0.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',       dsis(10)='gps_ref',           dval(10)=0.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',    dsis(11)='pcp_ssmi',          dval(11)=0.0,  dthin(11)=-1, dsfcalc(11)=0,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',    dsis(12)='pcp_tmi',           dval(12)=0.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',         dval(13)=0.0,  dthin(13)=0,  dsfcalc(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',         dval(14)=0.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',         dval(15)=0.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='hirs2bufr',      dtype(16)='hirs2',     dplat(16)='n14',     dsis(16)='hirs2_n14',         dval(16)=0.0,  dthin(16)=1,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr_skip', dtype(17)='hirs3',     dplat(17)='n16',     dsis(17)='hirs3_n16',         dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=0,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n17',     dsis(18)='hirs3_n17',         dval(18)=0.0,  dthin(18)=1,  dsfcalc(18)=0,
   dfile(19)='hirs4bufr_skip', dtype(19)='hirs4',     dplat(19)='n18',     dsis(19)='hirs4_n18',         dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='hirs4bufr_skip', dtype(20)='hirs4',     dplat(20)='metop-a', dsis(20)='hirs4_metop-a',     dval(20)=0.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='gsndrbufr',      dtype(21)='sndr',      dplat(21)='g11',     dsis(21)='sndr_g11',          dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=0,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g12',     dsis(22)='sndr_g12',          dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=0,
   dfile(23)='gimgrbufr',      dtype(23)='goes_img',  dplat(23)='g11',     dsis(23)='imgr_g11',          dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=0,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g12',     dsis(24)='imgr_g12',          dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=0,
   dfile(25)='airsbufr',       dtype(25)='airs',      dplat(25)='aqua',    dsis(25)='airs281SUBSET_aqua',dval(25)=0.0,  dthin(25)=3,  dsfcalc(25)=0,
   dfile(26)='msubufr',        dtype(26)='msu',       dplat(26)='n14',     dsis(26)='msu_n14',           dval(26)=0.0,  dthin(26)=1,  dsfcalc(26)=0,
   dfile(27)='amsuabufr_skip', dtype(27)='amsua',     dplat(27)='n15',     dsis(27)='amsua_n15',         dval(27)=0.0,  dthin(27)=1,  dsfcalc(27)=0,
   dfile(28)='amsuabufr_skip', dtype(28)='amsua',     dplat(28)='n16',     dsis(28)='amsua_n16',         dval(28)=0.0,  dthin(28)=1,  dsfcalc(28)=0,
   dfile(29)='amsuabufr_skip', dtype(29)='amsua',     dplat(29)='n17',     dsis(29)='amsua_n17',         dval(29)=0.0,  dthin(29)=1,  dsfcalc(29)=0,
   dfile(30)='amsuabufr_skip', dtype(30)='amsua',     dplat(30)='n18',     dsis(30)='amsua_n18',         dval(30)=0.0,  dthin(30)=1,  dsfcalc(30)=0,
   dfile(31)='amsuabufr_skip', dtype(31)='amsua',     dplat(31)='metop-a', dsis(31)='amsua_metop-a',     dval(31)=0.0,  dthin(31)=1,  dsfcalc(31)=0,
   dfile(32)='airsbufr_skip',  dtype(32)='amsua',     dplat(32)='aqua',    dsis(32)='amsua_aqua',        dval(32)=0.0,  dthin(32)=1,  dsfcalc(32)=0,
   dfile(33)='amsubbufr_skip', dtype(33)='amsub',     dplat(33)='n15',     dsis(33)='amsub_n15',         dval(33)=0.0,  dthin(33)=1,  dsfcalc(33)=0,
   dfile(34)='amsubbufr_skip', dtype(34)='amsub',     dplat(34)='n16',     dsis(34)='amsub_n16',         dval(34)=0.0,  dthin(34)=1,  dsfcalc(34)=0,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n17',     dsis(35)='amsub_n17',         dval(35)=0.0,  dthin(35)=1,  dsfcalc(35)=0,
   dfile(36)='mhsbufr_skip',   dtype(36)='mhs',       dplat(36)='n18',     dsis(36)='mhs_n18',           dval(36)=0.0,  dthin(36)=1,  dsfcalc(36)=0,
   dfile(37)='mhsbufr_skip',   dtype(37)='mhs',       dplat(37)='metop-a', dsis(37)='mhs_metop-a',       dval(37)=0.0,  dthin(37)=1,  dsfcalc(37)=0,
   dfile(38)='ssmitbufr',      dtype(38)='ssmi',      dplat(38)='f13',     dsis(38)='ssmi_f13',          dval(38)=0.0,  dthin(38)=1,  dsfcalc(38)=0,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f14',     dsis(39)='ssmi_f14',          dval(39)=0.0,  dthin(39)=1,  dsfcalc(39)=0,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f15',     dsis(40)='ssmi_f15',          dval(40)=0.0,  dthin(40)=1,  dsfcalc(40)=0,
   dfile(41)='amsrebufr',      dtype(41)='amsre_low', dplat(41)='aqua',    dsis(41)='amsre_aqua',        dval(41)=0.0,  dthin(41)=1,  dsfcalc(41)=0,
   dfile(42)='amsrebufr',      dtype(42)='amsre_mid', dplat(42)='aqua',    dsis(42)='amsre_aqua',        dval(42)=0.0,  dthin(42)=1,  dsfcalc(42)=0,
   dfile(43)='amsrebufr',      dtype(43)='amsre_hig', dplat(43)='aqua',    dsis(43)='amsre_aqua',        dval(43)=0.0,  dthin(43)=1,  dsfcalc(43)=0,
   dfile(44)='ssmisbufr',      dtype(44)='ssmis',     dplat(44)='f16',     dsis(44)='ssmis_f16',         dval(44)=0.0,  dthin(44)=1,  dsfcalc(44)=0,
   dfile(45)='gsnd1bufr_skip', dtype(45)='sndrd1',    dplat(45)='g12',     dsis(45)='sndrD1_g12',        dval(45)=0.0,  dthin(45)=1,  dsfcalc(45)=0,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd2',    dplat(46)='g12',     dsis(46)='sndrD2_g12',        dval(46)=0.0,  dthin(46)=1,  dsfcalc(46)=0,
   dfile(47)='gsnd1bufr_skip', dtype(47)='sndrd3',    dplat(47)='g12',     dsis(47)='sndrD3_g12',        dval(47)=0.0,  dthin(47)=1,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd4',    dplat(48)='g12',     dsis(48)='sndrD4_g12',        dval(48)=0.0,  dthin(48)=1,  dsfcalc(48)=0,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd1',    dplat(49)='g11',     dsis(49)='sndrD1_g11',        dval(49)=0.0,  dthin(49)=1,  dsfcalc(49)=0,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd2',    dplat(50)='g11',     dsis(50)='sndrD2_g11',        dval(50)=0.0,  dthin(50)=1,  dsfcalc(50)=0,
   dfile(51)='gsnd1bufr',      dtype(51)='sndrd3',    dplat(51)='g11',     dsis(51)='sndrD3_g11',        dval(51)=0.0,  dthin(51)=1,  dsfcalc(51)=0,
   dfile(52)='gsnd1bufr_skip', dtype(52)='sndrd4',    dplat(52)='g11',     dsis(52)='sndrD4_g11',        dval(52)=0.0,  dthin(52)=1,  dsfcalc(52)=0,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd1',    dplat(53)='g13',     dsis(53)='sndrD1_g13',        dval(53)=0.0,  dthin(53)=1,  dsfcalc(53)=0,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd2',    dplat(54)='g13',     dsis(54)='sndrD2_g13',        dval(54)=0.0,  dthin(54)=1,  dsfcalc(54)=0,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd3',    dplat(55)='g13',     dsis(55)='sndrD3_g13',        dval(55)=0.0,  dthin(55)=1,  dsfcalc(55)=0,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd4',    dplat(56)='g13',     dsis(56)='sndrD4_g13',        dval(56)=0.0,  dthin(56)=1,  dsfcalc(56)=0,
   dfile(57)='iasibufr',       dtype(57)='iasi',      dplat(57)='metop-a', dsis(57)='iasi616_metop-a',   dval(57)=0.0,  dthin(57)=4,  dsfcalc(57)=0,
   dfile(58)='gomebufr',       dtype(58)='gome',      dplat(58)='metop-a', dsis(58)='gome_metop-a',      dval(58)=0.0,  dthin(58)=2,  dsfcalc(58)=0,
   dfile(59)='omibufr',        dtype(59)='omi',       dplat(59)='aura',    dsis(59)='omi_aura',          dval(59)=0.0,  dthin(59)=2,  dsfcalc(59)=0,
   dfile(60)='sbuvbufr',       dtype(60)='sbuv2',     dplat(60)='n19',     dsis(60)='sbuv8_n19',         dval(60)=0.0,  dthin(60)=0,  dsfcalc(60)=0,
   dfile(61)='hirs4bufr',      dtype(61)='hirs4',     dplat(61)='n19',     dsis(61)='hirs4_n19',         dval(61)=0.0,  dthin(61)=1,  dsfcalc(61)=0,
   dfile(62)='amsuabufr',      dtype(62)='amsua',     dplat(62)='n19',     dsis(62)='amsua_n19',         dval(62)=0.0,  dthin(62)=1,  dsfcalc(62)=0,
   dfile(63)='mhsbufr',        dtype(63)='mhs',       dplat(63)='n19',     dsis(63)='mhs_n19',           dval(63)=0.0,  dthin(63)=1,  dsfcalc(63)=0,
   dfile(64)='tcvitl'          dtype(64)='tcp',       dplat(64)=' ',       dsis(64)='tcp',               dval(64)=0.0,  dthin(64)=0,  dsfcalc(64)=0,
   dfile(65)='modisbufr',      dtype(65)='modis',     dplat(65)='aqua',    dsis(65)='modis_aqua',        dval(65)=0.0,  dthin(65)=1,  dsfcalc(65)=0,
   dfile(66)='modisbufr',      dtype(66)='modis',     dplat(66)='terra',   dsis(66)='modis_terra',       dval(66)=0.0,  dthin(66)=1,  dsfcalc(66)=0,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${HYBENS_GLOBAL},
   n_ens=${ENSEMBLE_SIZE_GLOBAL},
   uv_hyb_ens=${HYBENS_UV_GLOBAL},
   beta1_inv=${BETA1_INV_GLOBAL},
   s_ens_h=${HYBENS_HOR_SCALE_GLOBAL},
   s_ens_v=${HYBENS_VER_SCALE_GLOBAL},
   generate_ens=${GENERATE_ENS_GLOBAL},
   aniso_a_en=${HYBENS_ANISO_GLOBAL},
   nlon_ens=${LONA},
   nlat_ens=${NLAT},
   jcap_ens=${JCAP},
   jcap_ens_test=${JCAP},
 /
 &RAPIDREFRESH_CLDSURF
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB
 /"

# Define namelist for global run (lanczos minimization)

export global_lanczos_T62_namelist=" 

 &SETUP
   miter=2,niter(1)=10,niter(2)=5,
   niter_no_qc(1)=500,niter_no_qc(2)=500,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=82,qoption=2,
   factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=66,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   lsqrtb=.true.,lcongrad=.true.,ltlint=.true.,ladtest=.true.,lgrtest=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcpdry=.false.,bamp_jcpdry=2.5e7,
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,dmesh(3)=5000.0,dmesh(4)=10000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                dval(01)=0.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                 dval(02)=0.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                 dval(03)=0.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',       dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                dval(04)=0.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',       dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                dval(05)=0.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='prepbufr',       dtype(06)='spd',       dplat(06)=' ',       dsis(06)='spd',               dval(06)=0.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',       dsis(07)='dw',                dval(07)=0.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='radarbufr',      dtype(08)='rw',        dplat(08)=' ',       dsis(08)='rw',                dval(08)=0.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='prepbufr',       dtype(09)='sst',       dplat(09)=' ',       dsis(09)='sst',               dval(09)=0.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',       dsis(10)='gps_ref',           dval(10)=0.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',    dsis(11)='pcp_ssmi',          dval(11)=0.0,  dthin(11)=-1, dsfcalc(11)=0,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',    dsis(12)='pcp_tmi',           dval(12)=0.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',         dval(13)=0.0,  dthin(13)=0,  dsfcalc(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',         dval(14)=0.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',         dval(15)=0.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='hirs2bufr',      dtype(16)='hirs2',     dplat(16)='n14',     dsis(16)='hirs2_n14',         dval(16)=0.0,  dthin(16)=1,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr_skip', dtype(17)='hirs3',     dplat(17)='n16',     dsis(17)='hirs3_n16',         dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=0,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n17',     dsis(18)='hirs3_n17',         dval(18)=0.0,  dthin(18)=1,  dsfcalc(18)=0,
   dfile(19)='hirs4bufr_skip', dtype(19)='hirs4',     dplat(19)='n18',     dsis(19)='hirs4_n18',         dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='hirs4bufr_skip', dtype(20)='hirs4',     dplat(20)='metop-a', dsis(20)='hirs4_metop-a',     dval(20)=0.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='gsndrbufr',      dtype(21)='sndr',      dplat(21)='g11',     dsis(21)='sndr_g11',          dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=0,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g12',     dsis(22)='sndr_g12',          dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=0,
   dfile(23)='gimgrbufr',      dtype(23)='goes_img',  dplat(23)='g11',     dsis(23)='imgr_g11',          dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=0,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g12',     dsis(24)='imgr_g12',          dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=0,
   dfile(25)='airsbufr',       dtype(25)='airs',      dplat(25)='aqua',    dsis(25)='airs281SUBSET_aqua',dval(25)=0.0,  dthin(25)=3,  dsfcalc(25)=0,
   dfile(26)='msubufr',        dtype(26)='msu',       dplat(26)='n14',     dsis(26)='msu_n14',           dval(26)=0.0,  dthin(26)=1,  dsfcalc(26)=0,
   dfile(27)='amsuabufr_skip', dtype(27)='amsua',     dplat(27)='n15',     dsis(27)='amsua_n15',         dval(27)=0.0,  dthin(27)=1,  dsfcalc(27)=0,
   dfile(28)='amsuabufr_skip', dtype(28)='amsua',     dplat(28)='n16',     dsis(28)='amsua_n16',         dval(28)=0.0,  dthin(28)=1,  dsfcalc(28)=0,
   dfile(29)='amsuabufr_skip', dtype(29)='amsua',     dplat(29)='n17',     dsis(29)='amsua_n17',         dval(29)=0.0,  dthin(29)=1,  dsfcalc(29)=0,
   dfile(30)='amsuabufr_skip', dtype(30)='amsua',     dplat(30)='n18',     dsis(30)='amsua_n18',         dval(30)=0.0,  dthin(30)=1,  dsfcalc(30)=0,
   dfile(31)='amsuabufr_skip', dtype(31)='amsua',     dplat(31)='metop-a', dsis(31)='amsua_metop-a',     dval(31)=0.0,  dthin(31)=1,  dsfcalc(31)=0,
   dfile(32)='airsbufr_skip',  dtype(32)='amsua',     dplat(32)='aqua',    dsis(32)='amsua_aqua',        dval(32)=0.0,  dthin(32)=1,  dsfcalc(32)=0,
   dfile(33)='amsubbufr_skip', dtype(33)='amsub',     dplat(33)='n15',     dsis(33)='amsub_n15',         dval(33)=0.0,  dthin(33)=1,  dsfcalc(33)=0,
   dfile(34)='amsubbufr_skip', dtype(34)='amsub',     dplat(34)='n16',     dsis(34)='amsub_n16',         dval(34)=0.0,  dthin(34)=1,  dsfcalc(34)=0,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n17',     dsis(35)='amsub_n17',         dval(35)=0.0,  dthin(35)=1,  dsfcalc(35)=0,
   dfile(36)='mhsbufr_skip',   dtype(36)='mhs',       dplat(36)='n18',     dsis(36)='mhs_n18',           dval(36)=0.0,  dthin(36)=1,  dsfcalc(36)=0,
   dfile(37)='mhsbufr_skip',   dtype(37)='mhs',       dplat(37)='metop-a', dsis(37)='mhs_metop-a',       dval(37)=0.0,  dthin(37)=1,  dsfcalc(37)=0,
   dfile(38)='ssmitbufr',      dtype(38)='ssmi',      dplat(38)='f13',     dsis(38)='ssmi_f13',          dval(38)=0.0,  dthin(38)=1,  dsfcalc(38)=0,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f14',     dsis(39)='ssmi_f14',          dval(39)=0.0,  dthin(39)=1,  dsfcalc(39)=0,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f15',     dsis(40)='ssmi_f15',          dval(40)=0.0,  dthin(40)=1,  dsfcalc(40)=0,
   dfile(41)='amsrebufr',      dtype(41)='amsre_low', dplat(41)='aqua',    dsis(41)='amsre_aqua',        dval(41)=0.0,  dthin(41)=1,  dsfcalc(41)=0,
   dfile(42)='amsrebufr',      dtype(42)='amsre_mid', dplat(42)='aqua',    dsis(42)='amsre_aqua',        dval(42)=0.0,  dthin(42)=1,  dsfcalc(42)=0,
   dfile(43)='amsrebufr',      dtype(43)='amsre_hig', dplat(43)='aqua',    dsis(43)='amsre_aqua',        dval(43)=0.0,  dthin(43)=1,  dsfcalc(43)=0,
   dfile(44)='ssmisbufr',      dtype(44)='ssmis',     dplat(44)='f16',     dsis(44)='ssmis_f16',         dval(44)=0.0,  dthin(44)=1,  dsfcalc(44)=0,
   dfile(45)='gsnd1bufr_skip', dtype(45)='sndrd1',    dplat(45)='g12',     dsis(45)='sndrD1_g12',        dval(45)=0.0,  dthin(45)=1,  dsfcalc(45)=0,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd2',    dplat(46)='g12',     dsis(46)='sndrD2_g12',        dval(46)=0.0,  dthin(46)=1,  dsfcalc(46)=0,
   dfile(47)='gsnd1bufr_skip', dtype(47)='sndrd3',    dplat(47)='g12',     dsis(47)='sndrD3_g12',        dval(47)=0.0,  dthin(47)=1,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd4',    dplat(48)='g12',     dsis(48)='sndrD4_g12',        dval(48)=0.0,  dthin(48)=1,  dsfcalc(48)=0,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd1',    dplat(49)='g11',     dsis(49)='sndrD1_g11',        dval(49)=0.0,  dthin(49)=1,  dsfcalc(49)=0,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd2',    dplat(50)='g11',     dsis(50)='sndrD2_g11',        dval(50)=0.0,  dthin(50)=1,  dsfcalc(50)=0,
   dfile(51)='gsnd1bufr',      dtype(51)='sndrd3',    dplat(51)='g11',     dsis(51)='sndrD3_g11',        dval(51)=0.0,  dthin(51)=1,  dsfcalc(51)=0,
   dfile(52)='gsnd1bufr_skip', dtype(52)='sndrd4',    dplat(52)='g11',     dsis(52)='sndrD4_g11',        dval(52)=0.0,  dthin(52)=1,  dsfcalc(52)=0,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd1',    dplat(53)='g13',     dsis(53)='sndrD1_g13',        dval(53)=0.0,  dthin(53)=1,  dsfcalc(53)=0,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd2',    dplat(54)='g13',     dsis(54)='sndrD2_g13',        dval(54)=0.0,  dthin(54)=1,  dsfcalc(54)=0,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd3',    dplat(55)='g13',     dsis(55)='sndrD3_g13',        dval(55)=0.0,  dthin(55)=1,  dsfcalc(55)=0,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd4',    dplat(56)='g13',     dsis(56)='sndrD4_g13',        dval(56)=0.0,  dthin(56)=1,  dsfcalc(56)=0,
   dfile(57)='iasibufr',       dtype(57)='iasi',      dplat(57)='metop-a', dsis(57)='iasi616_metop-a',   dval(57)=0.0,  dthin(57)=4,  dsfcalc(57)=0,
   dfile(58)='gomebufr',       dtype(58)='gome',      dplat(58)='metop-a', dsis(58)='gome_metop-a',      dval(58)=0.0,  dthin(58)=2,  dsfcalc(58)=0,
   dfile(59)='omibufr',        dtype(59)='omi',       dplat(59)='aura',    dsis(59)='omi_aura',          dval(59)=0.0,  dthin(59)=2,  dsfcalc(59)=0,
   dfile(60)='sbuvbufr',       dtype(60)='sbuv2',     dplat(60)='n19',     dsis(60)='sbuv8_n19',         dval(60)=0.0,  dthin(60)=0,  dsfcalc(60)=0,
   dfile(61)='hirs4bufr',      dtype(61)='hirs4',     dplat(61)='n19',     dsis(61)='hirs4_n19',         dval(61)=0.0,  dthin(61)=1,  dsfcalc(61)=0,
   dfile(62)='amsuabufr',      dtype(62)='amsua',     dplat(62)='n19',     dsis(62)='amsua_n19',         dval(62)=0.0,  dthin(62)=1,  dsfcalc(62)=0,
   dfile(63)='mhsbufr',        dtype(63)='mhs',       dplat(63)='n19',     dsis(63)='mhs_n19',           dval(63)=0.0,  dthin(63)=1,  dsfcalc(63)=0,
   dfile(64)='tcvitl'          dtype(64)='tcp',       dplat(64)=' ',       dsis(64)='tcp',               dval(64)=0.0,  dthin(64)=0,  dsfcalc(64)=0,
   dfile(65)='modisbufr',      dtype(65)='modis',     dplat(65)='aqua',    dsis(65)='modis_aqua',        dval(65)=0.0,  dthin(65)=1,  dsfcalc(65)=0,
   dfile(66)='modisbufr',      dtype(66)='modis',     dplat(66)='terra',   dsis(66)='modis_terra',       dval(66)=0.0,  dthin(66)=1,  dsfcalc(66)=0,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${HYBENS_GLOBAL},
   n_ens=${ENSEMBLE_SIZE_GLOBAL},
   uv_hyb_ens=${HYBENS_UV_GLOBAL},
   beta1_inv=${BETA1_INV_GLOBAL},
   s_ens_h=${HYBENS_HOR_SCALE_GLOBAL},
   s_ens_v=${HYBENS_VER_SCALE_GLOBAL},
   generate_ens=${GENERATE_ENS_GLOBAL},
   aniso_a_en=${HYBENS_ANISO_GLOBAL},
   nlon_ens=${LONA},
   nlat_ens=${NLAT},
   jcap_ens=${JCAP},
   jcap_ens_test=${JCAP},
 /
 &RAPIDREFRESH_CLDSURF
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB

 /"

# Define namelist for RTMA runs

export RTMA_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,
   factqmin=1.0,factqmax=1.0,deltim=$DELTIM,
   ndat=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,diag_aero=.false.,
   nhr_assimilation=3,use_compress=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,twodvar_regional=.true.,
   diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
   hzscl=1.414,1.000,0.707,
   vs=0.5,bw=0.0,
 /
 &ANBKGERR
   anisotropic=.true.,an_vs=0.5,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=0,grid_ratio=2.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   jcstrong=.false.,jcstrong_option=3,nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=600.0,dmesh(2)=600.0,dmesh(3)=600.0,dmesh(4)=600.0,time_window_max=0.5,
   dfile(01)='prepbufr',  dtype(01)='ps',  dplat(01)=' ', dsis(01)='ps',  dval(01)=1.0,  dthin(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',   dplat(02)=' ', dsis(02)='t',   dval(02)=1.0,  dthin(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',   dplat(03)=' ', dsis(03)='q',   dval(03)=1.0,  dthin(03)=0,
   dfile(04)='prepbufr',  dtype(04)='uv',  dplat(04)=' ', dsis(04)='uv',  dval(04)=1.0,  dthin(04)=0,
   dfile(05)='prepbufr',  dtype(05)='spd', dplat(05)=' ', dsis(05)='spd', dval(05)=1.0,  dthin(05)=0,
 /
 &SUPEROB_RADAR
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=36.,oblon=260.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for arw binary run

export arw_binary_namelist="

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=59,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
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
   dmesh(1)=1200.0,dmesh(2)=2000.0,dmesh(3)=2500.0,dmesh(4)=1000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=0.0,  dthin(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=0.0,  dthin(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=0.0,  dthin(03)=0,
   dfile(04)='prepbufr',       dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=0.0,  dthin(04)=0,
   dfile(05)='prepbufr',       dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',                 dval(05)=0.0,  dthin(05)=0,
   dfile(06)='radarbufr',      dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',                  dval(06)=0.0,  dthin(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=0.0,  dthin(07)=0,
   dfile(08)='prepbufr',       dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',                 dval(08)=0.0,  dthin(08)=0,
   dfile(09)='prepbufr',       dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',                  dval(09)=0.0,  dthin(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',             dval(10)=0.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=0.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=0.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=0.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=0.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=0.0,  dthin(15)=0,
   dfile(16)='omi',            dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',            dval(16)=0.0,  dthin(16)=4,
   dfile(17)='hirs2bufr',      dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',           dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',           dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr',      dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',           dval(19)=0.0,  dthin(19)=1,
   dfile(20)='hirs4bufr',      dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',           dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr',      dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=0.0,  dthin(21)=1,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr',      dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr',      dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',            dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',       dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=0.0,  dthin(26)=2,
   dfile(27)='msubufr',        dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',             dval(27)=0.0,  dthin(27)=1,
   dfile(28)='amsuabufr',      dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',           dval(28)=0.0,  dthin(28)=1,
   dfile(29)='amsuabufr',      dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',           dval(29)=0.0,  dthin(29)=1,
   dfile(30)='amsuabufr',      dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',           dval(30)=0.0,  dthin(30)=1,
   dfile(31)='amsuabufr',      dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',           dval(31)=0.0,  dthin(31)=1,
   dfile(32)='amsuabufr',      dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=0.0,  dthin(32)=1,
   dfile(33)='airsbufr',       dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',          dval(33)=0.0,  dthin(33)=1,
   dfile(34)='amsubbufr',      dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',           dval(34)=0.0,  dthin(34)=1,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',           dval(35)=0.0,  dthin(35)=1,
   dfile(36)='amsubbufr',      dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',           dval(36)=0.0,  dthin(36)=1,
   dfile(37)='mhsbufr',        dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',             dval(37)=0.0,  dthin(37)=1,
   dfile(38)='mhsbufr',        dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',         dval(38)=0.0,  dthin(38)=1,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',            dval(39)=0.0,  dthin(39)=1,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',            dval(40)=0.0,  dthin(40)=1,
   dfile(41)='ssmitbufr',      dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',            dval(41)=0.0,  dthin(41)=1,
   dfile(42)='amsrebufr',      dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=1,
   dfile(43)='amsrebufr',      dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=1,
   dfile(44)='amsrebufr',      dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=1,
   dfile(45)='ssmisbufr',      dtype(45)='ssmis',     dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=1,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd1',    dplat(46)='g12',       dsis(46)='sndrD1_g12',          dval(46)=0.0,  dthin(46)=1,
   dfile(47)='gsnd1bufr',      dtype(47)='sndrd2',    dplat(47)='g12',       dsis(47)='sndrD2_g12',          dval(47)=0.0,  dthin(47)=1,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd3',    dplat(48)='g12',       dsis(48)='sndrD3_g12',          dval(48)=0.0,  dthin(48)=1,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd4',    dplat(49)='g12',       dsis(49)='sndrD4_g12',          dval(49)=0.0,  dthin(49)=1,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd1',    dplat(50)='g11',       dsis(50)='sndrD1_g11',          dval(50)=0.0,  dthin(50)=1,
   dfile(51)='gsnd1bufr_skip', dtype(51)='sndrd2',    dplat(51)='g11',       dsis(51)='sndrD2_g11',          dval(51)=0.0,  dthin(51)=1,
   dfile(52)='gsnd1bufr',      dtype(52)='sndrd3',    dplat(52)='g11',       dsis(52)='sndrD3_g11',          dval(52)=0.0,  dthin(52)=1,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd4',    dplat(53)='g11',       dsis(53)='sndrD4_g11',          dval(53)=0.0,  dthin(53)=1,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd1',    dplat(54)='g13',       dsis(54)='sndrD1_g13',          dval(54)=0.0,  dthin(54)=1,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd2',    dplat(55)='g13',       dsis(55)='sndrD2_g13',          dval(55)=0.0,  dthin(55)=1,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd3',    dplat(56)='g13',       dsis(56)='sndrD3_g13',          dval(56)=0.0,  dthin(56)=1,
   dfile(57)='gsnd1bufr_skip', dtype(57)='sndrd4',    dplat(57)='g13',       dsis(57)='sndrD4_g13',          dval(57)=0.0,  dthin(57)=1,
   dfile(58)='iasibufr',       dtype(58)='iasi',      dplat(58)='metop-a',   dsis(58)='iasi616_metop-a',     dval(58)=0.0,  dthin(58)=3,
   dfile(59)='gomebufr',       dtype(59)='gome',      dplat(59)='metop-a',   dsis(59)='gome_metop-a',        dval(59)=0.0,  dthin(59)=4,
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
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for arw netcdf run

export arw_netcdf_namelist="

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=59,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
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
   dmesh(1)=1200.0,dmesh(2)=3000.0,dmesh(3)=4000.0,dmesh(4)=1000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=0.0,  dthin(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=0.0,  dthin(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=0.0,  dthin(03)=0,
   dfile(04)='prepbufr',       dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=0.0,  dthin(04)=0,
   dfile(05)='prepbufr',       dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',                 dval(05)=0.0,  dthin(05)=0,
   dfile(06)='radarbufr',      dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',                  dval(06)=0.0,  dthin(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=0.0,  dthin(07)=0,
   dfile(08)='prepbufr',       dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',                 dval(08)=0.0,  dthin(08)=0,
   dfile(09)='prepbufr',       dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',                  dval(09)=0.0,  dthin(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',             dval(10)=0.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=0.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=0.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=0.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=0.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=0.0,  dthin(15)=0,
   dfile(16)='omi',            dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',            dval(16)=0.0,  dthin(16)=4,
   dfile(17)='hirs2bufr',      dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',           dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',           dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr_skip', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',           dval(19)=0.0,  dthin(19)=1,
   dfile(20)='hirs4bufr',      dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',           dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr',      dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=0.0,  dthin(21)=1,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr',      dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr',      dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',            dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',       dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=0.0,  dthin(26)=2,
   dfile(27)='msubufr',        dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',             dval(27)=0.0,  dthin(27)=1,
   dfile(28)='amsuabufr_skip', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',           dval(28)=0.0,  dthin(28)=1,
   dfile(29)='amsuabufr',      dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',           dval(29)=0.0,  dthin(29)=1,
   dfile(30)='amsuabufr',      dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',           dval(30)=0.0,  dthin(30)=1,
   dfile(31)='amsuabufr_skip', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',           dval(31)=0.0,  dthin(31)=1,
   dfile(32)='amsuabufr',      dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=0.0,  dthin(32)=1,
   dfile(33)='airsbufr_skip',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',          dval(33)=0.0,  dthin(33)=1,
   dfile(34)='amsubbufr',      dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',           dval(34)=0.0,  dthin(34)=1,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',           dval(35)=0.0,  dthin(35)=1,
   dfile(36)='amsubbufr',      dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',           dval(36)=0.0,  dthin(36)=1,
   dfile(37)='mhsbufr_skip',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',             dval(37)=0.0,  dthin(37)=1,
   dfile(38)='mhsbufr',        dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',         dval(38)=0.0,  dthin(38)=1,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',            dval(39)=0.0,  dthin(39)=1,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',            dval(40)=0.0,  dthin(40)=1,
   dfile(41)='ssmitbufr',      dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',            dval(41)=0.0,  dthin(41)=1,
   dfile(42)='amsrebufr',      dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=1,
   dfile(43)='amsrebufr',      dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=1,
   dfile(44)='amsrebufr',      dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=1,
   dfile(45)='ssmisbufr',      dtype(45)='ssmis',     dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=1,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd1',    dplat(46)='g12',       dsis(46)='sndrD1_g12',          dval(46)=0.0,  dthin(46)=1,
   dfile(47)='gsnd1bufr',      dtype(47)='sndrd2',    dplat(47)='g12',       dsis(47)='sndrD2_g12',          dval(47)=0.0,  dthin(47)=1,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd3',    dplat(48)='g12',       dsis(48)='sndrD3_g12',          dval(48)=0.0,  dthin(48)=1,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd4',    dplat(49)='g12',       dsis(49)='sndrD4_g12',          dval(49)=0.0,  dthin(49)=1,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd1',    dplat(50)='g11',       dsis(50)='sndrD1_g11',          dval(50)=0.0,  dthin(50)=1,
   dfile(51)='gsnd1bufr_skip', dtype(51)='sndrd2',    dplat(51)='g11',       dsis(51)='sndrD2_g11',          dval(51)=0.0,  dthin(51)=1,
   dfile(52)='gsnd1bufr_skip', dtype(52)='sndrd3',    dplat(52)='g11',       dsis(52)='sndrD3_g11',          dval(52)=0.0,  dthin(52)=1,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd4',    dplat(53)='g11',       dsis(53)='sndrD4_g11',          dval(53)=0.0,  dthin(53)=1,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd1',    dplat(54)='g13',       dsis(54)='sndrD1_g13',          dval(54)=0.0,  dthin(54)=1,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd2',    dplat(55)='g13',       dsis(55)='sndrD2_g13',          dval(55)=0.0,  dthin(55)=1,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd3',    dplat(56)='g13',       dsis(56)='sndrD3_g13',          dval(56)=0.0,  dthin(56)=1,
   dfile(57)='gsnd1bufr_skip', dtype(57)='sndrd4',    dplat(57)='g13',       dsis(57)='sndrD4_g13',          dval(57)=0.0,  dthin(57)=1,
   dfile(58)='iasibufr',       dtype(58)='iasi',      dplat(58)='metop-a',   dsis(58)='iasi616_metop-a',     dval(58)=0.0,  dthin(58)=3,
   dfile(59)='gomebufr',       dtype(59)='gome',      dplat(59)='metop-a',   dsis(59)='gome_metop-a',        dval(59)=0.0,  dthin(59)=4,
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
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for nmm binary run

export nmm_binary_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=59,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,diagnostic_reg=.false.,
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
   dfact=0.75,dfact1=3.0,noiqc=.true.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=1200.0,dmesh(2)=5000.0,dmesh(3)=8000.0,dmesh(4)=1000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=0.0,  dthin(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=0.0,  dthin(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=0.0,  dthin(03)=0,
   dfile(04)='prepbufr',       dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=0.0,  dthin(04)=0,
   dfile(05)='prepbufr',       dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',                 dval(05)=0.0,  dthin(05)=0,
   dfile(06)='radarbufr',      dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',                  dval(06)=0.0,  dthin(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=0.0,  dthin(07)=0,
   dfile(08)='prepbufr',       dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',                 dval(08)=0.0,  dthin(08)=0,
   dfile(09)='prepbufr',       dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',                  dval(09)=0.0,  dthin(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',             dval(10)=0.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=0.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=0.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=0.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=0.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=0.0,  dthin(15)=0,
   dfile(16)='omi',            dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',            dval(16)=0.0,  dthin(16)=4,
   dfile(17)='hirs2bufr',      dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',           dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',           dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr_skip', dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',           dval(19)=0.0,  dthin(19)=1,
   dfile(20)='hirs4bufr',      dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',           dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr',      dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=0.0,  dthin(21)=1,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr',      dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr',      dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',            dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',       dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=0.0,  dthin(26)=2,
   dfile(27)='msubufr',        dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',             dval(27)=0.0,  dthin(27)=1,
   dfile(28)='amsuabufr_skip', dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',           dval(28)=0.0,  dthin(28)=1,
   dfile(29)='amsuabufr',      dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',           dval(29)=0.0,  dthin(29)=1,
   dfile(30)='amsuabufr',      dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',           dval(30)=0.0,  dthin(30)=1,
   dfile(31)='amsuabufr_skip', dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',           dval(31)=0.0,  dthin(31)=1,
   dfile(32)='amsuabufr',      dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=0.0,  dthin(32)=1,
   dfile(33)='airsbufr_skip',  dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',          dval(33)=0.0,  dthin(33)=1,
   dfile(34)='amsubbufr',      dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',           dval(34)=0.0,  dthin(34)=1,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',           dval(35)=0.0,  dthin(35)=1,
   dfile(36)='amsubbufr',      dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',           dval(36)=0.0,  dthin(36)=1,
   dfile(37)='mhsbufr_skip',   dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',             dval(37)=0.0,  dthin(37)=1,
   dfile(38)='mhsbufr',        dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',         dval(38)=0.0,  dthin(38)=1,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',            dval(39)=0.0,  dthin(39)=1,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',            dval(40)=0.0,  dthin(40)=1,
   dfile(41)='ssmitbufr',      dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',            dval(41)=0.0,  dthin(41)=1,
   dfile(42)='amsrebufr',      dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=1,
   dfile(43)='amsrebufr',      dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=1,
   dfile(44)='amsrebufr',      dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=1,
   dfile(45)='ssmisbufr',      dtype(45)='ssmis',     dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=1,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd1',    dplat(46)='g12',       dsis(46)='sndrD1_g12',          dval(46)=0.0,  dthin(46)=1,
   dfile(47)='gsnd1bufr',      dtype(47)='sndrd2',    dplat(47)='g12',       dsis(47)='sndrD2_g12',          dval(47)=0.0,  dthin(47)=1,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd3',    dplat(48)='g12',       dsis(48)='sndrD3_g12',          dval(48)=0.0,  dthin(48)=1,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd4',    dplat(49)='g12',       dsis(49)='sndrD4_g12',          dval(49)=0.0,  dthin(49)=1,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd1',    dplat(50)='g11',       dsis(50)='sndrD1_g11',          dval(50)=0.0,  dthin(50)=1,
   dfile(51)='gsnd1bufr_skip', dtype(51)='sndrd2',    dplat(51)='g11',       dsis(51)='sndrD2_g11',          dval(51)=0.0,  dthin(51)=1,
   dfile(52)='gsnd1bufr_skip', dtype(52)='sndrd3',    dplat(52)='g11',       dsis(52)='sndrD3_g11',          dval(52)=0.0,  dthin(52)=1,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd4',    dplat(53)='g11',       dsis(53)='sndrD4_g11',          dval(53)=0.0,  dthin(53)=1,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd1',    dplat(54)='g13',       dsis(54)='sndrD1_g13',          dval(54)=0.0,  dthin(54)=1,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd2',    dplat(55)='g13',       dsis(55)='sndrD2_g13',          dval(55)=0.0,  dthin(55)=1,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd3',    dplat(56)='g13',       dsis(56)='sndrD3_g13',          dval(56)=0.0,  dthin(56)=1,
   dfile(57)='gsnd1bufr_skip', dtype(57)='sndrd4',    dplat(57)='g13',       dsis(57)='sndrD4_g13',          dval(57)=0.0,  dthin(57)=1,
   dfile(58)='iasibufr',       dtype(58)='iasi',      dplat(58)='metop-a',   dsis(58)='iasi616_metop-a',     dval(58)=0.0,  dthin(58)=3,
   dfile(59)='gomebufr',       dtype(59)='gome',      dplat(59)='metop-a',   dsis(59)='gome_metop-a',        dval(59)=0.0,  dthin(59)=4,
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
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for nmm netcdf run

export nmm_netcdf_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=59,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,diagnostic_reg=.false.,
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
   dmesh(1)=1200.0,dmesh(2)=2000.0,dmesh(3)=2500.0,dmesh(4)=1000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',         dsis(01)='ps',                  dval(01)=0.0,  dthin(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',         dsis(02)='t',                   dval(02)=0.0,  dthin(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',         dsis(03)='q',                   dval(03)=0.0,  dthin(03)=0,
   dfile(04)='prepbufr',       dtype(04)='uv',        dplat(04)=' ',         dsis(04)='uv',                  dval(04)=0.0,  dthin(04)=0,
   dfile(05)='prepbufr',       dtype(05)='spd',       dplat(05)=' ',         dsis(05)='spd',                 dval(05)=0.0,  dthin(05)=0,
   dfile(06)='radarbufr',      dtype(06)='rw',        dplat(06)=' ',         dsis(06)='rw',                  dval(06)=0.0,  dthin(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',         dsis(07)='dw',                  dval(07)=0.0,  dthin(07)=0,
   dfile(08)='prepbufr',       dtype(08)='sst',       dplat(08)=' ',         dsis(08)='sst',                 dval(08)=0.0,  dthin(08)=0,
   dfile(09)='prepbufr',       dtype(09)='pw',        dplat(09)=' ',         dsis(09)='pw',                  dval(09)=0.0,  dthin(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',         dsis(10)='gps_ref',             dval(10)=0.0,  dthin(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',      dsis(11)='pcp_ssmi',            dval(11)=0.0,  dthin(11)=-1,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',      dsis(12)='pcp_tmi',             dval(12)=0.0,  dthin(12)=-1,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',       dsis(13)='sbuv8_n16',           dval(13)=0.0,  dthin(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',       dsis(14)='sbuv8_n17',           dval(14)=0.0,  dthin(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',       dsis(15)='sbuv8_n18',           dval(15)=0.0,  dthin(15)=0,
   dfile(16)='omi',            dtype(16)='omi',       dplat(16)='aura',      dsis(16)='omi_aura',            dval(16)=0.0,  dthin(16)=4,
   dfile(17)='hirs2bufr',      dtype(17)='hirs2',     dplat(17)='n14',       dsis(17)='hirs2_n14',           dval(17)=0.0,  dthin(17)=1,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n16',       dsis(18)='hirs3_n16',           dval(18)=0.0,  dthin(18)=1,
   dfile(19)='hirs3bufr',      dtype(19)='hirs3',     dplat(19)='n17',       dsis(19)='hirs3_n17',           dval(19)=0.0,  dthin(19)=1,
   dfile(20)='hirs4bufr',      dtype(20)='hirs4',     dplat(20)='n18',       dsis(20)='hirs4_n18',           dval(20)=0.0,  dthin(20)=1,
   dfile(21)='hirs4bufr',      dtype(21)='hirs4',     dplat(21)='metop-a',   dsis(21)='hirs4_metop-a',       dval(21)=0.0,  dthin(21)=1,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g11',       dsis(22)='sndr_g11',            dval(22)=0.0,  dthin(22)=1,
   dfile(23)='gsndrbufr',      dtype(23)='sndr',      dplat(23)='g12',       dsis(23)='sndr_g12',            dval(23)=0.0,  dthin(23)=1,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g11',       dsis(24)='imgr_g11',            dval(24)=0.0,  dthin(24)=1,
   dfile(25)='gimgrbufr',      dtype(25)='goes_img',  dplat(25)='g12',       dsis(25)='imgr_g12',            dval(25)=0.0,  dthin(25)=1,
   dfile(26)='airsbufr',       dtype(26)='airs',      dplat(26)='aqua',      dsis(26)='airs281SUBSET_aqua',  dval(26)=0.0,  dthin(26)=2,
   dfile(27)='msubufr',        dtype(27)='msu',       dplat(27)='n14',       dsis(27)='msu_n14',             dval(27)=0.0,  dthin(27)=1,
   dfile(28)='amsuabufr',      dtype(28)='amsua',     dplat(28)='n15',       dsis(28)='amsua_n15',           dval(28)=0.0,  dthin(28)=1,
   dfile(29)='amsuabufr',      dtype(29)='amsua',     dplat(29)='n16',       dsis(29)='amsua_n16',           dval(29)=0.0,  dthin(29)=1,
   dfile(30)='amsuabufr',      dtype(30)='amsua',     dplat(30)='n17',       dsis(30)='amsua_n17',           dval(30)=0.0,  dthin(30)=1,
   dfile(31)='amsuabufr',      dtype(31)='amsua',     dplat(31)='n18',       dsis(31)='amsua_n18',           dval(31)=0.0,  dthin(31)=1,
   dfile(32)='amsuabufr',      dtype(32)='amsua',     dplat(32)='metop-a',   dsis(32)='amsua_metop-a',       dval(32)=0.0,  dthin(32)=1,
   dfile(33)='airsbufr',       dtype(33)='amsua',     dplat(33)='aqua',      dsis(33)='amsua_aqua',          dval(33)=0.0,  dthin(33)=1,
   dfile(34)='amsubbufr',      dtype(34)='amsub',     dplat(34)='n15',       dsis(34)='amsub_n15',           dval(34)=0.0,  dthin(34)=1,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n16',       dsis(35)='amsub_n16',           dval(35)=0.0,  dthin(35)=1,
   dfile(36)='amsubbufr',      dtype(36)='amsub',     dplat(36)='n17',       dsis(36)='amsub_n17',           dval(36)=0.0,  dthin(36)=1,
   dfile(37)='mhsbufr',        dtype(37)='mhs',       dplat(37)='n18',       dsis(37)='mhs_n18',             dval(37)=0.0,  dthin(37)=1,
   dfile(38)='mhsbufr',        dtype(38)='mhs',       dplat(38)='metop-a',   dsis(38)='mhs_metop-a',         dval(38)=0.0,  dthin(38)=1,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f13',       dsis(39)='ssmi_f13',            dval(39)=0.0,  dthin(39)=1,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f14',       dsis(40)='ssmi_f14',            dval(40)=0.0,  dthin(40)=1,
   dfile(41)='ssmitbufr',      dtype(41)='ssmi',      dplat(41)='f15',       dsis(41)='ssmi_f15',            dval(41)=0.0,  dthin(41)=1,
   dfile(42)='amsrebufr',      dtype(42)='amsre_low', dplat(42)='aqua',      dsis(42)='amsre_aqua',          dval(42)=0.0,  dthin(42)=1,
   dfile(43)='amsrebufr',      dtype(43)='amsre_mid', dplat(43)='aqua',      dsis(43)='amsre_aqua',          dval(43)=0.0,  dthin(43)=1,
   dfile(44)='amsrebufr',      dtype(44)='amsre_hig', dplat(44)='aqua',      dsis(44)='amsre_aqua',          dval(44)=0.0,  dthin(44)=1,
   dfile(45)='ssmisbufr',      dtype(45)='ssmis',     dplat(45)='f16',       dsis(45)='ssmis_f16',           dval(45)=0.0,  dthin(45)=1,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd1',    dplat(46)='g12',       dsis(46)='sndrD1_g12',          dval(46)=0.0,  dthin(46)=1,
   dfile(47)='gsnd1bufr',      dtype(47)='sndrd2',    dplat(47)='g12',       dsis(47)='sndrD2_g12',          dval(47)=0.0,  dthin(47)=1,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd3',    dplat(48)='g12',       dsis(48)='sndrD3_g12',          dval(48)=0.0,  dthin(48)=1,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd4',    dplat(49)='g12',       dsis(49)='sndrD4_g12',          dval(49)=0.0,  dthin(49)=1,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd1',    dplat(50)='g11',       dsis(50)='sndrD1_g11',          dval(50)=0.0,  dthin(50)=1,
   dfile(51)='gsnd1bufr_skip', dtype(51)='sndrd2',    dplat(51)='g11',       dsis(51)='sndrD2_g11',          dval(51)=0.0,  dthin(51)=1,
   dfile(52)='gsnd1bufr_skip', dtype(52)='sndrd3',    dplat(52)='g11',       dsis(52)='sndrD3_g11',          dval(52)=0.0,  dthin(52)=1,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd4',    dplat(53)='g11',       dsis(53)='sndrD4_g11',          dval(53)=0.0,  dthin(53)=1,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd1',    dplat(54)='g13',       dsis(54)='sndrD1_g13',          dval(54)=0.0,  dthin(54)=1,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd2',    dplat(55)='g13',       dsis(55)='sndrD2_g13',          dval(55)=0.0,  dthin(55)=1,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd3',    dplat(56)='g13',       dsis(56)='sndrD3_g13',          dval(56)=0.0,  dthin(56)=1,
   dfile(57)='gsnd1bufr_skip', dtype(57)='sndrd4',    dplat(57)='g13',       dsis(57)='sndrD4_g13',          dval(57)=0.0,  dthin(57)=1,
   dfile(58)='iasibufr',       dtype(58)='iasi',      dplat(58)='metop-a',   dsis(58)='iasi616_metop-a',     dval(58)=0.0,  dthin(58)=3,
   dfile(59)='gomebufr',       dtype(59)='gome',      dplat(59)='metop-a',   dsis(59)='gome_metop-a',        dval(59)=0.0,  dthin(59)=4,
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
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for nems nmmb run

export nems_nmmb_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=64,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,preserve_restart_date=.true.,
   use_gfs_ozone=.true.,check_gfs_ozone_date=.true.,regional_ozone=.true.
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,nems_nmmb_regional=.true.,diagnostic_reg=.false.,
   nmmb_reference_grid='H',grid_ratio_nmmb=1.412,
   filled_grid=.false.,half_grid=.true.,netcdf=.false.,
 /
 &BKGERR
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
   dmesh(1)=1200.0,dmesh(2)=2000.0,dmesh(3)=2500.0,dmesh(4)=1000.0,time_window_max=0.5,
   dfile(01)='prepbufr',       dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',                dval(01)=0.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'        dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',                 dval(02)=0.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',       dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',                 dval(03)=0.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',       dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',                dval(04)=0.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',       dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',                dval(05)=0.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='prepbufr',       dtype(06)='spd',       dplat(06)=' ',       dsis(06)='spd',               dval(06)=0.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',       dtype(07)='dw',        dplat(07)=' ',       dsis(07)='dw',                dval(07)=0.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='radarbufr',      dtype(08)='rw',        dplat(08)=' ',       dsis(08)='rw',                dval(08)=0.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='prepbufr',       dtype(09)='sst',       dplat(09)=' ',       dsis(09)='sst',               dval(09)=0.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='gpsrobufr',      dtype(10)='gps_ref',   dplat(10)=' ',       dsis(10)='gps_ref',           dval(10)=0.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',     dtype(11)='pcp_ssmi',  dplat(11)='dmsp',    dsis(11)='pcp_ssmi',          dval(11)=0.0,  dthin(11)=-1, dsfcalc(11)=0,
   dfile(12)='tmirrbufr',      dtype(12)='pcp_tmi',   dplat(12)='trmm',    dsis(12)='pcp_tmi',           dval(12)=0.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='sbuvbufr',       dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',         dval(13)=0.0,  dthin(13)=0,  dsfcalc(13)=0,
   dfile(14)='sbuvbufr',       dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',         dval(14)=0.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',       dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',         dval(15)=0.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='hirs2bufr',      dtype(16)='hirs2',     dplat(16)='n14',     dsis(16)='hirs2_n14',         dval(16)=0.0,  dthin(16)=1,  dsfcalc(16)=0,
   dfile(17)='hirs3bufr_skip', dtype(17)='hirs3',     dplat(17)='n16',     dsis(17)='hirs3_n16',         dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=0,
   dfile(18)='hirs3bufr',      dtype(18)='hirs3',     dplat(18)='n17',     dsis(18)='hirs3_n17',         dval(18)=0.0,  dthin(18)=1,  dsfcalc(18)=0,
   dfile(19)='hirs4bufr_skip', dtype(19)='hirs4',     dplat(19)='n18',     dsis(19)='hirs4_n18',         dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='hirs4bufr',      dtype(20)='hirs4',     dplat(20)='metop-a', dsis(20)='hirs4_metop-a',     dval(20)=0.0,  dthin(20)=1,  dsfcalc(20)=0,
   dfile(21)='gsndrbufr',      dtype(21)='sndr',      dplat(21)='g11',     dsis(21)='sndr_g11',          dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=0,
   dfile(22)='gsndrbufr',      dtype(22)='sndr',      dplat(22)='g12',     dsis(22)='sndr_g12',          dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=0,
   dfile(23)='gimgrbufr',      dtype(23)='goes_img',  dplat(23)='g11',     dsis(23)='imgr_g11',          dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=0,
   dfile(24)='gimgrbufr',      dtype(24)='goes_img',  dplat(24)='g12',     dsis(24)='imgr_g12',          dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=0,
   dfile(25)='airsbufr',       dtype(25)='airs',      dplat(25)='aqua',    dsis(25)='airs281SUBSET_aqua',dval(25)=0.0,  dthin(25)=2,  dsfcalc(25)=0,
   dfile(26)='msubufr',        dtype(26)='msu',       dplat(26)='n14',     dsis(26)='msu_n14',           dval(26)=0.0,  dthin(26)=1,  dsfcalc(26)=0,
   dfile(27)='amsuabufr_skip', dtype(27)='amsua',     dplat(27)='n15',     dsis(27)='amsua_n15',         dval(27)=0.0,  dthin(27)=1,  dsfcalc(27)=0,
   dfile(28)='amsuabufr_skip', dtype(28)='amsua',     dplat(28)='n16',     dsis(28)='amsua_n16',         dval(28)=0.0,  dthin(28)=1,  dsfcalc(28)=0,
   dfile(29)='amsuabufr_skip', dtype(29)='amsua',     dplat(29)='n17',     dsis(29)='amsua_n17',         dval(29)=0.0,  dthin(29)=1,  dsfcalc(29)=0,
   dfile(30)='amsuabufr_skip', dtype(30)='amsua',     dplat(30)='n18',     dsis(30)='amsua_n18',         dval(30)=0.0,  dthin(30)=1,  dsfcalc(30)=0,
   dfile(31)='amsuabufr',      dtype(31)='amsua',     dplat(31)='metop-a', dsis(31)='amsua_metop-a',     dval(31)=0.0,  dthin(31)=1,  dsfcalc(31)=0,
   dfile(32)='airsbufr_skip',  dtype(32)='amsua',     dplat(32)='aqua',    dsis(32)='amsua_aqua',        dval(32)=0.0,  dthin(32)=1,  dsfcalc(32)=0,
   dfile(33)='amsubbufr_skip', dtype(33)='amsub',     dplat(33)='n15',     dsis(33)='amsub_n15',         dval(33)=0.0,  dthin(33)=1,  dsfcalc(33)=0,
   dfile(34)='amsubbufr_skip', dtype(34)='amsub',     dplat(34)='n16',     dsis(34)='amsub_n16',         dval(34)=0.0,  dthin(34)=1,  dsfcalc(34)=0,
   dfile(35)='amsubbufr',      dtype(35)='amsub',     dplat(35)='n17',     dsis(35)='amsub_n17',         dval(35)=0.0,  dthin(35)=1,  dsfcalc(35)=0,
   dfile(36)='mhsbufr_skip',   dtype(36)='mhs',       dplat(36)='n18',     dsis(36)='mhs_n18',           dval(36)=0.0,  dthin(36)=1,  dsfcalc(36)=0,
   dfile(37)='mhsbufr',        dtype(37)='mhs',       dplat(37)='metop-a', dsis(37)='mhs_metop-a',       dval(37)=0.0,  dthin(37)=1,  dsfcalc(37)=0,
   dfile(38)='ssmitbufr',      dtype(38)='ssmi',      dplat(38)='f13',     dsis(38)='ssmi_f13',          dval(38)=0.0,  dthin(38)=1,  dsfcalc(38)=0,
   dfile(39)='ssmitbufr',      dtype(39)='ssmi',      dplat(39)='f14',     dsis(39)='ssmi_f14',          dval(39)=0.0,  dthin(39)=1,  dsfcalc(39)=0,
   dfile(40)='ssmitbufr',      dtype(40)='ssmi',      dplat(40)='f15',     dsis(40)='ssmi_f15',          dval(40)=0.0,  dthin(40)=1,  dsfcalc(40)=0,
   dfile(41)='amsrebufr',      dtype(41)='amsre_low', dplat(41)='aqua',    dsis(41)='amsre_aqua',        dval(41)=0.0,  dthin(41)=1,  dsfcalc(41)=0,
   dfile(42)='amsrebufr',      dtype(42)='amsre_mid', dplat(42)='aqua',    dsis(42)='amsre_aqua',        dval(42)=0.0,  dthin(42)=1,  dsfcalc(42)=0,
   dfile(43)='amsrebufr',      dtype(43)='amsre_hig', dplat(43)='aqua',    dsis(43)='amsre_aqua',        dval(43)=0.0,  dthin(43)=1,  dsfcalc(43)=0,
   dfile(44)='ssmisbufr',      dtype(44)='ssmis',     dplat(44)='f16',     dsis(44)='ssmis_f16',         dval(44)=0.0,  dthin(44)=1,  dsfcalc(44)=0,
   dfile(45)='gsnd1bufr_skip', dtype(45)='sndrd1',    dplat(45)='g12',     dsis(45)='sndrD1_g12',        dval(45)=0.0,  dthin(45)=1,  dsfcalc(45)=0,
   dfile(46)='gsnd1bufr_skip', dtype(46)='sndrd2',    dplat(46)='g12',     dsis(46)='sndrD2_g12',        dval(46)=0.0,  dthin(46)=1,  dsfcalc(46)=0,
   dfile(47)='gsnd1bufr_skip', dtype(47)='sndrd3',    dplat(47)='g12',     dsis(47)='sndrD3_g12',        dval(47)=0.0,  dthin(47)=1,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr_skip', dtype(48)='sndrd4',    dplat(48)='g12',     dsis(48)='sndrD4_g12',        dval(48)=0.0,  dthin(48)=1,  dsfcalc(48)=0,
   dfile(49)='gsnd1bufr_skip', dtype(49)='sndrd1',    dplat(49)='g11',     dsis(49)='sndrD1_g11',        dval(49)=0.0,  dthin(49)=1,  dsfcalc(49)=0,
   dfile(50)='gsnd1bufr_skip', dtype(50)='sndrd2',    dplat(50)='g11',     dsis(50)='sndrD2_g11',        dval(50)=0.0,  dthin(50)=1,  dsfcalc(50)=0,
   dfile(51)='gsnd1bufr',      dtype(51)='sndrd3',    dplat(51)='g11',     dsis(51)='sndrD3_g11',        dval(51)=0.0,  dthin(51)=1,  dsfcalc(51)=0,
   dfile(52)='gsnd1bufr_skip', dtype(52)='sndrd4',    dplat(52)='g11',     dsis(52)='sndrD4_g11',        dval(52)=0.0,  dthin(52)=1,  dsfcalc(52)=0,
   dfile(53)='gsnd1bufr_skip', dtype(53)='sndrd1',    dplat(53)='g13',     dsis(53)='sndrD1_g13',        dval(53)=0.0,  dthin(53)=1,  dsfcalc(53)=0,
   dfile(54)='gsnd1bufr_skip', dtype(54)='sndrd2',    dplat(54)='g13',     dsis(54)='sndrD2_g13',        dval(54)=0.0,  dthin(54)=1,  dsfcalc(54)=0,
   dfile(55)='gsnd1bufr_skip', dtype(55)='sndrd3',    dplat(55)='g13',     dsis(55)='sndrD3_g13',        dval(55)=0.0,  dthin(55)=1,  dsfcalc(55)=0,
   dfile(56)='gsnd1bufr_skip', dtype(56)='sndrd4',    dplat(56)='g13',     dsis(56)='sndrD4_g13',        dval(56)=0.0,  dthin(56)=1,  dsfcalc(56)=0,
   dfile(57)='iasibufr',       dtype(57)='iasi',      dplat(57)='metop-a', dsis(57)='iasi616_metop-a',   dval(57)=0.0,  dthin(57)=3,  dsfcalc(57)=0,
   dfile(58)='gomebufr',       dtype(58)='gome',      dplat(58)='metop-a', dsis(58)='gome_metop-a',      dval(58)=0.0,  dthin(58)=4,  dsfcalc(58)=0,
   dfile(59)='omibufr',        dtype(59)='omi',       dplat(59)='aura',    dsis(59)='omi_aura',          dval(59)=0.0,  dthin(59)=4,  dsfcalc(59)=0,
   dfile(60)='sbuvbufr',       dtype(60)='sbuv2',     dplat(60)='n19',     dsis(60)='sbuv8_n19',         dval(60)=0.0,  dthin(60)=0,  dsfcalc(60)=0,
   dfile(61)='hirs4bufr',      dtype(61)='hirs4',     dplat(61)='n19',     dsis(61)='hirs4_n19',         dval(61)=0.0,  dthin(61)=1,  dsfcalc(61)=0,
   dfile(62)='amsuabufr',      dtype(62)='amsua',     dplat(62)='n19',     dsis(62)='amsua_n19',         dval(62)=0.0,  dthin(62)=1,  dsfcalc(62)=0,
   dfile(63)='mhsbufr',        dtype(63)='mhs',       dplat(63)='n19',     dsis(63)='mhs_n19',           dval(63)=0.0,  dthin(63)=1,  dsfcalc(63)=0,
   dfile(64)='tcvitl'          dtype(64)='tcp',       dplat(64)=' ',       dsis(64)='tcp',               dval(64)=0.0,  dthin(64)=0,  dsfcalc(64)=0,
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
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for cmaq binary run

 export cmaq_binary_namelist="

 &SETUP
   miter=2,niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   ndat=1,iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,hybrid=.true.,
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,
   cmaq_regional=.true.,diagnostic_reg=.true.,
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
   jcstrong=.false.,jcstrong_option=3,nstrong=0,nvmodes_keep=20,
   period_max=3.,baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,
   dmesh(5)=120,time_window_max=1.5,
   dfile(01)='anowbufr',  dtype(01)='pm2_5',        dplat(01)=' ',         dsis(01)='TEOM',              dval(01)=1.0,  dthin(01)=0,
 /
!max name length for dfile=13
!max name length for dtype=10
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
   berror_chem=.true.,
   oneobtest_chem=.true.,
   maginnov_chem=60,magoberr_chem=2.,oneob_type_chem='pm2_5',
   oblat_chem=45.,oblon_chem=270.,obpres_chem=1000.,
   diag_incr=.true.,elev_tolerance=500.,tunable_error=0.5,
   in_fname="\""${cmaq_input}"\"",out_fname="\""${cmaq_output}"\"",
   incr_fname="\""${chem_increment}"\"",
!diag_incr for diagnostic increment output
 /
 &SINGLEOB_TEST
   maginnov=5,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /"
