# Define namelist for global run (pcgsoi minimization)

export global_T62_namelist=" 

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,
   niter_no_qc(1)=1,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=82,qoption=2,
   factqmin=5.0,factqmax=5.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=.false.,lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=1,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,time_window_max=0.5,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype         dplat     dsis                dval    dthin dsfcalc
   prepbufr         ps          null      ps                  0.0     0     0
   prepbufr         t           null      t                   0.0     0     0
   prepbufr         q           null      q                   0.0     0     0
   prepbufr         pw          null      pw                  0.0     0     0
   prepbufr         uv          null      uv                  0.0     0     0
   satwndbufr       uv          null      uv                  0.0     0     0
   prepbufr         spd         null      spd                 0.0     0     0
   prepbufr         dw          null      dw                  0.0     0     0
   radarbufr        rw          null      rw                  0.0     0     0
   prepbufr         sst         null      sst                 0.0     0     0
   gpsrobufr        $gps_dtype  null      gps                 0.0     0     0
   ssmirrbufr       pcp_ssmi    dmsp      pcp_ssmi            0.0    -1     0
   tmirrbufr        pcp_tmi     trmm      pcp_tmi             0.0    -1     0
   sbuvbufr         sbuv2       n16       sbuv8_n16           0.0     0     0
   sbuvbufr         sbuv2       n17       sbuv8_n17           0.0     0     0
   sbuvbufr         sbuv2       n18       sbuv8_n18           0.0     0     0
   hirs3bufr        hirs3       n17       hirs3_n17           0.0     1     1
   hirs4bufr_skip   hirs4       metop-a   hirs4_metop-a       0.0     1     1
   gimgrbufr        goes_img    g11       imgr_g11            0.0     1     0
   gimgrbufr        goes_img    g12       imgr_g12            0.0     1     0
   airsbufr         airs        aqua      airs281SUBSET_aqua  0.0     1     1
   amsuabufr_skip   amsua       n15       amsua_n15           0.0     1     1
   amsuabufr_skip   amsua       n18       amsua_n18           0.0     1     1
   amsuabufr_skip   amsua       metop-a   amsua_metop-a       0.0     1     1
   airsbufr_skip    amsua       aqua      amsua_aqua          0.0     1     1
   amsubbufr        amsub       n17       amsub_n17           0.0     1     1
   mhsbufr_skip     mhs         n18       mhs_n18             0.0     1     1
   mhsbufr_skip     mhs         metop-a   mhs_metop-a         0.0     1     1
   ssmitbufr        ssmi        f14       ssmi_f14            0.0     1     0
   ssmitbufr        ssmi        f15       ssmi_f15            0.0     1     0
   amsrebufr        amsre_low   aqua      amsre_aqua          0.0     1     0
   amsrebufr        amsre_mid   aqua      amsre_aqua          0.0     1     0
   amsrebufr        amsre_hig   aqua      amsre_aqua          0.0     1     0
   ssmisbufr        ssmis_las   f16       ssmis_f16           0.0     1     0
   ssmisbufr        ssmis_uas   f16       ssmis_f16           0.0     1     0
   ssmisbufr        ssmis_img   f16       ssmis_f16           0.0     1     0
   ssmisbufr        ssmis_env   f16       ssmis_f16           0.0     1     0
   gsnd1bufr_skip   sndrd1      g12       sndrD1_g12          0.0     1     0
   gsnd1bufr_skip   sndrd2      g12       sndrD2_g12          0.0     1     0
   gsnd1bufr_skip   sndrd3      g12       sndrD3_g12          0.0     1     0
   gsnd1bufr_skip   sndrd4      g12       sndrD4_g12          0.0     1     0
   gsnd1bufr_skip   sndrd1      g11       sndrD1_g11          0.0     1     0
   gsnd1bufr_skip   sndrd2      g11       sndrD2_g11          0.0     1     0
   gsnd1bufr        sndrd3      g11       sndrD3_g11          0.0     1     0
   gsnd1bufr_skip   sndrd4      g11       sndrD4_g11          0.0     1     0
   gsnd1bufr_skip   sndrd1      g13       sndrD1_g13          0.0     1     0
   gsnd1bufr_skip   sndrd2      g13       sndrD2_g13          0.0     1     0
   gsnd1bufr_skip   sndrd3      g13       sndrD3_g13          0.0     1     0
   gsnd1bufr_skip   sndrd4      g13       sndrD4_g13          0.0     1     0
   iasibufr         iasi        metop-a   iasi616_metop-a     0.0     1     1
   gomebufr         gome        metop-a   gome_metop-a        0.0     2     0
   omibufr          omi         aura      omi_aura            0.0     2     0
   sbuvbufr         sbuv2       n19       sbuv8_n19           0.0     0     0
   hirs4bufr        hirs4       n19       hirs4_n19           0.0     1     1
   amsuabufr        amsua       n19       amsua_n19           0.0     1     1
   mhsbufr          mhs         n19       mhs_n19             0.0     1     1
   tcvitl           tcp         null      tcp                 0.0     0     0
   mlsbufr          mls30       aura      mls30_aura          1.0     0     0
   seviribufr       seviri      m08       seviri_m08          0.0     1     0
   seviribufr       seviri      m09       seviri_m09          0.0     1     0
   seviribufr       seviri      m10       seviri_m10          0.0     1     0
   hirs4bufr        hirs4       metop-b   hirs4_metop-b       0.0     1     0
   amsuabufr        amsua       metop-b   amsua_metop-b       0.0     1     0
   mhsbufr          mhs         metop-b   mhs_metop-b         0.0     1     0
   iasibufr         iasi        metop-b   iasi616_metop-b     0.0     1     0
   gomebufr         gome        metop-b   gome_metop-b        0.0     2     0
   atmsbufr         atms        npp       atms_npp            0.0     1     0
   crisbufr         cris        npp       cris_npp            0.0     1     0
::
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
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
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
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=10,gpstop=30.,
   lsqrtb=.true.,lcongrad=.true.,ltlint=.true.,ladtest=.true.,lgrtest=.false.,
   use_gfs_nemsio=.false.,lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=1,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,dmesh(3)=5000.0,dmesh(4)=10000.0,dmesh(5)=1450.0,time_window_max=0.5,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype         dplat     dsis                dval    dthin dsfcalc
   prepbufr         ps          null      ps                  0.0      0      0
   prepbufr         t           null      t                   0.0      0      0
   prepbufr         q           null      q                   0.0      0      0
   prepbufr         pw          null      pw                  0.0      0      0
   prepbufr         uv          null      uv                  0.0      0      0
   satwndbufr       uv          null      uv                  0.0      0      0
   prepbufr         spd         null      spd                 0.0      0      0
   prepbufr         dw          null      dw                  0.0      0      0
   radarbufr        rw          null      rw                  0.0      0      0
   prepbufr         sst         null      sst                 0.0      0      0
   gpsrobufr        $gps_dtype  null      gps                 0.0      0      0
   ssmirrbufr       pcp_ssmi    dmsp      pcp_ssmi            0.0     -1      0
   tmirrbufr        pcp_tmi     trmm      pcp_tmi             0.0     -1      0
   sbuvbufr         sbuv2       n16       sbuv8_n16           0.0      0      0
   sbuvbufr         sbuv2       n17       sbuv8_n17           0.0      0      0
   sbuvbufr         sbuv2       n18       sbuv8_n18           0.0      0      0
   hirs2bufr        hirs2       n14       hirs2_n14           0.0      1      1
   hirs3bufr_skip   hirs3       n16       hirs3_n16           0.0      1      1
   hirs3bufr        hirs3       n17       hirs3_n17           0.0      1      1
   hirs4bufr_skip   hirs4       n18       hirs4_n18           0.0      1      1
   hirs4bufr_skip   hirs4       metop-a   hirs4_metop-a       0.0      1      1
   gsndrbufr        sndr        g11       sndr_g11            0.0      1      0
   gsndrbufr        sndr        g12       sndr_g12            0.0      1      0
   gimgrbufr        goes_img    g11       imgr_g11            0.0      1      0
   gimgrbufr        goes_img    g12       imgr_g12            0.0      1      0
   airsbufr         airs        aqua      airs281SUBSET_aqua  0.0      3      1
   msubufr          msu         n14       msu_n14             0.0      1      1
   amsuabufr_skip   amsua       n15       amsua_n15           0.0      1      1
   amsuabufr_skip   amsua       n16       amsua_n16           0.0      1      1
   amsuabufr_skip   amsua       n17       amsua_n17           0.0      1      1
   amsuabufr_skip   amsua       n18       amsua_n18           0.0      1      1
   amsuabufr_skip   amsua       metop-a   amsua_metop-a       0.0      1      1
   airsbufr_skip    amsua       aqua      amsua_aqua          0.0      1      1
   amsubbufr_skip   amsub       n15       amsub_n15           0.0      1      1
   amsubbufr_skip   amsub       n16       amsub_n16           0.0      1      1
   amsubbufr        amsub       n17       amsub_n17           0.0      1      1
   mhsbufr_skip     mhs         n18       mhs_n18             0.0      1      1
   mhsbufr_skip     mhs         metop-a   mhs_metop-a         0.0      1      1
   ssmitbufr        ssmi        f13       ssmi_f13            0.0      1      0
   ssmitbufr        ssmi        f14       ssmi_f14            0.0      1      0
   ssmitbufr        ssmi        f15       ssmi_f15            0.0      1      0
   amsrebufr        amsre_low   aqua      amsre_aqua          0.0      1      1
   amsrebufr        amsre_mid   aqua      amsre_aqua          0.0      1      1
   amsrebufr        amsre_hig   aqua      amsre_aqua          0.0      1      1
   ssmisbufr        ssmis       f16       ssmis_f16           0.0      1      1
   gsnd1bufr_skip   sndrd1      g12       sndrD1_g12          0.0      1      0
   gsnd1bufr_skip   sndrd2      g12       sndrD2_g12          0.0      1      0
   gsnd1bufr_skip   sndrd3      g12       sndrD3_g12          0.0      1      0
   gsnd1bufr_skip   sndrd4      g12       sndrD4_g12          0.0      1      0
   gsnd1bufr_skip   sndrd1      g11       sndrD1_g11          0.0      1      0
   gsnd1bufr_skip   sndrd2      g11       sndrD2_g11          0.0      1      0
   gsnd1bufr        sndrd3      g11       sndrD3_g11          0.0      1      0
   gsnd1bufr_skip   sndrd4      g11       sndrD4_g11          0.0      1      0
   gsnd1bufr_skip   sndrd1      g13       sndrD1_g13          0.0      1      0
   gsnd1bufr_skip   sndrd2      g13       sndrD2_g13          0.0      1      0
   gsnd1bufr_skip   sndrd3      g13       sndrD3_g13          0.0      1      0
   gsnd1bufr_skip   sndrd4      g13       sndrD4_g13          0.0      1      0
   iasibufr         iasi        metop-a   iasi616_metop-a     0.0      4      1
   gomebufr         gome        metop-a   gome_metop-a        0.0      2      0
   omibufr          omi         aura      omi_aura            0.0      2      0
   sbuvbufr         sbuv2       n19       sbuv8_n19           0.0      0      0
   hirs4bufr        hirs4       n19       hirs4_n19           0.0      1      1
   amsuabufr        amsua       n19       amsua_n19           0.0      1      1
   mhsbufr          mhs         n19       mhs_n19             0.0      1      1
   tcvitl           tcp         null      tcp                 0.0      0      0
   modisbufr        modis       aqua      modis_aqua          0.0      1      0
   modisbufr        modis       terra     modis_terra         0.0      1      0
   mlsbufr          mls30       aura      mls30_aura          1.0      0      0
   seviribufr       seviri      m08       seviri_m08          0.0      5      0
   seviribufr       seviri      m09       seviri_m09          0.0      5      0
   seviribufr       seviri      m10       seviri_m10          0.0      5      0
::
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
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
   $SINGLEOB

 /"

export global_hybrid_T126_namelist="

 &SETUP
   miter=1,niter(1)=5,niter(2)=150,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=82,factqmin=0.1,factqmax=0.1,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_prepb_satwnd=.false.,
   nhr_assimilation=6,lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,

   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   bkgv_write=.false.,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=2.5e7,
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=1,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,time_window_max=0.5,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                dval    dthin  dsfcalc
   prepbufr       ps          null      ps                  0.0      0     0
   prepbufr       t           null      t                   0.0      0     0
   prepbufr       q           null      q                   0.0      0     0
   prepbufr       pw          null      pw                  0.0      0     0
   prepbufr       uv          null      uv                  0.0      0     0
   satwndbufr     uv          null      uv                  0.0      0     0
   prepbufr       spd         null      spd                 0.0      0     0
   prepbufr       dw          null      dw                  0.0      0     0
   radarbufr      rw          null      rw                  0.0      0     0
   prepbufr       sst         null      sst                 0.0      0     0
   gpsrobufr      gps_bnd     null      gps                 0.0      0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi            0.0     -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi             0.0     -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16           0.0      0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17           0.0      0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18           0.0      0     0
   hirs3bufr      hirs3       n17       hirs3_n17           0.0      1     0
   hirs4bufr_skip hirs4       metop-a   hirs4_metop-a       0.0      1     1
   gimgrbufr      goes_img    g11       imgr_g11            0.0      1     0
   gimgrbufr      goes_img    g12       imgr_g12            0.0      1     0
   airsbufr       airs        aqua      airs281SUBSET_aqua  0.0      1     1
   amsuabufr_skip amsua       n15       amsua_n15           0.0      1     1
   amsuabufr_skip amsua       n18       amsua_n18           0.0      1     1
   amsuabufr_skip amsua       metop-a   amsua_metop-a       0.0      1     1
   airsbufr_skip  amsua       aqua      amsua_aqua          0.0      1     1
   amsubbufr      amsub       n17       amsub_n17           0.0      1     1
   mhsbufr_skip   mhs         n18       mhs_n18             0.0      1     1
   mhsbufr_skip   mhs         metop-a   mhs_metop-a         0.0      1     1
   ssmitbufr      ssmi        f14       ssmi_f14            0.0      1     0
   ssmitbufr      ssmi        f15       ssmi_f15            0.0      1     0
   amsrebufr      amsre_low   aqua      amsre_aqua          0.0      1     0
   amsrebufr      amsre_mid   aqua      amsre_aqua          0.0      1     0
   amsrebufr      amsre_hig   aqua      amsre_aqua          0.0      1     0
   ssmisbufr      ssmis_las   f16       ssmis_f16           0.0      1     0
   ssmisbufr      ssmis_uas   f16       ssmis_f16           0.0      1     0
   ssmisbufr      ssmis_img   f16       ssmis_f16           0.0      1     0
   ssmisbufr      ssmis_env   f16       ssmis_f16           0.0      1     0
   gsnd1bufr_skip sndrd1      g12       sndrD1_g12          0.0      1     0
   gsnd1bufr_skip sndrd2      g12       sndrD2_g12          0.0      1     0
   gsnd1bufr_skip sndrd3      g12       sndrD3_g12          0.0      1     0
   gsnd1bufr_skip sndrd4      g12       sndrD4_g12          0.0      1     0
   gsnd1bufr_skip sndrd1      g11       sndrD1_g11          0.0      1     0
   gsnd1bufr_skip sndrd2      g11       sndrD2_g11          0.0      1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11          0.0      1     0
   gsnd1bufr_skip sndrd4      g11       sndrD4_g11          0.0      1     0
   gsnd1bufr_skip sndrd1      g13       sndrD1_g13          0.0      1     0
   gsnd1bufr_skip sndrd2      g13       sndrD2_g13          0.0      1     0
   gsnd1bufr_skip sndrd3      g13       sndrD3_g13          0.0      1     0
   gsnd1bufr_skip sndrd4      g13       sndrD4_g13          0.0      1     0
   iasibufr       iasi        metop-a   iasi616_metop-a     0.0      1     1
   gomebufr       gome        metop-a   gome_metop-a        0.0      2     0
   omibufr        omi         aura      omi_aura            0.0      2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19           0.0      0     0
   hirs4bufr      hirs4       n19       hirs4_n19           0.0      1     1
   amsuabufr      amsua       n19       amsua_n19           0.0      1     1
   mhsbufr        mhs         n19       mhs_n19             0.0      1     1
   tcvitl         tcp         null      tcp                 0.0      0     0
   seviribufr     seviri      m08       seviri_m08          0.0      1     0
   seviribufr     seviri      m09       seviri_m09          0.0      1     0
   seviribufr     seviri      m10       seviri_m10          0.0      1     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b       0.0      1     0
   amsuabufr      amsua       metop-b   amsua_metop-b       0.0      1     0
   mhsbufr        mhs         metop-b   mhs_metop-b         0.0      1     0
   iasibufr       iasi        metop-b   iasi616_metop-b     0.0      1     0
   gomebufr       gome        metop-b   gome_metop-b        0.0      2     0
   atmsbufr       atms        npp       atms_npp            0.0      1     0
   crisbufr       cris        npp       cris_npp            0.0      1     0
   gsnd1bufr      sndrd1      g14       sndrD1_g14          0.0      1     0
   gsnd1bufr      sndrd2      g14       sndrD2_g14          0.0      1     0
   gsnd1bufr      sndrd3      g14       sndrD3_g14          0.0      1     0
   gsnd1bufr      sndrd4      g14       sndrD4_g14          0.0      1     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15          0.0      1     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15          0.0      1     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15          0.0      1     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15          0.0      1     0
::
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,n_ens=20,beta1_inv=0.25,s_ens_h=800,s_ens_v=-0.7,generate_ens=.false.,uv_hyb_ens=.true.,jcap_ens=62,
   nlat_ens=94,nlon_ens=192,ANISO_A_EN=.false.,jcap_ens_test=62,oz_univ_static=.true.,readin_localization=.true.,
   write_ens_sprd=.false.,
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM

 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${global_hybrid_T126_adate},
   obhourset=0.,
   $SINGLEOB
 /"

# Define namelist for RTMA runs

export RTMA_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,tsensible=.true.
   factqmin=1.0,factqmax=1.0,factv=0.1,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,diag_aero=.false.,
   nhr_assimilation=6,min_offset=180,use_compress=.false.,lrun_subdirs=.true.,
   use_prepb_satwnd=.false.
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=1.,nord_f2a=4,
   rtma_subdomain_option=.true.,triad4=.true.,nsmooth=0,nsmooth_shapiro=0,lreadnorm=.true.
 /
 &JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=0,nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,oberrflg=.false.,c_varqc=0.02,vadfile='prepbufr',
   hilbert_curve=.true.,buddycheck_t=.false.,buddydiag_save=.true.
 /
 &OBS_INPUT
   dmesh(1)=600.0,dmesh(2)=600.0,dmesh(3)=600.0,dmesh(4)=600.0,time_window_max=0.5,
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis          dval   dthin dsfcalc
   prepbufr       ps          null        ps           1.0     0      0
   prepbufr       t           null        t            1.0     0      0
   prepbufr       q           null        q            1.0     0      0
   prepbufr       uv          null        uv           1.0     0      0
   satwndbufr     uv          null        uv           1.0     0      0
   prepbufr       spd         null        spd          1.0     0      0
   prepbufr       gust        null        gust         1.0     0      0
   prepbufr       vis         null        vis          1.0     0      0
   prepbufr       wspd10m     null        wspd10m      1.0     0      0
   prepbufr       td2m        null        td2m         1.0     0      0
   prepbufr       mxtm        null        mxtm         1.0     0      0
   prepbufr       mitm        null        mitm         1.0     0      0
   prepbufr       pmsl        null        pmsl         1.0     0      0
   prepbufr       howv        null        howv         1.0     0      0
   prepbufr       tcamt       null        tcamt        1.0     0      0
::
 &SUPEROB_RADAR
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
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=1200.0,dmesh(2)=2000.0,dmesh(3)=2500.0,dmesh(4)=1000.0,time_window_max=0.5,
 /
OBS_INPUT::
!  dfile        dtype           dplat       dsis                  dval   dthin dsfcalc
   prepbufr         ps          null        ps                    0.0     0     0
   prepbufr         t           null        t                     0.0     0     0
   prepbufr         q           null        q                     0.0     0     0
   prepbufr         uv          null        uv                    0.0     0     0
   satwndbufr       uv          null        uv                    0.0     0     0
   prepbufr         spd         null        spd                   0.0     0     0
   radarbufr        rw          null        rw                    0.0     0     0
   prepbufr         dw          null        dw                    0.0     0     0
   prepbufr         sst         null        sst                   0.0     0     0
   prepbufr         pw          null        pw                    0.0     0     0
   gpsrobufr        $gps_dtype  null        gps                   0.0     0     0
   ssmirrbufr       pcp_ssmi    dmsp        pcp_ssmi              0.0    -1     0
   tmirrbufr        pcp_tmi     trmm        pcp_tmi               0.0    -1     0
   sbuvbufr         sbuv2       n16         sbuv8_n16             0.0     0     0
   sbuvbufr         sbuv2       n17         sbuv8_n17             0.0     0     0
   sbuvbufr         sbuv2       n18         sbuv8_n18             0.0     0     0
   omi              omi         aura        omi_aura              0.0     4     0
   hirs2bufr        hirs2       n14         hirs2_n14             0.0     1     1
   hirs3bufr        hirs3       n16         hirs3_n16             0.0     1     1
   hirs3bufr        hirs3       n17         hirs3_n17             0.0     1     1
   hirs4bufr        hirs4       n18         hirs4_n18             0.0     1     1
   hirs4bufr        hirs4       metop-a     hirs4_metop-a         0.0     1     1
   gsndrbufr        sndr        g11         sndr_g11              0.0     1     0
   gsndrbufr        sndr        g12         sndr_g12              0.0     1     0
   gimgrbufr        goes_img    g11         imgr_g11              0.0     1     0
   gimgrbufr        goes_img    g12         imgr_g12              0.0     1     0
   airsbufr         airs        aqua        airs281SUBSET_aqua    0.0     2     1
   msubufr          msu         n14         msu_n14               0.0     1     1
   amsuabufr        amsua       n15         amsua_n15             0.0     1     1
   amsuabufr        amsua       n16         amsua_n16             0.0     1     1
   amsuabufr        amsua       n17         amsua_n17             0.0     1     1
   amsuabufr        amsua       n18         amsua_n18             0.0     1     1
   amsuabufr        amsua       metop-a     amsua_metop-a         0.0     1     1
   airsbufr         amsua       aqua        amsua_aqua            0.0     1     1
   amsubbufr        amsub       n15         amsub_n15             0.0     1     1
   amsubbufr        amsub       n16         amsub_n16             0.0     1     1
   amsubbufr        amsub       n17         amsub_n17             0.0     1     1
   mhsbufr          mhs         n18         mhs_n18               0.0     1     1
   mhsbufr          mhs         metop-a     mhs_metop-a           0.0     1     1
   ssmitbufr        ssmi        f13         ssmi_f13              0.0     1     0
   ssmitbufr        ssmi        f14         ssmi_f14              0.0     1     0
   ssmitbufr        ssmi        f15         ssmi_f15              0.0     1     0
   amsrebufr        amsre_low   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_mid   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_hig   aqua        amsre_aqua            0.0     1     1
   ssmisbufr        ssmis       f16         ssmis_f16             0.0     1     1
   gsnd1bufr_skip   sndrd1      g12         sndrD1_g12            0.0     1     0
   gsnd1bufr        sndrd2      g12         sndrD2_g12            0.0     1     0
   gsnd1bufr_skip   sndrd3      g12         sndrD3_g12            0.0     1     0
   gsnd1bufr_skip   sndrd4      g12         sndrD4_g12            0.0     1     0
   gsnd1bufr_skip   sndrd1      g11         sndrD1_g11            0.0     1     0
   gsnd1bufr_skip   sndrd2      g11         sndrD2_g11            0.0     1     0
   gsnd1bufr        sndrd3      g11         sndrD3_g11            0.0     1     0
   gsnd1bufr_skip   sndrd4      g11         sndrD4_g11            0.0     1     0
   gsnd1bufr_skip   sndrd1      g13         sndrD1_g13            0.0     1     0
   gsnd1bufr_skip   sndrd2      g13         sndrD2_g13            0.0     1     0
   gsnd1bufr_skip   sndrd3      g13         sndrD3_g13            0.0     1     0
   gsnd1bufr_skip   sndrd4      g13         sndrD4_g13            0.0     1     0
   iasibufr         iasi        metop-a     iasi616_metop-a       0.0     3     1
   gomebufr         gome        metop-a     gome_metop-a          0.0     4     0
   mlsbufr          mls30       aura        mls30_aura            1.0     0     0
::
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
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=1200.0,dmesh(2)=3000.0,dmesh(3)=4000.0,dmesh(4)=1000.0,time_window_max=0.5,
 /
OBS_INPUT::
!  dfile        dtype           dplat       dsis                  dval   dthin dsfcalc
   prepbufr         ps          null        ps                    0.0     0     0
   prepbufr         t           null        t                     0.0     0     0
   prepbufr         q           null        q                     0.0     0     0
   prepbufr         uv          null        uv                    0.0     0     0
   satwndbufr       uv          null        uv                    0.0     0     0
   prepbufr         spd         null        spd                   0.0     0     0
   radarbufr        rw          null        rw                    0.0     0     0
   prepbufr         dw          null        dw                    0.0     0     0
   prepbufr         sst         null        sst                   0.0     0     0
   prepbufr         pw          null        pw                    0.0     0     0
   gpsrobufr        $gps_dtype  null        gps                   0.0     0     0
   ssmirrbufr       pcp_ssmi    dmsp        pcp_ssmi              0.0    -1     0
   tmirrbufr        pcp_tmi     trmm        pcp_tmi               0.0    -1     0
   sbuvbufr         sbuv2       n16         sbuv8_n16             0.0     0     0
   sbuvbufr         sbuv2       n17         sbuv8_n17             0.0     0     0
   sbuvbufr         sbuv2       n18         sbuv8_n18             0.0     0     0
   omi              omi         aura        omi_aura              0.0     4     0
   hirs2bufr        hirs2       n14         hirs2_n14             0.0     1     1
   hirs3bufr        hirs3       n16         hirs3_n16             0.0     1     1
   hirs3bufr_skip   hirs3       n17         hirs3_n17             0.0     1     1
   hirs4bufr        hirs4       n18         hirs4_n18             0.0     1     1
   hirs4bufr        hirs4       metop-a     hirs4_metop-a         0.0     1     1
   gsndrbufr        sndr        g11         sndr_g11              0.0     1     0
   gsndrbufr        sndr        g12         sndr_g12              0.0     1     0
   gimgrbufr        goes_img    g11         imgr_g11              0.0     1     0
   gimgrbufr        goes_img    g12         imgr_g12              0.0     1     0
   airsbufr         airs        aqua        airs281SUBSET_aqua    0.0     2     1
   msubufr          msu         n14         msu_n14               0.0     1     1
   amsuabufr_skip   amsua       n15         amsua_n15             0.0     1     1
   amsuabufr        amsua       n16         amsua_n16             0.0     1     1
   amsuabufr        amsua       n17         amsua_n17             0.0     1     1
   amsuabufr_skip   amsua       n18         amsua_n18             0.0     1     1
   amsuabufr        amsua       metop-a     amsua_metop-a         0.0     1     1
   airsbufr_skip    amsua       aqua        amsua_aqua            0.0     1     1
   amsubbufr        amsub       n15         amsub_n15             0.0     1     1
   amsubbufr        amsub       n16         amsub_n16             0.0     1     1
   amsubbufr        amsub       n17         amsub_n17             0.0     1     1
   mhsbufr_skip     mhs         n18         mhs_n18               0.0     1     1
   mhsbufr          mhs         metop-a     mhs_metop-a           0.0     1     1
   ssmitbufr        ssmi        f13         ssmi_f13              0.0     1     0
   ssmitbufr        ssmi        f14         ssmi_f14              0.0     1     0
   ssmitbufr        ssmi        f15         ssmi_f15              0.0     1     0
   amsrebufr        amsre_low   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_mid   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_hig   aqua        amsre_aqua            0.0     1     1
   ssmisbufr        ssmis       f16         ssmis_f16             0.0     1     1
   gsnd1bufr_skip   sndrd1      g12         sndrD1_g12            0.0     1     0
   gsnd1bufr        sndrd2      g12         sndrD2_g12            0.0     1     0
   gsnd1bufr_skip   sndrd3      g12         sndrD3_g12            0.0     1     0
   gsnd1bufr_skip   sndrd4      g12         sndrD4_g12            0.0     1     0
   gsnd1bufr_skip   sndrd1      g11         sndrD1_g11            0.0     1     0
   gsnd1bufr_skip   sndrd2      g11         sndrD2_g11            0.0     1     0
   gsnd1bufr_skip   sndrd3      g11         sndrD3_g11            0.0     1     0
   gsnd1bufr_skip   sndrd4      g11         sndrD4_g11            0.0     1     0
   gsnd1bufr_skip   sndrd1      g13         sndrD1_g13            0.0     1     0
   gsnd1bufr_skip   sndrd2      g13         sndrD2_g13            0.0     1     0
   gsnd1bufr_skip   sndrd3      g13         sndrD3_g13            0.0     1     0
   gsnd1bufr_skip   sndrd4      g13         sndrD4_g13            0.0     1     0
   iasibufr         iasi        metop-a     iasi616_metop-a       0.0     3     1
   gomebufr         gome        metop-a     gome_metop-a          0.0     4     0
   mlsbufr          mls30       aura        mls30_aura            1.0     0     0
::
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
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=1200.0,dmesh(2)=5000.0,dmesh(3)=8000.0,dmesh(4)=1000.0,time_window_max=0.5,
 /
OBS_INPUT::
!  dfile        dtype           dplat       dsis                  dval   dthin dsfcalc
   prepbufr         ps          null        ps                    0.0     0     0
   prepbufr         t           null        t                     0.0     0     0
   prepbufr         q           null        q                     0.0     0     0
   prepbufr         uv          null        uv                    0.0     0     0
   satwndbufr       uv          null        uv                    0.0     0     0
   prepbufr         spd         null        spd                   0.0     0     0
   radarbufr        rw          null        rw                    0.0     0     0
   prepbufr         dw          null        dw                    0.0     0     0
   prepbufr         sst         null        sst                   0.0     0     0
   prepbufr         pw          null        pw                    0.0     0     0
   gpsrobufr        $gps_dtype  null        gps                   0.0     0     0
   ssmirrbufr       pcp_ssmi    dmsp        pcp_ssmi              0.0    -1     0
   tmirrbufr        pcp_tmi     trmm        pcp_tmi               0.0    -1     0
   sbuvbufr         sbuv2       n16         sbuv8_n16             0.0     0     0
   sbuvbufr         sbuv2       n17         sbuv8_n17             0.0     0     0
   sbuvbufr         sbuv2       n18         sbuv8_n18             0.0     0     0
   omi              omi         aura        omi_aura              0.0     4     0
   hirs2bufr        hirs2       n14         hirs2_n14             0.0     1     1
   hirs3bufr        hirs3       n16         hirs3_n16             0.0     1     1
   hirs3bufr_skip   hirs3       n17         hirs3_n17             0.0     1     1
   hirs4bufr        hirs4       n18         hirs4_n18             0.0     1     1
   hirs4bufr        hirs4       metop-a     hirs4_metop-a         0.0     1     1
   gsndrbufr        sndr        g11         sndr_g11              0.0     1     0
   gsndrbufr        sndr        g12         sndr_g12              0.0     1     0
   gimgrbufr        goes_img    g11         imgr_g11              0.0     1     0
   gimgrbufr        goes_img    g12         imgr_g12              0.0     1     0
   airsbufr         airs        aqua        airs281SUBSET_aqua    0.0     2     1
   msubufr          msu         n14         msu_n14               0.0     1     1
   amsuabufr_skip   amsua       n15         amsua_n15             0.0     1     1
   amsuabufr        amsua       n16         amsua_n16             0.0     1     1
   amsuabufr        amsua       n17         amsua_n17             0.0     1     1
   amsuabufr_skip   amsua       n18         amsua_n18             0.0     1     1
   amsuabufr        amsua       metop-a     amsua_metop-a         0.0     1     1
   airsbufr_skip    amsua       aqua        amsua_aqua            0.0     1     1
   amsubbufr        amsub       n15         amsub_n15             0.0     1     1
   amsubbufr        amsub       n16         amsub_n16             0.0     1     1
   amsubbufr        amsub       n17         amsub_n17             0.0     1     1
   mhsbufr_skip     mhs         n18         mhs_n18               0.0     1     1
   mhsbufr          mhs         metop-a     mhs_metop-a           0.0     1     1
   ssmitbufr        ssmi        f13         ssmi_f13              0.0     1     0
   ssmitbufr        ssmi        f14         ssmi_f14              0.0     1     0
   ssmitbufr        ssmi        f15         ssmi_f15              0.0     1     0
   amsrebufr        amsre_low   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_mid   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_hig   aqua        amsre_aqua            0.0     1     1
   ssmisbufr        ssmis       f16         ssmis_f16             0.0     1     1
   gsnd1bufr_skip   sndrd1      g12         sndrD1_g12            0.0     1     0
   gsnd1bufr        sndrd2      g12         sndrD2_g12            0.0     1     0
   gsnd1bufr_skip   sndrd3      g12         sndrD3_g12            0.0     1     0
   gsnd1bufr_skip   sndrd4      g12         sndrD4_g12            0.0     1     0
   gsnd1bufr_skip   sndrd1      g11         sndrD1_g11            0.0     1     0
   gsnd1bufr_skip   sndrd2      g11         sndrD2_g11            0.0     1     0
   gsnd1bufr_skip   sndrd3      g11         sndrD3_g11            0.0     1     0
   gsnd1bufr_skip   sndrd4      g11         sndrD4_g11            0.0     1     0
   gsnd1bufr_skip   sndrd1      g13         sndrD1_g13            0.0     1     0
   gsnd1bufr_skip   sndrd2      g13         sndrD2_g13            0.0     1     0
   gsnd1bufr_skip   sndrd3      g13         sndrD3_g13            0.0     1     0
   gsnd1bufr_skip   sndrd4      g13         sndrD4_g13            0.0     1     0
   iasibufr         iasi        metop-a     iasi616_metop-a       0.0     3     1
   gomebufr         gome        metop-a     gome_metop-a          0.0     4     0
   mlsbufr          mls30       aura        mls30_aura            1.0     0     0
::
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
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,gpstop=30.,
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=1200.0,dmesh(2)=2000.0,dmesh(3)=2500.0,dmesh(4)=1000.0,time_window_max=0.5,
 /
OBS_INPUT::
!  dfile        dtype           dplat       dsis                  dval   dthin dsfcalc
   prepbufr         ps          null        ps                    0.0     0     0
   prepbufr         t           null        t                     0.0     0     0
   prepbufr         q           null        q                     0.0     0     0
   prepbufr         uv          null        uv                    0.0     0     0
   satwndbufr       uv          null        uv                    0.0     0     0
   prepbufr         spd         null        spd                   0.0     0     0
   radarbufr        rw          null        rw                    0.0     0     0
   prepbufr         dw          null        dw                    0.0     0     0
   prepbufr         sst         null        sst                   0.0     0     0
   prepbufr         pw          null        pw                    0.0     0     0
   gpsrobufr        $gps_dtype  null        gps                   0.0     0     0
   ssmirrbufr       pcp_ssmi    dmsp        pcp_ssmi              0.0    -1     0
   tmirrbufr        pcp_tmi     trmm        pcp_tmi               0.0    -1     0
   sbuvbufr         sbuv2       n16         sbuv8_n16             0.0     0     0
   sbuvbufr         sbuv2       n17         sbuv8_n17             0.0     0     0
   sbuvbufr         sbuv2       n18         sbuv8_n18             0.0     0     0
   omi              omi         aura        omi_aura              0.0     4     0
   hirs2bufr        hirs2       n14         hirs2_n14             0.0     1     1
   hirs3bufr        hirs3       n16         hirs3_n16             0.0     1     1
   hirs3bufr        hirs3       n17         hirs3_n17             0.0     1     1
   hirs4bufr        hirs4       n18         hirs4_n18             0.0     1     1
   hirs4bufr        hirs4       metop-a     hirs4_metop-a         0.0     1     1
   gsndrbufr        sndr        g11         sndr_g11              0.0     1     0
   gsndrbufr        sndr        g12         sndr_g12              0.0     1     0
   gimgrbufr        goes_img    g11         imgr_g11              0.0     1     0
   gimgrbufr        goes_img    g12         imgr_g12              0.0     1     0
   airsbufr         airs        aqua        airs281SUBSET_aqua    0.0     2     1
   msubufr          msu         n14         msu_n14               0.0     1     1
   amsuabufr        amsua       n15         amsua_n15             0.0     1     1
   amsuabufr        amsua       n16         amsua_n16             0.0     1     1
   amsuabufr        amsua       n17         amsua_n17             0.0     1     1
   amsuabufr        amsua       n18         amsua_n18             0.0     1     1
   amsuabufr        amsua       metop-a     amsua_metop-a         0.0     1     1
   airsbufr         amsua       aqua        amsua_aqua            0.0     1     1
   amsubbufr        amsub       n15         amsub_n15             0.0     1     1
   amsubbufr        amsub       n16         amsub_n16             0.0     1     1
   amsubbufr        amsub       n17         amsub_n17             0.0     1     1
   mhsbufr          mhs         n18         mhs_n18               0.0     1     1
   mhsbufr          mhs         metop-a     mhs_metop-a           0.0     1     1
   ssmitbufr        ssmi        f13         ssmi_f13              0.0     1     0
   ssmitbufr        ssmi        f14         ssmi_f14              0.0     1     0
   ssmitbufr        ssmi        f15         ssmi_f15              0.0     1     0
   amsrebufr        amsre_low   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_mid   aqua        amsre_aqua            0.0     1     1
   amsrebufr        amsre_hig   aqua        amsre_aqua            0.0     1     1
   ssmisbufr        ssmis       f16         ssmis_f16             0.0     1     1
   gsnd1bufr_skip   sndrd1      g12         sndrD1_g12            0.0     1     0
   gsnd1bufr        sndrd2      g12         sndrD2_g12            0.0     1     0
   gsnd1bufr_skip   sndrd3      g12         sndrD3_g12            0.0     1     0
   gsnd1bufr_skip   sndrd4      g12         sndrD4_g12            0.0     1     0
   gsnd1bufr_skip   sndrd1      g11         sndrD1_g11            0.0     1     0
   gsnd1bufr_skip   sndrd2      g11         sndrD2_g11            0.0     1     0
   gsnd1bufr_skip   sndrd3      g11         sndrD3_g11            0.0     1     0
   gsnd1bufr_skip   sndrd4      g11         sndrD4_g11            0.0     1     0
   gsnd1bufr_skip   sndrd1      g13         sndrD1_g13            0.0     1     0
   gsnd1bufr_skip   sndrd2      g13         sndrD2_g13            0.0     1     0
   gsnd1bufr_skip   sndrd3      g13         sndrD3_g13            0.0     1     0
   gsnd1bufr_skip   sndrd4      g13         sndrD4_g13            0.0     1     0
   iasibufr         iasi        metop-a     iasi616_metop-a       0.0     3     1
   gomebufr         gome        metop-a     gome_metop-a          0.0     4     0
   mlsbufr          mls30       aura        mls30_aura            1.0     0     0
::
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
   oblat=45.,oblon=270.,obpres=850.,obdattim=${adate},
   obhourset=0.,
 /"

# Define namelist for nems nmmb run

export nems_nmmb_namelist="

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,niter_no_qc(1)=1,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,nsig_ext=13,preserve_restart_date=.true.,
   use_gfs_ozone=.true.,check_gfs_ozone_date=.true.,regional_ozone=.true.,gpstop=30.
   lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=8,period_max=3.,
    baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,
   vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=1200.0,dmesh(2)=2000.0,dmesh(3)=2500.0,dmesh(4)=1000.0,time_window_max=0.5,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile        dtype           dplat       dsis              dval   dthin dsfcalc
   prepbufr         ps          null      ps                  0.0     0     0
   prepbufr         t           null      t                   0.0     0     0
   prepbufr         q           null      q                   0.0     0     0
   prepbufr         pw          null      pw                  0.0     0     0
   prepbufr         uv          null      uv                  0.0     0     0
   satwndbufr       uv          null      uv                  0.0     0     0
   prepbufr         spd         null      spd                 0.0     0     0
   prepbufr         dw          null      dw                  0.0     0     0
   radarbufr        rw          null      rw                  0.0     0     0
   prepbufr         sst         null      sst                 0.0     0     0
   gpsrobufr        $gps_dtype  null      gps                 0.0     0     0
   ssmirrbufr       pcp_ssmi    dmsp      pcp_ssmi            0.0    -1     0
   tmirrbufr        pcp_tmi     trmm      pcp_tmi             0.0    -1     0
   sbuvbufr         sbuv2       n16       sbuv8_n16           0.0     0     0
   sbuvbufr         sbuv2       n17       sbuv8_n17           0.0     0     0
   sbuvbufr         sbuv2       n18       sbuv8_n18           0.0     0     0
   hirs2bufr        hirs2       n14       hirs2_n14           0.0     1     1
   hirs3bufr_skip   hirs3       n16       hirs3_n16           0.0     1     1
   hirs3bufr        hirs3       n17       hirs3_n17           0.0     1     1
   hirs4bufr_skip   hirs4       n18       hirs4_n18           0.0     1     1
   hirs4bufr        hirs4       metop-a   hirs4_metop-a       0.0     1     1
   gsndrbufr        sndr        g11       sndr_g11            0.0     1     0
   gsndrbufr        sndr        g12       sndr_g12            0.0     1     0
   gimgrbufr        goes_img    g11       imgr_g11            0.0     1     0
   gimgrbufr        goes_img    g12       imgr_g12            0.0     1     0
   airsbufr         airs        aqua      airs281SUBSET_aqua  0.0     2     1
   msubufr          msu         n14       msu_n14             0.0     1     1
   amsuabufr_skip   amsua       n15       amsua_n15           0.0     1     1
   amsuabufr_skip   amsua       n16       amsua_n16           0.0     1     1
   amsuabufr_skip   amsua       n17       amsua_n17           0.0     1     1
   amsuabufr_skip   amsua       n18       amsua_n18           0.0     1     1
   amsuabufr        amsua       metop-a   amsua_metop-a       0.0     1     1
   airsbufr_skip    amsua       aqua      amsua_aqua          0.0     1     1
   amsubbufr_skip   amsub       n15       amsub_n15           0.0     1     1
   amsubbufr_skip   amsub       n16       amsub_n16           0.0     1     1
   amsubbufr        amsub       n17       amsub_n17           0.0     1     1
   mhsbufr_skip     mhs         n18       mhs_n18             0.0     1     1
   mhsbufr          mhs         metop-a   mhs_metop-a         0.0     1     1
   ssmitbufr        ssmi        f13       ssmi_f13            0.0     1     0
   ssmitbufr        ssmi        f14       ssmi_f14            0.0     1     0
   ssmitbufr        ssmi        f15       ssmi_f15            0.0     1     0
   amsrebufr        amsre_low   aqua      amsre_aqua          0.0     1     1
   amsrebufr        amsre_mid   aqua      amsre_aqua          0.0     1     1
   amsrebufr        amsre_hig   aqua      amsre_aqua          0.0     1     1
   ssmisbufr        ssmis       f16       ssmis_f16           0.0     1     1
   gsnd1bufr_skip   sndrd1      g12       sndrD1_g12          0.0     1     0
   gsnd1bufr_skip   sndrd2      g12       sndrD2_g12          0.0     1     0
   gsnd1bufr_skip   sndrd3      g12       sndrD3_g12          0.0     1     0
   gsnd1bufr_skip   sndrd4      g12       sndrD4_g12          0.0     1     0
   gsnd1bufr_skip   sndrd1      g11       sndrD1_g11          0.0     1     0
   gsnd1bufr_skip   sndrd2      g11       sndrD2_g11          0.0     1     0
   gsnd1bufr        sndrd3      g11       sndrD3_g11          0.0     1     0
   gsnd1bufr_skip   sndrd4      g11       sndrD4_g11          0.0     1     0
   gsnd1bufr_skip   sndrd1      g13       sndrD1_g13          0.0     1     0
   gsnd1bufr_skip   sndrd2      g13       sndrD2_g13          0.0     1     0
   gsnd1bufr_skip   sndrd3      g13       sndrD3_g13          0.0     1     0
   gsnd1bufr_skip   sndrd4      g13       sndrD4_g13          0.0     1     0
   iasibufr         iasi        metop-a   iasi616_metop-a     0.0     3     1
   gomebufr         gome        metop-a   gome_metop-a        0.0     4     0
   omibufr          omi         aura      omi_aura            0.0     4     0
   sbuvbufr         sbuv2       n19       sbuv8_n19           0.0     0     0
   hirs4bufr        hirs4       n19       hirs4_n19           0.0     1     1
   amsuabufr        amsua       n19       amsua_n19           0.0     1     1
   mhsbufr          mhs         n19       mhs_n19             0.0     1     1
   tcvitl           tcp         null      tcp                 0.0     0     0
   mlsbufr          mls30       aura      mls30_aura          1.0     0     0
::
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
   full_ensemble=.true.,betaflg=.true.,pwgtflg=.true.,
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
 /"

# Define namelist for 4DEnVar nems nmmb run

export nems_nmmb_4denvar_namelist="

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,niter_no_qc(1)=1,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,
   iguess=-1,use_gfs_ozone=.true.,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=6,l_foto=.false.,
   use_pbl=.false.,gpstop=30.,
   print_diag_pcg=.true.,
   newpc4pred=.true., adp_anglebc=.true., angord=4,
   passive_bc=.false., use_edges=.false., emiss_bc=.true.,
   diag_precon=.true., step_start=1.e-3,
   l4densvar=.true.,nhr_obsbin=3,
   $SETUP
 /
 &GRIDOPTS
   wrf_nmm_regional=.false.,wrf_mass_regional=.false.,nems_nmmb_regional=.true.,diagnostic_reg=.false.,
   nmmb_reference_grid='H',grid_ratio_nmmb=3.0,
   filled_grid=.false.,half_grid=.false.,netcdf=.false.,nvege_type=20,
   $GRIDOPTS
 /
 &BKGERR
   hzscl=0.373,0.746,1.50,
   vs=1.0,bw=0.,fstat=.true.,
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
   nstrong=0,nvmodes_keep=20,period_max=3.,
    baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,
   vadfile='prepbufr',
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=120.0,time_window_max=3.0,ext_sonde=.true.,
   $OBS_INPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                  dval    dthin  dsfcalc
   prepbufr       ps          null        ps                    0.0      0      0
   prepbufr       t           null        t                     0.0      0      0
   prepbufr       q           null        q                     0.0      0      0
   prepbufr       pw          null        pw                    0.0      0      0
   prepbufr       uv          null        uv                    0.0      0      0
   prepbufr       spd         null        spd                   0.0      0      0
   prepbufr       dw          null        dw                    0.0      0      0
   radarbufr      rw          null        rw                    0.0      0      0
   prepbufr       sst         null        sst                   0.0      0      0
   gpsrobufr      gps_bnd     null        gps_bnd               0.0      0      0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi              0.0     -1      0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi               0.0     -1      0
   sbuvbufr       sbuv2       n16         sbuv8_n16             0.0      0      0
   sbuvbufr       sbuv2       n17         sbuv8_n17             0.0      0      0
   sbuvbufr       sbuv2       n18         sbuv8_n18             0.0      0      0
   hirs2bufr      hirs2       n14         hirs2_n14             0.0      1      0
   hirs3bufr_skip hirs3       n16         hirs3_n16             0.0      1      0
   hirs3bufr      hirs3       n17         hirs3_n17             0.0      1      0
   hirs4bufr_skip hirs4       n18         hirs4_n18             0.0      1      0
   hirs4bufr      hirs4       metop-a     hirs4_metop-a         0.0      1      0
   gsndrbufr      sndr        g11         sndr_g11              0.0      1      0
   gsndrbufr      sndr        g12         sndr_g12              0.0      1      0
   gimgrbufr      goes_img    g11         imgr_g11              0.0      1      0
   gimgrbufr      goes_img    g12         imgr_g12              0.0      1      0
   airsbufr       airs        aqua        airs281SUBSET_aqua    0.0      1      0
   msubufr        msu         n14         msu_n14               0.0      1      0
   amsuabufr_skip amsua       n15         amsua_n15             0.0      1      0
   amsuabufr_skip amsua       n16         amsua_n16             0.0      1      0
   amsuabufr_skip amsua       n17         amsua_n17             0.0      1      0
   amsuabufr_skip amsua       n18         amsua_n18             0.0      1      0
   amsuabufr      amsua       metop-a     amsua_metop-a         0.0      1      0
   airsbufr_skip  amsua       aqua        amsua_aqua            0.0      1      0
   amsubbufr_skip amsub       n15         amsub_n15             0.0      1      0
   amsubbufr_skip amsub       n16         amsub_n16             0.0      1      0
   amsubbufr      amsub       n17         amsub_n17             0.0      1      0
   mhsbufr_skip   mhs         n18         mhs_n18               0.0      1      0
   mhsbufr        mhs         metop-a     mhs_metop-a           0.0      1      0
   ssmitbufr      ssmi        f13         ssmi_f13              0.0      1      0
   ssmitbufr      ssmi        f14         ssmi_f14              0.0      1      0
   ssmitbufr      ssmi        f15         ssmi_f15              0.0      1      0
   amsrebufr      amsre_low   aqua        amsre_aqua            0.0      1      0
   amsrebufr      amsre_mid   aqua        amsre_aqua            0.0      1      0
   amsrebufr      amsre_hig   aqua        amsre_aqua            0.0      1      0
   ssmisbufr      ssmis       f16         ssmis_f16             0.0      1      0
   gsnd1bufr_skip sndrd1      g12         sndrD1_g12            0.0      1      0
   gsnd1bufr_skip sndrd2      g12         sndrD2_g12            0.0      1      0
   gsnd1bufr_skip sndrd3      g12         sndrD3_g12            0.0      1      0
   gsnd1bufr_skip sndrd4      g12         sndrD4_g12            0.0      1      0
   gsnd1bufr_skip sndrd1      g11         sndrD1_g11            0.0      1      0
   gsnd1bufr_skip sndrd2      g11         sndrD2_g11            0.0      1      0
   gsnd1bufr      sndrd3      g11         sndrD3_g11            0.0      1      0
   gsnd1bufr_skip sndrd4      g11         sndrD4_g11            0.0      1      0
   gsnd1bufr_skip sndrd1      g13         sndrD1_g13            0.0      1      0
   gsnd1bufr_skip sndrd2      g13         sndrD2_g13            0.0      1      0
   gsnd1bufr_skip sndrd3      g13         sndrD3_g13            0.0      1      0
   gsnd1bufr_skip sndrd4      g13         sndrD4_g13            0.0      1      0
   iasibufr       iasi        metop-a     iasi616_metop-a       0.0      1      0
   gomebufr       gome        metop-a     gome_metop-a          0.0      1      0
   omibufr        omi         aura        omi_aura              0.0      1      0
   sbuvbufr       sbuv2       n19         sbuv8_n19             0.0      1      0
   hirs4bufr      hirs4       n19         hirs4_n19             0.0      1      0
   amsuabufr      amsua       n19         amsua_n19             0.0      1      0
   mhsbufr        mhs         n19         mhs_n19               0.0      1      0
   tcvitl         tcp         null        tcp                   0.0      0      0
   satwndbufr     uv          null        uv                    0.0      0      0
   gsnd1bufr      sndrd1      g15         sndrD1_g15            0.0      1      0
   gsnd1bufr      sndrd2      g15         sndrD2_g15            0.0      1      0
   gsnd1bufr      sndrd3      g15         sndrD3_g15            0.0      1      0
   gsnd1bufr      sndrd4      g15         sndrD4_g15            0.0      1      0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
   $SUPEROB_RADAR
 /
 &LAG_DATA
   $LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,
   n_ens=10,
   uv_hyb_ens=.true.,
   beta1_inv=0.25,
   s_ens_h=800,
   s_ens_v=5,
   generate_ens=.false.,
   regional_ensemble_option=1,
   aniso_a_en=.false.,
   nlon_ens=0,
   nlat_ens=0,
   jcap_ens=0,
   jcap_ens_test=0,coef_bw=0.5,
   full_ensemble=.true.,betaflg=.true.,pwgtflg=.true.,
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   $RAPIDREFRESH_CLDSURF
 /
 &CHEM
   $CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=270.,obpres=850.,obdattim=2015031300,
   obhourset=0.,
   $SINGLEOB_TEST
 /"

# Define namelist for cmaq binary run

 export cmaq_binary_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_compress=.false.,
   diag_conv=.true.,lrun_subdirs=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   tlnmc_option=0,nstrong=0,nvmodes_keep=20,
   period_max=3.,baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,
   dmesh(5)=120,time_window_max=1.5,
 /
OBS_INPUT::
!  dfile        dtype       dplat       dsis          dval   dthin dsfcalc
   anowbufr     pm2_5       null        TEOM          1.0    0     0
::
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
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
   berror_chem=.true.,
   oneobtest_chem=.false.,
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

export hwrf_nmm_d2_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,niter_no_qc(1)=20,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=6,l_foto=.false.,
   use_pbl=.true.,use_compress=.false.,
   print_diag_pcg=.true.,
   use_gfs_stratosphere=$USE_GFS_STRATOSPHERE,
   use_gfs_ozone=$USE_GFS_OZONE,
   regional_ozone=$REGIONAL_OZONE,
   nsig_ext=12,gpstop=50.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$NLON,nsig=$LEVS,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,
   diagnostic_reg=.true.,
   filled_grid=.false.,half_grid=.true.,netcdf=$NETCDF,
 /
 &BKGERR
   hzscl=0.25,0.5,1.0,
   vs=0.6,bw=0.,fstat=.false.,
 /
 &ANBKGERR
   anisotropic=.false.,an_vs=1.0,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=4.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=0,reg_tlnmc_type=1,nstrong=1,nvmodes_keep=8,period_max=6.,
   period_width=1.5,baldiag_full=.false.,baldiag_inc=.false.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,erradar_inflate=1.0,tdrerr_inflate=.true.,tdrgross_fact=0.4,
   noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=90.0,dmesh(2)=45.0,dmesh(3)=45.0,dmesh(4)=45.0,dmesh(5)=90,dmesh(7)=9.0,time_window_max=3.0,l_foreaft_thin=.false.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                dval    dthin  dsfcalc
   prepbufr       ps          null      ps                  0.0      0     0
   prepbufr       t           null      t                   0.0      0     0
   prepbufr       q           null      q                   0.0      0     0
   prepbufr       pw          null      pw                  0.0      0     0
   prepbufr       uv          null      uv                  0.0      0     0
   satwndbufr     uv          null      uv                  0.0      0     0
   prepbufr       spd         null      spd                 0.0      0     0
   prepbufr       dw          null      dw                  0.0      0     0
   radarbufr      rw          null      rw                  0.0      0     0
   prepbufr       sst         null      sst                 0.0      0     0
   tcvitl         tcp         null      tcp                 0.0      0     0
   tldplrbufr     rw          null      rw                  0.0      7     0
   hdobbufr       uv          null      uv                  0.0      0     0
   hdobbufr       t           null      t                   0.0      0     0
   hdobbufr       q           null      q                   0.0      0     0
   hdobbufr       spd         null      spd                 0.0      0     0
   gpsrobufr      gps_bnd     null      gps                 0.0      0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi            0.0     -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi             0.0     -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16           0.0      0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17           0.0      0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18           0.0      0     0
   hirs3bufr_skip hirs3       n17       hirs3_n17           0.0      1     0
   hirs4bufr      hirs4       metop-a   hirs4_metop-a       0.0      1     1
   gimgrbufr      goes_img    g11       imgr_g11            0.0      1     0
   gimgrbufr      goes_img    g12       imgr_g12            0.0      1     0
   airsbufr       airs        aqua      airs281SUBSET_aqua  0.0      1     1
   amsuabufr_skip amsua       n15       amsua_n15           0.0      2     1
   amsuabufr_skip amsua       n18       amsua_n18           0.0      2     1
   amsuabufr      amsua       metop-a   amsua_metop-a       0.0      2     1
   airsbufr_skip  amsua       aqua      amsua_aqua          0.0      2     1
   amsubbufr      amsub       n17       amsub_n17           0.0      3     1
   mhsbufr_skip   mhs         n18       mhs_n18             0.0      3     1
   mhsbufr        mhs         metop-a   mhs_metop-a         0.0      3     1
   ssmitbufr      ssmi        f14       ssmi_f14            0.0      1     0
   ssmitbufr      ssmi        f15       ssmi_f15            0.0      1     0
   amsrebufr      amsre_low   aqua      amsre_aqua          0.0      4     0
   amsrebufr      amsre_mid   aqua      amsre_aqua          0.0      4     0
   amsrebufr      amsre_hig   aqua      amsre_aqua          0.0      4     0
   ssmisbufr      ssmis       f16       ssmis_f16           0.0      4     0
   ssmisbufr      ssmis       f17       ssmis_f17           0.0      4     0
   ssmisbufr      ssmis       f18       ssmis_f18           0.0      4     0
   ssmisbufr      ssmis       f19       ssmis_f19           0.0      4     0
   gsnd1bufr_skip sndrd1      g12       sndrD1_g12          0.0      5     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12          0.0      5     0
   gsnd1bufr_skip sndrd3      g12       sndrD3_g12          0.0      5     0
   gsnd1bufr_skip sndrd4      g12       sndrD4_g12          0.0      5     0
   gsnd1bufr_skip sndrd1      g11       sndrD1_g11          0.0      5     0
   gsnd1bufr_skip sndrd2      g11       sndrD2_g11          0.0      5     0
   gsnd1bufr_skip sndrd3      g11       sndrD3_g11          0.0      5     0
   gsnd1bufr_skip sndrd4      g11       sndrD4_g11          0.0      5     0
   gsnd1bufr_skip sndrd1      g13       sndrD1_g13          0.0      5     0
   gsnd1bufr_skip sndrd2      g13       sndrD2_g13          0.0      5     0
   gsnd1bufr_skip sndrd3      g13       sndrD3_g13          0.0      5     0
   gsnd1bufr_skip sndrd4      g13       sndrD4_g13          0.0      5     0
   iasibufr       iasi        metop-a   iasi616_metop-a     0.0      1     1
   gomebufr       gome        metop-a   gome_metop-a        0.0      2     0
   omibufr        omi         aura      omi_aura            0.0      2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19           0.0      0     0
   hirs4bufr      hirs4       n19       hirs4_n19           0.0      1     1
   amsuabufr      amsua       n19       amsua_n19           0.0      2     1
   mhsbufr        mhs         n19       mhs_n19             0.0      3     1
   seviribufr     seviri      m08       seviri_m08          0.0      1     0
   seviribufr     seviri      m09       seviri_m09          0.0      1     0
   seviribufr     seviri      m10       seviri_m10          0.0      1     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b       0.0      1     0
   amsuabufr      amsua       metop-b   amsua_metop-b       0.0      2     0
   mhsbufr        mhs         metop-b   mhs_metop-b         0.0      3     0
   iasibufr       iasi        metop-b   iasi616_metop-b     0.0      1     0
   gomebufr       gome        metop-b   gome_metop-b        0.0      2     0
   atmsbufr       atms        npp       atms_npp            0.0      2     0
   crisbufr       cris        npp       cris_npp            0.0      1     0
   gsnd1bufr      sndrd1      g14       sndrD1_g14          0.0      5     0
   gsnd1bufr      sndrd2      g14       sndrD2_g14          0.0      5     0
   gsnd1bufr      sndrd3      g14       sndrD3_g14          0.0      5     0
   gsnd1bufr      sndrd4      g14       sndrD4_g14          0.0      5     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15          0.0      5     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15          0.0      5     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15          0.0      5     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15          0.0      5     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,n_ens=80,uv_hyb_ens=.true.,beta1_inv=0.2,
   s_ens_h=300,s_ens_v=-0.5,readin_localization=.false.,
   generate_ens=.false.,regional_ensemble_option=1,grid_ratio_ens=1,
   pseudo_hybens=.false.,merge_two_grid_ensperts=.false.,
   pwgtflg=.false.,betaflg=.false.,aniso_a_en=.false.,
   nlon_ens=165,nlat_ens=335,jcap_ens=0,jcap_ens_test=0,
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${adate},
   obhourset=0.,
 /"

export hwrf_nmm_d3_namelist="

 &SETUP
   miter=2,niter(1)=1,niter(2)=2,niter_no_qc(1)=20,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=6,l_foto=.false.,
   use_pbl=.true.,use_compress=.false.,
   print_diag_pcg=.true.,
   use_gfs_stratosphere=$USE_GFS_STRATOSPHERE,
   use_gfs_ozone=$USE_GFS_OZONE,
   regional_ozone=$REGIONAL_OZONE,
   nsig_ext=12,gpstop=50.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$NLON,nsig=$LEVS,
   wrf_nmm_regional=.true.,wrf_mass_regional=.false.,
   diagnostic_reg=.true.,
   filled_grid=.false.,half_grid=.true.,netcdf=$NETCDF,
 /
 &BKGERR
   hzscl=0.2,0.4,0.8,
   vs=0.6,bw=0.,fstat=.false.,
 /
 &ANBKGERR
   anisotropic=.false.,an_vs=1.0,ngauss=1,
   an_flen_u=-5.,an_flen_t=3.,an_flen_z=-200.,
   ifilt_ord=2,npass=3,normal=-200,grid_ratio=4.,nord_f2a=4,
 /
 &JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=0,reg_tlnmc_type=1,nstrong=1,nvmodes_keep=8,period_max=6.,
   period_width=1.5,baldiag_full=.false.,baldiag_inc=.false.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,erradar_inflate=1.0,tdrerr_inflate=.true.,tdrgross_fact=0.4,
   noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=90.0,dmesh(2)=45.0,dmesh(3)=45.0,dmesh(4)=45.0,dmesh(5)=90,dmesh(7)=9.0,time_window_max=3.0,l_foreaft_thin=.false.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                dval    dthin  dsfcalc
   prepbufr       ps          null      ps                  0.0      0     0
   prepbufr       t           null      t                   0.0      0     0
   prepbufr       q           null      q                   0.0      0     0
   prepbufr       pw          null      pw                  0.0      0     0
   prepbufr       uv          null      uv                  0.0      0     0
   satwndbufr     uv          null      uv                  0.0      0     0
   prepbufr       spd         null      spd                 0.0      0     0
   prepbufr       dw          null      dw                  0.0      0     0
   radarbufr      rw          null      rw                  0.0      0     0
   prepbufr       sst         null      sst                 0.0      0     0
   tcvitl         tcp         null      tcp                 0.0      0     0
   tldplrbufr     rw          null      rw                  0.0      7     0
   hdobbufr       uv          null      uv                  0.0      0     0
   hdobbufr       t           null      t                   0.0      0     0
   hdobbufr       q           null      q                   0.0      0     0
   hdobbufr       spd         null      spd                 0.0      0     0
   gpsrobufr      gps_bnd     null      gps                 0.0      0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi            0.0     -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi             0.0     -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16           0.0      0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17           0.0      0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18           0.0      0     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,n_ens=80,uv_hyb_ens=.true.,beta1_inv=0.2,
   s_ens_h=150,s_ens_v=-0.5,readin_localization=.false.,
   generate_ens=.false.,regional_ensemble_option=1,grid_ratio_ens=1,
   pseudo_hybens=.false.,merge_two_grid_ensperts=.false.,
   pwgtflg=.false.,betaflg=.false.,aniso_a_en=.false.,
   nlon_ens=249,nlat_ens=499,jcap_ens=0,jcap_ens_test=0,
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${adate},
   obhourset=0.,
 /"
