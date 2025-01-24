regtest=$1

case $regtest in

    global_4denvar)

# Define namelist for global hybrid run

export gsi_namelist=" 

 &SETUP
   miter=2,niter(1)=5,niter(2)=5,
   niter_no_qc(1)=2,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=0,factqmin=0.5,factqmax=0.0002,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=45,gpstop=50.,
   commgpstop=45.,commgpserrinf=1.0,
   use_gfs_nemsio=.false.,lrun_subdirs=.true.,
   use_readin_anl_sfcmask=.true.,
   crtm_coeffs_path='./crtm_coeffs/',
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
   diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,thin4d=.true.,cwoption=3,
   verbose=.false.,imp_physics=8,lupp=.true.,
   binary_diag=.false.,netcdf_diag=.true.,
   lobsdiag_forenkf=.false.,
   nhr_anal=3,6,9,nhr_obsbin=1,
   l4densvar=.true.,ens_nstarthr=3,nhr_assimilation=6,lwrite4danl=.true.,
   optconv=0.05,cao_check=.true.,ta2tb=.true.,
   tzr_qc=1,sfcnst_comb=.true.,
   write_fv3_incr=.true.,incvars_to_zero= 'liq_wat_inc','icmr_inc','rwmr_inc','snmr_inc','grle_inc',
   incvars_zero_strat='sphum_inc','liq_wat_inc','icmr_inc','rwmr_inc','snmr_inc','grle_inc',incvars_efold=5, use_gfs_ncio=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP=$JCAP,JCAP_B=$JCAP_B,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
   regional=.false.,
   $GRIDOPTS   
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   bkgv_write=.false.,
   cwcoveqqcov=.false.,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR   
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
   ljc4tlevs=.true.,    
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=3,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   
   baldiag_full=.false.,baldiag_inc=.false.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.04,
   use_poq7=.true.,qc_noirjaco3_pole=.true.,vqc=.false.,nvqc=.true.,hub_norm=.true.,
   aircraft_t_bc=.true.,biaspredt=1.0e5,upd_aircraft=.true.,cleanup_tail=.true.,
   tcp_width=70.0,tcp_ermax=7.35,cris_cads=.true.,iasi_cads=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,dmesh(3)=1000.0,dmesh(4)=500.0,time_window_max=3.0,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                dval    dthin dsfcalc
   prepbufr       ps          null        ps                  0.0     0     0
   prepbufr       t           null        t                   0.0     0     0
   prepbufr_profl t           null        t                   0.0     0     0
   hdobbufr       t           null        t                   0.0     0     0
   prepbufr       q           null        q                   0.0     0     0
   prepbufr_profl q           null        q                   0.0     0     0
   hdobbufr       q           null        q                   0.0     0     0
   prepbufr       pw          null        pw                  0.0     0     0
   prepbufr       uv          null        uv                  0.0     0     0
   prepbufr_profl uv          null        uv                  0.0     0     0
   satwndbufr     uv          null        uv                  0.0     0     0
   hdobbufr       uv          null        uv                  0.0     0     0
   prepbufr       spd         null        spd                 0.0     0     0
   hdobbufr       spd         null        spd                 0.0     0     0
   prepbufr       dw          null        dw                  0.0     0     0
   radarbufr      rw          null        rw                  0.0     0     0
   nsstbufr       sst         nsst        sst                 0.0     0     0
   gpsrobufr      gps_bnd     null        gps                 0.0     0     0
   sbuvbufr       sbuv2       n16         sbuv8_n16           0.0     0     0
   sbuvbufr       sbuv2       n17         sbuv8_n17           0.0     0     0
   sbuvbufr       sbuv2       n18         sbuv8_n18           0.0     0     0
   gimgrbufr      goes_img    g11         imgr_g11            0.0     1     0
   gimgrbufr      goes_img    g12         imgr_g12            0.0     1     0
   airsbufr       airs        aqua        airs_aqua           0.0     1     1
   amsuabufr      amsua       n15         amsua_n15           0.0     1     1
   amsuabufr      amsua       n18         amsua_n18           0.0     1     1
   amsuabufr      amsua       metop-a     amsua_metop-a       0.0     1     1
   airsbufr       amsua       aqua        amsua_aqua          0.0     1     1
   amsubbufr      amsub       n17         amsub_n17           0.0     1     1
   mhsbufr        mhs         n18         mhs_n18             0.0     1     1
   mhsbufr        mhs         metop-a     mhs_metop-a         0.0     1     1
   ssmitbufr      ssmi        f15         ssmi_f15            0.0     1     0
   amsrebufr      amsre_low   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_mid   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_hig   aqua        amsre_aqua          0.0     1     0
   ssmisbufr      ssmis       f16         ssmis_f16           0.0     1     0
   ssmisbufr      ssmis       f17         ssmis_f17           0.0     1     0
   ssmisbufr      ssmis       f18         ssmis_f18           0.0     1     0
   gsnd1bufr      sndrd1      g12         sndrD1_g12          0.0     1     0
   gsnd1bufr      sndrd2      g12         sndrD2_g12          0.0     1     0
   gsnd1bufr      sndrd3      g12         sndrD3_g12          0.0     1     0
   gsnd1bufr      sndrd4      g12         sndrD4_g12          0.0     1     0
   gsnd1bufr      sndrd1      g11         sndrD1_g11          0.0     1     0
   gsnd1bufr      sndrd2      g11         sndrD2_g11          0.0     1     0
   gsnd1bufr      sndrd3      g11         sndrD3_g11          0.0     1     0
   gsnd1bufr      sndrd4      g11         sndrD4_g11          0.0     1     0
   gsnd1bufr      sndrd1      g13         sndrD1_g13          0.0     1     0
   gsnd1bufr      sndrd2      g13         sndrD2_g13          0.0     1     0
   gsnd1bufr      sndrd3      g13         sndrD3_g13          0.0     1     0
   gsnd1bufr      sndrd4      g13         sndrD4_g13          0.0     1     0
   iasibufr       iasi        metop-a     iasi_metop-a        0.0     1     1
   gomebufr       gome        metop-a     gome_metop-a        0.0     2     0
   omibufr        omi         aura        omi_aura            0.0     2     0
   sbuvbufr       sbuv2       n19         sbuv8_n19           0.0     0     0
   amsuabufr      amsua       n19         amsua_n19           0.0     1     1
   mhsbufr        mhs         n19         mhs_n19             0.0     1     1
   tcvitl         tcp         null        tcp                 0.0     0     0
   seviribufr     seviri      m08         seviri_m08          0.0     1     0
   seviribufr     seviri      m09         seviri_m09          0.0     1     0
   seviribufr     seviri      m10         seviri_m10          0.0     1     0
   seviribufr     seviri      m11         seviri_m11          0.0     1     0
   amsuabufr      amsua       metop-b     amsua_metop-b       0.0     1     1
   mhsbufr        mhs         metop-b     mhs_metop-b         0.0     1     1
   iasibufr       iasi        metop-b     iasi_metop-b        0.0     1     1
   gomebufr       gome        metop-b     gome_metop-b        0.0     2     0
   atmsbufr       atms        npp         atms_npp            0.0     1     1
   atmsbufr       atms        n20         atms_n20            0.0     1     1
   crisbufr       cris        npp         cris_npp            0.0     1     0
   crisfsbufr     cris-fsr    npp         cris-fsr_npp        0.0     1     0
   crisfsbufr     cris-fsr    n20         cris-fsr_n20        0.0     1     0
   gsnd1bufr      sndrd1      g14         sndrD1_g14          0.0     1     0
   gsnd1bufr      sndrd2      g14         sndrD2_g14          0.0     1     0
   gsnd1bufr      sndrd3      g14         sndrD3_g14          0.0     1     0
   gsnd1bufr      sndrd4      g14         sndrD4_g14          0.0     1     0
   gsnd1bufr      sndrd1      g15         sndrD1_g15          0.0     1     0
   gsnd1bufr      sndrd2      g15         sndrD2_g15          0.0     1     0
   gsnd1bufr      sndrd3      g15         sndrD3_g15          0.0     1     0
   gsnd1bufr      sndrd4      g15         sndrD4_g15          0.0     1     0
   oscatbufr      uv          null        uv                  0.0     0     0
   mlsbufr        mls30       aura        mls30_aura          0.0     0     0
   avhambufr      avhrr       metop-a     avhrr3_metop-a      0.0     4     0
   avhpmbufr      avhrr       n18         avhrr3_n18          0.0     4     0
   avhambufr      avhrr       metop-b     avhrr3_metop-b      0.0     4     0
   avhambufr      avhrr       metop-c     avhrr3_metop-c      0.0     4     0
   avhpmbufr      avhrr       n19         avhrr3_n19          0.0     4     0
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     1     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     1     0
   abibufr        abi         g16         abi_g16             0.0     1     0
   abibufr        abi         g17         abi_g17             0.0     1     0
   rapidscatbufr  uv          null        uv                  0.0     0     0
   ompsnpbufr     ompsnp      npp         ompsnp_npp          0.0     0     0
   ompslpbufr     ompslp      npp         ompslp_npp          0.0     0     0
   ompstcbufr     ompstc8     npp         ompstc8_npp         0.0     2     0
   ompsnpbufr     ompsnp      n20         ompsnp_n20          0.0     0     0
   ompstcbufr     ompstc8     n20         ompstc8_n20         0.0     2     0
   amsuabufr      amsua       metop-c     amsua_metop-c       0.0     1     1
   mhsbufr        mhs         metop-c     mhs_metop-c         0.0     1     1
   iasibufr       iasi        metop-c     iasi_metop-c        0.0     1     1
   sstviirs       viirs-m     npp         viirs-m_npp         0.0     4     0
   sstviirs       viirs-m     j1          viirs-m_j1          0.0     4     0
   abibufr        abi         g18         abi_g18             0.0     1     0
   ahibufr        ahi         himawari9   ahi_himawari9       0.0     1     0
   atmsbufr       atms        n21         atms_n21            0.0     1     1
   crisfsbufr     cris-fsr    n21         cris-fsr_n21        0.0     1     0
   sstviirs       viirs-m     j2          viirs-m_j2          0.0     4     0
   ompsnpbufr     ompsnp      n21         ompsnp_n21          0.0     0     0
   ompstcbufr     ompstc8     n21         ompstc8_n21         0.0     2     0
   gomebufr       gome        metop-c     gome_metop-c        0.0     2     0
::
  &SUPEROB_RADAR
   $SUPERRAD   
 /
  &LAG_DATA
   
 /
  &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,n_ens=$NMEM_ENKF,beta_s0=0.125,readin_beta=.false.,
   generate_ens=.false.,uv_hyb_ens=.true.,jcap_ens=48,nlat_ens=98,nlon_ens=192,
   ANISO_A_EN=.false.,jcap_ens_test=48,oz_univ_static=.false.,
   ensemble_path='./ensemble_data/',ens_fast_read=.true.,write_ens_sprd=.false.,
   s_ens_h=1000.0,450.0,685.0,s_ens_v=-0.5,-0.5,0.0,readin_localization=.false.,
   global_spectral_filter_sd=.false.,r_ensloccov4scl=1,nsclgrp=2,naensloc=3,
   $HYBRID_ENSEMBLE
 /
  &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
   
 /
  &CHEM
   
 /
  &NST
   nst_gsi=3,nstinfo=4,zsea1=0,zsea2=0,fac_dtl=1,fac_tsl=1,
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=5.,oblon=180.,obpres=850.,obdattim=2022110900,
   obhourset=0.,
 /
"
;;
    RTMA)

# Define namelist for RTMA runs

export gsi_namelist="

 &SETUP
   miter=2,niter(1)=5,niter(2)=5,
   write_diag(1)=.true.,write_diag(2)=.true.,write_diag(3)=.true.,
   gencode=78,qoption=1,tsensible=.true.
   factqmin=1.0,factqmax=1.0,factv=0.0,factcldch=0.0,factw10m=1.0,deltim=$DELTIM,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   diag_rad=.false.,diag_pcp=.false.,diag_ozone=.false.,diag_aero=.false.,
   nhr_assimilation=6,min_offset=180,use_compress=.false.,lrun_subdirs=.true.,
   use_similarity_2dvar=.true.,
   neutral_stability_windfact_2dvar=.false.,
   use_prepb_satwnd=.false.,
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
   rtma_subdomain_option=.true.,triad4=.true.,nsmooth=0,nsmooth_shapiro=0,lreadnorm=.true.,
 /
 &JCOPTS
 /
 &STRONGOPTS
   nstrong=1,nvmodes_keep=20,period_max=3.,
   baldiag_full=.true.,baldiag_inc=.true.,
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,c_varqc=0.02,vadfile='prepbufr',hilbert_curve=.true.,
   buddycheck_t=.false.,buddydiag_save=.false.,oberrflg=.true.,njqc=.true.,vqc=.false.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=60.0,dmesh(2)=60.0,dmesh(3)=60.0,dmesh(4)=60.0,time_window_max=3.0,
 /
OBS_INPUT::
!  dfile          dtype     dplat     dsis     dval    dthin  dsfcalc
   prepbufr       ps        null      ps       1.0     0      0
   prepbufr       t         null      t        1.0     0      0
   prepbufr       q         null      q        1.0     0      0
   prepbufr       uv        null      uv       1.0     0      0
   satwndbufr     uv        null      uv       1.0     0      0
   prepbufr       spd       null      spd      1.0     0      0
   prepbufr       wspd10m   null      wspd10m  1.0     0      0
   satwnd         wspd10m   null      wspd10m  1.0     0      0
   prepbufr       uwnd10m   null      uwnd10m  1.0     0      0
   satwnd         uwnd10m   null      uwnd10m  1.0     0      0
   prepbufr       vwnd10m   null      vwnd10m  1.0     0      0
   satwnd         vwnd10m   null      vwnd10m  1.0     0      0
   prepbufr       gust      null      gust     1.0     0      0
   prepbufr       vis       null      vis      1.0     0      0
   prepbufr       td2m      null      td2m     1.0     0      0
   mxtmdat        mxtm      null      mxtm     1.0     0      0
   mitmdat        mitm      null      mitm     1.0     0      0
   prepbufr       mxtm      null      mxtm     1.0     0      0
   prepbufr       mitm      null      mitm     1.0     0      0
   prepbufr       pmsl      null      pmsl     1.0     0      0
   prepbufr       howv      null      howv     1.0     0      0
   satmar         howv      null      howv     1.0     0      0
   prepbufr       tcamt     null      tcamt    1.0     0      0
   goessky        tcamt     null      tcamt    1.0     0      0
   prepbufr       cldch     null      cldch    1.0     0      0
::
 &SUPEROB_RADAR
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
   l_closeobs=.true.
 /
 &CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=36.,oblon=260.,obpres=1000.,obdattim=${adate},
   obhourset=0.,
 /
 &NST
 /
"
;;
    rrfs_3denvar_rdasens)

# Define namelist for rrfs 3d hybrid envar run with global ensembles

export gsi_namelist="

 &SETUP
   miter=2,niter(1)=5,niter(2)=5,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,print_obs_para=.true.,diag_fed=.true.,diag_radardbz=.false.,
   if_model_dbz=.true.,if_model_fed=.true.,static_gsi_nopcp_dbz=0.0,if_use_w_vr=.false.,
   rmesh_dbz=4.0,rmesh_vr=4.0,zmesh_dbz=1000.0,zmesh_vr=1000.0,
   inflate_dbz_obserr=.true.,missing_to_nopcp=.false.,radar_no_thinning=.true.,
   gencode=78,factqmin=0.0,factqmax=0.0,
   iguess=-1,crtm_coeffs_path='./',
   lread_obs_save=.false.,lread_obs_skip=.false.,
   ens_nstarthr=01,
   oneobtest=.false.,retrieval=.false.,
   nhr_assimilation=3,l_foto=.false.,
   use_pbl=.false.,use_prepb_satwnd=.false.,
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,emiss_bc=.true.,
   diag_precon=.true.,step_start=1.e-3,
   l4densvar=.false.,nhr_obsbin=3,
   use_gfs_nemsio=.false.,use_gfs_ncio=.true.,reset_bad_radbc=.true.,
   netcdf_diag=.true.,binary_diag=.false.,
   l_obsprvdiag=.false.,
   lwrite_peakwt=.true.,
   innov_use_model_fed=.true.,
 /
 &GRIDOPTS
   fv3_regional=.true.,grid_ratio_fv3_regional=2.0,nvege_type=20,
   fv3_io_layout_y=1,
 /
 &BKGERR
   vs=1.0,
   hzscl=0.7,1.4,2.80,
   bw=0.,fstat=.true.,
   usenewgfsberror=.true.,
/
 &ANBKGERR
   anisotropic=.false.,
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
   vadwnd_l2rw_qc=.true.,
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=1.5,time_window_rad=1.0,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   pm25bufr       pm2_5       null      TEOM                 1.0     0     0
   dbzobs.nc      dbz         null      dbz                  1.0     0     0
   fedobs.nc      fed         null      fed                  1.0     0     0
   prepbufr       ps          null      ps                   1.0     0     0
   prepbufr       t           null      t                    1.0     0     0
   prepbufr       q           null      q                    1.0     0     0
   prepbufr       pw          null      pw                   1.0     0     0
   satwndbufr     uv          null      uv                   1.0     0     0
   prepbufr       uv          null      uv                   1.0     0     0
   prepbufr       spd         null      spd                  1.0     0     0
   prepbufr       dw          null      dw                   1.0     0     0
   l2rwbufr       rw          null      l2rw                 1.0     0     0
   prepbufr       sst         null      sst                  1.0     0     0
   gpsrobufr      gps_ref     null      gps                  1.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi             1.0    -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi              1.0    -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16            0.0     0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17            0.0     0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18            0.0     0     0
   hirs3bufr      hirs3       n16       hirs3_n16            0.0     1     0
   hirs3bufr      hirs3       n17       hirs3_n17            0.0     1     0
   hirs4bufr      hirs4       metop-a   hirs4_metop-a        0.0     2     0
   hirs4bufr      hirs4       n18       hirs4_n18            0.0     1     0
   hirs4bufr      hirs4       n19       hirs4_n19            0.0     2     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b        0.0     2     0
   gimgrbufr      goes_img    g11       imgr_g11             0.0     1     0
   gimgrbufr      goes_img    g12       imgr_g12             0.0     1     0
   airsbufr       airs        aqua      airs_aqua            0.0     2     0
   amsuabufr      amsua       n15       amsua_n15            0.0     2     0
   amsuabufr      amsua       n18       amsua_n18            0.0     2     0
   amsuabufr      amsua       n19       amsua_n19            0.0     2     0
   amsuabufr      amsua       metop-a   amsua_metop-a        0.0     2     0
   amsuabufr      amsua       metop-b   amsua_metop-b        0.0     2     0
   amsuabufr      amsua       metop-c   amsua_metop-c        0.0     2     0
   airsbufr       amsua       aqua      amsua_aqua           0.0     2     0
   amsubbufr      amsub       n17       amsub_n17            0.0     1     0
   mhsbufr        mhs         n18       mhs_n18              0.0     2     0
   mhsbufr        mhs         n19       mhs_n19              0.0     2     0
   mhsbufr        mhs         metop-a   mhs_metop-a          0.0     2     0
   mhsbufr        mhs         metop-b   mhs_metop-b          0.0     2     0
   mhsbufr        mhs         metop-c   mhs_metop-c          0.0     2     0
   ssmitbufr      ssmi        f13       ssmi_f13             0.0     2     0
   ssmitbufr      ssmi        f14       ssmi_f14             0.0     2     0
   ssmitbufr      ssmi        f15       ssmi_f15             0.0     2     0
   amsrebufr      amsre_low   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_mid   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_hig   aqua      amsre_aqua           0.0     2     0
   ssmisbufr      ssmis       f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis       f17       ssmis_f17            0.0     2     0
   ssmisbufr      ssmis       f18       ssmis_f18            0.0     2     0
   ssmisbufr      ssmis       f19       ssmis_f19            0.0     2     0
   gsnd1bufr      sndrd1      g12       sndrD1_g12           0.0     1     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12           0.0     1     0
   gsnd1bufr      sndrd3      g12       sndrD3_g12           0.0     1     0
   gsnd1bufr      sndrd4      g12       sndrD4_g12           0.0     1     0
   gsnd1bufr      sndrd1      g11       sndrD1_g11           0.0     1     0
   gsnd1bufr      sndrd2      g11       sndrD2_g11           0.0     1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11           0.0     1     0
   gsnd1bufr      sndrd4      g11       sndrD4_g11           0.0     1     0
   gsnd1bufr      sndrd1      g13       sndrD1_g13           0.0     1     0
   gsnd1bufr      sndrd2      g13       sndrD2_g13           0.0     1     0
   gsnd1bufr      sndrd3      g13       sndrD3_g13           0.0     1     0
   gsnd1bufr      sndrd4      g13       sndrD4_g13           0.0     1     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15           0.0     2     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15           0.0     2     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15           0.0     2     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15           0.0     2     0
   iasibufr       iasi        metop-a   iasi_metop-a         0.0     2     0
   gomebufr       gome        metop-a   gome_metop-a         0.0     2     0
   omibufr        omi         aura      omi_aura             0.0     2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19            0.0     0     0
   tcvitl         tcp         null      tcp                  0.0     0     0
   seviribufr     seviri      m08       seviri_m08           0.0     2     0
   seviribufr     seviri      m09       seviri_m09           0.0     2     0
   seviribufr     seviri      m10       seviri_m10           0.0     2     0
   seviribufr     seviri      m11       seviri_m11           0.0     2     0
   iasibufr       iasi        metop-b   iasi_metop-b         0.0     2     0
   iasibufr       iasi        metop-c   iasi_metop-c         0.0     2     0
   gomebufr       gome        metop-b   gome_metop-b         0.0     2     0
   atmsbufr       atms        npp       atms_npp             0.0     2     0
   atmsbufr       atms        n20       atms_n20             0.0     2     0
   atmsbufr       atms        n21       atms_n21             0.0     2     0
   crisbufr       cris        npp       cris_npp             0.0     2     0
   crisfsbufr     cris-fsr    npp       cris-fsr_npp         0.0     2     0
   crisfsbufr     cris-fsr    n20       cris-fsr_n20         0.0     2     0
   crisfsbufr     cris-fsr    n21       cris-fsr_n21         0.0     2     0
   abibufr        abi         g16       abi_g16              0.0     2     0
   mlsbufr        mls30       aura      mls30_aura           0.0     0     0
   oscatbufr      uv          null      uv                   0.0     0     0
   prepbufr       mta_cld     null      mta_cld              1.0     0     0
   prepbufr       gos_ctp     null      gos_ctp              1.0     0     0
   refInGSI       rad_ref     null      rad_ref              1.0     0     0
   lghtInGSI      lghtn       null      lghtn                1.0     0     0
   larcInGSI      larccld     null      larccld              1.0     0     0
   abibufr        abi         g18       abi_g18              0.0     2     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000., l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${ifhyb},
   uv_hyb_ens=.true.,
   q_hyb_ens=.false.,
   aniso_a_en=.false.,generate_ens=.false.,
   n_ens=${nummem},
   l_both_fv3sar_gfs_ens=.false.,n_ens_gfs=0,n_ens_fv3sar=30,
   weight_ens_gfs=1.0,weight_ens_fv3sar=1.0,
   beta_s0=0.15,s_ens_h=328.632,82.1580,4.10790,4.10790,82.1580,s_ens_v=3,3,-0.30125,-0.30125,0.0,
   regional_ensemble_option=5,
   pseudo_hybens = .false.,
   grid_ratio_ens = 1,
   l_ens_in_diff_time=.true.,
   ensemble_path='',
   i_en_perts_io=1,
   jcap_ens=574,
   fv3sar_bg_opt=0,
   readin_localization=.false.,
   parallelization_over_ensmembers=.false.,
   nsclgrp=2,l_timloc_opt=.false.,ngvarloc=2,naensloc=5,
   r_ensloccov4tim=1.0,r_ensloccov4var=0.05,r_ensloccov4scl=1.0,
   global_spectral_filter_sd=.false.,assign_vdl_nml=.false.,vdl_scale=0,
   vloc_varlist(1,1)='sf ',vloc_varlist(2,1)='w  ',vloc_varlist(3,1)='sf ',vloc_varlist(4,1)='w  ',
   vloc_varlist(1,2)='vp ',vloc_varlist(2,2)='qr ',vloc_varlist(3,2)='vp ',vloc_varlist(4,2)='qr ',
   vloc_varlist(1,3)='ps ',vloc_varlist(2,3)='qs ',vloc_varlist(3,3)='ps ',vloc_varlist(4,3)='qs ',
   vloc_varlist(1,4)='t  ',vloc_varlist(2,4)='qi ',vloc_varlist(3,4)='t  ',vloc_varlist(4,4)='qi ',
   vloc_varlist(1,5)='q  ',vloc_varlist(2,5)='qg ',vloc_varlist(3,5)='q  ',vloc_varlist(4,5)='qg ',
   vloc_varlist(1,6)='sst',vloc_varlist(2,6)='ql ',vloc_varlist(3,6)='sst',vloc_varlist(4,6)='ql ',
   vloc_varlist(1,7)='stl',vloc_varlist(2,7)='dbz',vloc_varlist(3,7)='stl',vloc_varlist(4,7)='dbz',
   vloc_varlist(1,8)='sti',vloc_varlist(2,8)='aaa',vloc_varlist(3,8)='sti',vloc_varlist(4,8)='aaa',
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=20.0,
   metar_impact_radius=10.0,
   metar_impact_radius_lowCloud=4.0,
   l_gsd_terrain_match_surfTobs=.true.,
   l_sfcobserror_ramp_t=.true.,
   l_sfcobserror_ramp_q=.true.,
   l_PBL_pseudo_SurfobsT=.false.,
   l_PBL_pseudo_SurfobsQ=.false.,
   l_PBL_pseudo_SurfobsUV=.false.,
   pblH_ration=0.4,
   pps_press_incr=40.0,
   l_gsd_limit_ocean_q=.true.,
   l_pw_hgt_adjust=.true.,
   l_limit_pw_innov=.true.,
   max_innov_pct=0.1,
   l_cleanSnow_WarmTs=.true.,
   r_cleanSnow_WarmTs_threshold=5.0,
   l_conserve_thetaV=.true.,
   i_conserve_thetaV_iternum=3,
   l_gsd_soilTQ_nudge=.false.,
   l_cld_bld=.true.,
   l_numconc=.true.,
   l_closeobs=.true.,
   cld_bld_hgt=1200.0,
   build_cloud_frac_p=0.50,
   clear_cloud_frac_p=0.10,
   iclean_hydro_withRef_allcol=1,
   i_use_2mQ4B=2,
   i_use_2mT4B=1,
   i_gsdcldanal_type=0,
   i_gsdsfc_uselist=1,
   i_lightpcp=1,
   i_sfct_gross=1,
   i_coastline=3,
   i_gsdqc=2,
 /
 &CHEM
  laeroana_fv3smoke=.false.,
  berror_fv3_cmaq_regional=.false.,
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=2020040500,
   obhourset=0.,
 /
"
;;

    hafs_envar)
# Define namelist for hafs 3denvar run with global ensembles
export gsi_namelist="

 &SETUP
   miter=2,niter(1)=5,niter(2)=5,
   niter_no_qc(1)=2,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=78,deltim=1200,
   factqmin=0.0,factqmax=0.0,
   iguess=-1,
   aircraft_recon=.true.,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   nhr_assimilation=6,
   use_pbl=.true.,use_compress=.false.,nsig_ext=14,gpstop=50.,
   use_gfs_nemsio=.false.,use_gfs_ncio=.true.,
   print_diag_pcg=.true.,l2rwthin=.false.,hurricane_radar=.true.,
   use_gfs_ozone=.false.,l4densvar=${l4densvar},nhr_obsbin=${nhr_obsbin},
   lrun_subdirs=.true.,
   netcdf_diag=.true.,binary_diag=.false.,
   newpc4pred=.true., adp_anglebc=.true., angord=4,
   passive_bc=.false., use_edges=.false., emiss_bc=.true.,
   diag_precon=.true., step_start=1.e-3, upd_pred(1)=0,
   upd_pred(2)=0,upd_pred(3)=0,upd_pred(4)=0,
   upd_pred(5)=0,upd_pred(6)=0,upd_pred(7)=0,
   upd_pred(8)=0,upd_pred(9)=0,upd_pred(10)=0,
   upd_pred(11)=0,upd_pred(12)=0,
   lread_obs_save=.false.,
   lread_obs_skip=.false.,
   ens_nstarthr=6,
   lwrite_predterms=.false.,lwrite_peakwt=.false.,reduce_diag=.false.,
 /
 &GRIDOPTS
   fv3_regional=.true.,grid_ratio_fv3_regional=1,nvege_type=20,
 /
 &BKGERR
   vs=1.0,
   hzscl=0.2,0.4,0.8,
   bw=0.,
   fstat=.false.,
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
   dfact=0.75,dfact1=3.0,erradar_inflate=1.0,tdrerr_inflate=.true.,
   noiqc=.true.,c_varqc=0.03333,vadfile='prepbufr',njqc=.false.,vqc=.true.,vadwnd_l2rw_qc=.false.,
   q_doe_a_136=0.65,
   q_doe_b_136=0.0003,
   q_doe_a_137=0.75,
   q_doe_b_137=0.0003,
   t_doe_a_136=0.75,
   t_doe_b_136=0.2,
   t_doe_a_137=0.7,
   t_doe_b_137=0.2,
   uv_doe_a_236=0.5,
   uv_doe_b_236=0.85,
   uv_doe_a_237=0.5,
   uv_doe_b_237=0.85,
   uv_doe_a_213=0.4,
   uv_doe_b_213=1.0,
 /
 &OBS_INPUT
   dmesh(1)=90.0,dmesh(2)=45.0,dmesh(3)=45.0,dmesh(4)=45.0,dmesh(5)=90,time_window_max=3.0,l_foreaft_thin=.false.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   prepbufr       ps          null        ps                  0.0     0     0
   prepbufr       t           null        t                   0.0     0     0
   prepbufr_profl t           null        t                   0.0     0     0
   prepbufr       q           null        q                   0.0     0     0
   prepbufr_profl q           null        q                   0.0     0     0
   prepbufr       pw          null        pw                  0.0     0     0
   prepbufr       uv          null        uv                  0.0     0     0
   prepbufr_profl uv          null        uv                  0.0     0     0
   satwndbufr     uv          null        uv                  0.0     0     0
   satwhrbufr     uv          null        uv                  0.0     0     0
   prepbufr       spd         null        spd                 0.0     0     0
   prepbufr       dw          null        dw                  0.0     0     0
   radarbufr      rw          null        rw                  0.0     0     0
   prepbufr       sst         null        sst                 0.0     0     0
   tcvitl         tcp         null        tcp                 0.0     0     0
   tldplrbufr     rw          null        rw                  0.0     0     0
   l2rwbufr       rw          null        l2rw                0.0     0     0
   hdobbufr       uv          null        uv                  0.0     0     0
   hdobbufr       t           null        t                   0.0     0     0
   hdobbufr       q           null        q                   0.0     0     0
   hdobbufr       spd         null        spd                 0.0     0     0
   gpsrobufr      gps_bnd     null        gps                 0.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi            0.0    -1     0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi             0.0    -1     0
   sbuvbufr       sbuv2       n16         sbuv8_n16           0.0     0     0
   sbuvbufr       sbuv2       n17         sbuv8_n17           0.0     0     0
   sbuvbufr       sbuv2       n18         sbuv8_n18           0.0     0     0
   gimgrbufr      goes_img    g11         imgr_g11            0.0     1     0
   gimgrbufr      goes_img    g12         imgr_g12            0.0     1     0
   airsbufr       airs        aqua        airs_aqua           0.0     1     1
   amsuabufr      amsua       n15         amsua_n15           0.0     2     1
   amsuabufr      amsua       n18         amsua_n18           0.0     2     1
   amsuabufr      amsua       metop-a     amsua_metop-a       0.0     2     1
   airsbufr       amsua       aqua        amsua_aqua          0.0     2     1
   amsubbufr      amsub       n17         amsub_n17           0.0     3     1
   mhsbufr        mhs         n18         mhs_n18             0.0     3     1
   mhsbufr        mhs         metop-a     mhs_metop-a         0.0     3     1
   ssmitbufr      ssmi        f15         ssmi_f15            0.0     1     0
   amsrebufr      amsre_low   aqua        amsre_aqua          0.0     4     0
   amsrebufr      amsre_mid   aqua        amsre_aqua          0.0     4     0
   amsrebufr      amsre_hig   aqua        amsre_aqua          0.0     4     0
   ssmisbufr      ssmis       f16         ssmis_f16           0.0     4     0
   ssmisbufr      ssmis       f17         ssmis_f17           0.0     4     0
   ssmisbufr      ssmis       f18         ssmis_f18           0.0     4     0
   ssmisbufr      ssmis       f19         ssmis_f19           0.0     4     0
   gsnd1bufr      sndrd1      g12         sndrD1_g12          0.0     5     0
   gsnd1bufr      sndrd2      g12         sndrD2_g12          0.0     5     0
   gsnd1bufr      sndrd3      g12         sndrD3_g12          0.0     5     0
   gsnd1bufr      sndrd4      g12         sndrD4_g12          0.0     5     0
   gsnd1bufr      sndrd1      g11         sndrD1_g11          0.0     5     0
   gsnd1bufr      sndrd2      g11         sndrD2_g11          0.0     5     0
   gsnd1bufr      sndrd3      g11         sndrD3_g11          0.0     5     0
   gsnd1bufr      sndrd4      g11         sndrD4_g11          0.0     5     0
   gsnd1bufr      sndrd1      g13         sndrD1_g13          0.0     5     0
   gsnd1bufr      sndrd2      g13         sndrD2_g13          0.0     5     0
   gsnd1bufr      sndrd3      g13         sndrD3_g13          0.0     5     0
   gsnd1bufr      sndrd4      g13         sndrD4_g13          0.0     5     0
   iasibufr       iasi        metop-a     iasi_metop-a        0.0     1     1
   gomebufr       gome        metop-a     gome_metop-a        0.0     2     0
   omibufr        omi         aura        omi_aura            0.0     2     0
   sbuvbufr       sbuv2       n19         sbuv8_n19           0.0     0     0
   amsuabufr      amsua       n19         amsua_n19           0.0     2     1
   mhsbufr        mhs         n19         mhs_n19             0.0     3     1
   seviribufr     seviri      m08         seviri_m08          0.0     1     0
   seviribufr     seviri      m09         seviri_m09          0.0     1     0
   seviribufr     seviri      m10         seviri_m10          0.0     1     0
   amsuabufr      amsua       metop-b     amsua_metop-b       0.0     2     0
   mhsbufr        mhs         metop-b     mhs_metop-b         0.0     3     0
   iasibufr       iasi        metop-b     iasi_metop-b        0.0     1     0
   gomebufr       gome        metop-b     gome_metop-b        0.0     2     0
   atmsbufr       atms        npp         atms_npp            0.0     2     0
   atmsbufr       atms        n20         atms_n20            0.0     2     0
   crisbufr       cris        npp         cris_npp            0.0     1     0
   crisfsbufr     cris-fsr    npp         cris-fsr_npp        0.0     1     0
   crisfsbufr     cris-fsr    n20         cris-fsr_n20        0.0     1     0
   gsnd1bufr      sndrd1      g14         sndrD1_g14          0.0     5     0
   gsnd1bufr      sndrd2      g14         sndrD2_g14          0.0     5     0
   gsnd1bufr      sndrd3      g14         sndrD3_g14          0.0     5     0
   gsnd1bufr      sndrd4      g14         sndrD4_g14          0.0     5     0
   gsnd1bufr      sndrd1      g15         sndrD1_g15          0.0     5     0
   gsnd1bufr      sndrd2      g15         sndrD2_g15          0.0     5     0
   gsnd1bufr      sndrd3      g15         sndrD3_g15          0.0     5     0
   gsnd1bufr      sndrd4      g15         sndrD4_g15          0.0     5     0
   oscatbufr      uv          null        uv                  0.0     0     0
   mlsbufr        mls30       aura        mls30_aura          0.0     0     0
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     3     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     3     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=10000.,del_time=1.0,elev_angle_max=5.,minnum=1,range_max=200000.,
   l2superob_only=.false.,radar_sites=.false.,
   radar_box=.true.,radar_rmesh=10,radar_zmesh=500,
 /
SUPEROB_RADAR::
  KBRO  1
  KCRP  1
  KEWX  1
  KGRX  1
  KDFX  1
  KHGX  1
  KLCH  1
  KLIX  1
  KPOE  1
  KSHV  1
  KDGX  1
  KMOB  1
  KEVX  1
  KEOX  1
  KMXX  1
  KBMX  1
  KTLH  1
  KTBW  1
  KBYX  1
  KAMX  1
  KMLB  1
  KJAX  1
  KVAX  1
  KJGX  1
  KFFC  1
  KCLX  1
  KCAE  1
  KGSP  1
  KLTX  1
  KMHX  1
  KRAX  1
  KAKQ  1
  KFCX  1
  KLWX  1
  KDOX  1
  KCCX  1
  KDIX  1
  KOKX  1
  KENX  1
  KBGM  1
  KCXX  1
  KBOX  1
  KGYX  1
  KCBW  1
  TJUA  1
  PHWA  1
  PHKI  1
  PHMO  1
  PHKM  1
::
/
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,
   n_ens=${N_ENS},
   uv_hyb_ens=.true.,
   beta_s0=${BETA_S0},
   s_ens_h=150,
   s_ens_v=-0.5,
   readin_localization=.false.,
   generate_ens=.false.,
   regional_ensemble_option=${REGIONAL_ENSEMBLE_OPTION},
   grid_ratio_ens=${GRID_RATIO_ENS},
   pseudo_hybens=.false.,
   merge_two_grid_ensperts=F,
   pwgtflg=F,
   aniso_a_en=.false.,
   nlon_ens=387,
   nlat_ens=777,
   write_ens_sprd=F,
   l_both_fv3sar_gfs_ens=${l_both_fv3sar_gfs_ens},
   n_ens_gfs=${n_ens_gfs},
   n_ens_fv3sar=${n_ens_fv3sar},
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
 /
 &CHEM
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=2020040500,
   obhourset=0.,
 /
"
;;
    rrfs_enkf_conv)

# Define namelist for rrfs EnKF  run

export gsi_namelist="

    &nam_enkf
        datestring=${rrfs_enkf_adate},datapath='${tmpdir}/',
        analpertwtnh=1.10,analpertwtsh=1.10,analpertwttr=1.10,
        covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.true.,iassim_order=0,
        corrlengthnh=300,corrlengthsh=300,corrlengthtr=300,
        lnsigcutoffnh=0.5,lnsigcutoffsh=0.5,lnsigcutofftr=0.5,
        lnsigcutoffpsnh=0.5,lnsigcutoffpssh=0.5,lnsigcutoffpstr=0.5,
        lnsigcutoffsatnh=0.5,lnsigcutoffsatsh=0.5,lnsigcutoffsattr=0.5,
        obtimelnh=1.e30,obtimelsh=1.e30,obtimeltr=1.e30,
        saterrfact=1.0,numiter=1,
        sprd_tol=1.e30,paoverpb_thresh=0.98,
        nlons=420,nlats= 252, nlevs= 65,nanals=5,
        deterministic=.true.,sortinc=.true.,lupd_satbiasc=.false.,
        reducedgrid=.true.,readin_localization=.false.,
        use_gfs_nemsio=.true.,imp_physics=99,lupp=.false.,
        univaroz=.false.,adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,
        lobsdiag_forenkf=.false.,
        write_spread_diag=.false.,
        netcdf_diag=.true.,
        fv3_native=.true.,
    /
    &satobs_enkf
        sattypes_rad(1) = 'amsua_n15',     dsis(1) = 'amsua_n15',
        sattypes_rad(2) = 'amsua_n18',     dsis(2) = 'amsua_n18',
        sattypes_rad(3) = 'amsua_n19',     dsis(3) = 'amsua_n19',
        sattypes_rad(4) = 'amsub_n16',     dsis(4) = 'amsub_n16',
        sattypes_rad(5) = 'amsub_n17',     dsis(5) = 'amsub_n17',
        sattypes_rad(6) = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
        sattypes_rad(7) = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
        sattypes_rad(8) = 'airs_aqua',     dsis(8) = 'airs_aqua',
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
        sattypes_rad(20)= 'avhrr_n18',     dsis(20)= 'avhrr3_n18',
        sattypes_rad(21)= 'avhrr_metop-a', dsis(21)= 'avhrr3_metop-a',
        sattypes_rad(22)= 'avhrr_n19',     dsis(22)= 'avhrr3_n19',
        sattypes_rad(23)= 'amsre_aqua',    dsis(23)= 'amsre_aqua',
        sattypes_rad(24)= 'ssmis_f16',     dsis(24)= 'ssmis_f16',
        sattypes_rad(25)= 'ssmis_f17',     dsis(25)= 'ssmis_f17',
        sattypes_rad(26)= 'ssmis_f18',     dsis(26)= 'ssmis_f18',
        sattypes_rad(27)= 'ssmis_f19',     dsis(27)= 'ssmis_f19',
        sattypes_rad(28)= 'ssmis_f20',     dsis(28)= 'ssmis_f20',
        sattypes_rad(29)= 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
        sattypes_rad(30)= 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
        sattypes_rad(31)= 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
        sattypes_rad(32)= 'sndrd4_g11',    dsis(32)= 'sndrD4_g11',
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
        sattypes_rad(49)= 'iasi_metop-a',  dsis(49)= 'iasi_metop-a',
        sattypes_rad(50)= 'seviri_m08',    dsis(50)= 'seviri_m08',
        sattypes_rad(51)= 'seviri_m09',    dsis(51)= 'seviri_m09',
        sattypes_rad(52)= 'seviri_m10',    dsis(52)= 'seviri_m10',
        sattypes_rad(53)= 'seviri_m11',    dsis(53)= 'seviri_m11',
        sattypes_rad(54)= 'amsua_metop-b', dsis(54)= 'amsua_metop-b',
        sattypes_rad(55)= 'hirs4_metop-b', dsis(55)= 'hirs4_metop-b',
        sattypes_rad(56)= 'mhs_metop-b',   dsis(56)= 'mhs_metop-b',
        sattypes_rad(57)= 'iasi_metop-b',  dsis(57)= 'iasi_metop-b',
        sattypes_rad(58)= 'avhrr_metop-b', dsis(58)= 'avhrr3_metop-b',
	sattypes_rad(59)= 'atms_npp',      dsis(59)= 'atms_npp',
        sattypes_rad(60)= 'atms_n20',      dsis(60)= 'atms_n20',
        sattypes_rad(61)= 'cris_npp',      dsis(61)= 'cris_npp',
        sattypes_rad(62)= 'cris-fsr_npp',  dsis(62)= 'cris-fsr_npp',
        sattypes_rad(63)= 'cris-fsr_n20',  dsis(63)= 'cris-fsr_n20',
        sattypes_rad(64)= 'gmi_gpm',       dsis(64)= 'gmi_gpm',
        sattypes_rad(65)= 'saphir_meghat', dsis(65)= 'saphir_meghat',
        sattypes_rad(66)= 'amsua_metop-c', dsis(66)= 'amsua_metop-c',
        sattypes_rad(67)= 'mhs_metop-c',   dsis(67)= 'mhs_metop-c',
        sattypes_rad(68)= 'ahi_himawari8', dsis(68)= 'ahi_himawari8',
        sattypes_rad(69)= 'abi_g16',       dsis(69)= 'abi_g16',
        sattypes_rad(70)= 'abi_g17',       dsis(70)= 'abi_g17',
        sattypes_rad(71)= 'iasi_metop-c',  dsis(71)= 'iasi_metop-c',
        sattypes_rad(72)= 'viirs-m_npp',   dsis(72)= 'viirs-m_npp',
        sattypes_rad(73)= 'viirs-m_j1',    dsis(73)= 'viirs-m_j1',
        sattypes_rad(74)= 'avhrr_metop-c', dsis(74)= 'avhrr3_metop-c',
        sattypes_rad(75)= 'abi_g18',       dsis(75)= 'abi_g18',
        sattypes_rad(76)= 'ahi_himawari9', dsis(76)= 'ahi_himawari9',
        sattypes_rad(77)= 'viirs-m_j2',    dsis(77)= 'viirs-m_j2',
        sattypes_rad(78)= 'atms_n21',      dsis(78)= 'atms_n21',
        sattypes_rad(79)= 'cris-fsr_n21',  dsis(79)= 'cris-fsr_n21',
    /
    &ozobs_enkf
        sattypes_oz(1) = 'sbuv2_n16',
        sattypes_oz(2) = 'sbuv2_n17',
        sattypes_oz(3) = 'sbuv2_n18',
        sattypes_oz(4) = 'sbuv2_n19',
        sattypes_oz(5) = 'omi_aura',
        sattypes_oz(6) = 'gome_metop-a',
        sattypes_oz(7) = 'gome_metop-b',
        sattypes_oz(8) = 'mls30_aura',
    /
    &nam_fv3
        fv3fixpath="XXX",nx_res=${NX_RES:-420},ny_res=${NY_RES-252},ntiles=1,
        l_fv3reg_filecombined=.false.,
    /
"
;;
    global_enkf)

# Define namelist for global enkf run

export gsi_namelist="

 &nam_enkf
  datestring=${global_adate},datapath='${DATA}/',
  analpertwtnh=${analpertwt},analpertwtsh=${analpertwt},analpertwttr=${analpertwt},
  covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.false.,iassim_order=0,
  corrlengthnh=${corrlength},corrlengthsh=${corrlength},corrlengthtr=${corrlength},
  lnsigcutoffnh=${lnsigcutoff},lnsigcutoffsh=${lnsigcutoff},lnsigcutofftr=${lnsigcutoff},
  lnsigcutoffpsnh=${lnsigcutoff},lnsigcutoffpssh=${lnsigcutoff},lnsigcutoffpstr=${lnsigcutoff},
  lnsigcutoffsatnh=${lnsigcutoff},lnsigcutoffsatsh=${lnsigcutoff},lnsigcutoffsattr=${lnsigcutoff},
  obtimelnh=1.e30,obtimelsh=1.e30,obtimeltr=1.e30,
  saterrfact=1.0,numiter=0,
  sprd_tol=1.e30,paoverpb_thresh=0.98,
  nlons=$LONA,nlats=$LATA,nlevs=$LEVS,nanals=$NMEM_ENKF,
  deterministic=.true.,sortinc=.true.,lupd_satbiasc=.false.,
  reducedgrid=.false.,readin_localization=.false.,
  use_gfs_nemsio=${use_gfs_nemsio},use_gfs_ncio=${use_gfs_ncio},imp_physics=8,lupp=$lupp,
  univaroz=.false.,adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,
  letkf_flag=${letkf_flag},nobsl_max=${nobsl_max},denkf=${denkf},getkf=${getkf}.,
  nhr_anal=${IAUFHRS_ENKF},nhr_state=${IAUFHRS_ENKF},
  lobsdiag_forenkf=$lobsdiag_forenkf,taperanalperts=${taperanalperts},
  write_spread_diag=$write_spread_diag,
  modelspace_vloc=$modelspace_vloc,
  use_correlated_oberrs=${use_correlated_oberrs},
  netcdf_diag=$netcdf_diag,cnvw_option=$cnvw_option,
  paranc=$paranc,write_fv3_incr=$write_fv3_incr,
  $WRITE_INCR_ZERO
  $NAM_ENKF
 /
 &satobs_enkf
   sattypes_rad(1) = 'amsua_n15',     dsis(1) = 'amsua_n15',
   sattypes_rad(2) = 'amsua_n18',     dsis(2) = 'amsua_n18',
   sattypes_rad(3) = 'amsua_n19',     dsis(3) = 'amsua_n19',
   sattypes_rad(4) = 'amsub_n16',     dsis(4) = 'amsub_n16',
   sattypes_rad(5) = 'amsub_n17',     dsis(5) = 'amsub_n17',
   sattypes_rad(6) = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
   sattypes_rad(7) = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
   sattypes_rad(8) = 'airs_aqua',     dsis(8) = 'airs_aqua',
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
   sattypes_rad(20)= 'avhrr_n18',     dsis(20)= 'avhrr3_n18',
   sattypes_rad(21)= 'avhrr_metop-a', dsis(21)= 'avhrr3_metop-a',
   sattypes_rad(22)= 'avhrr_n19',     dsis(22)= 'avhrr3_n19',
   sattypes_rad(23)= 'amsre_aqua',    dsis(23)= 'amsre_aqua',
   sattypes_rad(24)= 'ssmis_f16',     dsis(24)= 'ssmis_f16',
   sattypes_rad(25)= 'ssmis_f17',     dsis(25)= 'ssmis_f17',
   sattypes_rad(26)= 'ssmis_f18',     dsis(26)= 'ssmis_f18',
   sattypes_rad(27)= 'ssmis_f19',     dsis(27)= 'ssmis_f19',
   sattypes_rad(28)= 'ssmis_f20',     dsis(28)= 'ssmis_f20',
   sattypes_rad(29)= 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
   sattypes_rad(30)= 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
   sattypes_rad(31)= 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
   sattypes_rad(32)= 'sndrd4_g11',    dsis(32)= 'sndrD4_g11',
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
   sattypes_rad(49)= 'iasi_metop-a',  dsis(49)= 'iasi_metop-a',
   sattypes_rad(50)= 'seviri_m08',    dsis(50)= 'seviri_m08',
   sattypes_rad(51)= 'seviri_m09',    dsis(51)= 'seviri_m09',
   sattypes_rad(52)= 'seviri_m10',    dsis(52)= 'seviri_m10',
   sattypes_rad(53)= 'seviri_m11',    dsis(53)= 'seviri_m11',
   sattypes_rad(54)= 'amsua_metop-b', dsis(54)= 'amsua_metop-b',
   sattypes_rad(55)= 'hirs4_metop-b', dsis(55)= 'hirs4_metop-b',
   sattypes_rad(56)= 'mhs_metop-b',   dsis(56)= 'mhs_metop-b',
   sattypes_rad(57)= 'iasi_metop-b',  dsis(57)= 'iasi_metop-b',
   sattypes_rad(58)= 'avhrr_metop-b', dsis(58)= 'avhrr3_metop-b',
   sattypes_rad(59)= 'atms_npp',      dsis(59)= 'atms_npp',
   sattypes_rad(60)= 'atms_n20',      dsis(60)= 'atms_n20',
   sattypes_rad(61)= 'cris_npp',      dsis(61)= 'cris_npp',
   sattypes_rad(62)= 'cris-fsr_npp',  dsis(62)= 'cris-fsr_npp',
   sattypes_rad(63)= 'cris-fsr_n20',  dsis(63)= 'cris-fsr_n20',
   sattypes_rad(64)= 'gmi_gpm',       dsis(64)= 'gmi_gpm',
   sattypes_rad(65)= 'saphir_meghat', dsis(65)= 'saphir_meghat',
   sattypes_rad(66)= 'amsua_metop-c', dsis(66)= 'amsua_metop-c',
   sattypes_rad(67)= 'mhs_metop-c',   dsis(67)= 'mhs_metop-c',
   sattypes_rad(68)= 'ahi_himawari8', dsis(68)= 'ahi_himawari8',
   sattypes_rad(69)= 'abi_g16',       dsis(69)= 'abi_g16',
   sattypes_rad(70)= 'abi_g17',       dsis(70)= 'abi_g17',
   sattypes_rad(71)= 'iasi_metop-c',  dsis(71)= 'iasi_metop-c',
   sattypes_rad(72)= 'viirs-m_npp',   dsis(72)= 'viirs-m_npp',
   sattypes_rad(73)= 'viirs-m_j1',    dsis(73)= 'viirs-m_j1',
   sattypes_rad(74)= 'avhrr_metop-c', dsis(74)= 'avhrr3_metop-c',
   sattypes_rad(75)= 'abi_g18',       dsis(75)= 'abi_g18',
   sattypes_rad(76)= 'ahi_himawari9', dsis(76)= 'ahi_himawari9',
   sattypes_rad(77)= 'viirs-m_j2',    dsis(77)= 'viirs-m_j2',
   sattypes_rad(78)= 'atms_n21',      dsis(78)= 'atms_n21',
   sattypes_rad(79)= 'cris-fsr_n21',  dsis(79)= 'cris-fsr_n21',
  $SATOBS_ENKF
 /
 &ozobs_enkf
  sattypes_oz(1) = 'sbuv2_n16',
  sattypes_oz(2) = 'sbuv2_n17',
  sattypes_oz(3) = 'sbuv2_n18',
  sattypes_oz(4) = 'sbuv2_n19',
  sattypes_oz(5) = 'omi_aura',
  sattypes_oz(6) = 'gome_metop-a',
  sattypes_oz(7) = 'gome_metop-b',
  sattypes_oz(8) = 'mls30_aura',
  sattypes_oz(9) = 'ompsnp_npp',
  sattypes_oz(10) = 'ompstc8_npp',
  sattypes_oz(11) = 'ompstc8_n20',
  sattypes_oz(12) = 'ompsnp_n20',
  sattypes_oz(13) = 'ompslp_npp',
  sattypes_oz(14) = 'ompstc8_n21',
  sattypes_oz(15) = 'ompsnp_n21',
  sattypes_oz(16) = 'gome_metop-c',
  $OZOBS_ENKF
 /"
;;

    *)

# EXIT out for unresolved regression test

    echo "unknown $regtest"
    exit 1

esac
