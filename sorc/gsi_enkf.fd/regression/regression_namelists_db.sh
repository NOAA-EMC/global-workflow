job_name=$1

case $job_name in

    global_4denvar)

# Define namelist for global hybrid run

export gsi_namelist="

 &SETUP
   miter=1,niter(1)=2,niter(2)=5,
   niter_no_qc(1)=1,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=82,factqmin=0.5,factqmax=0.0002,deltim=400,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=55,gpstop=55.,
   use_gfs_nemsio=.false.,use_gfs_ncio=.true.,lrun_subdirs=.true.,use_readin_anl_sfcmask=.true.,
   crtm_coeffs_path='./crtm_coeffs/',
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
   diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,thin4d=.true.,cwoption=3,
   verbose=.false.,imp_physics=8,lupp=.true.,binary_diag=.false.,netcdf_diag=.true.,
   lobsdiag_forenkf=.false.,
   nhr_anal=3,6,9,
   l4densvar=.true.,ens_nstarthr=3,nhr_obsbin=1,nhr_assimilation=6,lwrite4danl=.true., tzr_qc=1,sfcnst_comb=.true.,
   write_fv3_incr=.true.,incvars_to_zero= 'liq_wat_inc','icmr_inc',incvars_zero_strat='sphum_inc','liq_wat_inc','icmr_inc',
   incvars_efold=5,
   use_gfs_ncio=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP_B,JCAP=$JCAP,NLAT=$NLAT,NLON=$LONA,nsig=$LEVS,
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
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,ljc4tlevs=.true.,
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=2,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   baldiag_full=.false.,baldiag_inc=.false.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.04,
   use_poq7=.true.,qc_noirjaco3_pole=.true.,vqc=.false.,
   aircraft_t_bc=.true.,biaspredt=1.0e5,upd_aircraft=.true.,cleanup_tail=.true.
   nvqc=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=1450.0,dmesh(2)=1500.0,dmesh(3)=1000.0,time_window_max=0.5,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                dval    dthin dsfcalc
   prepbufr       ps          null        ps                  0.0     0     0
   prepbufr       t           null        t                   0.0     0     0
   prepbufr_profl t           null        t                   0.0     0     0
   prepbufr       q           null        q                   0.0     0     0
   prepbufr_profl q           null        q                   0.0     0     0
   prepbufr       pw          null        pw                  0.0     0     0
   prepbufr       uv          null        uv                  0.0     0     0
   prepbufr_profl uv          null        uv                  0.0     0     0
   satwndbufr     uv          null        uv                  0.0     0     0
   prepbufr       spd         null        spd                 0.0     0     0
   prepbufr       dw          null        dw                  0.0     0     0
   radarbufr      rw          null        rw                  0.0     0     0
   nsstbufr       sst         nsst        sst                 0.0     0     0
   gpsrobufr      gps_bnd     null        gps                 0.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi            0.0    -1     0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi             0.0    -1     0
   sbuvbufr_      sbuv2       n16         sbuv8_n16           0.0     0     0
   sbuvbufr_      sbuv2       n17         sbuv8_n17           0.0     0     0
   sbuvbufr_      sbuv2       n18         sbuv8_n18           0.0     0     0
   gimgrbufr_     goes_img    g11         imgr_g11            0.0     1     0
   gimgrbufr_     goes_img    g12         imgr_g12            0.0     1     0
   airsbufr_      airs        aqua        airs_aqua           0.0     1     1
   amsuabufr_skip amsua       n15         amsua_n15           0.0     1     1
   amsuabufr_skip amsua       n18         amsua_n18           0.0     1     1
   amsuabufr_skip amsua       metop-a     amsua_metop-a       0.0     1     1
   airsbufr_skip  amsua       aqua        amsua_aqua          0.0     1     1
   amsubbufr_     amsub       n17         amsub_n17           0.0     1     1
   mhsbufr_skip   mhs         n18         mhs_n18             0.0     1     1
   mhsbufr_skip   mhs         metop-a     mhs_metop-a         0.0     1     1
   ssmitbufr      ssmi        f15         ssmi_f15            0.0     1     0
   amsrebufr      amsre_low   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_mid   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_hig   aqua        amsre_aqua          0.0     1     0
   ssmisbufr      ssmis       f16         ssmis_f16           0.0     1     0
   ssmisbufr      ssmis       f17         ssmis_f17           0.0     1     0
   ssmisbufr      ssmis       f18         ssmis_f18           0.0     1     0
   gsnd1bufr_skip sndrd1      g12         sndrD1_g12          0.0     1     0
   gsnd1bufr_skip sndrd2      g12         sndrD2_g12          0.0     1     0
   gsnd1bufr_skip sndrd3      g12         sndrD3_g12          0.0     1     0
   gsnd1bufr_skip sndrd4      g12         sndrD4_g12          0.0     1     0
   gsnd1bufr_skip sndrd1      g11         sndrD1_g11          0.0     1     0
   gsnd1bufr_skip sndrd2      g11         sndrD2_g11          0.0     1     0
   gsnd1bufr_skip sndrd3      g11         sndrD3_g11          0.0     1     0
   gsnd1bufr_skip sndrd4      g11         sndrD4_g11          0.0     1     0
   gsnd1bufr_skip sndrd1      g13         sndrD1_g13          0.0     1     0
   gsnd1bufr_skip sndrd2      g13         sndrD2_g13          0.0     1     0
   gsnd1bufr_skip sndrd3      g13         sndrD3_g13          0.0     1     0
   gsnd1bufr_skip sndrd4      g13         sndrD4_g13          0.0     1     0
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
   avhambufr      avhrr       metop-a     avhrr3_metop-a      0.0     1     0
   avhpmbufr      avhrr       n18         avhrr3_n18          0.0     1     0
   avhambufr      avhrr       metop-b     avhrr3_metop-b      0.0     1     0
   avhpmbufr      avhrr       n19         avhrr3_n19          0.0     1     0
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     3     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     1     0
   abibufr        abi         g16         abi_g16             0.0     1     0
   abibufr        abi         g17         abi_g17             0.0     1     0
   rapidscatbufr  uv          null        uv                  0.0     0     0
   ompsnpbufr     ompsnp      npp         ompsnp_npp          0.0     0     0
   ompstcbufr     ompstc8     npp         ompstc8_npp         0.0     2     0
   amsuabufr      amsua       metop-c     amsua_metop-c       0.0     1     1
   mhsbufr        mhs         metop-c     mhs_metop-c         0.0     1     1
   iasibufr       iasi        metop-c     iasi_metop-c        0.0     1     1
   ompslpbufr     ompslp      npp         ompslp_npp          0.0     1     1
::
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
   $LAGDATA
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
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${global_4denvar_T670_adate},
   obhourset=0.,
   $SINGLEOB
 /
 &NST
   nst_gsi=3,nstinfo=4,fac_dtl=1,fac_tsl=1,zsea1=0,zsea2=0,
   $NST
 /
"
;;

RTMA)

# Define namelist for RTMA runs

export gsi_namelist="

 &SETUP
   miter=2,niter(1)=2,niter(2)=1,
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
   tlnmc_option=0,nstrong=1,nvmodes_keep=20,period_max=3.,
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
!  dfile          dtype       dplat       dsis          dval   dthin dsfcalc
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
 
 &NST
 /
"
;;

    rrfs_3denvar_rdasens)

# Define namelist for rrfs 3d hybrid envar run with global ensembles

export gsi_namelist="

 &SETUP
   miter=1,niter(1)=2,niter(2)=2,
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

# Define namelist for hafs 3d hybrid envar run with global ensembles

export gsi_namelist="

 &SETUP
   miter=1,niter(1)=2,niter(2)=2,
   niter_no_qc(1)=1,niter_no_qc(2)=0,
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

*)

# EXIT out for unresolved job_name

    echo "unknown $job_name"
    exit 1

esac
