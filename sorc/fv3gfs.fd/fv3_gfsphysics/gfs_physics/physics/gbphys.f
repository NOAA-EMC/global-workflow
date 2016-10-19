!at tune step========================================================= !
!  description:                                                         !
!                                                                       !
!     gbphys is the driver subroutine to invoke GFS AM physics          !
!     (except radiation but radiative heating is applied here)          !
!     at physics time steps                                             !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call gbphys                                                        !
!       inputs:                                                         !
!         ( im,ix,levs,lsoil,lsm,ntrac,ncld,ntoz,ntcw,ntke,             !
!           nmtvr,nrcm,ko3,lonr,latr,jcap,num_p3d,num_p2d,npdf3d,       !
!           kdt,lat,me,pl_coeff,nlons,ncw,flgmin,crtrh,cdmbgwd,         !
!           ccwf,dlqf,ctei_rm,clstp,cgwf,prslrd0,dtp,dtf,fhour,solhr,   !
!           slag,sdec,cdec,sinlat,coslat,pgr,ugrs,vgrs,                 !
!           tgrs,qgrs,vvel,prsi,prsl,prslk,prsik,phii,phil,             !
!           rann,prdout,poz,dpshc,hprime,xlon,xlat,dx, dy, area,        !
!           slope,shdmin,shdmax,snoalb,tg3,slmsk,vfrac,                 !
!           vtype,stype,uustar,oro,oro_uf,coszen,sfcdsw,sfcnsw,         !
!           sfcnirbmd,sfcnirdfd,sfcvisbmd,sfcvisdfd,                    !
!           sfcnirbmu,sfcnirdfu,sfcvisbmu,sfcvisdfu,                    !
!           sfcdlw,tsflw,sfcemis,sfalb,swh,swhc,hlw,hlwc,hlwd,lsidea,   !
!           ras,pre_rad,ldiag3d,lgocart,lssav,lssav_cpl                 !
!           xkzm_m,xkzm_h,xkzm_s,psautco,prautco,evpco,wminco,          !
!           pdfcld,shcnvcw,sup,redrag,hybedmf,dspheat,                  !
!           flipv,old_monin,cnvgwd,shal_cnv,sashal,newsas,cal_pre,      !
!           mom4ice,mstrat,trans_trac,nst_fcst,moist_adj,               !
!           thermodyn_id, sfcpress_id, gen_coord_hybrid,levr,adjtrc,nnp,!
!           cscnv,nctp,do_shoc,shocaftcnv,ntot3d,ntot2d,                !
!       input/outputs:                                                  !
!           hice,fice,tisfc,tsea,tprcp,cv,cvb,cvt,                      !
!           srflag,snwdph,weasd,sncovr,zorl,canopy,                     !
!           ffmm,ffhh,f10m,srunoff,evbsa,evcwa,snohfa,                  !
!           transa,sbsnoa,snowca,soilm,tmpmin,tmpmax,                   !
!           dusfc,dvsfc,dtsfc,dqsfc,totprcp,gflux,                      !
!           dlwsfc,ulwsfc,suntim,runoff,ep,cldwrk,                      !
!           dugwd,dvgwd,psmean,cnvprcp,spfhmin,spfhmax,rain,rainc,      !
!           dt3dt,dq3dt,du3dt,dv3dt,dqdt_v,cnvqc_v,acv,acvb,acvt,       !
!           slc,smc,stc,upd_mf,dwn_mf,det_mf,phy_f3d,phy_f2d,           !
!           dusfc_cpl, dvsfc_cpl, dtsfc_cpl, dqsfc_cpl,                 !
!           dlwsfc_cpl,dswsfc_cpl,dnirbm_cpl,dnirdf_cpl,                !
!           dvisbm_cpl,dvisdf_cpl,rain_cpl,  nlwsfc_cpl,nswsfc_cpl,     !
!           nnirbm_cpl,nnirdf_cpl,nvisbm_cpl,nvisdf_cpl,                !
!           xt,xs,xu,xv,xz,zm,xtts,xzts,d_conv,ifd,dt_cool,Qrain,       !
!           phy_fctd,                                                   !
!       outputs:                                                        !
!           gt0,gq0,gu0,gv0,t2m,q2m,u10m,v10m,                          !
!           zlvl,psurf,hpbl,pwat,t1,q1,u1,v1,                           !
!           chh,cmm,dlwsfci,ulwsfci,dswsfci,uswsfci,dusfci,dvsfci,      !
!           dtsfci,dqsfci,gfluxi,epi,smcwlt2,smcref2,wet1,              !
!           dusfci_cpl,dvsfci_cpl,dtsfci_cpl,dqsfci_cpl,                !
!           dlwsfci_cpl,dswsfci_cpl,                                    !
!           dnirbmi_cpl,dnirdfi_cpl,dvisbmi_cpl,dvisdfi_cpl,            !
!           nlwsfci_cpl,nswsfci_cpl,                                    !
!           nnirbmi_cpl,nnirdfi_cpl,nvisbmi_cpl,nvisdfi_cpl,            !
!           t2mi_cpl,q2mi_cpl,                                          !
!           u10mi_cpl,v10mi_cpl,tseai_cpl,psurfi_cpl,oro_cpl,slmsk_cpl, !
!           tref, z_c, c_0, c_d, w_0, w_d, rqtk,                        !
!           hlwd,lsidea                         )                       !
!                                                                       !
!  subprograms called:                                                  !
!                                                                       !
!     get_prs,  dcyc2t2_pre_rad (testing),    dcyc2t3,  sfc_diff,       !
!     sfc_ocean,sfc_drv,  sfc_land, sfc_sice, sfc_diag, moninp1,        !
!     moninp,   moninq1,  moninq,   gwdps,    ozphys,   get_phi,        !
!     sascnv,   sascnvn,  rascnv,   cs_convr, gwdc,     shalcvt3,shalcv,!
!     shalcnv,  cnvc90,   lrgscl,   gsmdrive, gscond,   precpd,         !
!     progt2.                                                           !
!                                                                       !
!                                                                       !
!  program history log:                                                 !
!           19xx  - ncep mrf/gfs                                        !
!           2002  - s. moorthi  modify and restructure and add Ferrier  !
!                               microphysics as an option               !
!           200x  - h. juang    modify (what?)                                  !
!      nov  2004  - x. wu       modify sea-ice model                    !
!      may  2005  - s. moorthi  modify and restructure                  !
!           2005  - s. lu       modify to include noah lsm              !
!      oct  2006  - h. wei      modify lsm options to include both      !
!                               noah and osu lsms.                      !
!           2006  - s. moorthi  added a. johansson's convective gravity !
!                               wave parameterization code              !
!           2007  - s. moorthi  added j. han's modified pbl/sas options !
!      dec  2007  - xu li       modified the operational version for    !
!                               nst model                               !
!           2008  - s. moorthi  applied xu li's nst model to new gfs    !
!      mar  2008  - y.-t. hou   added sunshine duration var (suntim) as !
!                     an input/output argument.                         !
!           2008  - jun wang    added spfhmax/spfhmin as input/output.  !
!      apr  2008  - y.-t. hou   added lw sfc emissivity var (sfcemis),  !
!                     define the lw sfc dn/up fluxes in two forms: atmos!
!                     and ground. also changed sw sfc net flux direction!
!                     (positive) from ground -> atmos to the direction  !
!                     of atmos -> ground. recode the program and add    !
!                     program documentation block.
!           2008/ - s. moorthi and y.t. hou upgraded the code to more   !
!           2009    modern form and changed all the inputs to MKS units.!
!      feb  2009  - s. moorthi  upgraded to add Hochun's gocart changes !
!      jul  2009  - s. moorthi  added rqtk for sela's semi-lagrangian   !
!      aug  2009  - s. moorthi  added j. han and h. pan updated shallow !
!                               convection package                      !
!      sep  2009  - s. moorthi  updated for the mcica (rrtm3) radiation !
!      dec  2010  - sarah lu    lgocart added to input arg;             !
!                               compute dqdt_v if inline gocart is on   !
!      feb  2011  - sarah lu    add the option to update surface diag   !
!                               fields (t2m,q2m,u10m,v10m) at the end   !
!      Jun  2011  - s. moorthi and Xu Li - updated the nst model        !
!                               !
!      sep  2011  - sarah lu    correct dqdt_v calculations             !
!      apr  2012  - henry juang add idea                                !
!      sep  2012  - s. moorthi  merge with operational version          !
!      Mar  2013  - Jun Wang    set idea heating rate to tmp tendency   !
!      May  2013  - Jun Wang    tmp updated after idea phys             !
!      Jun  2013  - s. moorthi  corrected a bug in 3d diagnostics for T !
!      Aug  2013  - s. moorthi updating J. Whitekar's changes related   !
!                              to stochastic physics perturnbation      !
!      Oct  2013  - Xingren Wu  add dusfci/dvsfci                       !
!      Mar  2014  - Xingren Wu  add "_cpl" for coupling                 !
!      Mar  2014  - Xingren Wu  add "nir/vis beam and nir/vis diff"     !
!      Apr  2014  - Xingren Wu  add "NET LW/SW including nir/vis"       !
!      Jan  2014  - Jun Wang    merge Moorthi's gwdc change and H.Juang !
!                               and F. Yang's energy conversion from GWD!
!      jan  2014  - y-t hou     revised sw sfc spectral component fluxes!
!                     for coupled mdl, added estimation of ocean albedo !
!                     without ice contamination.                        !
!      Jun  2014  - Xingren Wu  update net SW fluxes over the ocean     !
!                               (no ice contamination)                  !
!      Jul  2014  - Xingren Wu  add Sea/Land/Ice Mask - slmsk_cpl       !
!      Jul  2014  - s. moorthi  merge with standalone gfs and cleanup   !
!      Aug  2014  - s. moorthi  add tracer fixer                        !
!      Sep  2014  - Sarah Lu    disable the option to compute tracer    !
!                               scavenging in GFS phys (set fscav=0.)   !
!      Dec  2014  - Jun Wang    add cnvqc_v for gocart                  !
!      ---  2014  - D. Dazlich  Added Chikira-Sugiyama (CS) convection  !
!                               as an option in opr GFS.                !
!      Apr  2015    S. Moorthi  Added CS scheme to NEMS/GSM             !
!      Jun  2015    S. Moorthi  Added SHOC  to NEMS/GSM                 !
!  ====================  definition of variables  ====================  !
!                                                                       !
!  inputs:                                                       size   !
!     ix, im   - integer, horiz dimention and num of used pts      1    !
!     levs     - integer, vertical layer dimension                 1    !
!     lsoil    - integer, number of soil layers                    1    !
!     lsm      - integer, flag for land surface model to use       1    !
!                =0  for osu lsm; =1  for noah lsm                      !
!     ntrac    - integer, number of tracers                        1    !
!     ncld     - integer, number of cloud species                  1    !
!     ntoz     - integer, ozone location in the tracer array       1    !
!     ntcw     - integer, cloud condensate location in the tracer  1    !
!                         array                                    1    !
!     ntke     - integer, tke location in the tracer array         1    !
!     nmtvr    - integer, number of topographic variables such as  1    !
!                         variance etc used in the GWD parameterization !
!     nrcm     - integer, second dimension for the random number   1    !
!                         array rann                                    !
!     ko3      - integer, number of layers for ozone data          1    !
!     lonr,latr- integer, number of lon/lat points                 1    !
!     jcap     - integer, number of spectral wave trancation       1    !
!                         used only by sascnv shalcnv                   !
!     num_p3d  - integer, number of 3D arrays needed for           1    !
!                          microphysics                                 !
!     num_p2d  - integer, number of 2D arrays needed for           1    !
!                         microphysics                                  !
!     npdf3d   - integer, number of 3d arrays associated with pdf  1    !
!                         based clouds/microphysics                     !
!     kdt       -integer, number of the current time step          1    !
!     lat       -integer, latitude index - used for debug prints   1    !
!     me        -integer, pe number - used for debug prints        1    !
!     pl_coeff - integer, number coefficients in ozone forcing     1    !
!     nlons    - integer, number of total grid points in a latitude     !
!                         circle through a point                   im   !
!     ncw      - integer, range of droplet number concentrations for    !
!                         Ferrier microphysics                     2    !
!     flgmin   - real, range of  minimum large ice fraction for         !
!                         Ferrier microphys                        2    !
!     crtrh    - real, critical relative humidity at the surface, PBL   !
!                      top and at the top of the atmosphere        3    !
!     cdmbgwd  - real, multiplication factors for cdmb and gwd     2    !
!     ccwf     - real, multiplication factor for critical cloud         !
!                      workfunction for RAS                        2    !
!     dlqf     - real, factor for cloud condensate detrainment from     !
!                      cloud edges (RAS)                           2    !
!     ctei_rm  - real, critical cloud top entrainment instability  2    !
!                      criteria (used if mstrat=.true.)                 !
!     clstp    - real, index used by cnvc90 (for convective clouds)1    !
!                      legacy stuff - does not affect forecast          !
!     cgwf     - real, multiplication factor for convective GWD    2    !
!     prslrd0  - real, pressure level from which Rayleigh Damping       !
!                      is applied                                  1    !
!     dtp      - real, physics time step in seconds                1    !
!     dtf      - real, dynamics time step in seconds               1    !
!     fhour    - real, forecast hour                               1    !
!     solhr    - real, fcst hour at the end of prev time step      1    !
!     slag     - real, equation of time ( radian )                 1    !
!     sdec,cdec- real, sin and cos of the solar declination angle  1    !
!     sinlat   - real, sin of latitude                             im   !
!     coslat   - real, cos of latitude                             im   !
!     pgr      - real, surface pressure (Pa)                       im   !
!     ugrs,vgrs- real, u/v component of layer wind              ix,levs !
!     tgrs     - real, layer mean temperature ( k )             ix,levs !
!     qgrs     - real, layer mean tracer concentration     ix,levs,ntrac!
!     vvel     - real, layer mean vertical velocity (Pa/s)      ix,levs !
!     prsi     - real, pressure at layer interfaces             ix,levs+1
!     prsl     - real, mean layer presure                       ix,levs !
!     prsik    - real, Exner function at layer interface        ix,levs+1
!     prslk    - real, Exner function at layer                  ix,levs !
!     phii     - real, interface geopotential height            ix,levs+1
!     phil     - real, layer geopotential height                ix,levs !
!     rann     - real, random number array (0-1)                ix,nrcm !
!     prdout   - real, ozone forcing data                       ix,ko3,pl_coeff!
!     poz      - real, ozone forcing data level pressure (ln(Pa))  ko3  !
!     dpshc    - real, maximum pressure depth for shallow convection im   !
!     hprime   - real, orographic std dev                       ix,nmtvr!
!     xlon,xlat- real, longitude and latitude ( radian )           im   !
!     slope    - real, sfc slope type for lsm                      im   !
!     shdmin   - real, min fractional coverage of green veg        im   !
!     shdmax   - real, max fractnl cover of green veg (not used)   im   !
!     snoalb   - real, max snow albedo over land (for deep snow)   im   !
!     tg3      - real, deep soil temperature                       im   !
!     slmsk    - real, sea/land/ice mask (=0/1/2)                  im   !
!     vfrac    - real, vegetation fraction                         im   !
!     vtype    - real, vegetation type                             im   !
!     stype    - real, soil type                                   im   !
!     uustar   - real, boundary layer parameter                    im   !
!     oro      - real, orography                                   im   !
!     oro_uf   - real, unfiltered orography                        im   !
!     coszen   - real, avg cosz over daytime sw radiation interval im   !
!     sfcdsw   - real, total sky sfc downward sw flux ( w/m**2 )   im   !
!     sfcnsw   - real, total sky sfc netsw flx into ground(w/m**2) im   !
!     sfcdlw   - real, total sky sfc downward lw flux ( w/m**2 )   im   !
!     tsflw    - real, sfc air (layer 1) temp over lw interval (k) im   !
!     sfcemis  - real, sfc lw emissivity ( fraction )              im   !
!     sfalb    - real, mean sfc diffused sw albedo                 im   !
!     sfcnirbmu- real, sfc nir-beam sw upward flux (w/m2)          im   !
!     sfcnirdfu- real, sfc nir-diff sw upward flux (w/m2)          im   !
!     sfcvisbmu- real, sfc uv+vis-beam sw upward flux (w/m2)       im   !
!     sfcvisdfu- real, sfc uv+vis-diff sw upward flux (w/m2)       im   !
!     sfcnirbmd- real, sfc nir-beam sw downward flux (w/m2)        im   !
!     sfcnirdfd- real, sfc nir-diff sw downward flux (w/m2)        im   !
!     sfcvisbmd- real, sfc uv+vis-beam sw downward flux (w/m2)     im   !
!     sfcvisdfd- real, sfc uv+vis-diff sw downward flux (w/m2)     im   !
!     swh      - real, total sky sw heating rates ( k/s )       ix,levs !
!     swhc     - real, clear sky sw heating rates ( k/s )       ix,levs !
!     hlw      - real, total sky lw heating rates ( k/s )       ix,levs !
!     hlwc     - real, clear sky lw heating rates ( k/s )       ix,levs !
!     hlwd     - real, idea  sky lw heating rates ( k/s )       ix,levs !
!     ras      - logical, flag for ras convection scheme           1    !
!     cscnv    - logical, flag for Chikira-Sugiyama convection     1    !
!     nctp     - integer, number of cloud types in CS scheme       1    !
!     do_shoc  - logical, flag for SHOC                            1    !
!     shocaftcnv - logical, flag for SHOC                          1    !
!     ntot3d   - integer, number of total 3d fields for phy_f3d    1    !
!     ntot2d   - integer, number of total 2d fields for phy_f2d    1    !
!     pre_rad  - logical, flag for testing purpose                 1    !
!     ldiag3d  - logical, flag for 3d diagnostic fields            1    !
!     lgocart  - logical, flag for 3d diagnostic fields for gocart 1    !
!     lssav    - logical, flag controls data store and output      1    !
!     lssav_cpl- logical, flag for save data for A/O/I coupling    1    !
!     flipv    - logical, flag for vertical direction flip (ras)   1    !
!     xkzm_m   - real, background vertical diffusion for momentum  1    !
!     xkzm_h   - real, background vertical diffusion for heat, q   1    !
!     xkzm_s   - real, sigma threshold for background mom. diffusn 1    !
!     psautco  - real, auto conversion coeff from ice to snow      2    !
!     prautco  - real, auto conversion coeff from cloud to rain    2    !
!     evpco    - real, coeff for evaporation of largescale rain    1    !
!     wminco   - real, water and ice minimum threshold for Zhao    1    !
!     pdfcld   - logical, flag for pdfcld                          1    !
!     shcnvcw  - logical, flag for shallow convective cloud        1    !
!     sup      - real, supsaturation for ho. nucleation of ice     1    !
!     redrag   - logical, flag for reduced drag coeff. over sea    1    !
!     hybedmf  - logical, flag for hybrid edmf pbl scheme          1    !
!     dspheat  - logical, flag for tke dissipative heating         1    !
!     old_monin- logical, flag for diff monin schemes              1    !
!     cnvgwd   - logical, flag for conv gravity wave drag          1    !
!     shal_cnv - logical, flag for calling shallow convection      1    !
!     sashal   - logical, flag for new shallow conv scheme         1    !
!     newsas   - logical, flag for new sas conv scheme             1    !
!     cal_pre  - logical, flag controls precip type algorithm      1    !
!     mom4ice  - logical, flag controls mom4 sea-ice               1    !
!     mstrat   - logical, flag for moorthi approach for stratus    1    !
!     trans_trac-logical, flag for convective transport of tracers 1    !
!     nst_fcst  -integer, flag 0 for no nst, 1 for uncoupled nst        !
!                          and 2 for coupled NST                   1    !
!     moist_adj- logical, flag for moist convective adjustment     1    !
!     thermodyn_id - integer, valid for GFS only for get_prs/phi   1    !
!     sfcpress_id  - integer, valid for GFS only for get_prs/phi   1    !
!     gen_coord_hybrid - logical for pk=ak+bk*ps+ck*theta (Henry)  1    !
!     levr  - integer, the number of layers GFS Radiative heating calculated at 1 !
!     adjtrc   - real, dynamics adjustments to tracers             ntrac !
!     nnp      - integer, physics substep number                   1     !
!                                                                        !
!  input/outputs:                                                        !
!     hice      - real, sea-ice thickness                           im   !
!     fice      - real, sea-ice concentration                       im   !
!     tisfc     - real, sea-ice temperature                         im   !
!     tsea      - real, ground surface temperature ( k )            im   !
!     tprcp     - real, total precipitation                         im   !
!     the following three variables do not affect the forecast           !
!     cv,        -real, convective clouds amountt                   im   !
!     cvb        -real, convective clouds base pressure (kPa)       im   !
!     cvt        -real, convective clouds top  pressure (kPa)       im   !
!     srflag    - real, snow/rain flag for precipitation            im   !
!     snwdph    - real, actual snow depth (mm) over land/sea ice    im   !
!     weasd     - real, water equiv of accumulated  snow depth (kg/m**2) !
!                      over land and sea ice                       im    !
!     sncovr    - real, snow cover over land                        im   !
!     zorl      - real, surface roughness                           im   !
!     canopy    - real, canopy water                                im   !
!     ffmm      - real, fm parameter from PBL scheme                im   !
!     ffhh      - real, fh parameter from PBL scheme                im   !
!     f10m      - real, fm at 10m                                   im   !
!     srunoff   - real, surface water runoff (from lsm)             im   !
!     evbsa     - real, noah lsm diagnostics                        im   !
!     evcwa     - real, noah lsm diagnostics                        im   !
!     snohfa    - real, noah lsm diagnostics                        im   !
!     transa    - real, noah lsm diagnostics                        im   !
!     sbsnoa    - real, noah lsm diagnostics                        im   !
!     snowca    - real, noah lsm diagnostics                        im   !
!     soilm     - real, soil moisture                               im   !
!     tmpmin    - real, min temperature at 2m height (k)            im   !
!     tmpmax    - real, max temperature at 2m height (k)            im   !
!     dusfc     - real, u component of surface stress               im   !
!     dvsfc     - real, v component of surface stress               im   !
!     dtsfc     - real, sensible heat flux (w/m2)                   im   !
!     dqsfc     - real, latent heat flux (w/m2)                     im   !
!     totprcp   - real, accumulated total precipitation (kg/m2)     im   !
!     gflux     - real, groud conductive heat flux                  im   !
!     dlwsfc    - real, time accumulated sfc dn lw flux ( w/m**2 )  im   !
!     ulwsfc    - real, time accumulated sfc up lw flux ( w/m**2 )  im   !
!     suntim    - real, sunshine duration time (s)                  im   !
!     runoff    - real, total water runoff                          im   !
!     ep        - real, potential evaporation                       im   !
!     cldwrk    - real, cloud workfunction (valid only with sas)    im   !
!     dugwd     - real, vertically integrated u change by OGWD      im   !
!     dvgwd     - real, vertically integrated v change by OGWD      im   !
!     psmean    - real, surface pressure (kPa)                      im   !
!     cnvprcp   - real, accumulated convective precipitation (kg/m2)im   !
!     spfhmin   - real, minimum specific humidity                   im   !
!     spfhmax   - real, maximum specific humidity                   im   !
!     rain      - real, total rain at this time step                im   !
!     rainc     - real, convective rain at this time step           im   !
!     dt3dt     - real, temperature change due to physics           ix,levs,6 !
!     dq3dt     - real, moisture change due to physics              ix,levs,5+pl_coeff!
!     du3dt     - real, u momentum change due to physics            ix,levs,4 !
!     dv3dt     - real, v momentum change due to physics            ix,levs,4 !
!     dqdt_v    - real, total moisture tendency (kg/kg/s)        ix,levs   !
!     cnvqc_v   - real, total convective conensate (kg/kg)       ix,levs   !
!     acv       - real,  array containing accumulated convective clouds im !
!     acvb,acvt - real,  arrays used by cnvc90                      im   !
!     slc       - real, liquid soil moisture                     ix,lsoil!
!     smc       - real, total soil moisture                      ix,lsoil!
!     stc       - real, soil temperature                         ix,lsoil!
!     upd_mf    - real, convective updraft mass flux             ix,levs !
!     dwn_mf    - real, convective downdraft mass flux           ix,levs !
!     det_mf    - real, convective detrainment mass flux         ix,levs !
!  ------- not used below -----------
!     dkh       - real, vertical diffusion coefficient (gocart)  ix,levs !
!     rnp       - real, n cloud precipitation rate     (gocart)  ix,levs !
!  ------- not used  above -----------

!     phy_f3d   - real, 3d arrays saved for restart              ix,levs,num_p3d!
!                                                                  +npdf3d!
!     phy_f2d   - real, 2d arrays save for restart               ix,num_p2d!
!     dusfc_cpl - real, sfc u-momentum flux       for A/O/I coupling im !
!     dvsfc_cpl - real, sfc v-momentum flux       for A/O/I coupling im !
!     dtsfc_cpl - real, sfc sensible heat flux    for A/O/I coupling im !
!     dqsfc_cpl - real, sfc latent heat flux      for A/O/I coupling im !
!     dlwsfc_cpl- real, sfc dnwd lw flux (w/m**2) for A/O/I coupling im !
!     dswsfc_cpl- real, sfc dnwd sw flux (w/m**2) for A/O/I coupling im !
!     dnirbm_cpl- real, sfc nir beam dnwd sw rad flux (w/m**2)       im !
!     dnirdf_cpl- real, sfc nir diff dnwd sw rad flux (w/m**2)       im !
!     dvisbm_cpl- real, sfc uv+vis beam dnwd sw rad flux (w/m**2)    im !
!     dvisdf_cpl- real, sfc uv+vis diff dnwd sw rad flux (w/m**2)    im !
!     nlwsfc_cpl- real, net dnwd lw flux (w/m**2) for A/O/I coupling im !
!     nswsfc_cpl- real, net dnwd sw flux (w/m**2) for A/O/I coupling im !
!     nnirbm_cpl- real, net nir beam dnwd sw rad flux (w/m**2)       im !
!     nnirdf_cpl- real, net nir diff dnwd sw rad flux (w/m**2)       im !
!     nvisbm_cpl- real, net uv+vis beam dnwd sw rad flux (w/m**2)    im !
!     nvisdf_cpl- real, net uv+vis diff dnwd sw rad flux (w/m**2)    im !
!     rain_cpl  - real, total precipitation       for A/O/I coupling im !
!
!     xt        - real, heat content in DTL                         im  !
!     xs        - real, salinity  content in DTL                    im  !
!     xu        - real, u-current content in DTL                    im  !
!     xv        - real, v-current content in DTL                    im  !
!     xz        - real, DTL thickness                               im  !
!     zm        - real, MXL thickness                               im  !
!     xtts      - real, d(xt)/d(ts)                                 im  !
!     xzts      - real, d(xz)/d(ts)                                 im  !
!     d_conv    - real, thickness of Free Convection Layer (FCL)    im  !
!     ifd       - real, index to start DTM run or not               im  !
!     dt_cool   - real, Sub-layer cooling amount                    im  !
!     Qrain     - real, sensible heat flux due to rainfall (watts)  im  !
!     phy_fctd - real, cloud base mass flux for CScnv           ix,nctp !
!                                                                       !
!  outputs:                                                             !
!     gt0       - real, updated temperature                        ix,levs !
!     gq0       - real, updated tracers                            ix,levs,ntrac!
!     gu0       - real, updated zonal wind                         ix,levs !
!     gv0       - real, update meridional wind                     ix,levs !
!     t2m,q2m   - real, 2 meter temperature and humidity            im  !
!     u10m,v10m - real, 10 meater u/v wind speed                    im  !
!     zlvl      - real, layer 1 height (m)                          im  !
!     psurf     - real, surface pressure (Pa)                       im  !
!     hpbl      - real, pbl height (m)                              im  !
!     pwat      - real, precipitable water                          im  !
!     t1        - real, layer 1 temperature (K)                     im  !
!     q1        - real, layer 1 specific humidity (kg/kg)           im  !
!     u1        - real, layer 1 zonal wind (m/s)                    im  !
!     v1        - real, layer 1 merdional wind (m/s)                im  !
!     chh       - real, thermal exchange coefficient                im  !
!     cmm       - real, momentum exchange coefficient               im  !
!     dlwsfci   - real, instantaneous sfc dnwd lw flux ( w/m**2 )   im  !
!     ulwsfci   - real, instantaneous sfc upwd lw flux ( w/m**2 )   im  !
!     dswsfci   - real, instantaneous sfc dnwd sw flux ( w/m**2 )   im  !
!     uswsfci   - real, instantaneous sfc upwd sw flux ( w/m**2 )   im  !
!     dusfci    - real, instantaneous u component of surface stress im  !
!     dvsfci    - real, instantaneous v component of surface stress im  !
!     dtsfci    - real, instantaneous sfc sensible heat flux        im  !
!     dqsfci    - real, instantaneous sfc latent heat flux          im  !
!     gfluxi    - real, instantaneous sfc ground heat flux          im  !
!     epi       - real, instantaneous sfc potential evaporation     im  !
!     smcwlt2   - real, wilting point (volumetric)                  im  !
!     smcref2   - real, soil moisture threshold (volumetric)        im  !
!
!     dusfci_cpl  - real, sfc u-momentum flux at time step AOI cpl  im  !
!     dvsfci_cpl  - real, sfc v-momentum flux at time step AOI cpl  im  !
!     dtsfci_cpl  - real, sfc sensib heat flux at time step AOI cpl im  !
!     dqsfci_cpl  - real, sfc latent heat flux at time step AOI cpl im  !
!     dlwsfci_cpl - real, sfc dnwd lw flux at time step AOI cpl     im  !
!     dswsfci_cpl - real, sfc dnwd sw flux at time step AOI cpl     im  !
!     dnirbmi_cpl - real, sfc nir beam dnwd sw flx rad at time step im  !
!     dnirdfi_cpl - real, sfc nir diff dnwd sw flx rad at time step im  !
!     dvisbmi_cpl - real, sfc uv+vis beam dnwd sw flx at time step  im  !
!     dvisdfi_cpl - real, sfc uv+vis diff dnwd sw flx at time step  im  !
!     nlwsfci_cpl - real, net sfc dnwd lw flux at time step AOI cpl im  !
!     nswsfci_cpl - real, net sfc dnwd sw flux at time step AOI cpl im  !
!     nnirbmi_cpl - real, net nir beam dnwd sw flx rad at time step im  !
!     nnirdfi_cpl - real, net nir diff dnwd sw flx rad at time step im  !
!     nvisbmi_cpl - real, net uv+vis beam dnwd sw flx at time step  im  !
!     nvisdfi_cpl - real, net uv+vis diff dnwd sw flx at time step  im  !
!     ocalnirbm_cpl- real, ocean alb nir beam (no ice) at time step im  !
!     ocalnirdf_cpl- real, ocean alb nir diff (no ice) at time step im  !
!     ocalvisbm_cpl- real, ocean alb vis beam (no ice) at time step im  !
!     ocalvisdf_cpl- real, ocean alb vis diff (no ice) at time step im  !
!     t2mi_cpl    - real, T2m at time step AOI cpl                  im  !
!     q2mi_cpl    - real, Q2m at time step AOI cpl                  im  !
!     u10mi_cpl   - real, U10m at time step AOI cpl                 im  !
!     v10mi_cpl   - real, V10m at time step AOI cpl                 im  !
!     tseai_cpl   - real, sfc temp at time step AOI cpl             im  !
!     psurfi_cpl  - real, sfc pressure at time step AOI cpl         im  !
!     oro_cpl     - real, orography AOI cpl                         im  !
!     slmsk_cpl   - real, Land/Sea/Ice AOI cpl                      im  !

!     tref        - real, Reference Temperature                     im  !
!     z_c         - real, Sub-layer cooling thickness               im  !
!     c_0         - real, coefficient1 to calculate d(Tz)/d(Ts)     im  !
!     c_d         - real, coefficient2 to calculate d(Tz)/d(Ts)     im  !
!     w_0         - real, coefficient3 to calculate d(Tz)/d(Ts)     im  !
!     w_d         - real, coefficient4 to calculate d(Tz)/d(Ts)     im  !
!     rqtk        - real, mass change due to moisture variation     im  !
!     dtdtr       - real, temperature change due to radiative heating   !
!                         per time step (K)                      ix,levs!
!                                                                       !
!  ====================    end of description    =====================  !

      subroutine gbphys                                                 &
!  ---  inputs:
     &    ( im,ix,levs,lsoil,lsm,ntrac,ncld,ntoz,ntcw,ntke,             &
     &      nmtvr,nrcm,ko3,lonr,latr,jcap,num_p3d,num_p2d,npdf3d,       &
     &      kdt,lat,me,pl_coeff,nlons,ncw,flgmin,crtrh,cdmbgwd,         &
     &      ccwf,dlqf,ctei_rm,clstp,cgwf,prslrd0,dtp,dtf,fhour,solhr,   &
     &      slag,sdec,cdec,sinlat,coslat,pgr,ugrs,vgrs,                 &
     &      tgrs,qgrs,vvel,prsi,prsl,prslk,prsik,phii,phil,             &
     &      rann,prdout,poz,dpshc,fscav,fswtr,hprime,xlon,xlat,dx,dy,   &
     &      area,slope,shdmin,shdmax,snoalb,tg3,slmsk,vfrac,            &
     &      vtype,stype,uustar,oro,oro_uf,coszen,sfcdsw,sfcnsw,         &

     &      sfcnirbmd,sfcnirdfd,sfcvisbmd,sfcvisdfd,                    &
     &      sfcnirbmu,sfcnirdfu,sfcvisbmu,sfcvisdfu,                    &
     &      sfcdlw,tsflw,sfcemis,sfalb,swh,swhc,hlw,hlwc,hlwd,lsidea,   &
     &      ras,pre_rad,ldiag3d,lgocart,lssav,lssav_cpl,                &

     &      xkzm_m,xkzm_h,xkzm_s,psautco,prautco,evpco,wminco,          &
     &      pdfcld,shcnvcw,sup,redrag,hybedmf,dspheat,                  &
     &      flipv,old_monin,cnvgwd,shal_cnv,sashal,newsas,cal_pre,      &
     &      mom4ice,mstrat,trans_trac,nst_fcst,moist_adj,               &
     &      thermodyn_id, sfcpress_id, gen_coord_hybrid,levr,adjtrc,nnp,&
     &      cscnv,nctp,do_shoc,shocaftcnv,ntot3d,ntot2d,                &
!  ---  input/outputs:
     &      hice,fice,tisfc,tsea,tprcp,cv,cvb,cvt,                      &
     &      srflag,snwdph,weasd,sncovr,zorl,canopy,                     &
     &      ffmm,ffhh,f10m,srunoff,evbsa,evcwa,snohfa,                  &
     &      transa,sbsnoa,snowca,soilm,tmpmin,tmpmax,                   &
     &      dusfc,dvsfc,dtsfc,dqsfc,totprcp,gflux,                      &
     &      dlwsfc,ulwsfc,suntim,runoff,ep,cldwrk,                      &
     &      dugwd,dvgwd,psmean,cnvprcp,spfhmin,spfhmax,rain,rainc,      &

     &      dt3dt,dq3dt,du3dt,dv3dt,dqdt_v,cnvqc_v,acv,acvb,acvt,       &
     &      slc,smc,stc,upd_mf,dwn_mf,det_mf,phy_f3d,phy_f2d,           &
!    &      slc,smc,stc,upd_mf,dwn_mf,det_mf,dkh,rnp,phy_f3d,phy_f2d,   &
     &      dusfc_cpl, dvsfc_cpl, dtsfc_cpl, dqsfc_cpl,                 &
     &      dlwsfc_cpl,dswsfc_cpl,dnirbm_cpl,dnirdf_cpl,                & 
     &      dvisbm_cpl,dvisdf_cpl,rain_cpl,  nlwsfc_cpl,nswsfc_cpl,     &
     &      nnirbm_cpl,nnirdf_cpl,nvisbm_cpl,nvisdf_cpl,                &

     &      xt,xs,xu,xv,xz,zm,xtts,xzts,d_conv,ifd,dt_cool,Qrain,       &
     &      phy_fctd,                                                   &
!  ---  outputs:
     &      gt0,gq0,gu0,gv0,t2m,q2m,u10m,v10m,                          &
     &      zlvl,psurf,hpbl,pwat,t1,q1,u1,v1,                           &
     &      chh,cmm,dlwsfci,ulwsfci,dswsfci,uswsfci,dusfci,dvsfci,      &
     &      dtsfci,dqsfci,gfluxi,epi,smcwlt2,smcref2,wet1,sr,           &

     &      rqtk,                                                       &
!       Stochastic physics perturnbation
     &      dtdtr,                                                      &

     &      dusfci_cpl,  dvsfci_cpl, dtsfci_cpl, dqsfci_cpl,            &
     &      dlwsfci_cpl, dswsfci_cpl,                                   &
     &      dnirbmi_cpl, dnirdfi_cpl, dvisbmi_cpl, dvisdfi_cpl,         &
     &      nlwsfci_cpl, nswsfci_cpl,                                   &
     &      nnirbmi_cpl, nnirdfi_cpl, nvisbmi_cpl, nvisdfi_cpl,         &
     &      t2mi_cpl,    q2mi_cpl,    u10mi_cpl,   v10mi_cpl,           &
     &      tseai_cpl,   psurfi_cpl,                                    &
!    &      tseai_cpl,   psurfi_cpl,  oro_cpl,     slmsk_cpl,           &

     &      tref, z_c, c_0, c_d, w_0, w_d                               &
     &      )
!
      use machine ,   only : kind_phys
      use physcons,   only : con_cp, con_fvirt, con_g, con_rd, con_rv,  &
     &                       con_hvap, con_hfus, con_rerth, con_pi,     &
     &                       rhc_max, dxmin, dxinv, pa2mb, rlapse,      &
     &                       ozcalc,nocnv
      use cs_conv, only : cs_convr
!---GFDL addition
      use gfs_fv3_needs, only : get_prs_fv3, get_phi_fv3

      implicit none
!
!  ---  some constant parameters:

      real(kind=kind_phys), parameter :: hocp    = con_hvap/con_cp
!    &,                                  fhourpr = 0.0
     &,                                  qmin    = 1.0e-10
     &,                                  p850    = 85000.0
     &,                                  epsq    = 1.e-20
     &,                                  hsub    = con_hvap+con_hfus
     &,                                  czmin   = 0.0001      ! cos(89.994)

!  ---  inputs:

!  note: lgocart is the logical flag for in-line gocart;

      integer, intent(in) :: ix,   im,   levs, lsoil,   lsm,     ntrac, &
     &                       ncld, ntoz, ntcw, nmtvr,   nrcm,    ko3,   &
     &                       lonr, latr, jcap, num_p3d, num_p2d, kdt,   &
     &                       me,   pl_coeff, lat, npdf3d,               &
     &                       thermodyn_id, sfcpress_id, levr, nnp, nctp,&
     &                       ntke, ntot3d, ntot2d


      integer, intent(in) :: nlons(im), ncw(2), nst_fcst

      logical, intent(in) :: ras,        pre_rad,   ldiag3d, flipv,     &
     &                       old_monin,  cnvgwd,    sashal,  newsas,    &
     &                       redrag,     hybedmf,   dspheat,            &
     &                       lssav,                 mom4ice, mstrat,    &
     &                       trans_trac, moist_adj, cal_pre, cscnv,     &
     &                       shal_cnv,   gen_coord_hybrid,   lgocart,   &
     &                       lsidea,     lssav_cpl, pdfcld, shcnvcw,    &
     &                       do_shoc, shocaftcnv

      real(kind=kind_phys) :: adjtrc(ntrac)

      real(kind=kind_phys), dimension(im),            intent(in) ::     &
     &      sinlat, coslat, pgr,    dpshc,  xlon,   xlat, dx, dy, area, &
     &      slope,  shdmin, shdmax, snoalb, tg3,    slmsk,  vfrac,      &
     &      vtype,  stype,  oro,    coszen, sfcnsw, sfcdsw,             &
     &      sfcnirbmu,      sfcnirdfu,      sfcvisbmu,      sfcvisdfu,  &
     &      sfcnirbmd,      sfcnirdfd,      sfcvisbmd,      sfcvisdfd,  &
     &      sfcdlw, tsflw,  sfalb,  sfcemis, oro_uf

!GFDL Fixed with the okay of Patrick Tripp at NCEP/EMC
      real(kind=kind_phys), dimension(im),            intent(inout) ::  &
     &      uustar

      real(kind=kind_phys), dimension(ix,levs),       intent(in) ::     &
     &   ugrs, vgrs, tgrs, vvel, prsl, prslk, swh, swhc, hlw, hlwc

!idea add by hmhj
      real(kind=kind_phys), intent(in) ::  hlwd(ix,levs,6)

      real(kind=kind_phys), intent(inout) ::  qgrs(ix,levs,ntrac)

      real(kind=kind_phys), dimension(ix,levs+1),     intent(in) ::     &
     &      prsi, prsik
! SJL: bugs with original code; phii and phil are modified (or computed) inside
      real(kind=kind_phys), dimension(ix,levs+1), intent(inout):: phii
      real(kind=kind_phys), dimension(ix,levs  ), intent(inout):: phil

      real(kind=kind_phys), intent(in) ::  hprime(ix,nmtvr),            &
     &      prdout(ix,ko3,pl_coeff),       rann(ix,nrcm), poz(ko3)

      real(kind=kind_phys), intent(in) ::  dtp,     dtf, fhour, solhr,  &
     &      slag,    sdec,     cdec,       ctei_rm(2), clstp,           &
     &      ccwf(2), crtrh(3), flgmin(2),  dlqf(2), cdmbgwd(2),         &
     &      xkzm_m,  xkzm_h, xkzm_s, psautco(2),   prautco(2), evpco,   &
     &      wminco(2), cgwf(2), prslrd0, sup

!  ---  input/output:
      real(kind=kind_phys), dimension(im),            intent(inout) ::  &
     &      hice,   fice,    tisfc,  tsea,   tprcp,  cv,     cvb,  cvt, &
     &      srflag, snwdph,  weasd, sncovr, zorl,   canopy, ffmm, ffhh, &
     &      f10m,   srunoff, evbsa,  evcwa,  snohfa, transa, sbsnoa,    &
     &      snowca, soilm,   tmpmin, tmpmax, dusfc,  dvsfc,  dtsfc,     &
     &      dqsfc,  totprcp, gflux,  dlwsfc, ulwsfc, suntim, runoff, ep,&
     &      cldwrk, dugwd,   dvgwd,  psmean, cnvprcp,spfhmin, spfhmax,  &
     &      rain,   rainc,   acv,    acvb,   acvt
      real(kind=kind_phys), dimension(im), optional,  intent(inout) ::  &
! for A/O/I coupling
     &      dusfc_cpl, dvsfc_cpl, dtsfc_cpl, dqsfc_cpl,                 &
     &      dlwsfc_cpl,dswsfc_cpl,rain_cpl,                             &
     &      dnirbm_cpl,dnirdf_cpl,dvisbm_cpl,dvisdf_cpl,                &
     &      nlwsfc_cpl,nswsfc_cpl,                                      &
     &      nnirbm_cpl,nnirdf_cpl,nvisbm_cpl,nvisdf_cpl,                &
! for nst
     &      xt, xs, xu, xv, xz, zm, xtts, xzts, d_conv, ifd, dt_cool,
     &      Qrain

!
      real(kind=kind_phys), dimension(ix,lsoil),      intent(inout) ::  &
     &      smc, stc, slc

      real(kind=kind_phys), dimension(ix,levs),       intent(inout) ::  &
     &      upd_mf, dwn_mf, det_mf, dqdt_v, cnvqc_v
!    &      upd_mf, dwn_mf, det_mf, dkh, rnp

      real(kind=kind_phys),                           intent(inout) ::  &
     &      phy_f3d(ix,levs,ntot3d),  phy_f2d(ix,ntot2d),               &
     &      dt3dt(ix,levs,6), du3dt(ix,levs,4), dv3dt(ix,levs,4),       &
     &      dq3dt(ix,levs,5+pl_coeff)

      real(kind=kind_phys),                           intent(inout) ::  &
     &      phy_fctd(ix,nctp)                                           &
      real(kind=kind_phys), dimension(ntrac-ncld+2) ::  fscav, fswtr

!  ---  output:
      real(kind=kind_phys), dimension(im),            intent(out) ::    &
     &      t2m,     q2m,     u10m,    v10m,    zlvl,   psurf, hpbl,    &
     &      pwat,    t1,      q1,      u1,      v1,     chh,   cmm,     &
     &      dlwsfci, ulwsfci, dswsfci, uswsfci,                         &
     &      dusfci,  dvsfci,  dtsfci,  dqsfci,                          &
     &      gfluxi,  epi,     smcwlt2, smcref2, wet1, sr

      real(kind=kind_phys), dimension(im), optional,  intent(out) ::    &
     &      dusfci_cpl,dvsfci_cpl,dtsfci_cpl,dqsfci_cpl,                &
     &      dlwsfci_cpl,dswsfci_cpl,                                    &
     &      dnirbmi_cpl,dnirdfi_cpl,dvisbmi_cpl,dvisdfi_cpl,            &
     &      nlwsfci_cpl,nswsfci_cpl,                                    &
     &      nnirbmi_cpl,nnirdfi_cpl,nvisbmi_cpl,nvisdfi_cpl,            &
     &      t2mi_cpl,q2mi_cpl,                                          &
     &      u10mi_cpl,v10mi_cpl,tseai_cpl,psurfi_cpl,                   &
!    &      u10mi_cpl,v10mi_cpl,tseai_cpl,psurfi_cpl,oro_cpl,slmsk_cpl, &

     &      tref,    z_c,     c_0,     c_d,     w_0,   w_d
!  ---  changed intent to be consistent with scope of variable
      real(kind=kind_phys), dimension(im), optional, intent(inout) ::   &
     &      rqtk 

      real(kind=kind_phys), dimension(ix,levs),       intent(out) ::    &
     &      gt0, gu0, gv0

      real(kind=kind_phys), dimension(ix,levs,ntrac), intent(out) ::    &
     &      gq0

!  ---  local:
!     real(kind=kind_phys) ::  fscav(ntrac-ncld-1)
!     real(kind=kind_phys),allocatable  ::  fscav(:), fswtr(:)
      real(kind=kind_phys), dimension(im)          :: ccwfac, garea,    &
     &      dlength, xncw,   cumabs, qmax,   cice,    zice,   tice,     &
!    &      gflx,    rain,   rainc,  rainl,  rain1,   raincs, evapc,    &
     &      gflx,                    rainl,  rain1,   raincs,           &
     &      snowmt,  cd,     cdq,    qss,    dusfcg,  dvsfcg, dusfc1,   &
     &      dvsfc1,  dtsfc1, dqsfc1, rb,     rhscnpy, drain,  cld1d,    &
     &      evap,    hflx,   stress, t850,   ep1d,    gamt,   gamq,     &
     &      sigmaf,                  oc,     theta,   gamma,  sigma,    &
     &      elvmax,  wind,   work1,  work2,  runof,   xmu,              &
!    &      elvmax,  wind,   work1,  work2,  runof,   xmu,    oro_land, &
     &      fm10,    fh2,    tsurf,  tx1,    tx2,     ctei_r, flgmin_l, &
     &      evbs,    evcw,   trans,  sbsno,  snowc,                     &
     &      adjsfcdsw, adjsfcnsw, adjsfcdlw, adjsfculw,gabsbdlw,        &
     &      adjnirbmu, adjnirdfu, adjvisbmu, adjvisdfu,                 &
     &      adjnirbmd, adjnirdfd, adjvisbmd, adjvisdfd,                 &
     &      xcosz,  tseal,  snohf,  dlqfac,  work3, ctei_rml, cldf,     &
     &      domr,    domzr,  domip,  doms,   psautco_l, prautco_l
      real(kind=kind_phys), dimension(im)          :: ocalnirbm_cpl,    &
     &      ocalnirdf_cpl,ocalvisbm_cpl,ocalvisdf_cpl

!    &      dswsfc, radsl,                                              &
!    &      dlwsf1,  ulwsf1, xcosz,  tseal,  snohf,   dlqfac,           &
!    &      domr,    domzr,  domip,  doms

!     real(kind=kind_phys), dimension(ix,levs)     :: ud_mf, dd_mf,     &
!    &      dt_mf, del
      real(kind=kind_phys), dimension(ix,levs)     :: del, dtdtr
      real(kind=kind_phys), dimension(im,levs-1)   :: dkt

      real(kind=kind_phys), dimension(im,levs)     :: rhc, dtdt,        &
     &      dudt, dvdt, gwdcu, gwdcv, dtdtc, dmdt,                      &
!    &      diagn1, diagn2, cuhr, cumchr,                               &
     &      qr_col, fc_ice, rainp, ud_mf, dd_mf, dt_mf, prnum
!    &      qr_col, fc_ice, rainp, ud_mf, dd_mf, dt_mf, shoc_cld, prnum

      real(kind=kind_phys), dimension(im,lsoil)    :: smsoil, stsoil,   &
     &      ai, bi, cci, rhsmc, zsoil, slsoil

      real(kind=kind_phys) :: rhbbot, rhbtop, rhpbl, frain, f_rain,     &
     &      f_ice, qi, qw, qr, wc, tem, tem1, tem2,  sume,  sumr, sumq, &
     &      dqdt(im,levs,ntrac), oa4(im,4), clx(im,4), albbm, albdf,    &
     &      xcosz_loc


!           in clw, the first two varaibles are cloud water and ice. 
!           from third to ntrac are convective transportable tracers,
!           third being the ozone, when ntrac=3 (valid only with ras)

      real(kind=kind_phys), allocatable :: clw(:,:,:), qpl(:,:),qpi(:,:)

      integer, dimension(im) :: kbot, ktop, kcnv, soiltyp, vegtype,     &
     &          kpbl, slopetyp, kinver, lmh, levshc, islmsk

      integer :: i, nvdiff, kk, ic, k, n, ipr, lv, k1, iter, levshcm,   &
     &           tracers, trc_shft, tottracer, num2, num3               &
     &,          nshocm, nshoc, ntk

      logical, dimension(im) :: flag_iter, flag_guess, invrsn

      logical :: lprnt, calc_phil

      real(kind=kind_phys), allocatable :: cnvc(:,:),cnvw(:,:)
      real(kind=kind_phys) eng0, eng1, dtshoc
!
! for CS-convection
!     real(kind=kind_phys), parameter :: wcbmax1=2.8, wcbmax2=1.4
      real(kind=kind_phys), parameter :: wcbmax1=2.5, wcbmax2=1.5
!     real(kind=kind_phys), parameter :: wcbmax1=1.4, wcbmax2=1.4
      real(kind=kind_phys)  wcbmax(im)
!
      real(kind=kind_phys) tf, tcr, tcrf
!     parameter (tf=233.16, tcr=263.16, tcrf=1.0/(tcr-tf))
      parameter (tf=258.16, tcr=273.16, tcrf=1.0/(tcr-tf))
!
#ifndef GFS_HYDRO
! *** GFDL modification by SJL ***
      real:: del_gz(im,levs+1)
#endif
!
!===> ...  begin here
!
! The option to compute tracer scavenging in GSM is disabled
!     do i=1, ntrac-ncld-1
!       fscav(i) = 0.
!     enddo
 

!  --- ...  set up check print point (for debugging)
!
!*************************************************************************
!     lprnt = .true.
      lprnt = .false.
!     lprnt = me == 0 .and. kdt < 10
!     lprnt = kdt >= 19
      ipr = 1
!     lprnt = kdt >= 19

!     if (me == 0 .and. kdt < 5)
!    &write(0,*)' In gbphys:', im,ix,levs,lsoil,lsm,
!    &  ntrac,ncld,ntoz,ntcw,
!    &  nmtvr,nrcm,ko3,lonr,latr,jcap,num_p3d,num_p2d,npdf3d,
!    &      kdt,lat,me,pl_coeff,ncw,flgmin,crtrh,cdmbgwd
!    &,' ccwf=',ccwf,' dlqf=',dlqf,' ras=',ras,
!    & ' evpco=',evpco,' wminco=',wminco,' levr=',levr
!     ipr = 1
!     lprnt = kdt .gt. 0
!     do i = 1, im
!       work1(1) = xlon(i) * 57.29578
!       if (work1(1) >= 180.0) work1(1) = work1(1) - 360.0
!       work2(1) = xlat(i) * 57.29578
!       print *,' me=',me,' work1=',work1(1),' work2=',work2(1),' i=',i
!       lprnt = kdt > 4320
!       lprnt = kdt > 0 .and. abs(work1(1)-8.4375) < 0.5                &
!    &                  .and. abs(work2(1)+1.4)   < 0.5
!       lprnt = kdt >= 14 .and. lat == 43 
!       lprnt = kdt >= 256 .and. abs(xlon(i)*57.29578-136.07) < 0.08    &
!    &                   .and. abs(xlat(i)*57.29578+1.1)  < 0.101
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-21.5) < 0.501       &
!    &                   .and. abs(xlat(i)*57.29578+11.8)  < 0.501
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-8.4375) < 0.501     &
!    &                   .and. abs(xlat(i)*57.29578+1.4)  < 0.501
!       lprnt = kdt >= 40 .and. abs(xlon(i)*57.29578-288.75) < 1.501    &
!    &                   .and. abs(xlat(i)*57.29578+12.38)  < 1.501
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-135.0) < 0.201      &
!    &                   .and. abs(xlat(i)*57.29578-10.476)  < 0.201
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-110.3) < 0.201      &
!    &                   .and. abs(xlat(i)*57.29578-2.0)   < 0.201
!       if (kdt == 10)
!    &  print *,' i=',i,' xlon=',xlon(i)*57.29578,                      &
!    &                  ' xlat=',xlat(i)*57.29578,' i=',i,' me=',me
!       if (lprnt) then
!         ipr = i
!         exit
!       endif
!     enddo

!     write(0,*)' In GBPHYS LSIDEA=',lsidea
!     lprnt = .false.
!     if(lprnt) then
!       write(0,*)' im=',im,' ix=',ix,' levs=',levs,' lsoil=',lsoil,    &
!    &   ' ntrac=',ntrac,' ntoz=',ntoz,' ntcw=',ntcw,' me=',me,         &
!    &   ' xlat=',xlat(ipr),' kdt=',kdt,' slmsk=',slmsk(ipr),           &
!    & ' ntke=',ntke,' num_p3d=',num_p3d,' xlon=',xlon(ipr)
!    &   ' tsea=',tsea(ipr),' tref=',tref(ipr),' dt_cool=',dt_cool(ipr),&
!    &   ' dt_warm=',2.0*xt(ipr)/xz(ipr),' nrcm=',nrcm,' xlon=',
!    &    xlon(ipr),                                                    &
!    &   ' dt_warm=',dt_warm(ipr),' nrcm=',nrcm,' xlon=',xlon(ipr),     &
!    &   ' sfalb=',sfalb(ipr),' kdt=',kdt
!       write(0,*) ' pgr=',pgr(ipr),' kdt=',kdt,' ipr=',ipr
!       write(0,*)' ipr=',ipr,' phy_f2d=',phy_f2d(ipr,1:num_p2d)
!       write(0,*),' ugrs=',ugrs(ipr,:)
!       write(0,*)' vgrs=',vgrs(ipr,:)
!       write(0,*)' tgrs=',tgrs(ipr,:),' kdt=',kdt,' ipr=',ipr
!    &,  ' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!       write(0,*)' qgrs=',qgrs(ipr,:,1)
!       write(0,*)' ozg=',qgrs(ipr,:,2)
!       write(0,*)' tke=',qgrs(ipr,:,4)
!       print *,' clw=',qgrs(ipr,:,3)
!    &,  ' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!     endif
!
!*************************************************************************
!
      nvdiff = ntrac           ! vertical diffusion of all tracers!
!
!  --- ...                       figure out number of extra tracers
!
      tottracer = 0            ! no convective transport of tracers
      if (trans_trac) then
        if (ntcw > 0) then
          if (ntoz < ntcw) then
            trc_shft = ntcw + ncld - 1
          else
            trc_shft = ntoz
          endif
        elseif (ntoz > 0) then
          trc_shft = ntoz
        else
          trc_shft = 1
        endif

        tracers   = ntrac - trc_shft
        tottracer = tracers
        if (ntoz > 0) tottracer = tottracer + 1  ! ozone is added separately
      endif
      if (ntke > 0) ntk = ntke - trc_shft + 3

!     if (lprnt) write(0,*)' trans_trac=',trans_trac,' tottracer=',     &
!                write(0,*)' trans_trac=',trans_trac,' tottracer=',     &
!    &                   tottracer,' trc_shft=',trc_shft,' kdt=',kdt
!    &,                  ntrac-ncld+2,' clstp=',clstp,' kdt=',kdt
!    &,' ntk=',ntk,' lat=',lat

      allocate ( clw(ix,levs,tottracer+2) )
      if (do_shoc) then
        allocate (qpl(im,levs), qpi(im,levs))
      else
        allocate ( cnvc(ix,levs), cnvw(ix,levs))
      endif
!     allocate (fscav(tottracer+3), fswtr(tottracer+3))

! The option to compute tracer scavenging in GSM is disabled
      do i=1, tottracer+3
        fscav(i) = 0.
        fswtr(i) = 0.
      enddo

      if (nnp == 1) then
        do n=1,ntrac
          if (abs(1.0-adjtrc(n)) > 1.0e-7) then
            do k=1,levs
              do i=1,im
                qgrs(i,k,n) = qgrs(i,k,n) * adjtrc(n)
              enddo
            enddo
          endif
        enddo
      endif
!
#ifdef GFS_HYDRO
      calc_phil = .false.
      if (phil(1,levs) == 0.0) calc_phil = .true.
      call get_prs(im,ix,levs,ntrac,tgrs,qgrs,                          &
     &             thermodyn_id, sfcpress_id,                           &
     &             gen_coord_hybrid,                                    &
     &             prsi,prsik,prsl,prslk,phii,phil,del)
#else
! SJL: Adjust the geopotential height hydrostatically in a way consistent with FV3 discretization
      call get_prs_fv3(ix,levs,ntrac,phii,prsi,tgrs,qgrs,del,del_gz)
      calc_phil = .true.
#endif
!
!     if (lprnt) then
!       write(0,*)' prsi=',prsi(ipr,:)
!       write(0,*)' prsik=',prsik(ipr,:),' me=',me,' kdt=',kdt
!       write(0,*)' prslk=',prslk(ipr,:),' me=',me,' kdt=',kdt
!       write(0,*)' phil=',phil(ipr,:),' me=',me,' kdt=',kdt,' ipr=',ipr
!       write(0,*)' prsl=',prsl(ipr,:),' kdt=',kdt
!       print *,' del=',del(ipr,:)
!     endif
!
      rhbbot = crtrh(1)
      rhpbl  = crtrh(2)
      rhbtop = crtrh(3)
!
!  --- ...  frain=factor for centered difference scheme correction of rain amount.

      frain = dtf / dtp

      do i = 1, im
        sigmaf(i)   = max( vfrac(i),0.01 )
!       sigmaf(i)   = max( vfrac(i),0.3 )
        if (lsm == 0) sigmaf(i)   =  0.5 + vfrac(i) * 0.5

        islmsk(i)   = nint(slmsk(i))

        if (islmsk(i) == 2) then
          soiltyp(i)  = 9
          vegtype(i)  = 13
          slopetyp(i) = 9                      !! clu: qa(slopetyp)
        else
          soiltyp(i)  = int( stype(i)+0.5 )
          vegtype(i)  = int( vtype(i)+0.5 )
          slopetyp(i) = int( slope(i)+0.5 )    !! clu: slope -> slopetyp
        endif
!  --- ...  xw: transfer ice thickness & concentration from global to local variables
        zice(i) = hice(i)
        cice(i) = fice(i)
        tice(i) = tisfc(i)
!
!GFDL        work1(i)   = (log(coslat(i) / (nlons(i)*latr)) - dxmin) * dxinv
!GFDL   nlons will contain the global number of columns for the blending function
        if (coslat(i) .le. 0._kind_phys) then
          work1(i) = 1._kind_phys
        else
          work1(i)   = (log(coslat(i) / nlons(i)) - dxmin) * dxinv
        endif
        work1(i)   = max(0.0, min(1.0,work1(i)))
        work2(i)   = 1.0 - work1(i)
        psurf(i)   = pgr(i)
        work3(i)   = prsik(i,1) / prslk(i,1)
!GFDL        tem1       = con_rerth * (con_pi+con_pi)*coslat(i)/nlons(i)
!GFDL        tem2       = con_rerth * con_pi / latr
!GFDL        garea(i)   = tem1 * tem2
        tem1       = dx(i)
        tem2       = dy(i)
        garea(i)   = area(i)
        dlength(i) = sqrt( tem1*tem1+tem2*tem2 )
        cldf(i)    = cgwf(1)*work1(i) + cgwf(2)*work2(i)
        wcbmax(i)  = wcbmax1*work1(i) + wcbmax2*work2(i)
      enddo

!     if (lprnt) write(0,*)' in gbphys work1&2=',work1(ipr),work2(ipr)
!    &,' dxmin=',dxmin,' dxinv=',dxinv,' dx=',
!    &           log(coslat(ipr) / (nlons(ipr)*latr))
!    &,' coslat=',coslat(ipr),' nlons=',nlons(ipr),' latr=',latr

!  --- ...  transfer soil moisture and temperature from global to local variables

      do k = 1, lsoil
        do i = 1, im
          smsoil(i,k) = smc(i,k)
          stsoil(i,k) = stc(i,k)
          slsoil(i,k) = slc(i,k)          !! clu: slc -> slsoil
        enddo
      enddo

      do k = 1, levs
        do i = 1, im
          dudt(i,k)  = 0.
          dvdt(i,k)  = 0.
          dtdt(i,k)  = 0.
          dtdtc(i,k) = 0.
        enddo
      enddo

      do n = 1, ntrac
        do k = 1, levs
          do i = 1, im
            dqdt(i,k,n) = 0.
          enddo
        enddo
      enddo

!  --- ...  initialize dtdt with heating rate from dcyc2 

!     if (lprnt) then
!       do ipr=1,im
!         write(0,*)' before dcyc2: im=',im,' lsoil=',lsoil,' levs=',   &
!    &                                                             levs &
!    &,    ' sde=',sdec,' cdec=',cdec,' tsea=',tsea(ipr),' ipr=',ipr    &
!    &,    ' lat=',lat,' me=',me,' kdt=',kdt                            &
!    &,    ' sfcdlw=',sfcdlw(ipr),' sfcnsw=',sfcnsw(ipr)
!         print *,' hlw=',hlw(ipr,:),' me=',me,' lat=',lat,xlon(ipr)
!         print *,' swh=',swh(ipr,:),' me=',me,' lat=',lat,xlon(ipr)
!       enddo
!     endif

!  --- ...  adjust mean radiation fluxes and heating rates to fit for
!           faster model time steps.
!      sw:  using cos of zenith angle as scaling factor
!      lw:  using surface air skin temperature as scaling factor

      if (pre_rad) then

        call dcyc2t3_pre_rad                                            &
!  ---  inputs:
     &     ( solhr,slag,sdec,cdec,sinlat,coslat,                        &
     &       xlon,coszen,tsea,tgrs(1,1),tgrs(1,1),                      &
     &       sfcdsw,sfcnsw,sfcdlw,swh,hlw,                              &
     &       sfcnirbmu,sfcnirdfu,sfcvisbmu,sfcvisdfu,                   &
     &       sfcnirbmd,sfcnirdfd,sfcvisbmd,sfcvisdfd,                   &
     &       ix, im, levs,                                              &
!  ---  input/output:
     &       dtdt,                                                      &
!  ---  outputs:
     &       adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz,         &
     &       adjnirbmu,adjnirdfu,adjvisbmu,adjvisdfu,                   &
     &       adjnirbmd,adjnirdfd,adjvisbmd,adjvisdfd                    &
     &     )

      else

        call dcyc2t3                                                    &
!  ---  inputs:
     &     ( solhr,slag,sdec,cdec,sinlat,coslat,                        &
     &       xlon,coszen,tsea,tgrs(1,1),tsflw,sfcemis,                  &
     &       sfcdsw,sfcnsw,sfcdlw,swh,swhc,hlw,hlwc,                    &
     &       sfcnirbmu,sfcnirdfu,sfcvisbmu,sfcvisdfu,                   &
     &       sfcnirbmd,sfcnirdfd,sfcvisbmd,sfcvisdfd,                   &
     &       ix, im, levs,                                              &
!  ---  input/output:
     &       dtdt,dtdtc,                                                &
!  ---  outputs:
     &       adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz,         &
     &       adjnirbmu,adjnirdfu,adjvisbmu,adjvisdfu,                   &
     &       adjnirbmd,adjnirdfd,adjvisbmd,adjvisdfd                    &
     &     )

!
! save temp change due to radiation - need for sttp stochastic physics
!---------------------------------------------------------------------
        do k=1,levs
          do i=1,im
             dtdtr(i,k) = dtdtr(i,k) + dtdtc(i,k)*dtf
          enddo
        enddo

      endif
!
      if (lsidea) then                       !idea jw
        do k = 1, levs
          do i = 1, im
!           dtdt(i,k) = hlwd(i,k,2)
            dtdt(i,k) = 0.
          enddo
        enddo
      endif

!  ---  convert lw fluxes for land/ocean/sea-ice models
!  note: for sw: adjsfcdsw and adjsfcnsw are zenith angle adjusted downward/net fluxes.
!        for lw: adjsfcdlw is (sfc temp adjusted) downward fluxe with no emiss effect.
!                adjsfculw is (sfc temp adjusted) upward fluxe including emiss effect.
!        one needs to be aware that that the absorbed downward lw flux (used by land/ocean
!        models as downward flux) is not the same as adjsfcdlw but a value reduced by
!        the factor of emissivity.  however, the net effects are the same when seeing
!        it either above the surface interface or below.
!
!   - flux above the interface used by atmosphere model:
!        down: adjsfcdlw;    up: adjsfculw = sfcemis*sigma*T**4 + (1-sfcemis)*adjsfcdlw
!        net = up - down = sfcemis * (sigma*T**4 - adjsfcdlw)
!   - flux below the interface used by lnd/oc/ice models:
!        down: sfcemis*adjsfcdlw;  up: sfcemis*sigma*T**4
!        net = up - down = sfcemis * (sigma*T**4 - adjsfcdlw)

!  --- ...  define the downward lw flux absorbed by ground

        do i = 1, im
          gabsbdlw(i) = sfcemis(i) * adjsfcdlw(i)
        enddo

!      if( lsidea ) then     ! idea : moved temp adjust to idea_phys
!        print *,' in gbphys: lsidea is true '
!        DTDT = 0.
!      endif

      if (lssav) then      !  --- ...  accumulate/save output variables

!  --- ...  sunshine duration time is defined as the length of time (in mdl output
!           interval) that solar radiation falling on a plane perpendicular to the
!           direction of the sun >= 120 w/m2

        do i = 1, im
          if ( xcosz(i) >= czmin ) then   ! zenth angle > 89.994 deg
            tem1 = adjsfcdsw(i) / xcosz(i)

            if ( tem1 >= 120.0 ) then
              suntim(i) = suntim(i) + dtf
            endif
          endif
        enddo

!  --- ...  sfc lw fluxes used by atmospheric model are saved for output

        do i = 1, im
          dlwsfc(i) = dlwsfc(i) + adjsfcdlw(i)*dtf
          ulwsfc(i) = ulwsfc(i) + adjsfculw(i)*dtf
          psmean(i) = psmean(i) + pgr(i)*dtf        ! mean surface pressure
        enddo

        if (ldiag3d) then
          if( lsidea ) then
            do k = 1, levs
              do i = 1, im
                dt3dt(i,k,1) = dt3dt(i,k,1) + hlwd(i,k,1)*dtf
                dt3dt(i,k,2) = dt3dt(i,k,2) + hlwd(i,k,2)*dtf
                dt3dt(i,k,3) = dt3dt(i,k,3) + hlwd(i,k,3)*dtf
                dt3dt(i,k,4) = dt3dt(i,k,4) + hlwd(i,k,4)*dtf
                dt3dt(i,k,5) = dt3dt(i,k,5) + hlwd(i,k,5)*dtf
                dt3dt(i,k,6) = dt3dt(i,k,6) + hlwd(i,k,6)*dtf
              enddo
            enddo
          else
            do k = 1, levs
              do i = 1, im
                dt3dt(i,k,1) = dt3dt(i,k,1) + hlw(i,k)*dtf
                dt3dt(i,k,2) = dt3dt(i,k,2) + swh(i,k)*dtf*xmu(i)
              enddo
            enddo
          endif
        endif

      endif    ! end if_lssav_block

      do i = 1, im
        kcnv(i)   = 0
        kinver(i) = levs
        invrsn(i) = .false.
        tx1(i)    = 0.0
        tx2(i)    = 10.0
        ctei_r(i) = 10.0
      enddo

!    Only used for old shallow convection with mstrat=.true.

      if (((.not. sashal .and. shal_cnv) .or. old_monin)                &
     &                                   .and. mstrat) then
        do i = 1, im
          ctei_rml(i) = ctei_rm(1)*work1(i) + ctei_rm(2)*work2(i)
        enddo
        do k = 1, levs/2
          do i = 1, im
            if (prsi(i,1)-prsi(i,k+1) < 0.35*prsi(i,1)                  &
     &          .and. (.not. invrsn(i))) then
              tem = (tgrs(i,k+1)-tgrs(i,k)) / (prsl(i,k)-prsl(i,k+1))

              if ((tem > 0.00010 .and. tx1(i) < 0.0) .or. 
     &            (tem-abs(tx1(i)) > 0.0 .and. tx2(i) < 0.0)) then
                invrsn(i) = .true.

                if (qgrs(i,k,1) > qgrs(i,k+1,1)) then
                  tem1 = tgrs(i,k+1) + hocp*max(qgrs(i,k+1,1),qmin)
                  tem2 = tgrs(i,k)   + hocp*max(qgrs(i,k,1),qmin)

                  tem1 = tem1 / prslk(i,k+1) - tem2 / prslk(i,k)

!  --- ...  (cp/l)(deltathetae)/(deltatwater) > ctei_rm -> conditon for CTEI
                  ctei_r(i) = (1.0/hocp)*tem1/(qgrs(i,k+1,1)-qgrs(i,k,1)&
     &                      + qgrs(i,k+1,ntcw)-qgrs(i,k,ntcw))
                else
                  ctei_r(i) = 10
                endif

                if ( ctei_rml(i) > ctei_r(i) ) then
                  kinver(i) = k
                else
                  kinver(i) = levs
                endif
              endif

              tx2(i) = tx1(i)
              tx1(i) = tem
            endif
          enddo
        enddo
      endif

!  --- ...  check print

!     ipr = 1
!     if (lprnt) then
!       write(0,*)' before progtm: im=',im,' lsoil=',lsoil              &
!    &,         ' nvdiff=',nvdiff,' adjsfcnsw=',adjsfcnsw(ipr)          &
!    &,         ' adjsfcdlw=',adjsfcdlw(ipr),'adjsfculw=',adjsfculw(ipr)&
!    &,         ' sfcemis=',sfcemis(ipr),' tsea2=',tsea(ipr)            &
!    &,         ' ipr=',ipr,' me=',me,' lat=',lat,' xlon=',xlon(ipr)    &
!    &,         ' kdt=',kdt
!       write(0,*)' dtdth=',dtdt(ipr,:),' kdt=',kdt
!     endif

!     if (lprnt) write(0,*)' phil=',phil(1,1:5)

!  --- ...  lu: initialize flag_guess, flag_iter, tsurf

      do i = 1, im
        tsurf(i)      = tsea(i)
        flag_guess(i) = .false.
        flag_iter(i)  = .true.
        drain(i)      = 0.0
        ep1d(i)       = 0.0
        runof(i)      = 0.0
        hflx(i)       = 0.0
        evap(i)       = 0.0

        evbs(i)       = 0.0
        evcw(i)       = 0.0
        trans(i)      = 0.0
        sbsno(i)      = 0.0
        snowc(i)      = 0.0
        snohf(i)      = 0.0
        zlvl(i)       = phil(i,1) / con_g
        smcwlt2(i)    = 0.0
        smcref2(i)    = 0.0
      enddo

!  --- ...  lu: iter-loop over (sfc_diff,sfc_drv,sfc_ocean,sfc_sice)

      do iter = 1, 2

!  --- ...  surface exchange coefficients
!
!     if (lprnt) write(0,*)' tsea=',tsea(ipr),' tsurf=',tsurf(ipr),iter
        call sfc_diff(im,pgr,ugrs,vgrs,tgrs,qgrs,zlvl,                  &
     &                tsea,zorl,cd,cdq,rb,                              &
     &                prsl(1,1),work3,islmsk,                           &
     &                stress,ffmm,ffhh,                                 &
     +                uustar,wind,phy_f2d(1,num_p2d),fm10,fh2,          &
     &                sigmaf,vegtype,shdmax,                            &
     &                tsurf, flag_iter, redrag)

!       if (lprnt) write(0,*)' cdq=',cdq(ipr),' iter=',iter             &
!    &,   ' wind=',wind(ipr),'phy_f2d=',phy_f2d(ipr,num_p2d),' ugrs='   &
!    &,   ugrs(ipr,1),' vgrs=',vgrs(ipr,1)

!  --- ...  lu: update flag_guess

        do i = 1, im
          if (iter == 1 .and. wind(i) < 2.0) then
            flag_guess(i) = .true.
          endif
        enddo

!  --- ...  surface energy balance over ocean

        if ( nst_fcst > 0 ) then

          do i = 1, im
            if ( islmsk(i) == 0 ) then
              tem      = (oro(i)-oro_uf(i)) * rlapse
              tseal(i) = tsea(i)  + tem
              tsurf(i) = tsurf(i) + tem
            endif
          enddo
!
!         if ( nst_fcst > 1 ) then
!           do i = 1, im
!             if ( islmsk(i) == 0 ) then
!               tref(i)  = tseal(i) - (xt(i)+xt(i))/xz(i) + dt_cool(i)
!             endif
!           enddo
!         else
!           do i = 1, im
!             if ( islmsk(i) == 0 ) then
!               tref(i)  = tseal(i)
!             endif
!           enddo
!         endif

!         if (lprnt) write(0,*)' tseaz1=',tsea(ipr),' tref=',tref(ipr), &
!    &      ' dt_cool=',dt_cool(ipr),' dt_warm=',2.0*(xt(ipr)/xz(ipr)   &
!    &      ' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr)           &
!    &,     ' tgrs=',tgrs(ipr,1),' prsl=',prsl(ipr,1)
!    &,     ' work3=',work3(ipr),' kdt=',kdt

          call sfc_nst                                                  &
     &       ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,tref,cd,cdq,            &
     &         prsl(1,1),work3,islmsk,xlon,sinlat,stress,               &
     &         sfcemis,gabsbdlw,adjsfcnsw,tprcp,dtf,kdt,                &
     &         phy_f2d(1,num_p2d),flag_iter,flag_guess,nst_fcst,        &
     &         lprnt,ipr,                                               &
!  --- Input/output
     &         tseal,tsurf,xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,         &
     &         z_c,c_0,c_d,w_0,w_d,d_conv,ifd,qrain,                    &
!  ---  outputs:
     &         qss, gflx, cmm, chh, evap, hflx, ep1d)

!         if (lprnt) print *,' tseaz2=',tseal(ipr),' tref=',tref(ipr),  &
!    &     ' dt_cool=',dt_cool(ipr),' dt_warm=',2.0*xt(ipr)/xz(ipr),    &
!    &     ' kdt=',kdt
!    &     ' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr),' kdt=',kdt

          do i = 1, im
            if ( islmsk(i) == 0 ) then
              tsurf(i) = tsurf(i) - (oro(i)-oro_uf(i)) * rlapse
            endif
          enddo
          if ( nst_fcst > 1 ) then
            do i = 1, im
              if ( islmsk(i) == 0 ) then
                tsea(i) = max(271.0, tref(i) + (xt(i)+xt(i))/xz(i)
     &                                       - dt_cool(i))              &
     &                                       - (oro(i)-oro_uf(i))*rlapse
              endif
            enddo
          endif

!         if (lprnt) print *,' tseaz2=',tsea(ipr),' tref=',tref(ipr),   &
!    &    ' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr),' kdt=',kdt

        else

          call sfc_ocean                                                &
!  ---  inputs:
     &     ( im,pgr,ugrs,vgrs,tgrs,qgrs,tsea,cd,cdq,                    &
     &       prsl(1,1),work3,islmsk,phy_f2d(1,num_p2d),flag_iter,       &
!  ---  outputs:
     &       qss,cmm,chh,gflx,evap,hflx,ep1d                            &
     &     )

        endif
 
!       if (lprnt) write(0,*)' sfalb=',sfalb(ipr),' ipr=',ipr           &
!    &,   ' weasd=',weasd(ipr),' snwdph=',snwdph(ipr)                   &
!    &,   ' tprcp=',tprcp(ipr),' kdt=',kdt,' iter=',iter
!    &,' tseabefland=',tsea(ipr)
 
!  --- ...  surface energy balance over land
!
        if (lsm == 1) then                          ! noah lsm call

!     if (lprnt) write(0,*)' tsead=',tsea(ipr),' tsurf=',tsurf(ipr),iter
          call sfc_drv                                                  &
!  ---  inputs:
     &     ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,soiltyp,vegtype,sigmaf,   &
     &       sfcemis,gabsbdlw,adjsfcdsw,adjsfcnsw,dtf,tg3,cd,cdq,       &
     &       prsl(1,1),work3,zlvl,islmsk,phy_f2d(1,num_p2d),slopetyp,   &
     &       shdmin,shdmax,snoalb,sfalb,flag_iter,flag_guess,           &
!  ---  in/outs:
     &       weasd,snwdph,tsea,tprcp,srflag,smsoil,stsoil,slsoil,       &
     &       canopy,trans,tsurf,                                        &
!  ---  outputs:
     &       sncovr,qss,gflx,drain,evap,hflx,ep1d,runof,                &
     &       cmm,chh,evbs,evcw,sbsno,snowc,soilm,snohf,                 &
     &       smcwlt2,smcref2,zorl,wet1                                  &
     &     )

        else                                       ! osu lsm call

          call sfc_land                                                 &
!  ---  inputs:
     &     ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,smsoil,soiltyp,           &
     &       sigmaf,vegtype,sfcemis,adjsfcdlw,adjsfcnsw,dtf,            &
     &            tg3,cd,cdq,prsl(1,1),work3,islmsk,                    &
!    &       zorl,tg3,cd,cdq,prsl(1,1),work3,islmsk,                    &
     &       phy_f2d(1,num_p2d),flag_iter,flag_guess,                   &
!  ---  input/outputs:
     &       weasd,tsea,tprcp,srflag,stsoil,canopy,tsurf,               &
!  ---  outputs:
     &       qss,snowmt,gflx,zsoil,rhscnpy,rhsmc,                       &
     &       ai,bi,cci,drain,evap,hflx,ep1d,cmm,chh,                    &
     &       evbs,evcw,trans,sbsno,snowc,soilm,                         &
     &       snohf,smcwlt2,smcref2                                      &
     &     )

        endif

!       if (lprnt) write(0,*)' tseabeficemodel =',tsea(ipr),' me=',me   &
!    &,   ' kdt=',kdt

!  --- ...  surface energy balance over seaice

        call sfc_sice                                                   &
!  ---  inputs:
     &     ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,dtf,                      &
     &       sfcemis,gabsbdlw,adjsfcnsw,adjsfcdsw,srflag,               &
     &       cd,cdq,prsl(1,1),work3,islmsk,phy_f2d(1,num_p2d),          &
     &       flag_iter,mom4ice,lsm, lprnt,ipr,                          &
!    &       flag_iter,mom4ice,lsm,                                     &
!  ---  input/outputs:
     &       zice,cice,tice,weasd,tsea,tprcp,stsoil,ep1d,               &
!  ---  outputs:
     &       snwdph,qss,snowmt,gflx,cmm,chh,evap,hflx                   &
     &     )

!  --- ...  lu: update flag_iter and flag_guess

        do i = 1, im
          flag_iter(i)  = .false.
          flag_guess(i) = .false.

          if(islmsk(i) == 1 .and. iter == 1) then
            if (wind(i) < 2.0) flag_iter(i) = .true.
          elseif (islmsk(i) == 0 .and. iter == 1                        &
     &                           .and. nst_fcst > 0) then
            if (wind(i) < 2.0) flag_iter(i) = .true.
          endif
        enddo

      enddo   ! end iter_loop

      do i = 1, im
        epi(i)     = ep1d(i)
        dlwsfci(i) = adjsfcdlw(i)
        ulwsfci(i) = adjsfculw(i)
        uswsfci(i) = adjsfcdsw(i) - adjsfcnsw(i)
        dswsfci(i) = adjsfcdsw(i)
        gfluxi(i)  = gflx(i)
        t1(i)      = tgrs(i,1)
        q1(i)      = qgrs(i,1,1)
        u1(i)      = ugrs(i,1)
        v1(i)      = vgrs(i,1)
      enddo

      if (lsm == 0) then                          ! osu lsm call
        do i = 1, im
         sncovr(i) = 0.0
         if (weasd(i) > 0.0) sncovr(i) = 1.0
        enddo
      endif

!  --- ...  update near surface fields

      call sfc_diag(im,pgr,ugrs,vgrs,tgrs,qgrs,                         &
     &              tsea,qss,f10m,u10m,v10m,t2m,q2m,work3,              &
     &              evap,ffmm,ffhh,fm10,fh2)

      do i = 1, im
        phy_f2d(i,num_p2d) = 0.0
      enddo

      if (lssav_cpl) then
        do i = 1, im
          dlwsfci_cpl(i)   = adjsfcdlw(i)
          dswsfci_cpl(i)   = adjsfcdsw(i)
          dlwsfc_cpl(i)    = dlwsfc_cpl(i) + adjsfcdlw(i)
          dswsfc_cpl(i)    = dswsfc_cpl(i) + adjsfcdsw(i)
          dnirbmi_cpl(i)   = adjnirbmd(i)
          dnirdfi_cpl(i)   = adjnirdfd(i)
          dvisbmi_cpl(i)   = adjvisbmd(i)
          dvisdfi_cpl(i)   = adjvisdfd(i)
          dnirbm_cpl(i)    = dnirbm_cpl(i) + adjnirbmd(i)
          dnirdf_cpl(i)    = dnirdf_cpl(i) + adjnirdfd(i)
          dvisbm_cpl(i)    = dvisbm_cpl(i) + adjvisbmd(i)
          dvisdf_cpl(i)    = dvisdf_cpl(i) + adjvisdfd(i)
          nlwsfci_cpl(i)   = adjsfcdlw(i)  - adjsfculw(i)
          nlwsfc_cpl(i)    = nlwsfc_cpl(i) + nlwsfci_cpl(i)
          t2mi_cpl(i)      = t2m(i)
          q2mi_cpl(i)      = q2m(i)
          u10mi_cpl(i)     = u10m(i)
          v10mi_cpl(i)     = v10m(i)
          tseai_cpl(i)     = tsea(i)
          psurfi_cpl(i)    = pgr(i)
!         oro_cpl(i)       = oro(i)
!         slmsk_cpl(i)     = slmsk(i)
        enddo

!  ---  estimate mean albedo for ocean point without ice cover and apply
!       them to net SW heat fluxes

        albdf = 0.06
        do i = 1, im
          if (islmsk(i) /= 1) then  ! not a land point
!  ---  compute open water albedo
            xcosz_loc = max( 0.0, min( 1.0, xcosz(i) ))
            ocalnirdf_cpl(i) = 0.06
            ocalnirbm_cpl(i) = max(albdf, 0.026/(xcosz_loc**1.7+0.065)  &
     &                       + 0.15 * (xcosz_loc-0.1) * (xcosz_loc-0.5) &
     &                       * (xcosz_loc-1.0))
            ocalvisdf_cpl(i) = 0.06
            ocalvisbm_cpl(i) = ocalnirbm_cpl(i)

            nnirbmi_cpl(i) = adjnirbmd(i)-adjnirbmd(i)*ocalnirbm_cpl(i)
            nnirdfi_cpl(i) = adjnirdfd(i)-adjnirdfd(i)*ocalnirdf_cpl(i)
            nvisbmi_cpl(i) = adjvisbmd(i)-adjvisbmd(i)*ocalvisbm_cpl(i)
            nvisdfi_cpl(i) = adjvisdfd(i)-adjvisdfd(i)*ocalvisdf_cpl(i)
          else
            nnirbmi_cpl(i) = adjnirbmd(i) - adjnirbmu(i)
            nnirdfi_cpl(i) = adjnirdfd(i) - adjnirdfu(i)
            nvisbmi_cpl(i) = adjvisbmd(i) - adjvisbmu(i)
            nvisdfi_cpl(i) = adjvisdfd(i) - adjvisdfu(i)
          endif
          nswsfci_cpl(i) = nnirbmi_cpl(i) + nnirdfi_cpl(i)              &
     &                   + nvisbmi_cpl(i) + nvisdfi_cpl(i)
          nswsfc_cpl(i)  = nswsfc_cpl(i)  + nswsfci_cpl(i)
          nnirbm_cpl(i)  = nnirbm_cpl(i)  + nnirbmi_cpl(i)
          nnirdf_cpl(i)  = nnirdf_cpl(i)  + nnirdfi_cpl(i)
          nvisbm_cpl(i)  = nvisbm_cpl(i)  + nvisbmi_cpl(i)
          nvisdf_cpl(i)  = nvisdf_cpl(i)  + nvisdfi_cpl(i)
        enddo
      endif

      if (lssav) then
        do i = 1, im
          gflux(i)   = gflux(i)  + gflx(i)  * dtf
          evbsa(i)   = evbsa(i)  + evbs(i)  * dtf
          evcwa(i)   = evcwa(i)  + evcw(i)  * dtf
          transa(i)  = transa(i) + trans(i) * dtf
          sbsnoa(i)  = sbsnoa(i) + sbsno(i) * dtf
          snowca(i)  = snowca(i) + snowc(i) * dtf
          snohfa(i)  = snohfa(i) + snohf(i) * dtf
          ep(i)      = ep(i)     + ep1d(i)  * dtf

          tmpmax(i)  = max(tmpmax(i),t2m(i))
          tmpmin(i)  = min(tmpmin(i),t2m(i))

          spfhmax(i) = max(spfhmax(i),q2m(i))
          spfhmin(i) = min(spfhmin(i),q2m(i))
        enddo
      endif

!!!!!!!!!!!!!!!!!Commented by Moorthi on July 18, 2012 !!!!!!!!!!!!!!!!!!!
!     do i = 1, im
!  --- ...  compute coefficient of evaporation in evapc
!
!       if (evapc(i) > 1.0e0) evapc(i) = 1.0e0
!  --- ...  over snow cover or ice or sea, coef of evap =1.0e0
!       if (weasd(i) > 0.0 .or. slmsk(i) /= 1.0) evapc(i) = 1.0e0
!     enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  --- ...  Boundary Layer and Free atmospheic turbulence parameterization

!     if (lprnt) write(0,*)' tsea3=',tsea(ipr),' slmsk=',slmsk(ipr)     &
!    &, ' kdt=',kdt,' evap=',evap(ipr)
!     if (lprnt)  write(0,*)' dtdtb=',dtdt(ipr,1:10)

!     do i = 1, im
!       if (islmsk(i) == 0) then
!         oro_land(i) = 0.0
!       else
!         oro_land(i) = oro(i)
!       endif
!     enddo

!     write(0,*)' before monin clstp=',clstp,' kdt=',kdt,' lat=',lat

      if (do_shoc) then
        call moninshoc(ix,im,levs,ntrac,ntcw,dvdt,dudt,dtdt,dqdt,       &
     &                 ugrs,vgrs,tgrs,qgrs,phy_f3d(1,1,ntot3d-1),       &  ! tkh
     &                 prnum,ntke,                                      &
     &                 prsik(1,1),rb,zorl,u10m,v10m,ffmm,ffhh,          &
     &                 tsea,hflx,evap,stress,wind,kpbl,                 &
     &                 prsi,del,prsl,prslk,phii,phil,dtp,               &
     &                 dusfc1,dvsfc1,dtsfc1,dqsfc1,dkt,hpbl,            &
     &                 kinver, xkzm_m, xkzm_h, xkzm_s, lprnt, ipr,me)
      else

        if (hybedmf) then

          call moninedmf(ix,im,levs,nvdiff,ntcw,dvdt,dudt,dtdt,dqdt,    &
     &       ugrs,vgrs,tgrs,qgrs,swh,hlw,xmu,                           &
     &       prsik(1,1),rb,zorl,u10m,v10m,ffmm,ffhh,                    &
     &       tsea,qss,hflx,evap,stress,wind,kpbl,                       &
     &       prsi,del,prsl,prslk,phii,phil,dtp,dspheat,                 &
     &       dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,            &
     &       kinver, xkzm_m, xkzm_h, xkzm_s, lprnt, ipr)

        elseif (.not. old_monin) then

          call moninq(ix,im,levs,nvdiff,ntcw,dvdt,dudt,dtdt,dqdt,       &
     &       ugrs,vgrs,tgrs,qgrs,swh,hlw,xmu,                           &
     &       prsik(1,1),rb,ffmm,ffhh,                                   &
     &       tsea,qss,hflx,evap,stress,wind,kpbl,                       &
     &       prsi,del,prsl,prslk,phii,phil,dtp,dspheat,                 &
     &       dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,            &
     &       kinver, xkzm_m, xkzm_h, xkzm_s, lprnt, ipr)

        else

          if (mstrat) then
            call moninp1(ix,im,levs,nvdiff,dvdt,dudt,dtdt,dqdt,         &
     &       ugrs,vgrs,tgrs,qgrs,                                       &
     &       prsik(1,1),rb,ffmm,ffhh,tsea,qss,hflx,evap,stress,wind,    &
     &       kpbl,prsi,del,prsl,prslk,phii,phil,dtp,                    &
     &       dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,            &
     &       kinver, xkzm_m, xkzm_h)
!    &       kinver, oro_land, ctei_r, ctei_rm, xkzm_m, xkzm_h)
          else
            call moninp(ix,im,levs,nvdiff,dvdt,dudt,dtdt,dqdt,          &
     &       ugrs,vgrs,tgrs,qgrs,                                       &
     &       prsik(1,1),rb,ffmm,ffhh,tsea,qss,hflx,evap,stress,wind,    &
     &       kpbl,prsi,del,prsl,phii,phil,dtp,                          &
     &       dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,            &
     &       xkzm_m,xkzm_h)
          endif

        endif   ! end if_hybedmf
      endif   ! end if_do_shoc


!     if (lprnt) then
!       write(0,*) ' dusfc1=',dusfc1(ipr),' kdt=',kdt,' lat=',lat
!       write(0,*)' dtsfc1=',dtsfc1(ipr)
!       write(0,*)' dqsfc1=',dqsfc1(ipr)
!       write(0,*)' dtdt=',dtdt(ipr,1:10)
!       print *,' dudtm=',dudt(ipr,:)
!     endif

!  --- ...  coupling insertion

      if (lssav_cpl) then
        do i=1, im
          dusfc_cpl(i)  = dusfc_cpl(i) + dusfc1(i)
          dvsfc_cpl(i)  = dvsfc_cpl(i) + dvsfc1(i)
          dtsfc_cpl(i)  = dtsfc_cpl(i) + dtsfc1(i)
          dqsfc_cpl(i)  = dqsfc_cpl(i) + dqsfc1(i)
          dusfci_cpl(i) = dusfc1(i)
          dvsfci_cpl(i) = dvsfc1(i)
          dtsfci_cpl(i) = dtsfc1(i)
          dqsfci_cpl(i) = dqsfc1(i)
        enddo
      endif
!-------------------------------------------------------lssav if loop ----------
      if (lssav) then
        do i = 1, im
          dusfc(i)  = dusfc(i) + dusfc1(i)*dtf
          dvsfc(i)  = dvsfc(i) + dvsfc1(i)*dtf
          dtsfc(i)  = dtsfc(i) + dtsfc1(i)*dtf
          dqsfc(i)  = dqsfc(i) + dqsfc1(i)*dtf
          dusfci(i) = dusfc1(i)
          dvsfci(i) = dvsfc1(i)
          dtsfci(i) = dtsfc1(i)
          dqsfci(i) = dqsfc1(i)
        enddo
!       if (lprnt) then
!         write(0,*)' dusfc=',dusfc(ipr),' dusfc1=',dusfc1(ipr),' dtf=',
!    &     dtf,' kdt=',kdt,' lat=',lat
!       endif

        if (ldiag3d) then

          if (lsidea) then
            do k = 1, levs
              do i = 1, im
                dt3dt(i,k,3) = dt3dt(i,k,3) + dtdt(i,k)*dtf
              enddo
            enddo
          else
            do k = 1, levs
              do i = 1, im
                tem          = dtdt(i,k) - (hlw(i,k)+swh(i,k)*xmu(i))
                dt3dt(i,k,3) = dt3dt(i,k,3) + tem*dtf
              enddo
            enddo
          endif
          do k = 1, levs
            do i = 1, im
              du3dt(i,k,1) = du3dt(i,k,1) + dudt(i,k) * dtf
              du3dt(i,k,2) = du3dt(i,k,2) - dudt(i,k) * dtf
              dv3dt(i,k,1) = dv3dt(i,k,1) + dvdt(i,k) * dtf
              dv3dt(i,k,2) = dv3dt(i,k,2) - dvdt(i,k) * dtf
            enddo
          enddo
! update dqdt_v to include moisture tendency due to vertical diffusion
!         if (lgocart) then
!           do k = 1, levs
!             do i = 1, im
!               dqdt_v(i,k)  = dqdt(i,k,1) * dtf
!             enddo
!           enddo
!         endif
          do k = 1, levs
            do i = 1, im
              tem  = dqdt(i,k,1) * dtf
              dq3dt(i,k,1) = dq3dt(i,k,1) + tem
!             dqdt_v(i,k)  = tem
            enddo
          enddo
          if (ntoz > 0) then
            do k = 1, levs
              do i = 1, im
                dq3dt(i,k,5) = dq3dt(i,k,5) + dqdt(i,k,ntoz) * dtf
              enddo
            enddo
          endif
        endif

      endif   ! end if_lssav
!-------------------------------------------------------lssav if loop ----------
!
!            Orographic gravity wave drag parameterization
!            ---------------------------------------------

      if (nmtvr == 14) then         ! current operational - as of 2014

        do i = 1, im
          oc(i) = hprime(i,2)
        enddo
        do k = 1, 4
          do i = 1, im
            oa4(i,k) = hprime(i,k+2)
            clx(i,k) = hprime(i,k+6)
          enddo
        enddo
        do i = 1, im
          theta(i)  = hprime(i,11)
          gamma(i)  = hprime(i,12)
          sigma(i)  = hprime(i,13)
          elvmax(i) = hprime(i,14)
        enddo

      elseif (nmtvr == 10) then

        do i = 1, im
          oc(i) = hprime(i,2)
        enddo
        do k = 1, 4
          do i = 1, im
            oa4(i,k) = hprime(i,k+2)
            clx(i,k) = hprime(i,k+6)
          enddo
        enddo

      elseif (nmtvr == 6) then

        do i = 1, im
          oc(i) = hprime(i,2)
        enddo
        do k = 1, 4
          do i = 1, im
            oa4(i,k) = hprime(i,k+2)
            clx(i,k) = 0.0
          enddo
        enddo

      else

        oc = 0 ; oa4 = 0 ; clx = 0 ; theta = 0 ; gamma = 0 ; sigma = 0
        elvmax = 0

      endif   ! end if_nmtvr

!     write(0,*)' before gwd clstp=',clstp,' kdt=',kdt,' lat=',lat
      call gwdps(im, ix, im,   levs,  dvdt, dudt, dtdt,                 &
     &           ugrs,   vgrs, tgrs,  qgrs,                             &
     &           kpbl,   prsi, del,   prsl, prslk,                      &
     &           phii,   phil, dtp,                                     &
     &           kdt,    hprime(1,1), oc, oa4, clx,                     &
     &           theta,sigma,gamma,elvmax,dusfcg, dvsfcg,               &
     &           con_g,con_cp,con_rd,con_rv, lonr, nmtvr, cdmbgwd,      &
     &           me, lprnt,ipr)

!     if (lprnt)  print *,' dudtg=',dudt(ipr,:)

      if (lssav) then
        do i = 1, im
          dugwd(i) = dugwd(i) + dusfcg(i)*dtf
          dvgwd(i) = dvgwd(i) + dvsfcg(i)*dtf
        enddo

!       if (lprnt) print *,' dugwd=',dugwd(ipr),' dusfcg=',dusfcg(ipr)
!       if (lprnt) print *,' dvgwd=',dvgwd(ipr),' dvsfcg=',dvsfcg(ipr)

        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              du3dt(i,k,2) = du3dt(i,k,2) + dudt(i,k) * dtf
              dv3dt(i,k,2) = dv3dt(i,k,2) + dvdt(i,k) * dtf
              dt3dt(i,k,2) = dt3dt(i,k,2) + dtdt(i,k) * dtf
            enddo
          enddo
        endif
      endif

      if( .not. lsidea ) then
!        call rayleigh_damp_mesopause(im, ix, im, levs, dvdt, dudt, dtdt,
!     &                   ugrs, vgrs, dtp, con_cp, levr, prsl, prslrd0)
!      else
        call rayleigh_damp(im, ix, im, levs, dvdt, dudt, dtdt, ugrs,
     &                     vgrs, dtp, con_cp, levr, prsl, prslrd0)
      endif

      do  k = 1, levs
        do i = 1, im
          gt0(i,k) = tgrs(i,k) + dtdt(i,k) * dtp
          gu0(i,k) = ugrs(i,k) + dudt(i,k) * dtp
          gv0(i,k) = vgrs(i,k) + dvdt(i,k) * dtp
        enddo
      enddo

      do n = 1, ntrac
        do k = 1, levs
          do i = 1, im
            gq0(i,k,n) = qgrs(i,k,n) + dqdt(i,k,n) * dtp
          enddo
        enddo
      enddo

      if( lsidea ) then            ! idea convective adjustment
        call ideaca_up(prsi,gt0,ix,im,levs+1)
      endif

!  --- ...  check print

!     if (me == 0) then
!       sumq = 0.0
!       do k = 1, levs
!         do i = 1, im
!           sumq = sumq + (dqdt(i,k,1)+dqdt(i,k,ntcw)) * del(i,k)
!         enddo
!       enddo

!       sume = 0.0
!       do i = 1, im
!         sume = sume + dqsfc1(i)
!       enddo

!       sumq = sumq * 1000.0 / con_g
!       sume = sume / con_hvap
!       print *,' after mon: sumq=',sumq,' sume=',sume, ' kdt=',kdt
!     endif

!  --- ...  ozone physics

      if (ntoz > 0 .and. ntrac >= ntoz .and. ozcalc) then
        if (me == 0 .and. kdt < 5) then
          print *, "OZONE turned ON in gbphys - ozphys being called"
        end if
        call ozphys(ix,im,levs,ko3,dtp,gq0(1,1,ntoz),gq0(1,1,ntoz)      &
     &,             gt0, poz, prsl, prdout, pl_coeff, del, ldiag3d      &
     &,             dq3dt(1,1,6), me)
      else
! PT - turn off ozone in physics for NGGPS testing 8/25/2015
        if (me == 0 .and. kdt < 5) then
          print *, "OZONE turned OFF in gbphys - ozphys not called"
        end if
      endif

!  --- ...  to side-step the ozone physics

!      if (ntrac >= 2) then
!        do k = 1, levs
!          gq0(k,ntoz) = qgrs(k,ntoz)
!        enddo
!      endif

!     if (lprnt) then
!       print *,' levs=',levs,' jcap=',jcap,' dtp',dtp                  &
!    *,  ' slmsk=',slmsk(ilon,ilat),' kdt=',kdt
!       print *,' rann=',rann,' ncld=',ncld,' iq=',iq,' lat=',lat
!       print *,' pgr=',pgr
!       print *,' del=',del(ipr,:)
!       print *,' prsl=',prsl(ipr,:)
!       print *,' prslk=',prslk(ipr,:)
!       print *,' rann=',rann(ipr,1)
!       write(0,*)' gt0=',gt0(ipr,:)                                    &
!    &,         ' kdt=',kdt,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!       print *,' dtdt=',dtdt(ipr,:)
!       print *,' gu0=',gu0(ipr,:)
!       print *,' gv0=',gv0(ipr,:)
!       write(0,*)' gq0=',(gq0(ipr,k,1),k=1,levs),' lat=',lat
!       print *,' gq1=',(gq0(ipr,k,ntcw),k=1,levs)
!       print *,' vvel=',vvel
!     endif

      if (ldiag3d) then

        do k = 1, levs
          do i = 1, im
            dtdt(i,k)   = gt0(i,k)
!           dqdt(i,k,1) = gq0(i,k,1)
            dudt(i,k)   = gu0(i,k)
            dvdt(i,k)   = gv0(i,k)
          enddo
        enddo

      elseif (cnvgwd) then

        do k = 1, levs
          do i = 1, im
            dtdt(i,k)   = gt0(i,k)
          enddo
        enddo

      endif   ! end if_ldiag3d/cnvgwd

      if (ldiag3d .or. lgocart) then
        do k = 1, levs
          do i = 1, im
            dqdt(i,k,1)   = gq0(i,k,1)
          enddo
        enddo
      endif   ! end if_ldiag3d/lgocart

#ifdef GFS_HYDRO
      if (calc_phil) then
        call get_phi(im,ix,levs,ntrac,gt0,gq0,                            &
     &               thermodyn_id, sfcpress_id,                           &
     &               gen_coord_hybrid,                                    &
     &               prsi,prsik,prsl,prslk,phii,phil)
      endif
#else
! SJL: Adjust the heighz hydrostatically in a way consistent with FV3 discretization
      call get_phi_fv3(ix,levs,ntrac,gt0,gq0,del_gz,phii,phil)
#endif

!     if (lprnt) then
!       print *,' phii2=',phii(ipr,:)
!       print *,' phil2=',phil(ipr,:)
!     endif

      do k = 1, levs
        do i = 1, im
          clw(i,k,1) = 0.0
          clw(i,k,2) = -999.9
        enddo
      enddo
      if (.not. do_shoc) then
        do k = 1, levs
          do i = 1, im
            cnvc(i,k)  = 0.0
            cnvw(i,k)  = 0.0
          enddo
        enddo
      endif

!     write(0,*)' before cnv clstp=',clstp,' kdt=',kdt,' lat=',lat

!  --- ...  for convective tracer transport (while using ras)

      if (ras .or. cscnv) then
        if (tottracer > 0) then

          if (ntoz > 0) then
            do k=1,levs
              do i=1,im
                clw(i,k,3) = gq0(i,k,ntoz)
              enddo
            enddo

            if (tracers > 0) then
              do n = 1, tracers
                do k=1,levs
                  do i=1,im
                    clw(i,k,3+n) = gq0(i,k,n+trc_shft)
                  enddo
                enddo
              enddo
            endif
          else
            do n = 1, tracers
                do k=1,levs
                  do i=1,im
                    clw(i,k,2+n) = gq0(i,k,n+trc_shft)
                  enddo
                enddo
            enddo
          endif

        endif
      endif   ! end if_ras

      do i = 1, im
        ktop(i)  = 1
        kbot(i)  = levs
      enddo

!  --- ...  calling condensation/precipitation processes
!           --------------------------------------------

      if (ntcw > 0) then

        do k = 1, levs
          do i = 1, im
            tem      = rhbbot - (rhbbot-rhbtop) * (1.0-prslk(i,k))
            tem      = rhc_max * work1(i) + tem * work2(i)
            rhc(i,k) = max(0.0, min(1.0,tem))
          enddo
        enddo

        if (num_p3d == 3) then    !  brad ferrier's microphysics

!  --- ...  algorithm to separate different hydrometeor species

          do k = 1, levs
            do i = 1, im
              wc     = gq0(i,k,ntcw)
              qi     = 0.
              qr     = 0.
              qw     = 0.
              f_ice  = max(0.0, min(1.0, phy_f3d(i,k,1)))
              f_rain = max(0.0, min(1.0, phy_f3d(i,k,2)))

              qi = f_ice*wc
              qw = wc-qi
              if (qw > 0.0) then
                qr = f_rain*qw
                qw = qw-qr
              endif

!             if (f_ice >= 1.0) then
!               qi = wc
!             elseif (f_ice <= 0.0) then
!               qw = wc
!             else
!               qi = f_ice*wc
!               qw = wc-qi
!             endif

!             if (qw > 0.0 .and. f_rain > 0.0) then
!               if (f_rain >= 1.0) then
!                 qr = qw
!                 qw = 0.0
!               else
!                 qr = f_rain*qw
!                 qw = qw-qr
!               endif
!             endif

              qr_col(i,k) = qr
!             clw(i,k)    = qi + qw
              clw(i,k,1)  = qi
              clw(i,k,2)  = qw

!  --- ...  array to track fraction of "cloud" in the form of ice

!             if (qi+qw > epsq) then
!               fc_ice(i,k) = qi / (qi+qw)
!             else
!               fc_ice(i,k) = 0.0
!             endif
            enddo
          enddo

        elseif (num_p3d == 4) then   ! zhao-carr microphysics 

          do i = 1, im
            psautco_l(i) = psautco(1)*work1(i) + psautco(2)*work2(i)
            prautco_l(i) = prautco(1)*work1(i) + prautco(2)*work2(i)
          enddo
          do k = 1, levs
            do i = 1, im
              clw(i,k,1) = gq0(i,k,ntcw)
            enddo
          enddo

        endif  ! end if_num_p3d

      else    ! if_ntcw

        do i = 1, im
          psautco_l(i) = psautco(1)*work1(i) + psautco(2)*work2(i)
          prautco_l(i) = prautco(1)*work1(i) + prautco(2)*work2(i)
        enddo
        do k = 1, levs
          do i = 1, im
            rhc(i,k) = 1.0
          enddo
        enddo

      endif   ! end if_ntcw
!
!        Call SHOC iif do_shoc is true and shocaftcnv is false
!
      if (do_shoc .and. .not. shocaftcnv) then

        if (num_p3d == 4) then
          do k=1,levs
            do i=1,im
              qpl(i,k)   = 0.0
              qpi(i,k)   = 0.0
              tem = gq0(i,k,ntcw)                                         &
     &            * max(0.0, MIN(1.0, (TCR-gt0(i,k))*TCRF))
              clw(i,k,1) = tem                              ! ice
              clw(i,k,2) = gq0(i,k,ntcw) - tem              ! water
            enddo
          enddo
        endif
!       dtshoc = 60.0
!       dtshoc = 120.0
!       dtshoc = dtp
!       nshocm = (dtp/dtshoc) + 0.001
!       dtshoc = dtp / nshocm
!       do nshoc=1,nshocm
!      if (lprnt) write(1000+me,*)' before shoc tke=',clw(ipr,:,ntk),
!    &' kdt=',kdt,' lat=',lat,'xlon=',xlon(ipr),' xlat=',xlat(ipr)

!         call shoc(ix, im, 1, levs, levs+1, dtshoc, me, lat,           &
          call shoc(ix, im, 1, levs, levs+1, dtp, me, lat,              &
     &              prsl(1,1), phii(1,1), phil(1,1),                    &
     &              gu0(1,1),gv0(1,1), vvel(1,1), gt0(1,1), gq0(1,1,1), &
!    &              clw(1,1,1), clw(1,1,2), qpi, qpl, rhc, shoc_cld(1,1)&
!    &              clw(1,1,1), clw(1,1,2), qpi, qpl, shoc_cld(1,1)     &
     &              clw(1,1,1), clw(1,1,2), qpi, qpl,                   &
!    &              sgs_cld(1:im,1:levs)                                &
     &              phy_f3d(1,1,ntot3d-2), clw(1,1,ntk), hflx, evap,    &
     &              prnum, phy_f3d(1,1,ntot3d-1), phy_f3d(1,1,ntot3d),  &
     &              lprnt, ipr)
!       do k=1,levs
!         do i=1,im
!           sgs_cld(i,k) = sgs_cld(i,k) + shoc_cld(i,k)
!         enddo
!       enddo

!      if (lprnt) write(1000+me,*)' after shoc tke=',clw(1,:,ntk),
!    &' kdt=',kdt
!       enddo
!
!      do k=1,levs
!      write(1000+me,*)' maxcld=',maxval(sgs_cld(1:im,k)),
!      write(1000+me,*)' maxtkh=',maxval(phy_f3d(1:im,k,ntot3d-1)),
!    &' k=',k,' kdt=',kdt,' lat=',lat
!      enddo

!     write(0,*)' aft shoc gt0=',gt0(1,:),' lat=',lat
!     write(0,*)' aft shoc gq0=',gq0(1,:,1),' lat=',lat
!     write(0,*)' aft shoc gu0=',gu0(1,:),' lat=',lat
!
      endif   ! if(do_shoc)
      

!  --- ...  calling convective parameterization
!
!GFDL --- following if-test added to turn off deep convection parameterization
      if (.not. nocnv) then ! bypass convective parameterization
      if (.not. ras .and. .not. cscnv) then

        if (newsas) then             ! no random cloud top
          call sascnvn(im,ix,levs,jcap,dtp,del,prsl,pgr,phil,           &
     &                clw,gq0,gt0,gu0,gv0,cld1d,                        &
     &                rain1,kbot,ktop,kcnv,islmsk,                      &
     &                vvel,ncld,ud_mf,dd_mf,dt_mf,cnvw,cnvc)
!         if (lprnt) print *,' rain1=',rain1(ipr)
        else                         ! random cloud top
          call sascnv(im,ix,levs,jcap,dtp,del,prsl,pgr,phil,            &
     &                clw,gq0,gt0,gu0,gv0,cld1d,                        &
     &                rain1,kbot,ktop,kcnv,islmsk,                      &
     &                vvel,rann,ncld,ud_mf,dd_mf,dt_mf,cnvw,cnvc)
!         if (lprnt) print *,' rain1=',rain1(ipr),' rann=',rann(ipr,1)
        endif

      else    ! ras

!       if  (lprnt) print *,' calling ras for kdt=',kdt,' me=',me       &
!    &,                     ' lprnt=',lprnt

        if(cscnv) then    ! Chikira-Sugiyama                            !DD
           
!         fscav(:) = 0.0
!         fswtr(:) = 0.0
          call cs_convr(                                                & !DD
     &                  ix      ,im      ,levs    , tottracer+3 ,       & !DD
     &                  gt0     ,gq0     ,rain1   , clw       ,         & !DD
     &                  phil    ,phii    ,                              & !DD
     &                  prsl    ,prsi    ,                              & !DD
     &                  dtp     ,dtf     ,                              & !DD
     &                  ud_mf   ,dd_mf   ,dt_mf   ,                     & !DD
     &                  gu0     ,gv0     ,fscav, fswtr,                 & !DD
!    &                  phy_fctd, me         )                            !DD & moorthi
     &                  phy_fctd, me, wcbmax )                            !DD & moorthi
!    &                  phy_fctd     )                                    !DD
          do i = 1,im                                                     !DD
             rain1(i) = rain1(i) * (dtp*0.001)                            !DD
          enddo
        else
          if (ccwf(1) >= 0.0 .or. ccwf(2) >= 0 ) then
            do i=1,im
              ccwfac(i) = ccwf(1)*work1(i) + ccwf(2)*work2(i)
              dlqfac(i) = dlqf(1)*work1(i) + dlqf(2)*work2(i)
              lmh(i)    = levs
            enddo
          else
            do i=1,im
              ccwfac(i) = -999.0
              dlqfac(i) = 0.0
              lmh(i)    = levs
            enddo
          endif
!         if  (lprnt) write(0,*) ' calling ras for kdt=',kdt,' me=',me    &
!    &,                       ' lprnt=',lprnt,' ccwfac=',ccwfac(ipr)

          call rascnv(im,    ix,    levs,   dtp, dtf, rann                &
     &,               gt0,    gq0,   gu0,    gv0, clw, tottracer, fscav   &
     &,               prsi,   prsl,   prsik,  prslk, phil,  phii          &
     &,               kpbl,   cd,     rain1,  kbot,  ktop,  kcnv          &
     &,               phy_f2d(1,num_p2d), flipv, pa2mb                    &
     &,               me, garea, lmh, ccwfac, nrcm, rhc                   &
     &,               ud_mf, dd_mf, dt_mf, dlqfac, lprnt, ipr, kdt)
        endif

        cld1d = 0

        if (lgocart) then
          do k = 1, levs
            do i = 1, im
              upd_mf(i,k)  = upd_mf(i,k)  + ud_mf(i,k) * frain
              dwn_mf(i,k)  = dwn_mf(i,k)  + dd_mf(i,k) * frain
              det_mf(i,k)  = det_mf(i,k)  + dt_mf(i,k) * frain
              cnvqc_v(i,k) = cnvqc_v(i,k) + (clw(i,k,1)+clw(i,k,2)-     &
     &                                         gq0(i,k,ntcw)) * frain
            enddo
          enddo
        endif ! if (lgocart)

!  --- ...  update the tracers due to convective transport
  
        if (tottracer > 0) then
          if (ntoz > 0) then                         ! for ozone
            do k=1,levs
              do i=1,im
                gq0(i,k,ntoz) = clw(i,k,3)
              enddo
            enddo

            if (tracers > 0) then                    ! for other tracers
              do n = 1, tracers
                do k=1,levs
                  do i=1,im
                    gq0(i,k,n+trc_shft) = clw(i,k,3+n)
                  enddo
                enddo
              enddo
            endif
          else
            do n = 1, tracers
              do k=1,levs
                do i=1,im
                  gq0(i,k,n+trc_shft) = clw(i,k,2+n)
                enddo
              enddo
            enddo
          endif
        endif
      endif   ! end if_not_ras
!GFDL --- following added to turn off deep convection parameterization
      else   ! no cnv
       rain1(:)=0.0
      endif
!GFDL --- above added to turn off deep convection parameterization
!
      do i = 1, im
        rainc(i) = frain * rain1(i)
      enddo
!
      if (lssav) then
        do i = 1, im
          cldwrk(i)  = cldwrk(i)  + cld1d(i) * dtf
          cnvprcp(i) = cnvprcp(i) + rainc(i)
        enddo

        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dt3dt(i,k,4) = dt3dt(i,k,4) + (gt0(i,k)-dtdt(i,k)) * frain
              dq3dt(i,k,2) = dq3dt(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))    &
     &                                                           * frain
              du3dt(i,k,3) = du3dt(i,k,3) + (gu0(i,k)-dudt(i,k)) * frain
              dv3dt(i,k,3) = dv3dt(i,k,3) + (gv0(i,k)-dvdt(i,k)) * frain

              upd_mf(i,k)  = upd_mf(i,k)  + ud_mf(i,k) * (con_g*frain)
              dwn_mf(i,k)  = dwn_mf(i,k)  + dd_mf(i,k) * (con_g*frain)
              det_mf(i,k)  = det_mf(i,k)  + dt_mf(i,k) * (con_g*frain)

            enddo
          enddo
        endif ! if (ldiag3d)

      endif   ! end if_lssav
!
!       update dqdt_v to include moisture tendency due to deep convection
        if (lgocart) then
          do k = 1, levs
            do i = 1, im
              dqdt_v(i,k)  = (gq0(i,k,1)-dqdt(i,k,1))  * frain
              upd_mf(i,k)  = upd_mf(i,k)  + ud_mf(i,k) * frain
              dwn_mf(i,k)  = dwn_mf(i,k)  + dd_mf(i,k) * frain
              det_mf(i,k)  = det_mf(i,k)  + dt_mf(i,k) * frain
              cnvqc_v(i,k) = cnvqc_v(i,k) + (clw(i,k,1)+clw(i,k,2))
     &                                                        *frain
            enddo
          enddo
        endif ! if (lgocart)
!
      if( npdf3d == 3  .and. num_p3d == 4 ) then
        num2 = num_p3d + 2
        num3 = num2 + 1
        do k = 1, levs
          do i = 1, im
            phy_f3d(i,k,num2) = cnvw(i,k)
            phy_f3d(i,k,num3) = cnvc(i,k)
          enddo
        enddo
      endif 

!     write(0,*)' cnvgwd moninshobl clstp=',clstp,' kdt=',kdt,
!    &         ' lat=',lat
!
      if (cnvgwd) then         !        call convective gravity wave drag

!  --- ...  calculate maximum convective heating rate            qmax [k/s]
!           cuhr = temperature change due to deep convection

        do i = 1, im
!         qmax(i)   = 0.
          cumabs(i) = 0.0
          work3(i)  = 0.0
        enddo

        do k = 1, levs
          do i = 1, im
!           cuhr(i,k) = (gt0(i,k)-dtdt(i,k)) / dtf
!           cuhr(i,k) = (gt0(i,k)-dtdt(i,k)) / dtp    ! moorthi

!           cumchr(i,k) = 0.
!           gwdcu(i,k)  = 0.
!           gwdcv(i,k)  = 0.
!           diagn1(i,k) = 0.
!           diagn2(i,k) = 0.

            if (k >= kbot(i) .and. k <= ktop(i)) then
!             qmax(i)   = max(qmax(i),cuhr(i,k))
!             cumabs(i) = cuhr(i,k) + cumabs(i)
              cumabs(i) = cumabs(i) + (gt0(i,k)-dtdt(i,k)) * del(i,k)
              work3(i)  = work3(i)  + del(i,k)
            endif
          enddo
        enddo
        do i=1,im
          if (work3(i) > 0.0) cumabs(i) = cumabs(i) / (dtp*work3(i))
        enddo

!       do i = 1, im
!         do k = kbot(i), ktop(i)
!           do k1 = kbot(i), k
!             cumchr(i,k) = cuhr(i,k1) + cumchr(i,k)
!           enddo
!           cumchr(i,k) = cumchr(i,k) / cumabs(i)
!         enddo
!       enddo

!  --- ...  begin check print ******************************************

!       if (lprnt) then
!         if (kbot(ipr) <= ktop(ipr)) then
!           write(*,*) 'kbot <= ktop     for (lat,lon) = ',             &
!    &            xlon(ipr)*57.29578,xlat(ipr)*57.29578
!           write(*,*) 'kcnv kbot ktop qmax dlength  ',kcnv(ipr),       &
!    &            kbot(ipr),ktop(ipr),(86400.*qmax(ipr)),dlength(ipr)
!           write(*,9000) kdt
!9000       format(/,3x,'k',5x,'cuhr(k)',4x,'cumchr(k)',5x,             &
!    &            'at kdt = ',i4,/)

!           do k = ktop(ipr), kbot(ipr),-1
!             write(*,9010) k,(86400.*cuhr(ipr,k)),(100.*cumchr(ipr,k))
!9010         format(2x,i2,2x,f8.2,5x,f6.0)
!           enddo
!         endif

!         if (fhour >= fhourpr) then
!           print *,' before gwdc in gbphys start print'
!           write(*,*) 'fhour ix im levs = ',fhour,ix,im,levs
!           print *,'dtp  dtf  = ',dtp,dtf

!           write(*,9100)
!9100       format(//,14x,'pressure levels',//                          &
!    &             ' ilev',7x,'prsi',8x,'prsl',8x,'delp',/)

!           k = levs + 1
!           write(*,9110) k,(10.*prsi(ipr,k))
!9110       format(i4,2x,f10.3)

!           do k = levs, 1, -1
!             write(*,9120) k,(10.*prsl(ipr,k)),(10.*del(ipr,k))
!             write(*,9110) k,(10.*prsi(ipr,k))
!           enddo
!9120       format(i4,12x,2(2x,f10.3))

!           write(*,9130)
!9130       format(//,10x,'before gwdc in gbphys',//,' ilev',6x,        &
!    &             'ugrs',9x,'gu0',8x,'vgrs',9x,'gv0',8x,               &
!    &             'tgrs',9x,'gt0',8x,'gt0b',8x,'dudt',8x,'dvdt',/)

!           do k = levs, 1, -1
!             write(*,9140) k,ugrs(ipr,k),gu0(ipr,k),                   &
!    &                        vgrs(ipr,k),gv0(ipr,k),                   &
!    &                        tgrs(ipr,k),gt0(ipr,k),dtdt(ipr,k),       &
!    &                        dudt(ipr,k),dvdt(ipr,k)
!           enddo
!9140       format(i4,9(2x,f10.3))

!           print *,' before gwdc in gbphys end print'
!         endif
!       endif   ! end if_lprnt

!  --- ...  end check print ********************************************

        call gwdc(im, ix, im, levs, lat, ugrs, vgrs, tgrs, qgrs,        &
     &            prsl, prsi, del, cumabs,       ktop, kbot, kcnv,cldf, &
     &            con_g,con_cp,con_rd,con_fvirt, dlength,               &
     &            lprnt, ipr, fhour,                                    &
     &            gwdcu, gwdcv,dusfcg,dvsfcg)


!       if (lprnt) then
!         if (fhour >= fhourpr) then
!           print *,' after gwdc in gbphys start print'

!           write(*,9131)
!9131       format(//,10x,'after gwdc in gbphys',//,' ilev',6x,         &
!    &             'ugrs',9x,'gu0',8x,'vgrs',9x,'gv0',8x,               &
!    &             'tgrs',9x,'gt0',8x,'gt0b',7x,'gwdcu',7x,'gwdcv',/)

!           do k = levs, 1, -1
!             write(*,9141) k,ugrs(ipr,k),gu0(ipr,k),                   &
!    &                        vgrs(ipr,k),gv0(ipr,k),                   &
!    &                        tgrs(ipr,k),gt0(ipr,k),dtdt(ipr,k),       &
!    &                        gwdcu(ipr,k),gwdcv(ipr,k)
!           enddo
!9141       format(i4,9(2x,f10.3))

!           print *,' after gwdc in gbphys end print'
!         endif
!       endif

!  --- ...  write out cloud top stress and wind tendencies

        if (lssav) then
          do i = 1, im
            dugwd(i) = dugwd(i) + dusfcg(i)*dtf
            dvgwd(i) = dvgwd(i) + dvsfcg(i)*dtf
          enddo

          if (ldiag3d) then
            do k = 1, levs
              do i = 1, im
                du3dt(i,k,4) = du3dt(i,k,4) + gwdcu(i,k)  * dtf
                dv3dt(i,k,4) = dv3dt(i,k,4) + gwdcv(i,k)  * dtf
!               du3dt(i,k,2) = du3dt(i,k,2) + diagn1(i,k) * dtf
!               dv3dt(i,k,2) = dv3dt(i,k,2) + diagn2(i,k) * dtf
              enddo
            enddo
          endif
        endif   ! end if_lssav

!  --- ...  update the wind components with  gwdc tendencies

        do k = 1, levs
          do i = 1, im
            eng0      = 0.5*(gu0(i,k)*gu0(i,k)+gv0(i,k)*gv0(i,k))
            gu0(i,k)  = gu0(i,k) + gwdcu(i,k) * dtp
            gv0(i,k)  = gv0(i,k) + gwdcv(i,k) * dtp
            eng1      = 0.5*(gu0(i,k)*gu0(i,k)+gv0(i,k)*gv0(i,k))
            gt0(i,k)  = gt0(i,k) + (eng0-eng1)/(dtp*con_cp)
          enddo
        enddo

!       if (lprnt) then
!         if (fhour >= fhourpr) then
!           print *,' after tendency gwdc in gbphys start print'

!           write(*,9132)
!9132       format(//,10x,'after tendency gwdc in gbphys',//,' ilev',6x,&
!    &             'ugrs',9x,'gu0',8x,'vgrs',9x,'gv0',8x,               &
!    &             'tgrs',9x,'gt0',8x,'gt0b',7x,'gwdcu',7x,'gwdcv',/)

!           do k = levs, 1, -1
!             write(*,9142) k,ugrs(ipr,k),gu0(ipr,k),vgrs(ipr,k),       &
!    &              gv0(ipr,k),tgrs(ipr,k),gt0(ipr,k),dtdt(ipr,k),      &
!    &              gwdcu(ipr,k),gwdcv(ipr,k)
!           enddo
!9142       format(i4,9(2x,f10.3))

!           print *,' after tendency gwdc in gbphys end print'
!         endif
!       endif

      endif   ! end if_cnvgwd (convective gravity wave drag)

!     write(0,*)' after cnvgwd  clstp=',clstp,' kdt=',kdt,
!    &         ' lat=',lat

      if (ldiag3d) then
        do k = 1, levs
          do i = 1, im
            dtdt(i,k)   = gt0(i,k)
!           dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      endif
      if (ldiag3d .or. lgocart) then
        do k = 1, levs
          do i = 1, im
            dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      endif

!     write(0,*)' before do_shoc shal clstp=',clstp,' kdt=',kdt,
!    &         ' lat=',lat

      if (.not. do_shoc) then

        if (shal_cnv) then               ! Shallow convection parameterization
!                                        -----------------------------------
          if (sashal) then               ! opr option now at 2014
                                         !-----------------------
            call shalcnv(im,ix,levs,dtp,del,prsl,pgr,phil,              &
     &                   clw,gq0,gt0,gu0,gv0,                           &
     &                   kbot,ktop,kcnv,islmsk,                         &
     &                   vvel,ncld,hpbl,hflx,evap,ud_mf,dt_mf,          &
     &                   cnvw,cnvc)

            if (shcnvcw .and. num_p3d == 4 .and. npdf3d == 3 ) then
              do k = 1, levs
                do i = 1, im
                  phy_f3d(i,k,num2) = cnvw(i,k)
                  phy_f3d(i,k,num3) = cnvc(i,k)
!???              phy_f3d(i,k,num2) = phy_f3d(i,k,num2) + cnvw(i,k)
!???              phy_f3d(i,k,num3) = phy_f3d(i,k,num3) + cnvc(i,k)
                enddo
              enddo
            endif
!           do i = 1, im
!             raincs(i) = frain    * rain1(i)
!             rainc(i)  = rainc(i) + raincs(i)
!           enddo
!           if (lssav) then
!             do i = 1, im
!               cnvprcp(i) = cnvprcp(i) + raincs(i)
!             enddo
!           endif

          else                           ! modified Tiedtke Shallow convecton
                                         !-----------------------------------
            do i = 1, im
              levshc(i) = 0
            enddo
            do k = 2, levs
              do i = 1, im
                if (prsi(i,1)-prsi(i,k) <= dpshc(i)) levshc(i) = k
              enddo
            enddo
            levshcm = 1
            do i = 1, im
              levshcm = max(levshcm, levshc(i))
            enddo

!           if (lprnt) print *,' levshcm=',levshcm,' gt0sh=',gt0(ipr,:)
!    &,    ' lat=',lat

            if (mstrat) then             !  As in CFSv2
              call shalcv(im,ix,levshcm,dtp,del,prsi,prsl,prslk,kcnv,   &
     &                    gq0,gt0,levshc,phil,kinver,ctei_r,ctei_rml    &
     &,                                                    lprnt,ipr)
            else
              call shalcvt3(im,ix,levshcm,dtp,del,prsi,prsl,prslk,      &
     &                      kcnv,gq0,gt0)
            endif
!           if (lprnt) print *,' levshcm=',levshcm,' gt0sha=',gt0(ipr,:)

          endif   ! end if_sashal
        endif     ! end if_shal_cnv

        if (lssav) then
!          update dqdt_v to include moisture tendency due to shallow convection
          if (lgocart) then
            do k = 1, levs
              do i = 1, im
                tem         = (gq0(i,k,1)-dqdt(i,k,1)) * frain
                dqdt_v(i,k) = dqdt_v(i,k)  + tem
              enddo
            enddo
          endif
          if (ldiag3d) then
            do k = 1, levs
              do i = 1, im
                dt3dt(i,k,5) = dt3dt(i,k,5) + (gt0(i,k)-dtdt(i,k))
     &                                                          * frain
                dq3dt(i,k,3) = dq3dt(i,k,3) + (gq0(i,k,1)-dqdt(i,k,1))  &
     &                                                          * frain
                dtdt(i,k)   = gt0(i,k)
                dqdt(i,k,1) = gq0(i,k,1)
              enddo
            enddo
          endif
        endif   ! end if_lssav
!
        do k = 1, levs
          do i = 1, im
            if (clw(i,k,2) <= -999.0) clw(i,k,2) = 0.0
          enddo
        enddo

!       if (lprnt) then
!         write(0,*)' prsl=',prsl(ipr,:)
!         write(0,*) ' del=',del(ipr,:)
!         write(0,*) ' befshgt0=',gt0(ipr,:)
!         write(0,*) ' befshgq0=',gq0(ipr,:,1)
!       endif

      elseif (shocaftcnv) then ! if do_shoc is true and shocaftcnv is true call shoc

        if (clw(1,1,2) < -999.0) then ! if clw is not partitioned to ice and water
          do k=1,levs
            do i=1,im
              tem = gq0(i,k,ntcw)                                       &
     &            * max(0.0, MIN(1.0, (TCR-gt0(i,k))*TCRF))
              clw(i,k,1) = tem                              ! ice
              clw(i,k,2) = gq0(i,k,ntcw) - tem              ! water
            enddo
          enddo
        endif
        do k=1,levs
          do i=1,im
            qpl(i,k)   = 0.0
            qpi(i,k)   = 0.0
          enddo
        enddo
!       dtshoc = 60.0
!       nshocm = (dtp/dtshoc) + 0.001
!       dtshoc = dtp / nshocm
!       do nshoc=1,nshocm
!       call shoc(im, 1, levs, levs+1, dtp, me, lat,        &
!!       call shoc(im, 1, levs, levs+1, dtshoc, me, lat, &
!    &                       prsl(1:im,:), phii (1:im,:),  phil(1:im,:),&
!    &          gu0(1:im,:),gv0(1:im,:), vvel(1:im,:), gt0(1:im,:),     &
!    &                                                   gq0(1:im,:,1), &
!    &          clw(1:im,:,1), clw(1:im,:,2), qpi, qpl,  sgs_cld(1:im,:)&
!    &,         gq0(1:im,:,ntke),                                       &
!    &          phy_f3d(1:im,:,ntot3d-1), phy_f3d(1:im,:,ntot3d),       &
!    &          lprnt, ipr,                                             &
!    &          con_cp, con_g, con_hvap, con_hfus, con_hvap+con_hfus,   &
!    &          con_rv, con_rd, con_pi, con_fvirt)

!       call shoc(ix, im, 1, levs, levs+1, dtshoc, me, lat,             &
        call shoc(ix, im, 1, levs, levs+1, dtp, me, lat,                &
     &            prsl(1,1), phii(1,1), phil(1,1),                      &
     &            gu0(1,1),gv0(1,1), vvel(1,1), gt0(1,1), gq0(1,1,1),   &
     &            clw(1,1,1), clw(1,1,2), qpi, qpl,                     &
     &            phy_f3d(1,1,ntot3d-2),  gq0(1,1,ntke),hflx,evap,      &
     &            prnum, phy_f3d(1,1,ntot3d-1), phy_f3d(1,1,ntot3d),    &
     &            lprnt, ipr)
!
!      do k=1,levs
!      write(1000+me,*)' maxtkh=',maxval(phy_f3d(1:im,k,ntot3d-1)),
!    &' k=',k,' kdt=',kdt,' lat=',lat
!      enddo

!     write(0,*)' aft shoc gt0=',gt0(1,:),' lat=',lat
!     write(0,*)' aft shoc gq0=',gq0(1,:,1),' lat=',lat
!     write(0,*)' aft shoc gu0=',gu0(1,:),' lat=',lat
!
      endif   ! if( .not. do_shoc)
!
!       if (lprnt) then
!         write(0,*)' prsl=',prsl(ipr,:)
!         write(0,*) ' del=',del(ipr,:)
!         write(0,*) ' aftshgt0=',gt0(ipr,:)
!         write(0,*) ' aftshgq0=',gq0(ipr,:,1)
!       endif

      if (ntcw > 0) then

!     microphysics

        if (num_p3d == 3) then    ! call brad ferrier's microphysics

          do k = 1, levs
            do i = 1, im
!             qi = clw(i,k)*fc_ice(i,k)
!             qw = clw(i,k) - qi
              qi = clw(i,k,1)
              qw = clw(i,k,2)

!  --- ...  algorithm to combine different hydrometeor species

!             gq0(i,k,ntcw) = max(epsq, qi+qw+qr_col(i,k))
              gq0(i,k,ntcw) = qi + qw + qr_col(i,k)

              if (qi <= epsq) then
                phy_f3d(i,k,1) = 0.
              else
                phy_f3d(i,k,1) = qi/gq0(i,k,ntcw)
              endif

              if (qr_col(i,k) <= epsq) then
                phy_f3d(i,k,2) = 0.
              else
                phy_f3d(i,k,2) = qr_col(i,k) / (qw+qr_col(i,k))
              endif

            enddo
          enddo

        elseif (num_p3d == 4) then    ! if_num_p3d

          do k = 1, levs
            do i = 1, im
              gq0(i,k,ntcw) = clw(i,k,1) + clw(i,k,2)
            enddo
          enddo

        endif   ! end if_num_p3d

      else    ! if_ntcw

        do k = 1, levs
          do i = 1, im
            clw(i,k,1) = clw(i,k,1) + clw(i,k,2)
          enddo
        enddo

      endif   ! end if_ntcw

!     write(0,*)' bef cnvc90 clstp=',clstp,' kdt=',kdt,' lat=',lat
      call cnvc90(clstp, im,   ix,   rainc, kbot, ktop, levs, prsi,     &
     &            acv,   acvb, acvt, cv,    cvb,  cvt)


      if (moist_adj) then       ! moist convective adjustment
!                                 ---------------------------
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       if (me .eq. 0) then
!         sumq = 0.0
!         DO K=1,LEVS
!           do i=1,im
!             sumq = sumq - (gq0(i,k,1)+gq0(i,k,ntcw)) * del(i,k)
!           enddo
!         enddo
!       endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       To call moist convective adjustment
!
!       if (lprnt) then
!         print *,' prsl=',prsl(ipr,:)
!         print *,' del=',del(ipr,:)
!         print *,' gt0b=',gt0(ipr,:)
!         print *,' gq0b=',gq0(ipr,:,1)
!       endif

        call mstcnv(im,ix,levs,dtp,gt0,gq0,prsl,del,prslk,rain1
     &,                          gq0(1,1,ntcw), rhc, lprnt,ipr)

!       if (lprnt) then
!         print *,' rain1=',rain1(ipr),' rainc=',rainc(ipr)
!         print *,' gt0a=',gt0(ipr,:)
!         print *,' gq0a=',gq0(ipr,:,1)
!       endif
        do i=1,im
          rainc(i) = rainc(i) + frain * rain1(i)
        enddo
        if(lssav) then
          do i=1,im
            cnvprcp(i) = cnvprcp(i) + rain1(i) * frain
          enddo

! update dqdt_v to include moisture tendency due to surface processes
! dqdt_v : instaneous moisture tendency (kg/kg/sec)
!          if (lgocart) then
!            do k=1,levs
!              do i=1,im
!                tem = (gq0(i,k,1)-dqdt(i,k,1)) * frain
!                dqdt_v(i,k) = dqdt_v(i,k) + tem
!                dqdt_v(i,k) = dqdt_v(i,k) / dtf
!              enddo
!            enddo
!          endif
          if (ldiag3d) then
            do k=1,levs
              do i=1,im
                dt3dt(i,k,4) = dt3dt(i,k,4) + (gt0(i,k)-dtdt(i,k))
     &                                      * frain
                dq3dt(i,k,2) = dq3dt(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))
     &                                      * frain
              enddo
            enddo
          endif
         endif
!
        if (ldiag3d) then
          do k=1,levs
            do i=1,im
              dtdt(i,k)   = gt0(i,k)
              dqdt(i,k,1) = gq0(i,k,1)
            enddo
          enddo
        endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       if (me .eq. 0) then
!         DO K=1,LEVS
!           do i=1,im
!             sumq = sumq + (gq0(i,k,1)+gq0(i,k,ntcw)) * del(i,k)
!           enddo
!         enddo
!         sumr = 0.0
!         do i=1,im
!           sumr = sumr + rain1(i)
!         enddo
!         sumq = sumq * 1000.0 / grav
!         sumr = sumr *1000
!         print *,' after MCN: sumq=',sumq,' sumr=',sumr, ' kdt=',kdt
!       endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      endif               !       moist convective adjustment over

! dqdt_v : instaneous moisture tendency (kg/kg/sec)
      if (lgocart) then
        do k=1,levs
          do i=1,im
            dqdt_v(i,k) = dqdt_v(i,k) / dtf
          enddo
        enddo
      endif
!
!     grid-scale condensation/precipitations and microphysics parameterization
!     ------------------------------------------------------------------------

      if (ncld == 0) then           ! no cloud microphysics

        call lrgscl(ix,im,levs,dtp,gt0,gq0,prsl,del,prslk,rain1,clw)

      elseif (ncld == 1) then       ! microphysics with single cloud condensate

        if (num_p3d == 3) then      ! brad ferrier's microphysics
                                    ! ---------------------------
          do i = 1, im
            xncw(i)     = ncw(1)   * work1(i) + ncw(2)    * work2(i)
            flgmin_l(i) = flgmin(1)* work1(i) + flgmin(2) * work2(i)
          enddo

          if (kdt == 1 .and. abs(xlon(1)) < 0.0001) then
            write(0,*)' xncw=',xncw(1),' rhc=',rhc(1,1),' work1='       &
     &,          work1(1),' work2=',work2(1),' flgmin=',flgmin_l(1)     &
     &,         ' lon=',xlon(1) * 57.29578,' lat=',lat,' me=',me
          endif

          call gsmdrive(im, ix, levs, dtp, con_g, con_hvap, hsub, con_cp&
     &,                 me, lprnt, ipr                                  &
     &,                 prsl, del, rhc, xncw, flgmin_l                  &
     &,                 gt0, gq0(1,1,1), gq0(1,1,ntcw)                  &
     &,                 phy_f3d(1,1,1),  phy_f3d(1,1,2)                 &
     &,                 phy_f3d(1,1,3), rain1, sr)

        elseif (num_p3d == 4) then  ! call zhao/carr/sundqvist microphysics

          if (npdf3d /= 3) then               ! without pdf clouds

!           if (lprnt) then
!             write(0,*)' prsl=',prsl(ipr,:)
!             write(0,*) ' del=',del(ipr,:)
!             write(0,*) ' beflsgt0=',gt0(ipr,:),' kdt=',kdt
!             write(0,*) ' beflsgq0=',gq0(ipr,:,1),' kdt=',kdt
!           endif
                                              ! ------------------
            if (do_shoc) then
              call precpd_shoc(im, ix, levs, dtp, del, prsl,            &
     &                    gq0(1,1,1), gq0(1,1,ntcw), gt0, rain1, sr,    &
     &                    rainp, rhc, psautco_l, prautco_l, evpco,      &
     &                    wminco, phy_f3d(1,1,ntot3d-2), lprnt, ipr)
!    &                    wminco, sgs_cld(1:im,1:levs), lprnt, ipr)
!    &                    wminco, shoc_cld, lprnt, ipr)
            else
              call gscond(im, ix, levs, dtp, dtf, prsl, pgr,            &
     &                    gq0(1,1,1), gq0(1,1,ntcw), gt0,               &
     &                    phy_f3d(1,1,1), phy_f3d(1,1,2), phy_f2d(1,1), &
     &                    phy_f3d(1,1,3), phy_f3d(1,1,4), phy_f2d(1,2), &
     &                    rhc,lprnt, ipr)

              call precpd(im, ix, levs, dtp, del, prsl,                 &
     &                    gq0(1,1,1), gq0(1,1,ntcw), gt0, rain1, sr,    &
     &                    rainp, rhc, psautco_l, prautco_l, evpco,      &
     &                    wminco, lprnt, ipr)
            endif
!           if (lprnt) then
!             write(0,*)' prsl=',prsl(ipr,:)
!             write(0,*) ' del=',del(ipr,:)
!             write(0,*) ' aftlsgt0=',gt0(ipr,:),' kdt=',kdt
!             write(0,*) ' aftlsgq0=',gq0(ipr,:,1),' kdt=',kdt
!           endif
          else                                ! with pdf clouds
                                              ! ---------------
            call gscondp(im, ix, levs, dtp, dtf, prsl, pgr,             &
     &                  gq0(1,1,1), gq0(1,1,ntcw), gt0,                 &
     &                  phy_f3d(1,1,1), phy_f3d(1,1,2), phy_f2d(1,1),   &
     &                  phy_f3d(1,1,3), phy_f3d(1,1,4), phy_f2d(1,2),   &
     &                  rhc,phy_f3d(1,1,num_p3d+1),sup,lprnt,           &
     &                  ipr,kdt)

            call precpdp(im, ix, levs, dtp, del, prsl, pgr,             &
     &                  gq0(1,1,1), gq0(1,1,ntcw), gt0, rain1,sr,       &
     &                  rainp, rhc, phy_f3d(1,1,num_p3d+1),             &
     &                  psautco_l, prautco_l, evpco, wminco,            &
     &                  lprnt, ipr)

          endif   ! end of grid-scale precip/microphysics options
        endif     ! end if_num_p3d

      endif       ! end if_ncld

!     if (lprnt) write(0,*) ' rain1=',rain1(ipr),' rainc=',rainc(ipr)

      do i = 1, im
        rainl(i) = frain    * rain1(i)
        rain(i)  = rainc(i) + rainl(i)
      enddo

!  --- ...  coupling insertion

      if (lssav_cpl) then
        do i = 1, im
          rain_cpl(i) = rain_cpl(i) + rain(i)
        enddo
      endif

!  --- ...  end coupling insertion

      if (cal_pre) then       ! hchuang: add dominant precipitation type algorithm

        call calpreciptype(kdt,nrcm,im,ix,levs,levs+1,rann,
     &                     xlat,xlon,gt0,gq0,prsl,prsi,rain,
     &                     phii,num_p3d,tsea,sr,phy_f3d(1,1,3),             ! input
     &                     domr,domzr,domip,doms)                           ! output

!
!        if (lprnt) print*,'debug calpreciptype: DOMR,DOMZR,DOMIP,DOMS '
!     &,DOMR(ipr),DOMZR(ipr),DOMIP(ipr),DOMS(ipr)
!        do i=1,im
!         if (abs(xlon(i)*57.29578-114.0) .lt. 0.2  .and.
!     &    abs(xlat(i)*57.29578-40.0) .lt. 0.2)
!     &    print*,'debug calpreciptype: DOMR,DOMZR,DOMIP,DOMS ',
!     &    DOMR(i),DOMZR(i),DOMIP(i),DOMS(i)
!       end do
!       HCHUANG: use new precipitation type to decide snow flag for LSM snow accumulation

        do i=1,im
          if(doms(i) >0.0 .or. domip(i)>0.0)then
            srflag(i) = 1.
          else
            srflag(i) = 0.
          end if
        enddo
      endif

      if (lssav) then
        do i = 1, im
          totprcp(i) = totprcp(i) + rain(i)
        enddo

        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dt3dt(i,k,6) = dt3dt(i,k,6) + (gt0(i,k)-dtdt(i,k)) * frain
              dq3dt(i,k,4) = dq3dt(i,k,4) + (gq0(i,k,1)-dqdt(i,k,1))    &
     &                                                           * frain
            enddo
          enddo
        endif
      endif

!     if (lprnt) write(0,*) ' bef  estimate t850 for'
!  --- ...  estimate t850 for rain-snow decision

      do i = 1, im
        t850(i) = gt0(i,1)
      enddo

      do k = 1, levs-1
        do i = 1, im
          if (prsl(i,k) > p850 .and. prsl(i,k+1) <= p850) then
            t850(i) = gt0(i,k) - (prsl(i,k)-p850)                       &
     &              / (prsl(i,k)-prsl(i,k+1)) * (gt0(i,k)-gt0(i,k+1))
          endif
        enddo
      enddo

!  --- ...  lu: snow-rain detection is performed in land/sice module

      if (cal_pre) then ! hchuang: new precip type algorithm defines srflag
        do i = 1, im
          tprcp(i) = rain(i)            ! clu: rain -> tprcp
        enddo
      else
        do i = 1, im
          tprcp(i) = rain(i)            ! clu: rain -> tprcp
          srflag(i) = 0.                ! clu: default srflag as 'rain' (i.e. 0)

          if (t850(i) <= 273.16) then
            srflag(i) = 1.              ! clu: set srflag to 'snow' (i.e. 1)
          endif
        enddo
      endif

      if (lsm == 0) then   ! for OSU land model

!  --- ...  wei: when calling osu lsm update soil moisture and canopy water
!                after precipitation computaion
        do i = 1, im
          if (t850(i) <= 273.16 .and. islmsk(i) /= 0) then
            weasd(i)  = weasd(i) + 1.e3*rain(i)
            tprcp(i)  = 0.
          endif
        enddo
        call progt2(im,lsoil,rhscnpy,rhsmc,ai,bi,cci,smsoil,            &
     &              islmsk,canopy,tprcp,runof,snowmt,                   &
     &              zsoil,soiltyp,sigmaf,dtf,me)

!  --- ...  wei: let soil liquid water equal to soil total water

        do k = 1, lsoil  ! let soil liquid water equal to soil total water
          do i = 1, im
            if (islmsk(i) == 1) then
              slsoil(i,k) = smsoil(i,k)
             endif
          enddo
        enddo

      endif   ! end if_lsm
!!!
!!! update surface diagnosis fields at the end of phys package
!!! this change allows gocart to use filtered wind fields
!!!
      if ( lgocart ) then
          call sfc_diag(im,pgr,gu0,gv0,gt0,gq0,                         &
     &              tsea,qss,f10m,u10m,v10m,t2m,q2m,work3,              &
     &              evap,ffmm,ffhh,fm10,fh2)

          if (lssav) then
            do i = 1, im
              tmpmax(i)  = max(tmpmax(i),t2m(i))
              tmpmin(i)  = min(tmpmin(i),t2m(i))

              spfhmax(i) = max(spfhmax(i),q2m(i))
              spfhmin(i) = min(spfhmin(i),q2m(i))
            enddo
          endif
      endif

!  --- ...  total runoff is composed of drainage into water table and
!           runoff at the surface and is accumulated in unit of meters

      if (lssav) then
        tem = dtf * 0.001
        do i = 1, im
          runoff(i)  = runoff(i)  + (drain(i)+runof(i)) * tem
          srunoff(i) = srunoff(i) + runof(i) * tem
        enddo
      endif
!     if (lprnt) write(0,*) ' after srunoff'
       
!  --- ...  xw: return updated ice thickness & concentration to global array

      do i = 1, im
        if (islmsk(i) == 2) then
          hice(i)  = zice(i)
          fice(i)  = cice(i)
          tisfc(i) = tice(i)
        else
          hice(i)  = 0.0
          fice(i)  = 0.0
          tisfc(i) = tsea(i)
        endif
      enddo

!  --- ...  return updated smsoil and stsoil to global arrays

      do k = 1, lsoil
        do i = 1, im
          smc(i,k) = smsoil(i,k)
          stc(i,k) = stsoil(i,k)
          slc(i,k) = slsoil(i,k)
        enddo
      enddo

!  --- ...  calculate column precipitable water "pwat"

      do i = 1, im
        pwat(i)  = 0.
!       rqtk(i)  = 0.
!       work2(i) = 1.0 / pgr(i)
      enddo

      do k = 1, levs
        do i = 1, im
          work1(i) = 0.0
        enddo

        if (ncld > 0) then
          do ic = ntcw, ntcw+ncld-1
            do i = 1, im
              work1(i) = work1(i) + gq0(i,k,ic)
            enddo
          enddo
        endif

        do i = 1, im
!     if (lat == 45 .and. i == 1) write(1000+me,*)' pwatb=',pwat(1),
!    &' kdt=',kdt,'del=',del(1,k),' gq0=',gq0(1,k,1),' work1=',
!    &work1(1a,' k=',k
          pwat(i) = pwat(i) + del(i,k)*(gq0(i,k,1)+work1(i))
          rqtk(i) = rqtk(i) + del(i,k)*(gq0(i,k,1)-qgrs(i,k,1))
        enddo
      enddo

      do i = 1, im
        pwat(i) = pwat(i) * (1.0/con_g)
!       rqtk(i) = rqtk(i) * work2(i)
      enddo
!
!     if (lat == 45) write(1000+me,*)' pwat=',pwat(1),' kdt=',kdt
!       if (lprnt) then
!         write(0,*) ' endgt0=',gt0(ipr,:),' kdt=',kdt
!         write(0,*) ' endgq0=',gq0(ipr,:,1),' kdt=',kdt
!       endif
      deallocate (clw)
      if (do_shoc) then
        deallocate (qpl, qpi)
      else
        deallocate (cnvc, cnvw)
      endif
!     deallocate (fscav, fswtr)
!
!     if (lprnt) call mpi_quit(7)
!     if (kdt >300 ) call mpi_quit(7)

      return
!...................................
      end subroutine gbphys
!-----------------------------------

