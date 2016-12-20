!===================================================================== !
!  description:                                                         !
!                                                                       !
!     gbphys is the driver subroutine to invoke gfs am physics          !
!     (except radiation but radiative heating is applied here)          !
!     at physics time steps                                             !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call gbphys                                                        !
!       inputs:                                                         !
!         ( im,ix,levs,lsoil,ntrac,ncld,ntoz,ntcw,                      !
!           nmtvr,nrcm,ko3,lonf,latg,jcap,num_p3d,num_p2d,              !
!           kdt,lat,me,pl_coeff,nlons,ncw,flgmin,crtrh,cdmbgwd,         !
!           ccwf,dlqf,clstp,cgwf,dtp,dtf,fhour,solhr,                   !
!           slag,sdec,cdec,sinlat,coslat,pgr,ugrs,vgrs,                 !
!           tgrs,qgrs,vvel,prsi,prsl,prslk,prsik,phii,phil,             !
!           rann,prdout,poz,dpshc,hprime,xlon,xlat,                     !
!           slope,shdmin,shdmax,snoalb,tg3,slmsk,vfrac,                 !
!           vtype,stype,uustar,oro,oro_uf,coszen,sfcdsw,sfcnsw,         !
!           sfcdlw,tsflw,sfcemis,sfalb,swh,swhc,hlw,hlwc,ras,pre_rad,   !
!           ldiag3d,lssav,lssav_cc,                                     !
!           xkzm_m,xkzm_h,xkzm_s,psautco,prautco,evpco,wminco,          !
!           pdfcld,shcnvcw,flipv,cnvgwd,shal_cnv,                       !
!           redrag,hybedmf,dspheat,cal_pre,                             !
!           mom4ice,trans_trac,nst_fcst,fscav,                          !
!           thermodyn_id, sfcpress_id, gen_coord_hybrid, adjtrc,nnp,    !
!       input/outputs:                                                  !
!           hice,fice,tisfc,tsea,tprcp,cv,cvb,cvt,                      !
!           srflag,snwdph,weasd,sncovr,zorl,canopy,                     !
!           ffmm,ffhh,f10m,srunoff,evbsa,evcwa,snohfa,                  !
!           transa,sbsnoa,snowca,soilm,tmpmin,tmpmax,                   !
!           dusfc,dvsfc,dtsfc,dqsfc,totprcp,gflux,                      !
!           dlwsfc,ulwsfc,suntim,runoff,ep,cldwrk,                      !
!           dugwd,dvgwd,psmean,cnvprcp,spfhmin,spfhmax,                 !
!           dt3dt,dq3dt,du3dt,dv3dt,acv,acvb,acvt,                      !
!           slc,smc,stc,upd_mf,dwn_mf,det_mf,phy_f3d,phy_f2d,           !
!           dlwsfc_cc,ulwsfc_cc,dtsfc_cc,swsfc_cc,                      !
!           dusfc_cc, dvsfc_cc, dqsfc_cc,precr_cc,                      !
!           xt,xs,xu,xv,xz,zm,xtts,xzts,d_conv,ifd,dt_cool,qrain,       !
!       outputs:                                                        !
!           gt0,gq0,gu0,gv0,t2m,q2m,u10m,v10m,                          !
!           zlvl,psurf,hpbl,pwat,t1,q1,u1,v1,                           !
!           chh,cmm,dlwsfci,ulwsfci,dswsfci,uswsfci,                    !
!           dtsfci,dqsfci,gfluxi,epi,smcwlt2,smcref2,                   !
!           xmu_cc,dlw_cc,dsw_cc,snw_cc,lprec_cc,                       !
!           tref, z_c, c_0, c_d, w_0, w_d, rqtk )                       !
!                                                                       !
!  subprograms called:                                                  !
!                                                                       !
!     get_prs,  dcyc2t2_pre_rad (testing),    dcyc2t3,  sfc_diff,       !
!     sfc_ocean,sfc_drv,  sfc_land, sfc_sice, sfc_diag, moninp1,        !
!     moninp,   moninq1,  moninq,   gwdps,    ozphys,   get_phi,        !
!     sascnv,   sascnvn,  rascnv,   gwdc,                               !
!     shalcnv,  cnvc90,   lrgscl,   gsmdrive, gscond,   precpd,         !
!     progt2.                                                           !
!                                                                       !
!                                                                       !
!  program history log:                                                 !
!           19xx  - ncep mrf/gfs                                        !
!           2002  - s. moorthi  modify and restructure and add ferrier  !
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
!           2009    modern form and changed all the inputs to mks units.!
!      feb  2009  - s. moorthi  upgraded to add hochun's gocart changes !
!      jul  2009  - s. moorthi  added rqtk for sela's semi-lagrangian   !
!      aug  2009  - s. moorthi  added j. han and h. pan updated shallow !
!                               convection package                      !
!      sep  2009  - s. moorthi  updated for the mcica (rrtm3) radiation !
!      jun  2011  - s. moorthi and xu li - updated the nst model        !
!      jul  2013  - r. sun     added pdf cloud                          !
!      jul  2013  - j. han     added reduced drag coef. over sea        !
!      jan  2014  - h.juang and f.yang added energy conversion from gwd !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:                                                       size   !
!     ix, im   - integer, horiz dimention and num of used pts      1    !
!     levs     - integer, vertical layer dimension                 1    !
!     lsoil    - integer, number of soil layers                    1    !
!     ntrac    - integer, number of tracers                        1    !
!     ncld     - integer, number of cloud species                  1    !
!     ntoz     - integer, ozone location in the tracer array       1    !
!     ntcw     - integer, cloud condensate location in the tracer  1    !
!                         array                                    1    !
!     nmtvr    - integer, number of topographic variables such as  1    !
!                         variance etc used in the gwd parameterization !
!     nrcm     - integer, second dimension for the random number   1    !
!                         array rann                                    !
!     ko3      - integer, number of layers for ozone data          1    !
!     lonf,latg- integer, number of lon/lat points                 1    !
!     jcap     - integer, number of spectral wave trancation       1    !
!                         used only by sascnv shalcnv                   !
!     num_p3d  - integer, number of 3d arrays needed for           1    !
!                          microphysics                                 !
!     num_p2d  - integer, number of 2d arrays needed for           1    !
!                         microphysics                                  !
!     kdt       -integer, number of the current time step          1    !
!     lat       -integer, latitude index - used for debug prints   1    !
!     me        -integer, pe number - used for debug prints        1    !
!     pl_coeff - integer, number coefficients in ozone forcing     1    !
!     nlons    - integer, number of total grid points in a latitude     !
!                         circle through a point                   im   !
!     ncw      - integer, range of droplet number concentrations for    !
!                         ferrier microphysics                     2    !
!     flgmin   - real, range of  minimum large ice fraction for         !
!                         ferrier microphys                        2    !
!     crtrh    - real, critical relative humidity at the surface, pbl   !
!                      top and at the top of the atmosphere        3    !
!     cdmbgwd  - real, multiplication factors for cdmb and gwd     2    !
!     ccwf     - real, multiplication factor for critical cloud         !
!                      workfunction for ras                        2    !
!     dlqf     - real, factor for cloud condensate detrainment from     !
!                      cloud edges (ras)                           2    !
!     clstp    - real, index used by cnvc90 (for convective clouds)1    !
!                      legacy stuff - does not affect forecast          !
!     cgwf     - real, multiplication factor for convective gwd    2    !
!     dtp      - real, physics time step in seconds                1    !
!     dtf      - real, dynamics time step in seconds               1    !
!     fhour    - real, forecast hour                               1    !
!     solhr    - real, fcst hour at the end of prev time step      1    !
!     slag     - real, equation of time ( radian )                 1    !
!     sdec,cdec- real, sin and cos of the solar declination angle  1    !
!     sinlat   - real, sin of latitude                             im   !
!     coslat   - real, cos of latitude                             im   !
!     pgr      - real, surface pressure (pa)                       im   !
!     ugrs,vgrs- real, u/v component of layer wind              ix,levs !
!     tgrs     - real, layer mean temperature ( k )             ix,levs !
!     qgrs     - real, layer mean tracer concentration     ix,levs,ntrac!
!     vvel     - real, layer mean vertical velocity (pa/s)      ix,levs !
!     prsi     - real, pressure at layer interfaces             ix,levs+1
!     prsl     - real, mean layer presure                       ix,levs !
!     prsik    - real, exner function at layer interface        ix,levs+1
!     prslk    - real, exner function at layer                  ix,levs !
!     phii     - real, interface geopotential height            ix,levs+1
!     phil     - real, layer geopotential height                ix,levs !
!     rann     - real, random number array (0-1)                ix,nrcm !
!     prdout   - real, ozone forcing data                       ix,ko3,pl_coeff!
!     poz      - real, ozone forcing data level pressure (ln(pa))  ko3  !
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
!     swh      - real, total sky sw heating rates ( k/s )       ix,levs !
!     swhc     - real, clear sky sw heating rates ( k/s )       ix,levs !
!     hlw      - real, total sky lw heating rates ( k/s )       ix,levs !
!     hlwc     - real, clear sky lw heating rates ( k/s )       ix,levs !
!     ras      - logical, flag for ras convection scheme           1    !
!     pre_rad  - logical, flag for testing purpose                 1    !
!     ldiag3d  - logical, flag for 3d diagnostic fields            1    !
!     lssav    - logical, flag controls data store and output      1    !
!     lssav_cc - logical, flag for save data for ocean coupling    1    !
!     flipv    - logical, flag for vertical direction flip (ras)   1    !
!     xkzm_m   - real, background vertical diffusion for momentum  1    !
!     xkzm_h   - real, background vertical diffusion for heat, q   1    !
!     xkzm_s   - real, sigma threshold for background mom. diffusn 1    !
!     psautco  - real, auto conversion coeff from ice to snow      2    !
!     prautco  - real, auto conversion coeff from cloud to rain    2    !
!     evpco    - real, coeff for evaporation of largescale rain    1    !
!     wminco   - real, water and ice minimum threshold for zhao    1    !
!     pdfcld   - logical, flag for pdfcld                          1    !
!     shcnvcw  - logical, flag for shallow convective cloud        1    !
!     sup      - real, supsaturation for ho. nucleation of ice     1    !
!     cnvgwd   - logical, flag for conv gravity wave drag          1    !
!     shal_cnv - logical, flag for calling shallow convection      1    !
!     redrag   - logical, flag for reduced drag coeff. over sea    1    !
!     hybedmf  - logical, flag for hybrid edmf pbl scheme          1    !
!     dspheat  - logical, flag for tke dissipative heating         1    !
!     cal_pre  - logical, flag controls precip type algorithm      1    !
!     mom4ice  - logical, flag controls mom4 sea-ice               1    !
!     trans_trac-logical, flag for convective transport of tracers 1    !
!     nst_fcst  -integer, flag 0 for no nst, 1 for uncoupled nst        !
!                          and 2 for coupled nst                   1    !
!     fscav    - real, tracer convective scavenging coefficient ntrac-ncld-1!
!     thermodyn_id - integer, valid for gfs only for get_prs/phi   1    !
!     sfcpress_id  - integer, valid for gfs only for get_prs/phi   1    !
!     gen_coord_hybrid - logical for henry's gen coord             1    !
!     adjtrc       - real, dynamics adjustments to tracers       ntrac  !
!     nnp      - integer, physics substep number                   1    !
!                                                                       !
!  input/outputs:                                                       !
!     hice     - real, sea-ice thickness                           im   !
!     fice     - real, sea-ice concentration                       im   !
!     tisfc    - real, sea-ice temperature                         im   !
!     tsea     - real, ground surface temperature ( k )            im   !
!     tprcp    - real, total precipitation                         im   !
!     the following three variables do not affect the forecast          !
!     cv,       -real, convective clouds amountt                   im   !
!     cvb       -real, convective clouds base pressure (kpa)       im   !
!     cvt       -real, convective clouds top  pressure (kpa)       im   !
!     srflag   - real, snow/rain flag for precipitation            im   !
!     snwdph   - real, actual snow depth (mm) over land/sea ice    im   !
!     weasd    - real, water equiv of accumulated  snow depth (kg/m**2)
!                      over land and sea ice                       im   !
!     sncovr   - real, snow cover over land                        im   !
!     zorl     - real, surface roughness (cm)                      im   !
!     canopy   - real, canopy water                                im   !
!     ffmm     - real, fm parameter from pbl scheme                im   !
!     ffhh     - real, fh parameter from pbl scheme                im   !
!     f10m     - real, fm at 10m                                   im   !
!     srunoff  - real, surface water runoff (from lsm)             im   !
!     evbsa    - real, noah lsm diagnostics                        im   !
!     evcwa    - real, noah lsm diagnostics                        im   !
!     snohfa   - real, noah lsm diagnostics                        im   !
!     transa   - real, noah lsm diagnostics                        im   !
!     sbsnoa   - real, noah lsm diagnostics                        im   !
!     snowca   - real, noah lsm diagnostics                        im   !
!     soilm    - real, soil moisture                               im   !
!     tmpmin   - real, min temperature at 2m height (k)            im   !
!     tmpmax   - real, max temperature at 2m height (k)            im   !
!     dusfc    - real, u component of surface stress               im   !
!     dvsfc    - real, v component of surface stress               im   !
!     dtsfc    - real, sensible heat flux (w/m2)                   im   !
!     dqsfc    - real, latent heat flux (w/m2)                     im   !
!     totprcp  - real, accumulated total precipitation (kg/m2)     im   !
!     gflux    - real, groud conductive heat flux                  im   !
!     dlwsfc   - real, time accumulated sfc dn lw flux ( w/m**2 )  im   !
!     ulwsfc   - real, time accumulated sfc up lw flux ( w/m**2 )  im   !
!     suntim   - real, sunshine duration time (s)                  im   !
!     runoff   - real, total water runoff                          im   !
!     ep       - real, potential evaporation                       im   !
!     cldwrk   - real, cloud workfunction (valid only with sas)    im   !
!     dugwd    - real, vertically integrated u change by ogwd      im   !
!     dvgwd    - real, vertically integrated v change by ogwd      im   !
!     psmean   - real, surface pressure (kpa)                      im   !
!     cnvprcp  - real, accumulated convective precipitation (kg/m2)im   !
!     spfhmin  - real, minimum specific humidity                   im   !
!     spfhmax  - real, maximum specific humidity                   im   !
!     dt3dt    - real, temperature change due to physics           ix,levs,6 !
!     dq3dt    - real, moisture change due to physics              ix,levs,5+pl_coeff!
!     du3dt    - real, u momentum change due to physics            ix,levs,4 !
!     dv3dt    - real, v momentum change due to physics            ix,levs,4 !
!     acv      - real,  array containing accumulated convective clouds im   !
!     acvb,acvt- real,  arrays used by cnvc90                      im   !
!     slc      - real, liquid soil moisture                     ix,lsoil!
!     smc      - real, total soil moisture                      ix,lsoil!
!     stc      - real, soil temperature                         ix,lsoil!
!     upd_mf   - real, convective updraft mass flux             ix,levs !
!     dwn_mf   - real, convective downdraft mass flux           ix,levs !
!     det_mf   - real, convective detrainment mass flux         ix,levs !
!     phy_f3d  - real, 3d arrays saved for restart              ix,levs,num_p3d!
!     phy_f2d  - real, 2d arrays save for restart               ix,num_p2d!
!     dlwsfc_cc- real, sfc dnwd lw flux (w/m**2) for ocn coupling  im   !
!     ulwsfc_cc- real, sfc upwd lw flux (w/m**2) for ocn coupling  im   !
!     swsfc_cc - real, sfc net sw  flux (w/m**2) for ocn coupling  im   !
!     dusfc_cc - real, sfc u-wind                for ocn coupling  im   !
!     dvsfc_cc - real, sfc v-wind                for ocn coupling  im   !
!     dtsfc_cc - real, sfc sensible heat flux    for ocn coupling  im   !
!     dqsfc_cc - real, sfc moisture flux (evap)  for ocn coupling  im   !
!     precr_cc - real, total precipitation       for ocn coupling  im   !
!
!     xt       - real, heat content in dtl                         im   !
!     xs       - real, salinity  content in dtl                    im   !
!     xu       - real, u-current content in dtl                    im   !
!     xv       - real, v-current content in dtl                    im   !
!     xz       - real, dtl thickness                               im   !
!     zm       - real, mxl thickness                               im   !
!     xtts     - real, d(xt)/d(ts)                                 im   !
!     xzts     - real, d(xz)/d(ts)                                 im   !
!     d_conv   - real, thickness of free convection layer (fcl)    im   !
!     ifd      - real, index to start dtm run or not               im   !
!     dt_cool  - real, sub-layer cooling amount                    im   !
!     qrain    - real, sensible heat flux due to rainfall (watts)  im   !
!                                                                       !
!  outputs:                                                             !
!     gt0      - real, updated temperature                        ix,levs !
!     gq0      - real, updated tracers                            ix,levs,ntrac!
!     gu0      - real, updated zonal wind                         ix,levs !
!     gv0      - real, update meridional wind                     ix,levs !
!     t2m,q2m  - real, 2 meter temperature and humidity            im   !
!     u10m,v10m- real, 10 meter u/v wind speed                     im   !
!     zlvl     - real, layer 1 height (m)                          im   !
!     psurf    - real, surface pressure (pa)                       im   !
!     hpbl     - real, pbl height (m)                              im   !
!     pwat     - real, precipitable water                          im   !
!     t1       - real, layer 1 temperature (k)                     im   !
!     q1       - real, layer 1 specific humidity (kg/kg)           im   !
!     u1       - real, layer 1 zonal wind (m/s)                    im   !
!     v1       - real, layer 1 merdional wind (m/s)                im   !
!     chh      - real, thermal exchange coefficient                im   !
!     cmm      - real, momentum exchange coefficient               im   !
!     dlwsfci  - real, instantaneous sfc dnwd lw flux ( w/m**2 )   im   !
!     ulwsfci  - real, instantaneous sfc upwd lw flux ( w/m**2 )   im   !
!     dswsfci  - real, instantaneous sfc dnwd sw flux ( w/m**2 )   im   !
!     uswsfci  - real, instantaneous sfc upwd sw flux ( w/m**2 )   im   !
!     dtsfci   - real, instantaneous sfc sensible heat flux        im   !
!     dqsfci   - real, instantaneous sfc latent heat flux          im   !
!     gfluxi   - real, instantaneous sfc ground heat flux          im   !
!     epi      - real, instantaneous sfc potential evaporation     im   !
!     smcwlt2  - real, wilting point (volumetric)                  im   !
!     smcref2  - real, soil moisture threshold (volumetric)        im   !
!
!     sr       - real                                              im   !
!   
!     xmu_cc   - real, cosine of zenith angle at time step         im   !
!     dlw_cc   - real, sfc dnwd lw flux at time step for ocn cpl   im   !
!     dsw_cc   - real, sfc dnwd sw flux at time step for ocn cpl   im   !
!     snw_cc   - real, lower atms snow fall rate for ocn cpl       im   !
!     lprec_cc - real, lower atms rain fall rate for ocn cpl       im   !

!     tref     - real, reference temperature                       im   !
!     z_c      - real, sub-layer cooling thickness                 im   !
!     c_0      - real, coefficient1 to calculate d(tz)/d(ts)       im   !
!     c_d      - real, coefficient2 to calculate d(tz)/d(ts)       im   !
!     w_0      - real, coefficient3 to calculate d(tz)/d(ts)       im   !
!     w_d      - real, coefficient4 to calculate d(tz)/d(ts)       im   !
!     rqtk     - real, mass change due to moisture variation       im   !
!     dtdtr    - real, temperature change due to radiative heating      !
!                      per time step (k)                           ix,levs   !
!                                                                       !
!                                                                       !
!  ====================    end of description    =====================  !

      subroutine gbphys                                                 &
!  ---  inputs:
     &    ( im,ix,levs,lsoil,ntrac,ncld,ntoz,ntcw,                      &
     &      nmtvr,nrcm,ko3,lonf,latg,jcap,num_p3d,num_p2d,              &
     &      kdt,lat,me,pl_coeff,nlons,ncw,flgmin,crtrh,cdmbgwd,         &
     &      ccwf,dlqf,clstp,cgwf,dtp,dtf,fhour,solhr,                   &
     &      slag,sdec,cdec,sinlat,coslat,pgr,ugrs,vgrs,                 &
     &      tgrs,qgrs,vvel,prsi,prsl,prslk,prsik,phii,phil,             &
     &      rann,prdout,poz,dpshc,hprime,xlon,xlat,                     &
     &      slope,shdmin,shdmax,snoalb,tg3,slmsk,vfrac,                 &
     &      vtype,stype,uustar,oro,oro_uf,coszen,sfcdsw,sfcnsw,         &
     &      sfcdlw,tsflw,sfcemis,sfalb,swh,swhc,hlw,hlwc,ras,pre_rad,   &
     &      ldiag3d,lssav,lssav_cc,                                     &
     &      xkzm_m,xkzm_h,xkzm_s,psautco,prautco,evpco,wminco,sup,      &
     &      pdfcld,shcnvcw,flipv,cnvgwd,shal_cnv,                       &
     &      redrag,hybedmf,dspheat,cal_pre,                             &
     &      mom4ice,trans_trac,nst_fcst,fscav,                          &
     &      thermodyn_id, sfcpress_id, gen_coord_hybrid, adjtrc,nnp,    &
!  ---  input/outputs:
     &      hice,fice,tisfc,tsea,tprcp,cv,cvb,cvt,                      &
     &      srflag,snwdph,weasd,sncovr,zorl,canopy,                     &
     &      ffmm,ffhh,f10m,srunoff,evbsa,evcwa,snohfa,                  &
     &      transa,sbsnoa,snowca,soilm,tmpmin,tmpmax,                   &
     &      dusfc,dvsfc,dtsfc,dqsfc,totprcp,gflux,                      &
     &      dlwsfc,ulwsfc,suntim,runoff,ep,cldwrk,                      &
     &      dugwd,dvgwd,psmean,cnvprcp,spfhmin,spfhmax,                 &
     &      dt3dt,dq3dt,du3dt,dv3dt,acv,acvb,acvt,                      &
     &      slc,smc,stc,upd_mf,dwn_mf,det_mf,phy_f3d,phy_f2d,           &
     &      dlwsfc_cc,ulwsfc_cc,dtsfc_cc,swsfc_cc,                      &
     &      dusfc_cc, dvsfc_cc, dqsfc_cc,precr_cc,                      &
     &      xt,xs,xu,xv,xz,zm,xtts,xzts,d_conv,ifd,dt_cool,qrain,       &
!  ---  outputs:
     &      gt0,gq0,gu0,gv0,t2m,q2m,u10m,v10m,                          &
     &      zlvl,psurf,hpbl,pwat,t1,q1,u1,v1,                           &
     &      chh,cmm,dlwsfci,ulwsfci,dswsfci,uswsfci,                    &
     &      dtsfci,dqsfci,gfluxi,epi,smcwlt2,smcref2,                   &
     &      sr,                                                         &
     &      xmu_cc,dlw_cc,dsw_cc,snw_cc,lprec_cc,                       &
     &      tref, z_c, c_0, c_d, w_0, w_d,                              &
     &      rqtk,dtdtr                                                  &
     &      )

!
      use machine ,   only : kind_phys
      use physcons,   only : con_cp, con_fvirt, con_g, con_rd, con_rv,  &
     &                       con_hvap, con_hfus, con_rerth, con_pi
      use namelist_def, only : n3dzhaocld, n3dfercld, n3dcldpdf,        &
     &   n3dflxtvd, n2dzhaocld, n2dfercld, n2dcldpdf, n2dflxtvd
!
      implicit none
!

!  ---  constant parameters:
!     real(kind=kind_phys), parameter :: fhourpr = 0.0
      real(kind=kind_phys), parameter :: rlapse  = 0.65e-2
      real(kind=kind_phys), parameter :: rhc_max = 0.9999
!     real(kind=kind_phys), parameter :: qmin    = 1.0e-10
      real(kind=kind_phys), parameter :: p850    = 85000.0
      real(kind=kind_phys), parameter :: epsq    = 1.e-20
!     real(kind=kind_phys), parameter :: cb2mb   = 10.0
      real(kind=kind_phys), parameter :: pa2mb   = 0.01
      real(kind=kind_phys), parameter :: czmin   = 0.0001      ! cos(89.994)

      real(kind=kind_phys), parameter ::                                &
!    & dxmax=ln(1.0/(5000.0*2500.0)),  dxmin=ln(1.0/(192.0*94.0)
!    & dxmax=-16.118095651,dxmin=-9.800790154,dxinv=1.0/(dxmax-dxmin)
     & dxmaxs=-16.118095651,dxmins=-9.800790154,                        &
     & dxinvs=1.0/(dxmaxs-dxmins) 
!    & dxmax=ln(1.0/(2000.0*1000.0)),  dxmin=ln(1.0/(192.0*94.0)
!    & dxmaxr=-14.5086577385,dxminr=-9.800790154,                       &
!    & dxinvr=1.0/(dxmaxr-dxminr)
!    & dxmax=ln(1.0/(2500.0*1250.0)),  dxmin=ln(1.0/(192.0*94.0)
!    & dxmax=-14.95494484115,dxmin=-9.800790154, dxinv=1.0/(dxmax-dxmin)
       real(kind=kind_phys) dxmax, dxmin, dxinv

!  ---  inputs:
      integer, intent(in) :: ix,   im,   levs, lsoil,   ntrac,          &
     &                       ncld, ntoz, ntcw, nmtvr,   nrcm,    ko3,   &
     &                       lonf, latg, jcap, num_p3d, num_p2d, kdt,   &
     &                       me,   pl_coeff, lat,                       &
     &                       thermodyn_id, sfcpress_id, nnp


      integer, intent(in) :: nlons(im), ncw(2), nst_fcst

      logical, intent(in) :: ras,        pre_rad,   ldiag3d, flipv,     &
     &                       cnvgwd,                                    &
     &                       redrag,     hybedmf,   dspheat,            &
     &                       lssav,      lssav_cc,  mom4ice,            &
     &                       trans_trac, cal_pre,                       &
     &                       shal_cnv, gen_coord_hybrid

      real(kind=kind_phys) :: adjtrc(ntrac)

      real(kind=kind_phys), dimension(im),            intent(in) ::     &
     &      sinlat, coslat, pgr,    dpshc,  xlon,   xlat,               &
     &      slope,  shdmin, shdmax, snoalb, tg3,    slmsk,  vfrac,      &
     &      vtype,  stype,  uustar, oro,    coszen, sfcnsw, sfcdsw,     &
     &      sfcdlw, tsflw,  sfalb,  sfcemis, oro_uf

      real(kind=kind_phys), dimension(ix,levs),       intent(in) ::     &
     &      ugrs, vgrs, tgrs, vvel, prsl, prslk, phil, swh,swhc,hlw,hlwc

      real(kind=kind_phys), intent(inout) ::  qgrs(ix,levs,ntrac)

      real(kind=kind_phys), dimension(ix,levs+1),     intent(in) ::     &
     &      prsi, prsik, phii

      real(kind=kind_phys), intent(in) ::  hprime(ix,nmtvr),            &
     &      prdout(ix,ko3,pl_coeff),       rann(ix,nrcm), poz(ko3)

      real(kind=kind_phys), intent(in) ::  dtp,     dtf, fhour, solhr,  &
     &      slag,    sdec,     cdec,       clstp,                       &
     &      ccwf(2), crtrh(3), flgmin(2),  dlqf(2), cdmbgwd(2),         &
     &      xkzm_m,  xkzm_h, xkzm_s, psautco(2),   prautco(2), evpco,   &
     &      wminco(2), sup, cgwf(2)
      logical, intent(in) :: pdfcld,shcnvcw

      real(kind=kind_phys), intent(in) ::  fscav(ntrac-ncld-1)


!  ---  input/output:
      real(kind=kind_phys), dimension(im),            intent(inout) ::  &
     &      hice,   fice,    tisfc,  tsea,   tprcp,  cv,     cvb,  cvt, &
     &      srflag, snwdph,  weasd, sncovr, zorl,   canopy, ffmm, ffhh, &
     &      f10m,   srunoff, evbsa,  evcwa,  snohfa, transa, sbsnoa,    &
     &      snowca, soilm,   tmpmin, tmpmax, dusfc,  dvsfc,  dtsfc,     &
     &      dqsfc,  totprcp, gflux,  dlwsfc, ulwsfc, suntim, runoff, ep,&
     &      cldwrk, dugwd,   dvgwd,  psmean, cnvprcp,spfhmin, spfhmax,  &
     &      acv,    acvb,    acvt,                                      &
! om coupling
     &      dlwsfc_cc, ulwsfc_cc, dtsfc_cc, swsfc_cc, dusfc_cc,         &
     &      dvsfc_cc,  dqsfc_cc,  precr_cc,                             &
! nst
     &      xt, xs, xu, xv, xz, zm, xtts, xzts, d_conv, ifd, dt_cool,   &
     &      qrain
!
      real(kind=kind_phys), dimension(ix,lsoil),      intent(inout) ::  &
     &      smc, stc, slc

      real(kind=kind_phys), dimension(ix,levs),       intent(inout) ::  &
     &      upd_mf, dwn_mf, det_mf

      real(kind=kind_phys),                           intent(inout) ::  &
     &      phy_f3d(ix,levs,num_p3d), phy_f2d(ix,num_p2d),              &
     &      dt3dt(ix,levs,6), du3dt(ix,levs,4), dv3dt(ix,levs,4),       &
     &      dq3dt(ix,levs,5+pl_coeff)

!  ---  output:
      real(kind=kind_phys), dimension(im),            intent(out) ::    &
     &      t2m,     q2m,     u10m,    v10m,    zlvl,   psurf, hpbl,    &
     &      pwat,    t1,      q1,      u1,      v1,     chh,   cmm,     &
     &      dlwsfci, ulwsfci, dswsfci, uswsfci, dtsfci, dqsfci,         &
     &      gfluxi,  epi,     smcwlt2, smcref2,                         &
     &      xmu_cc,  dlw_cc,  dsw_cc,  snw_cc,  lprec_cc,               &
     &      tref,    z_c,     c_0,     c_d,     w_0,   w_d, rqtk, sr    &

      real(kind=kind_phys), dimension(ix,levs),       intent(out) ::    &
     &      gt0, gu0, gv0

      real(kind=kind_phys), dimension(ix,levs,ntrac), intent(out) ::    &
     &      gq0

!  ---  local:
      real(kind=kind_phys), dimension(im)          :: ccwfac, garea,    &
     &      dlength, xncw,   cumabs,         cice,    zice,   tice,     &
!    &      dlength, xncw,   cumabs, qmax,   cice,    zice,   tice,     &
!    &      gflx,    rain,   rainc,  rainl,  rain1,   raincs, evapc,    &
     &      gflx,    rain,   rainc,  rainl,  rain1,   raincs,           &
     &      snowmt,  cd,     cdq,    qss,    dusfcg,  dvsfcg, dusfc1,   &
     &      dvsfc1,  dtsfc1, dqsfc1, rb,     rhscnpy, drain,  cld1d,    &
     &      evap,    hflx,   stress, t850,   ep1d,    gamt,   gamq,     &
     &      sigmaf,                  oc,     theta,   gamma,  sigma,    &
     &      elvmax,  wind,   work1,  work2,  runof,   xmu,              &
     &      fm10,    fh2,    tsurf,  tx1,    tx2,     flgmin_l,         &
     &      evbs,    evcw,   trans,  sbsno,  snowc,   adjsfcdsw,        &
     &      adjsfcnsw, adjsfcdlw, adjsfculw, asfcdlw, asfculw, gsfcdlw, &
     &      gsfculw, xcosz,  tseal,  snohf,  dlqfac,  work3,            &
     &      domr,    domzr,  domip,  doms,   psautco_l, prautco_l,      &
     &      asr, cldf

!    &      dswsfc, radsl,                                              &
!    &      dlwsf1,  ulwsf1, xcosz,  tseal,  snohf,   dlqfac,           &
!    &      domr,    domzr,  domip,  doms


!     real(kind=kind_phys), dimension(ix,levs)     :: ud_mf, dd_mf,     &
!    &      dt_mf, del
      real(kind=kind_phys), dimension(ix,levs)     :: del, dtdtr
      real(kind=kind_phys), dimension(im,levs-1)   :: dkt

      real(kind=kind_phys), dimension(im,levs)     :: rhc, dtdt,        &
     &      dudt, dvdt, gwdcu, gwdcv, dtdtc,                            &
!    &      dudt, dvdt, gwdcu, gwdcv,                 cuhr,             &
!    &      dudt, dvdt, gwdcu, gwdcv, diagn1, diagn2, cuhr, cumchr,     &
     &      qr_col, fc_ice, rainp, ud_mf, dd_mf, dt_mf

      real(kind=kind_phys), dimension(im,lsoil)    :: smsoil, stsoil,   &
     &      ai, bi, cci, rhsmc, zsoil, slsoil

      real(kind=kind_phys) :: rhbbot, rhbtop, rhpbl, frain, f_rain,     &
     &      f_ice, qi, qw, qr, wc, tem, tem1, tem2,  sume,  sumr, sumq, &
     &      dqdt(im,levs,ntrac), oa4(im,4), clx(im,4)

!           in clw, the first two varaibles are cloud water and ice. 
!           from third to ntrac are convective transportable tracers,
!           third being the ozone, when ntrac=3 (valid only with ras)

      real(kind=kind_phys), allocatable :: clw(:,:,:)

      integer, dimension(im) :: kbot, ktop, kcnv, soiltyp, vegtype,     &
     &          kpbl, slopetyp, kinver, lmh, islmsk

      integer :: i, nvdiff, kk, ic, k, n, ipr, lv, k1, iter,            &
     &           tracers, trc_shft, tottracer

      logical, dimension(im) :: flag_iter, flag_guess, invrsn

      logical :: lprnt

      real(kind=kind_phys), allocatable :: cnvc(:,:),cnvw(:,:)
      real(kind=kind_phys) eng0, eng1, hsub

!
!===> ...  begin here
!
!     if (ras) then
!       dxmax = dxmaxr ; dxmin = dxminr ; dxinv = dxinvr
!     else
!       dxmax = dxmaxs ; dxmin = dxmins ; dxinv = dxinvs
!     endif
        dxmax = dxmaxs ; dxmin = dxmins ; dxinv = dxinvs
 
!  --- ...  set up check print point (for debugging)
!
!*************************************************************************
!     lprnt = .true.
      lprnt = .false.
!     lprnt = me == 0

      ipr = 1
!     lprnt = kdt .gt. 0
!     do i = 1, im
!       work1(1) = xlon(i) * 57.29578
!       if (work1(1) >= 180.0) work1(1) = work1(1) - 360.0
!       work2(1) = xlat(i) * 57.29578
!       print *,' me=',me,' work1=',work1(1),' work2=',work2(1),' i=',i
!       lprnt = kdt > 4320
!       lprnt = kdt > 0 .and. abs(work1(1)-110.3) < 0.5                 &
!    &                  .and. abs(work2(1)-2.0)   < 0.5
!       lprnt = kdt >= 14 .and. lat == 43 
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-143.182) < 0.101    &
!    &                   .and. abs(xlat(i)*57.29578-6.235)  < 0.101
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-135.0) < 0.201      &
!    &                   .and. abs(xlat(i)*57.29578-10.476)  < 0.201
!       lprnt = kdt >= 0 .and. abs(xlon(i)*57.29578-110.3) < 0.201      &
!    &                   .and. abs(xlat(i)*57.29578-2.0)   < 0.201
!       print *,' i=',i,' xlon=',xlon(i)*57.29578,                      &
!    &                  ' xlat=',xlat(i)*57.29578,' i=',i,' me=',me
!       if (lprnt) then
!         ipr = i
!         exit
!       endif
!     enddo

!     lprnt = .false.
!     if(lprnt) then
!       print *,' im=',im,' ix=',ix,' levs=',levs,' lsoil=',lsoil,      &
!    &   ' ntrac=',ntrac,' ntoz=',ntoz,' ntcw=',ntcw,' me=',me,         &
!    &   ' xlat=',xlat(ipr),' kdt=',kdt,' slmsk=',slmsk(ipr),           &
!    &   ' tsea=',tsea(ipr),' tref=',tref(ipr),' dt_cool=',dt_cool(ipr),&
!    &   ' dt_warm=',2.0*xt(ipr)/xz(ipr),' nrcm=',nrcm,' xlon=',
!    &    xlon(ipr),                                                    &
!    &   ' dt_warm=',dt_warm(ipr),' nrcm=',nrcm,' xlon=',xlon(ipr),     &
!    &   ' sfalb=',sfalb(ipr),' kdt=',kdt
!       print *,' pgr=',pgr(ipr),' kdt=',kdt,' ipr=',ipr
!       print *,' ipr=',ipr,' phy_f2d=',phy_f2d(ipr,1:num_p2d)
!       print *,' ugrs=',ugrs(ipr,:)
!       print *,' vgrs=',vgrs(ipr,:)
!       print *,' tgrs=',tgrs(ipr,:),' kdt=',kdt,' ipr=',ipr
!    &,  ' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!       print *,' qgrs=',qgrs(ipr,:,1)
!       print *,' ozg=',qgrs(ipr,:,2)
!       print *,' clw=',qgrs(ipr,:,3)
!    &,  ' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!     endif
!
!*************************************************************************
!
      nvdiff = ntrac    ! vertical diffusion of all tracers!
!
!  --- ...  figure out how many extra tracers are there
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

!     if (lprnt) print *,' trans_trac=',trans_trac,' tottracer=',       &
!    &                   tottracer,' trc_shft=',trc_shft,' kdt=',kdt

      allocate ( clw(ix,levs,tottracer+2) )
      allocate ( cnvc(ix,levs))
      allocate ( cnvw(ix,levs))
!
      if (nnp == 1) then
        do n=1,ntrac
!       write(0,*)' in gbphys adjtrc=',adjtrc(n),' for tracer=',n
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
      call get_prs(im,ix,levs,ntrac,tgrs,qgrs,                          &
     &             thermodyn_id, sfcpress_id,                           &
     &             gen_coord_hybrid,                                    &
     &             prsi,prsik,prsl,prslk,phii,phil,del)
!
!     if (lprnt) then
!       print *,' prsi=',prsi(ipr,:)
!       print *,' prsl=',prsl(ipr,:)
!       print *,' del=',del(ipr,:)
!     endif
!
      rhbbot = crtrh(1)
      rhpbl  = crtrh(2)
      rhbtop = crtrh(3)

!  --- ...  frain=factor for centered difference scheme correction of rain amount.

      frain = dtf / dtp


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

      do i = 1, im
        sigmaf(i)   = max( vfrac(i),0.01 )
!       sigmaf(i)   = max( vfrac(i),0.3 )


        islmsk(i)=nint(slmsk(i))
        if (islmsk(i) == 2) then
          soiltyp(i)  = 9
          vegtype(i)  = 13
          slopetyp(i) = 9                    !! clu: qa(slopetyp)
        else
          soiltyp(i)  = int( stype(i)+0.5 )
          vegtype(i)  = int( vtype(i)+0.5 )
          slopetyp(i) = int( slope(i)+0.5 )    !! clu: slope -> slopetyp
        endif
        zice(i)     = hice(i)
        cice(i)     = fice(i)
        tice(i)     = tisfc(i)
        work1(i)    = (log(coslat(i) / (nlons(i)*latg)) - dxmin) * dxinv
        work1(i)    = max(0.0, min(1.0,work1(i)))
        work2(i)    = 1.0 - work1(i)
        psurf(i)    = pgr(i)
        work3(i)    = prsik(i,1) / prslk(i,1)
        tem1        = con_rerth * (con_pi+con_pi)*coslat(i)/nlons(i)
        tem2        = con_rerth * con_pi/latg
        garea(i)    = tem1 * tem2
        dlength(i)  = sqrt( tem1*tem1+tem2*tem2 )
        cldf(i)     = cgwf(1)*work1(i) + cgwf(2)*work2(i)
      enddo

      if (lssav) then
        do i = 1, im
          psmean(i) = psmean(i) + pgr(i)*dtf
        enddo
      endif

!  --- ...  initialize dtdt with heating rate from dcyc2 

!     if (lprnt) then
!       do ipr=1,im
!         print *,' before dcyc2: im=',im,' lsoil=',lsoil,' levs=',levs &
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
     &       ix, im, levs,                                              &
!  ---  input/output:
     &       dtdt,                                                      &
!  ---  outputs:
     &       adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz          &
!old vars   ( dswsfc,    -radsl,  dlwsf1,   ulwsf1,  xmu,xcosz )
     &     )

      else

        call dcyc2t3                                                    &
!  ---  inputs:
     &     ( solhr,slag,sdec,cdec,sinlat,coslat,                        &
     &       xlon,coszen,tsea,tgrs(1,1),tsflw,                          &
     &       sfcdsw,sfcnsw,sfcdlw,swh,swhc,hlw,hlwc,                    &
     &       ix, im, levs,                                              &
!  ---  input/output:
     &       dtdt,dtdtc,                                                &
!  ---  outputs:
     &       adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz          &
!old vars   ( dswsfc,    -radsl,  dlwsf1,   ulwsf1,  xmu,xcosz )
     &     )
!
! save temp change due to clear sky radiation - need for sppt stochastic physics
!---------------------------------------------------------------------
        do k=1,levs
          do i=1,im
             dtdtr(i,k) = dtdtr(i,k) + dtdtc(i,k)*dtf
          enddo
        enddo

      endif

!  ---  convert lw fluxes for land/ocean/sea-ice models
!  note: adjsfcdlw and adjsfculw are time-step adjusted lw sfc dn/up fluxes.
!        those fluxes from dcyc2t3 subprogram are not applied with emissivity
!        effect.  one needs to be cautious that, when emis<1, sfc dn/up lw
!        fluxes will expressed differently when looking from atmospheric
!        (as the output from radiation) to those applied for land/ocean/sea-ice
!        model processes. however, the net effects are the same.
!
!   - flux to/from lnd/oc/ice: gsfcdlw=emis*adjsfcdlw; gsfculw=emis*adjsfculw
!   - flux from/to atmosphere: asfcdlw=adjsfcdlw; asfculw=emis*adjsfculw+(1-emis)*adjsfcdlw
        do i = 1, im
          gsfcdlw(i) = sfcemis(i) * adjsfcdlw(i)                  ! for lnd/ocn/sice
          gsfculw(i) = sfcemis(i) * adjsfculw(i)                  ! for lnd/ocn/sice
          asfcdlw(i) = adjsfcdlw(i)                               ! for atmos output
          asfculw(i) = gsfculw(i) + adjsfcdlw(i) - gsfcdlw(i)     ! for atmos output
!org      asfculw(i) = gsfculw(i) + (1.0-sfcemis(i))*adjsfcdlw(i) ! for atmos output
        enddo

!  --- ...  coupling insertion
!  note: all radiation fluxes are positive values.
!       sfc net sw is defined as sfcnsw = sfcdsw - sfcusw (*** positive downward
!       see subr grrad for def)

      if (lssav_cc) then
        do i = 1, im
          xmu_cc(i)    = max( 0.0, min( 1.0, xcosz(i) ))
          dsw_cc(i)    = adjsfcdsw(i)
          dlw_cc(i)    = adjsfcdlw(i)
          dlwsfc_cc(i) = dlwsfc_cc(i) + gsfcdlw(i)
          ulwsfc_cc(i) = ulwsfc_cc(i) + gsfculw(i)
          swsfc_cc(i)  = swsfc_cc(i)  - adjsfcnsw(i) ! swsfc is positive upward
!         sfcems_cc(i) = sfcemis(i)                  ! for ocn mdl if needed
        enddo
      end if

!  --- ...  accumulate/save output variables
     
      if (lssav) then

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
          dlwsfc(i) = dlwsfc(i) + asfcdlw(i)*dtf
          ulwsfc(i) = ulwsfc(i) + asfculw(i)*dtf
        enddo

        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dt3dt(i,k,1) = dt3dt(i,k,1) + hlw(i,k)*dtf
              dt3dt(i,k,2) = dt3dt(i,k,2) + swh(i,k)*dtf*xmu(i)
            enddo
          enddo
        endif

      endif    ! end if_lssav_block

      do i = 1, im
        kcnv(i)   = 0
        kinver(i) = levs
        invrsn(i) = .false.
        tx1(i)    = 0.0
        tx2(i)    = 10.0
      enddo

!  --- ...  check print

!     ipr = 1
!     if (lprnt) then
!       print *,' before progtm: im=',im,' lsoil=',lsoil                &
!    &,         ' nvdiff=',nvdiff,' adjsfcnsw=',adjsfcnsw(ipr)          &
!    &,         ' asfcdlw=',asfcdlw(ipr),' asfculw=',asfculw(ipr)       &
!    &,         ' gsfcdlw=',gsfcdlw(ipr),' gsfculw=',gsfculw(ipr)       &
!    &,         ' sfcemis=',sfcemis(ipr),' tsea2=',tsea(ipr)            &
!    &,         ' ipr=',ipr,' me=',me,' lat=',lat,' xlon=',xlon(ipr)    &
!    &,         ' kdt=',kdt

!       print *,' dtdth=',dtdt(ipr,:),' kdt=',kdt
!     endif

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
        call sfc_diff(im,pgr,ugrs,vgrs,tgrs,qgrs,zlvl,                  &
     &                tsea,zorl,cd,cdq,rb,                              &
     &                prsl(1,1),work3,islmsk,                           &
     &                stress,ffmm,ffhh,                                 &
     +                uustar,wind,phy_f2d(1,1),fm10,fh2,                &
!   this line below needs to commented for pre-helin correction to lm - moorthi
     &                sigmaf,vegtype,shdmax,                            &
     +                tsurf, flag_iter,redrag)

!       if (lprnt) print *,' cdq=',cdq(ipr),' iter=',iter               &
!    &,   ' wind=',wind(ipr),'phy_f2d=',phy_f2d(ipr,1),' ugrs='         &
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

!         if (lprnt) print *,' tseaz1=',tsea(ipr),' tref=',tref(ipr),   &
!    &      ' dt_cool=',dt_cool(ipr),' dt_warm=',2.0*(xt(ipr)/xz(ipr)   &
!    &      ' dt_cool=',dt_cool(ipr),' dt_warm=',dt_warm(ipr)           &
!    &,     ' tgrs=',tgrs(ipr,1),' prsl=',prsl(ipr,1)
!    &,     ' work3=',work3(ipr),' kdt=',kdt

          call sfc_nst                                                  &
     &       ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,tref,cd,cdq,            &
     &         prsl(1,1),work3,islmsk,xlon,sinlat,stress,               &
     &         sfcemis,gsfcdlw,adjsfcnsw,tprcp,dtf,kdt,                 &
     &         phy_f2d(1,1),flag_iter,flag_guess,nst_fcst,              &
     &         lprnt,ipr,                                               &
!  --- input/output
     &         tseal,tsurf,xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,         &
     &         z_c,c_0,c_d,w_0,w_d,d_conv,ifd,qrain,                    &

!    &         tseal, ifd, time_old, time_ins, i_sw, i_q,               &
!    &         i_qrain, i_m, i_tau, i_sw_zw, i_q_ts, i_m_ts, dt_cool,   &
!    &         dt_warm, z_c, z_w, c_0, c_d, w_0, w_d,                   &
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
     &       prsl(1,1),work3,islmsk,phy_f2d(1,1),flag_iter,             &
!  ---  outputs:
     &       qss,cmm,chh,gflx,evap,hflx,ep1d                            &
     &     )

        endif
 
!       if (lprnt) print *,' sfalb=',sfalb(ipr),' ipr=',ipr             &
!    &,   ' weasd=',weasd(ipr),' snwdph=',snwdph(ipr)                   &
!    &,   ' tprcp=',tprcp(ipr),' kdt=',kdt,' iter=',iter
 
!  --- ...  surface energy balance over land
!
                                                     ! noah lsm call

        call sfc_drv                                                    &
!  ---  inputs:
     &     ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,soiltyp,vegtype,sigmaf,   &
     &       sfcemis,gsfcdlw,adjsfcdsw,adjsfcnsw,dtf,tg3,cd,cdq,        &
     &       prsl(1,1),work3,zlvl,islmsk,phy_f2d(1,1),slopetyp,         &
     &       shdmin,shdmax,snoalb,sfalb,flag_iter,flag_guess,           &
!  ---  in/outs:
     &       weasd,snwdph,tsea,tprcp,srflag,smsoil,stsoil,slsoil,       &
     &       canopy,trans,tsurf,                                        &
!  ---  outputs:
     &       sncovr,qss,gflx,drain,evap,hflx,ep1d,runof,                &
     &       cmm,chh,evbs,evcw,sbsno,snowc,soilm,snohf,                 &
     &       smcwlt2,smcref2,zorl                                       &
     &     )


!       if (lprnt) print *,' tseabeficemodel =',tsea(ipr),' me=',me     &
!    &,   ' kdt=',kdt

!  --- ...  surface energy balance over seaice

        call sfc_sice                                                   &
!  ---  inputs:
     &     ( im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,dtf,                      &
     &       sfcemis,gsfcdlw,adjsfcnsw,adjsfcdsw,srflag,                &
     &       cd,cdq,prsl(1,1),work3,islmsk,phy_f2d(1,1),                &
     &       flag_iter,mom4ice,                                         &
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
     &                           .and. nst_fcst > 1) then
            if (wind(i) < 2.0) flag_iter(i) = .true.
          endif
        enddo

      enddo   ! end iter_loop

      do i = 1, im
        epi(i)     = ep1d(i)
        dlwsfci(i) = gsfcdlw(i)
        ulwsfci(i) = gsfculw(i)
        uswsfci(i) = adjsfcdsw(i) - adjsfcnsw(i)
        dswsfci(i) = adjsfcdsw(i)
        gfluxi(i)  = gflx(i)
        t1(i)      = tgrs(i,1)
        q1(i)      = qgrs(i,1,1)
        u1(i)      = ugrs(i,1)
        v1(i)      = vgrs(i,1)
      enddo

!  --- ...  update near surface fields

      call sfc_diag(im,lsoil,pgr,ugrs,vgrs,tgrs,qgrs,                   &
     &              tsea,qss,f10m,u10m,v10m,t2m,q2m,work3,              &
     &              evap,ffmm,ffhh,fm10,fh2)

      do i = 1, im
        phy_f2d(i,1) = 0.0
      enddo

!     if (lprnt) print *,' tseaim=',tsea(ipr),' me=',me,' kdt=',kdt

      if (lssav) then
        do i = 1, im
          gflux(i)   = gflux(i)  + gflx(i)*dtf
          evbsa(i)   = evbsa(i)  + evbs(i)*dtf
          evcwa(i)   = evcwa(i)  + evcw(i)*dtf
          transa(i)  = transa(i) + trans(i)*dtf
          sbsnoa(i)  = sbsnoa(i) + sbsno(i)*dtf
          snowca(i)  = snowca(i) + snowc(i)*dtf
          snohfa(i)  = snohfa(i) + snohf(i)*dtf

          tmpmax(i)  = max(tmpmax(i),t2m(i))
          tmpmin(i)  = min(tmpmin(i),t2m(i))

          spfhmax(i) = max(spfhmax(i),q2m(i))
          spfhmin(i) = min(spfhmin(i),q2m(i))
          ep(i)      = ep(i) + ep1d(i) * dtf
        enddo
      endif

!!!!!!!!!!!!!!!!!commented by moorthi on july 18, 2012 !!!!!!!!!!!!!!!!!!!
!     do i = 1, im

!  --- ...  compute coefficient of evaporation in evapc
!
!       if (evapc(i) > 1.0e0) evapc(i) = 1.0e0

!  --- ...  over snow cover or ice or sea, coef of evap =1.0e0

!       if (weasd(i) > 0.0 .or. islmsk(i) /= 1) evapc(i) = 1.0e0

!     enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  --- ...  vertical diffusion

!     if (lprnt) print *,' tsea3=',tsea(ipr),' islmsk=',islmsk(ipr)     &
!    &, ' kdt=',kdt,' evap=',evap(ipr)
!     if (lprnt)  print *,' dtdtb=',dtdt(ipr,:)


      if (hybedmf) then
         call moninedmf(ix,im,levs,nvdiff,ntcw,dvdt,dudt,dtdt,dqdt,     &
     &     ugrs,vgrs,tgrs,qgrs,swh,hlw,xmu,                             &
     &     prsik(1,1),rb,zorl,u10m,v10m,ffmm,ffhh,                      &
     &     tsea,qss,hflx,evap,stress,wind,kpbl,                         &
     &     prsi,del,prsl,prslk,phii,phil,dtp,dspheat,                   &
     &     dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,              &
     &     kinver, xkzm_m, xkzm_h, xkzm_s, lprnt, ipr)
       else
         call moninq(ix,im,levs,nvdiff,ntcw,dvdt,dudt,dtdt,dqdt,        &
     &     ugrs,vgrs,tgrs,qgrs,swh,hlw,xmu,                             &
     &     prsik(1,1),rb,ffmm,ffhh,                                     &
     &     tsea,qss,hflx,evap,stress,wind,kpbl,                         &
     &     prsi,del,prsl,prslk,phii,phil,dtp,dspheat,                   &
     &     dusfc1,dvsfc1,dtsfc1,dqsfc1,hpbl,gamt,gamq,dkt,              &
     &     kinver, xkzm_m, xkzm_h, xkzm_s, lprnt, ipr)
       endif

!     if (lprnt) then
!       print *,' dusfc1=',dusfc1(ipr)
!       print *,' dtsfc1=',dtsfc1(ipr)
!       print *,' dqsfc1=',dqsfc1(ipr)
!       print *,' dtdt=',dtdt(ipr,:)
!       print *,' dudtm=',dudt(ipr,:)
!     endif

!  --- ...  coupling insertion

      if (lssav_cc) then
        do i=1, im
          dusfc_cc(i) = dusfc_cc(i) + dusfc1(i)        !*dtf <-na h.(?)
          dvsfc_cc(i) = dvsfc_cc(i) + dvsfc1(i)        !*dtf <-na h.(?)
          dtsfc_cc(i) = dtsfc_cc(i) + dtsfc1(i)        !*dtf <-na h.(?)
          dqsfc_cc(i) = dqsfc_cc(i) + dqsfc1(i)        !*dtf <-na h.(?)
        enddo
      endif

      if (lssav) then
        do i = 1, im
          dusfc(i)  = dusfc(i) + dusfc1(i)*dtf
          dvsfc(i)  = dvsfc(i) + dvsfc1(i)*dtf
          dtsfc(i)  = dtsfc(i) + dtsfc1(i)*dtf
          dqsfc(i)  = dqsfc(i) + dqsfc1(i)*dtf
          dtsfci(i) = dtsfc1(i)
          dqsfci(i) = dqsfc1(i)
        enddo

        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              tem  = dtdt(i,k) - (hlw(i,k)+swh(i,k)*xmu(i))
              dt3dt(i,k,3) = dt3dt(i,k,3) + tem*dtf
!             dq3dt(i,k,1) = dq3dt(i,k,1) + dqdt(i,k,1) * dtf
              du3dt(i,k,1) = du3dt(i,k,1) + dudt(i,k)   * dtf
              du3dt(i,k,2) = du3dt(i,k,2) - dudt(i,k)   * dtf
              dv3dt(i,k,1) = dv3dt(i,k,1) + dvdt(i,k)   * dtf
              dv3dt(i,k,2) = dv3dt(i,k,2) - dvdt(i,k)   * dtf
            enddo
          enddo
        endif
        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dq3dt(i,k,1) = dq3dt(i,k,1) + dqdt(i,k,1) * dtf
            enddo
          enddo
          if (ntoz > 0) then
            do k = 1, levs
              do i = 1, im
                dq3dt(i,k,5) = dq3dt(i,k,5) + dqdt(i,k,ntoz)*dtf
              enddo
            enddo
          endif
        endif

      endif   ! end if_lssav

      if (nmtvr == 14) then

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

      else
        oc  = 0.0
        oa4 = 0.0
        clx = 0.0
        theta = 0.0
        gamma = 0.0
        sigma = 0.0
        elvmax = 0.0

        if (nmtvr == 6) then
  
          do i = 1, im
            oc(i) = hprime(i,2)
          enddo
  
          do k = 1, 4
            do i = 1, im
              oa4(i,k) = hprime(i,k+2)
            enddo
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
        end if

      endif   ! end if_nmtvr

      call gwdps(im, ix, im,  levs,  dvdt, dudt, dtdt,                  &
     &           ugrs,   vgrs, tgrs,  qgrs,                             &
     &           kpbl,   prsi, del,   prsl, prslk,                      &
     &           phii,   phil, dtp,                                     &
     &           kdt,    hprime(1,1), oc, oa4, clx,                     &
     &           theta,sigma,gamma,elvmax,dusfcg, dvsfcg,               &
     &           con_g,con_cp,con_rd,con_rv, lonf, nmtvr, cdmbgwd,      &
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

      call rayleigh_damp(im, ix, im, levs, dvdt, dudt, dtdt,
     &                   ugrs,   vgrs, dtp, con_cp)

      do  k = 1, levs
        do i = 1, im
          gt0(i,k)   = tgrs(i,k)   + dtdt(i,k)   * dtp
          gu0(i,k)   = ugrs(i,k)   + dudt(i,k)   * dtp
          gv0(i,k)   = vgrs(i,k)   + dvdt(i,k)   * dtp
        enddo
      enddo

      do n = 1, ntrac
        do k = 1, levs
          do i = 1, im
            gq0(i,k,n) = qgrs(i,k,n) + dqdt(i,k,n) * dtp
          enddo
        enddo
      enddo

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

      if (ntoz > 0 .and. ntrac >= ntoz) then

        call ozphys(ix,im,levs,ko3,dtp,gq0(1,1,ntoz),gq0(1,1,ntoz)      &
     &,             gt0, poz, prsl, prdout, pl_coeff, del, ldiag3d      &
!hchuang code change [r1l]
     &,             dq3dt(1,1,6), me)
!    &,             dq3dt(1,1,6), me)

      endif

!  --- ...  to side-step the ozone physics

!      if (ntrac >= 2) then
!        do k = 1, levs
!          gq0(k,ntoz) = qgrs(k,ntoz)
!        enddo
!      endif

!     if (lprnt) then
!       print *,' levs=',levs,' jcap=',jcap,' dtp',dtp                  &
!    *,  ' islmsk=',islmsk(ilon,ilat),' kdt=',kdt
!       print *,' rann=',rann,' ncld=',ncld,' iq=',iq,' lat=',lat
!       print *,' pgr=',pgr
!       print *,' del=',del(ipr,:)
!       print *,' prsl=',prsl(ipr,:)
!       print *,' prslk=',prslk(ipr,:)
!       print *,' rann=',rann(ipr,1)
!       print *,' gt0=',gt0(ipr,:)                                      &
!    &,         ' kdt=',kdt,' xlon=',xlon(ipr),' xlat=',xlat(ipr)
!       print *,' dtdt=',dtdt(ipr,:)
!       print *,' gu0=',gu0(ipr,:)
!       print *,' gv0=',gv0(ipr,:)
!       print *,' gq0=',(gq0(ipr,k,1),k=1,levs)
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
      if (ldiag3d) then
        do k = 1, levs
          do i = 1, im
            dqdt(i,k,1)   = gq0(i,k,1)
          enddo
        enddo
      endif   ! end if_ldiag3d

      call get_phi(im,ix,levs,ntrac,gt0,gq0,                            &
     &             thermodyn_id, sfcpress_id,                           &
     &             gen_coord_hybrid,                                    &
     &             prsi,prsik,prsl,prslk,phii,phil)

!     if (lprnt) then
!       print *,' phii2=',phii(ipr,:)
!       print *,' phil2=',phil(ipr,:)
!     endif

      do k = 1, levs
        do i = 1, im
          clw(i,k,1) = 0.0
          clw(i,k,2) = -999.9
          cnvc(i,k)  = 0.0
          cnvw(i,k)  = 0.0
        enddo
      enddo

!  --- ...  for convective tracer transport (while using ras)

      if (ras) then
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

!  --- ...  calling precipitation processes

!     do i = 1, im
!       work1(i) = (log(coslat(i) / (nlons(i)*latg)) - dxmin) * dxinv
!       work1(i) = max(0.0, min(1.0,work1(i)))
!       work2(i) = 1.0 - work1(i)
!     enddo

!  --- ...  calling convective parameterization

      if (ntcw > 0) then

        do k = 1, levs
          do i = 1, im
            rhc(i,k) = rhbbot - (rhbbot-rhbtop) * (1.0-prslk(i,k))
            rhc(i,k) = rhc_max * work1(i) + rhc(i,k) * work2(i)
            rhc(i,k) = max(0.0, min(1.0,rhc(i,k)))
          enddo
        enddo

        if ( n3dfercld > 0) then    ! call brad ferrier's microphysics

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

        elseif ( n3dzhaocld > 0) then    ! call zhao-carr microphysics
          do i = 1, im
            psautco_l(i) = psautco(1)*work1(i) + psautco(2)*work2(i)
            prautco_l(i) = prautco(1)*work1(i) + prautco(2)*work2(i)
          enddo
          do k = 1, levs
            do i = 1, im
              clw(i,k,1) = gq0(i,k,ntcw)
            enddo
          enddo
        endif  ! end zhao microphysics

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
      if (.not. ras) then

          call sascnvn(im,ix,levs,jcap,dtp,del,prsl,pgr,phil,           &
     &                 clw,gq0,gt0,gu0,gv0,cld1d,                       &
     &                 rain1,kbot,ktop,kcnv,islmsk,                     &
     &                 vvel,ncld,ud_mf,dd_mf,dt_mf,                     &
     &                 cnvw,cnvc)

        if( n3dcldpdf >0  .and. n3dzhaocld >0 ) then
          do k = 1, levs
            do i = 1, im
              phy_f3d(i,k,n3dzhaocld+2) = cnvw(i,k)
              phy_f3d(i,k,n3dzhaocld+3) = cnvc(i,k)
            enddo
          enddo
        endif

!       if (lprnt) print *,' rain1=',rain1(ipr),' rann=',rann(ipr,1)

      else    ! if_not_sas

!       if  (lprnt) print *,' calling ras for kdt=',kdt,' me=',me       &
!    &,                     ' lprnt=',lprnt

        if (ccwf(1) >= 0.0 .or. ccwf(2) >= 0 ) then
          do i=1,im
            ccwfac(i) = ccwf(1)*work1(i) + ccwf(2)*work2(i)
            dlqfac(i) = dlqf(1)*work1(i) + dlqf(2)*work2(i)
          enddo
        else
          ccwfac = -999.0
          dlqfac = 0.0
        endif

        do i=1,im
          lmh(i) = levs
        enddo
        call rascnv(im,     ix,    levs,   dtp, dtf, rann               &
     &,             gt0,    gq0,   gu0,    gv0, clw, tottracer          &
     &,             prsi,   prsl,   prsik,  prslk, phil,  phii          &
     &,             kpbl,   cd,     rain1,  kbot,  ktop,  kcnv          &
     &,             phy_f2d(1,1), flipv, pa2mb                          &
     &,             me, garea, lmh, ccwfac, nrcm, rhc                   &
     &,             ud_mf, dd_mf, dt_mf, dlqfac, lprnt, ipr, kdt,fscav)

!  --- ...  check print

!       if (lprnt) print *,' rain1=',rain1(ipr),' rann=',rann(ipr,1)
!    &,' lat=',lat
!       do i = 1, im
!         if (tsea(i) > 380.0 .or. tsea(i) < 10) then
!           print *,' tsea=', tsea(i),' i=',i,' lat=',lat,              &
!    &        ' kdt=',kdt,' xlon=',xlon(i),' xlat=',xlat(i),' islmsk=', &
!    &        islmsk(i),' me=',me
!           stop
!         endif
!       enddo

!       do k = 1, levs
!         do i = 1, im
!           if (gt0(i,k) > 330.0 .or. gt0(i,k) < 80.0) then             &
!             print *,' gt0=', gt0(i,k),' i=',i,' k=',k,' lat=',lat,    &
!    &          ' kdt=',kdt,' xlon=',xlon(i),' xlat=',xlat(i)
!             stop
!           endif

!           if (gq0(i,k,1) > 1.0 ) then
!             print *,' gq0=', gq0(i,k,1),' i=',i,' k=',k,' lat=',lat,  &
!    &          ' kdt=',kdt
!             stop
!           endif
!         enddo
!       enddo

!       if (lprnt) print *,' returning from ras for kdt=', kdt,         &
!    &                     ' me=',me,' lat=',lat

!  --- ...  end check print

        cld1d = 0.0

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
!
      do i = 1, im
        rainc(i) = frain * rain1(i)
      enddo

      if (lssav) then
        do i = 1, im
          cldwrk(i) = cldwrk(i) + cld1d(i) * dtf
        enddo

        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dt3dt(i,k,4) = dt3dt(i,k,4) + (gt0(i,k)-dtdt(i,k)) * frain
!             dq3dt(i,k,2) = dq3dt(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))    &
!    &                                                           * frain
              du3dt(i,k,3) = du3dt(i,k,3) + (gu0(i,k)-dudt(i,k)) * frain
              dv3dt(i,k,3) = dv3dt(i,k,3) + (gv0(i,k)-dvdt(i,k)) * frain

            enddo
          enddo
          do k = 1, levs
            do i = 1, im
              dq3dt(i,k,2) = dq3dt(i,k,2) + (gq0(i,k,1)-dqdt(i,k,1))    &
     &                                                           * frain
              upd_mf(i,k)  = upd_mf(i,k)  + ud_mf(i,k) * (con_g*frain)
              dwn_mf(i,k)  = dwn_mf(i,k)  + dd_mf(i,k) * (con_g*frain)
              det_mf(i,k)  = det_mf(i,k)  + dt_mf(i,k) * (con_g*frain)
            enddo
          enddo
        endif
        do i = 1, im
          cnvprcp(i) = cnvprcp(i) + rainc(i)
        enddo
      endif
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
!!          cuhr(i,k) = (gt0(i,k)-dtdt(i,k)) / dtf
!           cuhr(i,k) = (gt0(i,k)-dtdt(i,k)) / dtp    ! moorthi

!           cumchr(i,k) = 0.
!           gwdcu(i,k)  = 0.
!           gwdcv(i,k)  = 0.
!           diagn1(i,k) = 0.
!           diagn2(i,k) = 0.

            if (k >= kbot(i) .and. k <= ktop(i)) then
!             qmax(i)     = max(qmax(i),cuhr(i,k))
!             cumabs(i)   = cuhr(i,k) + cumabs(i)

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

!  --- ...  check print

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

!         print *,' before gwdc in gbphys fhour ',fhour

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

!  --- ...  end check print

        call gwdc(im, ix, im, levs, lat, ugrs, vgrs, tgrs, qgrs,        &
     &            prsl, prsi, del, cumabs,       ktop, kbot, kcnv,cldf, &
!    &            prsl, prsi, del, qmax,         ktop, kbot, kcnv,cldf, &
!    &            prsl, prsi, del, qmax, cumchr, ktop, kbot, kcnv,      &
     &            con_g,con_cp,con_rd,con_fvirt, dlength,               &
     &            lprnt, ipr, fhour,                                    &
     &            gwdcu, gwdcv,dusfcg,dvsfcg)
!    &            dusfcg,dvsfcg,diagn1,diagn2)

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
            eng0       = 0.5*(gu0(i,k)**2+gv0(i,k)**2)
            gu0(i,k)   = gu0(i,k)   + gwdcu(i,k)   * dtp
            gv0(i,k)   = gv0(i,k)   + gwdcv(i,k)   * dtp
            eng1       = 0.5*(gu0(i,k)**2+gv0(i,k)**2)
            gt0(i,k)   = gt0(i,k) + max((eng0-eng1),0.0)/(dtp*con_cp)
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

      if (ldiag3d) then
        do k = 1, levs
          do i = 1, im
            dtdt(i,k)   = gt0(i,k)
!           dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      endif
      if (ldiag3d) then
        do k = 1, levs
          do i = 1, im
            dqdt(i,k,1) = gq0(i,k,1)
          enddo
        enddo
      endif

      if (shal_cnv) then

          call shalcnv(im,ix,levs,jcap,dtp,del,prsl,pgr,phil,           &
     &                 clw,gq0,gt0,gu0,gv0,                             &
     &                 rain1,kbot,ktop,kcnv,islmsk,                     &
     &                 vvel,ncld,hpbl,hflx,evap,ud_mf,dt_mf,            &
     &                 cnvw,cnvc)

          if (shcnvcw .and. n3dzhaocld >0 .and. n3dcldpdf >0 ) then
            do k = 1, levs
              do i = 1, im
                phy_f3d(i,k,n3dzhaocld+2) = cnvw(i,k)
                phy_f3d(i,k,n3dzhaocld+3) = cnvc(i,k)
              enddo
            enddo
          endif 

          do i = 1, im
            raincs(i) = frain * rain1(i)
          enddo

          if (lssav) then
            do i = 1, im
              cnvprcp(i) = cnvprcp(i) + raincs(i)
            enddo
          endif

          do i = 1, im
            rainc(i) = rainc(i) + raincs(i)
          enddo

      endif   ! end if_shal_cnv

      if (lssav) then
        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dt3dt(i,k,5) = dt3dt(i,k,5) + (gt0(i,k)-dtdt(i,k)) * frain
!             dq3dt(i,k,3) = dq3dt(i,k,3) + (gq0(i,k,1)-dqdt(i,k,1))    &
!    &                                                           * frain
            enddo
          enddo
          do k = 1, levs
            do i = 1, im
              dtdt(i,k)   = gt0(i,k)
!             dqdt(i,k,1) = gq0(i,k,1)
            enddo
          enddo
        endif
        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dq3dt(i,k,3) = dq3dt(i,k,3) + (gq0(i,k,1)-dqdt(i,k,1))    &
     &                                                           * frain
            enddo
          enddo
          do k = 1, levs
            do i = 1, im
              dqdt(i,k,1) = gq0(i,k,1)
            enddo
          enddo
        endif
      endif   ! end if_lssav

      do k = 1, levs
        do i = 1, im
          if (clw(i,k,2) <= -999.0) clw(i,k,2) = 0.0
        enddo
      enddo

      if (ntcw > 0) then

        if ( n3dfercld > 0) then    ! call brad ferrier's microphysics

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

        elseif (n3dzhaocld > 0) then

          do k = 1, levs
            do i = 1, im
!             gq0(i,k,ntcw) = clw(i,k)   + gq0(i,k,ntcw)
              gq0(i,k,ntcw) = clw(i,k,1) + clw(i,k,2)
!             gq0(i,k,ntcw) = clw(i,k,1)               ! for pry
            enddo
          enddo

        endif   

      else    ! if_ntcw

        do k = 1, levs
          do i = 1, im
            clw(i,k,1) = clw(i,k,1) + clw(i,k,2)
          enddo
        enddo

      endif   ! end if_ntcw

      call cnvc90(clstp, im,   ix,   rainc, kbot, ktop, levs, prsi,     &
     &            acv,   acvb, acvt, cv,    cvb,  cvt)

      if (ncld > 0) then

!
        if (n3dfercld > 0) then    ! call brad ferrier's microphysics

          do i = 1, im
            xncw(i)     = ncw(1)  * work1(i) + ncw(2) *  work2(i)
            flgmin_l(i) = flgmin(1)*work1(i) + flgmin(2)*work2(i)
          enddo

          if (kdt == 1 .and. abs(xlon(1)) < 0.0001) then
            print *,' xncw=',xncw(1),' rhc=',rhc(1,1),' work1=',work1(1)&
     &,         ' work2=',work2(1),' flgmin=',flgmin_l(1)               &
     &,         ' lon=',xlon(1) * 57.29578,' lat=',lat,' me=',me
!    &,         ' lon=',xlon(1) * 57.29578,' lat=',xlat(1) * 57.29578
!    &,         ' kinver=',kinver(1)
          endif

!         if (lprnt) print *,' ipr=',ipr,' gt0_gsmb=',gt0(ipr,:)        &
!    &,         ' xlon=',xlon(ipr),' xlat=',xlat(ipr)

          hsub    = con_hvap+con_hfus
          call gsmdrive(im, ix, levs, dtp, prsl, del                    &
     &,                 gt0, gq0(1,1,1), gq0(1,1,ntcw),                 &
     &,                 phy_f3d(1,1,1),  phy_f3d(1,1,2)                 &
     &,                 phy_f3d(1,1,3), rain1, asr, con_g               &
     &,                 con_hvap, hsub,con_cp, rhc, xncw, flgmin_l      &
     &,                 me, lprnt, ipr)

!         if (lprnt) print *,' ipr=',ipr,' gt0_gsma=',gt0(ipr,:)

        elseif (n3dzhaocld > 0 .and. n3dcldpdf == 0 ) then ! call zhao/carr/sundqvist microphysics

          call gscond(im, ix, levs, dtp, dtf, prsl, pgr,                &
     &                gq0(1,1,1), gq0(1,1,ntcw), gt0,                   &
     &                phy_f3d(1,1,1), phy_f3d(1,1,2), phy_f2d(1,2),     &
     &                phy_f3d(1,1,3), phy_f3d(1,1,4), phy_f2d(1,3),     &
     &                rhc,lprnt, ipr)

          call precpd(im, ix, levs, dtp, del, prsl, pgr,                &
     &                gq0(1,1,1), gq0(1,1,ntcw), gt0, rain1, sr,        &
     &                rainp, rhc, psautco_l, prautco_l, evpco, wminco,  &
     &                lprnt, ipr)

        elseif ( n3dzhaocld > 0 .and. n3dcldpdf >0 ) then ! call zhao/carr/sundqvist + pdfcld

          call gscondp(im, ix, levs, dtp, dtf, prsl, pgr,               &
     &                gq0(1,1,1), gq0(1,1,ntcw), gt0,                   &
     &                phy_f3d(1,1,1), phy_f3d(1,1,2), phy_f2d(1,2),     &
     &                phy_f3d(1,1,3), phy_f3d(1,1,4), phy_f2d(1,3),     &
     &                rhc,phy_f3d(1,1,n3dzhaocld+1),sup,lprnt,ipr,kdt)

          call precpdp(im, ix, levs, dtp, del, prsl, pgr,               &
     &                gq0(1,1,1), gq0(1,1,ntcw), gt0, rain1,sr,         &
     &                rainp, rhc, phy_f3d(1,1,n3dzhaocld+1),            &
     &                psautco_l, prautco_l, evpco, wminco,              &
     &                lprnt, ipr)

        endif   ! end cloud microphysics options

      endif   ! end if_ncld

!     if (lprnt) print *,' rain1=',rain1(ipr),' rainc=',rainc(ipr)

      do i = 1, im
        rainl(i) = frain    * rain1(i)
        rain(i)  = rainc(i) + rainl(i)
      enddo

!  --- ...  coupling insertion

      if (lssav_cc) then
        precr_cc(1:im) = precr_cc(1:im) + rain(1:im)
      endif

!  --- ...  end coupling insertion

      if (cal_pre) then
!       hchuang: add dominant precipitation type algorithm

        call calpreciptype(kdt,nrcm,im,ix,levs,levs+1,rann,
     &    xlat,xlon,gt0,gq0,prsl,prsi,rain,
     &    phii,n3dfercld,tsea,sr,phy_f3d(1,1,3),    ! input
     &    domr,domzr,domip,doms)                    ! output

!
!        if (lprnt) print*,'debug calpreciptype: domr,domzr,domip,doms '
!     &,domr(ipr),domzr(ipr),domip(ipr),doms(ipr)
!        do i=1,im
!         if (abs(xlon(i)*57.29578-114.0) .lt. 0.2  .and.
!     &    abs(xlat(i)*57.29578-40.0) .lt. 0.2)
!     &    print*,'debug calpreciptype: domr,domzr,domip,doms ',
!     &    domr(i),domzr(i),domip(i),doms(i)
!       end do
!       hchuang: use new precipitation type to decide snow flag for lsm snow accumulation
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
!             dq3dt(i,k,4) = dq3dt(i,k,4) + (gq0(i,k,1)-dqdt(i,k,1))    &
!    &                                                           * frain
            enddo
          enddo
        endif
        if (ldiag3d) then
          do k = 1, levs
            do i = 1, im
              dq3dt(i,k,4) = dq3dt(i,k,4) + (gq0(i,k,1)-dqdt(i,k,1))    &
     &                                                           * frain
            enddo
          enddo
        endif
      endif

!  --- ...  estimate t850 for rain-snow decision

      do i = 1, im
        t850(i) = gt0(i,1)
      enddo

      do k = 1, levs - 1
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

!     if (lprnt) print *,' tprcp=',tprcp(ipr),' rain=',rain(ipr)

!  --- ...  cpl insertion

      do i = 1, im
!       if (t850(i) <= 273.16 .and. islmsk(i) /= 0) then
        if (t850(i) <= 273.16) then
           lprec_cc(i) = 0.0
           snw_cc(i)   = rain(i)
        else
           lprec_cc(i) = rain(i)
           snw_cc(i)   = 0.0
        endif
      enddo

!  --- ...  total runoff is composed of drainage into water table and
!           runoff at the surface and is accumulated in unit of meters

      if (lssav) then
        tem = dtf * 0.001
        do i = 1, im
          runoff(i)  = runoff(i)  + (drain(i)+runof(i)) * tem
          srunoff(i) = srunoff(i) + runof(i) * tem
        enddo
      endif
       
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
        rqtk(i)  = 0.
        work2(i) = 1.0 / pgr(i)
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
          pwat(i) = pwat(i) + del(i,k)*(gq0(i,k,1)+work1(i))
          rqtk(i) = rqtk(i) + del(i,k)*(gq0(i,k,1)-qgrs(i,k,1))
        enddo
      enddo

      do i = 1, im
        pwat(i) = pwat(i) * (1.0/con_g)
        rqtk(i) = rqtk(i) * work2(i)
      enddo
!
      deallocate (clw)
      deallocate (cnvc)
      deallocate (cnvw)
!
!     if (lprnt) call mpi_quit(7)
!     if (kdt > 2)  call mpi_quit(701)

      return
!...................................
      end subroutine gbphys
!-----------------------------------

