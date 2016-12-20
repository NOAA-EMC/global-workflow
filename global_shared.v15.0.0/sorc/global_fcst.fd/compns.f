!-----------------------------------------------------------------------
      subroutine compns(deltim,  iret,
!    &                  ntrac,   nxpt,    nypt,  jintmx, jcap,
     &                  ntrac,                           jcap,
     &                  levs,    levr,    lonf,  lonr,   latg,    latr,
     &                  ntoz,    ntcw,    ncld,  lsoil,  nmtvr,
     &                  num_p3d, num_p2d, num_a3d, num_a2d, me,
     &                  nlunit, gfs_namelist)
!
!$$$  subprogram documentation block
!
! subprogram:  compns     check and compute namelist frequencies
!   prgmmr: iredell       org: np23          date: 1999-01-26
!
! abstract: this subprogram checks global spectral model namelist
!           frequencies in hour units for validity.  if they are valid,
!           then the frequencies are computed in timestep units.
!           the following rules are applied:
!             1. the timestep must be positive;
!             2. the output frequency must be positive and
!                a multiple of the timestep to within tolerance;
!             3. the shortwave frequency must be positive and
!                a multiple of the timestep to within tolerance;
!             4. the longwave frequency must be positive and
!                a multiple of the timestep to within tolerance and
!                a multiple of the shortwave frequency;
!             5. the zeroing frequency must be positive and
!                a multiple of the timestep to within tolerance and
!                a multiple of the output frequency;
!             6. the restart frequency must be positive and
!                a multiple of the timestep to within tolerance and
!                a multiple of the longwave frequency and
!                a multiple of the zeroing frequency;
!             7. the initialization window must be non-negative and
!                a multiple of the timestep to within tolerance and
!                a multiple of the longwave frequency and
!                no longer than the restart frequency;
!             8. the cycling frequency must be non-negative and
!                a multiple of the timestep to within tolerance and
!                a multiple of the longwave frequency.
!
! program history log:
!   1999-01-26  iredell
!   2009-05-04  moorthi
!   2013-01     y-t hou    - 1. changed radiation calling time control
!                  variables, fhswr/fhlwr, from previous in unit of hours
!                  to in unit of seconds. thus radiation calling interval
!                  can be made less than the old one-hour limit.
!                            2. added aerosol model selection flag, iaer_mdl
!                  for choices of opac-clim, gocart-clim, and gocart-prog...
!  2013-07      r. sun   added pdf cloud related options:pdfcld,sup
!  2013-07      j. han   added reduced drag coefficient: redrag
!  2013-09      j. han   added hybrid edmf pbl scheme: hybedmf
!  2013-09      j. han   added tke dissipative heating: dspheat
!  2013-09      f. yang  restructured restart fields, and tvd restart field.           
!  2014-12      p. pegion changes to make SKEB and SHUM resolution/time step independent
!
! usage:    call compns(deltim,
!    &                  fhout,fhswr,fhlwr,fhzer,fhres,fhdfi,fhcyc,
!    &                  nsout,nsswr,nslwr,nszer,nsres,nsdfi,nscyc,
!    &                  iret)
!   input arguments:
!     tol      - real error tolerance allowed for input frequencies
!                (e.g. 0.01 for 1% of timestep maximum error allowed)
!     deltim   - real timestep in seconds
!     fhout    - real output frequency in hours
!     fhswr    - real shortwave frequency in seconds
!     fhlwr    - real longwave frequency in seconds
!     fhzer    - real zeroing frequency in hours
!     fhres    - real restart frequency in hours
!     fhdfi    - real initialization window in hours
!     fhcyc    - real cycling frequency in hours
!   output arguments:
!     nsout    - integer output frequency in timesteps
!     nsswr    - integer shortwave frequency in timesteps
!     nslwr    - integer longwave frequency in timesteps
!     nszer    - integer zeroing frequency in timesteps
!     nsres    - integer restart frequency in timesteps
!     nsdfi    - integer initialization window in timesteps
!     nscyc    - integer cycling frequency in timesteps
!     iret     - integer return code (0 if successful or
!                between 1 and 8 for which rule above was broken)
!     ldiag3d  - switch for 3d diagnostic- (default = false)
!
! attributes:
!   language: fortran 90
!
!$$$

      
      use namelist_def

!     use namelist_def , only :
!    & adiab,crtrh,fhcyc,fhdfi,fhlwr,fhmax,fhout,fhres,fhrot,
!    & fhseg,fhswr,fhzer,filta,flgmin,gen_coord_hybrid,
!    & gg_tracers,hybrid,iaer,ialb,ico2,iems,igen,
!    & iovr_lw,iovr_sw,isol,ldiag3d,lingg_a,lingg_b,
!    & lsm,ncw,ngptc,nscyc,nsdfi,nslwr,nsout,nsres,
!    & nsswr,nszer,old_monin,pre_rad,random_clds,ras,
!    & redgg_a,semilag,shuff_lats_a,shuff_lats_r,
!    & zhao_mic,
!    & max_kdt_switch_1,max_kdt_switch_2,kdt_switch

!cmy mpi_def holds liope
      use mpi_def, only : liope
      use layout_grid_tracers , only : xhalo, yhalo
      use pmgrid              , only : wgt, quamon
      implicit none

      real tol
 
      character (len=*), intent(in) :: gfs_namelist
      integer, intent(in)           :: me, nlunit
      real,intent(inout)            :: deltim
      integer,intent(out)           :: iret
!     integer ntrac,nxpt,nypt,jintmx,jcap,levs,lonf,lonr,latg,latr
      integer ntrac,jcap,levs,lonf,lonr,latg,latr
      integer levr
      integer ntoz,ntcw,ncld,lsoil,nmtvr
     &,       num_p3d,num_p2d,num_a3d,num_a2d
      real    tfiltc
      integer k

!sela - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     if output (fhout) more frequently than zeroing ,get partial rains
 
      namelist /nam_mrf/fhmax,fhout,fhres,fhzer,fhseg,fhrot,deltim,igen,
     & ngptc,fhdfi,fhswr,fhlwr,fhcyc,ras,ldiag3d,
!    & shuff_lats_a,shuff_lats_r,reshuff_lats_a,reshuff_lats_r,
     & shuff_lats_a,shuff_lats_r,
     & adiab,explicit,pre_rad,hybrid,gen_coord_hybrid,random_clds,liope,
     & run_enthalpy,out_virttemp, ndsl,
     & ntrac,          jcap,levs,lonf,lonr,latg,latr,levr,
     & ntoz,ntcw,ncld,lsoil,nmtvr,zhao_mic,nsout,lsm,tfiltc,
     & isol, ico2, ialb, iems, iaer, iovr_sw, iovr_lw, ictm,
     & isubc_sw, isubc_lw, iaer_mdl,
     & ncw, crtrh,old_monin,flgmin,gfsio_in,gfsio_out,ref_temp,cnvgwd,
     & semilag,redgg_a,lingg_a,lingg_b,gg_tracers,phigs_d,ref_pres,
     & ccwf,sashal,newsas,redrag,hybedmf,dspheat,cgwf,fixtrc,
     & zflxtvd,crick_proof,ccnorm,ctei_rm,mom4ice,
     & shal_cnv,herm_x,herm_y,herm_z,lin_xyz,wgt_cub_lin_xyz,lin_xy,
     & wgt_cub_lin_xyz_trc,
     & time_extrap_etadot,settls_dep3ds,settls_dep3dg,
     & iter_one_no_interp,cont_eq_opt1,opt1_3d_qcubic,
     & norad_precip,mstrat,trans_trac,dlqf,moist_adj,climate,
     & nst_fcst,nst_spinup,lsea,cal_pre,psautco,prautco,evpco,wminco,
!    & nst_active,nst_restart,tr_analysis,lsea,cal_pre,psautco,evpco,
     & fhout_hf,fhmax_hf,cdmbgwd,bkgd_vdif_m,bkgd_vdif_h,hdif_fac,
     & sl_epsln,bkgd_vdif_s,hdif_fac2,dtphys,levwgt,wgtm,quamon,
     & use_ufo,slrd0,yhalo,sup,pdfcld,shcnvcw,cdamp,k2o,
!             stochastic related
     & sppt,sppt_tau,sppt_lscale,sppt_logit,
     & stochphys,vcamp,vc_lscale,vc_tau,vc_logit,
     & iseed_shum,iseed_sppt,shum,shum_tau,shum_lscale,iseed_vc,
     & fhstoch,stochini,vc,skeb_varspect_opt,sppt_sfclimit,
     & skeb,skeb_tau,skeb_lscale,iseed_skeb,skeb_vfilt,skeb_diss_smooth,
     & skeb_sigtop1,skeb_sigtop2,sppt_sigtop1,sppt_sigtop2,
     & vc_sigtop1,vc_sigtop2,
     & shum_sigefold,iau,iaufiles_fg,iaufiles_anl,iaufhrs,iau_delthrs ! for IAU
!
!     integer n_rule,rule(8)
!
      real    tem
!
      gg_tracers = .false.
      redgg_a    = .true.
      semilag    = .true.
      lingg_a    = .true.
      lingg_b    = .true.
      sl_epsln   = 0.05
      phigs_d    = 60.0
      cdamp(1)   = 5.0e4
      cdamp(2)   = 2.0
      k2o        = -1
!
      herm_x     = .true.
      herm_y     = .true.
      herm_z     = .true.
      lin_xyz    = .false.
      lin_xy     = .false.

      wgt_cub_lin_xyz     = .false.
      wgt_cub_lin_xyz_trc = .false.
      quamon              = .false.
      iter_one_no_interp  = .false.
      cont_eq_opt1        = .false.
      opt1_3d_qcubic      = .false.
      time_extrap_etadot  = .false.
      settls_dep3ds       = .true.
      settls_dep3dg       = .true.
      fixtrc(:)           = .false.     !for all tracers
      fixtrc(2)           = .true.      !ozone fixer
!
      use_ufo    = .true.
!
      ndsl       = .false.
!
      fhmax    = 0
      fhout    = 0
      fhres    = 0
      fhzer    = 0
      fhseg    = 0
      fhrot    = 0
      fhout_hf = 1
      fhmax_hf = 0
      deltim   = 0
      dtphys   = 225 
      igen     = 0
      fhdfi    = 0
      fhswr    = 0
      fhlwr    = 0
      fhcyc    = 0
!     tfiltc   = 0.92
      tfiltc   = 0.85
      ccwf     = 1.0
      dlqf     = 0.0
      cgwf(1)  = 0.5      ! cloud top fraction for convective gwd scheme 
      cgwf(2)  = 0.05     ! cloud top fraction for convective gwd scheme
      ctei_rm  = 10.0
      ngptc    = 30
!
      bkgd_vdif_m = 1.0
      bkgd_vdif_h = 1.0
      bkgd_vdif_s = 1.0
      hdif_fac    = 1.0
      hdif_fac2   = 1.0
      slrd0       = 0.002 ! sigma level above which raleigh damping applied
!
!
      ras              = .false.
      zhao_mic         = .true.
      ldiag3d          = .false.
      shal_cnv         = .true.
      sashal           = .true.
      crick_proof      = .false.
      ccnorm           = .false.
      newsas           = .true.
      redrag           = .true.
      hybedmf          = .true.
      dspheat          = .true.
      norad_precip     = .false.   ! this is effective only for ferrier/moorthi
      mom4ice          = .false.   ! true when coupled to mom4 om
      mstrat           = .false.
      trans_trac       = .true.    ! this is effective only for ras
      moist_adj        = .false.   ! must be true to turn on moist convective
      cal_pre          = .true.   ! true for huiya's precip type algorithm

      climate          = .false.   ! .true. for flx file output in climate mode
                                   ! when false, 8 extra instantaneous fields
                                   ! are written to flx files for lsm use
!
      shuff_lats_a     = .false.
      shuff_lats_r     = .true.
!
      adiab            = .false.
      explicit         = .false.
      pre_rad          = .false.
      hybrid           = .true.    ! hybrid is now default 2/4/14
      gen_coord_hybrid = .false.
      random_clds      = .false.
      liope            = .true.
!
      old_monin        = .false.
      cnvgwd           = .true.
      zflxtvd          = .false.
      run_enthalpy     = .false.
      out_virttemp     = .true.
!
      ncw(1)           = 20
      ncw(2)           = 120
      crtrh(:)         = 0.90
      flgmin(1)        = 0.18
      flgmin(2)        = 0.22
!
      psautco(1)       = 6.0e-4    ! zhao scheme default opr value
      psautco(2)       = 3.0e-4    ! zhao scheme default opr value
      prautco(:)       = 1.0e-4    ! zhao scheme default opr value
      evpco            = 2.0e-5    ! zaho scheme evaporation coefficient
      wminco(:)        = 1.0e-5    ! zhao scheme default water and ice floor
      cdmbgwd(1)       = 2.00      ! mtn blking and gwd tuning factors
      cdmbgwd(2)       = 0.25      ! mtn blking and gwd tuning factors
      sup              = 1.1
      pdfcld           = .false.
      shcnvcw          = .false.
                                   ! for nst model
      nst_fcst         = 0         ! 0 - am only, 1 - uncoupled, 2 - coupled
      nst_spinup       = .false.
!     nst_active       = .false.
!     nst_restart      = .false.
!     tr_analysis      = .false.

      lsea             = 0
!
      ref_temp         = 350.0
      ref_pres         = 80.0
!
      gfsio_in         = .false.
      gfsio_out        = .false.

!  additions for stochastic perturbations...
!------------------------------------------
      ! can specify up to 5 values for the stochastic physics parameters
      ! (each is an array of length 5)
      sppt             = -999.  ! stochastic physics tendency amplitude
      shum             = -999.  ! stochastic boundary layer spf hum amp (1/hr)
      skeb             = -999.  ! stochastic ke backscatter amplitude
      vcamp            = -999.  ! stochastic vorticity confinment amp
! number of passes of 1-2-1 vertical filter
! for skeb random patterns.
      skeb_vfilt       = 0
      sppt_tau         = -999.  ! time scales
      shum_tau         = -999.
      skeb_tau         = -999.
      vc_tau           = -999.
      sppt_lscale      = -999.  ! length scales
      shum_lscale      = -999.
      skeb_lscale      = -999.
      vc_lscale        = -999.
      iseed_sppt       = 0      ! random seeds (if 0 use system clock)
      iseed_shum       = 0
      iseed_skeb       = 0
      iseed_vc         = 0
! parameters to control vertical tapering of stochastic physics with
! height
      sppt_sigtop1 = 0.1
      sppt_sigtop2 = 0.025
      skeb_sigtop1 = 0.1
      skeb_sigtop2 = 0.025
      vc_sigtop1 =   0.1 
      vc_sigtop2 =   0.025
      shum_sigefold = 0.2
! reduce amplitude of sppt near surface (lowest 2 levels)
      sppt_sfclimit = .false.
! wavenumber defining smoothing of skeb dissipation estimate.
      skeb_diss_smooth = 12.
! gaussian or power law variance spectrum for skeb (0: gaussian, 1:
! power law). if power law, skeb_lscale interpreted as a power not a
! length scale.
      skeb_varspect_opt = 0
      vc                = 0.      ! deterministic vorticity confinement parameter.
      sppt_logit        = .true.  ! logit transform for sppt to bounded interval [-1,+1]
      vc_logit          = .false. ! logit transform for vc to bounded interval [-1,+1]
      fhstoch           = -999.0  ! forecast hour to dump random patterns
      stochini          = .false. ! true= read in pattern, false=initialize from seed
! additions for iau
      iau              = .false.
      iaufhrs          = -1
      iaufiles_fg      = ''
      iaufiles_anl     = ''
!
      nsout    = 0
      nsout_hf = 0
      lsm      = 1        ! noah lsm is the default when lsm=1
      levr     = 0
!           default values for some radiation controls
!           ------------------------------------------
      isol     = 2        ! 0--fixed solar constant; 1--changing solar constant 
      ico2     = 2        ! 0--fixed co2 constant; 1--time varying global mean co2; 2--changing co2 
      ialb     = 0        ! 0--use climatology alb, based on sfc type; 1--modis alb 
      iems     = 1        ! 0-blackbody ground emission; 1-climatology on one-deg map 
      iaer     = 111      ! 111--with stratospheric aerosol, tropospheric aerosol lw, troposphere aerosol sw.
      iaer_mdl = 0        ! default aerosol model is opac-climatology
      iovr_sw  = 1        ! 0--random cloud overlap for sw; 1--maximum-random cloud overlap for sw
      iovr_lw  = 1        ! 0--random cloud overlap for lw; 1--maximum-random cloud overlap for lw 
      ictm     = 1        ! ictm=0 => use data at initial cond time, if not
                          ! available, use latest, no extrapolation.
                          ! ictm=1 => use data at the forecast time, if not
                          ! available, use latest and extrapolation.
                          ! ictm=yyyy0 => use yyyy data for the forecast time,
                          ! no further data extrapolation.
                          ! ictm=yyyy1 = > use yyyy data for the fcst.
                          ! if needed, do extrapolation to match the fcst time.
                          ! ictm=-1 => use user provided external data for
                          ! the fcst time, no extrapolation.
                          ! ictm=-2 => same as ictm=0, but add seasonal cycle
                          ! from climatology. no extrapolation.

      isubc_sw = 2        ! sw clouds without sub-grid approximation
      isubc_lw = 2        ! lw clouds without sub-grid approximation
                          ! =1 => sub-grid cloud with prescribed seeds
                          ! =2 => sub-grid cloud with randomly generated seeds
!
      xhalo   = 1         ! this value should remain 1 -- do not change
      yhalo   = 10
!
      levwgt(1) = 24
      levwgt(2) = 30
      wgtm(1)   = 0.0
      wgtm(2)   = 1.0
!
      if (me == 0) then
        print *,' nlunit=',nlunit,' gfs_namelist=',gfs_namelist
      endif
!$$$      read(5,nam_mrf)

      open(unit=nlunit,file=gfs_namelist)
      rewind (nlunit)
      read(nlunit,nam_mrf,err=999)
!
      if (cal_pre) then
        random_clds = .true.
        if (me == 0) print *,' cal_pre=',cal_pre,' random_clds=',
     &                       random_clds
      endif
!     print *,' fhmax=',fhmax,' nst_active =',nst_active,'
!    &  nst_restart =',nst_restart, ' tr_analysis = ',tr_analysis,
!    &  'lsea =',lsea
!
      if (k2o < 0) k2o = levs/2
      if (me == 0) then
        write(6,nam_mrf)
        print*,' yhalo =', yhalo
      endif
!
!     if(reshuff_lats_r .and. adiab) then
!      reshuff_lats_r=.false.
!      print*,' can not reshuff loopr and loopb adiabatically'
!     endif
!
      filta = tfiltc
      phigs = phigs_d * (atan(1.0)/45.0)
      if (me == 0 .and. semilag) then
        write(0,*)' phigs_d=',phigs_d,' phigs=',phigs
      endif
!
      if (levs > 100 .and. ref_temp < 400.0) ref_temp = 1500.0
!
      if (me == 0) then
        if (.not. (hybrid .or. gen_coord_hybrid)) then
         print *,' sigma coordinate no longer supported - use hybrid '
         call mpi_quit(2220)
        endif
        print *,' the time filter coefficient tfiltc=',tfiltc
        if (adiab) then
          print *,' this is an adiabatic run'
        else
          if (lsm == 1) then
            print *,' noah land surface model used'
          elseif (lsm == 0) then
            print *,' osu land surface model no longer active'
            call mpi_quit(2221)
          else
            print *,' unsupported lsm type - job aborted'
     &,                          ' - lsm=',lsm
            call mpi_quit(2222)
          endif
          print *,' in compns use_ufo=',use_ufo
!
          if (ras) then
            print *,' ras convection scheme used with ccwf=',ccwf
          else
            if (newsas) then
              print *,' new modified sas convection scheme used'
            else
              print *,' old sas convection option no longer available'
              call mpi_quit(2223)
            endif
          endif
          if(moist_adj) then
             print *,' moist_adj option no longer active '
             call mpi_quit(2226)
          end if
          if (redrag) then
           print *,'reduced drag coeff. over sea in high wind condition'
          endif
          if (hybedmf) then
           print *,'hybrid edmf pbl scheme is used'
          endif
          if (dspheat) then
           print *,'tke dissipative heating is added'
          endif
          if (.not. old_monin) then
             print *,' new pbl scheme moninq used'
          else
             print *,' old_monin option no longer active'
             call mpi_quit(2224)
          end if
          if (.not. sashal .or. mstrat)then
             print *,' mstrat or .not. sashal options no longer active'
             call mpi_quit(2225)
          end if
          if (.not. shal_cnv) then
            print *,' no shallow convection used'
          else
            if (sashal) print *,' new massflux based shallow convection'
     &,                         ' used'
          endif
          if (cnvgwd) print *,' convective gwd parameterization used'
          if (crick_proof) print *,' crick-proof cloud water used in'
     &,                            ' radiation '
          if (ccnorm) print *,' cloud condensate normalized by cloud'
     &,                       ' cover for radiation'
        endif
      endif
!
      if (levr == 0) then
        levr = levs
      endif
      if (me == 0) then
        if (.not. adiab) then
          print *,' radiative heating calculated at',levr, ' layers'
          if (iovr_sw == 0) then
            print *,' random cloud overlap for shortwave iovr_sw='
     &,             iovr_sw
          else
            print *,' max-random cloud overlap for shortwave iovr_sw='
     &,             iovr_sw
          endif
          if (iovr_lw == 0) then
            print *,' random cloud overlap for longwave iovr_lw='
     &,             iovr_lw
          else
            print *,' max-random cloud overlap for longwave iovr_lw='
     &,             iovr_lw
          endif
          if (isubc_sw == 0) then
            print *,' no sub-grid cloud for shortwave isubc_sw='
     &,             isubc_sw
          else
            print *,' sub-grid cloud for shortwave isubc_sw='
     &,             isubc_sw
          endif
          if (isubc_lw == 0) then
            print *,' no sub-grid cloud for longwave isubc_lw='
     &,             isubc_lw
          else
            print *,' sub-grid cloud for longwave isubc_lw='
     &,             isubc_lw
          endif
        endif
      endif
!my 
      if (herm_x .or. herm_y .or. herm_z) then
         if (me == 0) 
     & print*,'hermite interp turn off lin_xyz,wgt_cub_lin_xyz,quamon'
     &       ,',lin_xy'
         lin_xyz         = .false.
         wgt_cub_lin_xyz = .false.
         quamon          = .false.
         lin_xy          = .false.
         cont_eq_opt1    = .false.
      endif
!my 
!sk   if (lin_xyz .neqv. wgt_cub_lin_xyz) then
!        if (me == 0) 
!    & print*,'**error lin_xyz should always be same as wgt_cub_lin_xyz'
!      print*,' setting both to false '
!        lin_xyz         = .false.
!        wgt_cub_lin_xyz = .false.         
!     endif
!sk
      if (settls_dep3dg .neqv. settls_dep3ds) then
        if (me == 0) then 
          print*,'*error: settls_dep3dg should be same as settls_dep3ds'
          print*,' setting both to true '
        endif
        settls_dep3dg  = .true.
        settls_dep3ds  = .true.         
      endif
!
      if(.not. zhao_mic .or. ras) pdfcld = .false.
      if(.not. pdfcld   ) shcnvcw = .false.

! define 3d and 2d fields for writing restart files
      n3dzhaocld = 0; n3dfercld = 0; n3dcldpdf = 0; n3dflxtvd = 0
      n2dzhaocld = 0; n2dfercld = 0; n2dcldpdf = 0; n2dflxtvd = 0
      
      if (zhao_mic) then   ! zhao microphysics
        n2dzhaocld = 2
        n3dzhaocld = 4
      else                 ! ferrier microphysics
        n2dfercld  = 0
        n3dfercld  = 3
      endif
      if(pdfcld) then
        n2dcldpdf  = 0
        n3dcldpdf  = 3
      endif
      if(zflxtvd) then
        n2dflxtvd  = 0
        n3dflxtvd  = ntrac
      endif

!-number of restart fields for physics (gloopb)
!  num_p2d=1 is reserved for ddvel,wind enhancement due to convection
      num_p3d =     n3dzhaocld + n3dfercld + n3dcldpdf
      num_p2d = 1 + n2dzhaocld + n2dfercld + n2dcldpdf

!-number of restart fields for dynamics (gloopa)
      num_a3d = max(1,n3dflxtvd)
      num_a2d = max(1,n2dflxtvd)


      if (me == 0) then
         print *,'physics restart num_p3d=',num_p3d
         print *,'n3dzhaocld=',n3dzhaocld
         print *,'n3dfercld= ',n3dfercld
         print *,'n3dcldpdf= ',n3dcldpdf
         if (shcnvcw) print*,'shallow con. cloud water and cover'
         print *,'num_p3d=',num_p3d, ' num_p2d=',num_p2d
         print *,'n2dzhaocld=',n2dzhaocld
         print *,'n2dfercld= ',n2dfercld
         print *,'n2dcldpdf= ',n2dcldpdf
         print *,'dynamic restart num_a3d=',num_a3d
         print *,'dynamic restart num_a2d=',num_a2d
         print *,'n3dflxtvd= ',n3dflxtvd
         print *,'n2dflxtvd= ',n2dflxtvd
      endif

      if (me == 0 .and. .not. adiab) then
        print*,'crtrh=',crtrh,' ncw=',ncw,' flgmin=',flgmin
      endif

! set stochphys logical to .true. if any of the stochastic
! physics amplitude parameters are positive.
      if (vc      .le. 0. .and. vcamp(1) .le. 0. .and.
     &    skeb(1) .le. 0. .and. shum(1)  .le. 0. .and.
     &    sppt(1) .le. 0.) then
          stochphys = .false.
      else
          stochphys = .true.
      endif
      if (me == 0) then
        if (stochphys) then
           print*,'stochphys set to .true. (stochastic physics is on)'
        else
           print*,'stochphys set to .false. (stochastic physics is off)'
        endif
      endif
    ! shum parameter has units of 1/hour, to remove time step
    ! dependence.
    ! change shum parameter units from per hour to per timestep
      do k=1,5
         if (shum(k) .gt. 0.0) shum(k)=shum(k)*deltim/3600.0
      enddo
    ! adjust skeb values for resolution.
    ! scaling is such that a value of 1.0 at T574 with a 900 second
    ! timestep produces well-calibrated values of forecast spread.
      do k=1,5
         if (skeb(k) .gt. 0.0) then
            skeb(k)=skeb(k)*deltim/(jcap*(jcap+1))*365765.0  ! 365765 is a scale factor so the base SKEB value in the namelist is 1.0
         endif
      enddo
!
!  allocate and set up weighting function for two time level semi-lagangian
!  here levels ae from top to bottom
!
      allocate (wgt(levs))
      if (semilag) then
        if (ref_pres < 100.0) then
          ref_pres = 101.325
          if (me == 0) write(0,*)' restting ref_pres to ',ref_pres,' cb'
        endif

        do k = 1,levwgt(1)
          wgt(k) = wgtm(1)
        enddo
        tem = (wgtm(2) - wgtm(1)) / (levwgt(2) - levwgt(1) + 1)
        do k = levwgt(1)+1,levwgt(2)
          wgt(k) = wgtm(1) + tem * (k-levwgt(1))
        enddo
        do k = levwgt(2)+1,levs
          wgt(k) = wgtm(2)
        enddo
!       wgt(:) = 1.0
        if (me == 0) print *,' wgt=',wgt
      else
        gg_tracers = .false.
        fixtrc(:)  = .false.
      endif
!
!sela - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      tol=0.01
!  check rule 1.
      if(deltim <= 0) then
        iret=1
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nsout and check rule 2.
      if(nsout > 0) fhout=nsout*deltim/3600.
      nsout=nint(fhout*3600./deltim)
      if(nsout<=0 .or. abs(nsout-fhout*3600./deltim)>tol) then
        iret=2
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nsout_hf and check rule 21.
!     if(nsout_hf > 0) fhout=nsout_hf*deltim/3600.
      nsout_hf=nint(fhout_hf*3600./deltim)
      if(nsout_hf<=0 .or. abs(nsout_hf-fhout_hf*3600./deltim)>tol) then
        iret=9
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nsswr and check rule 3.
      nsswr=nint(fhswr/deltim)                              ! fhswr in seconds
      if(nsswr<=0 .or. abs(nsswr-fhswr/deltim)>tol) then
        iret=3
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nslwr and check rule 4.
      nslwr=nint(fhlwr/deltim)                              ! fhlwr in seconds
      if(nslwr<=0 .or. abs(nslwr-fhlwr/deltim)>tol .or.                 &
     &   mod(nslwr,nsswr)/=0) then
        iret=4
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nszer and check rule 5.
      nszer=nint(fhzer*3600./deltim)
      if(nszer<=0 .or. abs(nszer-fhzer*3600./deltim)>tol .or.           &
     &   mod(nszer,nsout)/=0) then
        iret=5
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nsres and check rule 6.
      nsres=nint(fhres*3600./deltim)
      if(nsres<=0 .or. abs(nsres-fhres*3600./deltim)>tol .or.           &
     &   mod(nsres,nslwr)/=0 .or. mod(nsres,nszer)/=0) then
        iret=6
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nsdfi and check rule 7.
      if(fhdfi == 0.) then
        nsdfi=0
      else
        nsdfi=nint(fhdfi*3600./deltim)
        if(nsdfi<=0 .or. abs(nsdfi-fhdfi*3600./deltim)>tol .or.         &
     &     mod(nsdfi,nslwr)/=0 .or. nsdfi>nsres) then
          iret=7
          return
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute nscyc and check rule 8.
      if(fhcyc == 0.) then
        nscyc=0
      else
        nscyc=nint(fhcyc*3600./deltim)
        if(nscyc<=0 .or. abs(nscyc-fhcyc*3600./deltim)>tol .or.         &
     &     mod(nscyc,nslwr)/=0) then
          iret=8
          return
        endif
      endif
!!
      if (ngptc > lonr) then
         ngptc=lonr
         write(*,*) "ngptc is too big, reset ngptc to lonr",ngptc
      endif
      if (me == 0)   write(*,*) "ngptc is set to ngptc :",ngptc
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  all checks are successful.
      iret=0

      return
!
  999 print *,' error reading  namelist - execution terminated by user'
      if (me == 0) then
        write(6,nam_mrf)
        print*,' yhalo =', yhalo
      endif
      call mpi_quit(999)
!
      end
