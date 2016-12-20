      module namelist_def
      use machine
      implicit none
      save
      integer nszer,nsres,nslwr,nsout,nsswr,nsdfi,nscyc,igen,jo3,ngptc
     &,       lsm,ens_mem,ncw(2),num_reduce,lsea,nsout_hf,dtphys
     &,       levwgt(2),k2o
      real(kind=kind_evod) fhswr,fhlwr,fhrot,fhseg,fhmax,fhout,fhres
     &,                    fhzer,fhini,fhdfi,fhcyc,filta,crtrh(3)
     &,                    flgmin(2),ref_temp,ref_pres,ccwf(2),dlqf(2)
     &,                    ctei_rm(2),fhout_hf,fhmax_hf
     &,                    cdmbgwd(2),bkgd_vdif_m, bkgd_vdif_h, hdif_fac
     &,                    psautco(2), prautco(2), wminco(2), evpco
     &,                    sl_epsln,phigs,phigs_d,bkgd_vdif_s,hdif_fac2
     &,                    wgtm(2),slrd0,sup,cgwf(2),cdamp(2)

! iau parameters
      logical                            :: iau = .false. ! iau forcing included
      integer                            :: iau_delthrs = 6 ! iau time interval (to scale increments)
      character(len=120),   dimension(7) :: iaufiles_fg,iaufiles_anl
      real(kind=kind_evod), dimension(7) :: iaufhrs
      real(kind=kind_evod), dimension(5) :: skeb,skeb_lscale,skeb_tau
     &,                                     sppt,sppt_lscale,sppt_tau
     &,                                     vcamp,vc_lscale,vc_tau
     &,                                     shum,shum_lscale,shum_tau
      real(kind=kind_evod) :: skeb_sigtop1,skeb_sigtop2, 
     &                       sppt_sigtop1,sppt_sigtop2,shum_sigefold,
     &                       vc_sigtop1,vc_sigtop2
      integer,              dimension(5) :: skeb_vfilt
      integer(8),           dimension(5) :: iseed_sppt,iseed_vc
     &,                                     iseed_shum,iseed_skeb
      integer skeb_varspect_opt
      logical sppt_sfclimit
!
      logical ldiag3d,ras,zhao_mic,sashal,newsas,redrag,
     &        crick_proof,ccnorm,hybedmf,dspheat,stochphys
      logical shal_cnv, use_ufo, fixtrc(20)
      logical mom4ice, mstrat, trans_trac, moist_adj, cal_pre
     &,       climate, lsfwd, lssav, lscca, lsswr, lslwr
     &,       pdfcld,shcnvcw

!     logical shuff_lats_a,shuff_lats_r,reshuff_lats_a,reshuff_lats_r
      logical shuff_lats_a, shuff_lats_r, hybrid, gen_coord_hybrid
     &,       zflxtvd,      run_enthalpy, out_virttemp, ndsl
     &,       adiab,        explicit,     pre_rad, random_clds
     &,       old_monin,cnvgwd, restart, gfsio_in, gfsio_out
!
      logical herm_x,  herm_y,  herm_z,  lin_xyz, wgt_cub_lin_xyz,lin_xy
     &,       semilag, redgg_a, lingg_a, lingg_b, gg_tracers
     &,       wgt_cub_lin_xyz_trc
     &,       time_extrap_etadot,settls_dep3ds,settls_dep3dg
     &,       iter_one_no_interp,cont_eq_opt1,opt1_3d_qcubic


!     logical nsst_active
!     logical nsst_restart
!     logical tr_analysis

      integer nst_fcst
      logical nst_spinup

      character*20 ens_nam
!
!     radiation control parameters
!
      logical norad_precip
      integer isol, ico2, ialb, iems, iaer, iovr_sw, iovr_lw, ictm
! stochastic parameters
      real(kind=kind_evod) skeb_diss_smooth
      real(kind=kind_evod) fhstoch,vc
      logical vc_logit,sppt_logit,stochini

      integer n3dzhaocld, n3dfercld, n3dcldpdf, n3dflxtvd
      integer n2dzhaocld, n2dfercld, n2dcldpdf, n2dflxtvd
!
!  *******************do we need the following?  *****************************
!     integer max_kdt_switch_1
!     integer max_kdt_switch_2
!     integer , allocatable :: kdt_switch(:,:)
!  ********************************************************

      integer isubc_sw, isubc_lw, iaer_mdl

      end module namelist_def
