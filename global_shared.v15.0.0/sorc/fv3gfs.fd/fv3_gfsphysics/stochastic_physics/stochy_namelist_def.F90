      module stochy_namelist_def
!
! program log
! 11 Oct 2016:    Philip Pegion create standalone stochastic physics
!
      use machine
      implicit none
      
      public
      integer nsskeb,lon_s,lat_s,ntrunc

! pjp stochastic phyics
      integer skeb_varspect_opt
      logical sppt_sfclimit

      real(kind=kind_dbl_prec) :: skeb_sigtop1,skeb_sigtop2,          &
                         sppt_sigtop1,sppt_sigtop2,shum_sigefold, &
                         vc_sigtop1,vc_sigtop2
      real(kind=kind_dbl_prec) fhstoch,vc,skeb_diss_smooth,skebint
      real(kind=kind_dbl_prec), dimension(5) :: skeb,skeb_lscale,skeb_tau
      real(kind=kind_dbl_prec), dimension(5) :: sppt,sppt_lscale,sppt_tau
      real(kind=kind_dbl_prec), dimension(5) :: vcamp,vc_lscale,vc_tau
      real(kind=kind_dbl_prec), dimension(5) :: shum,shum_lscale,shum_tau
      integer,dimension(5) ::skeb_vfilt
      integer(8),dimension(5) ::iseed_sppt,iseed_vc,iseed_shum,iseed_skeb
      logical stochini,vc_logit,sppt_logit
      logical do_shum,do_sppt,do_skeb,do_vc
      character*120 :: weight_file

      end module stochy_namelist_def
