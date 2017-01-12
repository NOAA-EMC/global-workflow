!-----------------------------------------------------------------------
      subroutine compns_stochy (me,nlunit,stochy_namelist,deltim,iret)
!$$$  Subprogram Documentation Block
!
! Subprogram:  compns     Check and compute namelist frequencies
!   Prgmmr: Iredell       Org: NP23          Date: 1999-01-26
!
! Abstract: This subprogram checks global spectral model namelist
!           frequencies in hour units for validity.  If they are valid,
!           then the frequencies are computed in timestep units.
!           The following rules are applied:
!             1. the timestep must be positive;
!
! Program History Log:
!   2016-10-11  Phil Pegion  make the stochastic physics stand alone
!
! Usage:    call compns_stochy (me,deltim,nlunit, stochy_namelist,iret)
!   Input Arguments:
!     deltim   - real timestep in seconds
!   Output Arguments:
!     iret     - integer return code (0 if successful or
!                between 1 and 8 for which rule above was broken)
!     stochy_namelist
!
! Attributes:
!   Language: Fortran 90
!
!$$$

      
      use stochy_namelist_def
      
      implicit none

 
      character (len=*), intent(in) :: stochy_namelist
      integer,intent(out)           :: iret
      integer,intent(in)           :: nlunit,me
      real,intent(inout)     :: deltim
      real tol
      integer k

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      namelist /nam_stochy/ntrunc,lon_s,lat_s,sppt,sppt_tau,sppt_lscale,sppt_logit, &
      vcamp,vc_lscale,vc_tau,vc_logit,iseed_shum,iseed_sppt,shum,shum_tau,& 
      shum_lscale,iseed_vc, fhstoch,stochini,vc,skeb_varspect_opt,sppt_sfclimit, &
      skeb,skeb_tau,skeb_lscale,iseed_skeb,skeb_vfilt,skeb_diss_smooth, &
      skeb_sigtop1,skeb_sigtop2,sppt_sigtop1,sppt_sigtop2,&
      shum_sigefold,vc_sigtop1,vc_sigtop2,skebint,weight_file
       tol=0.01  ! tolerance for calculations
!     spectral resolution defintion
      ntrunc=-999
      lon_s=-999
      lat_s=-999
      ! can specify up to 5 values for the stochastic physics parameters 
      ! (each is an array of length 5)
      sppt             = -999.  ! stochastic physics tendency amplitude
      shum             = -999.  ! stochastic boundary layer spf hum amp   
      skeb             = -999.  ! stochastic KE backscatter amplitude
      vcamp            = -999.  ! stochastic vorticity confinment amp
! logicals
      do_sppt = .false.
      do_shum = .false.
      do_skeb = .false.
      do_vc   = .false.
! number of passes of 1-2-1 vertical filter
! for SKEB random patterns.
      skeb_vfilt       = 0
      skebint          = 0
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
      vc_sigtop1 = 0.1
      vc_sigtop2 = 0.025
      shum_sigefold = 0.2
! reduce amplitude of sppt near surface (lowest 2 levels)
      sppt_sfclimit = .false.
! gaussian or power law variance spectrum for skeb (0: gaussian, 1:
! power law). If power law, skeb_lscale interpreted as a power not a
! length scale.
      skeb_varspect_opt = 0
      vc                = 0.      ! deterministic vorticity confinement parameter.
      sppt_logit        = .false. ! logit transform for sppt to bounded interval [-1,+1]
      vc_logit          = .false. ! logit transform for vc to bounded interval [-1,+1]
      fhstoch           = -999.0  ! forecast hour to dump random patterns
      stochini          = .false. ! true= read in pattern, false=initialize from seed
      weight_file       = 'NOT USED'

      if (me == 0) print *,' nlunit=',nlunit,' stochy_namelist=',stochy_namelist
      open(unit=nlunit,file=stochy_namelist,status='old',iostat=iret)
      if (iret.EQ.0) then
      rewind (nlunit)
      read(nlunit,nam_stochy)

      if (me == 0) print *,' in compns_stochy'

! PJP stochastic physics additions
      IF (sppt(1) > 0 ) THEN
        do_sppt=.true.
      ENDIF
      IF (shum(1) > 0 ) THEN
        do_shum=.true.
!     shum parameter has units of 1/hour, to remove time step
!     dependence.
!     change shum parameter units from per hour to per timestep
         DO k=1,5
            IF (shum(k) .gt. 0.0) shum(k)=shum(k)*deltim/3600.0
         ENDDO
      ENDIF
      IF (skeb(1) > 0 ) THEN
         do_skeb=.true.
!      adjust skeb values for resolution.
!      scaling is such that a value of 1.0 at T574 with a 900 second
!      timestep produces well-calibrated values of forecast spread.
         DO k=1,5
            IF  (skeb(k) .gt. 0.0) THEN
               skeb(k)=skeb(k)*deltim/(ntrunc*(ntrunc+1))*365765.0  ! 365765 is a scale factor so the base SKEB value in the namelist is 1.0
            ENDIF
         ENDDO
      ENDIF
!    compute frequencty to estimate dissipation timescale
      IF (skebint == 0.) skebint=deltim
      nsskeb=nint(skebint/deltim)                              ! skebint in seconds
      IF(nsskeb<=0 .or. abs(nsskeb-skebint/deltim)>tol) THEN
         WRITE(0,*) "SKEB interval is invalid",skebint
        iret=9
        return
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF (vc > tiny(vc) .or. vcamp(1) > 0 ) THEN
        do_vc=.true.
      ENDIF
!
!  All checks are successful.
!
      iret = 0
      endif
!
      return
      end subroutine compns_stochy   
