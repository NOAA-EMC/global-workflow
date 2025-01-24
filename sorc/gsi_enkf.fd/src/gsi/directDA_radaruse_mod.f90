MODULE directDA_radaruse_mod
! module for direct radar DA capability

!$$$   module documentation block
!$$$   end documentation block

  use kinds, only: r_kind,i_kind, r_double
  use constants, only: one, zero
  use mpimod, only: mype
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_radaruse_directDA
  public :: coef4dbzfwrd
  public :: init_mm_qnr
  public :: gammaDP
! set passed variables to public
  public :: l_use_rw_columntilt, l_use_dbz_directDA
  public :: oe_rw, oe_dbz
  public :: refl_lowbnd_rw, refl_lowbnd_dbz
  public :: be_q, hscl_q, vscl_q
  public :: be_t, hscl_t, vscl_t
  public :: be_sf, hscl_sf, vscl_sf
  public :: be_vp, hscl_vp, vscl_vp
  public :: be_qr, be_qs, be_qg, hscl_qx, vscl_qx
  public :: l_decouple_sf_vp, l_decouple_sf_tps           
  public :: l_set_be_rw, l_set_be_dbz
  public :: l_set_oerr_ratio_rw, l_set_oerr_ratio_dbz
  public :: rw_obs4wrd_bmwth
  public :: l_correct_azmu, l_correct_tilt, i_correct_tilt, l_azm_east1st
  public :: l_plt_be_stats, l_be_T_dep, l_use_cvpqx, cvpqx_pval
  public :: l_gpht2gmht
  public :: lvldbg
  public :: l_plt_diag_rw, l_chk_bmwth    ! moreopts
  public :: i_melt_snow, i_melt_graupel
  public :: cld_cv, cld_nt_updt
  public :: i_w_updt
  public :: l_cvpnr, cvpnr_pval
  public :: l_use_tdep_radarz

  public :: Cr,     Pr
  public :: Cs_dry, Ps_dry, Cs_wet, Ps_wet
  public :: Cg_dry, Pg_dry, Cg_wet, Pg_wet

  real(r_kind)      :: oe_rw, oe_dbz
  real(r_kind)      :: refl_lowbnd_rw, refl_lowbnd_dbz
  real(r_kind)      :: be_sf, hscl_sf, vscl_sf
  real(r_kind)      :: be_vp, hscl_vp, vscl_vp
  real(r_kind)      :: be_t,  hscl_t,  vscl_t
  real(r_kind)      :: be_q,  hscl_q,  vscl_q
  real(r_kind)      :: be_qr, be_qs, be_qg, hscl_qx, vscl_qx

! re-set background error statisitics for radar wind/reflectivity assimilation
  logical           :: l_set_be_rw, l_set_be_dbz
! re-set obs error (inflation ratio) for radar wind/reflectivity assimilation
  logical           :: l_set_oerr_ratio_rw, l_set_oerr_ratio_dbz

! logical, save   :: l_qltn
  logical :: l_use_cvpqx                 ! do power transformation of qx(qr/qs/qg)
  real(r_kind) :: cvpqx_pval       ! parameter p value for power transformation of qx(qr/qs/qg)
  logical :: l_use_rw_columntilt         ! assimilation of radar radial wind(rw) column-tilt data
  logical :: l_use_dbz_directDA          ! assimilation of radar reflectivity(dbz) data using direct DA capability
  logical :: l_decouple_sf_vp            ! de-couple balance correlation coef.  between s.f. and t, ps
  logical :: l_decouple_sf_tps           ! de-couple balance correlation coef.  between s.f. and v.p.
  logical :: l_be_T_dep                  ! temperature dependent error variance

! options for checking-up and diagnose of radial wind (used in setuprw and read_radar)
  logical   :: l_plt_diag_rw             ! moreopts
  logical   :: l_chk_bmwth               ! moreopts
  integer(i_kind) :: rw_obs4wrd_bmwth        ! beam width impact on radar wind obs forward operator
                                             ! 1: GSI original (vrminmax)
                                             ! 2: simple vertical interpolation
                                             ! 3: weighted average of multiple-layers

  logical   :: l_dbz4wrd_melt            ! effect of melting snow in dbz obs 4wrd operator
                                         ! .TRUE. : considering melting snow, and graupel

  integer(i_kind) :: i_melt_snow         ! control the melting effect in dbz obs forward operator for snow
                                         ! < 0 : no melting, and keeping dry at any temperature
                                         ! >=0 :    melting depends on temperature ! (273.15 K)
                                         ! =100:    melting and keeping wet at any temperature

  integer(i_kind) :: i_melt_graupel      ! control the melting effect in dbz obs forward operator for graupel
                                         ! < 0 : no melting, and keeping dry all the time
                                         ! >=0 :    melting depends on temperature ! (273.15 K)
                                         ! =100:    melting and keeping wet at any temperature

  integer(i_kind) :: cld_cv              ! cloud hydrometers used as control variables in analysis

  integer(i_kind) :: cld_nt_updt         ! cloud hydrometer number concentration 
                                         ! 0: no update to number concentration
                                         ! 1: updated through analysis (for now, only in hybrid analysis)
                                         ! 2: re-initialized after analysis with mixing-ratioi(qx) and intercept parameter(N0)
  integer(i_kind) :: i_w_updt            ! w (vertical velocity) is analysis variable and updated
                                         ! 0: not analyzed ; 1: analyzed

  logical   :: l_cvpnr                   ! power tranform for qnr
  real(r_kind) :: cvpnr_pval             ! power value for qnr

  logical   :: l_use_tdep_radarz         ! use temeperature dependent features in radarZ
                                         ! if it is set as .TRUE., GSI provide air temperature to radarZ
                                         ! if not used, ta is set as constant (273.16K)
                                         ! Affects only for TM operator in EnKF

! options for correction of azimuth and tilt angles of radar observations (used in read_radar.f90)
  logical   :: l_correct_azmu
  logical   :: l_correct_tilt
  integer(i_kind) :: i_correct_tilt          ! option for algorithm to compute corrected tilt
                                             ! 1. equations used in GSI;
                                             ! 2. equations used in ARPS
  logical   :: l_azm_east1st                 ! change azimuth to east as 0 before correct it.

  logical   :: l_plt_be_stats                ! output background error statistics for plot

  logical   :: l_gpht2gmht                   ! convert goepotential height to geometric height (used in setupdbz)

  integer(i_kind) :: lvldbg

! coefficients and raise-to-power inder numebrs used in 
! reflectivity obs forward operator equations (for single moment MP scheme)
  real(r_kind)  :: Cr        ! coefficient for rain drop
  real(r_kind)  :: Cs_dry    !             for dry snow
  real(r_kind)  :: Cs_wet    !             for wet snow (melting)
  real(r_kind)  :: Cg_dry    !             for drygraupel/hail
  real(r_kind)  :: Cg_wet    !             for drygraupel/hail
  real(r_kind)  :: Pr        ! power index for rain drop
  real(r_kind)  :: Ps_dry    !             for dry snow
  real(r_kind)  :: Ps_wet    !             for wet snow (melting)
  real(r_kind)  :: Pg_dry    !             for drygraupel/hail
  real(r_kind)  :: Pg_wet    !             for drygraupel/hail

contains
  subroutine init_radaruse_directDA
!   initialization of the options for using radar obs
    implicit none

    l_use_rw_columntilt = .FALSE.           ! (not) use radar radial wind column-tilt data
    l_use_dbz_directDA  = .FALSE.           ! (not) use radar reflectivity data of directDA
    l_set_be_rw         = .FALSE.           ! (not) re-set backgroudn error statistics for sf/vp/t/q (for radar wind assimilation)
    l_set_be_dbz        = .FALSE.           ! (not) re-set backgroudn error statistics for qr/qs/qg (for reflectivity assimilation)
    l_set_oerr_ratio_rw = .FALSE.           ! (not) re-set obs error (inflation ratio in setuprw) for radar wind obs
    l_set_oerr_ratio_dbz= .FALSE.           ! (not) re-set obs error (inflation ratio in setupdbz) for reflectivity obs
    l_decouple_sf_vp    = .FALSE.           ! (not) de-couple/zero-out balance between sf and vp
    l_decouple_sf_tps   = .FALSE.           ! (not) de-couple/zero-out balance among   sf and t/ps
    l_use_cvpqx         = .FALSE.           ! (not) use power transform to qx(qr/qs/qg) 
    cvpqx_pval          = 0.000001_r_kind   ! power transform value; nearly the same as use log transform to qx(qr/qs/qg) 

    l_plt_be_stats      = .TRUE.            ! output BE stats for plot
    l_be_T_dep          = .FALSE.

    oe_dbz              = 1.0_r_kind        ! observerion error of radar reflectivity (dbz)
    oe_rw               = 1.0_r_kind        ! observerion error of radar radial wind obs (m/s)

    refl_lowbnd_dbz     = 0.0_r_kind        ! lower-bound of obs dbz for dbz assimilation
                                            ! (if obs_dbz < dbz_lowbnd_dbz, then this obs_dbz is rejected for dbz assimilation)
    refl_lowbnd_rw      = 5.0_r_kind        ! lower-bound of obs dbz for rw assimilation
                                            ! (if obs_dbz < dbz_lowbnd_dbz, then the rw (wind) obs accompanied with
                                            ! this obs_dbz  is rejected for rw (wind) assimilation )

!   multiplying factor to tune the background error standard deviation of stream function (s.f.)
    be_sf       = 0.2_r_kind/4.5_r_kind     
    hscl_sf     = 20000.0_r_kind            ! horizontal correlation length scale of s.f. (meter)
    vscl_sf     = 1.5_r_kind                ! vertical   correlation length scale of s.f.

!   multiplying factor to tune the background error standard deviation of velocity potential (v.p.) 
    be_vp       = 0.2_r_kind/4.5_r_kind
    hscl_vp     = 20000.0_r_kind            ! horizontal correlation length scale of v.p. (meter)
    vscl_vp     = 1.5_r_kind                ! vertical   correlation length scale of v.p.

    be_t        = -1.0_r_kind               ! multiplying factor to tune the background error standard deviation of temperature (t)
    hscl_t      = -20000.0_r_kind           ! horizontal correlation length scale of t (meter)
    vscl_t      = -1.5_r_kind               ! vertical   correlation length scale of t

!   multiplying factor to tune the background error standard deviation of moisture mixing ration (q) 
    be_q        = -1.0_r_kind
    hscl_q      = -20000.0_r_kind           ! horizontal correlation length scale of q (meter)
    vscl_q      = -1.5_r_kind               ! vertical   correlation length scale of q

    be_qr       =  1.0E-3_r_kind            ! background error standard deviation for mixing ratio of rain water (kg/kg)
    be_qs       =  1.0E-3_r_kind            ! background error standard deviation for mixing ratio of snow water (kg/kg)
    be_qg       =  1.0E-3_r_kind            ! background error standard deviation for mixing ratio of graupel    (kg/kg)
    hscl_qx     =  6000.0_r_kind            ! horizontal correlation length scale for mixing ratio of cloud hydrometers (meter)
    vscl_qx     =  1.5_r_kind               ! vertical   correlation length scale for mixing ratio of cloud hydrometers

    l_plt_diag_rw   = .FALSE.               ! moreopts  
    l_chk_bmwth     = .FALSE.               ! moreopts 
    rw_obs4wrd_bmwth    = 2                 ! 2-layers vertical interpolation used in radar wind obs forward operator 

    i_melt_snow     =   0                   ! snow melting depends on temperature
                                            ! < 0 : dry ;  100: wet;   >=0:  melting at 273.15
    i_melt_graupel  =   0                   ! graupel melting depends on temperature
                                            ! < 0 : dry ;  100: wet;   >=0:  melting at 273.15+/-2.5

    l_correct_azmu = .true.
    l_correct_tilt = .true.
    l_azm_east1st  = .true.  ! true  : change azimuth to east as 0 first
                             ! false : change azimuth to east as 0 secondly after correct azimuth
    i_correct_tilt = 2       ! 1. equations used in GSI;
                             ! 2. equations used in ARPS

    l_gpht2gmht = .false.

    lvldbg      = 0

    Cr          = zero
    Pr          = zero
    Cs_dry      = zero
    Ps_dry      = zero
    Cs_wet      = zero
    Ps_wet      = zero
    Cg_dry      = zero
    Pg_dry      = zero
    Cg_wet      = zero
    Pg_wet      = zero

    cld_cv      = 0

    cld_nt_updt = 1          ! number concentration is NOT re-initialized after analsysis
                             ! 0: no update to number concentration
                             ! 1: updated through analysis (for now, only in hybrid analysis)
                             ! 2: re-initialized after analysis with mixing-ratioi(qx) and intercept parameter(N0)

    i_w_updt    = 0          ! w is not analyzed and not updated
                             ! 0: not analyzed ; 1: analyzed

    l_cvpnr = .FALSE.
    cvpnr_pval = 0.6_r_kind

    l_use_tdep_radarz= .TRUE.  

  return
  end subroutine init_radaruse_directDA

! subroutine to calaculate the coefficients used in dbz obs forward operator for
! single moment Lin scheme and WSM6 scheme

  subroutine coef4dbzfwrd(mphyopt,iret)
!----------------------------------------------------------------------!
!
! purpose:
! this subroutine is based on the program to calcualte coefficients for
! reflecivity formula used in arps3dvar and arpsenkf (Mingjing Tong's version)
! ref. Smith et al. 1975
! It is modified to work with WSM6 considering the density of graupel, instead
! of the density of hail in Lin scheme.
!
! prgmmr:
!   2018-02-19  g.zhao  initialization of the code
!----------------------------------------------------------------------!
!     use mpimod, only: mype

      implicit none

      integer(i_kind),  intent(in   )   :: mphyopt   ! microphy scheme
      integer(i_kind),  intent(inout)   :: iret     ! status code (0: normal;  -1: error)

!-----------------------------------------------------------------------
! Declare local parameters.
!-----------------------------------------------------------------------

      REAL(r_kind),PARAMETER :: ki2 = 0.176_r_kind ! Dielectric factor for ice if other
                                                   ! than melted drop diameters are used.
      REAL(r_kind),PARAMETER :: kw2 = 0.93_r_kind  ! Dielectric factor for water.

      REAL(r_kind),PARAMETER :: degKtoC = 273.15_r_kind ! Conversion factor from degrees K to
                                                        ! degrees C

      REAL(r_kind),PARAMETER :: m3todBZ = 1.0E+18_r_kind ! Conversion factor from m**3 to
                                                         ! mm**6 m**-3.

      REAL(r_kind),PARAMETER :: pi = 3.1415926_r_kind    ! Pi.

      REAL(r_kind),PARAMETER :: pipowf = 7.0_r_kind/4.0_r_kind ! Power to which pi is raised.

      REAL(r_kind),PARAMETER :: N0r_0 = 8.0E+06_r_kind ! Intercept parameter in 1/(m^4) for rain.
      REAL(r_kind),PARAMETER :: N0s_0 = 3.0E+06_r_kind ! Intercept parameter in 1/(m^4) for snow.
      REAL(r_kind),PARAMETER :: N0g_0 = 4.0E+05_r_kind ! Intercept parameter in 1/(m^4) for graupel. (<=4.0E+6)
      REAL(r_kind),PARAMETER :: N0h_0 = 4.0E+04_r_kind ! Intercept parameter in 1/(m^4) for hail.

      REAL(r_kind),PARAMETER :: N0xpowf = 3.0_r_kind/4.0_r_kind ! Power to which N0r,N0s & N0h are
                                                                ! raised.

      REAL(r_kind),PARAMETER :: approxpow_0 = 0.95_r_kind ! Approximation power for hail/graupel
                                                          ! integral.

      REAL(r_kind),PARAMETER :: rqrpowf = 7.0_r_kind/4.0_r_kind ! Power to which product rho * qr
                                                                ! is raised.
      REAL(r_kind),PARAMETER :: rqsnpowf = 7.0_r_kind/4.0_r_kind ! Power to which product rho * qs
                                                                 ! is raised (dry snow).
      REAL(r_kind),PARAMETER :: rqsppowf = 7.0_r_kind/4.0_r_kind ! Power to which product rho * qs
                                                                 ! is raised (wet snow).
      REAL(r_kind),PARAMETER :: rqhnpowf = 7.0_r_kind/4.0_r_kind ! Power to which product rho * qh
                                                                 ! is raised (dry hail)

      REAL(r_kind),PARAMETER :: rhoi_0 =917._r_kind  ! Density of ice (kg m**-3)
      REAL(r_kind),PARAMETER :: rhor_0 =1000._r_kind ! Density of rain (kg m**-3)
      REAL(r_kind),PARAMETER :: rhos_0 =100._r_kind  ! Density of snow (kg m**-3)
      REAL(r_kind),PARAMETER :: rhoh_0 =913._r_kind  ! Density of hail (kg m**-3)
      REAL(r_kind),PARAMETER :: rhog_0 =400._r_kind  ! Density of graupel (kg m**-3)

      REAL(r_kind),PARAMETER :: rhoipowf = 2.0_r_kind            ! Power to which rhoi is raised.
      REAL(r_kind),PARAMETER :: rhospowf = 1.0_r_kind/4.0_r_kind ! Power to which rhos is raised.
      REAL(r_kind),PARAMETER :: rhoxpowf = 7.0_r_kind/4.0_r_kind ! Power to which rhoh is raised.

      REAL(r_kind),PARAMETER :: Zefact = 720.0_r_kind ! Multiplier for Ze components.

      REAL(r_kind),PARAMETER :: lg10mul = 10.0_r_kind ! Log10 multiplier

!-----------------------------------------------------------------------
! Declare local variables.
!-----------------------------------------------------------------------
      REAL(r_kind)           :: rqhppowf     ! Power to which product rho * qh is raised (wet hail).

      REAL(r_kind) :: Zerf,Zesnegf,Zesposf
      REAL(r_kind) :: Zehnegf,Zehposf

      REAL(r_kind)      :: N0r, N0s, N0g, N0h, N0i
      REAL(r_kind)      :: rhor, rhos, rhog, rhoh, rhoi
      REAL(r_kind)      :: approxpow

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-----------------------------------------------------------------------
! Initialization of Namelist variables
      N0r=N0r_0
      N0s=N0s_0
      N0g=N0g_0
      N0h=N0h_0
      N0i=N0r_0                 ! N_not for ice is not used
      rhor=rhor_0
      rhos=rhos_0
      rhog=rhog_0
      rhoi=rhoi_0
      rhoh=rhoh_0
      approxpow=approxpow_0
!-----------------------------------------------------------------------
      rqhppowf=(7.0_r_kind/4.0_r_kind)*approxpow      ! Power to which product rho * qh is raised (wet hail).
!-----------------------------------------------------------------------
! First gather all the constants together.  (These are treated as
! variables because Fortran 90 does not allow real exponents when
! calculating parameters).
!-----------------------------------------------------------------------

      Zerf = (m3todBZ * Zefact) /  &
          ((pi ** pipowf) * (N0r ** N0xpowf) *  &
          (rhor ** rhoxpowf))
      Zesnegf = ((m3todBZ * Zefact   * Ki2 * (rhos ** rhospowf)) /  &
          ((pi ** pipowf) * Kw2 * (N0s ** N0xpowf) *  &
          (rhoi ** rhoipowf)))
      Zesposf = ((m3todBZ * Zefact) /  &
          ((pi ** pipowf) * (N0s ** N0xpowf) *  &
          (rhos ** rhoxpowf)))

      select case (mphyopt)
         case (2,3,4)
            ! hail
            ! Zehnegf = ((m3todBZ * Zefact) * Ki2  /  &
            !    ((pi ** pipowf) * Kw2 * (N0h ** N0xpowf) *  &
            !    (rhoh ** rhoxpowf)))
            ! Zehposf = (((m3todBZ * Zefact) /  &
            !    ((pi ** pipowf) * (N0h ** N0xpowf) *  &
            !    (rhoh ** rhoxpowf))) ** approxpow)
            ! iret = 0
          
            ! set this values in default / 20200420
            ! changed the density of hail to graupel to make it consistent with WRF Lin scheme
            Zehnegf = ((m3todBZ * Zefact) * Ki2  /  &
                ((pi ** pipowf) * Kw2 * (4.0E+06_r_kind ** N0xpowf) *  &
                (400._r_kind ** rhoxpowf)))
            Zehposf = (((m3todBZ * Zefact) /  &
                ((pi ** pipowf) * (4.0E+06_r_kind ** N0xpowf) *  &
                (400._r_kind ** rhoxpowf))) ** approxpow)
            iret = 0
         case (5,6,7)
            ! graupel
            Zehnegf = ((m3todBZ * Zefact) * Ki2  /  &
                ((pi ** pipowf) * Kw2 * (N0g ** N0xpowf) *  &
                (rhog ** rhoxpowf)))
            Zehposf = (((m3todBZ * Zefact) /  &
                ((pi ** pipowf) * (N0g ** N0xpowf) *  &
                (rhog ** rhoxpowf))) ** approxpow)
            iret = 0
         case default
            write(6,*) ' subroutine COEF4DBZFWRD: warning --> invalid mphyopt for single moment scheme'
            iret = -1
      end select

!     coefficients used in dbz formula
      Cr        = Zerf
      Cs_dry    = Zesnegf                            ! <= 273.15K
      Cs_wet    = Zesposf                            ! >  273.15K (melting)
      Cg_dry    = Zehnegf                            ! <= 273.15K
      Cg_wet    = Zehposf                            ! >  273.15K (melting)
!     raise-to-power index numbers used in dbz formula
      Pr        = rqrpowf
      Ps_dry    = rqsnpowf                           ! <= 273.15K
      Ps_wet    = rqsppowf                           ! >  273.15K (melting)
      Pg_dry    = rqhnpowf                           ! <= 273.15K
      Pg_wet    = rqhppowf                           ! >  273.15K (melting)

       if (mype == 0) then
          WRITE(6,*)'*****************************************************************'
          write(6,*)'COEF4DBZFWRD: mphyopt==',mphyopt
          write(6,*)'COEF4DBZFWRD: rain:    ',' Cr    =',Cr,    '  Pr    =',Pr
          write(6,*)'COEF4DBZFWRD: snow:    ',' Cs_dry=',Cs_dry,'  Ps_dry=',Ps_dry
          write(6,*)'COEF4DBZFWRD:          ',' Cs_wet=',Cs_wet,'  Ps_wet=',Ps_wet
          write(6,*)'COEF4DBZFWRD: graupel: ',' Cg_dry=',Cg_dry,'  Pg_dry=',Pg_dry
          write(6,*)'COEF4DBZFWRD:          ',' Cg_wet=',Cg_wet,'  Pg_wet=',Pg_wet
          WRITE(6,*)'*****************************************************************'
      end if

      return

  end subroutine coef4dbzfwrd

  subroutine init_mm_qnr(rho,qx,qntx)
!
!--------------------------------------------------------------------------------------------------!
!   g.zhao  2018-03-08
!
!   based on SUBROUTINE INIT_MM in initlib3d.f90 of ARPS pacakge
!   to diagnose number concentration for rain water with aii density and mixing
!   ratio of rain water
!--------------------------------------------------------------------------------------------------!
!
      use radarz_cst, only: mphyopt

      implicit none

      real(r_kind),    intent(in   )   :: rho     ! air density
      real(r_kind),    intent(in   )   :: qx      ! mixing ratio of rain water
      real(r_kind),    intent(  out)   :: qntx    ! number concentration

! -- local parameters
!    fixed intercept parameters for rain, snow, graupel and hail
      REAL(r_kind),PARAMETER :: N0r_0 = 8.0E+06_r_kind ! Intercept parameter in 1/(m^4) for rain.
      REAL(r_kind),PARAMETER :: N0s_0 = 3.0E+06_r_kind ! Intercept parameter in 1/(m^4) for snow.
      REAL(r_kind),PARAMETER :: N0g_0 = 4.0E+05_r_kind ! Intercept parameter in 1/(m^4) for graupel. (<=4.0E+6)
      REAL(r_kind),PARAMETER :: N0h_0 = 4.0E+04_r_kind ! Intercept parameter in 1/(m^4) for hail.
      
      REAL(r_kind),PARAMETER :: rhoi_0 = 917._r_kind  ! Density of ice (kg m**-3)
      REAL(r_kind),PARAMETER :: rhor_0 = 1000._r_kind ! Density of rain (kg m**-3)
      REAL(r_kind),PARAMETER :: rhos_0 = 100._r_kind  ! Density of snow (kg m**-3)
      REAL(r_kind),PARAMETER :: rhoh_0 = 913._r_kind  ! Density of hail (kg m**-3)
      REAL(r_kind),PARAMETER :: rhog_0 = 400._r_kind  ! Density of graupel (kg m**-3)

      REAL(r_kind),PARAMETER :: pi = 3.1415926_r_kind ! Pi.

! -- local variables
      REAL(r_kind)  :: alpharain
      REAL(r_kind)  :: N0r
      REAL(r_kind)  :: rhor

!--------------------------------------------------------------------------------------------------!

      if (mphyopt == 5) then
          alpharain = 1.0_r_kind
      else
          alpharain = 0.0_r_kind
      end if

      N0r = N0r_0
      rhor = rhor_0

      qntx = sngl(gammaDP(1._r_double+dble(alpharain)))*(N0r**(3._r_kind/(4._r_kind+alpharain)))* &
             (rho*qx/((pi/6._r_kind)*rhor* &
             sngl(gammaDP(4._r_double+dble(alpharain)))))**((1._r_kind+alpharain)/(4._r_kind+alpharain))
      return
  end subroutine init_mm_qnr

!=======================================================================

  FUNCTION gammaDP(xx)

!---------------------------------------------------------------!
!
!  Modified from "Numerical Recipes" (by original programer)
!     Copied from my3mom_fncs_mod.f90 in arps package (g.zhao)
!---------------------------------------------------------------!

      IMPLICIT NONE

! PASSING PARAMETERS:
      REAL(r_double), INTENT(IN) :: xx

! LOCAL PARAMETERS:
      REAL(r_double)    :: gammaDP
      REAL(r_double)    :: ser,stp,tmp,x,y,cof(6)
      INTEGER(i_kind)   :: j


      SAVE cof,stp
      DATA cof,stp/76.18009172947146_r_double,-86.50532032941677_r_double,               &
          24.01409824083091_r_double,-1.231739572450155_r_double,.1208650973866179e-2_r_double,  &
          -.5395239384953e-5_r_double,2.5066282746310005_r_double/
      x=xx
      y=x
      tmp=x+5.5_r_double
      tmp=(x+0.5_r_double)*log(tmp)-tmp
      ser=1.000000000190015_r_double
      do j=1,4
          y=y+1._r_double
          ser=ser+cof(j)/y
      enddo
      gammaDP=tmp+log(stp*ser/x)
      gammaDP= exp(gammaDP)

  END FUNCTION gammaDP
!=======================================================================

end module directDA_radaruse_mod

