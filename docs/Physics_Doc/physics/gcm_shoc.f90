
! Implementation of the Simplified High Order Closure (SHOC) scheme 
! of Bogenschutz and Krueger (2013), J. Adv. Model. Earth Syst, 5, 195-211,
! doi: 10.1002/jame.200118. (further referred to as BK13)
! in a single column form suitable for use in a GCM physics package. 
! Alex Belochitski, heavily based on the code of Peter Bogenschutz.
! S Moorthi - optimization, cleanup, improve and customize for gsm
!           - improved solution for sgs-tke equation
! S Moorthi - 05-11-17 - modified shear production term to eliminate
!                        spurious tke ove Antarctica.
! S Moorthi - 01-12-17 - added extra pressure dependent tke dissipation at
!                        pressures below a critical value pcrit
! S Moorthi - 04-12-17 - fixed a bug in the definition of hl on input
!                        replacing fac_fus by fac_sub



 subroutine shoc(ix, nx, ny, nzm, nz, dtn, me, lat,              &
                 prsl, phii, phil, u, v, omega, tabs,            &
                 qwv, qi, qc, qpi, qpl, rhc, supice, pcrit,      &
                 cld_sgs, tke, hflx, evap, prnum, tkh,           &
                 wthv_sec, lprnt, ipr, ncpl, ncpi)

  use funcphys , only : fpvsl, fpvsi, fpvs    ! saturation vapor pressure for water & ice

! Map constants of the NCEP GFS to those of SHOC
 
  use physcons, cp    => con_cp,      & ! Specific heat of air, J/kg/K
                ggr   => con_g,       & ! Gravity acceleration, m/s2
                lcond => con_hvap,    & ! Latent heat of condensation, J/kg
                lfus  => con_hfus,    & ! Latent heat of fusion, J/kg
                rv    => con_rv,      & ! Gas constant for water vapor, J/kg/K
                rgas  => con_rd,      & ! Gas constant for dry air, J/kg/K
                pi    => con_pi,      & ! Pi    
                epsv  => con_fvirt

  implicit none

  real, parameter :: lsub = lcond+lfus, fac_cond = lcond/cp, fac_fus = lfus/cp,      &
                     cpolv = cp/lcond,                                               &
                     fac_sub = lsub/cp, ggri = 1.0/ggr,      kapa = rgas/cp,         &
                     gocp = ggr/cp,     rog = rgas*ggri,     sqrt2 = sqrt(2.0),      &
                     sqrtpii = 1.0/sqrt(pi+pi), epsterm = rgas/rv, twoby3 = 2.0/3.0, &
                     onebeps = 1.0/epsterm, twoby15 = 2.0 / 15.0,                    &
!                    onebrvcp= 1.0/(rv*cp), skew_facw=1.2, skew_fact=1.0,            &
                     onebrvcp= 1.0/(rv*cp), skew_facw=1.2, skew_fact=0.0,            &
!                    tkef1=0.5, tkef2=1.0-tkef1, tkhmax=1000.0, cefac=3.0,           &
                     tkef1=0.5, tkef2=1.0-tkef1, tkhmax=1000.0, cefac=1.5,           &
!                    tkef1=0.7, tkef2=1.0-tkef1, tkhmax=1000.0, cefac=1.5,           &
                     zero=0.0,  one=1.0,  half=0.5, two=2.0,    eps=0.622,           &
                     three=3.0, oneb3=one/three,                                     &
                     scrit=2.0e-6

!                    scrit=5.0e-8
!                    scrit=3.0e-6
!                    scrit=1.0e-5
!                    scrit=5.0e-6
!                    scrit=1.0e-5
!                    scrit=1.0e-6
!                    skew_facw=1.2, skew_fact=0.5
!                    onebeps = 1.0/epsterm, twoby15 = 2.0 / 15.0, skew_facw=1.2  ! orig

! real, parameter :: supice=1.05

  logical lprnt
  integer ipr
  integer, intent(in) :: ix    ! max number of points in the physics window in the x
  integer, intent(in) :: nx    ! Number of points in the physics window in the x
  integer, intent(in) :: ny    ! and y directions
  integer, intent(in) :: me    ! MPI rank
  integer, intent(in) :: lat   ! latitude

  integer, intent(in) :: nzm   ! Number of vertical layers
  integer, intent(in) :: nz    ! Number of layer interfaces  (= nzm + 1)   
  real,    intent(in) :: dtn   ! Physics time step, s 
  real,    intent(in) :: pcrit ! pressure in Pa below which additional tke 
                               ! dissipation is applied
  real,    intent(in) :: hflx(nx)
  real,    intent(in) :: evap(nx)

! The interface is talored to GFS in a sense that input variables are 2D

  real, intent(in)    :: prsl   (ix,ny,nzm)   ! mean layer presure   
  real, intent(in)    :: phii   (ix,ny,nz )   ! interface geopotential height
  real, intent(in)    :: phil   (ix,ny,nzm)   ! layer geopotential height  
  real, intent(in)    :: u      (ix,ny,nzm)   ! u-wind, m/s
  real, intent(in)    :: v      (ix,ny,nzm)   ! v-wind, m/s
  real, intent(in)    :: omega  (ix,ny,nzm)   ! omega, Pa/s
  real, intent(inout) :: tabs   (ix,ny,nzm)   ! temperature, K
  real, intent(inout) :: qwv    (ix,ny,nzm)   ! water vapor mixing ratio, kg/kg
  real, intent(inout) :: qc     (ix,ny,nzm)   ! cloud water mixing ratio, kg/kg
  real, intent(inout) :: qi     (ix,ny,nzm)   ! cloud ice   mixing ratio, kg/kg
! Anning Cheng 03/11/2016 SHOC feedback to number concentration
  real, intent(inout) :: ncpl   (nx,ny,nzm)   ! cloud water number concentration,/m^3
  real, intent(inout) :: ncpi   (nx,ny,nzm)   ! cloud ice   number concentration,/m^3
  real, intent(inout) :: qpl    (nx,ny,nzm)   ! rain mixing ratio, kg/kg
  real, intent(inout) :: qpi    (nx,ny,nzm)   ! snow mixing ratio, kg/kg
  real, intent(inout) :: rhc    (nx,ny,nzm)   ! critical relative humidity
  real, intent(in)    :: supice               ! ice supersaturation parameter
  real, intent(inout) :: cld_sgs(ix,ny,nzm)   ! sgs cloud fraction
! real, intent(inout) :: cld_sgs(nx,ny,nzm)   ! sgs cloud fraction
  real, intent(inout) :: tke    (ix,ny,nzm)   ! turbulent kinetic energy. m**2/s**2
! real, intent(inout) :: tk     (nx,ny,nzm)   ! eddy viscosity
  real, intent(inout) :: tkh    (ix,ny,nzm)   ! eddy diffusivity
  real, intent(inout) :: prnum  (nx,ny,nzm)   ! turbulent Prandtl number
  real, intent(inout) :: wthv_sec (ix,ny,nzm) ! Buoyancy flux, K*m/s

! SHOC tunable parameters

  real, parameter :: lambda  = 0.04
! real, parameter :: min_tke = 1e-6  ! Minumum TKE value, m**2/s**2 
  real, parameter :: min_tke = 1e-4  ! Minumum TKE value, m**2/s**2 
  real, parameter :: max_tke = 100.0 ! Maximum TKE value, m**2/s**2 
! Maximum turbulent eddy length scale, m
! real, parameter :: max_eddy_length_scale  = 2000. 
  real, parameter :: max_eddy_length_scale  = 1000. 
! Maximum "return-to-isotropy" time scale, s
  real, parameter :: max_eddy_dissipation_time_scale = 2000.  
  real, parameter :: Pr    = 1.0             ! Prandtl number
! real, parameter :: Prnum = 1.0             ! Prandtl number

! Constants for the TKE dissipation term based on Deardorff (1980)
! real, parameter :: pt19=0.19, pt51=0.51, pt01=0.01
  real, parameter :: pt19=0.19, pt51=0.51, pt01=0.01, atmin=0.10, atmax=one-atmin
  real, parameter :: Cs  = 0.15, epsln=1.0e-6
  real, parameter :: Ck  = 0.1     ! Coeff in the eddy diffusivity - TKE relationship, see Eq. 7 in BK13 
! real, parameter :: Ce  = Ck**3/(0.7*Cs**4) 
! real, parameter :: Ce  = Ck**3/(0.7*Cs**4) * 2.2 
! real, parameter :: Ce  = Ck**3/(0.7*Cs**4) * 3.0 , Ces = Ce 
! real, parameter :: Ce  = Ck**3/(0.7*Cs**4) * 2.5 , Ces = Ce * 3.0 / 2.5 
! real, parameter :: Ces = Ce/0.7*3.0

! real, parameter :: Ce  = Ck**3/(0.7*Cs**4), Ces = Ce*3.0/0.7 ! Commented Moor

  real, parameter :: Ce  = Ck**3/Cs**4, Ces = Ce*3.0/0.7

! real, parameter :: vonk=0.35  ! Von Karman constant
  real, parameter :: vonk=0.4   ! Von Karman constant Moorthi - as in GFS
  real, parameter :: tscale=400.! time scale set based off of similarity results of BK13, s
  real, parameter :: w_tol_sqd = 4.0e-04   ! Min vlaue of second moment of w
! real, parameter :: w_tol_sqd = 1.0e-04   ! Min vlaue of second moment of w
  real, parameter :: w_thresh  = 0.0, thresh = 0.0


! These parameters are a tie-in with a microphysical scheme
! Double check their values for the Zhao-Carr scheme.
  real, parameter :: tbgmin = 258.16    ! Minimum temperature for cloud water., K (ZC)
! real, parameter :: tbgmin = 253.16    ! Minimum temperature for cloud water., K
  real, parameter :: tbgmax = 273.16    ! Maximum temperature for cloud ice, K
  real, parameter :: a_bg   = one/(tbgmax-tbgmin)
!
! Parameters to tune the second order moments-  No tuning is performed currently

  real, parameter :: thl2tune = 1.0,  qw2tune = 1.0,  qwthl2tune = 1.0,   &
!                    thl_tol  = 1.e-4, rt_tol = 1.e-8, basetemp  = 300.0
                     thl_tol  = 1.e-2, rt_tol = 1.e-4, basetemp  = 300.0

  integer, parameter :: nitr=6

! Local variables. Note that pressure is in millibars in the SHOC code.

  real zl      (nx,ny,nzm)  ! height of the pressure levels above surface, m
  real zi      (nx,ny,nz)   ! height of the interface levels, m
  real adzl    (nx,ny,nzm)  ! layer thickness i.e. zi(k+1)-zi(k) - defined at levels
  real adzi    (nx,ny,nz)   ! level thickness i.e. zl(k)-zl(k-1) - defined at interface
 
  real hl      (nx,ny,nzm)  ! liquid/ice water static energy , K
  real qv      (nx,ny,nzm)  ! water vapor, kg/kg
  real qcl     (nx,ny,nzm)  ! liquid water  (condensate), kg/kg
  real qci     (nx,ny,nzm)  ! ice water  (condensate), kg/kg
  real w       (nx,ny,nzm)  ! z-wind, m/s
  real bet     (nx,ny,nzm)  ! ggr/tv0
  real gamaz   (nx,ny,nzm)  ! ggr/cp*z

! Moments of the trivariate double Gaussian PDF for the SGS total water mixing ratio
! SGS liquid/ice static energy, and vertical velocity

  real qw_sec   (nx,ny,nzm) ! Second moment total water mixing ratio, kg^2/kg^2
  real thl_sec  (nx,ny,nzm) ! Second moment liquid/ice static energy, K^2
  real qwthl_sec(nx,ny,nzm) ! Covariance tot. wat. mix. ratio and static energy, K*kg/kg
  real wqw_sec  (nx,ny,nzm) ! Turbulent flux of tot. wat. mix., kg/kg*m/s
  real wthl_sec (nx,ny,nzm) ! Turbulent flux of liquid/ice static energy, K*m/s
  real w_sec    (nx,ny,nzm) ! Second moment of vertical velocity, m**2/s**2
  real w3       (nx,ny,nzm) ! Third moment of vertical velocity, m**3/s**3
  real wqp_sec  (nx,ny,nzm) ! Turbulent flux of precipitation, kg/kg*m/s

! Eddy length formulation 
  real smixt    (nx,ny,nzm) ! Turbulent length scale, m
  real isotropy (nx,ny,nzm) ! "Return-to-isotropy" eddy dissipation time scale, s
! real isotropy_debug (nx,ny,nzm) ! Return to isotropy scale, s without artificial limits
  real brunt    (nx,ny,nzm) ! Moist Brunt-Vaisalla frequency, s^-1
  real conv_vel2(nx,ny,nzm) ! Convective velocity scale cubed, m^3/s^3

! Output of SHOC
  real diag_frac, diag_qn, diag_qi, diag_ql

! real diag_frac(nx,ny,nzm) ! SGS cloud fraction
! real diag_qn  (nx,ny,nzm) ! SGS cloud+ice condensate, kg/kg
! real diag_qi  (nx,ny,nzm) ! SGS ice condensate, kg/kg
! real diag_ql  (nx,ny,nzm) ! SGS liquid condensate, kg/kg


! Horizontally averaged variables
! real conv_vel(nzm)        ! Convective velocity scale cubed, m^3/s^3
  real wqlsb   (nzm)        ! liquid water flux, kg/kg/ m/s
  real wqisb   (nzm)        ! ice flux, kg/kg m/s
! real thlv    (nzm)        ! Grid-scale level-average virtual potential temperature
!                              (not used)


! Local variables

! real, dimension(nx,ny,nzm) :: tkesbbuoy, tkesbshear, tkesbdiss, tkesbbuoy_debug   &
!                               tkebuoy_sgs, total_water, tscale1_debug, brunt2

  real, dimension(nx,ny,nzm) :: total_water, brunt2, thv, tkesbdiss
  real, dimension(nx,ny,nzm) :: def2
  real, dimension(nx,ny)     :: denom, numer, l_inf, cldarr, thedz, thedz2

  real lstarn,    depth,    omn,         betdz,      bbb,      term,   qsatt, dqsat,        &
                  conv_var,  tkes,       skew_w,     skew_qw,  aterm,  w1_1,  w1_2,  w2_1,  &
       w2_2,      w3var,     thl1_1,     thl1_2,     thl2_1,   thl2_2, qw1_1, qw1_2, qw2_1, &
       qw2_2,     ql1,       ql2,        w_ql1,      w_ql2,                                 &
       r_qwthl_1, r_wqw_1,   r_wthl_1,   testvar,    s1,    s2,    std_s1,  std_s2, C1, C2, &
       thl_first, qw_first,  w_first,    Tl1_1,      Tl1_2, betatest, pval, pkap,           &
       w2thl,     w2qw,w2ql, w2ql_1,     w2ql_2,                                            &
       thec,      thlsec,    qwsec,      qwthlsec,   wqwsec, wthlsec, thestd,dum,           &
       cqt1,      cthl1,     cqt2,       cthl2,      qn1,    qn2, qi1, qi2, omn1, omn2,     &
       basetemp2, beta1,     beta2,      qs1,        qs2,                                   &
       esval1_1,  esval2_1,  esval1_2,   esval2_2,   om1,    om2,                           &
       lstarn1,   lstarn2,   sqrtw2,     sqrtthl,    sqrtqt,                                &
       sqrtstd1,  sqrtstd2,  tsign,      tvar,       sqrtw2t, wqls, wqis,                   &
       sqrtqw2_1, sqrtqw2_2, sqrtthl2_1, sqrtthl2_2, sm,   prespot,                         &
       corrtest1, corrtest2, wrk,  wrk1, wrk2,       wrk3, onema, pfac, sfac, sfaci


  integer i,j,k,km1,ku,kd,ka,kb

! Map GFS variables to those of SHOC - SHOC operates on 3D fields
! Here a Y-dimension is added to the input variables, along with some unit conversions

  do k=1,nz
    do j=1,ny
      do i=1,nx
        zi(i,j,k) = phii(i,j,k) * ggri
      enddo
    enddo
  enddo
!
! move water from vapor to condensate if the condensate is negative
!
  do k=1,nzm
    do j=1,ny
      do i=1,nx
        if (qc(i,j,k) < zero) then
          wrk = qwv(i,j,k) + qc(i,j,k)
          if (wrk >= zero) then
            qwv(i,j,k)  = wrk
            tabs(i,j,k) = tabs(i,j,k) - fac_cond * qc(i,j,k)
            qc(i,j,k)   = zero
          else
            qc(i,j,k)   = zero
            tabs(i,j,k) = tabs(i,j,k) + fac_cond * qwv(i,j,k)
            qwv(i,j,k)  = zero
          endif
        endif
        if (qi(i,j,k) < zero) then
          wrk = qwv(i,j,k) + qi(i,j,k)
          if (wrk >= zero) then
            qwv(i,j,k)  = wrk
            tabs(i,j,k) = tabs(i,j,k) - fac_sub  * qi(i,j,k)
            qi(i,j,k)   = zero
          else
            qi(i,j,k)   = zero
            tabs(i,j,k) = tabs(i,j,k) + fac_sub  * qwv(i,j,k)
            qwv(i,j,k)  = zero
          endif
        endif
      enddo
    enddo
  enddo
             

  do k=1,nzm
    do j=1,ny
      do i=1,nx
        zl(i,j,k)    = phil(i,j,k) * ggri
        wrk          = one / prsl(i,j,k)
        qv(i,j,k)    = max(qwv(i,j,k), zero)
        thv(i,j,k)   = tabs(i,j,k) * (one+epsv*qv(i,j,k))
        w(i,j,k)     = - rog * omega(i,j,k) * thv(i,j,k) * wrk
        qcl(i,j,k)   = max(qc(i,j,k), zero)
        qci(i,j,k)   = max(qi(i,j,k), zero)
!
!       qpl(i,j,k)     = zero  ! comment or remove when using with prognostic rain/snow
!       qpi(i,j,k)     = zero  ! comment or remove when using with prognostic rain/snow
        wqp_sec(i,j,k) = zero  ! Turbulent flux of precipiation
!
        total_water(i,j,k) = qcl(i,j,k) + qci(i,j,k) + qv(i,j,k)

        prespot        = (100000.0*wrk) ** kapa ! Exner function
        bet(i,j,k)     = ggr/(tabs(i,j,k)*prespot)     ! Moorthi
        thv(i,j,k)     = thv(i,j,k)*prespot            ! Moorthi
!
! Lapse rate * height = reference temperature
        gamaz(i,j,k) = gocp * zl(i,j,k)

! Liquid/ice water static energy - ! Note the the units are degrees K
        hl(i,j,k) = tabs(i,j,k) + gamaz(i,j,k) - fac_cond*(qcl(i,j,k)+qpl(i,j,k)) &
                                               - fac_sub *(qci(i,j,k)+qpi(i,j,k))
        w3(i,j,k) = zero
      enddo
    enddo
  enddo

   
! Define vertical grid increments for later use in the vertical differentiation

  do k=2,nzm
    km1 = k - 1
    do j=1,ny
      do i=1,nx
        adzi(i,j,k)   = zl(i,j,k) - zl(i,j,km1)
        adzl(i,j,km1) = zi(i,j,k) - zi(i,j,km1)
      enddo
    enddo
  enddo
  do j=1,ny
    do i=1,nx
      adzi(i,j,1)     = (zl(i,j,1)-zi(i,j,1)) !  unused in the code
      adzi(i,j,nz)    = adzi(i,j,nzm)         ! at the top - probably unused
      adzl(i,j,nzm)   = zi(i,j,nz) - zi(i,j,nzm)
!
      wthl_sec(i,j,1) = hflx(i)
      wqw_sec(i,j,1)  = evap(i)
    enddo
  enddo


  call tke_shoc()        ! Integrate prognostic TKE equation forward in time

  
! diagnose second order moments of the subgrid PDF following
! Redelsperger J.L., and G. Sommeria, 1986, JAS, 43, 2619-2635 sans the use of stabilty
! weighting functions - Result is in global variables w_sec, thl_sec, qw_sec, and qwthl_sec
    
! call diag_moments(total_water,tke,tkh)

! Second moment of vertical velocity.
! Note that Eq 6 in BK13 gives a different expression that is dependent on 
! vertical gradient of grid scale vertical velocity 

  do k=1,nzm
    ku = k+1
    kd = k-1
    ka = ku
    kb = k
    if (k == 1) then
      kd = k
      kb = ka
    elseif (k == nzm) then
      ku = k
      ka = kb
    endif
    do j=1,ny
      do i=1,nx
        if (tke(i,j,k) > zero) then
          wrk  = half*(tkh(i,j,ka)+tkh(i,j,kb))*(w(i,j,ku) - w(i,j,kd)) &
               * sqrt(tke(i,j,k)) / (zl(i,j,ku) - zl(i,j,kd))
!              / (sqrt(tke(i,j,k)) * (zl(i,j,ku) - zl(i,j,kd)))
          w_sec(i,j,k) = max(twoby3 * tke(i,j,k) - twoby15 * wrk, zero)
        else
          w_sec(i,j,k) = zero
        endif
      enddo
    enddo
  enddo
    
  do k=2,nzm
       
    km1 = k - 1
    do j=1,ny
      do i=1,nx

! Use backward difference in the vertical, use averaged values of "return-to-isotropy"
! time scale and diffusion coefficient

        wrk1 = one / adzi(i,j,k)        ! adzi(k) = (zl(k)-zl(km1))
!       wrk3 = max(tkh(i,j,k),pt01) * wrk1
        wrk3 = max(tkh(i,j,k),epsln) * wrk1

        sm   = half*(isotropy(i,j,k)+isotropy(i,j,km1))*wrk1*wrk3 ! Tau*Kh/dz^2
             
! SGS vertical flux liquid/ice water static energy. Eq 1 in BK13
             
        wrk1            = hl(i,j,k) - hl(i,j,km1)
        wthl_sec(i,j,k) = - wrk3 * wrk1

! SGS vertical flux of total water. Eq 2 in BK13

        wrk2           = total_water(i,j,k) - total_water(i,j,km1)
        wqw_sec(i,j,k) = - wrk3 * wrk2

! Second moment of liquid/ice water static energy. Eq 4 in BK13

        thl_sec(i,j,k) = thl2tune * sm * wrk1 * wrk1

! Second moment of total water mixing ratio.  Eq 3 in BK13
             
        qw_sec(i,j,k)  = qw2tune * sm * wrk2 * wrk2
             
! Covariance of total water mixing ratio and liquid/ice water static energy.
! Eq 5 in BK13
             
        qwthl_sec(i,j,k) = qwthl2tune * sm * wrk1 * wrk2

      enddo ! i  loop
    enddo   ! j  loop
  enddo     ! k  loop

!   These would be at the surface - do we need them?
  do j=1,ny
    do i=1,nx
!     wthl_sec(i,j,1)  = wthl_sec(i,j,2)
!     wqw_sec(i,j,1)   = wqw_sec(i,j,2)
      thl_sec(i,j,1)   = thl_sec(i,j,2)
      qw_sec(i,j,1)    = qw_sec(i,j,2)
      qwthl_sec(i,j,1) = qwthl_sec(i,j,2)
    enddo
  enddo

! Diagnose the third moment of SGS vertical velocity

  call canuto()

! Recover parameters of the subgrid PDF using diagnosed moments
! and calculate SGS cloudiness, condensation and it's effects on temeperature
! and moisture variables

  call assumed_pdf()

contains

  subroutine tke_shoc()

! This subroutine solves the TKE equation, 
! Heavily based on SAM's tke_full.f90 by Marat Khairoutdinov

    real grd,betdz,Cek,Cee,lstarn, lstarp, bbb, omn, omp,qsatt,dqsat, smix,         &
         buoy_sgs,ratio,a_prod_sh,a_prod_bu,a_diss,a_prod_bu_debug, buoy_sgs_debug, &
         tscale1, wrk, wrk1, wtke, wtk2, rdtn
    integer i,j,k,ku,kd,itr,k1

    rdtn = one / dtn

    call tke_shear_prod(def2)   ! Calculate shear production of TKE

! Ensure values of TKE are reasonable

    do k=1,nzm
      do j=1,ny
        do i=1,nx
          tke(i,j,k)        = max(min_tke,tke(i,j,k))
          tkesbdiss(i,j,k)  = zero
!         tkesbshear(i,j,k) = zero
!         tkesbbuoy(i,j,k)  = zero
        enddo
      enddo
    enddo

    call eddy_length()   ! Find turbulent mixing length
    call check_eddy()    ! Make sure it's reasonable

    do k=1,nzm      
      ku = k+1
      kd = k
      
      Cek = Ce * cefac

      if(k == 1) then
        ku = 2
        kd = 2
        Cek = Ces
      elseif(k == nzm) then
        ku = k
        kd = k
        Cek = Ces
      endif

      do j=1,ny
        do i=1,nx
          grd = adzl(i,j,k)             !  adzl(k) = zi(k+1)-zi(k)

!         wrk = zl(i,j,k) / grd + 1.5
!         cek = one + 2.0 / (wrk*wrk -3.3)

! TKE boyancy production term. wthv_sec (buoyancy flux) is calculated in
! assumed_pdf(). The value used here is from the previous time step

          a_prod_bu = ggr / thv(i,j,k) * wthv_sec(i,j,k)

! If wthv_sec from subgrid PDF is not available use Brunt-Vaisalla frequency from eddy_length()

!Obtain Brunt-Vaisalla frequency from diagnosed SGS buoyancy flux
!Presumably it is more precise than BV freq. calculated in  eddy_length()?

          buoy_sgs = - (a_prod_bu+a_prod_bu) / (tkh(i,j,ku)+tkh(i,j,kd) + 0.0001)   ! tkh is eddy thermal diffussivity


!Compute $c_k$ (variable Cee) for the TKE dissipation term following Deardorff (1980)

          if (buoy_sgs <= zero) then
            smix = grd
          else
            smix = min(grd,max(0.1*grd, 0.76*sqrt(tke(i,j,k)/(buoy_sgs+1.e-10))))
          endif

          ratio     = smix/grd
          Cee       = Cek* (pt19 + pt51*ratio) * max(one, sqrt(pcrit/prsl(i,j,k)))

! TKE shear production term
          a_prod_sh = half*(def2(i,j,ku)*tkh(i,j,ku)*prnum(i,j,ku)   &
                          + def2(i,j,kd)*tkh(i,j,kd)*prnum(i,j,kd))


! smixt (turb. mixing lenght) is calculated in eddy_length() 
! Explicitly integrate TKE equation forward in time
!         a_diss     = Cee/smixt(i,j,k)*tke(i,j,k)**1.5 ! TKE dissipation term
!         tke(i,j,k) = max(zero,tke(i,j,k)+dtn*(max(zero,a_prod_sh+a_prod_bu)-a_diss))

! Semi-implicitly integrate TKE equation forward in time

          wtke = tke(i,j,k)
          wtk2 = wtke
!         wrk  = (dtn*Cee)/smixt(i,j,k)
          wrk  = (dtn*Cee) / smixt(i,j,k)
          wrk1 = wtke + dtn*(a_prod_sh+a_prod_bu)

          do itr=1,nitr                        ! iterate for implicit solution
            wtke   = min(max(min_tke, wtke), max_tke)
            a_diss = wrk*sqrt(wtke)            ! Coefficient in the TKE dissipation term
            wtke   = wrk1 / (one+a_diss)
            wtke   = tkef1*wtke + tkef2*wtk2   ! tkef1+tkef2 = 1.0

!     if (lprnt .and. i == ipr .and. k<15) write(0,*)' wtke=',wtke,' wtk2=',wtk2,&
!        ' a_diss=',a_diss,' a_prod_sh=',a_prod_sh,' a_prod_bu=',a_prod_bu,&
!        ' wrk1=',wrk1,' kdt=',kdt,' itr=',itr,' k=',k

            wtk2   = wtke
     
          enddo

          tke(i,j,k) = min(max(min_tke, wtke), max_tke)

          tscale1 = (dtn+dtn) / a_diss        ! See Eq 8 in BK13

          tkesbdiss(i,j,k) = rdtn*a_diss*tke(i,j,k) ! TKE dissipation term, epsilon


! Calculate "return-to-isotropy" eddy dissipation time scale, see Eq. 8 in BK13

          if (buoy_sgs <= zero) then
            isotropy(i,j,k) = min(max_eddy_dissipation_time_scale,tscale1)
          else
            isotropy(i,j,k) = min(max_eddy_dissipation_time_scale,          &
                             tscale1/(one+lambda*buoy_sgs*tscale1*tscale1))
          endif

! TKE budget terms

!         tkesbdiss(i,j,k)       = a_diss
!         tkesbshear(i,j,k)      = a_prod_sh
!         tkesbbuoy(i,j,k)       = a_prod_bu
!         tkesbbuoy_debug(i,j,k) = a_prod_bu_debug
!         tkebuoy_sgs(i,j,k)     = buoy_sgs

        enddo ! i loop
      enddo   ! j loop
    enddo     ! k
!
    wrk = half * ck
    do k=2,nzm
      k1 = k - 1
      do j=1,ny
        do i=1,nx
          tkh(i,j,k) = min(tkhmax, wrk * (isotropy(i,j,k)  * tke(i,j,k)    &
                                        + isotropy(i,j,k1) * tke(i,j,k1))) ! Eddy thermal diffusivity
        enddo ! i
      enddo   ! j
    enddo     ! k


  end subroutine tke_shoc

 
  subroutine tke_shear_prod(def2)

! Calculate TKE shear production term 

    real, intent(out) :: def2(nx,ny,nzm)

    real    rdzw, wrku, wrkv, wrkw
    integer i,j,k,k1
    
! Calculate TKE shear production term  at layer interface

    do k=2,nzm
      k1 = k - 1
      do j=1,ny
        do i=1,nx
          rdzw        = one / adzi(i,j,k)
          wrku        = (u(i,j,k)-u(i,j,k1)) * rdzw
          wrkv        = (v(i,j,k)-v(i,j,k1)) * rdzw
!         wrkw        = (w(i,j,k)-w(i,j,k1)) * rdzw
          def2(i,j,k) = wrku*wrku + wrkv*wrkv !+ 2*wrkw(1) * wrkw(1)
        enddo
      enddo
    enddo     ! k  loop
    do j=1,ny
      do i=1,nx
!       def2(i,j,1) = def2(i,j,2)
        def2(i,j,1) = (u(i,j,1)*u(i,j,1) + v(i,j,1)*v(i,j,1)) &
                    / (zl(i,j,1)*zl(i,j,1))
      enddo
    enddo

  end subroutine tke_shear_prod

  subroutine eddy_length()

! This subroutine computes the turbulent length scale based on a new
! formulation described in BK13

! Local variables
    real    wrk, wrk1, wrk2, wrk3
    integer i, j, k, kk, kl, ku, kb, kc, kli, kui
    
    do j=1,ny
      do i=1,nx
        cldarr(i,j) = zero
        numer(i,j)  = zero
        denom(i,j)  = zero
      enddo
    enddo
    
! Find the length scale outside of clouds, that includes boundary layers.
    
    do k=1,nzm
      do j=1,ny
        do i=1,nx
             
! Reinitialize the mixing length related arrays to zero
!         smixt(i,j,k)    = one   ! shoc_mod module variable smixt
          smixt(i,j,k)    = epsln ! shoc_mod module variable smixt
          brunt(i,j,k)    = zero

!Eq. 11 in BK13 (Eq. 4.13 in Pete's dissertation)
!Outside of cloud, integrate from the surface to the cloud base
!Should the 'if' below check if the cloud liquid < a small constant instead?

          if (qcl(i,j,k)+qci(i,j,k) <= zero) then 
            tkes       = sqrt(tke(i,j,k)) * adzl(i,j,k)
            numer(i,j) = numer(i,j) + tkes*zl(i,j,k) ! Numerator   in Eq. 11 in BK13
            denom(i,j) = denom(i,j) + tkes           ! Denominator in Eq. 11 in BK13
          else
            cldarr(i,j) = one   ! Take note of columns containing cloud.
          endif
        enddo
      enddo
    enddo

! Calculate the measure of PBL depth,  Eq. 11 in BK13 (Is this really PBL depth?)
    do j=1,ny
      do i=1,nx
        if (denom(i,j) >  zero .and. numer(i,j) > zero) then
          l_inf(i,j) = min(0.1 * (numer(i,j)/denom(i,j)), 100.0)
        else
          l_inf(i,j) = 100.0
        endif
      enddo
    enddo
    
!Calculate length scale outside of cloud, Eq. 10 in BK13 (Eq. 4.12 in Pete's dissertation)
    do k=1,nzm

      kb = k-1
      kc = k+1
      if (k == 1) then
        kb = 1
        kc = 2
        thedz(:,:) = adzi(:,:,kc)
      elseif (k == nzm) then
        kb = nzm-1
        kc = nzm
        thedz(:,:) = adzi(:,:,k)
      else
        thedz(:,:) = adzi(:,:,kc) + adzi(:,:,k) !  = (z(k+1)-z(k-1))
      endif

      do j=1,ny
        do i=1,nx

!  vars module variable bet (=ggr/tv0) ; grid module variable  adzi
       
          betdz = bet(i,j,k) / thedz(i,j)
           
          tkes = sqrt(tke(i,j,k))
             
! Compute local Brunt-Vaisalla frequency
             
          wrk = qcl(i,j,k) + qci(i,j,k)
          if (wrk > zero) then            ! If in the cloud
             
! Find the in-cloud Brunt-Vaisalla frequency
                
             omn = qcl(i,j,k) / (wrk+1.e-20) ! Ratio of liquid water to total water

! Latent heat of phase transformation based on relative water phase content
! fac_cond = lcond/cp, fac_fus = lfus/cp

             lstarn = fac_cond + (one-omn)*fac_fus

! Derivative of saturation mixing ratio over water/ice wrt temp. based on relative water phase content
             dqsat =      omn  * dtqsatw(tabs(i,j,k),prsl(i,j,k))            &
                   + (one-omn) * dtqsati(tabs(i,j,k),prsl(i,j,k))

! Saturation mixing ratio over water/ice wrt temp  based on relative water phase content

             qsatt =      omn  * qsatw(tabs(i,j,k),prsl(i,j,k))               &
                   + (one-omn) * qsati(tabs(i,j,k),prsl(i,j,k))

! liquid/ice moist static energy static energy divided by cp?

             bbb = (one + epsv*qsatt-wrk-qpl(i,j,k)-qpi(i,j,k)                &
                 + 1.61*tabs(i,j,k)*dqsat) / (one+lstarn*dqsat)

! Calculate Brunt-Vaisalla frequency using centered differences in the vertical

             brunt(i,j,k) = betdz*(bbb*(hl(i,j,kc)-hl(i,j,kb))                &
                          + (bbb*lstarn - (one+lstarn*dqsat)*tabs(i,j,k))     &
                          * (total_water(i,j,kc)-total_water(i,j,kb))         & 
                          + (bbb*fac_cond - (one+fac_cond*dqsat)*tabs(i,j,k))*(qpl(i,j,kc)-qpl(i,j,kb))  &
                          + (bbb*fac_sub  - (one+fac_sub*dqsat)*tabs(i,j,k))*(qpi(i,j,kc)-qpi(i,j,kb)) )
                
          else                       ! outside of cloud
                
! Find outside-of-cloud Brunt-Vaisalla frequency
! Only unsaturated air, rain and snow contribute to virt. pot. temp. 
! liquid/ice moist static energy divided by cp?

             bbb = one + epsv*qv(i,j,k) - qpl(i,j,k) - qpi(i,j,k)
             brunt(i,j,k) = betdz*( bbb*(hl(i,j,kc)-hl(i,j,kb))                        &
                          + epsv*tabs(i,j,k)*(total_water(i,j,kc)-total_water(i,j,kb)) &
                          + (bbb*fac_cond-tabs(i,j,k))*(qpl(i,j,kc)-qpl(i,j,kb))       &
                          + (bbb*fac_sub -tabs(i,j,k))*(qpi(i,j,kc)-qpi(i,j,kb)) )
          endif
             
! Reduction of mixing length in the stable regions (where B.-V. freq. > 0) is required.
! Here we find regions of Brunt-Vaisalla freq. > 0 for later use. 

          if (brunt(i,j,k) >= zero) then
            brunt2(i,j,k) = brunt(i,j,k)
          else
            brunt2(i,j,k) = zero
          endif
             
! Calculate turbulent length scale in the boundary layer.
! See Eq. 10 in BK13 (Eq. 4.12 in Pete's dissertation)

! Keep the length scale adequately small near the surface following Blackadar (1984)
! Note that this is not documented in BK13 and was added later for SP-CAM runs

!         if (k == 1) then
!           term = 600.*tkes
!           smixt(i,j,k) = term + (0.4*zl(i,j,k)-term)*exp(-zl(i,j,k)*0.01)
!         else

! tscale is the eddy turnover time scale in the boundary layer and is 
! an empirically derived constant 

            if (tkes > zero .and. l_inf(i,j) > zero) then
              wrk1 = one / (tscale*tkes*vonk*zl(i,j,k))
              wrk2 = one / (tscale*tkes*l_inf(i,j))
              wrk1 = wrk1 + wrk2 + pt01 * brunt2(i,j,k) / tke(i,j,k)
              wrk1 = sqrt(one / max(wrk1,1.0e-8)) * (one/0.3)
!             smixt(i,j,k) = min(max_eddy_length_scale, 2.8284*sqrt(wrk1)/0.3)
              smixt(i,j,k) = min(max_eddy_length_scale, wrk1)

!           smixt(i,j,k) = min(max_eddy_length_scale,(2.8284*sqrt(1./((1./(tscale*tkes*vonk*zl(i,j,k))) & 
!                  + (1./(tscale*tkes*l_inf(i,j)))+0.01*(brunt2(i,j,k)/tke(i,j,k)))))/0.3)
!           else
!             smixt(i,j,k) = zero
            endif
                
!         endif
             
        enddo
          
      enddo
    enddo
    
    
! Now find the in-cloud turbulence length scale
! See Eq. 13 in BK13 (Eq. 4.18 in Pete's disseration)
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Remove after coupling to subgrid PDF.
!wthv_sec = -300/ggr*brunt*tk
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
! determine cubed convective velocity scale (conv_vel2) inside the cloud

!   call conv_scale()           ! inlining the relevant code

    do j=1,ny
      do i=1,nx
        conv_vel2(i,j,1) = zero ! Convective velocity scale cubed
      enddo
    enddo
                                ! Integrate velocity scale in the vertical
    do k=2,nzm
      do j=1,ny
        do i=1,nx
          conv_vel2(i,j,k) = conv_vel2(i,j,k-1)                               &
                           + 2.5*adzi(i,j,k)*bet(i,j,k)*wthv_sec(i,j,k)
        enddo
      enddo
    enddo
    
    do j=1,ny
      do i=1,nx
          
        if (cldarr(i,j) == 1) then ! If there's a cloud in this column 
             
          kl = 0
          ku = 0
          do k=2,nzm-3
                
! Look for the cloud base in this column  
! thresh (=0) is a  variable local to eddy_length(). Should be a module constant.
            wrk = qcl(i,j,k) + qci(i,j,k)
            if (wrk > thresh .and. kl == 0) then
              kl = k
            endif
                
! Look for the cloud top in this column
            if (wrk > thresh .and. qcl(i,j,k+1)+qci(i,j,k+1) <= thresh) then
              ku = k
! conv_vel2 (Cubed convective velocity scale) is calculated in conv_scale()
! Use the value of conv_vel2 at the top of the cloud. 
              conv_var = conv_vel2(i,j,k)**(oneb3)
            endif
                
! Compute the mixing length scale for the cloud layer that we just found
            if (kl > 0 .and. ku > 0 .and. ku-kl > 1) then
                
              if (conv_var > 0) then ! If convective vertical velocity scale > 0
                 
                depth = (zl(i,j,ku)-zl(i,j,kl)) + adzl(i,j,kl)
                      
                     
                do kk=kl,ku
! in-cloud turbulence length scale, Eq. 13 in BK13 (Eq. 4.18)

                  wrk = conv_var/(depth*sqrt(tke(i,j,kk)))
                  wrk = wrk * wrk + pt01*brunt2(i,j,kk)/tke(i,j,kk)

                  smixt(i,j,kk) = min(max_eddy_length_scale, (one/0.3)*sqrt(one/wrk))

                enddo
                      
              endif ! If convective vertical velocity scale > 0
              kl = zero
              ku = zero
            endif ! if inside the cloud layer
                
          enddo   ! k=2,nzm-3
        endif     ! if in the cloudy column
      enddo       ! i=1,nx
    enddo         ! j=1,ny
    
    
  end subroutine eddy_length


  subroutine conv_scale()

! This subroutine calculates the cubed convective velocity scale needed 
! for the definition of the length scale in clouds
! See Eq. 16 in BK13 (Eq. 4.21 in Pete's dissertation)

    integer i, j, k

!!!!!!!!!
!! A bug in formulation of conv_vel
!  Obtain it by averaging conv_vel2 in the horizontal
!!!!!!!!!!

!   conv_vel(1)=zero      ! Horizontally averaged convective velocity scale cubed 
    do j=1,ny
      do i=1,nx
        conv_vel2(i,j,1) = zero ! Convective velocity scale cubed
      enddo
    enddo
! Integrate velocity scale in the vertical
    do k=2,nzm
!     conv_vel(k)=conv_vel(k-1)
      do j=1,ny
        do i=1,nx
!**********************************************************************
!Do not include grid-scale contribution to convective velocity scale in GCM applications 
!         conv_vel(k)=conv_vel(k-1)+2.5*adzi(k)*bet(k)*(tvwle(k)+tvws(k))
!         conv_vel(k)=conv_vel(k)+2.5*adzi(i,j,k)*bet(i,j,k)*(tvws(k))
!Do not include grid-scale contribution to convective velocity scale in GCM applications 
!         conv_vel2(i,j,k)=conv_vel2(i,j,k-1)+2.5*adzi(k)*bet(k)*(tvwle(k)+wthv_sec(i,j,k))
!**********************************************************************

          conv_vel2(i,j,k) = conv_vel2(i,j,k-1)                               &
                           + 2.5*adzi(i,j,k)*bet(i,j,k)*wthv_sec(i,j,k)
        enddo
      enddo
    enddo

  end subroutine conv_scale


  subroutine check_eddy()

! This subroutine checks eddy length values 

    integer i, j, k, kb, ks, zend
    real    wrk
!   real zstart, zthresh, qthresh

! Temporary kludge for marine stratocumulus under very strong inversions at coarse resolution
! Placement until some explicity PBL top is put in
! Not used.
!   zthresh = 100.
!   qthresh = -6.0

    do k=1,nzm

      if (k == nzm) then
        kb = k
      else
        kb = k+1
      endif

      do j=1,ny
        do i=1,nx

          wrk = 0.1*adzl(i,j,k)
                                                            ! Minimum 0.1 of local dz
          smixt(i,j,k) = max(wrk, min(max_eddy_length_scale,smixt(i,j,k))) 

! If chracteristic grid dimension in the horizontal< 1000m, set lengthscale to 
! be not larger that that. 
!         if (sqrt(dx*dy) .le. 1000.) smixt(i,j,k)=min(sqrt(dx*dy),smixt(i,j,k)) 

          if (qcl(i,j,kb) == 0 .and. qcl(i,j,k) > 0 .and. brunt(i,j,k) > 1.e-4) then
!If just above the cloud top and atmosphere is stable, set to  0.1 of local dz
            smixt(i,j,k) = wrk
          endif

        enddo ! i
      enddo   ! j
    enddo     ! k

  end subroutine check_eddy

  subroutine canuto()

! Subroutine impements an analytic expression for the third moment of SGS vertical velocity
! based on Canuto et at, 2001, JAS, 58, 1169-1172 (further referred to as C01)
! This allows to avoid having a prognostic equation for the third moment.
! Result is returned in a global variable w3 defined at the interface levels.
    
! Local variables
    integer i, j, k, kb, kc

    real bet2,   f0,     f1,     f2,  f3,    f4,   f5,  iso, isosqr,         &
         omega0, omega1, omega2, X0,  Y0,    X1,   Y1,  AA0, AA1, buoy_sgs2, &
         cond,   wrk, wrk1,  wrk2, wrk3, avew
!
! See Eq. 7 in C01 (B.7 in Pete's dissertation)
    real, parameter :: c=7.0, a0=0.52/(c*c*(c-2.)), a1=0.87/(c*c),      &
                       a2=0.5/c, a3=0.6/(c*(c-2.)), a4=2.4/(3.*c+5.),   &
                       a5=0.6/(c*(3.*c+5))
!Moorthi               a5=0.6/(c*(3.+5.*c))
    
!   do k=1,nzm
    do k=2,nzm

      kb = k-1
      kc = k+1

      if(k == 1) then
        kb = 1
        kc = 2
        do j=1,ny
          do i=1,nx
            thedz(i,j)  = one / adzl(i,j,kc)
            thedz2(i,j) = thedz(i,j)
          enddo
        enddo
      elseif(k == nzm) then
        kb = nzm-1
        kc = nzm
        do j=1,ny
          do i=1,nx
            thedz(i,j)  = one / adzl(i,j,k)
            thedz2(i,j) = thedz(i,j)
          enddo
        enddo
      else
        do j=1,ny
          do i=1,nx
            thedz(i,j)  = one / adzl(i,j,k)
            thedz2(i,j) = one / (adzl(i,j,k)+adzl(i,j,kb))
          enddo
        enddo
      endif

       
      do j=1,ny
        do i=1,nx
             
          iso       = half*(isotropy(i,j,k)+isotropy(i,j,kb))
          isosqr    = iso*iso ! Two-level average of "return-to-isotropy" time scale squared
          buoy_sgs2 = isosqr*half*(brunt(i,j,k)+brunt(i,j,kb))
          bet2      = half*(bet(i,j,k)+bet(i,j,kb))  !Two-level average of BV frequency squared

        
! Compute functions f0-f5, see Eq, 8 in C01 (B.8 in Pete's dissertation)
        

          avew = half*(w_sec(i,j,k)+w_sec(i,j,kb))
          cond = 1.2*sqrt(max(1.0e-20,2.*avew*avew*avew))
          wrk1 = bet2*iso
          wrk2 = thedz2(i,j)*wrk1*wrk1*iso
          wrk3 = thl_sec(i,j,kc) - thl_sec(i,j,kb)

          f0   = wrk2 * wrk1 * wthl_sec(i,j,k) * wrk3

          wrk  = wthl_sec(i,j,kc) - wthl_sec(i,j,kb)
             
          f1   = wrk2 * (wrk*wthl_sec(i,j,k) + half*avew*wrk3)
             
          wrk1 = bet2*isosqr
          f2   = thedz(i,j)*wrk1*wthl_sec(i,j,k)*(w_sec(i,j,k)-w_sec(i,j,kb))     &
               + (thedz2(i,j)+thedz2(i,j))*bet(i,j,k)*isosqr*wrk
             
          f3   = thedz2(i,j)*wrk1*wrk + thedz(i,j)*bet2*isosqr*(wthl_sec(i,j,k)*(tke(i,j,k)-tke(i,j,kb)))

          wrk1 = thedz(i,j)*iso*avew
          f4   = wrk1*(w_sec(i,j,k)-w_sec(i,j,kb) + tke(i,j,k)-tke(i,j,kb))
 
          f5   = wrk1*(w_sec(i,j,k)-w_sec(i,j,kb))
             
       
! Compute the "omega" terms, see Eq. 6 in C01 (B.6 in Pete's dissertation)

          omega0 = a4 / (one-a5*buoy_sgs2)
          omega1 = omega0 / (c+c)
          omega2 = omega1*f3+(5./4.)*omega0*f4
 
! Compute the X0, Y0, X1, Y1 terms,  see Eq. 5 a-b in C01  (B.5 in Pete's dissertation)

          wrk1 = one / (one-(a1+a3)*buoy_sgs2)
          wrk2 = one / (one-a3*buoy_sgs2)
          X0   = wrk1 * (a2*buoy_sgs2*(one-a3*buoy_sgs2))
          Y0   = wrk2 * (two*a2*buoy_sgs2*X0)
          X1   = wrk1 * (a0*f0+a1*f1+a2*(one-a3*buoy_sgs2)*f2)
          Y1   = wrk2 * (two*a2*(buoy_sgs2*X1+(a0/a1)*f0+f1))

! Compute the A0, A1 terms,  see Eq. 5d in C01 (B.5 in Pete's dissertation)

          AA0 = omega0*X0 + omega1*Y0
          AA1 = omega0*X1 + omega1*Y1 + omega2

! Finally, we have the third moment of w, see Eq. 4c in C01 (B.4 in Pete's dissertation)
! cond is an estimate of third moment from second oment - If the third moment is larger
! than the estimate - limit w3.

           w3(i,j,k) = max(-cond, min(cond, (AA1-1.2*X1-1.5*f5)/(c-1.2*X0+AA0)))

! Implemetation of the C01 approach in this subroutine is nearly complete
! (the missing part are Eqs. 5c and 5e which are very simple)
! therefore it's easy to diagnose other third order moments obtained in C01 using this code. 

        enddo
      enddo
    enddo
    do j=1,ny
      do i=1,nx
        w3(i,j,1) = w3(i,j,2)
      enddo
    enddo
    
  end subroutine canuto

  subroutine assumed_pdf()

! Compute SGS buoyancy flux, SGS cloud fraction, and SGS condensation 
! using assumed analytic double-gaussian PDF for SGS vertical velocity, 
! moisture, and  liquid/ice water static energy, based on the 
! general approach of  Larson et al 2002, JAS, 59, 3519-3539, 
! and Golaz et al 2002, JAS, 59, 3540-3551
! References in the comments in this code are given to 
! the Appendix A of Pete Bogenschutz's dissertation. 
    
! Local variables

    integer i,j,k,ku,kd
    real wrk, wrk1, wrk2, wrk3, wrk4, bastoeps, eps_ss1, eps_ss2

!   bastoeps = basetemp / epsterm


! Initialize for statistics
    do k=1,nzm
      wqlsb(k) = zero
      wqisb(k) = zero
    enddo

!   sfac  = scrit
!   sfaci = one / sfac
    
    DO k=1,nzm
      
      kd = k
      ku = k + 1
!     if (k == nzm) ku = k
      
      DO j=1,ny
        DO i=1,nx

! Initialize cloud variables to zero  
          diag_qn   = zero
          diag_frac = zero
          diag_ql   = zero
          diag_qi   = zero

          pval  = prsl(i,j,k)
          pfac  = pval * 1.0e-5
          pkap  = pfac ** kapa

!         sfac  = scrit * sqrt(pfac)
!         sfac  = scrit
          sfac  = scrit * pfac * pfac
          sfaci = one / sfac
             
! Read in liquid/ice static energy, total water mixing ratio, 
! and vertical velocity to variables PDF needs
          thl_first = hl(i,j,k)
          qw_first  = total_water(i,j,k)
!         w_first   = half*(w(i,j,kd)+w(i,j,ku))
          w_first   = w(i,j,k)
            
             
! GET ALL INPUT VARIABLES ON THE SAME GRID
! Points to be computed with relation to thermo point
! Read in points that need to be averaged

          if (k < nzm) then
            w3var    = half*(w3(i,j,kd)+w3(i,j,ku))
            thlsec   = max(zero, half*(thl_sec(i,j,kd)+thl_sec(i,j,ku)) )
            qwsec    = max(zero, half*(qw_sec(i,j,kd)+qw_sec(i,j,ku)) )
            qwthlsec = half * (qwthl_sec(i,j,kd) + qwthl_sec(i,j,ku))
            wqwsec   = half * (wqw_sec(i,j,kd)   + wqw_sec(i,j,ku))
            wthlsec  = half * (wthl_sec(i,j,kd)  + wthl_sec(i,j,ku))   
          else                ! at the model top assuming zeros
            w3var    = half*w3(i,j,k)
            thlsec   = max(zero, half*thl_sec(i,j,k))
            qwsec    = max(zero, half*qw_sec(i,j,k))
            qwthlsec = half * qwthl_sec(i,j,k)
            wqwsec   = half * wqw_sec(i,j,k)
            wthlsec  = half * wthl_sec(i,j,k)
          endif

!         w3var    = w3(i,j,k)
!         thlsec   = max(zero,thl_sec(i,j,k))
!         qwsec    = max(zero,qw_sec(i,j,k))
!         qwthlsec = qwthl_sec(i,j,k)
!         wqwsec   = wqw_sec(i,j,k)
!         wthlsec  = wthl_sec(i,j,k)
  
! Compute square roots of some variables so we don't have to do it again
          if (w_sec(i,j,k) > zero) then
            sqrtw2  = sqrt(w_sec(i,j,k))
          else
            sqrtw2  = zero
          endif
          if (thlsec > zero) then
            sqrtthl = sqrt(thlsec)
          else
            sqrtthl = zero
          endif
          if (qwsec > zero) then
            sqrtqt  = sqrt(qwsec)
          else
            sqrtqt  = zero
          endif
             

! Find parameters of the double Gaussian PDF of vertical velocity

! Skewness of vertical velocity
!         Skew_w = w3var / w_sec(i,j,k)**(3./2.)
!         Skew_w = w3var / (sqrtw2*sqrtw2*sqrtw2)     ! Moorthi

          IF (w_sec(i,j,k) <= w_tol_sqd) THEN ! If variance of w is too small then
                                              ! PDF is a sum of two delta functions
            Skew_w = zero
            w1_1   = w_first
            w1_2   = w_first
            w2_1   = zero
            w2_2   = zero
            aterm  = half
            onema  = half
          ELSE
                
            Skew_w = w3var / (sqrtw2*sqrtw2*sqrtw2)     ! Moorthi
! Proportionality coefficients between widths of each vertical velocity 
! gaussian and the sqrt of the second moment of w
            w2_1 = 0.4
            w2_2 = 0.4
                
! Compute realtive weight of the first PDF "plume" 
! See Eq A4 in Pete's dissertaion -  Ensure 0.01 < a < 0.99

            wrk   = one - w2_1
            aterm = max(atmin,min(half*(one-Skew_w*sqrt(one/(4.*wrk*wrk*wrk+Skew_w*Skew_w))),atmax))
            onema = one - aterm
                
            sqrtw2t = sqrt(wrk)
                
! Eq. A.5-A.6
            wrk  =   sqrt(onema/aterm)
            w1_1 =   sqrtw2t * wrk
            w1_2 = - sqrtw2t / wrk

            w2_1 = w2_1 * w_sec(i,j,k)
            w2_2 = w2_2 * w_sec(i,j,k)

          ENDIF
             
!  Find parameters of the  PDF of liquid/ice static energy

          IF (thlsec <= thl_tol*thl_tol .or. abs(w1_2-w1_1) <= w_thresh) THEN
            thl1_1     = thl_first
            thl1_2     = thl_first
            thl2_1     = zero
            thl2_2     = zero
            sqrtthl2_1 = zero
            sqrtthl2_2 = zero
          ELSE

            corrtest1 = max(-one,min(one,wthlsec/(sqrtw2*sqrtthl)))

            thl1_1 = -corrtest1 / w1_2                 ! A.7
            thl1_2 = -corrtest1 / w1_1                 ! A.8
                
            wrk1   = thl1_1 * thl1_1
            wrk2   = thl1_2 * thl1_2
            wrk3   = three * (one - aterm*wrk1 - onema*wrk2)
            wrk4   = -skew_fact*Skew_w - aterm*wrk1*thl1_1 - onema*wrk2*thl1_2  ! testing - Moorthi
!           wrk4   =     - aterm*wrk1*thl1_1 - onema*wrk2*thl1_2
            wrk    = three * (thl1_2-thl1_1)
            if (wrk /= zero) then
              thl2_1 = thlsec * min(100.,max(zero,( thl1_2*wrk3-wrk4)/(aterm*wrk))) ! A.10
              thl2_2 = thlsec * min(100.,max(zero,(-thl1_1*wrk3+wrk4)/(onema*wrk))) ! A.11
            else
              thl2_1 = zero
              thl2_2 = zero
            endif
!
            thl1_1 = thl1_1*sqrtthl + thl_first
            thl1_2 = thl1_2*sqrtthl + thl_first

            sqrtthl2_1 = sqrt(thl2_1)
            sqrtthl2_2 = sqrt(thl2_2)

          ENDIF

!  FIND PARAMETERS FOR TOTAL WATER MIXING RATIO

          IF (qwsec <= rt_tol*rt_tol .or. abs(w1_2-w1_1) <= w_thresh) THEN
            qw1_1     = qw_first
            qw1_2     = qw_first
            qw2_1     = zero
            qw2_2     = zero
            sqrtqw2_1 = zero
            sqrtqw2_2 = zero
          ELSE

            corrtest2 = max(-one,min(one,wqwsec/(sqrtw2*sqrtqt)))

            qw1_1 = - corrtest2 / w1_2            ! A.7
            qw1_2 = - corrtest2 / w1_1            ! A.8

            tsign = abs(qw1_2-qw1_1)
                
!           Skew_qw = skew_facw*Skew_w

            IF (tsign > 0.4) THEN
              Skew_qw = skew_facw*Skew_w
            ELSEIF (tsign <= 0.2) THEN
              Skew_qw = zero
            ELSE
              Skew_qw = (skew_facw/0.2) * Skew_w * (tsign-0.2)
            ENDIF

            wrk1  = qw1_1 * qw1_1
            wrk2  = qw1_2 * qw1_2
            wrk3  = three * (one - aterm*wrk1 - onema*wrk2)
            wrk4  = Skew_qw - aterm*wrk1*qw1_1 - onema*wrk2*qw1_2
            wrk   = three * (qw1_2-qw1_1)

            if (wrk /= zero) then
              qw2_1 = qwsec * min(100.,max(zero,( qw1_2*wrk3-wrk4)/(aterm*wrk))) ! A.10
              qw2_2 = qwsec * min(100.,max(zero,(-qw1_1*wrk3+wrk4)/(onema*wrk))) ! A.11
            else
              qw2_1 = zero
              qw2_2 = zero
            endif
!
            qw1_1 = qw1_1*sqrtqt + qw_first
            qw1_2 = qw1_2*sqrtqt + qw_first

            sqrtqw2_1 = sqrt(qw2_1)
            sqrtqw2_2 = sqrt(qw2_2)

          ENDIF

!  CONVERT FROM TILDA VARIABLES TO "REAL" VARIABLES

          w1_1 = w1_1*sqrtw2 + w_first
          w1_2 = w1_2*sqrtw2 + w_first

!  FIND WITHIN-PLUME CORRELATIONS 

          testvar = aterm*sqrtqw2_1*sqrtthl2_1 + onema*sqrtqw2_2*sqrtthl2_2

          IF (testvar == 0) THEN
            r_qwthl_1 = zero
          ELSE
            r_qwthl_1 = max(-one,min(one,(qwthlsec-aterm*(qw1_1-qw_first)*(thl1_1-thl_first) &
                                                  -onema*(qw1_2-qw_first)*(thl1_2-thl_first))/testvar)) ! A.12
          ENDIF

!  BEGIN TO COMPUTE CLOUD PROPERTY STATISTICS

          wrk1  = gamaz(i,j,k) - fac_cond*qpl(i,j,k) - fac_sub*qpi(i,j,k)
          Tl1_1 = thl1_1 - wrk1
          Tl1_2 = thl1_2 - wrk1

! Now compute qs

          esval1_1 = zero
          esval2_1 = zero
          om1      = one
          eps_ss1  = eps
          eps_ss2  = eps
             
! Partition based on temperature for the first plume

          IF (Tl1_1 >= tbgmax) THEN
            esval1_1 = fpvsl(Tl1_1)
!           esval1_1 = esatw(Tl1_1)
            lstarn1  = lcond
          ELSE IF (Tl1_1 < tbgmin) THEN
            esval1_1 = fpvsi(Tl1_1)
!           esval1_1 = esati(Tl1_1)
            lstarn1  = lsub
            eps_ss1   = eps * supice
          ELSE
            esval1_1 = fpvsl(Tl1_1)
            esval2_1 = fpvsi(Tl1_1)
!           esval1_1 = esatw(Tl1_1)
!           esval2_1 = esati(Tl1_1)
            om1      = max(zero, min(one, a_bg*(Tl1_1-tbgmin)))
            lstarn1  = lcond + (one-om1)*lfus
            eps_ss2   = eps * supice
          ENDIF

          qs1   =      om1  * (eps_ss1*esval1_1/max(esval1_1,pval-0.378*esval1_1))      &
                + (one-om1) * (eps_ss2*esval2_1/max(esval2_1,pval-0.378*esval2_1))

!         beta1 = (rgas/rv)*(lstarn1/(rgas*Tl1_1))*(lstarn1/(cp*Tl1_1))
          beta1 = (lstarn1*lstarn1*onebrvcp) / (Tl1_1*Tl1_1)              ! A.18


! Are the two plumes equal?  If so then set qs and beta
! in each column to each other to save computation
          IF (Tl1_1 == Tl1_2) THEN
            qs2   = qs1     
            beta2 = beta1
          ELSE 

            esval1_2 = zero
            esval2_2 = zero
            om2      = one
            eps_ss1  = eps
            eps_ss2  = eps

            IF (Tl1_2 < tbgmin) THEN
              esval1_2 = fpvsi(Tl1_2)
!             esval1_2 = esati(Tl1_2)
              lstarn2  = lsub
              eps_ss1   = eps * supice
            ELSE IF (Tl1_2 >= tbgmax) THEN
              esval1_2 = fpvsl(Tl1_2)
!             esval1_2 = esatw(Tl1_2)
              lstarn2  = lcond
            ELSE
              esval1_2 = fpvsl(Tl1_2)
              esval2_2 = fpvsi(Tl1_2)
!             esval1_2 = esatw(Tl1_2)
!             esval2_2 = esati(Tl1_2)
              om2      = max(zero, min(one, a_bg*(Tl1_2-tbgmin)))
              lstarn2  = lcond + (one-om2)*lfus
              eps_ss2  = eps * supice
            ENDIF
               
            qs2   =      om2  * (eps_ss1*esval1_2/max(esval1_2,pval-0.378*esval1_2))    &
                  + (one-om2) * (eps_ss2*esval2_2/max(esval2_2,pval-0.378*esval2_2))
                
!           beta2 = (rgas/rv)*(lstarn2/(rgas*Tl1_2))*(lstarn2/(cp*Tl1_2))   ! A.18
            beta2 = (lstarn2*lstarn2*onebrvcp) / (Tl1_2*Tl1_2)              ! A.18
                
          ENDIF

          qs1 = qs1 * rhc(i,j,k)
          qs2 = qs2 * rhc(i,j,k)

!  Now compute cloud stuff -  compute s term

          cqt1   = one / (one+beta1*qs1)                                    ! A.19
          wrk    = qs1 * (one+beta1*qw1_1) * cqt1
          s1     = qw1_1 - wrk                                              ! A.17
          cthl1  = cqt1*wrk*cpolv*beta1*pkap                                ! A.20

          wrk1   = cthl1 * cthl1
          wrk2   = cqt1  * cqt1
!         std_s1 = sqrt(max(zero,wrk1*thl2_1+wrk2*qw2_1-2.*cthl1*sqrtthl2_1*cqt1*sqrtqw2_1*r_qwthl_1))
          std_s1 = sqrt(max(zero, wrk1*thl2_1+wrk2*qw2_1                        &
                                - two*cthl1*sqrtthl2_1*cqt1*sqrtqw2_1*r_qwthl_1))
             
          qn1 = zero
          C1  = zero
             
          IF (std_s1 > zero) THEN
            wrk = s1 / (std_s1*sqrt2)
            C1 = max(zero, min(one, half*(one+erf(wrk))))                   ! A.15
!     if (lprnt .and. i == ipr) write(0,*)' in shoc wrk=',wrk,' s1=','std=',std_s1,&
!         ' c1=',c1*100,' qs1=',qs1,' qw1_1=',qw1_1

            IF (C1 > zero) qn1 = s1*C1 + (std_s1*sqrtpii)*exp(-wrk*wrk)     ! A.16
            if (qn1 < sfac) then
              c1  = min(c1, qn1*sfaci)
            endif
          ELSEIF (s1 > zero) THEN
            C1  = min(one, max(zero,s1*sfaci))
            qn1 = s1
          ENDIF
             
! now compute non-precipitating cloud condensate 

! If two plumes exactly equal, then just set many of these 
! variables to themselves to save on computation.
          IF (qw1_1 == qw1_2 .and. thl2_1 == thl2_2 .and. qs1 == qs2) THEN
            s2     = s1
            cthl2  = cthl1
            cqt2   = cqt1
            std_s2 = std_s1
            C2     = C1
            qn2    = qn1
          ELSE

            cqt2   = one / (one+beta2*qs2)
            wrk    = qs2 * (one+beta2*qw1_2) * cqt2
            s2     = qw1_2 - wrk
            cthl2  = wrk*cqt2*cpolv*beta2*pkap
            wrk1   = cthl2 * cthl2
            wrk2   = cqt2  * cqt2
!           std_s2 = sqrt(max(zero,wrk1*thl2_2+wrk2*qw2_2-2.*cthl2*sqrtthl2_2*cqt2*sqrtqw2_2*r_qwthl_1))
            std_s2 = sqrt(max(zero, wrk1*thl2_2+wrk2*qw2_2                        &
                                  - two*cthl2*sqrtthl2_2*cqt2*sqrtqw2_2*r_qwthl_1))

            qn2 = zero
            C2  = zero

            IF (std_s2 > zero) THEN
              wrk = s2 / (std_s2*sqrt2)
              C2  = max(zero, min(one, half*(one+erf(wrk))))
              IF (C2 > zero) qn2 = s2*C2 + (std_s2*sqrtpii)*exp(-wrk*wrk)
              if (qn2 < sfac) then
                c2  = min(c2, qn2*sfaci)
              endif
            ELSEIF (s2 > zero) THEN
              C2  = min(one, max(zero,s2*sfaci))
              qn2 = s2
            ENDIF
               
          ENDIF

! finally, compute the SGS cloud fraction
          diag_frac = aterm*C1 + onema*C2
             
          om1 = max(zero, min(one, (Tl1_1-tbgmin)*a_bg))      
          om2 = max(zero, min(one, (Tl1_2-tbgmin)*a_bg))
             
          qn1 = min(qn1,qw1_1)
          qn2 = min(qn2,qw1_2)
             
          ql1 = qn1*om1
          ql2 = qn2*om2
             
          qi1 = qn1 - ql1
          qi2 = qn2 - ql2
             
!     if (lprnt .and. i == ipr) write(0,*)' in shoc qi=',qi1,qi2,' ql=',ql1,ql2,&
!        ' c1=',c1,' c2=',c2,' s1=',s1,' s2=',s2,' k=',k

          diag_qn = min(max(zero, aterm*qn1 + onema*qn2), total_water(i,j,k))
          diag_ql = min(max(zero, aterm*ql1 + onema*ql2), diag_qn)
          diag_qi = diag_qn - diag_ql

             
! Update temperature variable based on diagnosed cloud properties
          om1         = max(zero, min(one, (tabs(i,j,k)-tbgmin)*a_bg))
          lstarn1     = lcond + (one-om1)*lfus
          tabs(i,j,k) = hl(i,j,k) - gamaz(i,j,k) + fac_cond*(diag_ql+qpl(i,j,k)) &
                                                 + fac_sub *(diag_qi+qpi(i,j,k)) &
                      + tkesbdiss(i,j,k) * (dtn/cp)      ! tke dissipative heating

! Update moisture fields

! Update ncpl and ncpi Anning Cheng 03/11/2016
!         ncpl(i,j,k)    = diag_ql/max(qc(i,j,k),1.e-10)*ncpl(i,j,k)
! The following commneted by Moorthi on April 26, 2017 to test blowing up
!         ncpl(i,j,k)    = (1.0-diag_ql/max(qc(i,j,k),1.e-10)) * ncpl(i,j,k)
!         ncpi(i,j,k)    = (1.0-diag_qi/max(qi(i,j,k),1.e-10)) * ncpi(i,j,k)
          qc(i,j,k)      = diag_ql
          qi(i,j,k)      = diag_qi
          qwv(i,j,k)     = total_water(i,j,k) - diag_qn
          cld_sgs(i,j,k) = diag_frac

             
! Compute the liquid water flux
          wqls = aterm * ((w1_1-w_first)*ql1) + onema * ((w1_2-w_first)*ql2)
          wqis = aterm * ((w1_1-w_first)*qi1) + onema * ((w1_2-w_first)*qi2)
             
! Compute statistics for the fluxes so we don't have to save these variables
          wqlsb(k) = wqlsb(k) + wqls
          wqisb(k) = wqisb(k) + wqis
             
! diagnostic buoyancy flux.  Includes effects from liquid water, ice
! condensate, liquid & ice precipitation
!         wrk = epsv * basetemp
          wrk = epsv * thv(i,j,k)

          bastoeps = onebeps * thv(i,j,k)

          if (k < nzm) then
            wthv_sec(i,j,k) = wthlsec + wrk*wqwsec                                    &
                            + (fac_cond-bastoeps)*wqls                                &
                            + (fac_sub-bastoeps) *wqis                                &
                            + ((lstarn1/cp)-thv(i,j,k))*half*(wqp_sec(i,j,kd)+wqp_sec(i,j,ku))
          else
            wthv_sec(i,j,k) = wthlsec + wrk*wqwsec                                    &
                            + (fac_cond-bastoeps)*wqls                                &
                            + (fac_sub-bastoeps) *wqis                                &
                            + ((lstarn1/cp)-thv(i,j,k))*half*wqp_sec(i,j,k)
          endif

!           wthv_sec(i,j,k) = wthlsec + wrk*wqwsec                                     &
!                         + (fac_cond-bastoeps)*wqls                                 &
!                         + (fac_sub-bastoeps)*wqis                                  &
!                         + ((lstarn1/cp)-basetemp)*half*(wqp_sec(i,j,kd)+wqp_sec(i,j,ku))

        ENDDO
      ENDDO
    ENDDO
    

  end subroutine assumed_pdf


! Saturation vapor pressure and mixing ratio subroutines
! Based on Flatau et al (1992), J. App. Met., 31, 1507-1513
! Code by Marat Khairoutdinov
 

  real function esatw(t)
    real t	! temperature (K)
    real a0,a1,a2,a3,a4,a5,a6,a7,a8 
    data a0,a1,a2,a3,a4,a5,a6,a7,a8 /                       &
         6.11239921,       0.443987641,     0.142986287e-1, &
         0.264847430e-3,   0.302950461e-5,  0.206739458e-7, &
         0.640689451e-10, -0.952447341e-13,-0.976195544e-15/
    real dt
    dt    = max(-80.,t-273.16)
    esatw = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt))))))) 
  end function esatw

  real function qsatw(t,p)
!    implicit none
    real t	! temperature (K)
    real p	! pressure    (Pa)
    real esat
!   esat  = fpvs(t)
    esat  = fpvsl(t)
    qsatw = 0.622 * esat/max(esat,p-0.378*esat) 
!   esat  = esatw(t)
!   qsatw = 0.622 * esat/max(esat,p-esat) 
  end function qsatw
  
  
  real function esati(t)
    real t	! temperature (K)
    real a0,a1,a2,a3,a4,a5,a6,a7,a8 
    data a0,a1,a2,a3,a4,a5,a6,a7,a8 /                     &
         6.11147274,     0.503160820,     0.188439774e-1, &
         0.420895665e-3, 0.615021634e-5,  0.602588177e-7, &
         0.385852041e-9, 0.146898966e-11, 0.252751365e-14/
    real dt
!    real esatw
    if(t > 273.15) then
       esati = esatw(t)
    else if(t.gt.185.) then
       dt    = t-273.16
       esati = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt))))))) 
    else   ! use some additional interpolation below 184K
       dt    = max(-100.,t-273.16)
       esati = 0.00763685 + dt*(0.000151069+dt*7.48215e-07)
    endif
  end function esati
        
  real function qsati(t,p)
    real t	! temperature (K)
    real p	! pressure    (Pa)
    real esat !,esati
!   esat  = fpvs(t)
    esat  = fpvsi(t)
    qsati = 0.622 * esat/max(esat,p-0.378*esat)
!   esat  = esati(t)
!   qsati = 0.622 * esat/max(esat,p-esat)
  end function qsati
  
  real function dtesatw(t)
    real t	! temperature (K)
    real a0,a1,a2,a3,a4,a5,a6,a7,a8 
    data a0,a1,a2,a3,a4,a5,a6,a7,a8 /                        &
         0.443956472,      0.285976452e-1,   0.794747212e-3, &
         0.121167162e-4,   0.103167413e-6,   0.385208005e-9, &
        -0.604119582e-12, -0.792933209e-14, -0.599634321e-17/
    real dt
    dt      = max(-80.,t-273.16)
    dtesatw = a0 + dt* (a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt))))))) 
  end function dtesatw
        
  real function dtqsatw(t,p)
    real t	! temperature (K)
    real p	! pressure    (Pa)
!    real dtesatw
    dtqsatw = 100.0*0.622*dtesatw(t)/p
  end function dtqsatw
  
  real function dtesati(t)
    real t	! temperature (K)
    real a0,a1,a2,a3,a4,a5,a6,a7,a8 
    data a0,a1,a2,a3,a4,a5,a6,a7,a8 /                      &
         0.503223089,     0.377174432e-1,  0.126710138e-2, &
         0.249065913e-4,  0.312668753e-6,  0.255653718e-8, &
         0.132073448e-10, 0.390204672e-13, 0.497275778e-16/
    real dt
!    real dtesatw
    if(t > 273.15) then
       dtesati = dtesatw(t)
    else if(t > 185.) then
       dt      = t-273.16
       dtesati = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt))))))) 
    else  ! use additional interpolation below 185K
       dt      = max(-100.,t-273.16)
       dtesati = 0.0013186 + dt*(2.60269e-05+dt*1.28676e-07)
    endif
  end function dtesati
  
  
  real function dtqsati(t,p)
    real t	! temperature (K)
    real p	! pressure    (Pa)
!    real dtesati
    dtqsati = 100.0*0.622*dtesati(t)/p
  end function dtqsati
  
end subroutine shoc
