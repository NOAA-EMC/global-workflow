!$$$   module documentation block
!                .      .    .                                       .
! module:  sfc_model
! prgmmr:  lee/parrish           org: np23            date: 2005-12-22
!
! abstract: This module containes routines (including forward and tangent 
!           linear) to apply a boundary layer model for use when 
!           assimilating near surface conventional observations
!
! program history log:
!   2004        s.j.lee 
!   2005-12-22  parrish - reformat and introduce into then current GSI release
!   2006-04-07  park - change the sensible temperature of the lowest 1st and 2nd sigma level 
!                      and ground virtual temperature on SFC_WTQ_FWD
!   2006-04-07  park - change the sensible temperature perturbation of the lowest sigma level 
!                      and use the T2 on sfc_wtq_Lin
!   2006-05-05  park - use the constants module
!   2006-07-14  parrish - modify so input/output pressure is in units of cb, and remove ln(psfc) references
!   2006-07-28  derber - use r1000 from constants module
!   2006-09-20  derber - add t sensible or virtual dependency on iqtflg
!   2006-09-28  treadon - add f10 (10m wind factor) to subroutine argument list
!   2007-04-03  treadon - replace 1_1 on "pi =( -five * rib ) / (1_1 - five * rib )" line with r1_1
!
! subroutines included:
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block


SUBROUTINE SFC_WTQ_FWD (psfc_in,tg,ps_in,tvs,qs,us,vs, &
                        ps2_in,tvs2,qs2, hs, roughness, iland, &
                        f10, u10, v10, t2, q2, regime, iqtflg)
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   SFC_WTQ_FWD
!
!   prgrmmr:
!
! abstract:  Calculate the 10m wind, 2m (virtual) temperature and moisture based on the
!              similarity theory/
!
!            The unit for input pressure variables psfc_in, ps_in, ps2_in is cb.
!            The unit for pressure   : psfc, ps, ps2     is Pa.
!            The unit for temperature: tg, tvs, tvs2, tv2,t2   is K.
!            The unit for moisture   : qs, qs2, q2       is kg/kg.
!            The unit for wind       : us, vs, u10, v10  is m/s.
!            The unit for height     : hs, roughness     is m.
!            xland and regime are dimensionless.
!
! Reference:
! ---------
!  Detail MM5/physics/pbl_sfc/mrfpbl/MRFPBL.F
!
! program history log:
!   NOTE:   changed code so input layer temps are virtual, and are internally
!           converted to sensible as required.  this corrects error where
!           ths is potential temperature for lowest half sigma layer, but
!           before this change is in fact potential virtual temperature.
!           also, output t2 should be potential temperature, so changed t2 to tv2
!   2008-04-14  safford - completed standard documentation block, rm unused vars
!
!  Input argument list:
!    psfc, tg                : surface pressure and ground temperature
!    ps, tvs, qs, us, vs, hs : model variable at lowlest half sigma leve1
!    ps2, tvs2, qs2          : model variables at the second lowest half 
!                              sigma level
!    iqtflg                  : flag  true if output is virtual temperature
!                              otherwise sensible temperature
!
!  Output argument list:
!    regime                 : PBL regime
!    u10, v10               : 10-m high observed wind components
!    t2 , q2                : 2-m high observed virtual (or sensible) temperature and 
!                             mixing ratio
!  Constant argument list:
!
!    hs                     : height at the lowest half sigma level  (above surface)
!    roughness              : roughness
!    xland                  : land-water-mask (<=0.5 water, >0.5 land)
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  
      use kinds, only: r_kind,i_kind   

      use constants, only: grav,fv,rd_over_cp,zero,quarter,one,two,four,five,r1000,r10,r100,r0_01

      IMPLICIT NONE

      real(r_kind)   ,intent(in   ) :: ps_in,tvs,qs,us,vs
      real(r_kind)   ,intent(in   ) :: ps2_in,tvs2,qs2,psfc_in,tg
      real(r_kind)   ,intent(in   ) :: hs,roughness
      integer(i_kind),intent(in   ) :: iland
      logical        ,intent(in   ) :: iqtflg
      integer(i_kind),intent(  out) :: regime
      real(r_kind)   ,intent(  out) :: f10,u10,v10,q2,t2

! Maximum number of iterations in computing psim, psih

!     INTEGER(i_kind), PARAMETER :: k_iteration = 10
!     INTEGER(i_kind), PARAMETER :: k_iteration = 1

! h10 is the height of 10m where the wind observed
! h2  is the height of 2m where the temperature and 
!        moisture observed.

      REAL(r_kind), PARAMETER :: h10 = 10.0_r_kind
      real(r_kind), parameter :: h2  = two
!
! Default roughness over the land

      REAL(r_kind), PARAMETER :: zint0 = 0.01_r_kind
!
! Von Karman constant

      REAL(r_kind), PARAMETER :: k_kar = 0.4_r_kind
!
! Working variables

      real(r_kind) :: psfc,ps,ps2
      REAL(r_kind) :: Vc2, Va2, V2 
      REAL(r_kind) :: rib, xx, yy, cc
      REAL(r_kind) :: psiw, psiz, mol, ust, hol, holz, hol2
      REAL(r_kind) :: psim, psimz, psim2, psih, psihz, psih2
      REAL(r_kind) :: psit, psit2, psiq, psiq2
      REAL(r_kind) :: gzsoz0, gz10oz0, gz2oz0
      REAL(r_kind) :: eg, qg, tvg, ts, ts2   !  change tvs, tvs2 to ts, ts2, add t2
      REAL(r_kind) :: ths, thg, thvs, thvg, thvs2
      REAL(r_kind) :: zq0, z0

      REAL(r_kind), PARAMETER :: ka = 2.4E-5_r_kind

! local 

      real(r_kind),parameter :: r16   = 16.0_r_kind
      real(r_kind),parameter :: r1_1  = 1.1_r_kind
      real(r_kind),parameter :: r0_9  = 0.9_r_kind
      real(r_kind),parameter :: r0_2  = 0.2_r_kind

!-----------------------------------------------------------------------------!

!  convert input pressure variables to mb from cb.

      psfc = r10*psfc_in
      ps   = r10*ps_in
      ps2  = r10*ps2_in
      
! 1 Compute the roughness length based upon season and land use 
! =====================================

! 1.1 Define the rouhness length
!     -----------------

      z0 = roughness/r100      ! unit change : originally cm --> m 

      if( z0 < 0.0001_r_kind) z0 = 0.0001_r_kind

! 1.2 Define the rouhgness length for moisture
!     -----------------

      if ( iland == 0 ) then
         zq0 = z0
      else
         zq0 =  zint0
      endif

! 1.3 Define the some constant variable for psi
!     -----------------

      gzsoz0 = log(hs/z0)

      gz10oz0 = log(h10/z0)

      gz2oz0 = log(h2/z0)


! 2. Calculate the virtual temperature
! =====================================

! 2.1 Compute Virtual temperature on the lowest half sigma level
!     ---------------------------------------------------------

      ts   = tvs / (one + fv * qs) 

! 2.2 Compute Virtual temperature on the second lowest half sigma level
!     -----------------------------------------------------------------

      ts2  = tvs2 / (one + fv * qs2)   

! 2.3 Convert ground virtual temperature assuming it's saturated
!     ----------------------------------------------------------

      call DA_TP_To_Qs(  tg, psfc, eg, qg )
      tvg  = tg * (one + fv * qg)

! 3.  Compute the potential temperature
! ======================================

! 3.1 Potential temperature on the lowest half sigma level
!     ----------------------------------------------------

      ths  = ts * (r1000 / ps) ** rd_over_cp

! 3.2 Potential temperature at the ground
!     -----------------------------------

      thg  = tg * (r1000 / psfc) ** rd_over_cp


! 4. Virtual potential temperature
! ==================================

! 4.1 Virtual potential temperature on the lowest half sigma level
!     ------------------------------------------------------------

      thvs = tvs * (r1000 / ps) ** rd_over_cp

! 4.2 Virtual potential temperature on the second lowest half sigma level
!     ------------------------------------------------------------------

      thvs2 = tvs2 * (r1000 / ps2) ** rd_over_cp

! 4.3 Virtual potential temperature at ground
!     ---------------------------------------

      thvg = tvg * (r1000 / psfc) ** rd_over_cp


! 5.  BULK RICHARDSON NUMBER AND MONI-OBUKOV LENGTH
! =================================================

! 5.1 Velocity
!     --------
!
!     Wind speed:

      Va2 =   us*us + vs*vs
!  
!     Convective velocity:

      if ( thvg >= thvs ) then
         Vc2 = four * (thvg - thvs)
      else
         Vc2 = zero
      endif
!
      V2  = .000001_r_kind + Va2 + Vc2         ! can't let V2 = 0, so add small value on, corresponding to 1mm/sec wind

! 5.2 Bulk richardson number
!     ----------------------

      rib = (grav * hs / ths) * (thvs - thvg) / V2

! 6.  CALCULATE PSI BASED UPON REGIME
! =======================================

! 6.1 Stable conditions (REGIME 1)
!     ---------------------------

      IF       (rib >= r0_2) THEN
         regime = 1
         psim = -r10*gzsoz0
         psimz = -r10*gz10oz0
         psim2 = -r10*gz2oz0
         psim = max(psim,-r10)
         psimz = max(psimz,-r10)
         psim2 = max(psim2,-r10)
         psih = psim
         psihz = psimz
         psih2 = psim2

! 6.2 Mechanically driven turbulence (REGIME 2)
!     ------------------------------------------

      ELSE IF ((rib < r0_2) .AND. (rib > zero)) THEN

         regime = 2
         psim = ( -five * rib ) * gzsoz0 / (r1_1 - five*rib)
         psimz = ( -five * rib ) * gz10oz0 / (r1_1 - five*rib)
         psim2 = ( -five * rib ) * gz2oz0 / (r1_1 - five*rib)

         psim = max(psim,-r10)
         psimz = max(psimz,-r10)
         psim2 = max(psim2,-r10)
         psih = psim
         psihz = psimz
         psih2 = psim2

! 6.3 Unstable Forced convection (REGIME 3)
!     -------------------------------------

      ELSE IF ((rib == zero) .or. (rib<zero .and. thvs2>thvs)) THEN
         regime = 3
         psim = zero
         psimz = zero
         psim2 = zero
         psih = psim
         psihz = psimz
         psih2 = psim2


! 6.4 Free convection (REGIME 4)
!     --------------------------

      ELSE 
         regime = 4
       
!      Calculate psi m and pshi h using iteration method
       
         psim = zero
         psih = zero
         cc = two * atan(one)

!        do k = 1 , k_iteration

! 6.4.1  Calculate   ust, m/L (mol), h/L (hol)
!        --------------------------

!        Friction speed

         ust = k_kar * sqrt(v2) /( gzsoz0 - psim)

!        Heat flux factor

         mol = k_kar * (ths - thg )/( gzsoz0 - psih)

!        Ratio of PBL height to Monin-Obukhov length

         if ( ust < r0_01 ) then
            hol = rib * gzsoz0
         else
            hol = k_kar * grav * hs * mol / ( ths * ust * ust )
         endif

! 6.4.2  Calculate n, nz, R, Rz
!        --------------------------
       
         hol = min(hol,zero)
         hol = max(hol,-r10)
         
         holz = (h10 / hs) * hol
         holz = min(holz,zero)
         holz = max(holz,-r10)

         hol2 = (h2 / hs) * hol
         hol2 = min(hol2,zero)
         hol2 = max(hol2,-r10)

! 6.4.3 Calculate Psim & psih
!        --------------------------

!        Using the look-up table:
!         nn = int( -r100 * hol )
!         rr = ( -r100 * hol ) - nn
!         psim = psimtb(nn) + rr * ( psimtb(nn+1) - psimtb(nn))
!         psih = psihtb(nn) + rr * ( psihtb(nn+1) - psihtb(nn))
!        Using the continuous function:
         xx = (one - r16 * hol) ** quarter
         yy = log((one+xx*xx)/two)
         psim = two * log((one+xx)/two) + yy - two * atan(xx) + cc
         psih = two * yy

!        Using the look-up table:
!         nz = int( -r100 * holz )
!         rz = ( -r100 * holz ) - nz
!         psimz = psimtb(nz) + rz * ( psimtb(nz+1) - psimtb(nz))
!         psihz = psihtb(nz) + rz * ( psihtb(nz+1) - psihtb(nz))
!        Using the continuous function:
         xx = (one - r16 * holz) ** quarter
         yy = log((one+xx*xx)/two)
         psimz = two * log((one+xx)/two) + yy - two * atan(xx) + cc
         psihz = two * yy

!        Using the look-up table:
!         n2 = int( -r100 * hol2 )
!         r2 = ( -r100 * hol2 ) - n2
!         psim2 = psimtb(n2) + r2 * ( psimtb(n2+1) - psimtb(n2))
!         psih2 = psihtb(n2) + r2 * ( psihtb(n2+1) - psihtb(n2))
!        Using the continuous function:
         xx = (one - r16 * hol2) ** quarter
         yy = log((one+xx*xx)/two)
         psim2 = two * log((one+xx)/two) + yy - two * atan(xx) + cc
         psih2 = two * yy

!        enddo 

! 6.4.4 Define the limit value for psim & psih
!        --------------------------

         psim = min(psim,r0_9*gzsoz0)
         psimz = min(psimz,r0_9*gz10oz0)
         psim2 = min(psim2,r0_9*gz2oz0)
         psih = min(psih,r0_9*gzsoz0)
         psihz = min(psihz,r0_9*gz10oz0)
         psih2 = min(psih2,r0_9*gz2oz0)
  
      ENDIF  ! Regime

! 7.  CALCULATE PSI FOR WIND, TEMPERATURE AND MOISTURE
! =======================================

      psiw = gzsoz0 - psim
      psiz = gz10oz0 - psimz
      psit = gzsoz0 - psih
      psit2 = gz2oz0 - psih2


!     Friction speed
      ust = k_kar * sqrt(v2) /( gzsoz0 - psim)

      psiq  = log(k_kar*ust*hs/ka + hs / zq0 ) - psih
      psiq2 = log(k_kar*ust*h2/ka + h2 / zq0 ) - psih2

! 8.  CALCULATE 10M WIND, 2M TEMPERATURE AND MOISTURE
! =======================================

      u10 = us * psiz / psiw
      v10 = vs * psiz / psiw
      f10 = psiz / psiw
      t2 = ( thg + ( ths - thg )*psit2/psit)*(psfc/r1000)**rd_over_cp
      q2 = qg + (qs - qg)*psiq2/psiq 
      if(iqtflg)then
         t2  = t2 * (one + fv * q2)         
      end if 

return
END SUBROUTINE SFC_WTQ_FWD


SUBROUTINE DA_TP_To_Qs( t, p, es, qs )

!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   DA_TP_To_Qs
!
!   prgrmmr:
!
! abstract:  Convert T/p to saturation specific humidity.
!
!  METHOD: qs = es_alpha * es / ( p - ( 1 - rd_over_rv ) * es ).
!          Use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
! program history log: 
!   2000-10-03  Barker  - Creation of F90 version.
!   2008-04-14  safford - added standard documentation block
!
!   input argument list:
!     t          - Temperature.
!     p          - Pressure.
!
!   output argument list:
!     es         - Sat. vapour pressure.
!     qs         - Sat. specific humidity.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_kind
   use constants, only: eps,omeps,t0c,r0_01

   IMPLICIT NONE

   REAL(r_kind), INTENT(IN   ) :: t                ! Temperature.
   REAL(r_kind), INTENT(IN   ) :: p                ! Pressure.
   REAL(r_kind), INTENT(  OUT) :: es               ! Sat. vapour pressure.
   REAL(r_kind), INTENT(  OUT) :: qs               ! Sat. specific humidity.

!  Saturation Vapour Pressure Constants(Rogers & Yau, 1989)
   REAL(r_kind), PARAMETER    :: es_alpha = 611.2_r_kind
   REAL(r_kind), PARAMETER    :: es_beta = 17.67_r_kind
   REAL(r_kind), PARAMETER    :: es_gamma = 243.5_r_kind
    
   REAL(r_kind)                          :: t_c              ! T in degreesC.

 
!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   t_c = t - t0c
    
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure:
!------------------------------------------------------------------------------
    
   es = r0_01 * es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )       !  change to mb
    
!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity:
!------------------------------------------------------------------------------
    
   qs = eps * es / ( p - omeps * es )

    
return
END SUBROUTINE DA_TP_To_Qs


SUBROUTINE sfc_wtq_Lin(psfc_in, tg, ps_in, tvs, qs, us, vs, regime,           &
                       psfc_prime_in,tg_prime,sig1,ts_in,qs_prime, &
                       us_prime,vs_prime, hs,roughness,iland,          &
                       u10_prime,v10_prime,t2_prime,q2_prime,iqtflg) 

!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   sfc_wtq_Lin
!
!   prgrmmr:
!
! abstract:  Calculate the 10m wind, 2m (virtual) temperature and moisture based on the
!               similarity theory/
!
! Reference:
! ---------
!  Detail MM5/physics/pbl_sfc/mrfpbl/MRFPBL.F
!
!
!  program history log:
!   NOTE:   changed code so input layer temps are virtual, and are internally
!               converted to sensible as required.  this corrects error where
!                ths is potential temperature for lowest half sigma layer, but
!                before this change is in fact potential virtual temperature.
!              also, output t2 should be potential temperature, so changed t2 to tv2
!   2008-04-14  safford - added standard documentation block, rm unused vars
!
!  Input argument list(basic state):
! 
!   psfc_in, tg                : surface pressure and ground temperature
!   ps_in, tvs, qs, us, vs, hs : model variable at lowlest half sigma leve1
!   regime                     : PBL regime
!   iqtflg                     : if true return virtual temperature
!                              : otherwise return sensible temperature
!
!  Input argument list(pertubation):
! 
!   psfc_prime_in, tg_prime    : Surface pressure and ground temperature
!   ps_prime, tvs_prime,       : Model variables at the lowest half sigma
!   qs_prime, us_prime,        : level 
!   vs_prime                   : 
!
!  Constants:
!   hs                         : height at the lowest half sigma level
!   roughness                  : roughness
!   xland                      : land-water-mask (=2 water, =1 land)
!
!  Output argument list(pertubation):
!
!   u10_prime, v10_prime       : 10-m high observed wind components
!   tv2_prime , q2_prime       : 2-m high observed temperature and mixing ratio
!
! attributes:
!   language:  f90
!   machine    ibm RS/6000 SP
!
!$$$ end documentation block


      use kinds, only: r_kind,i_kind  
      use constants, only: grav,fv,rd_over_cp,zero,quarter,half,one,two,four,five,r1000,r10,r100,r0_01

      IMPLICIT NONE

      INTEGER(i_kind), INTENT (in   ) :: regime,iland
      REAL(r_kind)   , INTENT (in   ) :: ps_in , tvs , qs , us, vs, psfc_in, tg    !   change ts to tvs
      REAL(r_kind)   , INTENT (in   ) :: sig1, ts_in, qs_prime  , &  !   change ts_prime to tvs_prime
                                       us_prime, vs_prime, psfc_prime_in, tg_prime
      REAL(r_kind)   , INTENT (in   ) :: hs, roughness

      REAL(r_kind)   , INTENT (  out) :: u10_prime, v10_prime, t2_prime, q2_prime  
      logical        , intent (in   ) :: iqtflg
                                                                                         

! Maximum number of iterations in computing psim, psih

!      INTEGER(i_kind), PARAMETER :: k_iteration = 10
!      INTEGER, PARAMETER :: k_iteration = 1

! h10 is the height of 10m where the wind observed
! h2  is the height of 2m where the temperature and 
!        moisture observed.

      REAL(r_kind), PARAMETER :: h10 = 10.0_r_kind
      real(r_kind), parameter :: h2  = two
!
! Default roughness over the land

      REAL(r_kind), PARAMETER :: zint0 = 0.01_r_kind
!
! Von Karman constant

      REAL(r_kind), PARAMETER :: k_kar = 0.4_r_kind
!
! Working variables

      real(r_kind) :: psfc,ps
      REAL(r_kind) :: Vc2, Va2, V2 
      REAL(r_kind) :: rib, xx, yy, cc, Pi
      REAL(r_kind) :: psiw, psiz, mol, ust, hol, holz, hol2
      REAL(r_kind) :: psim, psimz, psim2, psih, psihz, psih2
      REAL(r_kind) :: psit, psit2, psiq, psiq2
      REAL(r_kind) :: gzsoz0, gz10oz0, gz2oz0
      REAL(r_kind) :: eg, qg, tvg, ts       ! change tvs to ts
      REAL(r_kind) :: ths, thg, thvs, thvg
      REAL(r_kind) :: zq0, z0

      real(r_kind) ps_prime,psfc_prime
      REAL(r_kind) :: Vc2_prime, Va2_prime, V2_prime
      REAL(r_kind) :: rib_prime, xx_prime, yy_prime
      REAL(r_kind) :: psiw_prime, psiz_prime, mol_prime, ust_prime, &
              hol_prime, holz_prime, hol2_prime
      REAL(r_kind) :: psim_prime, psimz_prime, psim2_prime, &
              psih_prime, psihz_prime, psih2_prime
      REAL(r_kind) :: psit_prime, psit2_prime, &
              psiq_prime, psiq2_prime
      REAL(r_kind) :: qg_prime, tvg_prime, ts_prime, tvs_prime
      REAL(r_kind) :: ths_prime, thg_prime, thvs_prime, thvg_prime !  add t2_prime
      real(r_kind) t2,q2             

      REAL(r_kind), PARAMETER :: ka = 2.4E-5_r_kind

! local
      real(r_kind),parameter :: r16   = 16.0_r_kind
      real(r_kind),parameter :: r5_5  = 5.5_r_kind
      real(r_kind),parameter :: r1_1  = 1.1_r_kind
      real(r_kind),parameter :: r0_9  = 0.9_r_kind
      real(r_kind),parameter :: r0_75 = 0.75_r_kind

!-----------------------------------------------------------------------------!

!  convert input pressure variables from cb to mb

      psfc = r10*psfc_in
      ps   = r10*ps_in
      psfc_prime = r10*psfc_prime_in

! 1 Compute the roughness length based upon season and land use 
! =====================================

! 1.1 Define the rouhness length
!     -----------------

      z0 = roughness/r100      ! unit change : originally cm --> m

      if( z0 < 0.0001_r_kind) z0 = 0.0001_r_kind

! 1.2 Define the rouhgness length for moisture
!     -----------------

      if ( iland == 0 ) then
         zq0 = z0
      else
         zq0 =  zint0
      endif

! 1.3 Define the some constant variable for psi
!     -----------------

      gzsoz0 = log(hs/z0)

      gz10oz0 = log(h10/z0)

      gz2oz0 = log(h2/z0)


! 2. Calculate the virtual temperature
! =====================================

! 2.1 Compute Virtual temperature on the lowest half sigma level
!     ---------------------------------------------------------

!     tvs_prime  = ts_prime * (one + fv * qs) + fv * ts * qs_prime
!     tvs  = ts * (one + fv * qs)
      ts   = tvs / (one + fv * qs)       !  input now tvs, and need also ts
       
      if(iqtflg)then
         ts_prime = (ts_in + fv*(qs*ts_in-tvs*qs_prime))/(one+fv*qs)**2
         tvs_prime=ts_in
      else
         ts_prime = ts_in
         tvs_prime = (ts_in*(one+fv*qs)**2+fv*tvs*qs_prime)/(one+fv*qs)
      end if

! 2.2 Compute the ground saturated mixing ratio and the ground virtual 
!     temperature
!     ----------------------------------------------------------------

      call DA_TP_To_Qs( tg, psfc, eg, qg )
      call da_TP_To_Qs_Lin( tg, psfc, eg, tg_prime, psfc_prime, qg_prime )
      qg_prime = qg_prime * qg

      tvg_prime  = tg_prime * (one + fv * qg) + fv * tg * qg_prime
      tvg  = tg * (one + fv * qg)

! 3.  Compute the potential temperature and virtual potential temperature
! =======================================================================

! 3.1 Potential temperature on the lowest half sigma level
!     ----------------------------------------------------

      ps_prime=sig1*psfc_prime
      Pi = (r1000 / ps) ** rd_over_cp
      ths_prime  = (ts_prime - ps_prime * rd_over_cp * ts/ps ) * Pi 
      ths  = ts * Pi

! 3.2 Virtual potential temperature on the lowest half sigma level
!     ------------------------------------------------------------

      thvs_prime  = (tvs_prime - ps_prime * rd_over_cp * tvs/ps) * Pi 
      thvs = tvs * Pi

! 3.3 Potential temperature at the ground
!     -----------------------------------

      Pi = (r1000 / psfc) ** rd_over_cp
      thg_prime  = (tg_prime - psfc_prime * rd_over_cp * tg/psfc) * Pi 
      thg  = tg * Pi

! 3.4 Virtual potential temperature at ground
!     ---------------------------------------

      thvg_prime  = (tvg_prime - psfc_prime * rd_over_cp * tvg/psfc) * Pi
      thvg = tvg * Pi

! 4.  BULK RICHARDSON NUMBER AND MONI-OBUKOV LENGTH
! =================================================

! 4.1 Velocity
!     --------
!
!     Wind speed:

      Va2_prime =  two*us*us_prime + two*vs*vs_prime
      Va2 =   us*us + vs*vs
!  
!     Convective velocity:

      if ( thvg >= thvs ) then
         Vc2_prime = four * (thvg_prime - thvs_prime)
         Vc2 = four * (thvg - thvs)
      else
         Vc2_prime = zero
         Vc2 = zero
      endif
!
      V2_prime  = Va2_prime+ Vc2_prime
      V2  = .000001_r_kind + Va2 + Vc2

! 4.2 Bulk richardson number
!     ----------------------

      Pi = grav * hs / (ths*V2)
      rib_prime = ( thvs_prime - thvg_prime   &
                 - (thvs-thvg)/V2  * V2_prime &
                 - (thvs-thvg)/ths * ths_prime ) * Pi 
      rib = (thvs - thvg) * Pi
 
! 5.  CALCULATE PSI BASED UPON REGIME
! =======================================

      SELECT CASE ( regime  ) 

! 5.1 Stable conditions (REGIME 1)
!     ---------------------------

         CASE ( 1 );

            psim_prime  = zero
            psimz_prime = zero
            psim2_prime = zero
            psim  = -r10*gzsoz0
            psimz = -r10*gz10oz0
            psim2 = -r10*gz2oz0
            psim  = max(psim,-r10)
            psimz = max(psimz,-r10)
            psim2 = max(psim2,-r10)

            psih_prime  = psim_prime
            psihz_prime = psimz_prime
            psih2_prime = psim2_prime
            psih  = psim
            psihz = psimz
            psih2 = psim2

! 5.2 Mechanically driven turbulence (REGIME 2)
!     ------------------------------------------

         CASE ( 2 );

            Pi =  - one / ((r1_1 - five*rib)*(r1_1 - five*rib))
            psim_prime  = r5_5 * gzsoz0  * rib_prime * Pi 
            psimz_prime = r5_5 * gz10oz0 * rib_prime * Pi
            psim2_prime = r5_5 * gz2oz0  * rib_prime * Pi

            Pi =  ( -five * rib ) / (r1_1 - five*rib)
            psim  = gzsoz0  * Pi
            psimz = gz10oz0 * Pi
            psim2 = gz2oz0  * Pi

            if ( psim >= -r10 ) then
               psim = psim
               psim_prime = psim_prime
            else
               psim = -r10
               psim_prime = zero
            endif
            if ( psimz >= -r10 ) then
               psimz = psimz
               psimz_prime = psimz_prime
            else
               psimz = -r10
               psimz_prime = zero
            endif
            if ( psim2 >= -r10 ) then
               psim2 = psim2
               psim2_prime = psim2_prime
            else
               psim2 = -r10
               psim2_prime = zero
            endif

            psih_prime  = psim_prime
            psihz_prime = psimz_prime
            psih2_prime = psim2_prime
            psih = psim
            psihz = psimz
            psih2 = psim2

! 5.3 Unstable Forced convection (REGIME 3)
!     -------------------------------------

         CASE ( 3 );

            psim_prime = zero
            psimz_prime = zero
            psim2_prime = zero

            psim = zero
            psimz = zero
            psim2 = zero

            psih_prime = psim_prime
            psihz_prime = psimz_prime
            psih2_prime = psim2_prime
            psih = psim
            psihz = psimz
            psih2 = psim2


! 5.4 Free convection (REGIME 4)
!     --------------------------

         CASE ( 4 );

!      Calculate psi m and pshi h using iteration method
       
            psim_prime = zero
            psih_prime = zero
            psim = zero
            psih = zero
            cc = two * atan(one)

!        do k = 1 , k_iteration

! 5.4.1  Calculate   ust, m/L (mol), h/L (hol)
!        --------------------------

!       Friction speed

            ust = k_kar * sqrt(v2) /( gzsoz0 - psim)
            ust_prime = (half/V2 * v2_prime + psim_prime /(gzsoz0 - psim)) * ust

!       Heat fux factor

            mol = k_kar * (ths - thg )/( gzsoz0 - psih)
            mol_prime = ( (ths_prime - thg_prime ) /(ths - thg) + &
                          psih_prime /( gzsoz0 - psih) ) * mol

!       Ratio of PBL height to Monin-Obukhov length

            if ( ust < r0_01 ) then
               hol_prime = rib_prime * gzsoz0
               hol = rib * gzsoz0
            else
               hol = k_kar * grav * hs * mol / ( ths * ust * ust )
               hol_prime = ( mol_prime / mol - ths_prime / ths &
                             - two* ust_prime / ust ) * hol
            endif

! 5.4.2  Calculate n, nz, R, Rz
!        --------------------------
       
            if ( hol >= zero ) then
               hol_prime = zero
               hol = zero
            else
               hol_prime = hol_prime
               hol = hol
            endif
            if ( hol >= -r10 ) then
               hol_prime = hol_prime
               hol = hol
            else
               hol_prime = zero
               hol = -r10
            endif
           
            holz_prime = (h10 / hs) * hol_prime
            holz = (h10 / hs) * hol
            if ( holz >= zero ) then
               holz_prime = zero
               holz = zero
            else
               holz_prime = holz_prime
               holz = holz
            endif
            if ( holz >= -r10 ) then
               holz_prime = holz_prime
               holz = holz
            else
               holz_prime = zero
               holz = -r10
            endif

            hol2_prime = (h2 / hs) * hol_prime
            hol2 = (h2 / hs) * hol
            if ( hol2 >= zero ) then
               hol2_prime = zero
               hol2 = zero
            else
               hol2_prime = hol2_prime
               hol2 = hol2
            endif
            if ( hol2 >= -r10 ) then
               hol2_prime = hol2_prime
               hol2 = hol2
            else
               hol2_prime = zero
               hol2 = -r10
            endif

! 5.4.3 Calculate Psim & psih
!        --------------------------

!       Using the continuous function:
            xx_prime = -four* hol_prime /((one - r16 * hol) ** r0_75)
            xx = (one - r16 * hol) ** quarter
            yy_prime = two* xx * xx_prime /(one+xx*xx)
            yy = log((one+xx*xx)/two)
            psim_prime = 2 * xx_prime *(one/(one+xx)-one/(1+xx*xx)) + yy_prime 
            psim = two * log((one+xx)/two) + yy - two * atan(xx) + cc
            psih_prime = two * yy_prime
            psih = two * yy

!       Using the continuous function:
            xx_prime = -four* holz_prime /((one - r16 * holz) ** r0_75)
            xx = (one - r16 * holz) ** quarter
            yy_prime = two* xx * xx_prime /(one+xx*xx)
            yy = log((one+xx*xx)/two)
            psimz_prime = two* xx_prime *(one/(one+xx)-one/(1+xx*xx)) + yy_prime
            psimz = two * log((one+xx)/two) + yy - two * atan(xx) + cc
            psihz_prime = two * yy_prime
            psihz = two * yy

!       Using the continuous function:
            xx_prime = -four* hol2_prime /((one - r16 * hol2) ** r0_75)
            xx = (one - r16 * hol2) ** quarter
            yy_prime = two* xx * xx_prime /(one+xx*xx)
            yy = log((one+xx*xx)/two)
            psim2_prime = two* xx_prime *(one/(one+xx)-one/(1+xx*xx)) + yy_prime
            psim2 = two * log((one+xx)/two) + yy - two * atan(xx) + cc
            psih2_prime = two * yy_prime
            psih2 = two * yy

!      enddo 

! 5.4.4 Define the limit value for psim & psih
!        --------------------------

            if ( psim <= r0_9*gzsoz0 ) then
               psim_prime = psim_prime
               psim = psim
            else
               psim = r0_9*gzsoz0
               psim_prime = zero
            endif
            if ( psimz <= r0_9*gz10oz0 ) then
               psimz_prime = psimz_prime
               psimz = psimz
            else
               psimz_prime = zero
               psimz = r0_9*gz10oz0
            endif
            if ( psim2 <= r0_9*gz2oz0 ) then
               psim2_prime = psim2_prime
               psim2 = psim2
            else
               psim2_prime = zero
               psim2 = r0_9*gz2oz0
            endif
            if ( psih <= r0_9*gzsoz0 ) then
               psih_prime = psih_prime
               psih = psih
            else
               psih_prime = zero
               psih = r0_9*gzsoz0
            endif
            if ( psihz <= r0_9*gz10oz0 ) then
               psihz_prime = psihz_prime
               psihz = psihz
            else
               psihz_prime = zero
               psihz = r0_9*gz10oz0
            endif
            if ( psih2 <= r0_9*gz2oz0 ) then
               psih2_prime = psih2_prime
               psih2 = psih2
            else
               psih2_prime = zero
               psih2 = r0_9*gz2oz0
            endif

         CASE DEFAULT;

            write(unit=*, fmt='(/a,i2,a/)') "Regime=",regime," is invalid."
            stop "sfc_wtq_Lin"

      END SELECT

!
! 6.  CALCULATE PSI FOR WIND, TEMPERATURE AND MOISTURE
! =======================================

      psiw_prime = - psim_prime
      psiw = gzsoz0 - psim
      psiz_prime = - psimz_prime
      psiz = gz10oz0 - psimz
      psit_prime = - psih_prime
      psit = gzsoz0 - psih
      psit2_prime = - psih2_prime
      psit2 = gz2oz0 - psih2

      ust = k_kar * sqrt(v2) /( gzsoz0 - psim)
      ust_prime = (half/V2 * v2_prime + psim_prime /(gzsoz0 - psim)) * ust

      psiq2_prime = k_kar*hs/(ka*(k_kar*ust*hs/ka + hs / zq0 ))*ust_prime
      psiq_prime  = psiq2_prime - psih_prime
      psiq2_prime = psiq2_prime - psih2_prime

      psiq  = log(k_kar*ust*hs/ka + hs / zq0 ) - psih
      psiq2 = log(k_kar*ust*h2/ka + h2 / zq0 ) - psih2

! 7.  CALCULATE THE PERTURBATIONS for 10M WIND, 2M TEMPERATURE AND MOISTURE
! =======================================

      Pi = psiz / psiw
      u10_prime= (us_prime + us/psiz * psiz_prime - us/psiw * psiw_prime) * Pi
      v10_prime= (vs_prime + vs/psiz * psiz_prime - vs/psiw * psiw_prime) * Pi

      t2_prime = ( (one-psit2/psit) * thg_prime + ( ths_prime + &
                              (ths - thg)/psit2 * psit2_prime - &
                              (ths - thg)/psit  * psit_prime ) * psit2/psit &
                + rd_over_cp*( thg + ( ths - thg )*psit2/psit)/psfc * psfc_prime ) &
                * (psfc/r1000)**rd_over_cp

      q2_prime = (one-psiq2/psiq) * qg_prime + psiq2/psiq * qs_prime + &
                 (qs -qg)*(psiq2/psiq) * (psiq2_prime/psiq2 - psiq_prime/psiq)

      t2 = ( thg + ( ths - thg )*psit2/psit)*(psfc/r1000)**rd_over_cp
      q2 = qg + (qs - qg)*psiq2/psiq
      if(iqtflg)then
         t2_prime=t2_prime*(one+fv*q2)+fv*t2*q2_prime
      end if

END SUBROUTINE sfc_wtq_Lin

SUBROUTINE DA_TP_To_Qs_Lin( t, p, es, t_prime, p_prime, &
                             qs_prime_over_qs )
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   DA_TP_To_Qs_Lin
!
!   prgrmmr:
!
! abstract:  Convert es/p/es_prime to saturation specific humidity increment.
!   METHOD:  qs~ = qs * ( p es'/es - p' ) / ( p - (1-rd_over_rv) es ).
!            Use Rogers & Yau (1989) formula: es = a exp( bTc / (T_c + c) ).
!
! program history log:
!   2000-10-03 Barker  - Creation of F90 version.
!   2008-04-14 safford - add standard documentation block, rm unused uses
!
!   input argument list:
!     t                - temperature
!     p                - pressure
!     es               - Sat. vapour pressure
!     t_prime          - temperature increment
!     p_prime          - pressure increment
!
!   output argument list:
!     qs_prime_over_qs - qs~/qs
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds,only: r_kind
   use constants, only: omeps,t0c

   IMPLICIT NONE

   REAL(r_kind), INTENT(IN   ) :: t                ! Temperature.
   REAL(r_kind), INTENT(IN   ) :: p                ! Pressure.
   REAL(r_kind), INTENT(IN   ) :: es               ! Sat. vapour pressure.
   REAL(r_kind), INTENT(IN   ) :: t_prime          ! Temperature increment.
   REAL(r_kind), INTENT(IN   ) :: p_prime          ! Pressure increment.
   REAL(r_kind), INTENT(  OUT) :: qs_prime_over_qs ! qs~/qs.

!  Saturation Vapour Pressure Constants(Rogers & Yau, 1989)
!  REAL(r_kind), PARAMETER    :: es_alpha = 611.2_r_kind
   REAL(r_kind), PARAMETER    :: es_beta = 17.67_r_kind
   REAL(r_kind), PARAMETER    :: es_gamma = 243.5_r_kind
   REAL(r_kind), PARAMETER    :: es_gammakelvin = es_gamma-t0c
   REAL(r_kind), PARAMETER    :: es_gammabeta = es_gamma*es_beta
   
   REAL(r_kind)                          :: temp           ! Temporary value.
   REAL(r_kind)                          :: es_prime_over_es ! es~/es

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   temp = t + es_gammakelvin
   
!------------------------------------------------------------------------------
!  [2.0] Calculate saturation vapour pressure increment:
!------------------------------------------------------------------------------

   es_prime_over_es = es_gammabeta * t_prime / ( temp * temp )

!------------------------------------------------------------------------------
!  [3.0] Calculate saturation specific humidity increment:
!------------------------------------------------------------------------------

   qs_prime_over_qs = ( p * es_prime_over_es - p_prime ) / &
                      ( p - omeps * es )

END SUBROUTINE DA_TP_To_Qs_Lin


subroutine get_tlm_tsfc(tlm_tsfc,psges2,tgges,prsltmp2, &
                  tvtmp,qtmp,utmp,vtmp,hsges,roges,msges,regime,iqtflg)
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   get_tlm_tsfc
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-14 safford  - add documentation block
!
!   input argument list:
!     psges2       -
!     tgges        -
!     prsltmp2     -
!     tvtmp        -
!     qtmp         -
!     utmp         -
!     vtmp         -
!     hsges        -
!     roges        -
!     msges        -
!     regime       -
!     iqtflg       -
!
!   output argument list:
!     tlm_tsfc     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one

  implicit none

  real(r_kind)   ,intent(  out) :: tlm_tsfc(6)
  real(r_kind)   ,intent(in   ) :: psges2,tgges,prsltmp2,tvtmp,qtmp,utmp,vtmp,hsges,roges
  integer(i_kind),intent(in   ) :: regime,msges
  logical        ,intent(in   ) :: iqtflg

  real(r_kind) perturb(6),u10_prime,v10_prime,q2_prime
  real(r_kind) sig1
  integer(i_kind) i

  sig1=prsltmp2/psges2
  do i=1,6

!  1  -- psfc_prime   (perturbation of psfc, psfc in cb)
!  2  -- tg_prime
!  3  -- ts_prime     (sensible or virtual depending on iqtflg)
!  4  -- qs_prime
!  5  -- us_prime
!  6  -- vs_prime

     perturb=zero
     perturb(i)=one
     call sfc_wtq_lin(psges2,tgges,prsltmp2,tvtmp,qtmp,utmp,vtmp,regime, &
             perturb(1),perturb(2),sig1,perturb(3),perturb(4),perturb(5),perturb(6), &
             hsges,roges,msges,u10_prime,v10_prime,tlm_tsfc(i),q2_prime,iqtflg)
  end do

end subroutine get_tlm_tsfc
