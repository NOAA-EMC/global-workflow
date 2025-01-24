SUBROUTINE hydro_mxr_thompson (nx, ny, nz, t_3d, p_3d, ref_3d, qr_3d, qnr_3d, qs_3d, istatus, mype )
!
! PURPOSE:
!   Calculate (1) snow mixing ratio, (2) rain mixing ratio, and (3) rain number concentration
!   from reflectivity for Thompson microphysics scheme.  A Marshall-Palmer drop-size distribution
!   is assumed for rain.
!
! HISTORY:
!   2013-01-30:  created by David Dowell, Greg Thompson, Ming Hu
!
! ACKNOWLEDGMENTS:
!   Donghai Wang and Eric Kemp (code template from pcp_mxr_ferrier)
!
!   input argument list:
!     nx          - no. of lons on subdomain (buffer points on ends)
!     ny          - no. of lats on subdomain (buffer points on ends)
!     nz          - no. of levels
!     t_3d        - 3D background temperature (K)
!     p_3d        - 3D background pressure  (hPa)
!     ref_3d      - 3D reflectivity in analysis grid (dBZ)
!
!   output argument list:
!     qr_3d       - rain mixing ratio (g/kg)
!     qnr_3d      - rain number concentration (/kg)
!     qs_3d       - snow mixing ratio (g/kg)
!     istatus     -
!


!-----------------------------------------------------------------------
!
!  Variable Declarations.
!
!-----------------------------------------------------------------------
!
  use kinds, only: r_single, i_kind, r_kind
  IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
! INPUT:
  INTEGER(i_kind),intent(in) :: nx,ny,nz            ! Model grid size
  REAL(r_kind),   intent(inout) :: ref_3d(nx,ny,nz) ! radar reflectivity (dBZ)
  REAL(r_single), intent(in) :: t_3d(nx,ny,nz)      ! Temperature (deg. Kelvin)
  REAL(r_single), intent(in) :: p_3d(nx,ny,nz)      ! Pressure (Pascal)
  INTEGER(i_kind),intent(in) :: mype
!
! OUTPUT:
  INTEGER(i_kind),intent(out):: istatus
  REAL(r_single),intent(out) :: qs_3d(nx,ny,nz)  ! snow mixing ratio (g/kg)
  REAL(r_single),intent(out) :: qr_3d(nx,ny,nz)  ! rain mixing ratio (g/kg)
  REAL(r_single),intent(out) :: qnr_3d(nx,ny,nz) ! rain number concentration (/kg)
!
! PARAMETERS:
  REAL(r_kind), PARAMETER :: min_ref = 0.0_r_kind         ! minimum reflectivity (dBZ) for converting to qs and qr
  REAL(r_kind), PARAMETER :: max_ref_snow = 28.0_r_kind   ! maximum reflectivity (dBZ) for converting to qs
                                                          !   (values above max_ref are treated as max_ref)
  REAL(r_kind), PARAMETER :: max_ref_rain = 55.0_r_kind   ! maximum reflectivity (dBZ) for converting to qr
                                                          !   (values above max_ref are treated as max_ref)
  REAL(r_kind), PARAMETER :: n0r_mp = 8.0e6_r_kind        ! Marshall-Palmer intercept parameter for rain (m**-4)
  REAL(r_kind), PARAMETER :: rd= 287.0_r_kind             ! Gas constant for dry air  (m**2/(s**2*K))
  REAL(r_kind), PARAMETER :: am_s = 0.069_r_kind
  REAL(r_kind), PARAMETER :: bm_s = 2.0_r_kind
  REAL(r_kind), PARAMETER :: PI = 3.1415926536_r_kind
  REAL(r_kind), PARAMETER :: rho_i = 890.0_r_kind
  REAL(r_kind), PARAMETER :: rho_w = 1000.0_r_kind
!
! LOCAL VARIABLES:
  INTEGER(i_kind) :: i,j,k
  REAL(r_kind) :: rho         ! air density (kg m**-3)
  REAL(r_kind) :: zes         ! reflectivity (m**6 m**-3) associated with snow
  REAL(r_kind) :: zer         ! reflectivity (m**6 m**-3) associated with rain
  REAL(r_kind) :: tc          ! temperature (Celsius)
  REAL(r_kind) :: rfract      ! rain fraction
  REAL(r_kind) :: tc0  
  REAL(r_kind) :: smo2
  REAL(r_kind) :: rs
  REAL(r_kind) :: f
  REAL(r_kind) :: loga_
  REAL(r_kind) :: a_
  REAL(r_kind), PARAMETER :: a_min = 1.0e-5_r_kind     ! lower bound for a_, to avoid large mixing ratios retrieved
                                                       !   for tiny particles sizes in cold temperatures
  REAL(r_kind) :: b_
  REAL(r_kind) :: sa(10)
  REAL(r_kind) :: sb(10)
  REAL(r_kind) :: cse(3)
  REAL(r_kind) :: crg(4)
  REAL(r_kind) :: am_r
  REAL(r_kind) :: oams
  REAL(r_kind) :: qs          ! snow mixing ratio in kg / kg
  REAL(r_kind) :: qr          ! rain mixing ratio in kg / kg
!
! for snow moments conversions (from Field et al. 2005)
  DATA sa / 5.065339, -0.062659, -3.032362, 0.029469, -0.000285,   &
            0.31255,   0.000204,  0.003199, 0.0,      -0.015952/
  DATA sb / 0.476221, -0.015896,  0.165977, 0.007468, -0.000141,   &
            0.060366,  0.000079,  0.000594, 0.0,      -0.003577/

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  Beginning of executable code...
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  istatus=0

  f = (0.176_r_kind/0.93_r_kind) * (6.0_r_kind/PI)*(6.0_r_kind/PI) * (am_s/rho_i)*(am_s/rho_i)
  cse(1) = bm_s + 1.0_r_kind
  cse(2) = bm_s + 2.0_r_kind
  cse(3) = bm_s * 2.0_r_kind
  oams = 1.0_r_kind / am_s

  crg(1) = 24.0_r_kind
  crg(2) = 1.0_r_kind
  crg(3) = 24.0_r_kind
  crg(4) = 5040.0_r_kind
  am_r = PI * rho_w / 6.0_r_kind

  DO k = 2,nz-1
    DO j = 2,ny-1
      DO i = 2,nx-1

        IF (ref_3d(i,j,k) >= min_ref) THEN

          rho = p_3d(i,j,k) / (rd*t_3d(i,j,k))
          tc = t_3d(i,j,k) - 273.15_r_kind

          IF (tc <= 0.0_r_kind) THEN
            rfract = 0.0_r_kind
          ELSE IF (tc >= 5.0_r_kind) THEN
            rfract = 1.0_r_kind
          ELSE
            rfract = 0.20_r_kind*tc
          ENDIF

          zes = ( 10.0_r_kind**( 0.1_r_kind * min(ref_3d(i,j,k), max_ref_snow) ) )  &
                * (1.0_r_kind-rfract)                                               &
                * 1.0e-18_r_kind      ! conversion from (mm**6 m**-3) to (m**6 m**-3)

          zer = ( 10.0_r_kind**( 0.1_r_kind * min(ref_3d(i,j,k), max_ref_rain) ) )  &
                * rfract                                                            &
                * 1.0e-18_r_kind      ! conversion from (mm**6 m**-3) to (m**6 m**-3)

          tc0 = MIN(-0.1, tc)
          IF (bm_s.lt.(1.999_r_kind) .or. bm_s.gt.(2.001_r_kind)) THEN
            PRINT*, 'ABORT (hydro_mxr_thompson):  bm_s = ', bm_s
            STOP
          ENDIF
         
          ! Calculate bm_s*2 (th) moment.  Useful for reflectivity.
          loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(3)                 &
                + sa(4)*tc0*cse(3) + sa(5)*tc0*tc0                 &
                + sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3)       &
                + sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0      &
                + sa(10)*cse(3)*cse(3)*cse(3) 
          a_ = max( 10.0_r_kind ** loga_, a_min )
          b_ = sb(1) + sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3) &
               + sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3)               &
               + sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3)    &
               + sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)

          qs = ( (zes / (f*a_)) ** (1.0_r_kind / b_) ) / (rho*oams)
          qs_3d(i,j,k) = 1000.0_r_kind * qs       ! convert from kg / kg to g / kg

          qr = n0r_mp * am_r * crg(3) / rho * (zer / (n0r_mp*crg(4)))**(4.0_r_kind/7.0_r_kind)
          qnr_3d(i,j,k) = (n0r_mp/rho)**(3.0_r_kind/4.0_r_kind)           &
                        * (qr / (am_r * crg(3)))**(1.0_r_kind/4.0_r_kind)

          qnr_3d(i,j,k) = max(1.0_r_kind, qnr_3d(i,j,k))
          qr_3d(i,j,k) = 1000.0_r_kind * qr       ! convert from kg / kg to g / kg


!          if(mype==51 ) then
!            write(*,'(a10,3i5,2f10.5,3f8.2)') 'b=',i,j,k,qs_3d(i,j,k),qr_3d(i,j,k),ref_3d(i,j,k),&
!                        p_3d(i,j,k)/100.0,tc
!          endif


        ELSE

          qs_3d(i,j,k) = -999._r_kind
          qr_3d(i,j,k) = -999._r_kind
          qnr_3d(i,j,k) = -999._r_kind

        END IF

      END DO ! k
    END DO ! i
  END DO ! j
!
!  PRINT*,'finish hydro_mxr_thompson...'
!
!-----------------------------------------------------------------------
!
  istatus = 1
!
  RETURN
END SUBROUTINE hydro_mxr_thompson
