!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************

module monin_obukhov_mod

!==============================================================================
! Kernel routines
!==============================================================================

! explicit interface to all kernel routines

  use ocean_rough_mod, only: compute_ocean_roughness
 
  implicit none
  private
  
  public :: Mon_obkv

  integer, parameter :: i8 = selected_int_kind(18)
  integer(i8)        :: ier_tot, ier
  integer :: i, j, ier_l, n

  real, parameter :: grav          = 9.80
  real, parameter :: vonkarm       = 0.4
  real, parameter :: error         = 1.0e-4
  real, parameter :: zeta_min      = 1.0e-6
  integer, parameter :: max_iter      = 20
  real, parameter :: small         = 1.0e-4
  logical, parameter :: neutral       = .false.
  integer, parameter :: stable_option = 1
  real, parameter :: rich_crit     =10.0
  real, parameter :: zeta_trans    = 0.5
  real, parameter :: drag_min      = 1.0e-5
  real, parameter :: ustar_min     = 1.e-10
  real, parameter :: rdgas = 287.04
  real, parameter :: kappa = 2./7.
  real, parameter :: cp_air = rdgas/kappa
  real, parameter :: zref   = 10.
  real, parameter :: zref_t = 2.

contains

  subroutine Mon_obkv(zvir, ps, t_atm, z, rho, p_atm, u_atm, v_atm,   &
                      t_surf0, q_surf0, q_atm, flux_t, flux_q, flux_u, &
                      flux_v, u_star, delm, dt, mu, t_fac, master)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   pt, virtual potential temperature at lowest model level (kelvin)
!!   pt0, virtual potential temperature at surface (kelvin)
!!   t_atm:  temperature at the lowest model layer
!!   z, height above surface of lowest model layer (meter)
!!   rho, air density
!!   p_atm, pressure at lowest model level (Pa)
!!   u_atm, x-dir wind velocity at lowest model level (m/s)
!!   v_atm, y-dir wind velocity at lowest model level (m/s)
!!   t_surf0, SST (kelvin)
!!   th_atm, potential temperature using surface pressure as reference
!!   q_surf0, mixing ratio at surface
!!   q_atm, mixing ratio at lowest model level
!!   flux_t, heat flux (W/m^2)
!!   flux_q, moisture flux (kg/sm^2)
!!   flux_v, momentum flux (N/m^2, kg/ms^2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SJL:
! PS: surface pressure (Pa)

  logical:: master
  real, intent(in):: zvir, dt
  real, intent(in):: t_fac        ! t_flux enhancer!
  real, intent(in), dimension(:,:):: ps, t_atm
  real, intent(in) :: z(:,:), rho(:,:), delm(:,:)
  real, intent(in) :: p_atm(:,:), u_atm(:,:), v_atm(:,:)
  real, intent(in) :: t_surf0(:,:), q_surf0(:,:), q_atm(:,:)
  real, intent(inout) :: u_star(:,:)
  real, intent(out) :: flux_t(:,:), flux_q(:,:), flux_u(:,:), flux_v(:,:)   
  real, intent(out) :: mu(:,:)

  logical, dimension(size(ps,1)) :: avail 
  logical :: lavail
  real, dimension(size(ps,1),size(ps,2)) :: speed, drag_m, drag_t, drag_q, rho_drag
  real, dimension(size(ps,1),size(ps,2)) :: b_star, u_surf0, v_surf0
  real, dimension(size(ps,1),size(ps,2)) :: rough_mom, rough_heat, rough_moist
!
! Local:
  real, dimension(size(ps,1),size(ps,2)) :: pt, pt0, p_fac, deno
  real, parameter:: p00 = 1.E5
  integer:: i,j

     p_fac(:,:) = (ps(:,:)/p_atm(:,:)) ** kappa
        pt(:,:) =   t_atm(:,:)*(1.+zvir*q_atm(:,:  ))*(p00/p_atm(:,:))**kappa
       pt0(:,:) = t_surf0(:,:)*(1.+zvir*q_surf0(:,:))*(p00/   ps(:,:))**kappa
     speed(:,:) = sqrt(u_atm(:,:)**2+v_atm(:,:)**2) 

  lavail = .false.
  avail  = .true.

#ifdef MON_DEBUG
  if ( master ) write(*,*) 'p_atm=',maxval(p_atm)
  if ( master ) write(*,*) 'u_atm=',maxval(u_atm)
  if ( master ) write(*,*) 'v_atm=',maxval(v_atm)
  if ( master ) write(*,*) 't_surf0=',maxval(t_surf0)
  if ( master ) write(*,*) 'q_surf0=',maxval(q_surf0)
  if ( master ) write(*,*) 'q_atm=',maxval(q_atm)
  if ( master ) write(*,*) 'u_star=',maxval(u_star)
#endif

! u_star should be an output?
  call compute_ocean_roughness ( u_star, speed, rough_mom, rough_heat, rough_moist, master )

  n=size(ps,1)
  do j = 1, size(ps,2)
     call monin_obukhov_drag_1d(grav, vonkarm, error, zeta_min, max_iter, small,     &
                                neutral, stable_option, rich_crit, zeta_trans, drag_min,  &
                                n, pt(:,j), pt0(:,j), z(:,j), rough_mom(:,j),             &
                                rough_heat(:,j), rough_moist(:,j), speed(:,j), drag_m(:,j), drag_t(:,j), &
                                drag_q(:,j), u_star(:,j), b_star(:,j), lavail, avail, ier_l)
  end do

! Ocean currents:
  u_surf0(:,:) = 0.
  v_surf0(:,:) = 0.

! momentum flux
        mu(:,:) = drag_m(:,:)*speed(:,:)   ! diffusion coefficient / Z
  rho_drag(:,:) = rho(:,:)*mu(:,:)

#ifdef IMPLICIT_FLUX
  flux_u(:,:) = rho_drag(:,:) * (u_surf0(:,:) - u_atm(:,:))
  flux_v(:,:) = rho_drag(:,:) * (v_surf0(:,:) - v_atm(:,:))

! flux of sensible heat (W/m**2)
  rho_drag(:,:) = rho(:,:)*speed(:,:)
    flux_t(:,:) = cp_air*drag_t(:,:)*rho_drag(:,:)*(t_surf0(:,:)-t_atm(:,:)*p_fac(:,:))*t_fac
!                                         flux of water vapor  (Kg/(m**2 s))
    flux_q(:,:) =        drag_q(:,:)*rho_drag(:,:)*(q_surf0(:,:)-q_atm(:,:)) 

#else
  deno(:,:) = 1. + dt*rho_drag(:,:)/delm(:,:)
  flux_u(:,:) = rho_drag(:,:) * (u_surf0(:,:) - u_atm(:,:)) / deno(:,:)
  flux_v(:,:) = rho_drag(:,:) * (v_surf0(:,:) - v_atm(:,:)) / deno(:,:)
!                                         flux of sensible heat (W/m**2)
! flux of sensible heat (W/m**2)
  rho_drag(:,:) = rho(:,:)*drag_t(:,:)*speed(:,:) * t_fac
      deno(:,:) = delm(:,:) / ( delm(:,:) +  dt*rho_drag(:,:)*p_fac(:,:))
  flux_t(:,:) = cp_air*rho_drag(:,:)*(t_surf0(:,:)-t_atm(:,:)*p_fac(:,:))*deno(:,:)
!                                         flux of water vapor  (Kg/(m**2 s))
  rho_drag(:,:) = rho(:,:)*drag_q(:,:)*speed(:,:)
  deno(:,:) = 1. + dt*rho_drag(:,:)/delm(:,:)
  flux_q(:,:) = rho_drag(:,:)*(q_surf0(:,:)-q_atm(:,:))/deno(:,:) 
#endif

  end subroutine Mon_obkv

!==============================================================================
   subroutine monin_obukhov_drag_1d(grav, vonkarm,               &
     & error, zeta_min, max_iter, small,                         &
     & neutral, stable_option, rich_crit, zeta_trans, drag_min,  &
     & n, pt, pt0, z, z0, zt, zq, speed, drag_m, drag_t,         &
     & drag_q, u_star, b_star, lavail, avail, ier)

  implicit none

  real   , intent(in   )                :: grav     
  real   , intent(in   )                :: vonkarm   
  real   , intent(in   )                :: error    ! = 1.e-04
  real   , intent(in   )                :: zeta_min ! = 1.e-06
  integer, intent(in   )                :: max_iter ! = 20
  real   , intent(in   )                :: small    ! = 1.e-04
  logical, intent(in   )                :: neutral
  integer, intent(in   )                :: stable_option
  real   , intent(in   )                :: rich_crit, zeta_trans, drag_min
  integer, intent(in   )                :: n
  real   , intent(in   ), dimension(n)  :: pt, pt0, z, z0, zt, zq, speed
  real   , intent(inout), dimension(n)  :: drag_m, drag_t, drag_q, u_star, b_star
  logical, intent(in   )                :: lavail ! whether to use provided mask or not
  logical, intent(in   ), dimension(n)  :: avail  ! provided mask
  integer, intent(out  )                :: ier

  real   , dimension(n) :: rich, fm, ft, fq, zz
  logical, dimension(n) :: mask, mask_1, mask_2
  real   , dimension(n) :: delta_b !!, us, bs, qs
  real                  :: r_crit, sqrt_drag_min
  real                  :: us, bs, qs
  integer               :: i

  r_crit = 0.95*rich_crit  ! convergence can get slow if one is 
                           ! close to rich_crit
  sqrt_drag_min = 0.0
  if(drag_min.ne.0.0) sqrt_drag_min = sqrt(drag_min)

  mask = .true. 
!  if(lavail) mask = avail

  where(mask) 
     delta_b = grav*(pt0 - pt)/pt0
     rich    = - z*delta_b/(speed*speed + small)
     zz      = max(z,z0,zt,zq)
  elsewhere 
     rich = 0.0
  end where

  if(neutral) then

     do i = 1, n
        if(mask(i)) then
           fm(i)   = log(zz(i)/z0(i))
           ft(i)   = log(zz(i)/zt(i))
           fq(i)   = log(zz(i)/zq(i))
           us   = vonkarm/fm(i)
           bs   = vonkarm/ft(i)
           qs   = vonkarm/fq(i)
           drag_m(i)    = us*us
           drag_t(i)    = us*bs
           drag_q(i)    = us*qs
           u_star(i) = us*speed(i)
           b_star(i) = bs*delta_b(i)
        end if
     enddo

  else

     mask_1 = mask .and. rich <  r_crit
     mask_2 = mask .and. rich >= r_crit

     do i = 1, n
        if(mask_2(i)) then
           drag_m(i)   = drag_min
           drag_t(i)   = drag_min
           drag_q(i)   = drag_min
           us       = sqrt_drag_min
           bs       = sqrt_drag_min
           u_star(i)   = us*speed(i)
           b_star(i)   = bs*delta_b(i)
        end if
     enddo

     call monin_obukhov_solve_zeta (error, zeta_min, max_iter, small, &
          & stable_option, rich_crit, zeta_trans,                     &
          & n, rich, zz, z0, zt, zq, fm, ft, fq, mask_1, ier)

     do i = 1, n
        if(mask_1(i)) then
           us   = max(vonkarm/fm(i), sqrt_drag_min)
           bs   = max(vonkarm/ft(i), sqrt_drag_min)
           qs   = max(vonkarm/fq(i), sqrt_drag_min)
           drag_m(i)   = us*us
           drag_t(i)   = us*bs
           drag_q(i)   = us*qs
           u_star(i)   = us*speed(i)
           b_star(i)   = bs*delta_b(i)
        endif
     enddo

  end if

end subroutine monin_obukhov_drag_1d
!==============================================================================
  subroutine monin_obukhov_solve_zeta(error, zeta_min, max_iter, small,  &
     & stable_option, rich_crit, zeta_trans,                           &
     & n, rich, z, z0, zt, zq, f_m, f_t, f_q, mask, ier)

  implicit none

  real   , intent(in   )                :: error    ! = 1.e-04
  real   , intent(in   )                :: zeta_min ! = 1.e-06
  integer, intent(in   )                :: max_iter ! = 20
  real   , intent(in   )                :: small    ! = 1.e-04
  integer, intent(in   )                :: stable_option
  real   , intent(in   )                :: rich_crit, zeta_trans
  integer, intent(in   )                :: n
  real   , intent(in   ), dimension(n)  :: rich, z, z0, zt, zq
  logical, intent(in   ), dimension(n)  :: mask
  real   , intent(  out), dimension(n)  :: f_m, f_t, f_q
  integer, intent(  out)                :: ier


  real    :: max_cor
  integer :: iter

  real, dimension(n) ::   &
       d_rich, rich_1, correction, corr, z_z0, z_zt, z_zq, &
       ln_z_z0, ln_z_zt, ln_z_zq, zeta,                    &
       phi_m, phi_m_0, phi_t, phi_t_0, rzeta,              &
       zeta_0, zeta_t, zeta_q, df_m, df_t

  logical, dimension(n) :: mask_1

  ier = 0

  z_z0 = z/z0
  z_zt = z/zt
  z_zq = z/zq
  ln_z_z0 = log(z_z0)
  ln_z_zt = log(z_zt)
  ln_z_zq = log(z_zq)

  corr = 0.0
  mask_1 = mask

  ! initial guess

  zeta = 0.0
  where(mask_1) 
     zeta = rich*ln_z_z0*ln_z_z0/ln_z_zt
  end where

  where (mask_1 .and. rich >= 0.0)
     zeta = zeta/(1.0 - rich/rich_crit)
  end where

  iter_loop: do iter = 1, max_iter

     where (mask_1 .and. abs(zeta).lt.zeta_min) 
        zeta = 0.0
        f_m = ln_z_z0
        f_t = ln_z_zt
        f_q = ln_z_zq
        mask_1 = .false.  ! don't do any more calculations at these pts
     end where

     
     zeta_0 = 0.0
     zeta_t = 0.0
     zeta_q = 0.0
     where (mask_1)
        rzeta  = 1.0/zeta
        zeta_0 = zeta/z_z0
        zeta_t = zeta/z_zt
        zeta_q = zeta/z_zq
     end where

     call monin_obukhov_derivative_m(stable_option, rich_crit, zeta_trans, &
          & n, phi_m  , zeta  , mask_1, ier)
     call monin_obukhov_derivative_m(stable_option, rich_crit, zeta_trans, &
          & n, phi_m_0, zeta_0,  mask_1, ier)
     call monin_obukhov_derivative_t(stable_option, rich_crit, zeta_trans, &
          & n, phi_t  , zeta  , mask_1, ier)
     call monin_obukhov_derivative_t(stable_option, rich_crit, zeta_trans, &
          & n, phi_t_0, zeta_t, mask_1, ier)

     call monin_obukhov_integral_m(stable_option, rich_crit, zeta_trans, &
          & n, f_m, zeta, zeta_0, ln_z_z0, mask_1, ier)
     call monin_obukhov_integral_tq(stable_option, rich_crit, zeta_trans, &
          & n, f_t, f_q, zeta, zeta_t, zeta_q, ln_z_zt, ln_z_zq, mask_1, ier)

     where (mask_1)
        df_m  = (phi_m - phi_m_0)*rzeta
        df_t  = (phi_t - phi_t_0)*rzeta
        rich_1 = zeta*f_t/(f_m*f_m)
        d_rich = rich_1*( rzeta +  df_t/f_t - 2.0 *df_m/f_m) 
        correction = (rich - rich_1)/d_rich  
        corr = min(abs(correction),abs(correction/zeta)) 
        ! the criterion corr < error seems to work ok, but is a bit arbitrary
        !  when zeta is small the tolerance is reduced
     end where

     max_cor= maxval(corr)

     if(max_cor > error) then
        mask_1 = mask_1 .and. (corr > error)  
        ! change the mask so computation proceeds only on non-converged points
        where(mask_1) 
           zeta = zeta + correction
        end where
        cycle iter_loop
     else
        return
     end if

  end do iter_loop

  ier = 1 ! surface drag iteration did not converge

end subroutine monin_obukhov_solve_zeta
!==============================================================================
  subroutine monin_obukhov_derivative_t(stable_option, rich_crit, zeta_trans, &
     & n, phi_t, zeta, mask, ier)

  ! the differential similarity function for buoyancy and tracers
  ! Note: seems to be the same as monin_obukhov_derivative_m?

  implicit none

  integer, intent(in   )                :: stable_option
  real   , intent(in   )                :: rich_crit, zeta_trans
  integer, intent(in   )                :: n
  real   , intent(  out), dimension(n)  :: phi_t
  real   , intent(in   ), dimension(n)  :: zeta
  logical, intent(in   ), dimension(n)  :: mask  
  integer, intent(  out)                :: ier

  logical, dimension(n) :: stable, unstable
  real                  :: b_stab, lambda

  ier = 0
  b_stab     = 1.0/rich_crit

  stable   = mask .and. zeta >= 0.0
  unstable = mask .and. zeta <  0.0

  where (unstable) 
     phi_t = (1 - 16.0*zeta)**(-0.5)
  end where

  if(stable_option == 1) then 

     where (stable) 
        phi_t = 1.0 + zeta*(5.0 + b_stab*zeta)/(1.0 + zeta)
     end where

  else if(stable_option == 2) then

     lambda = 1.0 + (5.0 - b_stab)*zeta_trans

     where (stable .and. zeta < zeta_trans)
        phi_t = 1 + 5.0*zeta
     end where
     where (stable .and. zeta >= zeta_trans)
        phi_t = lambda + b_stab*zeta
     end where

  endif

end subroutine monin_obukhov_derivative_t
!=============================================================================
  subroutine monin_obukhov_derivative_m(stable_option, rich_crit, zeta_trans, &
     & n, phi_m, zeta, mask, ier)

  ! the differential similarity function for momentum

  implicit none

  integer, intent(in   )                :: stable_option
  real   , intent(in   )                :: rich_crit, zeta_trans
  integer, intent(in   )                :: n
  real   , intent(  out), dimension(n)  :: phi_m
  real   , intent(in   ), dimension(n)  :: zeta
  logical, intent(in   ), dimension(n)  :: mask
  integer, intent(out  )                :: ier

  logical, dimension(n) :: stable, unstable
  real   , dimension(n) :: x
  real                  :: b_stab, lambda


  ier = 0
  b_stab     = 1.0/rich_crit

  stable   = mask .and. zeta >= 0.0
  unstable = mask .and. zeta <  0.0

  where (unstable) 
     x     = (1 - 16.0*zeta  )**(-0.5)
     phi_m = sqrt(x)  ! phi_m = (1 - 16.0*zeta)**(-0.25)
  end where

  if(stable_option == 1) then 

     where (stable) 
        phi_m = 1.0 + zeta  *(5.0 + b_stab*zeta)/(1.0 + zeta)
     end where

  else if(stable_option == 2) then

     lambda = 1.0 + (5.0 - b_stab)*zeta_trans

     where (stable .and. zeta < zeta_trans)
        phi_m = 1 + 5.0*zeta
     end where
     where (stable .and. zeta >= zeta_trans)
        phi_m = lambda + b_stab*zeta
     end where

  endif

end subroutine monin_obukhov_derivative_m
!==============================================================================
  subroutine monin_obukhov_integral_m(stable_option, rich_crit, zeta_trans, &
     & n, psi_m, zeta, zeta_0, ln_z_z0, mask, ier)

  !  the integral similarity function for momentum

  implicit none

  integer, intent(in   )                :: stable_option
  real   , intent(in   )                :: rich_crit, zeta_trans
  integer, intent(in   )                :: n
  real   , intent(  out), dimension(n)  :: psi_m
  real   , intent(in)   , dimension(n)  :: zeta, zeta_0, ln_z_z0
  logical, intent(in)   , dimension(n)  :: mask
  integer, intent(out)                  :: ier

  real                   :: b_stab, lambda

  real, dimension(n) :: x, x_0, x1, x1_0, num, denom, y
  logical, dimension(n) :: stable, unstable, &
       weakly_stable, strongly_stable

  ier = 0

  b_stab     = 1.0/rich_crit

  stable   = mask .and. zeta >= 0.0
  unstable = mask .and. zeta <  0.0

  where(unstable) 

     x     = sqrt(1 - 16.0*zeta)
     x_0   = sqrt(1 - 16.0*zeta_0)

     x      = sqrt(x)
     x_0    = sqrt(x_0)

     x1     = 1.0 + x
     x1_0   = 1.0 + x_0

     num    = x1*x1*(1.0 + x*x)
     denom  = x1_0*x1_0*(1.0 + x_0*x_0)
     y      = atan(x) - atan(x_0)
     psi_m  = ln_z_z0 - log(num/denom) + 2*y

  end where

  if( stable_option == 1) then

     where (stable) 
        psi_m = ln_z_z0 + (5.0 - b_stab)*log((1.0 + zeta)/(1.0 + zeta_0)) &
             + b_stab*(zeta - zeta_0) 
     end where

  else if (stable_option == 2) then

     lambda = 1.0 + (5.0 - b_stab)*zeta_trans

     weakly_stable   = stable .and. zeta <= zeta_trans
     strongly_stable = stable .and. zeta >  zeta_trans

     where (weakly_stable)
        psi_m = ln_z_z0 + 5.0*(zeta - zeta_0) 
     end where

     where(strongly_stable)
        x = (lambda - 1.0)*log(zeta/zeta_trans) + b_stab*(zeta - zeta_trans)
     endwhere

     where (strongly_stable .and. zeta_0 <= zeta_trans)
        psi_m = ln_z_z0 + x + 5.0*(zeta_trans - zeta_0)
     end where
     where (strongly_stable .and. zeta_0 > zeta_trans)
        psi_m = lambda*ln_z_z0 + b_stab*(zeta  - zeta_0)
     endwhere

  end if

end subroutine monin_obukhov_integral_m
!==============================================================================
  subroutine monin_obukhov_integral_tq(stable_option, rich_crit, zeta_trans, &
     & n, psi_t, psi_q, zeta, zeta_t, zeta_q, &
     & ln_z_zt, ln_z_zq, mask, ier)

  ! the integral similarity function for moisture and tracers

  implicit none

  integer, intent(in   )                :: stable_option
  real,    intent(in   )                :: rich_crit, zeta_trans
  integer, intent(in   )                :: n
  real   , intent(  out), dimension(n)  :: psi_t, psi_q
  real   , intent(in)   , dimension(n)  :: zeta, zeta_t, zeta_q, ln_z_zt, ln_z_zq
  logical, intent(in)   , dimension(n)  :: mask
  integer, intent(  out)                :: ier
  
  real, dimension(n)     :: x, x_t, x_q                              
  logical, dimension(n)  :: stable, unstable, &
                                  weakly_stable, strongly_stable
  real                   :: b_stab, lambda

  ier = 0
  
  b_stab     = 1.0/rich_crit

stable   = mask .and. zeta >= 0.0
unstable = mask .and. zeta <  0.0

where(unstable) 

  x     = sqrt(1 - 16.0*zeta)
  x_t   = sqrt(1 - 16.0*zeta_t)
  x_q   = sqrt(1 - 16.0*zeta_q)
  
  psi_t = ln_z_zt - 2.0*log( (1.0 + x)/(1.0 + x_t) )
  psi_q = ln_z_zq - 2.0*log( (1.0 + x)/(1.0 + x_q) )

end where

if( stable_option == 1) then

  where (stable) 
  
    psi_t = ln_z_zt + (5.0 - b_stab)*log((1.0 + zeta)/(1.0 + zeta_t)) &
       + b_stab*(zeta - zeta_t) 
    psi_q = ln_z_zq + (5.0 - b_stab)*log((1.0 + zeta)/(1.0 + zeta_q)) &
       + b_stab*(zeta - zeta_q) 
       
  end where
  
else if (stable_option == 2) then

   lambda = 1.0 + (5.0 - b_stab)*zeta_trans

  weakly_stable   = stable .and. zeta <= zeta_trans
  strongly_stable = stable .and. zeta >  zeta_trans

  where (weakly_stable)
    psi_t = ln_z_zt + 5.0*(zeta - zeta_t) 
    psi_q = ln_z_zq + 5.0*(zeta - zeta_q) 
  end where
  
  where(strongly_stable)
    x = (lambda - 1.0)*log(zeta/zeta_trans) + b_stab*(zeta - zeta_trans)
  endwhere
  
  where (strongly_stable .and. zeta_t <= zeta_trans)
    psi_t = ln_z_zt + x + 5.0*(zeta_trans - zeta_t)
  end where
  where (strongly_stable .and. zeta_t > zeta_trans)
    psi_t = lambda*ln_z_zt + b_stab*(zeta  - zeta_t)
  endwhere
  
  where (strongly_stable .and. zeta_q <= zeta_trans)
    psi_q = ln_z_zq + x + 5.0*(zeta_trans - zeta_q)
  end where
  where (strongly_stable .and. zeta_q > zeta_trans)
    psi_q = lambda*ln_z_zq + b_stab*(zeta  - zeta_q)
  endwhere
  
end if

end subroutine monin_obukhov_integral_tq

end module monin_obukhov_mod
