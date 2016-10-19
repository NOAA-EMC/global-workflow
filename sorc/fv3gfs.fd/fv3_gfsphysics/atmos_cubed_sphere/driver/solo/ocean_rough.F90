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

module ocean_rough_mod

!-----------------------------------------------------------------------

use       fms_mod, only: error_mesg, FATAL, file_exist, open_namelist_file,  &
                         check_nml_error, mpp_pe, mpp_root_pe, close_file, &
                         write_version_number, stdlog

implicit none
private

public :: compute_ocean_roughness, fixed_ocean_roughness

!-----------------------------------------------------------------------
!----- namelist -----

  real    :: roughness_init = 0.00044   ! not used in this version
  real    :: roughness_min  = 1.e-6
  real    :: charnock       = 0.032
  
  real    :: roughness_mom   = 5.8e-5
  real    :: roughness_heat  = 5.8e-5   ! was 4.00e-4
  real    :: roughness_moist = 5.8e-5
! real, parameter :: zcoh1 = 1.4e-5
! real, parameter :: zcoq1 = 1.3e-4
  real            :: zcoh1 = 0.0 !miz
  real            :: zcoq1 = 0.0 !miz
  logical :: do_highwind     = .false.
  logical :: do_cap40        = .false.
  real    :: v10m  = 32.5 !jhc
  real    :: v10n  = 17.5 !jhc
  logical :: do_init = .true.

  character(len=32) :: rough_scheme = 'fixed'   ! possible values:
                                                !   'fixed'
                                                !   'charnock'
                                                !   'beljaars'
  logical:: read_namelist = .true.

namelist /ocean_rough_nml/ roughness_init, roughness_heat,  &
                           roughness_mom,  roughness_moist, &
                           roughness_min,                   &
                           charnock,                        &
                           rough_scheme, do_highwind,       &!miz
                           v10m, v10n, do_cap40, do_init, zcoh1, zcoq1   !sjl

!-----------------------------------------------------------------------
! ---- constants ----

! ..... high wind speed - rough sea
  real, parameter :: zcom1 = 1.8e-2    ! Charnock's constant
! ..... low wind speed - smooth sea
  real, parameter :: gnu   = 1.5e-5
  real, parameter :: zcom2 = 0.11
  real, parameter :: zcoh2 = 0.40
  real, parameter :: zcoq2 = 0.62
  real, parameter :: grav = 9.80
  real, parameter :: us10_adj = 0.9     ! reduction factor; added by SJL

contains

!#######################################################################

 subroutine compute_ocean_roughness (u_star, speed,     &
                           rough_mom, rough_heat, rough_moist, master )

 real,    intent(in)    :: speed(:,:)
 real,    intent(inout) :: u_star(:,:)
 real,    intent(out)   :: rough_mom(:,:), rough_heat(:,:), rough_moist(:,:)
 logical:: master
!-----------------------------------------------------------------------
!  computes ocean roughness for momentum using wind stress
!  and sets roughness for heat/moisture using namelist value
!-----------------------------------------------------------------------

   real, dimension(size(speed,1),size(speed,2)) :: ustar2, xx1, xx2, w10 !miz
   real:: zt1
   integer :: i, j
   integer :: unit, ierr, io

!   ----- read and write namelist -----
    if ( read_namelist .and. file_exist('input.nml')) then
          unit = open_namelist_file ('input.nml')
          if(master) write(*,*)'read input, unit', unit
           ierr=1; do while (ierr /= 0)
           read  (unit, nml=ocean_rough_nml, iostat=io, end=10)
           ierr = check_nml_error(io,'ocean_rough_nml')
           if(master) write(*,*)'ierr =',ierr
        enddo
 10     call close_file (unit)
        if(master) write(*,*)'do_init=',do_init
        if(master) write(*,*)'rough_scheme=',rough_scheme
        read_namelist = .false.
    endif


   if (do_init) then

       call ocean_rough_init(us10_adj*speed, rough_mom, rough_heat, rough_moist)
! SJL: compute u_star using Eq (2), Moon et al.
       u_star(:,:) = 0.4*speed(:,:)*us10_adj/log(10./rough_mom(:,:))

   else
   if (trim(rough_scheme) == 'fixed') then
!  --- set roughness for momentum and heat/moisture ---

      call fixed_ocean_roughness (speed, rough_mom, rough_heat, rough_moist )


!  --- compute roughness for momentum, heat, moisture ---

   else if (trim(rough_scheme) == 'beljaars' .or. &
            trim(rough_scheme) == 'charnock') then

          ustar2(:,:) = max(gnu*gnu, u_star(:,:)*u_star(:,:))          
          xx1(:,:) = gnu / sqrt(ustar2(:,:))
          xx2(:,:) = ustar2(:,:) / grav

      if (trim(rough_scheme) == 'charnock') then
              rough_mom  (:,:) = charnock * xx2(:,:)
              rough_mom  (:,:) = max( rough_mom(:,:), roughness_min )
              rough_heat (:,:) = rough_mom  (:,:)
              rough_moist(:,:) = rough_mom  (:,:)
      else if (trim(rough_scheme) == 'beljaars') then
          if (do_highwind) then       !  Moon et al. formular
! --- SJL ---- High Wind correction following Moon et al 2007 ------
              do j=1,size(speed,2)
                 do i=1,size(speed,1)
                      w10(i,j) = 2.458 + u_star(i,j)*(20.255-0.56*u_star(i,j))  ! Eq(7) Moon et al.
                      if ( w10(i,j) > 12.5 ) then
                           rough_mom(i,j) = 0.001*(0.085*w10(i,j) - 0.58)    ! Eq(8b) Moon et al.
! SJL mods: cap the growth of z0 with w10 up to 40 m/s
! z0 (w10=40) = 2.82E-3
                           if(do_cap40) rough_mom(i,j) = min( rough_mom(i,j), 2.82E-3)
                      else    
                           rough_mom(i,j) = 0.0185/grav*u_star(i,j)**2  ! (8a) Moon et al.
                      endif
! Ramp up the coefficient:
                      zt1 = min( 1., (w10(i,j)-v10n)/(v10m-v10n) )
                      rough_heat (i,j) = zcoh1*zt1*xx2(i,j) + zcoh2 * xx1(i,j)
                      rough_moist(i,j) = zcoq1*zt1*xx2(i,j) + zcoq2 * xx1(i,j)
!                 --- lower limit on roughness? ---
                      rough_mom  (i,j) = max( rough_mom  (i,j), roughness_min )
                      rough_heat (i,j) = max( rough_heat (i,j), roughness_min )
                      rough_moist(i,j) = max( rough_moist(i,j), roughness_min )
                 enddo
              enddo
! SJL -----------------------------------------------------------------------------------
          else
              rough_mom  (:,:) = zcom1 * xx2(:,:) + zcom2 * xx1(:,:)
              rough_heat (:,:) = zcoh1 * xx2(:,:) + zcoh2 * xx1(:,:)
              rough_moist(:,:) = zcoq1 * xx2(:,:) + zcoq2 * xx1(:,:)
!             --- lower limit on roughness? ---
              rough_mom  (:,:) = max( rough_mom  (:,:), roughness_min )
              rough_heat (:,:) = max( rough_heat (:,:), roughness_min )
              rough_moist(:,:) = max( rough_moist(:,:), roughness_min )
          endif
      endif
   endif
   endif
!-----------------------------------------------------------------------

 end subroutine compute_ocean_roughness

!#######################################################################

 subroutine fixed_ocean_roughness ( speed, rough_mom, rough_heat, rough_moist )

 real,    intent(in) :: speed(:,:)
 real,    intent(out) :: rough_mom(:,:), rough_heat(:,:), rough_moist(:,:)

       rough_mom   = roughness_mom
       rough_heat  = roughness_heat
       rough_moist = roughness_moist
 end subroutine fixed_ocean_roughness

!#######################################################################

 subroutine ocean_rough_init(speed, z0, zt, zq)

   real, intent(in)  :: speed(:,:)  ! 10-m wind speed
   real, intent(out) :: z0(:,:), zt(:,:), zq(:,:)
   integer i,j
   integer :: unit, ierr, io

   do j=1, size(speed,2)
      do i=1, size(speed,1)
         if ( speed(i,j) > 12.5 ) then
            z0(i,j) = 0.001*(0.085*speed(i,j) - 0.58)
         else
            z0(i,j) = 0.0185/grav*(0.001*speed(i,j)**2+0.028*speed(i,j))**2
         endif
         z0(i,j) = max(z0(i,j), roughness_min)   ! prevents blowup if cold start (V=0)
         zt(i,j) = z0(i,j)
         zq(i,j) = z0(i,j)
      enddo
   enddo

   do_init = .false.

 end subroutine ocean_rough_init

end module ocean_rough_mod

