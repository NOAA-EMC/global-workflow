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

module qs_tables_mod

use constants_mod,         only: rdgas, rvgas, cp_air, cp_vapor, hlv

implicit none
logical:: qs_table_is_initialized = .false.
real, allocatable, dimension(:,:) :: table_w(:), des_w(:)
public :: qs_wat0, qs_wat, qs_wat_init

  real, parameter:: e0 = 610.71  ! saturation vapor pressure at T0
  real, parameter:: tice = 273.16
  real, parameter:: c_liq = 4190.       ! heat capacity of water at 0C
  real, parameter:: cp_vap = cp_vapor   ! 1846.
! For consistency, cv_vap derived FMS constants:
  real, parameter:: cv_vap = cp_vap - rvgas  ! 1384.5
  real, parameter:: cv_air = cp_air - rdgas
#ifdef SIM_NGGPS
  real, parameter:: dc_vap = 0.
#else
  real, parameter:: dc_vap = cp_vap - c_liq     ! = -2344.    isobaric heating/cooling
#endif
  real, parameter:: Lv0 =  hlv - dc_vap*tice
!            L = hlv + (Cp_vapor-C_liq)*(T-T_ice)

contains

 real function qs_wat0(ta, den)
! Pure water phase; universal dry/moist formular using air density
! Input "den" can be either dry or moist air density
  real, intent(in):: ta, den
! local:
  real es, ap1, dem
  real, parameter:: tmin = tice - 160.
  integer it

! if (.not. qs_table_is_initialized) call qs_wat_init
       ap1 = 10.*dim(ta, tmin) + 1.    ! lower bound enforced 
       ap1 = min(2621., ap1)           ! upper bound enforced
        it = ap1
        es = table_w(it) + (ap1-it)*des_w(it)
       dem = rvgas*ta*den
   qs_wat0 = es / dem

 end function qs_wat0

 real function qs_wat(ta, den, dqdt)
! Pure water phase; universal dry/moist formular using air density
! Input "den" can be either dry or moist air density
! Full-form:
!  qsat = e0*rdgas/(rvgas*p_in)*exp((dc_vap*log(T_in/tice)+Lv0*(T_in-tice)/(T_in*tice))/rvgas)
! simple-form:
!  qsat = e0*rdgas/(rvgas*p_in)*exp( hlv/rvgas*(T_in-tice)/(T_in*tice) )
!
  real, intent(in):: ta, den
  real, intent(out):: dqdt
! local:
  real es, ap1, dem
  real, parameter:: tmin = tice - 160.
  integer it

! if (.not. qs_table_is_initialized) call qs_wat_init
       ap1 = 10.*dim(ta, tmin) + 1.    ! lower bound enforced 
       ap1 = min(2621., ap1)           ! upper bound enforced
        it = ap1
        es = table_w(it) + (ap1-it)*des_w(it)
       dem = rvgas*ta*den
    qs_wat = es / dem
        it = ap1 - 0.5
! Finite diff, del_T = 0.1:
      dqdt = 10.*(des_w(it) + (ap1-it)*(des_w(it+1)-des_w(it))) / dem

 end function qs_wat

 subroutine qs_wat_init
  integer, parameter:: length=2621
  integer i

  if( .not. qs_table_is_initialized ) then
!                            generate es table (dt = 0.1 deg. c)
       allocate ( table_w(length) )
       allocate (   des_w(length) )

       call qs_table_w(length )

       do i=1,length-1
          des_w(i) = max(0., table_w(i+1) - table_w(i))
       enddo
       des_w(length) = des_w(length-1)

       qs_table_is_initialized = .true.
  endif

 end subroutine qs_wat_init

 subroutine qs_table_w(n)
      integer, intent(in):: n
      real, parameter:: del_t=0.1
      real:: tmin, tem, f0
      integer i

! constants
     tmin = tice - 160.

     do i=1,n
        tem = tmin + del_t*real(i-1)
!  compute es over water
! Lv0 =  hlv - dc_vap*tice
        table_w(i) = e0*exp((dc_vap*log(tem/tice)+Lv0*(tem-tice)/(tem*tice))/rvgas)
     enddo

 end subroutine qs_table_w

end module qs_tables_mod
