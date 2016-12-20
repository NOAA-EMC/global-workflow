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
module fv_nggps_diags_mod

use mpp_mod, only: mpp_pe, mpp_root_pe
 use constants_mod,      only: grav, rdgas
 use fms_io_mod,         only: set_domain, nullify_domain
 use time_manager_mod,   only: time_type
 use diag_manager_mod,   only: register_diag_field, send_data
 use tracer_manager_mod, only: get_tracer_names, get_number_tracers, get_tracer_index
 use field_manager_mod,  only: MODEL_ATMOS
 use fv_diagnostics_mod, only: range_check
 use fv_arrays_mod,      only: fv_atmos_type

 implicit none
 private

 real, parameter:: missing_value = -1.e10
 logical master
 integer :: id_ua, id_va, id_pt, id_delp, id_pfhy, id_pfnh, id_w, id_delz 
 integer, allocatable :: id_tracer(:)

 logical :: module_is_initialized=.false.
 integer :: sphum, liq_wat, ice_wat       ! GFDL physics
 integer :: rainwat, snowwat, graupel
 real :: vrange(2), wrange(2), trange(2)

! tracers
 character(len=128)   :: tname
 character(len=256)   :: tlongname, tunits


 public :: fv_nggps_diag_init, fv_nggps_diag

contains

 subroutine fv_nggps_diag_init(Atm, axes, Time)
    type(fv_atmos_type), intent(inout), target :: Atm(:)
    integer, intent(in) :: axes(4)
    type(time_type), intent(in) :: Time

    character(len=64) :: field
    integer :: n, ncnst, i

    vrange = (/ -330.,  330. /)  ! winds
    wrange = (/ -100.,  100. /)  ! vertical wind
    trange = (/  100.,  350. /)  ! temperature

    n = 1
    ncnst = Atm(1)%ncnst

    call set_domain(Atm(1)%domain)  ! Set domain so that diag_manager can access tile information

    sphum   = get_tracer_index (MODEL_ATMOS, 'sphum')
    liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
    ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')

    rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
    snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
    graupel = get_tracer_index (MODEL_ATMOS, 'graupel')

!--------------------------------------------------------------
! Register main prognostic fields: ps, (u,v), t, omega (dp/dt)
!--------------------------------------------------------------
    allocate(id_tracer(ncnst))
    id_tracer(:) = 0

    field= 'gfs_dyn'

!-------------------
! A grid winds (lat-lon)
!-------------------
       id_ua = register_diag_field ( trim(field), 'ucomp', axes(1:3), Time,        &
            'zonal wind', 'm/sec', missing_value=missing_value, range=vrange )

       id_va = register_diag_field ( trim(field), 'vcomp', axes(1:3), Time,        &
            'meridional wind', 'm/sec', missing_value=missing_value, range=vrange)

       if( Atm(n)%flagstruct%hydrostatic ) then 
          id_pfhy = register_diag_field ( trim(field), 'pfhy', axes(1:3), Time,        &
               'hydrostatic pressure', 'pa', missing_value=missing_value )
       else
          id_pfnh = register_diag_field ( trim(field), 'pfnh', axes(1:3), Time,        &
               'non-hydrostatic pressure', 'pa', missing_value=missing_value )
          id_w = register_diag_field ( trim(field), 'w', axes(1:3), Time,        &
               'vertical wind', 'm/sec', missing_value=missing_value, range=wrange )
          id_delz = register_diag_field ( trim(field), 'delz', axes(1:3), Time,        &
               'height thickness', 'm', missing_value=missing_value )
       endif

       id_pt   = register_diag_field ( trim(field), 'temp', axes(1:3), Time,       &
            'temperature', 'K', missing_value=missing_value, range=trange )

       id_delp = register_diag_field ( trim(field), 'delp', axes(1:3), Time,        &
            'pressure thickness', 'pa', missing_value=missing_value )

!--------------------
! Tracer diagnostics:
!--------------------
       do i=1, ncnst
         call get_tracer_names ( MODEL_ATMOS, i, tname, tlongname, tunits )
         id_tracer(i) = register_diag_field ( field, trim(tname),  &
                                   axes(1:3), Time, trim(tlongname), &
                                   trim(tunits), missing_value=missing_value)
       enddo

 end subroutine fv_nggps_diag_init


 subroutine fv_nggps_diag(Atm, zvir, Time)

    type(fv_atmos_type), intent(inout) :: Atm(:)
    real,                intent(in):: zvir
    type(time_type),     intent(in) :: Time

    integer :: isc, iec, jsc, jec, npz
    integer :: i, j, k, n, ngc, nq, itrac
    logical :: bad_range
    logical :: used
    real    :: ptop
    real, allocatable :: wk(:,:,:)

    n = 1
    isc = Atm(n)%bd%isc; iec = Atm(n)%bd%iec
    jsc = Atm(n)%bd%jsc; jec = Atm(n)%bd%jec
    ngc = Atm(n)%ng
    npz = Atm(n)%npz
    ptop = Atm(n)%ak(1)
    nq = size (Atm(n)%q,4)
    allocate ( wk(isc:iec,jsc:jec,npz) )

    if ( Atm(n)%flagstruct%range_warn ) then
         call range_check('DELP', Atm(n)%delp, isc, iec, jsc, jec, ngc, npz, Atm(n)%gridstruct%agrid,    &
                           0.01*ptop, 200.E2, bad_range)
         call range_check('UA', Atm(n)%ua, isc, iec, jsc, jec, ngc, npz, Atm(n)%gridstruct%agrid,   &
                           -220., 250., bad_range)
         call range_check('VA', Atm(n)%va, isc, iec, jsc, jec, ngc, npz, Atm(n)%gridstruct%agrid,   &
                           -220., 220., bad_range)
         call range_check('TA', Atm(n)%pt, isc, iec, jsc, jec, ngc, npz, Atm(n)%gridstruct%agrid,   &
                           130., 350., bad_range) !DCMIP ICs have very low temperatures
    endif

    !--- A-GRID WINDS
    if(id_ua > 0) used=send_data(id_ua, Atm(n)%ua(isc:iec,jsc:jec,:), Time)
    if(id_va > 0) used=send_data(id_va, Atm(n)%va(isc:iec,jsc:jec,:), Time)

    !--- W (non-hydrostatic)
    if ( .not.Atm(n)%flagstruct%hydrostatic .and. id_w>0  ) then
       used=send_data(id_w, Atm(n)%w(isc:iec,jsc:jec,:), Time)
    endif

    !--- TEMPERATURE
    if(id_pt   > 0) used=send_data(id_pt  , Atm(n)%pt  (isc:iec,jsc:jec,:), Time)

    !--- TRACERS
    do itrac=1, Atm(n)%ncnst
      call get_tracer_names (MODEL_ATMOS, itrac, tname)
      if (id_tracer(itrac) > 0 .and. itrac.gt.nq) then
        used = send_data (id_tracer(itrac), Atm(n)%qdiag(isc:iec,jsc:jec,:,itrac), Time )
      else
        used = send_data (id_tracer(itrac), Atm(n)%q(isc:iec,jsc:jec,:,itrac), Time )
      endif
    enddo

    !--- DELZ (non-hydrostatic)
    if((.not. Atm(n)%flagstruct%hydrostatic) .and. id_delz > 0) then
       do k=1,npz
         do j=jsc,jec
           do i=isc,iec
             wk(i,j,k) = -Atm(n)%delz(i,j,k)
           enddo
         enddo
       enddo
       used=send_data(id_delz, wk, Time)
    endif

    !--- PRESSURE (hydrostatic)
    if( Atm(n)%flagstruct%hydrostatic .and. id_pfhy > 0 ) then
       do k=1,npz
         do j=jsc,jec
           do i=isc,iec         
             wk(i,j,k) = 0.5 *(Atm(n)%pe(i,k,j)+Atm(n)%pe(i,k+1,j))
           enddo
         enddo
       enddo
       used=send_data(id_pfhy, wk, Time)
    endif

#ifdef GFS_PHYS
    !--- DELP
    if(id_delp > 0 .or. ((.not. Atm(n)%flagstruct%hydrostatic) .and. id_pfnh > 0)) then
       do k=1,npz
         do j=jsc,jec
           do i=isc,iec         
             wk(i,j,k) = Atm(n)%delp(i,j,k)*(1.-Atm(n)%q(i,j,k,liq_wat))
           enddo
         enddo
       enddo
       if (id_delp > 0) used=send_data(id_delp, wk, Time)
    endif

    !--- PRESSURE (non-hydrostatic)
    if( (.not. Atm(n)%flagstruct%hydrostatic) .and. id_pfnh > 0) then
       do k=1,npz
         do j=jsc,jec
           do i=isc,iec         
             wk(i,j,k) = -wk(i,j,k)/(Atm(n)%delz(i,j,k)*grav)*rdgas*          &
                         Atm(n)%pt(i,j,k)*(1.+zvir*Atm(n)%q(i,j,k,sphum))     
           enddo
         enddo
       enddo
       used=send_data(id_pfnh, wk, Time)
    endif
#else
    !--- DELP
    if(id_delp > 0) used=send_data(id_delp, Atm(n)%delp(isc:iec,jsc:jec,:), Time)

    !--- PRESSURE (non-hydrostatic)
    if( (.not. Atm(n)%flagstruct%hydrostatic) .and. id_pfnh > 0) then
       do k=1,npz
         do j=jsc,jec
           do i=isc,iec
             wk(i,j,k) = -Atm(n)%delp(i,j,k)/(Atm(n)%delz(i,j,k)*grav)*rdgas*          &
                          Atm(n)%pt(i,j,k)*(1.+zvir*Atm(n)%q(i,j,k,sphum))
           enddo
         enddo
       enddo
       used=send_data(id_pfnh, wk, Time)
    endif
#endif

    deallocate ( wk )

    call nullify_domain()

 end subroutine fv_nggps_diag

end module fv_nggps_diags_mod
