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
module atmosphere_mod
#include <fms_platform.h>

!-----------------------------------------------------------------------
!
! Interface for Cubed_Sphere fv dynamical core
!
!-----------------------------------------------------------------------

!-----------------
! FMS modules:
!-----------------
use block_control_mod,      only: block_control_type
use constants_mod,          only: cp_air, rdgas, grav, rvgas, kappa, pstd_mks, R_GRID
use time_manager_mod,       only: time_type, get_time, set_time, operator(+) 
use fms_mod,                only: file_exist, open_namelist_file,    &
                                  close_file, error_mesg, FATAL,     &
                                  check_nml_error, stdlog,           &
                                  write_version_number,              &
                                  set_domain,   &
                                  mpp_clock_id, mpp_clock_begin,     &
                                  mpp_clock_end, CLOCK_SUBCOMPONENT, &
                                  clock_flag_default, nullify_domain
use mpp_mod,                only: mpp_error, stdout, FATAL, NOTE, &
                                  input_nml_file, mpp_root_pe,    &
                                  mpp_npes, mpp_pe, mpp_chksum,   &
                                  mpp_get_current_pelist,         &
                                  mpp_set_current_pelist
use mpp_domains_mod,        only: domain2d
use xgrid_mod,              only: grid_box_type
use field_manager_mod,      only: MODEL_ATMOS
use tracer_manager_mod,     only: get_tracer_index, get_number_tracers, &
                                  NO_TRACER
use gfs_physics_driver_mod, only: state_fields_in, state_fields_out, kind_phys

!-----------------
! FV core modules:
!-----------------
use fv_arrays_mod,      only: fv_atmos_type
use fv_control_mod,     only: fv_init, fv_end, ngrids
use fv_eta_mod,         only: get_eta_level
use fv_io_mod,          only: fv_io_register_nudge_restart
use fv_dynamics_mod,    only: fv_dynamics
use fv_nesting_mod,     only: twoway_nesting
use fv_diagnostics_mod, only: fv_diag_init, fv_diag, fv_time, prt_maxmin
use fv_restart_mod,     only: fv_restart, fv_write_restart
use fv_timing_mod,      only: timing_on, timing_off
use fv_mp_mod,          only: switch_current_Atm 
use fv_sg_mod,          only: fv_subgrid_z
use fv_update_phys_mod, only: fv_update_phys
use fv_nwp_nudge_mod,   only: fv_nwp_nudge_init, fv_nwp_nudge_end, do_adiabatic_init

use mpp_domains_mod, only:  mpp_get_data_domain, mpp_get_compute_domain
use boundary_mod, only: update_coarse_grid

implicit none
private

!--- driver routines 
public :: atmosphere_init, atmosphere_end, atmosphere_restart, &
          atmosphere_dynamics, atmosphere_state_update

!--- utility routines
public :: atmosphere_resolution, atmosphere_boundary, &
          atmosphere_grid_center, atmosphere_domain, &
          atmosphere_control_data, atmosphere_pref, &
          get_atmosphere_axes, get_bottom_mass, &
          get_bottom_wind, get_stock_pe, &
          set_atmosphere_pelist, get_atmosphere_grid

!--- physics/radiation data exchange routines
public :: atmos_phys_driver_statein

!-----------------------------------------------------------------------

character(len=128) :: version = '$Id$'
character(len=128) :: tagname = '$Name$'
character(len=7)   :: mod_name = 'atmos'

!---- private data ----
  type (time_type) :: Time_step_atmos
  public Atm

  !These are convenience variables for local use only, and are set to values in Atm%
  real    :: dt_atmos
  real    :: zvir
  integer :: npx, npy, npz, ncnst, pnats
  integer :: isc, iec, jsc, jec
  integer :: isd, ied, jsd, jed
  integer :: nq                       ! transported tracers
  integer :: sec, seconds, days
  integer :: id_dynam, id_fv_diag, id_dryconv
  logical :: cold_start = .false.       ! read in initial condition

  integer, dimension(:), allocatable :: id_tracerdt_dyn
  integer :: num_tracers = 0

  integer :: mytile = 1
  integer :: p_split = 1
  integer, allocatable :: pelist(:)
  logical, allocatable :: grids_on_this_pe(:)
  type(fv_atmos_type), allocatable, target :: Atm(:)

  integer :: id_udt_dyn, id_vdt_dyn

  real, parameter:: w0_big = 60.  ! to prevent negative w-tracer diffusion

!---dynamics tendencies for use in fv_subgrid_z and during fv_update_phys
  real, allocatable, dimension(:,:,:)   :: u_dt, v_dt, t_dt
  real, allocatable :: pref(:,:), dum1d(:)

  logical :: first_diag = .true.

contains



 subroutine atmosphere_init (Time_init, Time, Time_step, Grid_box, dx, dy, area)
   type (time_type),    intent(in)    :: Time_init, Time, Time_step
   type(grid_box_type), intent(inout) :: Grid_box
   real(kind=kind_phys), pointer, dimension(:,:), intent(inout) :: dx, dy, area

!--- local variables ---
   integer :: i, n
   integer :: itrac
   logical :: do_atmos_nudge
   character(len=32) :: tracer_name, tracer_units
   real :: ps1, ps2

                    call timing_on('ATMOS_INIT')
   allocate(pelist(mpp_npes()))
   call mpp_get_current_pelist(pelist)

   call get_number_tracers(MODEL_ATMOS, num_prog= num_tracers)

   zvir = rvgas/rdgas - 1.

!---- compute physics/atmos time step in seconds ----

   Time_step_atmos = Time_step
   call get_time (Time_step_atmos, sec)
   dt_atmos = real(sec)

!----- initialize FV dynamical core -----
   !NOTE do we still need the second file_exist call?
   cold_start = (.not.file_exist('INPUT/fv_core.res.nc') .and. .not.file_exist('INPUT/fv_core.res.tile1.nc'))

   call fv_init( Atm, dt_atmos, grids_on_this_pe, p_split )  ! allocates Atm components

   do n=1,ngrids
      if (grids_on_this_pe(n)) mytile = n
   enddo

!----- write version and namelist to log file -----
   call write_version_number ( version, tagname )

!-----------------------------------

   npx   = Atm(mytile)%npx
   npy   = Atm(mytile)%npy
   npz   = Atm(mytile)%npz
   ncnst = Atm(mytile)%ncnst
   pnats = Atm(mytile)%flagstruct%pnats

   isc = Atm(mytile)%bd%isc
   iec = Atm(mytile)%bd%iec
   jsc = Atm(mytile)%bd%jsc
   jec = Atm(mytile)%bd%jec

   isd = isc - Atm(mytile)%bd%ng
   ied = iec + Atm(mytile)%bd%ng
   jsd = jsc - Atm(mytile)%bd%ng
   jed = jec + Atm(mytile)%bd%ng

   nq = ncnst-pnats

   ! Allocate grid variables to be used to calculate gradient in 2nd order flux exchange
   ! This data is only needed for the COARSEST grid.
   call switch_current_Atm(Atm(mytile))

   allocate(Grid_box%dx    (   isc:iec  , jsc:jec+1))
   allocate(Grid_box%dy    (   isc:iec+1, jsc:jec  ))
   allocate(Grid_box%area  (   isc:iec  , jsc:jec  ))
   allocate(Grid_box%edge_w(              jsc:jec+1))
   allocate(Grid_box%edge_e(              jsc:jec+1))
   allocate(Grid_box%edge_s(   isc:iec+1           ))
   allocate(Grid_box%edge_n(   isc:iec+1           ))
   allocate(Grid_box%en1   (3, isc:iec  , jsc:jec+1))
   allocate(Grid_box%en2   (3, isc:iec+1, jsc:jec  ))
   allocate(Grid_box%vlon  (3, isc:iec  , jsc:jec  ))
   allocate(Grid_box%vlat  (3, isc:iec  , jsc:jec  ))
   Grid_box%dx    (   isc:iec  , jsc:jec+1) = Atm(mytile)%gridstruct%dx    (   isc:iec,   jsc:jec+1)
   Grid_box%dy    (   isc:iec+1, jsc:jec  ) = Atm(mytile)%gridstruct%dy    (   isc:iec+1, jsc:jec  )
   Grid_box%area  (   isc:iec  , jsc:jec  ) = Atm(mytile)%gridstruct%area  (   isc:iec  , jsc:jec  )
   Grid_box%edge_w(              jsc:jec+1) = Atm(mytile)%gridstruct%edge_w(              jsc:jec+1)
   Grid_box%edge_e(              jsc:jec+1) = Atm(mytile)%gridstruct%edge_e(              jsc:jec+1)
   Grid_box%edge_s(   isc:iec+1           ) = Atm(mytile)%gridstruct%edge_s(   isc:iec+1)
   Grid_box%edge_n(   isc:iec+1           ) = Atm(mytile)%gridstruct%edge_n(   isc:iec+1)
   Grid_box%en1   (:, isc:iec  , jsc:jec+1) = Atm(mytile)%gridstruct%en1   (:, isc:iec  , jsc:jec+1)
   Grid_box%en2   (:, isc:iec+1, jsc:jec  ) = Atm(mytile)%gridstruct%en2   (:, isc:iec+1, jsc:jec  )
   do i = 1,3
     Grid_box%vlon  (i, isc:iec  , jsc:jec  ) = Atm(mytile)%gridstruct%vlon  (isc:iec ,  jsc:jec, i )
     Grid_box%vlat  (i, isc:iec  , jsc:jec  ) = Atm(mytile)%gridstruct%vlat  (isc:iec ,  jsc:jec, i )
   enddo
   allocate (dx  (isc:iec  , jsc:jec+1))
   allocate (dy  (isc:iec+1, jsc:jec  ))
   allocate (area(isc:iec  , jsc:jec  ))
   dx(isc:iec,jsc:jec+1) = Atm(mytile)%gridstruct%dx_64(isc:iec,jsc:jec+1)
   dy(isc:iec+1,jsc:jec) = Atm(mytile)%gridstruct%dy_64(isc:iec+1,jsc:jec)
   area(isc:iec,jsc:jec) = Atm(mytile)%gridstruct%area_64(isc:iec,jsc:jec)

!----- allocate and zero out the dynamics (and accumulated) tendencies
   allocate( u_dt(isd:ied,jsd:jed,npz), &
             v_dt(isd:ied,jsd:jed,npz), &
             t_dt(isc:iec,jsc:jec,npz) )
!--- allocate pref
    allocate(pref(npz+1,2), dum1d(npz+1))

   call set_domain ( Atm(mytile)%domain )
   call fv_restart(Atm(mytile)%domain, Atm, dt_atmos, seconds, days, cold_start, Atm(mytile)%gridstruct%grid_type, grids_on_this_pe)

   fv_time = Time

!----- initialize atmos_axes and fv_dynamics diagnostics
       !I've had trouble getting this to work with multiple grids at a time; worth revisiting?
   call fv_diag_init(Atm(mytile:mytile), Atm(mytile)%atmos_axes, Time, npx, npy, npz, Atm(mytile)%flagstruct%p_ref)

!---------- reference profile -----------
    ps1 = 101325.
    ps2 =  81060.
    pref(npz+1,1) = ps1
    pref(npz+1,2) = ps2
    call get_eta_level ( npz, ps1, pref(1,1), dum1d, Atm(mytile)%ak, Atm(mytile)%bk )
    call get_eta_level ( npz, ps2, pref(1,2), dum1d, Atm(mytile)%ak, Atm(mytile)%bk )

! This call needs to be separate from the register nudging restarts after initialization
!!!   call fv_io_register_nudge_restart ( Atm )

!  --- initialize clocks for dynamics, physics_down and physics_up
   id_dynam     = mpp_clock_id ('FV dy-core', flags = clock_flag_default, grain=CLOCK_SUBCOMPONENT )
   id_dryconv   = mpp_clock_id ('FV dry conv',flags = clock_flag_default, grain=CLOCK_SUBCOMPONENT )
   id_fv_diag   = mpp_clock_id ('FV Diag',    flags = clock_flag_default, grain=CLOCK_SUBCOMPONENT )

                    call timing_off('ATMOS_INIT')

   if ( Atm(mytile)%flagstruct%na_init>0 ) then
      call nullify_domain ( )
      if ( .not. Atm(mytile)%flagstruct%hydrostatic ) then
           call prt_maxmin('Before adi: W', Atm(mytile)%w, isc, iec, jsc, jec, Atm(mytile)%ng, npz, 1.)
      endif
      call adiabatic_init(zvir)
      if ( .not. Atm(mytile)%flagstruct%hydrostatic ) then
           call prt_maxmin('After adi: W', Atm(mytile)%w, isc, iec, jsc, jec, Atm(mytile)%ng, npz, 1.)
      endif
   else
      call mpp_error(NOTE,'No adiabatic initialization correction in use')
   endif

#ifdef DEBUG
   call nullify_domain()
   call fv_diag(Atm(mytile:mytile), zvir, Time, -1)
#endif

   n = mytile
   call switch_current_Atm(Atm(n)) 
      
 end subroutine atmosphere_init


 subroutine p_adi(km, ng, ifirst, ilast, jfirst, jlast, ptop,   &
                  delp, pt, ps, pe, peln, pk, pkz, hydrostatic)
! Given (ptop, delp) computes (ps, pk, pe, peln, pkz)
! Input:
   integer,  intent(in):: km, ng
   integer,  intent(in):: ifirst, ilast            ! Longitude strip
   integer,  intent(in):: jfirst, jlast            ! Latitude strip
   logical, intent(in)::  hydrostatic
   real, intent(in):: ptop
   real, intent(in)::   pt(ifirst-ng:ilast+ng,jfirst-ng:jlast+ng, km)
   real, intent(in):: delp(ifirst-ng:ilast+ng,jfirst-ng:jlast+ng, km)
! Output:
   real, intent(out) ::   ps(ifirst-ng:ilast+ng, jfirst-ng:jlast+ng)
   real, intent(out) ::   pk(ifirst:ilast, jfirst:jlast, km+1)
   real, intent(out) ::   pe(ifirst-1:ilast+1,km+1,jfirst-1:jlast+1) ! Ghosted Edge pressure
   real, intent(out) :: peln(ifirst:ilast, km+1, jfirst:jlast)    ! Edge pressure
   real, intent(out) ::  pkz(ifirst:ilast, jfirst:jlast, km)
! Local
   real pek
   integer i, j, k

   pek = ptop ** kappa
!$OMP parallel do default (none) &
!$OMP              shared (ifirst,ilast,jfirst,jlast,km,ptop,pek,pe,pk, &
!$OMP                      ps,delp,peln,hydrostatic,pkz) &
!$OMP             private (j, i, k)
   do j=jfirst,jlast
      do i=ifirst,ilast
         pe(i,1,j) = ptop
         pk(i,j,1) = pek
      enddo

      do k=2,km+1
         do i=ifirst,ilast
            pe(i,k,j) = pe(i,k-1,j) + delp(i,j,k-1)
            peln(i,k,j) = log(pe(i,k,j))
            pk(i,j,k) = exp( kappa*peln(i,k,j) )
         enddo
      enddo

      do i=ifirst,ilast
         ps(i,j) = pe(i,km+1,j)
      enddo

      if ( hydrostatic ) then
         do k=1,km
            do i=ifirst,ilast
               pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
            enddo
         enddo
      endif
   enddo

 end subroutine p_adi


 subroutine atmosphere_dynamics ( Time )
   type(time_type),intent(in) :: Time
   integer :: itrac, n, psc
   integer :: k, w_diff, nt_dyn

!---- Call FV dynamics -----

   call mpp_clock_begin (id_dynam)

   n = mytile
   do psc=1,abs(p_split)
                    call timing_on('fv_dynamics')
!uc/vc only need be same on coarse grid? However BCs do need to be the same
     call fv_dynamics(npx, npy, npz, nq, Atm(n)%ng, dt_atmos/real(abs(p_split)),&
                      Atm(n)%flagstruct%consv_te, Atm(n)%flagstruct%fill,  &
                      Atm(n)%flagstruct%reproduce_sum, kappa, cp_air, zvir,&
                      Atm(n)%ptop, Atm(n)%ks, nq,                          &
                      Atm(n)%flagstruct%n_split, Atm(n)%flagstruct%q_split,&
                      Atm(n)%u, Atm(n)%v, Atm(n)%w, Atm(n)%delz,           &
                      Atm(n)%flagstruct%hydrostatic,                       & 
                      Atm(n)%pt, Atm(n)%delp, Atm(n)%q, Atm(n)%ps,         &
                      Atm(n)%pe, Atm(n)%pk, Atm(n)%peln,                   &
                      Atm(n)%pkz, Atm(n)%phis, Atm(n)%q_con,               &
                      Atm(n)%omga, Atm(n)%ua, Atm(n)%va, Atm(n)%uc,        &
                      Atm(n)%vc, Atm(n)%ak, Atm(n)%bk, Atm(n)%mfx,         &
                      Atm(n)%mfy, Atm(n)%cx, Atm(n)%cy, Atm(n)%ze0,        &
                      Atm(n)%flagstruct%hybrid_z,                          &
                      Atm(n)%gridstruct, Atm(n)%flagstruct,                &
                      Atm(n)%neststruct, Atm(n)%idiag, Atm(n)%bd,          &
                      Atm(n)%parent_grid, Atm(n)%domain)

     call timing_off('fv_dynamics')

    if (ngrids > 1 .and. (psc < p_split .or. p_split < 0)) then
       call timing_on('TWOWAY_UPDATE')
       call twoway_nesting(Atm, ngrids, grids_on_this_pe, zvir, dt_atmos)
       call timing_off('TWOWAY_UPDATE')
    endif

    end do !p_split
    call mpp_clock_end (id_dynam)

!-----------------------------------------------------
!--- COMPUTE SUBGRID Z
!-----------------------------------------------------
!--- zero out tendencies 
    call mpp_clock_begin (id_dryconv)
    u_dt(:,:,:)   = 0 
    v_dt(:,:,:)   = 0 
    t_dt(:,:,:)   = 0 

    w_diff = get_tracer_index (MODEL_ATMOS, 'w_diff' )
    if ( Atm(n)%flagstruct%fv_sg_adj > 0 ) then
      nt_dyn = nq
      if ( w_diff /= NO_TRACER ) then
        nt_dyn = nq - 1
      endif
      call fv_subgrid_z(isd, ied, jsd, jed, isc, iec, jsc, jec, Atm(n)%npz, &
                        nt_dyn, dt_atmos, Atm(n)%flagstruct%fv_sg_adj,      &
                        Atm(n)%flagstruct%nwat, Atm(n)%delp, Atm(n)%pe,     &
                        Atm(n)%peln, Atm(n)%pkz, Atm(n)%pt, Atm(n)%q,       &
                        Atm(n)%ua, Atm(n)%va, Atm(n)%flagstruct%hydrostatic,&
                        Atm(n)%w, Atm(n)%delz, u_dt, v_dt, t_dt, Atm(n)%flagstruct%n_sponge)
    endif

#ifdef USE_Q_DT
    if ( .not. Atm(n)%flagstruct%hydrostatic .and. w_diff /= NO_TRACER ) then
!$OMP parallel do default (none) &
!$OMP              shared (isc, iec, jsc, jec, w_diff, n, Atm, q_dt) &
!$OMP             private (k)
       do k=1, Atm(n)%npz
          Atm(n)%q(isc:iec,jsc:jec,k,w_diff) = Atm(n)%w(isc:iec,jsc:jec,k) + w0_big
          q_dt(:,:,k,w_diff) = 0.
        enddo
    endif
#endif

   call mpp_clock_end (id_dryconv)

 end subroutine atmosphere_dynamics


 subroutine atmosphere_end (Time, Grid_box )!rab, Radiation, Physics)
   type (time_type),      intent(in)    :: Time
   type(grid_box_type),   intent(inout) :: Grid_box
!rab   type (radiation_type), intent(inout) :: Radiation
!rab   type (physics_type),   intent(inout) :: Physics

  ! initialize domains for writing global physics data
   call set_domain ( Atm(mytile)%domain )

   call nullify_domain ( )
   if (first_diag) then
      call timing_on('FV_DIAG')
      call fv_diag(Atm(mytile:mytile), zvir, fv_time, Atm(mytile)%flagstruct%print_freq)
      first_diag = .false.
      call timing_off('FV_DIAG')
   endif

   call fv_end(Atm, grids_on_this_pe)
   deallocate (Atm)

   deallocate( u_dt, v_dt, t_dt, pref, dum1d )

 end subroutine atmosphere_end



  !#######################################################################
  ! <SUBROUTINE NAME="atmosphere_restart">
  ! <DESCRIPTION>
  !  Write out restart files registered through register_restart_file
  ! </DESCRIPTION>
  subroutine atmosphere_restart(timestamp)
    character(len=*),  intent(in) :: timestamp

    call fv_write_restart(Atm, grids_on_this_pe, timestamp)

  end subroutine atmosphere_restart
  ! </SUBROUTINE>


 subroutine atmosphere_resolution (i_size, j_size, global)
   integer, intent(out)          :: i_size, j_size
   logical, intent(in), optional :: global
   logical :: local

   local = .true.
   if( PRESENT(global) ) local = .NOT.global

   if( local ) then
       i_size = iec - isc + 1
       j_size = jec - jsc + 1
   else
       i_size = npx - 1
       j_size = npy - 1
   end if

 end subroutine atmosphere_resolution


 subroutine atmosphere_pref (p_ref)
   real, dimension(:,:), intent(inout) :: p_ref

   p_ref = pref

 end subroutine atmosphere_pref


 subroutine atmosphere_control_data (i1, i2, j1, j2, kt, p_hydro, hydro)
   integer, intent(out)           :: i1, i2, j1, j2, kt
   logical, intent(out), optional :: p_hydro, hydro
   i1 = Atm(mytile)%bd%isc
   i2 = Atm(mytile)%bd%iec
   j1 = Atm(mytile)%bd%jsc
   j2 = Atm(mytile)%bd%jec
   kt = Atm(mytile)%npz

   if (present(p_hydro)) p_hydro = Atm(mytile)%flagstruct%phys_hydrostatic
   if (present(  hydro))   hydro = Atm(mytile)%flagstruct%hydrostatic

 end subroutine atmosphere_control_data


 subroutine atmosphere_grid_center (lon, lat)
!---------------------------------------------------------------
!    returns the longitude and latitude cell centers
!---------------------------------------------------------------
    real(kind=kind_phys), intent(out) :: lon(:,:), lat(:,:)   ! Unit: radian
! Local data:
    integer i,j

    do j=jsc,jec
       do i=isc,iec
          lon(i-isc+1,j-jsc+1) = Atm(mytile)%gridstruct%agrid_64(i,j,1)
          lat(i-isc+1,j-jsc+1) = Atm(mytile)%gridstruct%agrid_64(i,j,2)
       enddo
    end do

 end subroutine atmosphere_grid_center


 subroutine atmosphere_boundary (blon, blat, global)
!---------------------------------------------------------------
!    returns the longitude and latitude grid box edges
!    for either the local PEs grid (default) or the global grid
!---------------------------------------------------------------
    real,    intent(out) :: blon(:,:), blat(:,:)   ! Unit: radian
    logical, intent(in), optional :: global
! Local data:
    integer i,j

    if( PRESENT(global) ) then
      if (global) call mpp_error(FATAL, '==> global grid is no longer available &
                               & in the Cubed Sphere')
    endif

    do j=jsc,jec+1
       do i=isc,iec+1
          blon(i-isc+1,j-jsc+1) = Atm(mytile)%gridstruct%grid(i,j,1)
          blat(i-isc+1,j-jsc+1) = Atm(mytile)%gridstruct%grid(i,j,2)
       enddo
    end do

 end subroutine atmosphere_boundary


 subroutine set_atmosphere_pelist ()
   call mpp_set_current_pelist(Atm(mytile)%pelist, no_sync=.TRUE.)
 end subroutine set_atmosphere_pelist


 subroutine atmosphere_domain ( fv_domain )
   type(domain2d), intent(out) :: fv_domain
!  returns the domain2d variable associated with the coupling grid
!  note: coupling is done using the mass/temperature grid with no halos

   fv_domain = Atm(mytile)%domain_for_coupler

 end subroutine atmosphere_domain


 subroutine get_atmosphere_grid (dxmax, dxmin)
   real(kind=R_GRID), intent(out) :: dxmax, dxmin

   dxmax = Atm(mytile)%gridstruct%da_max
   dxmin = Atm(mytile)%gridstruct%da_min

 end subroutine get_atmosphere_grid


 subroutine get_atmosphere_axes ( axes )
   integer, intent(out) :: axes (:)

!----- returns the axis indices for the atmospheric (mass) grid -----
   if ( size(axes(:)) < 0 .or. size(axes(:)) > 4 ) call error_mesg (    &
                               'get_atmosphere_axes in atmosphere_mod', &
                               'size of argument is incorrect', FATAL   )

   axes (1:size(axes(:))) = Atm(mytile)%atmos_axes (1:size(axes(:)))
 
 end subroutine get_atmosphere_axes



 subroutine get_bottom_mass ( t_bot, tr_bot, p_bot, z_bot, p_surf, slp )
!--------------------------------------------------------------
! returns temp, sphum, pres, height at the lowest model level
! and surface pressure
!--------------------------------------------------------------
   real, intent(out), dimension(isc:iec,jsc:jec):: t_bot, p_bot, z_bot, p_surf
   real, intent(out), optional, dimension(isc:iec,jsc:jec):: slp
   real, intent(out), dimension(isc:iec,jsc:jec,nq):: tr_bot
   integer :: i, j, m, k, kr
   real    :: rrg, sigtop, sigbot
   real, dimension(isc:iec,jsc:jec) :: tref
   real, parameter :: tlaps = 6.5e-3

   rrg  = rdgas / grav

   do j=jsc,jec
      do i=isc,iec
         p_surf(i,j) = Atm(mytile)%ps(i,j)
         t_bot(i,j) = Atm(mytile)%pt(i,j,npz)
         p_bot(i,j) = Atm(mytile)%delp(i,j,npz)/(Atm(mytile)%peln(i,npz+1,j)-Atm(mytile)%peln(i,npz,j))
         z_bot(i,j) = rrg*t_bot(i,j)*(1.+zvir*Atm(mytile)%q(i,j,npz,1)) *  &
                      (1. - Atm(mytile)%pe(i,npz,j)/p_bot(i,j))
      enddo
   enddo

   if ( present(slp) ) then
     ! determine 0.8 sigma reference level
     sigtop = Atm(mytile)%ak(1)/pstd_mks+Atm(mytile)%bk(1)
     do k = 1, npz 
        sigbot = Atm(mytile)%ak(k+1)/pstd_mks+Atm(mytile)%bk(k+1)
        if (sigbot+sigtop > 1.6) then
           kr = k  
           exit    
        endif   
        sigtop = sigbot
     enddo
     do j=jsc,jec
        do i=isc,iec
           ! sea level pressure
           tref(i,j) = Atm(mytile)%pt(i,j,kr) * (Atm(mytile)%delp(i,j,kr)/ &
                            ((Atm(mytile)%peln(i,kr+1,j)-Atm(mytile)%peln(i,kr,j))*Atm(mytile)%ps(i,j)))**(-rrg*tlaps)
           slp(i,j) = Atm(mytile)%ps(i,j)*(1.+tlaps*Atm(mytile)%phis(i,j)/(tref(i,j)*grav))**(1./(rrg*tlaps))
        enddo
     enddo
   endif

! Copy tracers
   do m=1,nq
      do j=jsc,jec
         do i=isc,iec
            tr_bot(i,j,m) = Atm(mytile)%q(i,j,npz,m)
         enddo
      enddo
   enddo

 end subroutine get_bottom_mass


 subroutine get_bottom_wind ( u_bot, v_bot )
!-----------------------------------------------------------
! returns u and v on the mass grid at the lowest model level
!-----------------------------------------------------------
   real, intent(out), dimension(isc:iec,jsc:jec):: u_bot, v_bot
   integer i, j

   do j=jsc,jec
      do i=isc,iec
         u_bot(i,j) = Atm(mytile)%u_srf(i,j)
         v_bot(i,j) = Atm(mytile)%v_srf(i,j)
      enddo
   enddo

 end subroutine get_bottom_wind



 subroutine get_stock_pe(index, value)
   integer, intent(in) :: index
   real,   intent(out) :: value

#ifdef USE_STOCK
   include 'stock.inc' 
#endif

   real wm(isc:iec,jsc:jec)
   integer i,j,k
   real, pointer :: area(:,:)
 
   area => Atm(mytile)%gridstruct%area

   select case (index)

#ifdef USE_STOCK
   case (ISTOCK_WATER)
#else
   case (1)
#endif
     
!----------------------
! Perform vertical sum:
!----------------------
     wm = 0.
     do j=jsc,jec
        do k=1,npz
           do i=isc,iec
! Warning: the following works only with AM2 physics: water vapor; cloud water, cloud ice.
              wm(i,j) = wm(i,j) + Atm(mytile)%delp(i,j,k) * ( Atm(mytile)%q(i,j,k,1) +    &
                                                         Atm(mytile)%q(i,j,k,2) +    &
                                                         Atm(mytile)%q(i,j,k,3) )
           enddo
        enddo
     enddo

!----------------------
! Horizontal sum:
!----------------------
     value = 0.
     do j=jsc,jec
        do i=isc,iec
           value = value + wm(i,j)*area(i,j)
        enddo
     enddo
     value = value/grav

   case default
     value = 0.0
   end select

 end subroutine get_stock_pe 


 subroutine atmosphere_state_update (Time, Statein, Stateout, Atm_block)
   type(time_type),                      intent(in) :: Time
   type(state_fields_in),  dimension(:), intent(in) :: Statein
   type(state_fields_out), dimension(:), intent(in) :: Stateout
   type(block_control_type),             intent(in) :: Atm_block
   type(time_type) :: Time_prev, Time_next
!--- local variables ---
   integer :: i, j, ix, k, k1, n, w_diff, nt_dyn, iq
   integer :: nb, ibs, ibe, jbs, jbe
   real(kind=kind_phys):: rcp, q0, q1, q2, q3, rdt

   Time_prev = Time
   Time_next = Time + Time_step_atmos
   rdt = 1.d0 / dt_atmos

   n = mytile

   if( nq<3 ) call mpp_error(FATAL, 'GFS phys must have 3 interactive tracers')

   call set_domain ( Atm(mytile)%domain )

   call timing_on('GFS_TENDENCIES')
!--- put u/v tendencies into haloed arrays u_dt and v_dt
!$OMP parallel do default (none) & 
!$OMP              shared (rdt,n,nq,npz,ncnst, mytile, u_dt, v_dt, t_dt, Atm, Statein, Stateout, Atm_block) &
!$OMP             private (nb, ibs, ibe, jbs, jbe, i, j, k, k1, ix, q0, q1, q2, q3)
   do nb = 1,Atm_block%nblks
     ibs = Atm_block%ibs(nb)
     ibe = Atm_block%ibe(nb)
     jbs = Atm_block%jbs(nb)
     jbe = Atm_block%jbe(nb)

!SJL: perform vertical filling to fix the negative humidity if the SAS convection scheme is used
!     This call may be commented out if RAS or other positivity-preserving CPS is used.
     ix = Atm_block%ix(nb)%ix(ibe,jbe)
     call fill_gfs(ix, npz, Statein(nb)%prsi(1:ix,1:npz+1), Stateout(nb)%gq0(1:ix,1:npz,1), 1.e-9_kind_phys)

     do k = 1, npz
      k1 = npz+1-k !reverse the k direction 
      do j=jbs,jbe
       do i=ibs,ibe
         ix = Atm_block%ix(nb)%ix(i,j)
         u_dt(i,j,k1) = u_dt(i,j,k1) + (Stateout(nb)%gu0(ix,k) - Statein(nb)%ugrs(ix,k)) * rdt
         v_dt(i,j,k1) = v_dt(i,j,k1) + (Stateout(nb)%gv0(ix,k) - Statein(nb)%vgrs(ix,k)) * rdt
         t_dt(i,j,k1) = (Stateout(nb)%gt0(ix,k) - Statein(nb)%tgrs(ix,k)) * rdt
! SJL notes:
! ---- DO not touch the code below; dry mass conservation may change due to 64bit <-> 32bit conversion
! GFS total air mass = dry_mass + water_vapor (condensate excluded)
! GFS mixing ratios  = tracer_mass / (air_mass + vapor_mass)
! FV3 total air mass = dry_mass + [water_vapor + condensate ]
! FV3 mixing ratios  = tracer_mass / (dry_mass+vapor_mass+cond_mass)
         q0 = Statein(nb)%prsi(ix,k) - Statein(nb)%prsi(ix,k+1)
         q1 = q0*Stateout(nb)%gq0(ix,k,1)
         q2 = q0*Stateout(nb)%gq0(ix,k,2)
         q3 = q0*Stateout(nb)%gq0(ix,k,3)
! **********************************************************************************************************
! Dry mass: the following way of updating delp is key to mass conservation with hybrid 32-64 bit computation
! **********************************************************************************************************
! The following is for 2 water species. Must include more when more sophisticated cloud microphysics is used.
!        q0 = Atm(n)%delp(i,j,k1)*(1.-(Atm(n)%q(i,j,k1,1)+Atm(n)%q(i,j,k1,2))) + q1 + q2
         q0 = Atm(n)%delp(i,j,k1)*(1.-(Atm(n)%q(i,j,k1,1)+Atm(n)%q(i,j,k1,2))) + (q1+q2)
!--- bad conservation ---
!!!!!    q0 = Atm(n)%delp(i,j,k1)*(1._kind_phys-(Atm(n)%q(i,j,k1,1)+Atm(n)%q(i,j,k1,2))) + q1 + q2
!--- bad conservation ---
         Atm(n)%delp(i,j,k1) = q0
         Atm(n)%q(i,j,k1,1) = q1 / q0
         Atm(n)%q(i,j,k1,2) = q2 / q0
         Atm(n)%q(i,j,k1,3) = q3 / q0
       enddo
      enddo
     enddo

!rab#ifdef GFS_TRACER_TRANSPORT
! The following does nothing...
     if ( nq > 3 ) then
     do iq=4, nq
       do k = 1, npz
         k1 = npz+1-k !reverse the k direction 
         do j=jbs,jbe
           do i=ibs,ibe
             ix = Atm_block%ix(nb)%ix(i,j)
             Atm(n)%q(i,j,k1,iq) = Atm(n)%q(i,j,k1,iq) + (Stateout(nb)%gq0(ix,k,iq)-Statein(nb)%qgrs(ix,k,iq))
!                                * (Statein(nb)%prsi(ix,k)-Statein(nb)%prsi(ix,k+1))/Atm(n)%delp(i,j,k1)
           enddo
         enddo
       enddo
     enddo
     endif
!rab#endif

     !--- diagnostic tracers are being updated in-place
     !--- tracer fields must be returned to the Atm structure
     do iq = nq+1, ncnst
       do k = 1, npz
         k1 = npz+1-k !reverse the k direction 
         do j=jbs,jbe
           do i=ibs,ibe
             ix = Atm_block%ix(nb)%ix(i,j)
             Atm(mytile)%qdiag(i,j,k1,iq) = Stateout(nb)%gq0(ix,k,iq)
           enddo
         enddo
       enddo
     enddo
   enddo  ! nb-loop

   call timing_off('GFS_TENDENCIES')

   w_diff = get_tracer_index (MODEL_ATMOS, 'w_diff' )
   nt_dyn = ncnst-pnats   !nothing more than nq
   if ( w_diff /= NO_TRACER ) then
      nt_dyn = nt_dyn - 1
   endif

!--- adjust w and heat tendency for non-hydrostatic case
#ifdef USE_Q_DT
    if ( .not.Atm(n)%flagstruct%hydrostatic .and. w_diff /= NO_TRACER ) then
      rcp = 1. / cp_air
!$OMP parallel do default (none) &
!$OMP              shared (jsc, jec, isc, iec, n, w_diff, Atm, q_dt, t_dt, rcp, dt_atmos) &
!$OMP             private (i, j, k)
       do k=1, Atm(n)%npz
         do j=jsc, jec
           do i=isc, iec
             Atm(n)%q(i,j,k,w_diff) = q_dt(i,j,k,w_diff) ! w tendency due to phys
! Heating due to loss of KE (vertical diffusion of w)
             t_dt(i,j,k) = t_dt(i,j,k) - q_dt(i,j,k,w_diff)*rcp*&
                                     (Atm(n)%w(i,j,k)+0.5*dt_atmos*q_dt(i,j,k,w_diff))
             Atm(n)%w(i,j,k) = Atm(n)%w(i,j,k) + dt_atmos*Atm(n)%q(i,j,k,w_diff)
           enddo
         enddo
       enddo
    endif
#endif

   call mpp_clock_begin (id_dynam)
       call timing_on('FV_UPDATE_PHYS')
    call fv_update_phys( dt_atmos, isc, iec, jsc, jec, isd, ied, jsd, jed, Atm(n)%ng, nt_dyn, &
                         Atm(n)%u,  Atm(n)%v,   Atm(n)%w,  Atm(n)%delp, Atm(n)%pt,         &
                         Atm(n)%q,  Atm(n)%qdiag,                                          &
                         Atm(n)%ua, Atm(n)%va,  Atm(n)%ps, Atm(n)%pe,   Atm(n)%peln,       &
                         Atm(n)%pk, Atm(n)%pkz, Atm(n)%ak, Atm(n)%bk,   Atm(n)%phis,       &
                         Atm(n)%u_srf, Atm(n)%v_srf, Atm(n)%ts, Atm(n)%delz,               &
                         Atm(n)%flagstruct%hydrostatic, u_dt, v_dt, t_dt,             &
                         .true., Time_next, Atm(n)%flagstruct%nudge, Atm(n)%gridstruct,    &
                         Atm(n)%gridstruct%agrid(:,:,1), Atm(n)%gridstruct%agrid(:,:,2),   &
                         Atm(n)%npx, Atm(n)%npy, Atm(n)%npz, Atm(n)%flagstruct,            &
                         Atm(n)%neststruct, Atm(n)%bd, Atm(n)%domain, Atm(n)%ptop)
       call timing_off('FV_UPDATE_PHYS')
   call mpp_clock_end (id_dynam)

!--- nesting update after updating atmospheric variables with
!--- physics tendencies
    if (ngrids > 1 .and. p_split > 0) then
       call timing_on('TWOWAY_UPDATE')
       call twoway_nesting(Atm, ngrids, grids_on_this_pe, zvir, dt_atmos)
       call timing_off('TWOWAY_UPDATE')
    endif   
   call nullify_domain()

  !---- diagnostics for FV dynamics -----
   if (Atm(mytile)%flagstruct%print_freq /= -99) then
     call mpp_clock_begin(id_fv_diag)

     fv_time = Time_next
     call get_time (fv_time, seconds,  days)

     call nullify_domain()
     call timing_on('FV_DIAG')
     call fv_diag(Atm(mytile:mytile), zvir, fv_time, Atm(mytile)%flagstruct%print_freq)
     first_diag = .false.
     call timing_off('FV_DIAG')

     call mpp_clock_end(id_fv_diag)
   endif

 end subroutine atmosphere_state_update


 subroutine adiabatic_init(zvir)
   real, allocatable, dimension(:,:,:):: u0, v0, t0, dp0
   real, intent(in):: zvir
   real, parameter:: wt = 1.  ! was 2.
   real:: xt
   integer:: isc, iec, jsc, jec, npz
   integer:: m, n, i,j,k, ngc, sphum

   character(len=80) :: errstr

   xt = 1./(1.+wt)

   write(errstr,'(A, I4, A)') 'Performing adiabatic init',  Atm(mytile)%flagstruct%na_init, ' times'
   call mpp_error(NOTE, errstr)
   sphum = get_tracer_index (MODEL_ATMOS, 'sphum' )

    npz = Atm(mytile)%npz

    isc = Atm(mytile)%bd%isc
    iec = Atm(mytile)%bd%iec
    jsc = Atm(mytile)%bd%jsc
    jec = Atm(mytile)%bd%jec

    ngc = Atm(mytile)%ng
    isd = isc - ngc
    ied = iec + ngc
    jsd = jsc - ngc
    jed = jec + ngc

     call timing_on('adiabatic_init')
     do_adiabatic_init = .true.

     allocate ( u0(isc:iec,  jsc:jec+1, npz) )
     allocate ( v0(isc:iec+1,jsc:jec,   npz) )
     allocate ( t0(isc:iec,jsc:jec, npz) )
     allocate (dp0(isc:iec,jsc:jec, npz) )

#ifdef NUDGE_GZ
     call p_adi(npz, Atm(mytile)%ng, isc, iec, jsc, jec, Atm(mytile)%ptop,  &
                Atm(mytile)%delp, Atm(mytile)%pt, Atm(mytile)%ps, Atm(mytile)%pe,     &
                Atm(mytile)%peln, Atm(mytile)%pk, Atm(mytile)%pkz, Atm(mytile)%flagstruct%hydrostatic)
#endif

!$omp parallel do default (none) & 
!$omp              shared (npz, jsc, jec, isc, iec, n, sphum, u0, v0, t0, dp0, Atm, zvir, mytile) &
!$omp             private (k, j, i) 
       do k=1,npz
          do j=jsc,jec+1
             do i=isc,iec
                u0(i,j,k) = Atm(mytile)%u(i,j,k)
             enddo
          enddo
          do j=jsc,jec
             do i=isc,iec+1
                v0(i,j,k) = Atm(mytile)%v(i,j,k)
             enddo
          enddo
          do j=jsc,jec
             do i=isc,iec
#ifdef NUDGE_GZ
                t0(i,j,k) = Atm(mytile)%pt(i,j,k)*(1.+zvir*Atm(mytile)%q(i,j,k,1))*(Atm(mytile)%peln(i,k+1,j)-Atm(mytile)%peln(i,k,j))
#else
                t0(i,j,k) = Atm(mytile)%pt(i,j,k)*(1.+zvir*Atm(mytile)%q(i,j,k,sphum))  ! virt T
#endif
               dp0(i,j,k) = Atm(mytile)%delp(i,j,k)
             enddo
          enddo
       enddo

     do m=1,Atm(mytile)%flagstruct%na_init
! Forward call
    call fv_dynamics(Atm(mytile)%npx, Atm(mytile)%npy, npz,  nq, Atm(mytile)%ng, dt_atmos, 0.,      &
                     Atm(mytile)%flagstruct%fill, Atm(mytile)%flagstruct%reproduce_sum, kappa, cp_air, zvir,  &
                     Atm(mytile)%ptop, Atm(mytile)%ks, nq, Atm(mytile)%flagstruct%n_split,        &
                     Atm(mytile)%flagstruct%q_split, Atm(mytile)%u, Atm(mytile)%v, Atm(mytile)%w,         &
                     Atm(mytile)%delz, Atm(mytile)%flagstruct%hydrostatic,                      & 
                     Atm(mytile)%pt, Atm(mytile)%delp, Atm(mytile)%q, Atm(mytile)%ps,                     &
                     Atm(mytile)%pe, Atm(mytile)%pk, Atm(mytile)%peln, Atm(mytile)%pkz, Atm(mytile)%phis,      &
                     Atm(mytile)%q_con, Atm(mytile)%omga, Atm(mytile)%ua, Atm(mytile)%va, Atm(mytile)%uc, Atm(mytile)%vc, &
                     Atm(mytile)%ak, Atm(mytile)%bk, Atm(mytile)%mfx, Atm(mytile)%mfy,                    &
                     Atm(mytile)%cx, Atm(mytile)%cy, Atm(mytile)%ze0, Atm(mytile)%flagstruct%hybrid_z,    &
                     Atm(mytile)%gridstruct, Atm(mytile)%flagstruct,                            &
                     Atm(mytile)%neststruct, Atm(mytile)%idiag, Atm(mytile)%bd, Atm(mytile)%parent_grid,  &
                     Atm(mytile)%domain)
! Backward
    call fv_dynamics(Atm(mytile)%npx, Atm(mytile)%npy, npz,  nq, Atm(mytile)%ng, -dt_atmos, 0.,      &
                     Atm(mytile)%flagstruct%fill, Atm(mytile)%flagstruct%reproduce_sum, kappa, cp_air, zvir,  &
                     Atm(mytile)%ptop, Atm(mytile)%ks, nq, Atm(mytile)%flagstruct%n_split,        &
                     Atm(mytile)%flagstruct%q_split, Atm(mytile)%u, Atm(mytile)%v, Atm(mytile)%w,         &
                     Atm(mytile)%delz, Atm(mytile)%flagstruct%hydrostatic,                      & 
                     Atm(mytile)%pt, Atm(mytile)%delp, Atm(mytile)%q, Atm(mytile)%ps,                     &
                     Atm(mytile)%pe, Atm(mytile)%pk, Atm(mytile)%peln, Atm(mytile)%pkz, Atm(mytile)%phis,      &
                     Atm(mytile)%q_con, Atm(mytile)%omga, Atm(mytile)%ua, Atm(mytile)%va, Atm(mytile)%uc, Atm(mytile)%vc, &
                     Atm(mytile)%ak, Atm(mytile)%bk, Atm(mytile)%mfx, Atm(mytile)%mfy,                    &
                     Atm(mytile)%cx, Atm(mytile)%cy, Atm(mytile)%ze0, Atm(mytile)%flagstruct%hybrid_z,    &
                     Atm(mytile)%gridstruct, Atm(mytile)%flagstruct,                            &
                     Atm(mytile)%neststruct, Atm(mytile)%idiag, Atm(mytile)%bd, Atm(mytile)%parent_grid,  &
                     Atm(mytile)%domain)
! Nudging back to IC
!$omp parallel do default (none) &
!$omp             shared (npz, jsc, jec, isc, iec, n, sphum, Atm, u0, v0, t0, dp0, xt, zvir, mytile) &
!$omp            private (i, j, k)
       do k=1,npz
          do j=jsc,jec+1
             do i=isc,iec
                Atm(mytile)%u(i,j,k) = xt*(Atm(mytile)%u(i,j,k) + wt*u0(i,j,k))
             enddo
          enddo
          do j=jsc,jec
             do i=isc,iec+1
                Atm(mytile)%v(i,j,k) = xt*(Atm(mytile)%v(i,j,k) + wt*v0(i,j,k))
             enddo
          enddo
          do j=jsc,jec
             do i=isc,iec
#ifndef NUDGE_GZ
                Atm(mytile)%pt(i,j,k) = xt*(Atm(mytile)%pt(i,j,k) + wt*t0(i,j,k)/(1.+zvir*Atm(mytile)%q(i,j,k,sphum)))
#endif
                Atm(mytile)%delp(i,j,k) = xt*(Atm(mytile)%delp(i,j,k) + wt*dp0(i,j,k))
             enddo
          enddo
       enddo

#ifdef NUDGE_GZ
     call p_adi(npz, Atm(mytile)%ng, isc, iec, jsc, jec, Atm(mytile)%ptop,  &
                Atm(mytile)%delp, Atm(mytile)%pt, Atm(mytile)%ps, Atm(mytile)%pe,     &
                Atm(mytile)%peln, Atm(mytile)%pk, Atm(mytile)%pkz, Atm(mytile)%flagstruct%hydrostatic)
!$omp parallel do default (none) &
!$omp             shared (npz, jsc, jec, isc, iec, Atm, t0, xt, zvir, mytile) &
!$omp            private (i, j, k)
       do k=1,npz
          do j=jsc,jec
             do i=isc,iec
                Atm(mytile)%pt(i,j,k) = xt*(Atm(mytile)%pt(i,j,k)+wt*t0(i,j,k)/((1.+zvir*Atm(mytile)%q(i,j,k,1))*(Atm(mytile)%peln(i,k+1,j)-Atm(mytile)%peln(i,k,j))))
             enddo
          enddo
       enddo
#endif

! Backward
    call fv_dynamics(Atm(mytile)%npx, Atm(mytile)%npy, npz,  nq, Atm(mytile)%ng, -dt_atmos, 0.,      &
                     Atm(mytile)%flagstruct%fill, Atm(mytile)%flagstruct%reproduce_sum, kappa, cp_air, zvir,  &
                     Atm(mytile)%ptop, Atm(mytile)%ks, nq, Atm(mytile)%flagstruct%n_split,        &
                     Atm(mytile)%flagstruct%q_split, Atm(mytile)%u, Atm(mytile)%v, Atm(mytile)%w,         &
                     Atm(mytile)%delz, Atm(mytile)%flagstruct%hydrostatic,                      & 
                     Atm(mytile)%pt, Atm(mytile)%delp, Atm(mytile)%q, Atm(mytile)%ps,                     &
                     Atm(mytile)%pe, Atm(mytile)%pk, Atm(mytile)%peln, Atm(mytile)%pkz, Atm(mytile)%phis,      &
                     Atm(mytile)%q_con, Atm(mytile)%omga, Atm(mytile)%ua, Atm(mytile)%va, Atm(mytile)%uc, Atm(mytile)%vc, &
                     Atm(mytile)%ak, Atm(mytile)%bk, Atm(mytile)%mfx, Atm(mytile)%mfy,                    &
                     Atm(mytile)%cx, Atm(mytile)%cy, Atm(mytile)%ze0, Atm(mytile)%flagstruct%hybrid_z,    &
                     Atm(mytile)%gridstruct, Atm(mytile)%flagstruct,                            &
                     Atm(mytile)%neststruct, Atm(mytile)%idiag, Atm(mytile)%bd, Atm(mytile)%parent_grid,  &
                     Atm(mytile)%domain)
! Forward call
    call fv_dynamics(Atm(mytile)%npx, Atm(mytile)%npy, npz,  nq, Atm(mytile)%ng, dt_atmos, 0.,      &
                     Atm(mytile)%flagstruct%fill, Atm(mytile)%flagstruct%reproduce_sum, kappa, cp_air, zvir,  &
                     Atm(mytile)%ptop, Atm(mytile)%ks, nq, Atm(mytile)%flagstruct%n_split,        &
                     Atm(mytile)%flagstruct%q_split, Atm(mytile)%u, Atm(mytile)%v, Atm(mytile)%w,         &
                     Atm(mytile)%delz, Atm(mytile)%flagstruct%hydrostatic,                      & 
                     Atm(mytile)%pt, Atm(mytile)%delp, Atm(mytile)%q, Atm(mytile)%ps,                     &
                     Atm(mytile)%pe, Atm(mytile)%pk, Atm(mytile)%peln, Atm(mytile)%pkz, Atm(mytile)%phis,      &
                     Atm(mytile)%q_con, Atm(mytile)%omga, Atm(mytile)%ua, Atm(mytile)%va, Atm(mytile)%uc, Atm(mytile)%vc, &
                     Atm(mytile)%ak, Atm(mytile)%bk, Atm(mytile)%mfx, Atm(mytile)%mfy,                    &
                     Atm(mytile)%cx, Atm(mytile)%cy, Atm(mytile)%ze0, Atm(mytile)%flagstruct%hybrid_z,    &
                     Atm(mytile)%gridstruct, Atm(mytile)%flagstruct,                            &
                     Atm(mytile)%neststruct, Atm(mytile)%idiag, Atm(mytile)%bd, Atm(mytile)%parent_grid,  &
                     Atm(mytile)%domain)
! Nudging back to IC
!$omp parallel do default (none) &
!$omp             shared (npz, jsc, jec, isc, iec, n, sphum, Atm, u0, v0, t0, dp0, xt, zvir, mytile) &
!$omp            private (i, j, k)
       do k=1,npz
          do j=jsc,jec+1
             do i=isc,iec
                Atm(mytile)%u(i,j,k) = xt*(Atm(mytile)%u(i,j,k) + wt*u0(i,j,k))
             enddo
          enddo
          do j=jsc,jec
             do i=isc,iec+1
                Atm(mytile)%v(i,j,k) = xt*(Atm(mytile)%v(i,j,k) + wt*v0(i,j,k))
             enddo
          enddo
          do j=jsc,jec
             do i=isc,iec
#ifndef NUDGE_GZ
                Atm(mytile)%pt(i,j,k) = xt*(Atm(mytile)%pt(i,j,k) + wt*t0(i,j,k)/(1.+zvir*Atm(mytile)%q(i,j,k,sphum)))
#endif
                Atm(mytile)%delp(i,j,k) = xt*(Atm(mytile)%delp(i,j,k) + wt*dp0(i,j,k))
             enddo
          enddo
       enddo

#ifdef NUDGE_GZ
     call p_adi(npz, Atm(mytile)%ng, isc, iec, jsc, jec, Atm(mytile)%ptop,  &
                Atm(mytile)%delp, Atm(mytile)%pt, Atm(mytile)%ps, Atm(mytile)%pe,     &
                Atm(mytile)%peln, Atm(mytile)%pk, Atm(mytile)%pkz, Atm(mytile)%flagstruct%hydrostatic)
!$omp parallel do default (none) &
!$omp             shared (npz, jsc, jec, isc, iec, Atm, t0, xt, zvir, mytile) &
!$omp            private (i, j, k)
       do k=1,npz
          do j=jsc,jec
             do i=isc,iec
                Atm(mytile)%pt(i,j,k) = xt*(Atm(mytile)%pt(i,j,k)+wt*t0(i,j,k)/((1.+zvir*Atm(mytile)%q(i,j,k,1))*(Atm(mytile)%peln(i,k+1,j)-Atm(mytile)%peln(i,k,j))))
             enddo
          enddo
       enddo
#endif

     enddo

     deallocate ( u0 )
     deallocate ( v0 )
     deallocate ( t0 )
     deallocate (dp0 )

     do_adiabatic_init = .false.
     call timing_off('adiabatic_init')

 end subroutine adiabatic_init



#if defined(OVERLOAD_R4)
#define _DBL_(X) DBLE(X)
#define _RL_(X) REAL(X,KIND=4)
#else
#define _DBL_(X) X
#define _RL_(X) X
#endif
 subroutine atmos_phys_driver_statein (Statein, Atm_block)
   type (state_fields_in), dimension(:), intent(inout) :: Statein
   type (block_control_type),            intent(in)    :: Atm_block
!--------------------------------------
! Local GFS-phys consistent parameters:
!--------------------------------------
   real(kind=kind_phys), parameter:: p00 = 1.e5
   real(kind=kind_phys), parameter:: qmin = 1.0e-10   
   real(kind=kind_phys):: pk0inv, ptop, pktop
   real(kind=kind_phys) :: rTv
   integer :: nb, npz, ibs, ibe, jbs, jbe, i, j, k, ix, sphum, liq_wat, k1
   logical :: diag_sounding = .false.

!!! NOTES: lmh 6nov15
!!! - "Layer" means "layer mean", ie. the average value in a layer
!!! - "Level" means "level interface", ie the point values at the top or bottom of a layer

   ptop =  _DBL_(_RL_(Atm(mytile)%ak(1)))
   pktop  = (ptop/p00)**kappa
   pk0inv = (1.0_kind_phys/p00)**kappa

   sphum = get_tracer_index (MODEL_ATMOS, 'sphum' )
   liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat' )
   if ( liq_wat<0 ) call mpp_error(FATAL, 'GFS condensate does not exist')

   npz = Atm_block%npz

!---------------------------------------------------------------------
! use most up to date atmospheric properties when running serially
!---------------------------------------------------------------------
!$OMP parallel do default (none) & 
!$OMP             shared  (Atm_block, Atm, Statein, npz, nq, ncnst, sphum, liq_wat, pk0inv, &
!$OMP                      ptop, pktop, zvir, mytile, diag_sounding) &
!$OMP             private (nb, ibs, ibe, jbs, jbe, i, j, ix, k1, rTv)

   do nb = 1,Atm_block%nblks
     ibs = Atm_block%ibs(nb)
     ibe = Atm_block%ibe(nb)
     jbs = Atm_block%jbs(nb)
     jbe = Atm_block%jbe(nb)

! gase_phase_mass <-- prsl
! log(pe) <-- prsik

     !-- level interface geopotential height (relative to the surface)
     Statein(nb)%phii(:,1) = 0.0_kind_phys
     Statein(nb)%adjtrc = 1.0_kind_phys
     Statein(nb)%prsik(:,:) = 1.e25_kind_phys

     do k = 1, npz
        ix = 0 
        do j=jbs,jbe
           do i=ibs,ibe
              ix = ix + 1
                 !Indices for FV's vertical coordinate, for which 1 = top
                 !here, k is the index for GFS's vertical coordinate, for which 1 = bottom
              k1 = npz+1-k ! flipping the index
              Statein(nb)%tgrs(ix,k) = _DBL_(_RL_(Atm(mytile)%pt(i,j,k1)))
              Statein(nb)%ugrs(ix,k) = _DBL_(_RL_(Atm(mytile)%ua(i,j,k1)))
              Statein(nb)%vgrs(ix,k) = _DBL_(_RL_(Atm(mytile)%va(i,j,k1)))
               Statein(nb)%vvl(ix,k) = _DBL_(_RL_(Atm(mytile)%omga(i,j,k1)))
              Statein(nb)%prsl(ix,k) = _DBL_(_RL_(Atm(mytile)%delp(i,j,k1)))   ! Total mass

              if (.not.Atm(mytile)%flagstruct%hydrostatic .and. (.not.Atm(mytile)%flagstruct%use_hydro_pressure))  &
              Statein(nb)%phii(ix,k+1) = Statein(nb)%phii(ix,k) - _DBL_(_RL_(Atm(mytile)%delz(i,j,k1)*grav))

! Convert to tracer mass:
              Statein(nb)%qgrs_rad(ix,k)  =  _DBL_(_RL_(max(qmin, Atm(mytile)%q(i,j,k1,sphum)))) * Statein(nb)%prsl(ix,k)
              Statein(nb)%qgrs(ix,k,1:nq) =  _DBL_(_RL_(          Atm(mytile)%q(i,j,k1,1:nq))) * Statein(nb)%prsl(ix,k)
              Statein(nb)%qgrs(ix,k,nq+1:ncnst) = _DBL_(_RL_(Atm(mytile)%qdiag(i,j,k1,nq+1:ncnst))) * Statein(nb)%prsl(ix,k)
! Remove the contribution of condensates to delp (mass):
              Statein(nb)%prsl(ix,k) = Statein(nb)%prsl(ix,k) - Statein(nb)%qgrs(ix,k,liq_wat)
           enddo
        enddo
     enddo

! Re-compute pressure (dry_mass + water_vapor) derived fields:
     do i=1,ix
        Statein(nb)%prsi(i,npz+1) = ptop 
     enddo
     do k=npz,1,-1
        do i=1,ix
           Statein(nb)%prsi(i,k) = Statein(nb)%prsi(i,k+1) + Statein(nb)%prsl(i,k)
           Statein(nb)%prsik(i,k) = log( Statein(nb)%prsi(i,k) )
! Redefine mixing ratios for GFS == tracer_mass / (dry_air_mass + water_vapor_mass)
           Statein(nb)%qgrs_rad(i,k)     = Statein(nb)%qgrs_rad(i,k)     / Statein(nb)%prsl(i,k)
           Statein(nb)%qgrs(i,k,1:ncnst) = Statein(nb)%qgrs(i,k,1:ncnst) / Statein(nb)%prsl(i,k)
        enddo
     enddo
     do i=1,ix
        Statein(nb)%pgr(i) = Statein(nb)%prsi(i,1)    ! surface pressure for GFS
        Statein(nb)%prsik(i,npz+1) = log(ptop)
     enddo

     do k=1,npz
        do i=1,ix
! Geo-potential at interfaces:
           rTv = rdgas*Statein(nb)%tgrs(i,k)*(1.+zvir*Statein(nb)%qgrs_rad(i,k))
           if ( Atm(mytile)%flagstruct%hydrostatic .or. Atm(mytile)%flagstruct%use_hydro_pressure )   &
                Statein(nb)%phii(i,k+1) = Statein(nb)%phii(i,k) + rTv*(Statein(nb)%prsik(i,k)-Statein(nb)%prsik(i,k+1))
! Layer mean pressure by perfect gas law:
           Statein(nb)%prsl(i,k) = Statein(nb)%prsl(i,k)*rTv/(Statein(nb)%phii(i,k+1)-Statein(nb)%phii(i,k))
        enddo
     enddo

     do k = 1,npz
        do i=1,ix
! Exner function layer center: large sensitivity to non-hydro runs with moist kappa
           Statein(nb)%prslk(i,k) = exp( kappa*log(Statein(nb)%prsl(i,k)/p00) )
!--  layer center geopotential; geometric midpoint
           Statein(nb)%phil(i,k) = 0.5_kind_phys*(Statein(nb)%phii(i,k) + Statein(nb)%phii(i,k+1))
        enddo
     enddo

! Compute Exner function at layer "interfaces"
    do i=1,ix
!      Top & Bottom edges computed hydrostaticlly
       Statein(nb)%prsik(i,    1) = exp( kappa*Statein(nb)%prsik(i,1) )*pk0inv  ! bottom
       Statein(nb)%prsik(i,npz+1) = pktop                           ! TOA
    enddo

    if ( Atm(mytile)%flagstruct%hydrostatic .or. Atm(mytile)%flagstruct%use_hydro_pressure ) then
        do k=2,npz
           do i=1,ix
              Statein(nb)%prsik(i,k) = exp( kappa*Statein(nb)%prsik(i,k) )*pk0inv 
           enddo
        enddo
#ifdef DEBUG_GFS
    else
! Bottom limited by hydrostatic surface pressure
        do i=1,ix
           Statein(nb)%prsik(i,1) = max(Statein(nb)%prsik(i,1), Statein(nb)%prslk(i,1))
        enddo
! Interior: linear interpolation in height using layer mean
        do k=2,npz
           do i=1,ix
!             Statein(nb)%prsik(i,k) = 0.5*(Statein(nb)%prslk(i,k-1) + Statein(nb)%prslk(i,k))
              Statein(nb)%prsik(i,k) = Statein(nb)%prslk(i,k-1) + (Statein(nb)%prslk(i,k)-Statein(nb)%prslk(i,k-1)) *   &
                   (Statein(nb)%phii(i,k)-Statein(nb)%phil(i,k-1))/(Statein(nb)%phil(i,k)-Statein(nb)%phil(i,k-1))
           enddo
        enddo
#endif
    endif

!!$!!! DEBUG CODE
!!$     if (diag_sounding) then
!!$        if (ibs == 1 .and. jbs == 1) then
!!$           do k=1,npz
!!$              write(mpp_pe()+1000,'(I4,3x, 6(F,3x))') k, Statein(nb)%prsi(1,k), Statein(nb)%prsl(1,k), &
!!$                   Statein(nb)%phii(1,k), Statein(nb)%phil(1,k), &
!!$                   Atm(mytile)%delp(1,1,npz+1-k), Atm(mytile)%pe(1,npz+1-k,1)
!!$           enddo
!!$           k = npz+1
!!$        endif
!!$        diag_sounding = .false.
!!$     endif
!!$!!! END DEBUG CODE

  enddo


 end subroutine atmos_phys_driver_statein

 subroutine fill_gfs(im, km, pe2, q, q_min)
!SJL: this routine is the equivalent of fillz except that the vertical index is upside down
   integer, intent(in):: im, km
   real(kind=kind_phys), intent(in):: pe2(im,km+1)       ! pressure interface
   real(kind=kind_phys), intent(in):: q_min
   real(kind=kind_phys), intent(inout):: q(im,km)
!  LOCAL VARIABLES:
   real(kind=kind_phys) :: dp(im,km)
   integer:: i, k, k1

   do k=1,km
      do i=1,im
         dp(i,k) = pe2(i,k) - pe2(i,k+1)
      enddo
   enddo
 
! From bottom up:
   do k=1,km-1
      k1 = k+1
      do i=1,im
         if ( q(i,k)<q_min ) then
! Take mass from above so that q >= q_min
              q(i,k1) = q(i,k1) + (q(i,k)-q_min)*dp(i,k)/dp(i,k1)
              q(i,k ) = q_min
         endif
      enddo
   enddo

! From top down:
   do k=km,2,-1
      k1 = k-1
      do i=1,im
         if ( q(i,k)<0.0 ) then
! Take mass from below
              q(i,k1) = q(i,k1) + q(i,k)*dp(i,k)/dp(i,k1)
              q(i,k ) = 0.
         endif
      enddo
   enddo

 end subroutine fill_gfs

end module atmosphere_mod
