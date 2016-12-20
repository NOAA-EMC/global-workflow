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
module fv_restart_mod

  !<OVERVIEW>
  ! Restart facilities for FV core
  !</OVERVIEW>
  !<DESCRIPTION>
  ! This module writes and reads restart files for the FV core. Additionally
  ! it provides setup and calls routines necessary to provide a complete restart
  ! for the model.
  !</DESCRIPTION>

  use constants_mod,       only: kappa, pi=>pi_8, omega, rdgas, grav, rvgas, cp_air, radius, R_GRID
  use fv_arrays_mod,       only: fv_atmos_type, fv_nest_type, fv_grid_bounds_type
  use fv_io_mod,           only: fv_io_init, fv_io_read_restart, fv_io_write_restart, &
                                 remap_restart, fv_io_register_restart, fv_io_register_nudge_restart, &
                                 fv_io_register_restart_BCs, fv_io_register_restart_BCs_NH, fv_io_write_BCs, fv_io_read_BCs
  use fv_grid_utils_mod,   only: ptop_min, fill_ghost, g_sum, &
                                 make_eta_level, cubed_to_latlon, great_circle_dist
  use fv_diagnostics_mod,  only: prt_maxmin
  use init_hydro_mod,      only: p_var
  use mpp_domains_mod,     only: mpp_update_domains, domain2d, DGRID_NE
  use mpp_mod,             only: mpp_chksum, stdout, mpp_error, FATAL, NOTE, get_unit, mpp_sum
  use test_cases_mod,      only: test_case, alpha, init_case, init_double_periodic, init_latlon
  use fv_mp_mod,           only: is_master, switch_current_Atm, mp_reduce_min, mp_reduce_max
  use fv_surf_map_mod,     only: sgh_g, oro_g
  use tracer_manager_mod,  only: get_tracer_names
  use field_manager_mod,   only: MODEL_ATMOS
  use external_ic_mod,     only: get_external_ic, get_cubed_sphere_terrain
  use fv_eta_mod,          only: compute_dz_var, compute_dz_L32, set_hybrid_z
  use fv_surf_map_mod,     only: del2_cubed_sphere, del4_cubed_sphere
  use boundary_mod,        only: fill_nested_grid, nested_grid_BC, update_coarse_grid
   use tracer_manager_mod, only: get_tracer_index
   use field_manager_mod,  only: MODEL_ATMOS
   use fv_timing_mod,      only: timing_on, timing_off
  use mpp_domains_mod,     only: mpp_get_compute_domain, mpp_get_data_domain, mpp_get_global_domain
  use mpp_mod,             only: mpp_send, mpp_recv, mpp_sync_self, mpp_set_current_pelist, mpp_get_current_pelist, mpp_npes, mpp_pe, mpp_sync
  use mpp_domains_mod,     only: CENTER, CORNER, NORTH, EAST,  mpp_get_C2F_index, WEST, SOUTH
  use mpp_domains_mod,     only: mpp_global_field
  use fms_mod,             only: file_exist
  use external_sst_mod,    only: use_ncep_sst           

  implicit none
  private

  public :: fv_restart_init, fv_restart_end, fv_restart, fv_write_restart, setup_nested_boundary_halo
  public :: d2c_setup, d2a_setup

  real(kind=R_GRID), parameter :: cnst_0p20=0.20d0
  !--- private data type
  logical                       :: module_is_initialized = .FALSE.

!---- version number -----
  character(len=128) :: version = '$Id$'
  character(len=128) :: tagname = '$Name$'

contains 

  !#####################################################################
  ! <SUBROUTINE NAME="fv_restart_init">
  !
  ! <DESCRIPTION>
  ! Initialize the fv core restart facilities
  ! </DESCRIPTION>
  !
  subroutine fv_restart_init()
    call fv_io_init()
    module_is_initialized = .TRUE.
  end subroutine fv_restart_init
  ! </SUBROUTINE> NAME="fv_restart_init"


    !#####################################################################
  ! <SUBROUTINE NAME="fv_restart">
  !
  ! <DESCRIPTION>
  ! The fv core restart facility
  ! </DESCRIPTION>
  !
  subroutine fv_restart(fv_domain, Atm, dt_atmos, seconds, days, cold_start, grid_type, grids_on_this_pe)
    type(domain2d),      intent(inout) :: fv_domain
    type(fv_atmos_type), intent(inout) :: Atm(:)
    real,                intent(in)    :: dt_atmos
    integer,             intent(out)   :: seconds
    integer,             intent(out)   :: days
    logical,             intent(inout)    :: cold_start
    integer,             intent(in)    :: grid_type
    logical, intent(INOUT) :: grids_on_this_pe(:)


    integer :: i, j, k, n, ntileMe, nt, iq
    integer :: isc, iec, jsc, jec, npz, npz_rst, ncnst, ntprog, ntdiag
    integer :: isd, ied, jsd, jed
    integer isd_p, ied_p, jsd_p, jed_p, isc_p, iec_p, jsc_p, jec_p, isg, ieg, jsg,jeg, npx_p, npy_p
    real, allocatable :: g_dat(:,:,:)

    integer :: unit
    real, allocatable :: dz1(:)
    real rgrav, f00, ztop, pertn
    logical :: hybrid
    logical :: cold_start_grids(size(Atm))
    character(len=128):: tname, errstring, fname, tracer_name
    character(len=120):: fname_ne, fname_sw
    character(len=3) :: gn

    integer :: npts
    real    :: sumpertn

    rgrav = 1. / grav

    if(.not.module_is_initialized) call mpp_error(FATAL, 'You must call fv_restart_init.')

    ntileMe = size(Atm(:))

    cold_start_grids(:) = cold_start
    do n = 1, ntileMe

       if (is_master()) then
          print*, 'FV_RESTART: ', n, cold_start_grids(n)
       endif

       if (Atm(n)%neststruct%nested) then
          write(fname,'(A, I2.2, A)') 'INPUT/fv_core.res.nest', Atm(n)%grid_number, '.nc'
          write(fname_ne,'(A, I2.2, A)') 'INPUT/fv_BC_ne.res.nest', Atm(n)%grid_number, '.nc'
          write(fname_sw,'(A, I2.2, A)') 'INPUT/fv_BC_sw.res.nest', Atm(n)%grid_number, '.nc'
          if (Atm(n)%flagstruct%external_ic) then
             if (is_master()) print*, 'External IC set on grid', Atm(n)%grid_number, ', re-initializing grid'
             cold_start_grids(n) = .true.
             Atm(n)%flagstruct%warm_start = .false. !resetting warm_start flag to avoid FATAL error below
          else
             if (is_master()) print*, 'Searching for nested grid restart file ', trim(fname)
             cold_start_grids(n) = .not. file_exist(fname, Atm(n)%domain)
             Atm(n)%flagstruct%warm_start = file_exist(fname, Atm(n)%domain)!resetting warm_start flag to avoid FATAL error below
          endif
       endif

       if (.not. grids_on_this_pe(n)) then
          
          !Even if this grid is not on this PE, if it has child grids we must send
          !along the data that is needed. 

          if (Atm(n)%neststruct%nested) then
             if (cold_start_grids(n)) then
                if (Atm(n)%parent_grid%flagstruct%n_zs_filter > 0) call fill_nested_grid_topo_halo(Atm(n), .false.)
                if (Atm(n)%flagstruct%nggps_ic) then
                   call fill_nested_grid_topo(Atm(n), .false.)
                   call fill_nested_grid_topo_halo(Atm(n), .false.)
                   call nested_grid_BC(Atm(n)%ps, Atm(n)%parent_grid%ps, Atm(n)%neststruct%nest_domain, &
                        Atm(n)%neststruct%ind_h, Atm(n)%neststruct%wt_h, 0, 0, &
                        Atm(n)%npx, Atm(n)%npy,Atm(n)%bd, isg, ieg, jsg, jeg, proc_in=.false.)         
                   call setup_nested_boundary_halo(Atm(n),.false.) 
                else
                   call fill_nested_grid_topo(Atm(n), .false.)
                   call setup_nested_boundary_halo(Atm(n),.false.) 
                   if ( Atm(n)%flagstruct%external_ic .and. grid_type < 4 ) call fill_nested_grid_data(Atm(n:n), .false.)
                endif
             else
                if (is_master()) print*, 'Searching for nested grid BC files ', trim(fname_ne), ' ', trim (fname_sw)

                !!!! PROBLEM: file_exist doesn't know to look for fv_BC_ne.res.nest02.nc instead of fv_BC_ne.res.nc on coarse grid
                if (file_exist(fname_ne, Atm(n)%domain) .and. file_exist(fname_sw, Atm(n)%domain)) then
                else
                   if ( is_master() ) write(*,*) 'BC files not found, re-generating nested grid boundary conditions'
                   call fill_nested_grid_topo_halo(Atm(n), .false.)
                   call setup_nested_boundary_halo(Atm(n), .false.)
                   Atm(N)%neststruct%first_step = .true.                   
                endif
             end if

             if (.not. Atm(n)%flagstruct%hydrostatic .and. Atm(n)%flagstruct%make_nh) then
                call nested_grid_BC(Atm(n)%delz, Atm(n)%parent_grid%delz, Atm(n)%neststruct%nest_domain, &
                     Atm(n)%neststruct%ind_h, Atm(n)%neststruct%wt_h, 0, 0, &
                     Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%bd, isg, ieg, jsg, jeg, proc_in=.false.)
                call nested_grid_BC(Atm(n)%w, Atm(n)%parent_grid%w, Atm(n)%neststruct%nest_domain, &
                     Atm(n)%neststruct%ind_h, Atm(n)%neststruct%wt_h, 0, 0, &
                     Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%bd, isg, ieg, jsg, jeg, proc_in=.false.)
             endif


          endif

          cycle

       endif
       !This call still appears to be necessary to get isd, etc. correct
       call switch_current_Atm(Atm(n))

    npz     = Atm(1)%npz
    npz_rst = Atm(1)%flagstruct%npz_rst

    !--- call fv_io_register_restart to register restart field to be written out in fv_io_write_restart
    call fv_io_register_restart(Atm(n)%domain,Atm(n:n))
    if (Atm(n)%neststruct%nested) call fv_io_register_restart_BCs(Atm(n))
    if( .not.cold_start_grids(n) .and. (.not. Atm(n)%flagstruct%external_ic) ) then


        if ( npz_rst /= 0 .and. npz_rst /= npz ) then
!            Remap vertically the prognostic variables for the chosen vertical resolution
             if( is_master() ) then
                 write(*,*) ' '
                 write(*,*) '***** Important Note from FV core ********************'
                 write(*,*) 'Remapping dynamic IC from', npz_rst, 'levels to ', npz,'levels'
                 write(*,*) '***** End Note from FV core **************************'
                 write(*,*) ' '
             endif
             call remap_restart( Atm(n)%domain, Atm(n:n) )
             if( is_master() ) write(*,*) 'Done remapping dynamical IC'
        else
             if( is_master() ) write(*,*) 'Warm starting, calling fv_io_restart'
             call fv_io_read_restart(Atm(n)%domain,Atm(n:n))
        endif
    endif

!---------------------------------------------------------------------------------------------
! Read, interpolate (latlon to cubed), then remap vertically with terrain adjustment if needed
!---------------------------------------------------------------------------------------------
    if (Atm(n)%neststruct%nested) then
          if (cold_start_grids(n)) call fill_nested_grid_topo(Atm(n), .true.)
          !if (cold_start_grids(n) .and. .not. Atm(n)%flagstruct%nggps_ic) call fill_nested_grid_topo(Atm(n), .true.)
       if (cold_start_grids(n)) then
          if (Atm(n)%parent_grid%flagstruct%n_zs_filter > 0 .or. Atm(n)%flagstruct%nggps_ic) call fill_nested_grid_topo_halo(Atm(n), .true.)
       end if
       if (Atm(n)%flagstruct%external_ic .and. Atm(n)%flagstruct%nggps_ic) then
          !Fill nested grid halo with ps
          call nested_grid_BC(Atm(n)%ps, Atm(n)%parent_grid%ps, Atm(n)%neststruct%nest_domain, &
               Atm(n)%neststruct%ind_h, Atm(n)%neststruct%wt_h, 0, 0, &
               Atm(n)%npx, Atm(n)%npy,Atm(n)%bd, isg, ieg, jsg, jeg, proc_in=.true.)         
       endif
    endif
    if ( Atm(n)%flagstruct%external_ic ) then
         if( is_master() ) write(*,*) 'Calling get_external_ic'
         call get_external_ic(Atm(n:n), Atm(n)%domain, cold_start_grids(n)) 
         if( is_master() ) write(*,*) 'IC generated from the specified external source'
    endif

    seconds = 0; days = 0   ! Restart needs to be modified to record seconds and days.

! Notes by Jeff D.
  ! This logic doesn't work very well.
  ! Shouldn't have read for all tiles then loop over tiles

       isd = Atm(n)%bd%isd
       ied = Atm(n)%bd%ied
       jsd = Atm(n)%bd%jsd
       jed = Atm(n)%bd%jed
       ncnst = Atm(n)%ncnst
       isc = Atm(n)%bd%isc; iec = Atm(n)%bd%iec; jsc = Atm(n)%bd%jsc; jec = Atm(n)%bd%jec

    ! Init model data
       if(.not.cold_start_grids(n))then
          Atm(N)%neststruct%first_step = .false.
          if (Atm(n)%neststruct%nested) then
             if ( npz_rst /= 0 .and. npz_rst /= npz ) then
                call setup_nested_boundary_halo(Atm(n)) 
             else
                !If BC file is found, then read them in. Otherwise we need to initialize the BCs.
                if (is_master()) print*, 'Searching for nested grid BC files ', trim(fname_ne), ' ', trim (fname_sw)
                if (file_exist(fname_ne, Atm(n)%domain) .and. file_exist(fname_sw, Atm(n)%domain)) then
                   call fv_io_read_BCs(Atm(n))
                else
                   if ( is_master() ) write(*,*) 'BC files not found, re-generating nested grid boundary conditions'
                   call fill_nested_grid_topo_halo(Atm(n), .true.)
                   call setup_nested_boundary_halo(Atm(n), .true.)
                   Atm(N)%neststruct%first_step = .true.
                endif
                !Following line to make sure u and v are consistent across processor subdomains
                call mpp_update_domains(Atm(n)%u, Atm(n)%v, Atm(n)%domain, gridtype=DGRID_NE, complete=.true.)
             endif
          endif

        if ( Atm(n)%flagstruct%mountain ) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! !!! Additional terrain filter -- should not be called repeatedly !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if ( Atm(n)%flagstruct%n_zs_filter > 0 ) then
              if ( Atm(n)%flagstruct%nord_zs_filter == 2 ) then
                   call del2_cubed_sphere(Atm(n)%npx, Atm(n)%npy, Atm(n)%phis, &
                                          Atm(n)%gridstruct%area_64, Atm(n)%gridstruct%dx, Atm(n)%gridstruct%dy,   &
                                          Atm(n)%gridstruct%dxc, Atm(n)%gridstruct%dyc, Atm(n)%gridstruct%sin_sg, &
                                          Atm(n)%flagstruct%n_zs_filter, cnst_0p20*Atm(n)%gridstruct%da_min, &
                                          .false., oro_g, Atm(n)%neststruct%nested, Atm(n)%domain, Atm(n)%bd)
                   if ( is_master() ) write(*,*) 'Warning !!! del-2 terrain filter has been applied ', &
                        Atm(n)%flagstruct%n_zs_filter, ' times'
              else if( Atm(n)%flagstruct%nord_zs_filter == 4 ) then
                   call del4_cubed_sphere(Atm(n)%npx, Atm(n)%npy, Atm(n)%phis, Atm(n)%gridstruct%area_64, &
                                          Atm(n)%gridstruct%dx, Atm(n)%gridstruct%dy,   &
                                          Atm(n)%gridstruct%dxc, Atm(n)%gridstruct%dyc, Atm(n)%gridstruct%sin_sg, &
                                          Atm(n)%flagstruct%n_zs_filter, .false., oro_g, Atm(n)%neststruct%nested, &
                                          Atm(n)%domain, Atm(n)%bd)
                 if ( is_master() ) write(*,*) 'Warning !!! del-4 terrain filter has been applied ', &
                      Atm(n)%flagstruct%n_zs_filter, ' times'
              endif
            endif

            call mpp_update_domains( Atm(n)%phis, Atm(n)%domain, complete=.true. )
        else
             Atm(n)%phis = 0.
            if( is_master() ) write(*,*) 'phis set to zero'
         endif !mountain


#ifdef SW_DYNAMICS
        Atm(n)%pt(:,:,:)=1.
#else
        if ( .not.Atm(n)%flagstruct%hybrid_z ) then
           if(Atm(n)%ptop/=Atm(n)%ak(1)) call mpp_error(FATAL,'FV restart: ptop not equal Atm(n)%ak(1)')
        else
           Atm(n)%ptop = Atm(n)%ak(1);  Atm(n)%ks = 0
        endif
        call p_var(npz,         isc,         iec,       jsc,     jec,   Atm(n)%ptop,     ptop_min,  &
                   Atm(n)%delp, Atm(n)%delz, Atm(n)%pt, Atm(n)%ps, Atm(n)%pe, Atm(n)%peln,   &
                   Atm(n)%pk,   Atm(n)%pkz, kappa, Atm(n)%q, Atm(n)%ng, &
                   ncnst,  Atm(n)%gridstruct%area_64, Atm(n)%flagstruct%dry_mass,  &
                   Atm(n)%flagstruct%adjust_dry_mass,  Atm(n)%flagstruct%mountain, &
                   Atm(n)%flagstruct%moist_phys,  Atm(n)%flagstruct%hydrostatic, &
                   Atm(n)%flagstruct%nwat, Atm(n)%domain, Atm(n)%flagstruct%make_nh)

#endif
        if ( grid_type < 7 .and. grid_type /= 4 ) then
! Fill big values in the non-existing corner regions:
!          call fill_ghost(Atm(n)%phis, Atm(n)%npx, Atm(n)%npy, big_number)
           do j=jsd,jed+1
           do i=isd,ied+1
              Atm(n)%gridstruct%fc(i,j) = 2.*omega*( -cos(Atm(n)%gridstruct%grid(i,j,1))*cos(Atm(n)%gridstruct%grid(i,j,2))*sin(alpha) + &
                                    sin(Atm(n)%gridstruct%grid(i,j,2))*cos(alpha) )
           enddo
           enddo
           do j=jsd,jed
           do i=isd,ied
             Atm(n)%gridstruct%f0(i,j) = 2.*omega*( -cos(Atm(n)%gridstruct%agrid(i,j,1))*cos(Atm(n)%gridstruct%agrid(i,j,2))*sin(alpha) + &
                                    sin(Atm(n)%gridstruct%agrid(i,j,2))*cos(alpha) )
           enddo
           enddo
        else
           f00 = 2.*omega*sin(Atm(n)%flagstruct%deglat/180.*pi)
           do j=jsd,jed+1
              do i=isd,ied+1
                 Atm(n)%gridstruct%fc(i,j) = f00
              enddo
           enddo
           do j=jsd,jed
              do i=isd,ied
                 Atm(n)%gridstruct%f0(i,j) = f00
              enddo
           enddo
        endif
     else
       if ( Atm(n)%flagstruct%warm_start ) then
         call mpp_error(FATAL, 'FV restart files not found; set warm_start = .F. if cold_start is desired.')
      endif
! Cold start
       if ( Atm(n)%flagstruct%make_hybrid_z ) then
         hybrid = .false.
       else
         hybrid = Atm(n)%flagstruct%hybrid_z
       endif
         if (grid_type < 4) then
            if ( .not. Atm(n)%flagstruct%external_ic ) then
            call init_case(Atm(n)%u,Atm(n)%v,Atm(n)%w,Atm(n)%pt,Atm(n)%delp,Atm(n)%q, &
                           Atm(n)%phis, Atm(n)%ps,Atm(n)%pe, Atm(n)%peln,Atm(n)%pk,Atm(n)%pkz, &
                           Atm(n)%uc,Atm(n)%vc, Atm(n)%ua,Atm(n)%va,        & 
                           Atm(n)%ak, Atm(n)%bk, Atm(n)%gridstruct, Atm(n)%flagstruct,&
                           Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%ng, &
                           ncnst, Atm(n)%flagstruct%nwat,  &
                           Atm(n)%flagstruct%ndims, Atm(n)%flagstruct%ntiles, &
                           Atm(n)%flagstruct%dry_mass, &
                           Atm(n)%flagstruct%mountain,       &
                           Atm(n)%flagstruct%moist_phys, Atm(n)%flagstruct%hydrostatic, &
                           hybrid, Atm(n)%delz, Atm(n)%ze0, &
                           Atm(n)%flagstruct%adiabatic, Atm(n)%ks, Atm(n)%neststruct%npx_global, &
                           Atm(n)%ptop, Atm(n)%domain, Atm(n)%tile, Atm(n)%bd)
            endif
         elseif (grid_type == 4) then
            call init_double_periodic(Atm(n)%u,Atm(n)%v,Atm(n)%w,Atm(n)%pt, &
                                      Atm(n)%delp,Atm(n)%q,Atm(n)%phis, Atm(n)%ps,Atm(n)%pe, &
                                      Atm(n)%peln,Atm(n)%pk,Atm(n)%pkz, &
                                      Atm(n)%uc,Atm(n)%vc, Atm(n)%ua,Atm(n)%va,        & 
                                      Atm(n)%ak, Atm(n)%bk, &
                                      Atm(n)%gridstruct, Atm(n)%flagstruct, &
                                      Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%ng, &
                                      ncnst, Atm(n)%flagstruct%nwat,  &
                                      Atm(n)%flagstruct%ndims, Atm(n)%flagstruct%ntiles, &
                                      Atm(n)%flagstruct%dry_mass, Atm(n)%flagstruct%mountain, &
                                      Atm(n)%flagstruct%moist_phys, Atm(n)%flagstruct%hydrostatic, &
                                      hybrid, Atm(n)%delz, Atm(n)%ze0, Atm(n)%ks, Atm(n)%ptop, &
                                      Atm(n)%domain, Atm(n)%tile, Atm(n)%bd)
            if( is_master() ) write(*,*) 'Doubly Periodic IC generated'
         elseif (grid_type == 5 .or. grid_type == 6) then
            call init_latlon(Atm(n)%u,Atm(n)%v,Atm(n)%pt,Atm(n)%delp,Atm(n)%q,&
                             Atm(n)%phis, Atm(n)%ps,Atm(n)%pe, &
                             Atm(n)%peln,Atm(n)%pk,Atm(n)%pkz, &
                             Atm(n)%uc,Atm(n)%vc, Atm(n)%ua,Atm(n)%va,        &
                             Atm(n)%ak, Atm(n)%bk, Atm(n)%gridstruct, &
                             Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%ng, ncnst, &
                             Atm(n)%flagstruct%ndims, Atm(n)%flagstruct%ntiles, &
                             Atm(n)%flagstruct%dry_mass, &
                             Atm(n)%flagstruct%mountain,       &
                             Atm(n)%flagstruct%moist_phys, hybrid, Atm(n)%delz, &
                             Atm(n)%ze0, Atm(n)%domain, Atm(n)%tile)
         endif

         !Turn this off on the nested grid if you are just interpolating topography from the coarse grid!
        if ( Atm(n)%flagstruct%fv_land ) then
             do j=jsc,jec
                do i=isc,iec
                   Atm(n)%sgh(i,j) = sgh_g(i,j)
                   Atm(n)%oro(i,j) = oro_g(i,j)
                enddo
             enddo
          endif


          !Set up nested grids
          !Currently even though we do fill in the nested-grid IC from
          ! init_case or external_ic we appear to overwrite it using
          !  coarse-grid data
          !if (Atm(n)%neststruct%nested) then
          ! Only fill nested-grid data if external_ic is called for the cubed-sphere grid
          if (Atm(n)%neststruct%nested) then
             call setup_nested_boundary_halo(Atm(n), .true.) 
             if (Atm(n)%flagstruct%external_ic .and.  .not. Atm(n)%flagstruct%nggps_ic .and. grid_type < 4 ) then
                call fill_nested_grid_data(Atm(n:n))
             endif
          end if

       endif  !end cold_start check

       if ( (.not.Atm(n)%flagstruct%hydrostatic) .and. Atm(n)%flagstruct%make_nh .and. Atm(n)%neststruct%nested) then
          call nested_grid_BC(Atm(n)%delz, Atm(n)%parent_grid%delz, Atm(n)%neststruct%nest_domain, &
               Atm(n)%neststruct%ind_h, Atm(n)%neststruct%wt_h, 0, 0, &
               Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%bd, isg, ieg, jsg, jeg, proc_in=.true.)
          call nested_grid_BC(Atm(n)%w, Atm(n)%parent_grid%w, Atm(n)%neststruct%nest_domain, &
               Atm(n)%neststruct%ind_h, Atm(n)%neststruct%wt_h, 0, 0, &
               Atm(n)%npx, Atm(n)%npy, npz, Atm(n)%bd, isg, ieg, jsg, jeg, proc_in=.true.)
          call fv_io_register_restart_BCs_NH(Atm(n)) !needed to register nested-grid BCs not registered earlier
       endif

  end do


    do n = ntileMe,1,-1
       if (Atm(n)%neststruct%nested .and. Atm(n)%flagstruct%external_ic .and. &
            Atm(n)%flagstruct%grid_type < 4 .and. cold_start_grids(n)) then
          call fill_nested_grid_data_end(Atm(n), grids_on_this_pe(n))
       endif
    end do

    do n = 1, ntileMe
       if (.not. grids_on_this_pe(n)) cycle

       isd = Atm(n)%bd%isd
       ied = Atm(n)%bd%ied
       jsd = Atm(n)%bd%jsd
       jed = Atm(n)%bd%jed
       ncnst = Atm(n)%ncnst
       ntprog = size(Atm(n)%q,4)
       ntdiag = size(Atm(n)%qdiag,4)
       isc = Atm(n)%bd%isc; iec = Atm(n)%bd%iec; jsc = Atm(n)%bd%jsc; jec = Atm(n)%bd%jec


!---------------------------------------------------------------------------------------------
! Transform the (starting) Eulerian vertical coordinate from sigma-p to hybrid_z
     if ( Atm(n)%flagstruct%hybrid_z ) then
       if ( Atm(n)%flagstruct%make_hybrid_z ) then
          allocate ( dz1(npz) )
          if( npz==32 ) then
              call compute_dz_L32(npz, ztop, dz1)
          else
              ztop = 45.E3
              call compute_dz_var(npz, ztop, dz1)
          endif
          call set_hybrid_z(isc, iec, jsc, jec, Atm(n)%ng, npz, ztop, dz1, rgrav,  &
                            Atm(n)%phis, Atm(n)%ze0)
          deallocate ( dz1 )
!         call prt_maxmin('ZE0', Atm(n)%ze0,  isc, iec, jsc, jec, 0, npz, 1.E-3)
!         call prt_maxmin('DZ0', Atm(n)%delz, isc, iec, jsc, jec, 0, npz, 1.   )
       endif
!      call make_eta_level(npz, Atm(n)%pe, area, Atm(n)%ks, Atm(n)%ak, Atm(n)%bk, Atm(n)%ptop)
     endif
!---------------------------------------------------------------------------------------------

     if (Atm(n)%flagstruct%add_noise > 0.) then
        write(errstring,'(A, E16.9)') "Adding thermal noise of amplitude ", Atm(n)%flagstruct%add_noise
        call mpp_error(NOTE, errstring)
        call random_seed
        npts = 0
        sumpertn = 0.
        do k=1,npz
        do j=jsc,jec
        do i=isc,iec
           call random_number(pertn)
           Atm(n)%pt(i,j,k) = Atm(n)%pt(i,j,k) + pertn*Atm(n)%flagstruct%add_noise
           npts = npts + 1
           sumpertn = sumpertn + pertn*Atm(n)%flagstruct%add_noise ** 2
        enddo
        enddo
        enddo
        call mpp_update_domains(Atm(n)%pt, Atm(n)%domain)
        call mpp_sum(sumpertn)
        call mpp_sum(npts)
        write(errstring,'(A, E16.9)') "RMS added noise: ", sqrt(sumpertn/npts)
        call mpp_error(NOTE, errstring)
     endif

      if (Atm(n)%grid_number > 1) then
         write(gn,'(A2, I1)') " g", Atm(n)%grid_number
      else
         gn = ''
      end if

      unit = stdout()
      write(unit,*)
      write(unit,*) 'fv_restart u   ', trim(gn),' = ', mpp_chksum(Atm(n)%u(isc:iec,jsc:jec,:))
      write(unit,*) 'fv_restart v   ', trim(gn),' = ', mpp_chksum(Atm(n)%v(isc:iec,jsc:jec,:))
      if ( .not.Atm(n)%flagstruct%hydrostatic )   &
        write(unit,*) 'fv_restart w   ', trim(gn),' = ', mpp_chksum(Atm(n)%w(isc:iec,jsc:jec,:))
      write(unit,*) 'fv_restart delp', trim(gn),' = ', mpp_chksum(Atm(n)%delp(isc:iec,jsc:jec,:))
      write(unit,*) 'fv_restart phis', trim(gn),' = ', mpp_chksum(Atm(n)%phis(isc:iec,jsc:jec))

#ifdef SW_DYNAMICS
      call prt_maxmin('H ', Atm(n)%delp, isc, iec, jsc, jec, Atm(n)%ng, 1, rgrav)
#else
      write(unit,*) 'fv_restart pt  ', trim(gn),' = ', mpp_chksum(Atm(n)%pt(isc:iec,jsc:jec,:))
      if (ntprog>0) &
           write(unit,*) 'fv_restart q(prog) nq  ', trim(gn),' =',ntprog, mpp_chksum(Atm(n)%q(isc:iec,jsc:jec,:,:))
      if (ntdiag>0) &
           write(unit,*) 'fv_restart q(diag) nq  ', trim(gn),' =',ntdiag, mpp_chksum(Atm(n)%qdiag(isc:iec,jsc:jec,:,:))
      do iq=1,min(7, ntprog)     ! Check up to 7 tracers
        call get_tracer_names(MODEL_ATMOS, iq, tracer_name)
        write(unit,*) 'fv_restart '//trim(tracer_name)//' = ', mpp_chksum(Atm(n)%q(isc:iec,jsc:jec,:,iq))
      enddo
!---------------
! Check Min/Max:
!---------------
      call pmaxmn_g('ZS', Atm(n)%phis, isc, iec, jsc, jec, 1, rgrav, Atm(n)%gridstruct%area_64, Atm(n)%domain)
      call pmaxmn_g('PS', Atm(n)%ps,   isc, iec, jsc, jec, 1, 0.01,  Atm(n)%gridstruct%area_64, Atm(n)%domain)
      call pmaxmn_g('T ', Atm(n)%pt,   isc, iec, jsc, jec, npz, 1.,  Atm(n)%gridstruct%area_64, Atm(n)%domain)

! Check tracers:
      do i=1, ntprog
          call get_tracer_names ( MODEL_ATMOS, i, tname )
          call pmaxmn_g(trim(tname), Atm(n)%q(isd:ied,jsd:jed,1:npz,i:i), isc, iec, jsc, jec, npz, &
                        1., Atm(n)%gridstruct%area_64, Atm(n)%domain)
      enddo
#endif
      call prt_maxmin('U ', Atm(n)%u(isc:iec,jsc:jec,1:npz), isc, iec, jsc, jec, 0, npz, 1.)
      call prt_maxmin('V ', Atm(n)%v(isc:iec,jsc:jec,1:npz), isc, iec, jsc, jec, 0, npz, 1.)

      if ( (.not.Atm(n)%flagstruct%hydrostatic) .and. Atm(n)%flagstruct%make_nh ) then
         call mpp_error(NOTE, "  Initializing w to 0")
         Atm(n)%w = 0.
         if ( .not.Atm(n)%flagstruct%hybrid_z ) then
            call mpp_error(NOTE, "  Initializing delz from hydrostatic state")
             do k=1,npz
                do j=jsc,jec
                   do i=isc,iec
                      Atm(n)%delz(i,j,k) = (rdgas*rgrav)*Atm(n)%pt(i,j,k)*(Atm(n)%peln(i,k,j)-Atm(n)%peln(i,k+1,j))
                   enddo
                enddo
             enddo
         endif
      endif

      if ( .not.Atm(n)%flagstruct%hydrostatic )   &
      call pmaxmn_g('W ', Atm(n)%w, isc, iec, jsc, jec, npz, 1., Atm(n)%gridstruct%area_64, Atm(n)%domain)

      if (is_master()) write(unit,*)

!--------------------------------------------
! Initialize surface winds for flux coupler:
!--------------------------------------------
    if ( .not. Atm(n)%flagstruct%srf_init ) then
         call cubed_to_latlon(Atm(n)%u, Atm(n)%v, Atm(n)%ua, Atm(n)%va, &
              Atm(n)%gridstruct, &
              Atm(n)%npx, Atm(n)%npy, npz, 1, &              
              Atm(n)%gridstruct%grid_type, Atm(n)%domain, &
              Atm(n)%gridstruct%nested, Atm(n)%flagstruct%c2l_ord, Atm(n)%bd)
         do j=jsc,jec
            do i=isc,iec
               Atm(n)%u_srf(i,j) = Atm(n)%ua(i,j,npz)
               Atm(n)%v_srf(i,j) = Atm(n)%va(i,j,npz)
            enddo
         enddo
         Atm(n)%flagstruct%srf_init = .true.
    endif

    end do   ! n_tile

  end subroutine fv_restart
  ! </SUBROUTINE> NAME="fv_restart"

  subroutine setup_nested_boundary_halo(Atm, proc_in)

    !This routine is now taking the "easy way out" with regards
    ! to pt (virtual potential temperature), q_con, and cappa;
    ! their halo values are now set up when the BCs are set up
    ! in fv_dynamics

    type(fv_atmos_type), intent(INOUT) :: Atm
    logical, INTENT(IN), OPTIONAL :: proc_in
    real, allocatable :: g_dat(:,:,:), g_dat2(:,:,:)
    real, allocatable :: pt_coarse(:,:,:)
    integer i,j,k,nq, sphum, ncnst, istart, iend, npz, nwat
    integer isc, iec, jsc, jec, isd, ied, jsd, jed, is, ie, js, je
    integer isd_p, ied_p, jsd_p, jed_p, isc_p, iec_p, jsc_p, jec_p, isg, ieg, jsg,jeg, npx_p, npy_p
    real zvir
    logical process
    integer :: liq_wat, ice_wat, rainwat, snowwat, graupel
    real :: qv, dp1, q_liq, q_sol, q_con, cvm, cappa, dp, pt, dz, pkz, rdg

    if (PRESENT(proc_in)) then
       process = proc_in
    else
       process = .true.
    endif

    isd = Atm%bd%isd
    ied = Atm%bd%ied
    jsd = Atm%bd%jsd
    jed = Atm%bd%jed
    ncnst = Atm%ncnst
    isc = Atm%bd%isc; iec = Atm%bd%iec; jsc = Atm%bd%jsc; jec = Atm%bd%jec
    is  = Atm%bd%is ; ie  = Atm%bd%ie ; js  = Atm%bd%js ; je  = Atm%bd%je
    npz     = Atm%npz    
    nwat = Atm%flagstruct%nwat

   if (nwat>=3 ) then
      liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
      ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
   endif
   if ( nwat==6 ) then
      rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
      snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
      graupel = get_tracer_index (MODEL_ATMOS, 'graupel')
   endif

    call mpp_get_data_domain( Atm%parent_grid%domain, &
         isd_p,  ied_p,  jsd_p,  jed_p  )
    call mpp_get_compute_domain( Atm%parent_grid%domain, &
         isc_p,  iec_p,  jsc_p,  jec_p  )
    call mpp_get_global_domain( Atm%parent_grid%domain, &
         isg, ieg, jsg, jeg, xsize=npx_p, ysize=npy_p)

    call nested_grid_BC(Atm%delp, Atm%parent_grid%delp, Atm%neststruct%nest_domain, &
         Atm%neststruct%ind_h, Atm%neststruct%wt_h, 0, 0, &
         Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)
    do nq=1,ncnst
       call nested_grid_BC(Atm%q(:,:,:,nq), &
            Atm%parent_grid%q(:,:,:,nq), Atm%neststruct%nest_domain, &
            Atm%neststruct%ind_h, Atm%neststruct%wt_h, 0, 0, &
            Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)
    end do

    if (process) then
       if (is_master()) print*, 'FILLING NESTED GRID HALO'
    else
       if (is_master()) print*, 'SENDING DATA TO FILL NESTED GRID HALO'
    endif


    !Filling phis?
    !In idealized test cases, where the topography is EXACTLY known (ex case 13),
    !interpolating the topography yields a much worse result. In comparison in
    !real topography cases little difference is seen.

    !This is probably because the halo phis, which is used to compute
    !geopotential height (gz, gh), only affects the interior by being
    !used to compute corner gz in a2b_ord[24]. We might suppose this
    !computation would be more accurate when using values of phis which
    !are more consistent with those on the interior (ie the exactly-known
    !values) than the crude values given through linear interpolation.

    !For real topography cases, or in cases in which the coarse-grid topo
    ! is smoothed, we fill the boundary halo with the coarse-grid topo.

#ifndef SW_DYNAMICS
    !pt --- actually temperature

    call nested_grid_BC(Atm%pt, Atm%parent_grid%pt, Atm%neststruct%nest_domain, &
         Atm%neststruct%ind_h, Atm%neststruct%wt_h, 0, 0, &
         Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)    

    if (.not. Atm%flagstruct%hydrostatic) then

       !w
       call nested_grid_BC(Atm%w(:,:,:), &
            Atm%parent_grid%w(:,:,:), &
            Atm%neststruct%nest_domain, Atm%neststruct%ind_h, Atm%neststruct%wt_h, 0, 0, &
            Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)


       !delz
       call nested_grid_BC(Atm%delz(:,:,:), &
            Atm%parent_grid%delz(:,:,:), &
            Atm%neststruct%nest_domain, Atm%neststruct%ind_h, Atm%neststruct%wt_h, 0, 0, &
            Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)

    end if

#endif

    if (Atm%neststruct%child_proc) then
       call nested_grid_BC(Atm%u, Atm%parent_grid%u(:,:,:), &
            Atm%neststruct%nest_domain, Atm%neststruct%ind_u, Atm%neststruct%wt_u, 0, 1, &
            Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)
       call nested_grid_BC(Atm%v, Atm%parent_grid%v(:,:,:), &
            Atm%neststruct%nest_domain, Atm%neststruct%ind_v, Atm%neststruct%wt_v, 1, 0, &
            Atm%npx, Atm%npy, npz, Atm%bd, isg, ieg, jsg, jeg, proc_in=process)
    else
       call nested_grid_BC(Atm%parent_grid%u(:,:,:), &
            Atm%neststruct%nest_domain, 0, 1)
       call nested_grid_BC(Atm%parent_grid%v(:,:,:), &
            Atm%neststruct%nest_domain, 1, 0)
    endif


    if (process) then
!!$#ifdef SW_DYNAMICS
!!$    !ps: first level only
!!$    !This is only valid for shallow-water simulations
!!$    do j=jsd,jed
!!$       do i=isd,ied
!!$
!!$          Atm%ps(i,j) = Atm%delp(i,j,1)/grav
!!$
!!$       end do
!!$    end do
!!$#endif
       call mpp_update_domains(Atm%u, Atm%v, Atm%domain, gridtype=DGRID_NE)
       call mpp_update_domains(Atm%w, Atm%domain, complete=.true.) ! needs an update-domain for rayleigh damping
      endif

      call mpp_sync_self()

  end subroutine setup_nested_boundary_halo

  subroutine fill_nested_grid_topo_halo(Atm, proc_in)

    type(fv_atmos_type), intent(INOUT) :: Atm
    logical, intent(IN), OPTIONAL :: proc_in
    integer :: isg, ieg, jsg, jeg

    if (.not. Atm%neststruct%nested) return

    call mpp_get_global_domain( Atm%parent_grid%domain, &
         isg, ieg, jsg, jeg)

    if (is_master()) print*, '  FILLING NESTED GRID HALO WITH INTERPOLATED TERRAIN'
    call nested_grid_BC(Atm%phis, Atm%parent_grid%phis, Atm%neststruct%nest_domain, &
         Atm%neststruct%ind_h, Atm%neststruct%wt_h, 0, 0, &
         Atm%npx, Atm%npy, Atm%bd, isg, ieg, jsg, jeg, proc_in=proc_in)
    
  end subroutine fill_nested_grid_topo_halo

!!! We call this routine to fill the nested grid with topo so that we can do the boundary smoothing.
!!! Interior topography is then over-written in get_external_ic.
  subroutine fill_nested_grid_topo(Atm, proc_in) 

    type(fv_atmos_type), intent(INOUT) :: Atm
    logical, intent(IN), OPTIONAL :: proc_in
    real, allocatable :: g_dat(:,:,:)
    integer :: p, sending_proc
    integer :: isd_p, ied_p, jsd_p, jed_p
    integer :: isg, ieg, jsg,jeg

    logical :: process

    process = .true.
    if (present(proc_in)) then
       process = proc_in
    else
       process = .true.
    endif

!!$    if (.not. Atm%neststruct%nested) return

    call mpp_get_global_domain( Atm%parent_grid%domain, &
         isg, ieg, jsg, jeg)
    call mpp_get_data_domain( Atm%parent_grid%domain, &
         isd_p,  ied_p,  jsd_p,  jed_p  )

    allocate(g_dat( isg:ieg, jsg:jeg, 1) )
    call timing_on('COMM_TOTAL')

    !!! FIXME: For whatever reason this code CRASHES if the lower-left corner
    !!!        of the nested grid lies within the first PE of a grid tile.

    if (is_master() .and. .not. Atm%flagstruct%external_ic ) print*, ' FILLING NESTED GRID INTERIOR WITH INTERPOLATED TERRAIN'

    sending_proc = Atm%parent_grid%pelist(1) + (Atm%neststruct%parent_tile-1)*Atm%parent_grid%npes_per_tile
    if (Atm%neststruct%parent_proc .and. Atm%neststruct%parent_tile == Atm%parent_grid%tile) then
       call mpp_global_field( &
            Atm%parent_grid%domain, &
            Atm%parent_grid%phis(isd_p:ied_p,jsd_p:jed_p), g_dat(isg:,jsg:,1), position=CENTER)
       if (mpp_pe() == sending_proc) then 
          do p=1,size(Atm%pelist)
             call mpp_send(g_dat,size(g_dat),Atm%pelist(p))
          enddo
       endif
    endif

    if (ANY(Atm%pelist == mpp_pe())) then
       call mpp_recv(g_dat, size(g_dat), sending_proc)
    endif

    call timing_off('COMM_TOTAL')
    if (process) call fill_nested_grid(Atm%phis, g_dat(isg:,jsg:,1), &
         Atm%neststruct%ind_h, Atm%neststruct%wt_h, &
         0, 0,  isg, ieg, jsg, jeg, Atm%bd)

    call mpp_sync_self

    deallocate(g_dat)


  end subroutine fill_nested_grid_topo

  subroutine fill_nested_grid_data(Atm, proc_in)

    type(fv_atmos_type), intent(INOUT) :: Atm(:) !Only intended to be one element; needed for cubed_sphere_terrain
    logical, intent(IN), OPTIONAL :: proc_in
    real, allocatable :: g_dat(:,:,:), pt_coarse(:,:,:)
    integer :: i,j,k,nq, sphum, ncnst, istart, iend, npz
    integer :: isc, iec, jsc, jec, isd, ied, jsd, jed
    integer :: isd_p, ied_p, jsd_p, jed_p, isc_p, iec_p, jsc_p, jec_p
    integer :: isg, ieg, jsg,jeg, npx_p, npy_p
    integer :: isg_n, ieg_n, jsg_n, jeg_n, npx_n, npy_n
    real zvir, gh0, p1(2), p2(2), r, r0

    integer :: p, sending_proc, gid
    logical process

    if (present(proc_in)) then
       process = proc_in
    else
       process = .true.
    endif

    isd = Atm(1)%bd%isd
    ied = Atm(1)%bd%ied
    jsd = Atm(1)%bd%jsd
    jed = Atm(1)%bd%jed
    ncnst = Atm(1)%ncnst
    isc = Atm(1)%bd%isc; iec = Atm(1)%bd%iec; jsc = Atm(1)%bd%jsc; jec = Atm(1)%bd%jec
    npz     = Atm(1)%npz    
    
    gid = mpp_pe()

    sending_proc = Atm(1)%parent_grid%pelist(1) + (Atm(1)%neststruct%parent_tile-1)*Atm(1)%parent_grid%npes_per_tile

       call mpp_get_data_domain( Atm(1)%parent_grid%domain, &
            isd_p,  ied_p,  jsd_p,  jed_p  )
       call mpp_get_compute_domain( Atm(1)%parent_grid%domain, &
            isc_p,  iec_p,  jsc_p,  jec_p  )
    call mpp_get_global_domain( Atm(1)%parent_grid%domain, &
         isg, ieg, jsg, jeg, xsize=npx_p, ysize=npy_p)

    if (process) then 
       
       call mpp_error(NOTE, "FILLING NESTED GRID DATA")

    else

       call mpp_error(NOTE, "SENDING TO FILL NESTED GRID DATA")

    endif

    !delp

    allocate(g_dat( isg:ieg, jsg:jeg, npz) )

    call timing_on('COMM_TOTAL')

    !Call mpp_global_field on the procs that have the required data.
       !Then broadcast from the head PE to the receiving PEs
       if (Atm(1)%neststruct%parent_proc .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
          call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%delp(isd_p:ied_p,jsd_p:jed_p,:), g_dat, position=CENTER)
          if (gid == sending_proc) then !crazy logic but what we have for now
             do p=1,size(Atm(1)%pelist)
                call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
             enddo
          endif
       endif
       if (ANY(Atm(1)%pelist == gid)) then
          call mpp_recv(g_dat, size(g_dat), sending_proc)
       endif

    call timing_off('COMM_TOTAL')
    if (process) call fill_nested_grid(Atm(1)%delp, g_dat, &
         Atm(1)%neststruct%ind_h, Atm(1)%neststruct%wt_h, &
         0, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)

    call mpp_sync_self

    !tracers
    do nq=1,ncnst

       call timing_on('COMM_TOTAL')

          if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%q(isd_p:ied_p,jsd_p:jed_p,:,nq), g_dat, position=CENTER)
             if (gid == sending_proc) then
                do p=1,size(Atm(1)%pelist)
                   call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
                enddo
             endif
          endif
          if (ANY(Atm(1)%pelist == gid)) then
             call mpp_recv(g_dat, size(g_dat), sending_proc)
          endif

       call timing_off('COMM_TOTAL')
       if (process) call fill_nested_grid(Atm(1)%q(isd:ied,jsd:jed,:,nq), g_dat, &
            Atm(1)%neststruct%ind_h, Atm(1)%neststruct%wt_h, &
            0, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)

    call mpp_sync_self

    end do

    !Note that we do NOT fill in phis (surface geopotential), which should 
    !be computed exactly instead of being interpolated.


#ifndef SW_DYNAMICS
    !pt --- actually temperature

    call timing_on('COMM_TOTAL')

       if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%pt(isd_p:ied_p,jsd_p:jed_p,:), g_dat, position=CENTER)
          if (gid == sending_proc) then
             do p=1,size(Atm(1)%pelist)
                call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
             enddo
          endif
       endif
       if (ANY(Atm(1)%pelist == gid)) then
          call mpp_recv(g_dat, size(g_dat), sending_proc)
       endif

    call mpp_sync_self

    call timing_off('COMM_TOTAL')
    if (process) call fill_nested_grid(Atm(1)%pt, g_dat, &
         Atm(1)%neststruct%ind_h, Atm(1)%neststruct%wt_h, &
         0, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)


    if ( Atm(1)%flagstruct%nwat > 0 ) then
       sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
    else
       sphum = 1
    endif
    if ( Atm(1)%parent_grid%flagstruct%adiabatic .or. Atm(1)%parent_grid%flagstruct%do_Held_Suarez ) then
       zvir = 0.         ! no virtual effect
    else
       zvir = rvgas/rdgas - 1.
    endif

    call timing_on('COMM_TOTAL')

       if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%pkz(isc_p:iec_p,jsc_p:jec_p,:), g_dat, position=CENTER)
          if (gid == sending_proc) then
             do p=1,size(Atm(1)%pelist)
                call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
             enddo
          endif
       endif
       if (ANY(Atm(1)%pelist == gid)) then
          call mpp_recv(g_dat, size(g_dat), sending_proc)
       endif

    call mpp_sync_self

    call timing_off('COMM_TOTAL')
    if (process) then 
       allocate(pt_coarse(isd:ied,jsd:jed,npz))
       call fill_nested_grid(pt_coarse, g_dat, &
            Atm(1)%neststruct%ind_h, Atm(1)%neststruct%wt_h, &
            0, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)

       if (Atm(1)%bd%is == 1) then
          do k=1,npz
             do j=Atm(1)%bd%jsd,Atm(1)%bd%jed
                do i=Atm(1)%bd%isd,0
                   Atm(1)%pt(i,j,k) = cp_air*Atm(1)%pt(i,j,k)/pt_coarse(i,j,k)*(1.+zvir*Atm(1)%q(i,j,k,sphum))
                end do
             end do
          end do
       end if

       if (Atm(1)%bd%js == 1) then
          if (Atm(1)%bd%is == 1) then
             istart = Atm(1)%bd%is
          else
             istart = Atm(1)%bd%isd
          end if
          if (Atm(1)%bd%ie == Atm(1)%npx-1) then
             iend = Atm(1)%bd%ie
          else
             iend = Atm(1)%bd%ied
          end if

          do k=1,npz
             do j=Atm(1)%bd%jsd,0
                do i=istart,iend
                   Atm(1)%pt(i,j,k) = cp_air*Atm(1)%pt(i,j,k)/pt_coarse(i,j,k)*(1.+zvir*Atm(1)%q(i,j,k,sphum))
                end do
             end do
          end do
       end if

       if (Atm(1)%bd%ie == Atm(1)%npx-1) then
          do k=1,npz
             do j=Atm(1)%bd%jsd,Atm(1)%bd%jed
                do i=Atm(1)%npx,Atm(1)%bd%ied
                   Atm(1)%pt(i,j,k) = cp_air*Atm(1)%pt(i,j,k)/pt_coarse(i,j,k)*(1.+zvir*Atm(1)%q(i,j,k,sphum))
                end do
             end do
          end do
       end if

       if (Atm(1)%bd%je == Atm(1)%npy-1) then
          if (Atm(1)%bd%is == 1) then
             istart = Atm(1)%bd%is
          else
             istart = Atm(1)%bd%isd
          end if
          if (Atm(1)%bd%ie == Atm(1)%npx-1) then
             iend = Atm(1)%bd%ie
          else
             iend = Atm(1)%bd%ied
          end if

          do k=1,npz
             do j=Atm(1)%npy,Atm(1)%bd%jed
                do i=istart,iend
                   Atm(1)%pt(i,j,k) = cp_air*Atm(1)%pt(i,j,k)/pt_coarse(i,j,k)*(1.+zvir*Atm(1)%q(i,j,k,sphum))
                end do
             end do
          end do
       end if

       deallocate(pt_coarse)

    end if

    if (.not. Atm(1)%flagstruct%hydrostatic) then

       !delz
       call timing_on('COMM_TOTAL')

          if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%delz(isd_p:ied_p,jsd_p:jed_p,:), g_dat, position=CENTER)
             if (gid == sending_proc) then
                do p=1,size(Atm(1)%pelist)
                   call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
                enddo
             endif
          endif
          if (ANY(Atm(1)%pelist == gid)) then
             call mpp_recv(g_dat, size(g_dat), sending_proc)
          endif

    call mpp_sync_self

       call timing_off('COMM_TOTAL')
       if (process) call fill_nested_grid(Atm(1)%delz, g_dat, &
            Atm(1)%neststruct%ind_h, Atm(1)%neststruct%wt_h, &
            0, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)

       !w

       call timing_on('COMM_TOTAL')

          if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%w(isd_p:ied_p,jsd_p:jed_p,:), g_dat, position=CENTER)
             if (gid == sending_proc) then
                do p=1,size(Atm(1)%pelist)
                   call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
                enddo
             endif
          endif
          if (ANY(Atm(1)%pelist == gid)) then
             call mpp_recv(g_dat, size(g_dat), sending_proc)
          endif

    call mpp_sync_self

       call timing_off('COMM_TOTAL')
       if (process) call fill_nested_grid(Atm(1)%w, g_dat, &
            Atm(1)%neststruct%ind_h, Atm(1)%neststruct%wt_h, &
            0, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)
       !

    end if

#endif
    deallocate(g_dat) 

    !u

    allocate(g_dat( isg:ieg, jsg:jeg+1, npz) )
    g_dat = 1.e25

    call timing_on('COMM_TOTAL')

       if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%u(isd_p:ied_p,jsd_p:jed_p+1,:), g_dat, position=NORTH)
          if (gid == sending_proc) then
             do p=1,size(Atm(1)%pelist)
                call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
             enddo
          endif
       endif
       if (ANY(Atm(1)%pelist == gid)) then
          call mpp_recv(g_dat, size(g_dat), sending_proc)
       endif

    call mpp_sync_self

    call timing_off('COMM_TOTAL')
    call mpp_sync_self
    if (process) call fill_nested_grid(Atm(1)%u, g_dat, &
         Atm(1)%neststruct%ind_u, Atm(1)%neststruct%wt_u, &
         0, 1,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)
    deallocate(g_dat)

    !v

    allocate(g_dat( isg:ieg+1, jsg:jeg, npz) )
    g_dat = 1.e25

    call timing_on('COMM_TOTAL')

       if (ANY(Atm(1)%parent_grid%pelist == gid) .and. Atm(1)%neststruct%parent_tile == Atm(1)%parent_grid%tile) then
             call mpp_global_field( &
               Atm(1)%parent_grid%domain, &
               Atm(1)%parent_grid%v(isd_p:ied_p+1,jsd_p:jed_p,:), g_dat, position=EAST)
          if (gid == sending_proc) then
             do p=1,size(Atm(1)%pelist)
                call mpp_send(g_dat,size(g_dat),Atm(1)%pelist(p))
             enddo
          endif
       endif
       if (ANY(Atm(1)%pelist == gid)) then
          call mpp_recv(g_dat, size(g_dat), sending_proc)
       endif

    call mpp_sync_self
                                      call timing_off('COMM_TOTAL')

    if (process) call fill_nested_grid(Atm(1)%v, g_dat, &
         Atm(1)%neststruct%ind_v, Atm(1)%neststruct%wt_v, &
         1, 0,  isg, ieg, jsg, jeg, npz, Atm(1)%bd)

    deallocate(g_dat)

  end subroutine fill_nested_grid_data

  subroutine fill_nested_grid_data_end(Atm, proc_in)

    type(fv_atmos_type), intent(INOUT) :: Atm  
    logical, intent(IN), OPTIONAL :: proc_in
    real, allocatable :: g_dat(:,:,:), pt_coarse(:,:,:)
    integer :: i,j,k,nq, sphum, ncnst, istart, iend, npz
    integer :: isc, iec, jsc, jec, isd, ied, jsd, jed
    integer :: isd_p, ied_p, jsd_p, jed_p, isc_p, iec_p, jsc_p, jec_p
    integer :: isg, ieg, jsg,jeg, npx_p, npy_p
    integer :: isg_n, ieg_n, jsg_n, jeg_n, npx_n, npy_n
    real zvir

    integer :: p , sending_proc
    logical :: process

    if (present(proc_in)) then
       process = proc_in
    else
       process = .true.
    endif

    isd = Atm%bd%isd
    ied = Atm%bd%ied
    jsd = Atm%bd%jsd
    jed = Atm%bd%jed
    ncnst = Atm%ncnst
    isc = Atm%bd%isc; iec = Atm%bd%iec; jsc = Atm%bd%jsc; jec = Atm%bd%jec
    npz     = Atm%npz    
    
          isd_p = Atm%parent_grid%bd%isd
          ied_p = Atm%parent_grid%bd%ied
          jsd_p = Atm%parent_grid%bd%jsd
          jed_p = Atm%parent_grid%bd%jed
          isc_p = Atm%parent_grid%bd%isc
          iec_p = Atm%parent_grid%bd%iec
          jsc_p = Atm%parent_grid%bd%jsc
          jec_p = Atm%parent_grid%bd%jec
       sending_proc = Atm%parent_grid%pelist(1) + (Atm%neststruct%parent_tile-1)*Atm%parent_grid%npes_per_tile

    call mpp_get_global_domain( Atm%parent_grid%domain, &
         isg, ieg, jsg, jeg, xsize=npx_p, ysize=npy_p)


    !NOW: what we do is to update the nested-grid terrain to the coarse grid,
    !to ensure consistency between the two grids.
    if ( process ) call mpp_update_domains(Atm%phis, Atm%domain, complete=.true.)
    if (Atm%neststruct%twowaynest) then
       if (ANY(Atm%parent_grid%pelist == mpp_pe()) .or. Atm%neststruct%child_proc) then
          call update_coarse_grid(Atm%parent_grid%phis, &
               Atm%phis, Atm%neststruct%nest_domain, &
               Atm%neststruct%ind_update_h(isd_p:ied_p+1,jsd_p:jed_p+1,:), &
               Atm%gridstruct%dx, Atm%gridstruct%dy, Atm%gridstruct%area, &
               isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
               Atm%neststruct%isu, Atm%neststruct%ieu, Atm%neststruct%jsu, Atm%neststruct%jeu, &
               Atm%npx, Atm%npy, 0, 0, &
               Atm%neststruct%refinement, Atm%neststruct%nestupdate, 0, 0, &
               Atm%neststruct%parent_proc, Atm%neststruct%child_proc, Atm%parent_grid)
          Atm%parent_grid%neststruct%parent_of_twoway = .true.
          !NOTE: mpp_update_nest_coarse (and by extension, update_coarse_grid) does **NOT** pass data
          !allowing a two-way update into the halo of the coarse grid. It only passes data so that the INTERIOR
          ! can have the two-way update. Thus, on the nest's cold start, if this update_domains call is not done,
          ! the coarse grid will have the wrong topography in the halo, which will CHANGE when a restart is done!!
          if (Atm%neststruct%parent_proc) call mpp_update_domains(Atm%parent_grid%phis, Atm%parent_grid%domain)
       end if

    end if




#ifdef SW_DYNAMICS
!!$    !ps: first level only
!!$    !This is only valid for shallow-water simulations
!!$    if (process) then
!!$    do j=jsd,jed
!!$       do i=isd,ied
!!$
!!$          Atm%ps(i,j) = Atm%delp(i,j,1)/grav
!!$
!!$       end do
!!$    end do
!!$    endif
#else
    !Sets up flow to be initially hydrostatic (shouldn't be the case for all ICs?)
    if (process) call p_var(npz, isc, iec, jsc, jec, Atm%ptop, ptop_min, Atm%delp, &
         Atm%delz, Atm%pt, Atm%ps,   &
         Atm%pe, Atm%peln, Atm%pk, Atm%pkz, kappa, Atm%q, &
         Atm%ng, ncnst, Atm%gridstruct%area_64, Atm%flagstruct%dry_mass, .false., Atm%flagstruct%mountain, &
         Atm%flagstruct%moist_phys, .true., Atm%flagstruct%nwat, Atm%domain)
#endif

 

  end subroutine fill_nested_grid_data_end


  !#######################################################################
  ! <SUBROUTINE NAME="fv_write_restart">
  ! <DESCRIPTION>
  !  Write out restart files registered through register_restart_file
  ! </DESCRIPTION>
  subroutine fv_write_restart(Atm, grids_on_this_pe, timestamp)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    character(len=*),    intent(in)    :: timestamp
    logical, intent(IN) :: grids_on_this_pe(:)
    integer n

    call fv_io_write_restart(Atm, grids_on_this_pe, timestamp)
    do n=1,size(Atm)
       if (Atm(n)%neststruct%nested .and. grids_on_this_pe(n)) then
          call fv_io_write_BCs(Atm(n))
       endif
    enddo

  end subroutine fv_write_restart
  ! </SUBROUTINE>



  !#####################################################################
  ! <SUBROUTINE NAME="fv_restart_end">
  !
  ! <DESCRIPTION>
  ! Initialize the fv core restart facilities
  ! </DESCRIPTION>
  !
  subroutine fv_restart_end(Atm, grids_on_this_pe)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    logical, intent(INOUT) :: grids_on_this_pe(:)

    integer :: isc, iec, jsc, jec
    integer :: iq, n, ntileMe, ncnst, ntprog, ntdiag
    integer :: isd, ied, jsd, jed, npz
    integer :: unit
    integer :: file_unit
    integer, allocatable :: pelist(:)
    character(len=128):: tracer_name
    character(len=3):: gn


    ntileMe = size(Atm(:))

    do n = 1, ntileMe

      if (.not. grids_on_this_pe(n)) then
         cycle
      endif

      call mpp_set_current_pelist(Atm(n)%pelist)

      isc = Atm(n)%bd%isc; iec = Atm(n)%bd%iec; jsc = Atm(n)%bd%jsc; jec = Atm(n)%bd%jec

      isd = Atm(n)%bd%isd
      ied = Atm(n)%bd%ied
      jsd = Atm(n)%bd%jsd
      jed = Atm(n)%bd%jed
      npz = Atm(n)%npz
      ncnst = Atm(n)%ncnst
      ntprog = size(Atm(n)%q,4)
      ntdiag = size(Atm(n)%qdiag,4)

      if (Atm(n)%grid_number > 1) then
         write(gn,'(A2, I1)') " g", Atm(n)%grid_number
      else
         gn = ''
      end if

      unit = stdout()
      write(unit,*)
      write(unit,*) 'fv_restart_end u   ', trim(gn),' = ', mpp_chksum(Atm(n)%u(isc:iec,jsc:jec,:))
      write(unit,*) 'fv_restart_end v   ', trim(gn),' = ', mpp_chksum(Atm(n)%v(isc:iec,jsc:jec,:))
      if ( .not. Atm(n)%flagstruct%hydrostatic )    &
         write(unit,*) 'fv_restart_end w   ', trim(gn),' = ', mpp_chksum(Atm(n)%w(isc:iec,jsc:jec,:))
      write(unit,*) 'fv_restart_end delp', trim(gn),' = ', mpp_chksum(Atm(n)%delp(isc:iec,jsc:jec,:))
      write(unit,*) 'fv_restart_end phis', trim(gn),' = ', mpp_chksum(Atm(n)%phis(isc:iec,jsc:jec))
#ifndef SW_DYNAMICS
      write(unit,*) 'fv_restart_end pt  ', trim(gn),' = ', mpp_chksum(Atm(n)%pt(isc:iec,jsc:jec,:))
      if (ntprog>0) &
           write(unit,*) 'fv_restart_end q(prog) nq  ', trim(gn),' =',ntprog, mpp_chksum(Atm(n)%q(isc:iec,jsc:jec,:,:))
      if (ntdiag>0) &
           write(unit,*) 'fv_restart_end q(diag) nq  ', trim(gn),' =',ntdiag, mpp_chksum(Atm(n)%qdiag(isc:iec,jsc:jec,:,:))
      do iq=1,min(7, ntprog)     ! Check up to 7 tracers
        call get_tracer_names(MODEL_ATMOS, iq, tracer_name)
        write(unit,*) 'fv_restart_end '//trim(tracer_name)// trim(gn),' = ', mpp_chksum(Atm(n)%q(isc:iec,jsc:jec,:,iq))
      enddo

!---------------
! Check Min/Max:
!---------------
!     call prt_maxmin('ZS', Atm(n)%phis, isc, iec, jsc, jec, Atm(n)%ng, 1, 1./grav)
      call pmaxmn_g('ZS', Atm(n)%phis, isc, iec, jsc, jec, 1, 1./grav, Atm(n)%gridstruct%area_64, Atm(n)%domain)
      call pmaxmn_g('PS ', Atm(n)%ps,   isc, iec, jsc, jec, 1, 0.01   , Atm(n)%gridstruct%area_64, Atm(n)%domain)
      call prt_maxmin('PS*', Atm(n)%ps, isc, iec, jsc, jec, Atm(n)%ng, 1, 0.01)
      call prt_maxmin('U ', Atm(n)%u(isd:ied,jsd:jed,1:npz), isc, iec, jsc, jec, Atm(n)%ng, npz, 1.)
      call prt_maxmin('V ', Atm(n)%v(isd:ied,jsd:jed,1:npz), isc, iec, jsc, jec, Atm(n)%ng, npz, 1.)
      if ( .not. Atm(n)%flagstruct%hydrostatic )    &
      call prt_maxmin('W ', Atm(n)%w , isc, iec, jsc, jec, Atm(n)%ng, npz, 1.)
      call prt_maxmin('T ', Atm(n)%pt, isc, iec, jsc, jec, Atm(n)%ng, npz, 1.)
      do iq=1, ntprog
          call get_tracer_names ( MODEL_ATMOS, iq, tracer_name )
          call pmaxmn_g(trim(tracer_name), Atm(n)%q(isd:ied,jsd:jed,1:npz,iq:iq), isc, iec, jsc, jec, npz, &
                        1., Atm(n)%gridstruct%area_64, Atm(n)%domain)
      enddo
! Write4 energy correction term
#endif

!! JHC
!     if (use_ncep_sst .or. Atm(1)%flagstruct%nudge) call fv_io_register_nudge_restart(Atm)


   enddo

   call fv_io_write_restart(Atm, grids_on_this_pe)
   do n=1,ntileMe
      if (Atm(n)%neststruct%nested .and. grids_on_this_pe(n)) call fv_io_write_BCs(Atm(n))
    end do

    module_is_initialized = .FALSE.

#ifdef EFLUX_OUT
    if( is_master() ) then
        write(*,*) steps, 'Mean equivalent Heat flux for this integration period=',Atm(1)%idiag%efx_sum/real(max(1,Atm(1)%idiag%steps)), &
                          'Mean nesting-related flux for this integration period=',Atm(1)%idiag%efx_sum_nest/real(max(1,Atm(1)%idiag%steps)), &
                          'Mean mountain torque=',Atm(1)%idiag%mtq_sum/real(max(1,Atm(1)%idiag%steps))
        file_unit = get_unit()
        open (unit=file_unit, file='e_flux.data', form='unformatted',status='unknown', access='sequential')
        do n=1,steps
           write(file_unit) Atm(1)%idiag%efx(n)
           write(file_unit) Atm(1)%idiag%mtq(n)    ! time series global mountain torque
           !write(file_unit) Atm(1)%idiag%efx_nest(n)  
        enddo
        close(unit=file_unit)
    endif
#endif

  end subroutine fv_restart_end
  ! </SUBROUTINE> NAME="fv_restart_end"

 subroutine d2c_setup(u, v, &
      ua, va, &
	  uc, vc, dord4, &
      isd,ied,jsd,jed, is,ie,js,je, npx,npy, &
      grid_type, nested, &
      se_corner, sw_corner, ne_corner, nw_corner, &
      rsin_u,rsin_v,cosa_s,rsin2 )

  logical, intent(in):: dord4
  real, intent(in) ::  u(isd:ied,jsd:jed+1)
  real, intent(in) ::  v(isd:ied+1,jsd:jed)
  real, intent(out), dimension(isd:ied  ,jsd:jed  ):: ua
  real, intent(out), dimension(isd:ied  ,jsd:jed  ):: va
  real, intent(out), dimension(isd:ied+1,jsd:jed  ):: uc
  real, intent(out), dimension(isd:ied  ,jsd:jed+1):: vc
  integer, intent(in) :: isd,ied,jsd,jed, is,ie,js,je, npx,npy,grid_type
  logical, intent(in) :: nested, se_corner, sw_corner, ne_corner, nw_corner
  real, intent(in) :: rsin_u(isd:ied+1,jsd:jed)
  real, intent(in) :: rsin_v(isd:ied,jsd:jed+1)
  real, intent(in) :: cosa_s(isd:ied,jsd:jed)
  real, intent(in) :: rsin2(isd:ied,jsd:jed)

! Local 
  real, dimension(isd:ied,jsd:jed):: utmp, vtmp
  real, parameter:: t11=27./28., t12=-13./28., t13=3./7., t14=6./7., t15=3./28.
  real, parameter:: a1 =  0.5625
  real, parameter:: a2 = -0.0625
  real, parameter:: c1 = -2./14.
  real, parameter:: c2 = 11./14.
  real, parameter:: c3 =  5./14.
  integer npt, i, j, ifirst, ilast, id

  if ( dord4) then
       id = 1
  else
       id = 0
  endif


  if (grid_type < 3 .and. .not. nested) then
     npt = 4
  else
     npt = -2
  endif

  if ( nested) then  

     do j=jsd+1,jed-1
        do i=isd,ied
           utmp(i,j) = a2*(u(i,j-1)+u(i,j+2)) + a1*(u(i,j)+u(i,j+1))
        enddo
     enddo
     do i=isd,ied
        j = jsd
        utmp(i,j) = 0.5*(u(i,j)+u(i,j+1))
        j = jed
        utmp(i,j) = 0.5*(u(i,j)+u(i,j+1))
     end do

     do j=jsd,jed
        do i=isd+1,ied-1
           vtmp(i,j) = a2*(v(i-1,j)+v(i+2,j)) + a1*(v(i,j)+v(i+1,j))
        enddo
        i = isd
        vtmp(i,j) = 0.5*(v(i,j)+v(i+1,j)) 
        i = ied
        vtmp(i,j) = 0.5*(v(i,j)+v(i+1,j))
     enddo

     do j=jsd,jed
        do i=isd,ied
           ua(i,j) = (utmp(i,j)-vtmp(i,j)*cosa_s(i,j)) * rsin2(i,j)
           va(i,j) = (vtmp(i,j)-utmp(i,j)*cosa_s(i,j)) * rsin2(i,j)
        enddo
     enddo

  else

     !----------
     ! Interior:
     !----------
     utmp = 0.
     vtmp = 0.


     do j=max(npt,js-1),min(npy-npt,je+1)
        do i=max(npt,isd),min(npx-npt,ied)
           utmp(i,j) = a2*(u(i,j-1)+u(i,j+2)) + a1*(u(i,j)+u(i,j+1))
        enddo
     enddo
     do j=max(npt,jsd),min(npy-npt,jed)
        do i=max(npt,is-1),min(npx-npt,ie+1)
           vtmp(i,j) = a2*(v(i-1,j)+v(i+2,j)) + a1*(v(i,j)+v(i+1,j))
        enddo
     enddo

     !----------
     ! edges:
     !----------
     if (grid_type < 3) then

        if ( js==1 .or. jsd<npt) then
           do j=jsd,npt-1
              do i=isd,ied
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

        if ( (je+1)==npy .or. jed>=(npy-npt)) then
           do j=npy-npt+1,jed
              do i=isd,ied
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

        if ( is==1 .or. isd<npt ) then
           do j=max(npt,jsd),min(npy-npt,jed)
              do i=isd,npt-1
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

        if ( (ie+1)==npx .or. ied>=(npx-npt)) then
           do j=max(npt,jsd),min(npy-npt,jed)
              do i=npx-npt+1,ied
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

     endif
     do j=js-1-id,je+1+id
        do i=is-1-id,ie+1+id
           ua(i,j) = (utmp(i,j)-vtmp(i,j)*cosa_s(i,j)) * rsin2(i,j)
           va(i,j) = (vtmp(i,j)-utmp(i,j)*cosa_s(i,j)) * rsin2(i,j)
        enddo
     enddo

  end if

! A -> C
!--------------
! Fix the edges
!--------------
! Xdir:
     if( sw_corner ) then
         do i=-2,0
            utmp(i,0) = -vtmp(0,1-i)
         enddo
     endif
     if( se_corner ) then
         do i=0,2
            utmp(npx+i,0) = vtmp(npx,i+1)
         enddo
     endif
     if( ne_corner ) then
         do i=0,2
            utmp(npx+i,npy) = -vtmp(npx,je-i)
         enddo
     endif
     if( nw_corner ) then
         do i=-2,0
            utmp(i,npy) = vtmp(0,je+i)
         enddo
     endif

  if (grid_type < 3 .and. .not. nested) then
     ifirst = max(3,    is-1)
     ilast  = min(npx-2,ie+2)
  else
     ifirst = is-1
     ilast  = ie+2
  endif
!---------------------------------------------
! 4th order interpolation for interior points:
!---------------------------------------------
     do j=js-1,je+1
        do i=ifirst,ilast
           uc(i,j) = a1*(utmp(i-1,j)+utmp(i,j))+a2*(utmp(i-2,j)+utmp(i+1,j))
        enddo
     enddo

     if (grid_type < 3) then
! Xdir:
     if( is==1 .and. .not. nested ) then
        do j=js-1,je+1
           uc(0,j) = c1*utmp(-2,j) + c2*utmp(-1,j) + c3*utmp(0,j) 
           uc(1,j) = ( t14*(utmp( 0,j)+utmp(1,j))    &
                     + t12*(utmp(-1,j)+utmp(2,j))    &
                     + t15*(utmp(-2,j)+utmp(3,j)) )*rsin_u(1,j)
           uc(2,j) = c1*utmp(3,j) + c2*utmp(2,j) + c3*utmp(1,j)
        enddo
     endif

     if( (ie+1)==npx .and. .not. nested ) then
        do j=js-1,je+1
           uc(npx-1,j) = c1*utmp(npx-3,j)+c2*utmp(npx-2,j)+c3*utmp(npx-1,j) 
           uc(npx,j) = (t14*(utmp(npx-1,j)+utmp(npx,j))+      &
                        t12*(utmp(npx-2,j)+utmp(npx+1,j))     &
                      + t15*(utmp(npx-3,j)+utmp(npx+2,j)))*rsin_u(npx,j)
           uc(npx+1,j) = c3*utmp(npx,j)+c2*utmp(npx+1,j)+c1*utmp(npx+2,j) 
        enddo
     endif

     endif

!------
! Ydir:
!------
     if( sw_corner ) then
         do j=-2,0
            vtmp(0,j) = -utmp(1-j,0)
         enddo
     endif
     if( nw_corner ) then
         do j=0,2
            vtmp(0,npy+j) = utmp(j+1,npy)
         enddo
     endif
     if( se_corner ) then
         do j=-2,0
            vtmp(npx,j) = utmp(ie+j,0)
         enddo
     endif
     if( ne_corner ) then
         do j=0,2
            vtmp(npx,npy+j) = -utmp(ie-j,npy)
         enddo
     endif

     if (grid_type < 3) then

     do j=js-1,je+2
      if ( j==1  .and. .not. nested) then
        do i=is-1,ie+1
           vc(i,1) = (t14*(vtmp(i, 0)+vtmp(i,1))    &
                    + t12*(vtmp(i,-1)+vtmp(i,2))    &
                    + t15*(vtmp(i,-2)+vtmp(i,3)))*rsin_v(i,1)
        enddo
      elseif ( (j==0 .or. j==(npy-1))  .and. .not. nested) then
        do i=is-1,ie+1
           vc(i,j) = c1*vtmp(i,j-2) + c2*vtmp(i,j-1) + c3*vtmp(i,j)
        enddo
      elseif ( (j==2 .or. j==(npy+1))  .and. .not. nested) then
        do i=is-1,ie+1
           vc(i,j) = c1*vtmp(i,j+1) + c2*vtmp(i,j) + c3*vtmp(i,j-1)
        enddo
      elseif ( j==npy  .and. .not. nested) then
        do i=is-1,ie+1
           vc(i,npy) = (t14*(vtmp(i,npy-1)+vtmp(i,npy))    &
                      + t12*(vtmp(i,npy-2)+vtmp(i,npy+1))  &
                      + t15*(vtmp(i,npy-3)+vtmp(i,npy+2)))*rsin_v(i,npy)
        enddo
      else
! 4th order interpolation for interior points:
        do i=is-1,ie+1
           vc(i,j) = a2*(vtmp(i,j-2)+vtmp(i,j+1))+a1*(vtmp(i,j-1)+vtmp(i,j))
        enddo
     endif
     enddo
    else
! 4th order interpolation:
       do j=js-1,je+2
          do i=is-1,ie+1
             vc(i,j) = a2*(vtmp(i,j-2)+vtmp(i,j+1))+a1*(vtmp(i,j-1)+vtmp(i,j))
          enddo
       enddo
    endif

  end subroutine d2c_setup

 subroutine d2a_setup(u, v, ua, va, dord4, &
      isd,ied,jsd,jed, is,ie,js,je, npx,npy, &
      grid_type, nested, &
      cosa_s,rsin2 )

  logical, intent(in):: dord4
  real, intent(in) ::  u(isd:ied,jsd:jed+1)
  real, intent(in) ::  v(isd:ied+1,jsd:jed)
  real, intent(out), dimension(isd:ied  ,jsd:jed  ):: ua
  real, intent(out), dimension(isd:ied  ,jsd:jed  ):: va
  integer, intent(in) :: isd,ied,jsd,jed, is,ie,js,je, npx,npy,grid_type
  real, intent(in) :: cosa_s(isd:ied,jsd:jed)
  real, intent(in) :: rsin2(isd:ied,jsd:jed)
  logical, intent(in) :: nested

! Local 
  real, dimension(isd:ied,jsd:jed):: utmp, vtmp
  real, parameter:: t11=27./28., t12=-13./28., t13=3./7., t14=6./7., t15=3./28.
  real, parameter:: a1 =  0.5625
  real, parameter:: a2 = -0.0625
  real, parameter:: c1 = -2./14.
  real, parameter:: c2 = 11./14.
  real, parameter:: c3 =  5./14.
  integer npt, i, j, ifirst, ilast, id

  if ( dord4) then
       id = 1
  else
       id = 0
  endif


  if (grid_type < 3 .and. .not. nested) then
     npt = 4
  else
     npt = -2
  endif

  if ( nested) then  

     do j=jsd+1,jed-1
        do i=isd,ied
           utmp(i,j) = a2*(u(i,j-1)+u(i,j+2)) + a1*(u(i,j)+u(i,j+1))
        enddo
     enddo
     do i=isd,ied
        j = jsd
        utmp(i,j) = 0.5*(u(i,j)+u(i,j+1))
        j = jed
        utmp(i,j) = 0.5*(u(i,j)+u(i,j+1))
     end do

     do j=jsd,jed
        do i=isd+1,ied-1
           vtmp(i,j) = a2*(v(i-1,j)+v(i+2,j)) + a1*(v(i,j)+v(i+1,j))
        enddo
        i = isd
        vtmp(i,j) = 0.5*(v(i,j)+v(i+1,j)) 
        i = ied
        vtmp(i,j) = 0.5*(v(i,j)+v(i+1,j))
     enddo

  else

     !----------
     ! Interior:
     !----------

     do j=max(npt,js-1),min(npy-npt,je+1)
        do i=max(npt,isd),min(npx-npt,ied)
           utmp(i,j) = a2*(u(i,j-1)+u(i,j+2)) + a1*(u(i,j)+u(i,j+1))
        enddo
     enddo
     do j=max(npt,jsd),min(npy-npt,jed)
        do i=max(npt,is-1),min(npx-npt,ie+1)
           vtmp(i,j) = a2*(v(i-1,j)+v(i+2,j)) + a1*(v(i,j)+v(i+1,j))
        enddo
     enddo

     !----------
     ! edges:
     !----------
     if (grid_type < 3) then

        if ( js==1 .or. jsd<npt) then
           do j=jsd,npt-1
              do i=isd,ied
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

        if ( (je+1)==npy .or. jed>=(npy-npt)) then
           do j=npy-npt+1,jed
              do i=isd,ied
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

        if ( is==1 .or. isd<npt ) then
           do j=max(npt,jsd),min(npy-npt,jed)
              do i=isd,npt-1
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

        if ( (ie+1)==npx .or. ied>=(npx-npt)) then
           do j=max(npt,jsd),min(npy-npt,jed)
              do i=npx-npt+1,ied
                 utmp(i,j) = 0.5*(u(i,j) + u(i,j+1))
                 vtmp(i,j) = 0.5*(v(i,j) + v(i+1,j))
              enddo
           enddo
        endif

     endif

  end if



  do j=js-1-id,je+1+id
     do i=is-1-id,ie+1+id
        ua(i,j) = (utmp(i,j)-vtmp(i,j)*cosa_s(i,j)) * rsin2(i,j)
        va(i,j) = (vtmp(i,j)-utmp(i,j)*cosa_s(i,j)) * rsin2(i,j)
     enddo
  enddo

end subroutine d2a_setup

subroutine pmaxmn_g(qname, q, is, ie, js, je, km, fac, area, domain)
      character(len=*), intent(in)::  qname
      integer, intent(in):: is, ie, js, je
      integer, intent(in):: km
      real, intent(in)::    q(is-3:ie+3, js-3:je+3, km)
      real, intent(in)::    fac
      real(kind=R_GRID), intent(IN)::    area(is-3:ie+3, js-3:je+3)
      type(domain2d), intent(INOUT) :: domain
!
      real qmin, qmax, gmean
      integer i,j,k

      qmin = q(is,js,1)
      qmax = qmin

      do k=1,km
      do j=js,je
         do i=is,ie
            if( q(i,j,k) < qmin ) then
                qmin = q(i,j,k)
            elseif( q(i,j,k) > qmax ) then
                qmax = q(i,j,k)
            endif
          enddo
      enddo
      enddo

      call mp_reduce_min(qmin)
      call mp_reduce_max(qmax)

      gmean = g_sum(domain, q(is:ie,js:je,km), is, ie, js, je, 3, area, 1, .true.) 
      if(is_master()) write(6,*) qname, qmax*fac, qmin*fac, gmean*fac

end subroutine pmaxmn_g
end module fv_restart_mod
