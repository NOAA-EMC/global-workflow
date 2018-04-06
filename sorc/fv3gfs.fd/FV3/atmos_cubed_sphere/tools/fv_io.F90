!***********************************************************************
!*                   GNU Lesser General Public License                 
!*
!* This file is part of the FV3 dynamical core.
!*
!* The FV3 dynamical core is free software: you can redistribute it 
!* and/or modify it under the terms of the
!* GNU Lesser General Public License as published by the
!* Free Software Foundation, either version 3 of the License, or 
!* (at your option) any later version.
!*
!* The FV3 dynamical core is distributed in the hope that it will be 
!* useful, but WITHOUT ANYWARRANTY; without even the implied warranty 
!* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
!* See the GNU General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with the FV3 dynamical core.  
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

!>@brief The module 'fv_io' contains restart facilities for FV core.
!>@details This module writes and reads restart files for the FV core. Additionally
!! it provides setup and calls routines necessary to provide a complete restart
!! for the model.
!>@note NOTE: Merging in the seasonal forecast initialization code
!! has proven problematic in the past, since many conflicts
!! occur. Leaving this for now --- lmh 10aug15

module fv_io_mod

! <table>
! <tr>
!     <th>Module Name</th>
!     <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>external_sst_mod</td>
!     <td>sst_ncep, sst_anom, use_ncep_sst</td>
!   </tr>
!   <tr>
!     <td>field_manager_mod</td>
!     <td>MODEL_ATMOS</td>
!   </tr>
!   <tr>
!     <td>fms_mod</td>
!     <td>file_exist</td>
!   </tr>
!   <tr>
!     <td>fms_io_mod</td>
!     <td>fms_io_exit, get_tile_string,restart_file_type, 
!         register_restart_field, save_restart, restore_state, 
!         set_domain, nullify_domain, set_filename_appendix, 
!         get_mosaic_tile_file, get_instance_filename, 
!         save_restart_border, restore_state_border,
!         free_restart_type,field_exist</td>
!   </tr>
!   <tr>
!     <td>fv_arrays_mod</td>
!     <td>fv_atmos_type, fv_nest_BC_type_3D</td>
!   </tr>
!   <tr>
!     <td>fv_eta_mod</td>
!     <td>set_external_eta</td>
!   </tr>
!   <tr>
!     <td>fv_mp_mod</td>
!     <td>ng, mp_gather, is_master</td>
!   </tr>
!   <tr>
!     <td>mpp_mod</td>
!     <td>mpp_error, FATAL, NOTE, WARNING, mpp_root_pe,
!         mpp_sync, mpp_pe, mpp_declare_pelist</td>
!   </tr>
!   <tr>
!     <td>mpp_domains_mod</td>
!     <td>domain2d, EAST, WEST, NORTH, CENTER, SOUTH, CORNER,
!         mpp_get_compute_domain, mpp_get_data_domain,  
!         mpp_get_layout, mpp_get_ntile_count,mpp_get_global_domain</td>
!   </tr>
!   <tr>
!     <td>tracer_manager_mod</td>
!     <td>tr_get_tracer_names=>get_tracer_names, 
!         get_tracer_names, get_number_tracers, 
!         set_tracer_profile, get_tracer_index</td>
!   </tr>
! </table>

  use fms_mod,                 only: file_exist
  use fms_io_mod,              only: fms_io_exit, get_tile_string, &
                                     restart_file_type, register_restart_field, &
                                     save_restart, restore_state, &
                                     set_domain, nullify_domain, set_filename_appendix, &
                                     get_mosaic_tile_file, get_instance_filename, & 
                                     save_restart_border, restore_state_border, free_restart_type, &
                                     field_exist
  use mpp_mod,                 only: mpp_error, FATAL, NOTE, WARNING, mpp_root_pe, &
                                     mpp_sync, mpp_pe, mpp_declare_pelist
  use mpp_domains_mod,         only: domain2d, EAST, WEST, NORTH, CENTER, SOUTH, CORNER, &
                                     mpp_get_compute_domain, mpp_get_data_domain, & 
                                     mpp_get_layout, mpp_get_ntile_count, &
                                     mpp_get_global_domain
  use tracer_manager_mod,      only: tr_get_tracer_names=>get_tracer_names, &
                                     get_tracer_names, get_number_tracers, &
                                     set_tracer_profile, &
                                     get_tracer_index
  use field_manager_mod,       only: MODEL_ATMOS  
  use external_sst_mod,        only: sst_ncep, sst_anom, use_ncep_sst
  use fv_arrays_mod,           only: fv_atmos_type, fv_nest_BC_type_3D
  use fv_eta_mod,              only: set_external_eta

  use fv_mp_mod,               only: ng, mp_gather, is_master
  implicit none
  private

  public :: fv_io_init, fv_io_exit, fv_io_read_restart, remap_restart, fv_io_write_restart
  public :: fv_io_read_tracers, fv_io_register_restart, fv_io_register_nudge_restart
  public :: fv_io_register_restart_BCs, fv_io_register_restart_BCs_NH
  public :: fv_io_write_BCs, fv_io_read_BCs

  logical                       :: module_is_initialized = .FALSE.


  integer ::grid_xtdimid, grid_ytdimid, haloid, pfullid !For writing BCs
  integer ::grid_xtstagdimid, grid_ytstagdimid, oneid

contains 


  !>@brief Initialize the fv core restart facilities
  subroutine fv_io_init()
    module_is_initialized = .TRUE.
  end subroutine fv_io_init


  !>@brief Close the fv core restart facilities
  subroutine fv_io_exit
    module_is_initialized = .FALSE.
  end subroutine fv_io_exit


  !>@brief Write the fv core restart quantities 
  subroutine  fv_io_read_restart(fv_domain,Atm)
    type(domain2d),      intent(inout) :: fv_domain
    type(fv_atmos_type), intent(inout) :: Atm(:)

    character(len=64)    :: fname, tracer_name
    character(len=6)  :: stile_name
    integer              :: isc, iec, jsc, jec, n, nt, nk, ntracers
    integer              :: ntileMe
    integer              :: ks, ntiles
    real                 :: ptop

    character(len=128)           :: tracer_longname, tracer_units

    ntileMe = size(Atm(:))  ! This will need mods for more than 1 tile per pe

    call restore_state(Atm(1)%Fv_restart)
    if (Atm(1)%flagstruct%external_eta) then
       call set_external_eta(Atm(1)%ak, Atm(1)%bk, Atm(1)%ptop, Atm(1)%ks)
    endif

    if ( use_ncep_sst .or. Atm(1)%flagstruct%nudge .or. Atm(1)%flagstruct%ncep_ic ) then
       call mpp_error(NOTE, 'READING FROM SST_RESTART DISABLED')
       !call restore_state(Atm(1)%SST_restart)
    endif

! fix for single tile runs where you need fv_core.res.nc and fv_core.res.tile1.nc
    ntiles = mpp_get_ntile_count(fv_domain)
    if(ntiles == 1 .and. .not. Atm(1)%neststruct%nested) then
       stile_name = '.tile1'
    else
       stile_name = ''
    endif
 
    do n = 1, ntileMe
       call restore_state(Atm(n)%Fv_tile_restart)

!--- restore data for fv_tracer - if it exists
       fname = 'INPUT/fv_tracer.res'//trim(stile_name)//'.nc'
       if (file_exist(fname)) then
         call restore_state(Atm(n)%Tra_restart)
       else
         call mpp_error(NOTE,'==> Warning from fv_read_restart: Expected file '//trim(fname)//' does not exist')
       endif

!--- restore data for surface winds - if it exists
       fname = 'INPUT/fv_srf_wnd.res'//trim(stile_name)//'.nc'
       if (file_exist(fname)) then
         call restore_state(Atm(n)%Rsf_restart)
         Atm(n)%flagstruct%srf_init = .true.
       else
         call mpp_error(NOTE,'==> Warning from fv_read_restart: Expected file '//trim(fname)//' does not exist')
         Atm(n)%flagstruct%srf_init = .false.
       endif

       if ( Atm(n)%flagstruct%fv_land ) then
!--- restore data for mg_drag - if it exists
         fname = 'INPUT/mg_drag.res'//trim(stile_name)//'.nc'
         if (file_exist(fname)) then
           call restore_state(Atm(n)%Mg_restart)
         else
           call mpp_error(NOTE,'==> Warning from fv_read_restart: Expected file '//trim(fname)//' does not exist')
         endif
!--- restore data for fv_land - if it exists
         fname = 'INPUT/fv_land.res'//trim(stile_name)//'.nc'
         if (file_exist(fname)) then
           call restore_state(Atm(n)%Lnd_restart)
         else
           call mpp_error(NOTE,'==> Warning from fv_read_restart: Expected file '//trim(fname)//' does not exist')
         endif
       endif

    end do

    return

  end subroutine  fv_io_read_restart
  
  !>@brief The subroutine 'fv_io_read_tracers' reads in only tracers from restart files.
  !>@details This subroutine is useful when initializing a cycled or nudged model 
  !! from an analysis that does not have a whole set of microphysical, aerosol, or 
  !! chemical tracers
  subroutine fv_io_read_tracers(fv_domain,Atm)
    type(domain2d),      intent(inout) :: fv_domain
    type(fv_atmos_type), intent(inout) :: Atm(:)
    integer :: n, ntracers, ntprog, nt, isc, iec, jsc, jec, id_restart
    character(len=6) :: stile_name
    character(len=64):: fname, tracer_name
    type(restart_file_type) :: Tra_restart_r
    integer :: ntiles

    n = 1
    isc = Atm(n)%bd%isc
    iec = Atm(n)%bd%iec
    jsc = Atm(n)%bd%jsc
    jec = Atm(n)%bd%jec
    call get_number_tracers(MODEL_ATMOS, num_tracers=ntracers, num_prog=ntprog)

! fix for single tile runs where you need fv_core.res.nc and fv_core.res.tile1.nc
    ntiles = mpp_get_ntile_count(fv_domain)
    if(ntiles == 1 .and. .not. Atm(1)%neststruct%nested) then
       stile_name = '.tile1'
    else
       stile_name = ''
    endif

    fname = 'fv_tracer.res'//trim(stile_name)//'.nc'
    do nt = 2, ntprog
       call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
       call set_tracer_profile (MODEL_ATMOS, nt, Atm(n)%q(isc:iec,jsc:jec,:,nt)  )
       id_restart = register_restart_field(Tra_restart_r, fname, tracer_name, Atm(n)%q(:,:,:,nt), &
                    domain=fv_domain, mandatory=.false., tile_count=n)
    enddo
    do nt = ntprog+1, ntracers
       call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
       call set_tracer_profile (MODEL_ATMOS, nt, Atm(n)%qdiag(isc:iec,jsc:jec,:,nt)  )
       id_restart = register_restart_field(Tra_restart_r, fname, tracer_name, Atm(n)%qdiag(:,:,:,nt), &
                    domain=fv_domain, mandatory=.false., tile_count=n)
    enddo
    if (file_exist('INPUT'//trim(fname))) then
      call restore_state(Tra_restart_r)
      call free_restart_type(Tra_restart_r)
    else
      call mpp_error(NOTE,'==> Warning from fv_io_read_tracers: Expected file '//trim(fname)//' does not exist')
    endif

    return

  end subroutine  fv_io_read_tracers
 
  !>@brief The subroutine 'remap_restart' remaps the model state from remap files 
  !! to a new set of Eulerian coordinates.
  !>@details Use if npz (run time z-dimension) /= npz_rst (restart z-dimension)
  subroutine  remap_restart(fv_domain,Atm)
  use fv_mapz_mod,       only: rst_remap

    type(domain2d),      intent(inout) :: fv_domain
    type(fv_atmos_type), intent(inout) :: Atm(:)

    character(len=64)    :: fname, tracer_name
    character(len=6)     :: stile_name
    integer              :: isc, iec, jsc, jec, n, nt, nk, ntracers, ntprog, ntdiag
    integer              :: isd, ied, jsd, jed
    integer              :: ntiles
    type(restart_file_type) :: FV_restart_r, FV_tile_restart_r, Tra_restart_r
    integer :: id_restart

!
!-------------------------------------------------------------------------
    real, allocatable:: ak_r(:), bk_r(:)
    real, allocatable:: u_r(:,:,:), v_r(:,:,:), pt_r(:,:,:), delp_r(:,:,:)
    real, allocatable:: w_r(:,:,:), delz_r(:,:,:), ze0_r(:,:,:)
    real, allocatable:: q_r(:,:,:,:), qdiag_r(:,:,:,:)
!-------------------------------------------------------------------------
    integer npz, npz_rst, ng

    npz     = Atm(1)%npz       ! run time z dimension
    npz_rst = Atm(1)%flagstruct%npz_rst   ! restart z dimension
    isc = Atm(1)%bd%isc; iec = Atm(1)%bd%iec; jsc = Atm(1)%bd%jsc; jec = Atm(1)%bd%jec
    ng = Atm(1)%ng

    isd = isc - ng;  ied = iec + ng
    jsd = jsc - ng;  jed = jec + ng


!   call get_number_tracers(MODEL_ATMOS, num_tracers=ntracers)
    ntprog = size(Atm(1)%q,4)  ! Temporary until we get tracer manager integrated
    ntdiag = size(Atm(1)%qdiag,4)
    ntracers = ntprog+ntdiag

!    ntileMe = size(Atm(:))  ! This will have to be modified for mult tiles per PE


! Allocate arrays for reading old restart file:
    allocate ( ak_r(npz_rst+1) )
    allocate ( bk_r(npz_rst+1) )

    allocate ( u_r(isc:iec,  jsc:jec+1,npz_rst) )
    allocate ( v_r(isc:iec+1,jsc:jec  ,npz_rst) )

    allocate (   pt_r(isc:iec, jsc:jec,  npz_rst) )
    allocate ( delp_r(isc:iec, jsc:jec,  npz_rst) )
    allocate (    q_r(isc:iec, jsc:jec,  npz_rst, ntprog) )
    allocate (qdiag_r(isc:iec, jsc:jec,  npz_rst, ntprog+1:ntracers) )

    if ( (.not.Atm(1)%flagstruct%hydrostatic) .and. (.not.Atm(1)%flagstruct%make_nh) ) then
           allocate (    w_r(isc:iec, jsc:jec,  npz_rst) )
           allocate ( delz_r(isc:iec, jsc:jec,  npz_rst) )
           if ( Atm(1)%flagstruct%hybrid_z )   &
           allocate ( ze0_r(isc:iec, jsc:jec,  npz_rst+1) )
    endif

    fname = 'fv_core.res.nc'
    id_restart = register_restart_field(Fv_restart_r, fname, 'ak', ak_r(:), no_domain=.true.)
    id_restart = register_restart_field(Fv_restart_r, fname, 'bk', bk_r(:), no_domain=.true.)
    call restore_state(Fv_restart_r)
    call free_restart_type(Fv_restart_r)

! fix for single tile runs where you need fv_core.res.nc and fv_core.res.tile1.nc
    ntiles = mpp_get_ntile_count(fv_domain)
    if(ntiles == 1 .and. .not. Atm(1)%neststruct%nested) then
       stile_name = '.tile1'
    else
       stile_name = ''
    endif

!    do n = 1, ntileMe
    n = 1
       fname = 'fv_core.res'//trim(stile_name)//'.nc'
       id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'u', u_r, &
                     domain=fv_domain, position=NORTH,tile_count=n)
       id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'v', v_r, &
                     domain=fv_domain, position=EAST,tile_count=n)
       if (.not.Atm(n)%flagstruct%hydrostatic) then
          id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'W', w_r, &
                        domain=fv_domain, mandatory=.false., tile_count=n)
          id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'DZ', delz_r, &
                        domain=fv_domain, mandatory=.false., tile_count=n)
          if ( Atm(n)%flagstruct%hybrid_z ) then
             id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'ZE0', ze0_r, &
                           domain=fv_domain, mandatory=.false., tile_count=n)
          endif
       endif
       id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'T', pt_r, &
                     domain=fv_domain, tile_count=n)
       id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'delp', delp_r, &
                     domain=fv_domain, tile_count=n)
       id_restart =  register_restart_field(Fv_tile_restart_r, fname, 'phis', Atm(n)%phis, &
                     domain=fv_domain, tile_count=n)
       call restore_state(FV_tile_restart_r)
       call free_restart_type(FV_tile_restart_r)
       fname = 'INPUT/fv_srf_wnd.res'//trim(stile_name)//'.nc'
       if (file_exist(fname)) then
         call restore_state(Atm(n)%Rsf_restart)
         Atm(n)%flagstruct%srf_init = .true.
       else
         call mpp_error(NOTE,'==> Warning from remap_restart: Expected file '//trim(fname)//' does not exist')
         Atm(n)%flagstruct%srf_init = .false.
       endif

       if ( Atm(n)%flagstruct%fv_land ) then
!--- restore data for mg_drag - if it exists
         fname = 'INPUT/mg_drag.res'//trim(stile_name)//'.nc'
         if (file_exist(fname)) then
           call restore_state(Atm(n)%Mg_restart)
         else
           call mpp_error(NOTE,'==> Warning from remap_restart: Expected file '//trim(fname)//' does not exist')
         endif
!--- restore data for fv_land - if it exists
         fname = 'INPUT/fv_land.res'//trim(stile_name)//'.nc'
         if (file_exist(fname)) then
           call restore_state(Atm(n)%Lnd_restart)
         else
           call mpp_error(NOTE,'==> Warning from remap_restart: Expected file '//trim(fname)//' does not exist')
         endif
       endif

       fname = 'fv_tracer.res'//trim(stile_name)//'.nc'
       if (file_exist('INPUT'//trim(fname))) then
         do nt = 1, ntprog
            call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
            call set_tracer_profile (MODEL_ATMOS, nt, q_r(isc:iec,jsc:jec,:,nt)  )
            id_restart = register_restart_field(Tra_restart_r, fname, tracer_name, q_r(:,:,:,nt), &
                         domain=fv_domain, mandatory=.false., tile_count=n)
         enddo
         do nt = ntprog+1, ntracers
            call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
            call set_tracer_profile (MODEL_ATMOS, nt, qdiag_r(isc:iec,jsc:jec,:,nt)  )
            id_restart = register_restart_field(Tra_restart_r, fname, tracer_name, qdiag_r(:,:,:,nt), &
                         domain=fv_domain, mandatory=.false., tile_count=n)
         enddo
         call restore_state(Tra_restart_r)
         call free_restart_type(Tra_restart_r)
       else
         call mpp_error(NOTE,'==> Warning from remap_restart: Expected file '//trim(fname)//' does not exist')
       endif

       call rst_remap(npz_rst, npz, isc, iec, jsc, jec, isd, ied, jsd, jed, ntracers, ntprog,      &
                      delp_r,      u_r,      v_r,      w_r,      delz_r,      pt_r,  q_r,  qdiag_r,&
                      Atm(n)%delp, Atm(n)%u, Atm(n)%v, Atm(n)%w, Atm(n)%delz, Atm(n)%pt, Atm(n)%q, &
                      Atm(n)%qdiag, ak_r,  bk_r, Atm(n)%ptop, Atm(n)%ak, Atm(n)%bk,                &
                      Atm(n)%flagstruct%hydrostatic, Atm(n)%flagstruct%make_nh, Atm(n)%domain,     &
                      Atm(n)%gridstruct%square_domain)
    !end do

    deallocate( ak_r )
    deallocate( bk_r )
    deallocate( u_r )
    deallocate( v_r )
    deallocate( pt_r )
    deallocate( delp_r )
    deallocate( q_r )
    deallocate( qdiag_r )

    if ( (.not.Atm(1)%flagstruct%hydrostatic) .and. (.not.Atm(1)%flagstruct%make_nh) ) then
         deallocate ( w_r )
         deallocate ( delz_r )
         if ( Atm(1)%flagstruct%hybrid_z ) deallocate ( ze0_r )
    endif

  end subroutine  remap_restart

!>@brief The subroutine 'fv_io_register_nudge_restart' registers restarts for SST
!! fields used in HiRAM.
!>@note This option is currently not supported.
  subroutine  fv_io_register_nudge_restart(Atm)
    type(fv_atmos_type), intent(inout) :: Atm(:)
    character(len=64) :: fname
    integer           :: id_restart

! use_ncep_sst may not be initialized at this point?
    call mpp_error(NOTE, 'READING FROM SST_restart DISABLED')
!!$    if ( use_ncep_sst .or. Atm(1)%nudge .or. Atm(1)%ncep_ic ) then
!!$       fname = 'sst_ncep.res.nc'
!!$       id_restart = register_restart_field(Atm(1)%SST_restart, fname, 'sst_ncep', sst_ncep)
!!$       id_restart = register_restart_field(Atm(1)%SST_restart, fname, 'sst_anom', sst_anom)
!!$    endif

  end subroutine  fv_io_register_nudge_restart

!>@brief The subroutine 'fv_io_register_restart' registers model restart fields.
  subroutine  fv_io_register_restart(fv_domain,Atm)
    type(domain2d),      intent(inout) :: fv_domain
    type(fv_atmos_type), intent(inout) :: Atm(:)

    character(len=64) :: fname, tracer_name
    character(len=6)  :: gn, stile_name
    integer           :: id_restart
    integer           :: n, nt, ntracers, ntprog, ntdiag, ntileMe, ntiles

    ntileMe = size(Atm(:)) 
    ntprog = size(Atm(1)%q,4) 
    ntdiag = size(Atm(1)%qdiag,4) 
    ntracers = ntprog+ntdiag

!--- set the 'nestXX' appendix for all files using fms_io
    if (Atm(1)%grid_number > 1) then
       write(gn,'(A4, I2.2)') "nest", Atm(1)%grid_number
    else
       gn = ''
    end if
    call set_filename_appendix(gn)

!--- fix for single tile runs where you need fv_core.res.nc and fv_core.res.tile1.nc
    ntiles = mpp_get_ntile_count(fv_domain)
    if(ntiles == 1 .and. .not. Atm(1)%neststruct%nested) then
       stile_name = '.tile1'
    else
       stile_name = ''
    endif

! use_ncep_sst may not be initialized at this point?
#ifndef DYCORE_SOLO
    call mpp_error(NOTE, 'READING FROM SST_RESTART DISABLED')
!!$   if ( use_ncep_sst .or. Atm(1)%flagstruct%nudge .or. Atm(1)%flagstruct%ncep_ic ) then
!!$       fname = 'sst_ncep'//trim(gn)//'.res.nc'
!!$       id_restart = register_restart_field(Atm(1)%SST_restart, fname, 'sst_ncep', sst_ncep)
!!$       id_restart = register_restart_field(Atm(1)%SST_restart, fname, 'sst_anom', sst_anom)
!!$   endif
#endif

    fname = 'fv_core.res.nc'
    id_restart = register_restart_field(Atm(1)%Fv_restart, fname, 'ak', Atm(1)%ak(:), no_domain=.true.)
    id_restart = register_restart_field(Atm(1)%Fv_restart, fname, 'bk', Atm(1)%bk(:), no_domain=.true.) 

    do n = 1, ntileMe
       fname = 'fv_core.res'//trim(stile_name)//'.nc'
       id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'u', Atm(n)%u, &
                     domain=fv_domain, position=NORTH,tile_count=n)
       id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'v', Atm(n)%v, &
                     domain=fv_domain, position=EAST,tile_count=n)
       if (.not.Atm(n)%flagstruct%hydrostatic) then
          id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'W', Atm(n)%w, &
                        domain=fv_domain, mandatory=.false., tile_count=n)
          id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'DZ', Atm(n)%delz, &
                        domain=fv_domain, mandatory=.false., tile_count=n)
          if ( Atm(n)%flagstruct%hybrid_z ) then
             id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'ZE0', Atm(n)%ze0, &
                           domain=fv_domain, mandatory=.false., tile_count=n)
          endif
       endif
       id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'T', Atm(n)%pt, &
                     domain=fv_domain, tile_count=n)
       id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'delp', Atm(n)%delp, &
                     domain=fv_domain, tile_count=n)
       id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'phis', Atm(n)%phis, &
                     domain=fv_domain, tile_count=n)

       !--- include agrid winds in restarts for use in data assimilation 
       if (Atm(n)%flagstruct%agrid_vel_rst) then
         id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'ua', Atm(n)%ua, &
                       domain=fv_domain, tile_count=n, mandatory=.false.)
         id_restart =  register_restart_field(Atm(n)%Fv_tile_restart, fname, 'va', Atm(n)%va, &
                       domain=fv_domain, tile_count=n, mandatory=.false.)
       endif

       fname = 'fv_srf_wnd.res'//trim(stile_name)//'.nc'
       id_restart =  register_restart_field(Atm(n)%Rsf_restart, fname, 'u_srf', Atm(n)%u_srf, &
                     domain=fv_domain, tile_count=n)
       id_restart =  register_restart_field(Atm(n)%Rsf_restart, fname, 'v_srf', Atm(n)%v_srf, &
                     domain=fv_domain, tile_count=n)
#ifdef SIM_PHYS
       id_restart =  register_restart_field(Rsf_restart(n), fname, 'ts', Atm(n)%ts, &
                     domain=fv_domain, tile_count=n)
#endif

       if ( Atm(n)%flagstruct%fv_land ) then
          !-------------------------------------------------------------------------------------------------
          ! Optional terrain deviation (sgh) and land fraction (oro)
          fname = 'mg_drag.res'//trim(stile_name)//'.nc'
          id_restart =  register_restart_field(Atm(n)%Mg_restart, fname, 'ghprime', Atm(n)%sgh, &
                        domain=fv_domain, tile_count=n)  

          fname = 'fv_land.res'//trim(stile_name)//'.nc'
          id_restart = register_restart_field(Atm(n)%Lnd_restart, fname, 'oro', Atm(n)%oro, &
                        domain=fv_domain, tile_count=n)
       endif

       fname = 'fv_tracer.res'//trim(stile_name)//'.nc'
       do nt = 1, ntprog
          call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
          ! set all tracers to an initial profile value
          call set_tracer_profile (MODEL_ATMOS, nt, Atm(n)%q(:,:,:,nt)  )
          id_restart = register_restart_field(Atm(n)%Tra_restart, fname, tracer_name, Atm(n)%q(:,:,:,nt), &
                       domain=fv_domain, mandatory=.false., tile_count=n)
       enddo
       do nt = ntprog+1, ntracers
          call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
          ! set all tracers to an initial profile value
          call set_tracer_profile (MODEL_ATMOS, nt, Atm(n)%qdiag(:,:,:,nt)  )
          id_restart = register_restart_field(Atm(n)%Tra_restart, fname, tracer_name, Atm(n)%qdiag(:,:,:,nt), &
                       domain=fv_domain, mandatory=.false., tile_count=n)
       enddo

    enddo

  end subroutine  fv_io_register_restart

  !>@brief The subroutine 'fv_io_write_restart' writes restart files.
  subroutine  fv_io_write_restart(Atm, grids_on_this_pe, timestamp)

    type(fv_atmos_type),        intent(inout) :: Atm(:)
    logical, intent(IN) :: grids_on_this_pe(:)
    character(len=*), optional, intent(in) :: timestamp
    integer                                :: n, ntileMe

    ntileMe = size(Atm(:))  ! This will need mods for more than 1 tile per pe

    if ( use_ncep_sst .or. Atm(1)%flagstruct%nudge .or. Atm(1)%flagstruct%ncep_ic ) then
       call mpp_error(NOTE, 'READING FROM SST_RESTART DISABLED')
       !call save_restart(Atm(1)%SST_restart, timestamp)
    endif
 
    do n = 1, ntileMe
       if (.not. grids_on_this_pe(n)) cycle

       if ( (use_ncep_sst .or. Atm(n)%flagstruct%nudge) .and. .not. Atm(n)%gridstruct%nested ) then
          call save_restart(Atm(n)%SST_restart, timestamp)
       endif
 
       call save_restart(Atm(n)%Fv_restart, timestamp)
       call save_restart(Atm(n)%Fv_tile_restart, timestamp)
       call save_restart(Atm(n)%Rsf_restart, timestamp)

       if ( Atm(n)%flagstruct%fv_land ) then
          call save_restart(Atm(n)%Mg_restart, timestamp)
          call save_restart(Atm(n)%Lnd_restart, timestamp)
       endif

       call save_restart(Atm(n)%Tra_restart, timestamp)

    end do

  end subroutine  fv_io_write_restart

  subroutine register_bcs_2d(Atm, BCfile_ne, BCfile_sw, fname_ne, fname_sw, &
                             var_name, var, var_bc, istag, jstag)
    type(fv_atmos_type),      intent(in)    :: Atm
    type(restart_file_type),  intent(inout) :: BCfile_ne, BCfile_sw
    character(len=120),       intent(in)    :: fname_ne, fname_sw
    character(len=*),         intent(in)    :: var_name
    real, dimension(:,:),     intent(in), optional :: var
    type(fv_nest_BC_type_3D), intent(in), optional :: var_bc
    integer,                  intent(in), optional :: istag, jstag

    integer :: npx, npy, i_stag, j_stag
    integer :: is, ie, js, je, isd, ied, jsd, jed, n
    integer :: x_halo, y_halo, x_halo_ns, id_restart
    integer :: layout(2), global_size(2), indices(4)
    integer, allocatable, dimension(:) :: x1_pelist, y1_pelist
    integer, allocatable, dimension(:) :: x2_pelist, y2_pelist
    logical :: is_root_pe

    i_stag = 0 
    j_stag = 0 
    if (present(istag)) i_stag = i_stag
    if (present(jstag)) j_stag = j_stag
    call mpp_get_global_domain(Atm%domain, xsize = npx, ysize = npy, position=CORNER )
    call mpp_get_data_domain(Atm%domain, isd, ied, jsd, jed )
    call mpp_get_compute_domain(Atm%domain, is, ie, js, je )
    call mpp_get_layout(Atm%domain, layout)
    allocate (x1_pelist(layout(1)))
    allocate (y1_pelist(layout(2)))
    allocate (x2_pelist(layout(1)))
    allocate (y2_pelist(layout(2)))
    x_halo = is-isd
    y_halo = js-jsd
! define west and east pelist
    do n = 1,layout(2)
      y1_pelist(n)=mpp_root_pe()+layout(1)*n-1
      y2_pelist(n)=mpp_root_pe()+layout(1)*(n-1)
    enddo
! define south and north pelist
    do n = 1,layout(1)
      x1_pelist(n)=mpp_root_pe()+layout(1)*(layout(2)-1)+(n-1)
      x2_pelist(n)=mpp_root_pe()+(n-1)
    enddo
! declare the pelists inside of mpp (creates the MPI communicator)
    call mpp_declare_pelist(x1_pelist)
    call mpp_declare_pelist(x2_pelist)
    call mpp_declare_pelist(y1_pelist)
    call mpp_declare_pelist(y2_pelist)

!EAST & WEST
!set defaults for west/east halo regions
    indices(1) = 1
    indices(2) = x_halo
    indices(3) = jsd
    indices(4) = jed+j_stag
    global_size(1) = x_halo
    global_size(2) = npy-1+2*y_halo+j_stag

!define west root_pe
    is_root_pe = .FALSE.
    if (is.eq.1 .and. js.eq.1) is_root_pe = .TRUE.
!register west halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_west_t1', &
                                        var_bc%west_t1, & 
                                        indices, global_size, y2_pelist, &
                                        is_root_pe, jshift=y_halo)
!register west prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_west', &
                                        var, indices, global_size, &
                                        y2_pelist, is_root_pe, jshift=y_halo)

!define east root_pe
    is_root_pe = .FALSE.
    if (ie.eq.npx-1 .and. je.eq.npy-1) is_root_pe = .TRUE.
!register east halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_east_t1', &
                                        var_bc%east_t1, & 
                                        indices, global_size, y1_pelist, &
                                        is_root_pe, jshift=y_halo)

!reset indices for prognostic variables in the east halo
    indices(1) = ied-x_halo+1+i_stag
    indices(2) = ied+i_stag
!register east prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_east', &
                                        var, indices, global_size, &
                                        y1_pelist, is_root_pe, jshift=y_halo, &
                                        x_halo=(size(var,1)-x_halo), ishift=-(ie+i_stag))

!NORTH & SOUTH
!set defaults for north/south halo regions
    indices(1) = isd
    indices(2) = ied+i_stag
    indices(3) = 1
    indices(4) = y_halo
    global_size(1) = npx-1+i_stag
    global_size(2) = y_halo
!modify starts and ends for certain pes
    if (is.eq.1)     indices(1) = is
    if (ie.eq.npx-1) indices(2) = ie+i_stag
    x_halo_ns = 0
    if (is.eq.1) x_halo_ns=x_halo

!define south root_pe
    is_root_pe = .FALSE.
    if (is.eq.1 .and. js.eq.1) is_root_pe = .TRUE.
!register south halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_south_t1', &
                                        var_bc%south_t1, & 
                                        indices, global_size, x2_pelist, &
                                        is_root_pe, x_halo=x_halo_ns)
!register south prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_south', &
                                        var, indices, global_size, &
                                        x2_pelist, is_root_pe, x_halo=x_halo_ns)

!define north root_pe
    is_root_pe = .FALSE.
    if (ie.eq.npx-1 .and. je.eq.npy-1) is_root_pe = .TRUE.
!register north halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_north_t1', &
                                        var_bc%north_t1, & 
                                        indices, global_size, x1_pelist, &
                                        is_root_pe, x_halo=x_halo_ns)

!reset indices for prognostic variables in the north halo
    indices(3) = jed-y_halo+1+j_stag
    indices(4) = jed+j_stag
!register north prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_north', &
                                        var, indices, global_size, &
                                        x1_pelist, is_root_pe, x_halo=x_halo_ns, &
                                        y_halo=(size(var,2)-y_halo), jshift=-(je+j_stag))

  end subroutine register_bcs_2d


  subroutine register_bcs_3d(Atm, BCfile_ne, BCfile_sw, fname_ne, fname_sw, &
                             var_name, var, var_bc, istag, jstag, mandatory)
    type(fv_atmos_type),      intent(in)    :: Atm
    type(restart_file_type),  intent(inout) :: BCfile_ne, BCfile_sw
    character(len=120),       intent(in)    :: fname_ne, fname_sw
    character(len=*),         intent(in)    :: var_name
    real, dimension(:,:,:),   intent(in), optional :: var
    type(fv_nest_BC_type_3D), intent(in), optional :: var_bc
    integer,                  intent(in), optional :: istag, jstag
    logical,                  intent(IN), optional :: mandatory

    integer :: npx, npy, i_stag, j_stag
    integer :: is, ie, js, je, isd, ied, jsd, jed, n
    integer :: x_halo, y_halo, x_halo_ns, id_restart
    integer :: layout(2), global_size(3), indices(4)
    integer, allocatable, dimension(:) :: x1_pelist, y1_pelist
    integer, allocatable, dimension(:) :: x2_pelist, y2_pelist
    logical :: is_root_pe

    i_stag = 0
    j_stag = 0
    if (present(istag)) i_stag = istag
    if (present(jstag)) j_stag = jstag
    call mpp_get_global_domain(Atm%domain, xsize = npx, ysize = npy, position=CORNER )
    call mpp_get_data_domain(Atm%domain, isd, ied, jsd, jed )
    call mpp_get_compute_domain(Atm%domain, is, ie, js, je )
    call mpp_get_layout(Atm%domain, layout)
    allocate (x1_pelist(layout(1)))
    allocate (y1_pelist(layout(2)))
    allocate (x2_pelist(layout(1)))
    allocate (y2_pelist(layout(2)))
    x_halo = is-isd
    y_halo = js-jsd
! define west and east pelist
    do n = 1,layout(2)
      y1_pelist(n)=mpp_root_pe()+layout(1)*n-1
      y2_pelist(n)=mpp_root_pe()+layout(1)*(n-1)
    enddo
! define south and north pelist
    do n = 1,layout(1)
      x1_pelist(n)=mpp_root_pe()+layout(1)*(layout(2)-1)+(n-1)
      x2_pelist(n)=mpp_root_pe()+(n-1)
    enddo
! declare the pelists inside of mpp (creates the MPI communicator)
    call mpp_declare_pelist(x1_pelist)
    call mpp_declare_pelist(x2_pelist)
    call mpp_declare_pelist(y1_pelist)
    call mpp_declare_pelist(y2_pelist)

!EAST & WEST
!set defaults for west/east halo regions
    indices(1) = 1
    indices(2) = x_halo
    indices(3) = jsd
    indices(4) = jed + j_stag
    global_size(1) = x_halo
    global_size(2) = npy-1+2*y_halo + j_stag
    global_size(3) = Atm%npz

!define west root_pe
    is_root_pe = .FALSE.
    if (is.eq.1 .and. js.eq.1) is_root_pe = .TRUE.
!register west halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_west_t1', &
                                        var_bc%west_t1, & 
                                        indices, global_size, y2_pelist, &
                                        is_root_pe, jshift=y_halo, mandatory=mandatory)
!register west prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_west', &
                                        var, indices, global_size, &
                                        y2_pelist, is_root_pe, jshift=y_halo, mandatory=mandatory)

!define east root_pe
    is_root_pe = .FALSE.
    if (ie.eq.npx-1 .and. je.eq.npy-1) is_root_pe = .TRUE.
!register east halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_east_t1', &
                                        var_bc%east_t1, & 
                                        indices, global_size, y1_pelist, &
                                        is_root_pe, jshift=y_halo, mandatory=mandatory)

!reset indices for prognostic variables in the east halo
    indices(1) = ied-x_halo+1+i_stag
    indices(2) = ied+i_stag
!register east prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_east', &
                                        var, indices, global_size, &
                                        y1_pelist, is_root_pe, jshift=y_halo, &
                                        x_halo=(size(var,1)-x_halo), ishift=-(ie+i_stag), mandatory=mandatory)

!NORTH & SOUTH
!set defaults for north/south halo regions
    indices(1) = isd
    indices(2) = ied+i_stag
    indices(3) = 1
    indices(4) = y_halo
    global_size(1) = npx-1+i_stag
    global_size(2) = y_halo
    global_size(3) = Atm%npz
!modify starts and ends for certain pes
    if (is.eq.1)     indices(1) = is
    if (ie.eq.npx-1) indices(2) = ie+i_stag
    x_halo_ns = 0
    if (is.eq.1) x_halo_ns=x_halo

!define south root_pe
    is_root_pe = .FALSE.
    if (is.eq.1 .and. js.eq.1) is_root_pe = .TRUE.
!register south halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_south_t1', &
                                        var_bc%south_t1, & 
                                        indices, global_size, x2_pelist, &
                                        is_root_pe, x_halo=x_halo_ns, mandatory=mandatory)
!register south prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_sw, trim(fname_sw), &
                                        trim(var_name)//'_south', &
                                        var, indices, global_size, &
                                        x2_pelist, is_root_pe, x_halo=x_halo_ns, mandatory=mandatory)

!define north root_pe
    is_root_pe = .FALSE.
    if (ie.eq.npx-1 .and. je.eq.npy-1) is_root_pe = .TRUE.
!register north halo data in t1
    if (present(var_bc)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_north_t1', &
                                        var_bc%north_t1, & 
                                        indices, global_size, x1_pelist, &
                                        is_root_pe, x_halo=x_halo_ns, mandatory=mandatory)

!reset indices for prognostic variables in the north halo
    indices(3) = jed-y_halo+1+j_stag
    indices(4) = jed+j_stag
!register north prognostic halo data
    if (present(var)) id_restart = register_restart_field(BCfile_ne, trim(fname_ne), &
                                        trim(var_name)//'_north', &
                                        var, indices, global_size, &
                                        x1_pelist, is_root_pe, x_halo=x_halo_ns, &
                                        y_halo=(size(var,2)-y_halo), jshift=-(je+j_stag), mandatory=mandatory)

  end subroutine register_bcs_3d

!>@brief The subroutine 'fv_io_register_restart_BCs' registers restarts for 
!! nested-grid boundary conditions.
  subroutine fv_io_register_restart_BCs(Atm)
    type(fv_atmos_type),        intent(inout) :: Atm

    integer :: n, ntracers, ntprog, ntdiag
    character(len=120) :: tname, fname_ne, fname_sw

    fname_ne = 'fv_BC_ne.res.nc'
    fname_sw = 'fv_BC_sw.res.nc'

    ntprog=size(Atm%q,4)
    ntdiag=size(Atm%qdiag,4)
    ntracers=ntprog+ntdiag

    call set_domain(Atm%domain)

    call register_bcs_2d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'phis', var=Atm%phis)
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'delp', Atm%delp, Atm%neststruct%delp_BC)
    do n=1,ntprog
       call get_tracer_names(MODEL_ATMOS, n, tname)
       call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                            fname_ne, fname_sw, trim(tname), Atm%q(:,:,:,n), Atm%neststruct%q_BC(n), mandatory=.false.)
    enddo
    do n=ntprog+1,ntracers
       call get_tracer_names(MODEL_ATMOS, n, tname)
       call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                            fname_ne, fname_sw, trim(tname), var=Atm%qdiag(:,:,:,n), mandatory=.false.)
    enddo
#ifndef SW_DYNAMICS
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'pt', Atm%pt, Atm%neststruct%pt_BC)
    if ((.not.Atm%flagstruct%hydrostatic) .and. (.not.Atm%flagstruct%make_nh)) then
       if (is_master()) print*, 'fv_io_register_restart_BCs: REGISTERING NH BCs', Atm%flagstruct%hydrostatic, Atm%flagstruct%make_nh
      call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                           fname_ne, fname_sw, 'w', Atm%w, Atm%neststruct%w_BC, mandatory=.false.)
      call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                           fname_ne, fname_sw, 'delz', Atm%delz, Atm%neststruct%delz_BC, mandatory=.false.)
    endif
#ifdef USE_COND
       call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                            fname_ne, fname_sw,'q_con', var_bc=Atm%neststruct%q_con_BC, mandatory=.false.)
#ifdef MOIST_CAPPA
       call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
            fname_ne, fname_sw, 'cappa', var_bc=Atm%neststruct%cappa_BC, mandatory=.false.)
#endif
#endif
#endif
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'u', Atm%u, Atm%neststruct%u_BC, jstag=1)
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'v', Atm%v, Atm%neststruct%v_BC, istag=1)
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'uc', var_bc=Atm%neststruct%uc_BC, istag=1)
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'vc', var_bc=Atm%neststruct%vc_BC, jstag=1)
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
                         fname_ne, fname_sw, 'divg', var_bc=Atm%neststruct%divg_BC, istag=1,jstag=1, mandatory=.false.)
    Atm%neststruct%divg_BC%initialized = field_exist(fname_ne, 'divg_north_t1', Atm%domain)


    return
  end subroutine fv_io_register_restart_BCs


  subroutine fv_io_register_restart_BCs_NH(Atm)
    type(fv_atmos_type),        intent(inout) :: Atm

    integer :: n
    character(len=120) :: tname, fname_ne, fname_sw

    fname_ne = 'fv_BC_ne.res.nc'
    fname_sw = 'fv_BC_sw.res.nc'

    call set_domain(Atm%domain)

    if (is_master()) print*, 'fv_io_register_restart_BCs_NH: REGISTERING NH BCs', Atm%flagstruct%hydrostatic, Atm%flagstruct%make_nh
#ifndef SW_DYNAMICS
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
         fname_ne, fname_sw, 'w', Atm%w, Atm%neststruct%w_BC)
    call register_bcs_3d(Atm, Atm%neststruct%BCfile_ne, Atm%neststruct%BCfile_sw, &
         fname_ne, fname_sw, 'delz', Atm%delz, Atm%neststruct%delz_BC)
#endif

    return
  end subroutine fv_io_register_restart_BCs_NH


!>@brief The subroutine 'fv_io_write_BCs' writes BCs to a restart file.
  subroutine fv_io_write_BCs(Atm, timestamp)
    type(fv_atmos_type), intent(inout) :: Atm
    character(len=*),    intent(in), optional :: timestamp

    call save_restart_border(Atm%neststruct%BCfile_ne, timestamp)
    call save_restart_border(Atm%neststruct%BCfile_sw, timestamp)

    return
  end subroutine fv_io_write_BCs

!>@brief The subroutine 'fv_io_read_BCs' reads BCs from a restart file.
  subroutine fv_io_read_BCs(Atm)
    type(fv_atmos_type), intent(inout) :: Atm

    call restore_state_border(Atm%neststruct%BCfile_ne)
    call restore_state_border(Atm%neststruct%BCfile_sw)

    return
  end subroutine fv_io_read_BCs

end module fv_io_mod
