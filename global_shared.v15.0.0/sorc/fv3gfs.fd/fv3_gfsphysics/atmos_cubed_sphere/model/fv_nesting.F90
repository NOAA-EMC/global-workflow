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
module fv_nesting_mod

   use mpp_domains_mod,     only: mpp_update_domains
   use mpp_domains_mod,     only: mpp_global_field
   use field_manager_mod,   only: MODEL_ATMOS
   use tracer_manager_mod,  only: get_tracer_index
   use fv_sg_mod,           only: neg_adj3
   use mpp_domains_mod,     only: mpp_get_data_domain, mpp_get_compute_domain, mpp_get_global_domain
   use mpp_domains_mod,     only: DGRID_NE, mpp_update_domains, domain2D
   use fv_restart_mod,      only: d2a_setup, d2c_setup
   use mpp_mod,             only: mpp_sync_self, mpp_sync, mpp_send, mpp_recv, mpp_error, FATAL
   use mpp_domains_mod,     only: mpp_global_sum, BITWISE_EFP_SUM, BITWISE_EXACT_SUM
   use boundary_mod,        only: update_coarse_grid
   use boundary_mod,        only: nested_grid_BC_send, nested_grid_BC_recv, nested_grid_BC_save_proc
   use fv_mp_mod,           only: is, ie, js, je, isd, ied, jsd, jed, isc, iec, jsc, jec
   use fv_arrays_mod,       only: fv_grid_type, fv_flags_type, fv_atmos_type, fv_nest_type, fv_diag_type, fv_nest_BC_type_3D
   use fv_arrays_mod,       only: allocate_fv_nest_BC_type, fv_atmos_type, fv_grid_bounds_type
   use fv_grid_utils_mod,   only: ptop_min, g_sum, cubed_to_latlon, f_p
   use init_hydro_mod,      only: p_var
   use constants_mod,       only: grav, pi=>pi_8, radius, hlv, rdgas, cp_air, rvgas, cp_vapor, kappa
   use fv_mapz_mod,         only: mappm
   use fv_timing_mod,       only: timing_on, timing_off
   use fv_mp_mod,           only: is_master
   use fv_mp_mod,           only: mp_reduce_sum
   use fv_diagnostics_mod,  only: sphum_ll_fix
   use sw_core_mod,         only: divergence_corner, divergence_corner_nest

implicit none
   logical :: RF_initialized = .false.
   logical :: bad_range
   real, allocatable ::  rf(:), rw(:)
   integer :: kmax=1
   !Arrays for global grid total energy, used for grid nesting
   real, allocatable :: te_2d_coarse(:,:)
   real, allocatable :: dp1_coarse(:,:,:)

   !For nested grid buffers
	!Individual structures are allocated by nested_grid_BC_recv
   type(fv_nest_BC_type_3d) :: u_buf, v_buf, uc_buf, vc_buf, delp_buf, delz_buf, pt_buf, pkz_buf, w_buf, divg_buf
   type(fv_nest_BC_type_3d), allocatable:: q_buf(:)
private
public :: twoway_nesting, setup_nested_grid_BCs

!---- version number -----
   character(len=128) :: version = '$Id$'
   character(len=128) :: tagname = '$Name$'

contains

 subroutine setup_nested_grid_BCs(npx, npy, npz, zvir, ncnst,     &
                        u, v, w, pt, delp, delz,q, uc, vc, pkz, &
                        nested, inline_q, make_nh, ng, &
                        gridstruct, flagstruct, neststruct, &
                        nest_timestep, tracer_nest_timestep, &
                        domain, bd, nwat)

   
    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(IN) :: zvir

    integer, intent(IN) :: npx, npy, npz
    integer, intent(IN) :: ncnst, ng, nwat
    logical, intent(IN) :: inline_q, make_nh,nested

    real, intent(inout), dimension(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) :: u ! D grid zonal wind (m/s)
    real, intent(inout), dimension(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) :: v ! D grid meridional wind (m/s)
    real, intent(inout) :: w(   bd%isd:        ,bd%jsd:        ,1:)  !  W (m/s)
    real, intent(inout) :: pt(  bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! temperature (K)
    real, intent(inout) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! pressure thickness (pascal)
    real, intent(inout) :: delz(bd%isd:        ,bd%jsd:        ,1:)  ! height thickness (m)
    real, intent(inout) :: q(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, ncnst) ! specific humidity and constituents
    real, intent(inout) :: uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) ! (uc,vc) mostly used as the C grid winds
    real, intent(inout) :: vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
    real, intent(inout) :: pkz (bd%is:bd%ie,bd%js:bd%je,npz)             ! finite-volume mean pk
    integer, intent(INOUT) :: nest_timestep, tracer_nest_timestep

    type(fv_grid_type), intent(INOUT) :: gridstruct
    type(fv_flags_type), intent(INOUT) :: flagstruct
    type(fv_nest_type), intent(INOUT), target :: neststruct
    type(domain2d), intent(INOUT) :: domain
    real :: divg(bd%isd:bd%ied+1,bd%jsd:bd%jed+1, npz)
    real :: ua(bd%isd:bd%ied,bd%jsd:bd%jed)
    real :: va(bd%isd:bd%ied,bd%jsd:bd%jed)

    real :: pkz_coarse(  bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
    integer :: i,j,k,n,p, sphum
    logical :: do_pd

   type(fv_nest_BC_type_3d) :: pkz_BC

    !local pointers
    logical, pointer :: child_grids(:)

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

    child_grids => neststruct%child_grids
    

    !IF nested, set up nested grid BCs for time-interpolation
    !(actually applying the BCs is done in dyn_core

    nest_timestep = 0
    if (.not. inline_q) tracer_nest_timestep = 0


    if (neststruct%nested .and. (.not. (neststruct%first_step) .or. make_nh) ) then
       do_pd = .true.
       call set_BCs_t0(ncnst, flagstruct%hydrostatic, neststruct) 
    else
       !On first timestep the t0 BCs are not initialized and may contain garbage
       do_pd = .false.
    end if

    !compute uc/vc for nested-grid BCs
    !!! CLEANUP: if we compute uc/vc here we don't need to do on the first call of c_sw, right?
    if (ANY(neststruct%child_grids)) then
       call timing_on('COMM_TOTAL')
       !!! CLEANUP: could we make this a non-blocking operation?
       !!! Is this needed? it is on the initialization step.
       call mpp_update_domains(u, v, &
            domain, gridtype=DGRID_NE, complete=.true.)
       call timing_off('COMM_TOTAL')
!$OMP parallel do default(none) shared(isd,jsd,ied,jed,is,ie,js,je,npx,npy,npz, &
!$OMP       gridstruct,flagstruct,bd,u,v,uc,vc,nested,divg) &
!$OMP       private(ua,va)
       do k=1,npz
          call d2c_setup(u(isd,jsd,k),  v(isd,jsd,k),   &
               ua, va, &
               uc(isd,jsd,k), vc(isd,jsd,k), flagstruct%nord>0, &
               isd,ied,jsd,jed, is,ie,js,je, npx,npy, &
               gridstruct%grid_type, gridstruct%nested, &
               gridstruct%se_corner, gridstruct%sw_corner, &
               gridstruct%ne_corner, gridstruct%nw_corner, &
               gridstruct%rsin_u, gridstruct%rsin_v, &
               gridstruct%cosa_s, gridstruct%rsin2 )
          if (nested) then
             call divergence_corner_nest(u(isd,jsd,k), v(isd,jsd,k), ua, va, divg(isd,jsd,k), gridstruct, flagstruct, bd)
          else
             call divergence_corner(u(isd,jsd,k), v(isd,jsd,k), ua, va, divg(isd,jsd,k), gridstruct, flagstruct, bd)
          endif
       end do       
    endif

#ifndef SW_DYNAMICS
    if (flagstruct%hydrostatic) then
!$OMP parallel do default(none) shared(npz,is,ie,js,je,pkz,pkz_coarse)
       do k=1,npz
       do j=js,je
       do i=is,ie
          pkz_coarse(i,j,k) = pkz(i,j,k)
       enddo
       enddo
       enddo
    endif
#endif 
!! Nested grid: receive from parent grid
    if (neststruct%nested) then
       if (.not. allocated(q_buf)) then
          allocate(q_buf(ncnst))
       endif

       call nested_grid_BC_recv(neststruct%nest_domain, 0, 0,  npz, bd, &
            delp_buf)
       do n=1,ncnst
          call nested_grid_BC_recv(neststruct%nest_domain, 0, 0, npz, bd, &
               q_buf(n))
       enddo
#ifndef SW_DYNAMICS
       call nested_grid_BC_recv(neststruct%nest_domain, 0, 0, npz, bd, &
            pt_buf)

       if (flagstruct%hydrostatic) then
          call allocate_fv_nest_BC_type(pkz_BC,is,ie,js,je,isd,ied,jsd,jed,npx,npy,npz,ng,0,0,0,.false.)
          call nested_grid_BC_recv(neststruct%nest_domain, 0, 0, npz, bd, &
               pkz_buf)
       else
          call nested_grid_BC_recv(neststruct%nest_domain, 0, 0,  npz, bd, &
               w_buf)
          call nested_grid_BC_recv(neststruct%nest_domain, 0, 0,  npz, bd, &
               delz_buf)
       endif
#endif
       call nested_grid_BC_recv(neststruct%nest_domain, 0, 1,  npz, bd, &
            u_buf)
       call nested_grid_BC_recv(neststruct%nest_domain, 0, 1,  npz, bd, &
            vc_buf)
       call nested_grid_BC_recv(neststruct%nest_domain, 1, 0,  npz, bd, &
            v_buf)
       call nested_grid_BC_recv(neststruct%nest_domain, 1, 0,  npz, bd, &
            uc_buf)
       call nested_grid_BC_recv(neststruct%nest_domain, 1, 1,  npz, bd, &
            divg_buf)
    endif


!! Coarse grid: send to child grids

    do p=1,size(child_grids)
       if (child_grids(p)) then
          call nested_grid_BC_send(delp, neststruct%nest_domain_all(p), 0, 0)
          do n=1,ncnst
             call nested_grid_BC_send(q(:,:,:,n), neststruct%nest_domain_all(p), 0, 0)
          enddo
#ifndef SW_DYNAMICS
          call nested_grid_BC_send(pt, neststruct%nest_domain_all(p), 0, 0)

          if (flagstruct%hydrostatic) then
             !Working with PKZ is more complicated since it is only defined on the interior of the grid.
             call nested_grid_BC_send(pkz_coarse, neststruct%nest_domain_all(p), 0, 0)
          else
             call nested_grid_BC_send(w, neststruct%nest_domain_all(p), 0, 0)
             call nested_grid_BC_send(delz, neststruct%nest_domain_all(p), 0, 0)
          endif          
#endif
          call nested_grid_BC_send(u, neststruct%nest_domain_all(p), 0, 1)
          call nested_grid_BC_send(vc, neststruct%nest_domain_all(p), 0, 1)
          call nested_grid_BC_send(v, neststruct%nest_domain_all(p), 1, 0)
          call nested_grid_BC_send(uc, neststruct%nest_domain_all(p), 1, 0)
       call nested_grid_BC_send(divg, neststruct%nest_domain_all(p), 1, 1)
       endif
    enddo
    
    !Nested grid: do computations
    if (nested) then
       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_h, neststruct%wt_h, 0, 0,  npx, npy, npz, bd, &
            neststruct%delp_BC, delp_buf, pd_in=do_pd)
       do n=1,ncnst
          call nested_grid_BC_save_proc(neststruct%nest_domain, &
               neststruct%ind_h, neststruct%wt_h, 0, 0, npx,  npy,  npz, bd, &
               neststruct%q_BC(n), q_buf(n), pd_in=do_pd)
       enddo
#ifndef SW_DYNAMICS
       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_h, neststruct%wt_h, 0, 0, npx,  npy,  npz, bd, &
            neststruct%pt_BC, pt_buf)

       sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
       if (flagstruct%hydrostatic) then
          call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_h, neststruct%wt_h, 0, 0, npx,  npy,  npz, bd, &
            pkz_BC, pkz_buf)
          call setup_pt_BC(neststruct%pt_BC, pkz_BC, neststruct%q_BC(sphum), npx, npy, npz, zvir, bd)
       else
          call nested_grid_BC_save_proc(neststruct%nest_domain, &
               neststruct%ind_h, neststruct%wt_h, 0, 0,  npx,  npy,  npz, bd, &
               neststruct%w_BC, w_buf)
          call nested_grid_BC_save_proc(neststruct%nest_domain, &
               neststruct%ind_h, neststruct%wt_h, 0, 0,  npx,  npy,  npz, bd, &
               neststruct%delz_BC, delz_buf) !Need a negative-definite method? 
          
          call setup_pt_NH_BC(neststruct%pt_BC, neststruct%delp_BC, neststruct%delz_BC, &
               neststruct%q_BC(sphum), &
#ifdef USE_COND
               neststruct%q_BC, neststruct%q_con_BC, ncnst, &
#ifdef MOIST_CAPPA
               neststruct%cappa_BC, &
#endif
#endif
               npx, npy, npz, zvir, bd)
       endif
#endif
       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_u, neststruct%wt_u, 0, 1,  npx,  npy,  npz, bd, &
            neststruct%u_BC, u_buf)
       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_u, neststruct%wt_u, 0, 1,  npx,  npy,  npz, bd, &
            neststruct%vc_BC, vc_buf)
       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_v, neststruct%wt_v, 1, 0,  npx,  npy,  npz, bd, &
            neststruct%v_BC, v_buf)
       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_v, neststruct%wt_v, 1, 0,  npx,  npy,  npz, bd, &
            neststruct%uc_BC, uc_buf)

       call nested_grid_BC_save_proc(neststruct%nest_domain, &
            neststruct%ind_b, neststruct%wt_b, 1, 1,  npx,  npy,  npz, bd, &
            neststruct%divg_BC, divg_buf)
    endif

    if (neststruct%first_step) then
       if (neststruct%nested) call set_BCs_t0(ncnst, flagstruct%hydrostatic, neststruct)
       neststruct%first_step = .false.
       if (.not. flagstruct%hydrostatic) flagstruct%make_nh= .false. 
    else if (flagstruct%make_nh) then
       if (neststruct%nested) call set_NH_BCs_t0(neststruct)
       flagstruct%make_nh= .false. 
    endif

    !Unnecessary?
!!$    if ( neststruct%nested .and. .not. neststruct%divg_BC%initialized) then
!!$       neststruct%divg_BC%east_t0  = neststruct%divg_BC%east_t1
!!$       neststruct%divg_BC%west_t0  = neststruct%divg_BC%west_t1
!!$       neststruct%divg_BC%north_t0 = neststruct%divg_BC%north_t1
!!$       neststruct%divg_BC%south_t0 = neststruct%divg_BC%south_t1 
!!$       neststruct%divg_BC%initialized = .true.
!!$    endif


    call mpp_sync_self

 end subroutine setup_nested_grid_BCs

 subroutine setup_pt_BC(pt_BC, pkz_BC, sphum_BC, npx, npy, npz, zvir, bd)

   type(fv_grid_bounds_type), intent(IN) :: bd
   type(fv_nest_BC_type_3d), intent(IN), target    :: pkz_BC, sphum_BC
   type(fv_nest_BC_type_3d), intent(INOUT), target :: pt_BC
   integer, intent(IN) :: npx, npy, npz
   real, intent(IN) :: zvir

   real, dimension(:,:,:), pointer :: ptBC, pkzBC, sphumBC

   integer :: i,j,k, istart, iend

   integer :: is,  ie,  js,  je
   integer :: isd, ied, jsd, jed

   is  = bd%is
   ie  = bd%ie
   js  = bd%js
   je  = bd%je
   isd = bd%isd
   ied = bd%ied
   jsd = bd%jsd
   jed = bd%jed
   
   if (is == 1) then
      ptBC    =>    pt_BC%west_t1
      pkzBC   =>   pkz_BC%west_t1
      sphumBC => sphum_BC%west_t1
!$OMP parallel do default(none) shared(npz,jsd,jed,isd,ptBC,pkzBC,zvir,sphumBC)
      do k=1,npz
      do j=jsd,jed
      do i=isd,0
         ptBC(i,j,k) = ptBC(i,j,k)/pkzBC(i,j,k)*(1.+zvir*sphumBC(i,j,k))
      end do
      end do
      end do
   end if

   if (js == 1) then
      ptBC    =>    pt_BC%south_t1
      pkzBC   =>   pkz_BC%south_t1
      sphumBC => sphum_BC%south_t1
      if (is == 1) then
         istart = is
      else
         istart = isd
      end if
      if (ie == npx-1) then
         iend = ie
      else
         iend = ied
      end if

!$OMP parallel do default(none) shared(npz,jsd,istart,iend,ptBC,pkzBC,zvir,sphumBC)
      do k=1,npz
      do j=jsd,0
      do i=istart,iend
         ptBC(i,j,k) = ptBC(i,j,k)/pkzBC(i,j,k) * &
              (1.+zvir*sphumBC(i,j,k))
      end do
      end do
      end do
   end if


   if (ie == npx-1) then
      ptBC    =>    pt_BC%east_t1
      pkzBC   =>   pkz_BC%east_t1
      sphumBC => sphum_BC%east_t1
!$OMP parallel do default(none) shared(npz,jsd,jed,npx,ied,ptBC,pkzBC,zvir,sphumBC)
      do k=1,npz
      do j=jsd,jed
      do i=npx,ied
         ptBC(i,j,k) = ptBC(i,j,k)/pkzBC(i,j,k) * &
              (1.+zvir*sphumBC(i,j,k))
      end do
      end do
      end do
   end if

   if (je == npy-1) then
      ptBC    =>    pt_BC%north_t1
      pkzBC   =>   pkz_BC%north_t1
      sphumBC => sphum_BC%north_t1
      if (is == 1) then
         istart = is
      else
         istart = isd
      end if
      if (ie == npx-1) then
         iend = ie
      else
         iend = ied
      end if

!$OMP parallel do default(none) shared(npz,npy,jed,npx,istart,iend,ptBC,pkzBC,zvir,sphumBC)
      do k=1,npz
      do j=npy,jed
      do i=istart,iend
         ptBC(i,j,k) = ptBC(i,j,k)/pkzBC(i,j,k) * &
              (1.+zvir*sphumBC(i,j,k))
      end do
      end do
      end do
   end if
   
 end subroutine setup_pt_BC

 subroutine setup_pt_NH_BC(pt_BC, delp_BC, delz_BC, sphum_BC, &
#ifdef USE_COND
      q_BC, q_con_BC, nq, &
#ifdef MOIST_CAPPA
      cappa_BC, &
#endif
#endif
      npx, npy, npz, zvir, bd)

   type(fv_grid_bounds_type), intent(IN) :: bd
   type(fv_nest_BC_type_3d), intent(IN), target    :: delp_BC, delz_BC, sphum_BC
   type(fv_nest_BC_type_3d), intent(INOUT), target :: pt_BC
#ifdef USE_COND
   integer, intent(IN) :: nq
   type(fv_nest_BC_type_3d), intent(INOUT), target :: q_con_BC
   type(fv_nest_BC_type_3d), intent(IN), target :: q_BC(nq)
#ifdef MOIST_CAPPA
   type(fv_nest_BC_type_3d), intent(INOUT), target :: cappa_BC
#endif
#endif
   integer, intent(IN) :: npx, npy, npz
   real, intent(IN) :: zvir

    real, parameter:: c_liq = 4190.       ! heat capacity of water at 0C
    real, parameter:: c_ice = 2106.       ! heat capacity of ice at 0C: c=c_ice+7.3*(T-Tice) 
    real, parameter:: cv_vap = cp_vapor - rvgas  ! 1384.5

   real, dimension(:,:,:), pointer :: ptBC, sphumBC, qconBC, delpBC, delzBC, cappaBC
   real, dimension(:,:,:), pointer :: liq_watBC, ice_watBC, rainwatBC, snowwatBC, graupelBC

   real :: dp1, q_liq, q_sol, q_con = 0., cvm, pkz, rdg, cv_air

   integer :: i,j,k, istart, iend
   integer :: liq_wat, ice_wat, rainwat, snowwat, graupel
   real, parameter:: tice = 273.16 ! For GFS Partitioning
   real, parameter:: t_i0 = 15.

   integer :: is,  ie,  js,  je
   integer :: isd, ied, jsd, jed

   is  = bd%is
   ie  = bd%ie
   js  = bd%js
   je  = bd%je
   isd = bd%isd
   ied = bd%ied
   jsd = bd%jsd
   jed = bd%jed
   
   rdg = -rdgas / grav
   cv_air =  cp_air - rdgas

   liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
   ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
   rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
   snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
   graupel = get_tracer_index (MODEL_ATMOS, 'graupel')

   if (is == 1) then
      ptBC    =>    pt_BC%west_t1
      sphumBC => sphum_BC%west_t1
#ifdef USE_COND
      qconBC  => q_con_BC%west_t1
      liq_watBC => q_BC(liq_wat)%west_t1
#ifndef GFS_PHYS
      ice_watBC => q_BC(ice_wat)%west_t1
      rainwatBC => q_BC(rainwat)%west_t1
      snowwatBC => q_BC(snowwat)%west_t1
      graupelBC => q_BC(graupel)%west_t1
#endif
#ifdef MOIST_CAPPA
      cappaBC =>  cappa_BC%west_t1
#endif
#endif
      delpBC  =>  delp_BC%west_t1
      delzBC  =>  delz_BC%west_t1

!$OMP parallel do default(none) shared(npz,jsd,jed,isd,zvir,sphumBC,liq_watBC,rainwatBC,ice_watBC,snowwatBC,graupelBC,qconBC,cappaBC, &
!$OMP      rdg,cv_air,delpBC,delzBC,ptBC) &
!$OMP      private(dp1,q_con,q_liq,q_sol,cvm,pkz)
      do k=1,npz
      do j=jsd,jed
      do i=isd,0
         dp1 = zvir*sphumBC(i,j,k)
#ifdef USE_COND
#ifdef GFS_PHYS
         q_con = liq_watBC(i,j,k)
         q_sol = q_con*max(min((tice-ptBC(i,j,k))/t_i0,1.),0.)
         q_liq = q_con - q_sol
#else
         q_liq = liq_watBC(i,j,k) + rainwatBC(i,j,k)
         q_sol = ice_watBC(i,j,k) + snowwatBC(i,j,k) + graupelBC(i,j,k)
         q_con = q_liq + q_sol
#endif 
         qconBC(i,j,k) = q_con
#ifdef MOIST_CAPPA
         cvm = (1.-(sphumBC(i,j,k)+q_con))*cv_air+sphumBC(i,j,k)*cv_vap+q_liq*c_liq+q_sol*c_ice
         cappaBC(i,j,k) = rdgas/(rdgas + cvm/(1.+dp1))
         pkz = exp( cappaBC(i,j,k)*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))         
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))
#endif
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)*(1.-q_con)/pkz
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)/delzBC(i,j,k)))
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)/pkz
#endif
      end do
      end do
      end do
   end if


   if (js == 1) then
      ptBC    =>    pt_BC%south_t1
      sphumBC => sphum_BC%south_t1
#ifdef USE_COND
      qconBC  => q_con_BC%south_t1
      liq_watBC => q_BC(liq_wat)%south_t1
#ifndef GFS_PHYS
      ice_watBC => q_BC(ice_wat)%south_t1
      rainwatBC => q_BC(rainwat)%south_t1
      snowwatBC => q_BC(snowwat)%south_t1
      graupelBC => q_BC(graupel)%south_t1
#endif
#ifdef MOIST_CAPPA
      cappaBC =>  cappa_BC%south_t1
#endif
#endif
      delpBC  =>  delp_BC%south_t1
      delzBC  =>  delz_BC%south_t1
      if (is == 1) then
         istart = is
      else
         istart = isd
      end if
      if (ie == npx-1) then
         iend = ie
      else
         iend = ied
      end if

!$OMP parallel do default(none) shared(npz,jsd,istart,iend,zvir,sphumBC,liq_watBC,rainwatBC,ice_watBC,snowwatBC,graupelBC,qconBC,cappaBC, &
!$OMP      rdg,cv_air,delpBC,delzBC,ptBC) &
!$OMP      private(dp1,q_con,q_liq,q_sol,cvm,pkz)
      do k=1,npz
      do j=jsd,0
      do i=istart,iend
         dp1 = zvir*sphumBC(i,j,k)
#ifdef USE_COND
#ifdef GFS_PHYS
         q_con = liq_watBC(i,j,k)
         q_sol = q_con*max(min((tice-ptBC(i,j,k))/t_i0,1.),0.)
         q_liq = q_con - q_sol
#else
         q_liq = liq_watBC(i,j,k) + rainwatBC(i,j,k)
         q_sol = ice_watBC(i,j,k) + snowwatBC(i,j,k) + graupelBC(i,j,k)
         q_con = q_liq + q_sol
#endif 
         qconBC(i,j,k) = q_con
#ifdef MOIST_CAPPA
         cvm = (1.-(sphumBC(i,j,k)+q_con))*cv_air+sphumBC(i,j,k)*cv_vap+q_liq*c_liq+q_sol*c_ice
         cappaBC(i,j,k) = rdgas/(rdgas + cvm/(1.+dp1))
         pkz = exp( cappaBC(i,j,k)*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))         
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))
#endif
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)*(1.-q_con)/pkz
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)/delzBC(i,j,k)))
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)/pkz
#endif
      end do
      end do
      end do
   end if


   if (ie == npx-1) then
      ptBC    =>    pt_BC%east_t1
      sphumBC => sphum_BC%east_t1
#ifdef USE_COND
      qconBC  => q_con_BC%east_t1
      liq_watBC => q_BC(liq_wat)%east_t1
#ifndef GFS_PHYS
      ice_watBC => q_BC(ice_wat)%east_t1
      rainwatBC => q_BC(rainwat)%east_t1
      snowwatBC => q_BC(snowwat)%east_t1
      graupelBC => q_BC(graupel)%east_t1
#endif
#ifdef MOIST_CAPPA
      cappaBC =>  cappa_BC%east_t1
#endif
#endif
      delpBC  =>  delp_BC%east_t1
      delzBC  =>  delz_BC%east_t1
!OMP parallel do default(none) shared(npz,jsd,jed,npx,ied,zvir,sphumBC,liq_watBC,ice_watBC,snowwatBC,graupelBC,qconBC,cappaBC, &
!OMP      rdg,cv_air,delpBC,delzBC,ptBC) &
!OMP      private(dp1,q_con,q_liq,q_sol,cvm,pkz)
      do k=1,npz
      do j=jsd,jed
      do i=npx,ied
         dp1 = zvir*sphumBC(i,j,k)
#ifdef USE_COND
#ifdef GFS_PHYS
         q_con = liq_watBC(i,j,k)
         q_sol = q_con*max(min((tice-ptBC(i,j,k))/t_i0,1.),0.)
         q_liq = q_con - q_sol
#else
         q_liq = liq_watBC(i,j,k) + rainwatBC(i,j,k)
         q_sol = ice_watBC(i,j,k) + snowwatBC(i,j,k) + graupelBC(i,j,k)
         q_con = q_liq + q_sol
#endif 
         qconBC(i,j,k) = q_con
#ifdef MOIST_CAPPA
         cvm = (1.-(sphumBC(i,j,k)+q_con))*cv_air+sphumBC(i,j,k)*cv_vap+q_liq*c_liq+q_sol*c_ice
         cappaBC(i,j,k) = rdgas/(rdgas + cvm/(1.+dp1))
         pkz = exp( cappaBC(i,j,k)*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))         
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))
#endif
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)*(1.-q_con)/pkz
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)/delzBC(i,j,k)))
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)/pkz
#endif
      end do
      end do
      end do
   end if

   if (je == npy-1) then
      ptBC    =>    pt_BC%north_t1
      sphumBC => sphum_BC%north_t1
#ifdef USE_COND
      qconBC  => q_con_BC%north_t1
      liq_watBC => q_BC(liq_wat)%north_t1
#ifndef GFS_PHYS
      ice_watBC => q_BC(ice_wat)%north_t1
      rainwatBC => q_BC(rainwat)%north_t1
      snowwatBC => q_BC(snowwat)%north_t1
      graupelBC => q_BC(graupel)%north_t1
#endif
#ifdef MOIST_CAPPA
      cappaBC =>  cappa_BC%north_t1
#endif
#endif
      delpBC  =>  delp_BC%north_t1
      delzBC  =>  delz_BC%north_t1
      if (is == 1) then
         istart = is
      else
         istart = isd
      end if
      if (ie == npx-1) then
         iend = ie
      else
         iend = ied
      end if

!OMP parallel do default(none) shared(npz,npy,jed,istart,iend,zvir,sphumBC,liq_watBC,ice_watBC,snowwatBC,graupelBC,qconBC,cappaBC, &
!OMP      rdg,cv_air,delpBC,delzBC,ptBC) &
!OMP      private(dp1,q_con,q_liq,q_sol,cvm,pkz)
      do k=1,npz
      do j=npy,jed
      do i=istart,iend
         dp1 = zvir*sphumBC(i,j,k)
#ifdef USE_COND
#ifdef GFS_PHYS
         q_con = liq_watBC(i,j,k)
         q_sol = q_con*max(min((tice-ptBC(i,j,k))/t_i0,1.),0.)
         q_liq = q_con - q_sol
#else
         q_liq = liq_watBC(i,j,k) + rainwatBC(i,j,k)
         q_sol = ice_watBC(i,j,k) + snowwatBC(i,j,k) + graupelBC(i,j,k)
         q_con = q_liq + q_sol
#endif 
         qconBC(i,j,k) = q_con
#ifdef MOIST_CAPPA
         cvm = (1.-(sphumBC(i,j,k)+q_con))*cv_air+sphumBC(i,j,k)*cv_vap+q_liq*c_liq+q_sol*c_ice
         cappaBC(i,j,k) = rdgas/(rdgas + cvm/(1.+dp1))
         pkz = exp( cappaBC(i,j,k)*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))         
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)*(1.-q_con)/delzBC(i,j,k)))
#endif
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)*(1.-q_con)/pkz
#else
         pkz = exp( kappa*log(rdg*delpBC(i,j,k)*ptBC(i,j,k) * &
              (1.+dp1)/delzBC(i,j,k)))
         ptBC(i,j,k) = ptBC(i,j,k)*(1.+dp1)/pkz
#endif
      end do
      end do
      end do
   end if



 end subroutine setup_pt_NH_BC


 subroutine set_NH_BCs_t0(neststruct)

   type(fv_nest_type), intent(INOUT) :: neststruct

#ifndef SW_DYNAMICS
   neststruct%delz_BC%east_t0  = neststruct%delz_BC%east_t1
   neststruct%delz_BC%west_t0  = neststruct%delz_BC%west_t1
   neststruct%delz_BC%north_t0 = neststruct%delz_BC%north_t1
   neststruct%delz_BC%south_t0 = neststruct%delz_BC%south_t1

   neststruct%w_BC%east_t0  = neststruct%w_BC%east_t1
   neststruct%w_BC%west_t0  = neststruct%w_BC%west_t1
   neststruct%w_BC%north_t0 = neststruct%w_BC%north_t1
   neststruct%w_BC%south_t0 = neststruct%w_BC%south_t1
#endif

 end subroutine set_NH_BCs_t0

 subroutine set_BCs_t0(ncnst, hydrostatic, neststruct)

   integer, intent(IN) :: ncnst
   logical, intent(IN) :: hydrostatic
   type(fv_nest_type), intent(INOUT) :: neststruct

   integer :: n

   neststruct%delp_BC%east_t0  = neststruct%delp_BC%east_t1
   neststruct%delp_BC%west_t0  = neststruct%delp_BC%west_t1
   neststruct%delp_BC%north_t0 = neststruct%delp_BC%north_t1
   neststruct%delp_BC%south_t0 = neststruct%delp_BC%south_t1
   do n=1,ncnst
      neststruct%q_BC(n)%east_t0  = neststruct%q_BC(n)%east_t1
      neststruct%q_BC(n)%west_t0  = neststruct%q_BC(n)%west_t1
      neststruct%q_BC(n)%north_t0 = neststruct%q_BC(n)%north_t1
      neststruct%q_BC(n)%south_t0 = neststruct%q_BC(n)%south_t1
   enddo
#ifndef SW_DYNAMICS
   neststruct%pt_BC%east_t0    = neststruct%pt_BC%east_t1
   neststruct%pt_BC%west_t0    = neststruct%pt_BC%west_t1
   neststruct%pt_BC%north_t0   = neststruct%pt_BC%north_t1
   neststruct%pt_BC%south_t0   = neststruct%pt_BC%south_t1
   neststruct%pt_BC%east_t0    = neststruct%pt_BC%east_t1
   neststruct%pt_BC%west_t0    = neststruct%pt_BC%west_t1
   neststruct%pt_BC%north_t0   = neststruct%pt_BC%north_t1
   neststruct%pt_BC%south_t0   = neststruct%pt_BC%south_t1

#ifdef USE_COND
   neststruct%q_con_BC%east_t0    = neststruct%q_con_BC%east_t1
   neststruct%q_con_BC%west_t0    = neststruct%q_con_BC%west_t1
   neststruct%q_con_BC%north_t0   = neststruct%q_con_BC%north_t1
   neststruct%q_con_BC%south_t0   = neststruct%q_con_BC%south_t1
#ifdef MOIST_CAPPA
   neststruct%cappa_BC%east_t0    = neststruct%cappa_BC%east_t1
   neststruct%cappa_BC%west_t0    = neststruct%cappa_BC%west_t1
   neststruct%cappa_BC%north_t0   = neststruct%cappa_BC%north_t1
   neststruct%cappa_BC%south_t0   = neststruct%cappa_BC%south_t1
#endif
#endif

   if (.not. hydrostatic) then
      call set_NH_BCs_t0(neststruct)
   endif
#endif
   neststruct%u_BC%east_t0  = neststruct%u_BC%east_t1
   neststruct%u_BC%west_t0  = neststruct%u_BC%west_t1
   neststruct%u_BC%north_t0 = neststruct%u_BC%north_t1
   neststruct%u_BC%south_t0 = neststruct%u_BC%south_t1
   neststruct%v_BC%east_t0  = neststruct%v_BC%east_t1
   neststruct%v_BC%west_t0  = neststruct%v_BC%west_t1
   neststruct%v_BC%north_t0 = neststruct%v_BC%north_t1
   neststruct%v_BC%south_t0 = neststruct%v_BC%south_t1


   neststruct%vc_BC%east_t0  = neststruct%vc_BC%east_t1
   neststruct%vc_BC%west_t0  = neststruct%vc_BC%west_t1
   neststruct%vc_BC%north_t0 = neststruct%vc_BC%north_t1
   neststruct%vc_BC%south_t0 = neststruct%vc_BC%south_t1
   neststruct%uc_BC%east_t0  = neststruct%uc_BC%east_t1
   neststruct%uc_BC%west_t0  = neststruct%uc_BC%west_t1
   neststruct%uc_BC%north_t0 = neststruct%uc_BC%north_t1
   neststruct%uc_BC%south_t0 = neststruct%uc_BC%south_t1

   neststruct%divg_BC%east_t0  = neststruct%divg_BC%east_t1
   neststruct%divg_BC%west_t0  = neststruct%divg_BC%west_t1
   neststruct%divg_BC%north_t0 = neststruct%divg_BC%north_t1
   neststruct%divg_BC%south_t0 = neststruct%divg_BC%south_t1


 end subroutine set_BCs_t0


!! nestupdate types
!! 1 - Interpolation update on all variables
!! 2 - Conserving update (over areas on cell-
!!     centered variables, over faces on winds) on all variables
!! 3 - Interpolation update on winds only
!! 4 - Interpolation update on all variables except delp (mass conserving)
!! 5 - Remap interpolating update, delp not updated
!! 6 - Remap conserving update, delp not updated
!! 7 - Remap conserving update, delp and q not updated
!! 8 - Remap conserving update, only winds updated

!! Note that nestupdate > 3 will not update delp.

!! "Remap update" remaps updated variables from the nested grid's
!!  vertical coordinate to that of the coarse grid. When delp is not
!!  updated (nestbctype >= 3) the vertical coordinates differ on
!!  the two grids, because the surface pressure will be different
!!  on the two grids.
!! Note: "conserving updates" do not guarantee global conservation
!!  unless flux nested grid BCs are specified, or if a quantity is
!!  not updated at all. For tracers this requires setting
!!  nestbctype > 1; this is not implemented for air mass (delp)

subroutine twoway_nesting(Atm, ngrids, grids_on_this_pe, zvir, dt_atmos)

   type(fv_atmos_type), intent(INOUT) :: Atm(ngrids)
   integer, intent(IN) :: ngrids
   logical, intent(IN) :: grids_on_this_pe(ngrids)
   real, intent(IN) :: zvir, dt_atmos

   integer :: n, p, sphum

   
   if (ngrids > 1) then

      do n=ngrids,2,-1 !loop backwards to allow information to propagate from finest to coarsest grids

         !two-way updating    
         if (Atm(n)%neststruct%twowaynest ) then
            if  (grids_on_this_pe(n) .or. grids_on_this_pe(Atm(n)%parent_grid%grid_number)) then
               sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
               call twoway_nest_update(Atm(n)%npx, Atm(n)%npy, Atm(n)%npz, zvir, &
                    Atm(n)%ncnst, sphum, Atm(n)%u, Atm(n)%v, Atm(n)%w, Atm(n)%omga, &
                    Atm(n)%pt, Atm(n)%delp, Atm(n)%q, Atm(n)%uc, Atm(n)%vc, &
                    Atm(n)%pkz, Atm(n)%delz, Atm(n)%ps, Atm(n)%ptop, &
                    Atm(n)%gridstruct, Atm(n)%flagstruct, Atm(n)%neststruct, Atm(n)%parent_grid, Atm(N)%bd, .false.)
            endif
         endif

      end do

      !NOTE: these routines need to be used with any grid which has been updated to, not just the coarsest grid.
      do n=1,ngrids
         if (Atm(n)%neststruct%parent_of_twoway .and. grids_on_this_pe(n)) then
            call after_twoway_nest_update( Atm(n)%npx, Atm(n)%npy, Atm(n)%npz, Atm(n)%ng, Atm(n)%ncnst,   &
                 Atm(n)%u,  Atm(n)%v,  Atm(n)%w,  Atm(n)%delz, &
                 Atm(n)%pt,  Atm(n)%delp,  Atm(n)%q,   &
                 Atm(n)%ps,  Atm(n)%pe,  Atm(n)%pk,  Atm(n)%peln,  Atm(n)%pkz, &
                 Atm(n)%phis,  Atm(n)%ua,  Atm(n)%va,  &
                 Atm(n)%ptop, Atm(n)%gridstruct, Atm(n)%flagstruct, &
                 Atm(n)%domain, Atm(n)%bd)
         endif
      enddo

   endif ! ngrids > 1




  end subroutine twoway_nesting

!!!CLEANUP: this routine assumes that the PARENT GRID has pt = (regular) temperature,
!!!not potential temperature; which may cause problems when updating if this is not the case.
 subroutine twoway_nest_update(npx, npy, npz, zvir, ncnst, sphum,     &
                        u, v, w, omga, pt, delp, q,   &
                        uc, vc, pkz, delz, ps, ptop, &
                        gridstruct, flagstruct, neststruct, &
                        parent_grid, bd, conv_theta_in)

    real, intent(IN) :: zvir, ptop

    integer, intent(IN) :: npx, npy, npz
    integer, intent(IN) :: ncnst, sphum
    logical, intent(IN), OPTIONAL :: conv_theta_in

    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(inout), dimension(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) :: u ! D grid zonal wind (m/s)
    real, intent(inout), dimension(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) :: v ! D grid meridional wind (m/s)
    real, intent(inout) :: w(   bd%isd:        ,bd%jsd:        ,1: )  !  W (m/s)
    real, intent(inout) :: omga(bd%isd:bd%ied,bd%jsd:bd%jed,npz)      ! Vertical pressure velocity (pa/s)
    real, intent(inout) :: pt(  bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! temperature (K)
    real, intent(inout) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! pressure thickness (pascal)
    real, intent(inout) :: q(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, ncnst) ! specific humidity and constituents
    real, intent(inout) :: uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) ! (uc,vc) C grid winds
    real, intent(inout) :: vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)

    real, intent(inout) :: pkz (bd%is:bd%ie,bd%js:bd%je,npz)             ! finite-volume mean pk
    real, intent(inout) :: delz(bd%isd:      ,bd%jsd:      ,1: )   ! delta-height (m); non-hydrostatic only
    real, intent(inout) :: ps  (bd%isd:bd%ied  ,bd%jsd:bd%jed)           ! Surface pressure (pascal)

    type(fv_grid_type), intent(INOUT) :: gridstruct
    type(fv_flags_type), intent(INOUT) :: flagstruct
    type(fv_nest_type), intent(INOUT) :: neststruct

    type(fv_atmos_type), intent(INOUT) :: parent_grid

    real, allocatable :: t_nest(:,:,:), ps0(:,:)
    integer :: i,j,k,n
    integer :: isd_p, ied_p, jsd_p, jed_p, isc_p, iec_p, jsc_p, jec_p
    integer :: isg, ieg, jsg,jeg, npx_p, npy_p
    integer :: istart, iend
    real :: qmass_b, qmass_a, fix = 1.
    logical :: used, conv_theta=.true.

    real :: qdp(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
    real, allocatable :: qdp_coarse(:,:,:)
    real(kind=f_p), allocatable :: q_diff(:,:,:)
    real :: L_sum_b(npz), L_sum_a(npz)
    
    integer :: upoff
    integer :: is,  ie,  js,  je
    integer :: isd, ied, jsd, jed
    integer :: isu, ieu, jsu, jeu

    is  = bd%is
    ie  = bd%ie
    js  = bd%js
    je  = bd%je
    isd = bd%isd
    ied = bd%ied
    jsd = bd%jsd
    jed = bd%jed
    isu = neststruct%isu
    ieu = neststruct%ieu
    jsu = neststruct%jsu
    jeu = neststruct%jeu

    upoff = neststruct%upoff

    !We update actual temperature, not theta.
    !If pt is actual temperature, set conv_theta to .false.
    if (present(conv_theta_in)) conv_theta = conv_theta_in

    if ((.not. neststruct%parent_proc) .and. (.not. neststruct%child_proc)) return

    call mpp_get_data_domain( parent_grid%domain, &
         isd_p,  ied_p,  jsd_p,  jed_p  )
    call mpp_get_compute_domain( parent_grid%domain, &
         isc_p,  iec_p,  jsc_p,  jec_p  )


   !delp/ps

   if (neststruct%nestupdate < 3) then

         call update_coarse_grid(parent_grid%delp, delp, neststruct%nest_domain,&
              neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
              isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
              neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
              npx, npy, npz, 0, 0, &
              neststruct%refinement, neststruct%nestupdate, upoff, 0, &
              neststruct%parent_proc, neststruct%child_proc, parent_grid)

      call mpp_sync!self

#ifdef SW_DYNAMICS
      if (neststruct%parent_proc) then
         do j=jsd_p,jed_p
            do i=isd_p,ied_p

               parent_grid%ps(i,j) = &
                    parent_grid%delp(i,j,1)/grav 

            end do
         end do
      endif
#endif

   end if

   !if (neststruct%nestupdate /= 3 .and. neststruct%nestbctype /= 3) then
   if (neststruct%nestupdate /= 3 .and. neststruct%nestupdate /= 7 .and. neststruct%nestupdate /= 8) then

      allocate(qdp_coarse(isd_p:ied_p,jsd_p:jed_p,npz))
      if (parent_grid%flagstruct%nwat > 0) then
         allocate(q_diff(isd_p:ied_p,jsd_p:jed_p,npz))
         q_diff = 0.
      endif

      do n=1,parent_grid%flagstruct%nwat

         qdp_coarse = 0.
         if (neststruct%child_proc) then
            do k=1,npz
            do j=jsd,jed
            do i=isd,ied
               qdp(i,j,k) = q(i,j,k,n)*delp(i,j,k)
            enddo
            enddo
            enddo
         else
            qdp = 0.
         endif

         if (neststruct%parent_proc) then
            !Add up ONLY region being replaced by nested grid
            do k=1,npz
            do j=jsu,jeu
            do i=isu,ieu
               qdp_coarse(i,j,k) = parent_grid%q(i,j,k,n)*parent_grid%delp(i,j,k)
            enddo
            enddo
            enddo
            call level_sum(qdp_coarse, parent_grid%gridstruct%area, parent_grid%domain, &
                 parent_grid%bd, npz, L_sum_b)
         else
            qdp_coarse = 0.
         endif
         if (neststruct%parent_proc) then
            if (n <= parent_grid%flagstruct%nwat) then
            do k=1,npz
            do j=jsu,jeu
            do i=isu,ieu
               q_diff(i,j,k) = q_diff(i,j,k) - qdp_coarse(i,j,k)
            enddo
            enddo
            enddo
            endif
         endif

            call update_coarse_grid(qdp_coarse, qdp, neststruct%nest_domain, &
                 neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
              isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
              neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
              npx, npy, npz, 0, 0, &
                 neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)

               call mpp_sync!self

         if (neststruct%parent_proc) then
            call level_sum(qdp_coarse, parent_grid%gridstruct%area, parent_grid%domain, &
                 parent_grid%bd, npz, L_sum_a)
            do k=1,npz
               if (L_sum_a(k) > 0.) then
                  fix = L_sum_b(k)/L_sum_a(k)
               do j=jsu,jeu
               do i=isu,ieu
                  !Normalization mass fixer
                  parent_grid%q(i,j,k,n) = qdp_coarse(i,j,k)*fix
            enddo
            enddo
               endif
            enddo
               if (n == 1) sphum_ll_fix = 1. - fix
         endif
         if (neststruct%parent_proc) then
            if (n <= parent_grid%flagstruct%nwat) then
            do k=1,npz
            do j=jsu,jeu
            do i=isu,ieu
               q_diff(i,j,k) = q_diff(i,j,k) + parent_grid%q(i,j,k,n)
            enddo
            enddo
            enddo
            endif
         endif

      end do

         if (neststruct%parent_proc) then
            if (parent_grid%flagstruct%nwat > 0) then
               do k=1,npz
            do j=jsu,jeu
            do i=isu,ieu
               parent_grid%delp(i,j,k) = parent_grid%delp(i,j,k) + q_diff(i,j,k)
            enddo
            enddo
            enddo
         endif

         do n=1,parent_grid%flagstruct%nwat
               do k=1,npz
         do j=jsu,jeu
         do i=isu,ieu
            parent_grid%q(i,j,k,n) = parent_grid%q(i,j,k,n)/parent_grid%delp(i,j,k)
         enddo
         enddo
         enddo               
         enddo
         endif

      deallocate(qdp_coarse)
      if  (allocated(q_diff)) deallocate(q_diff)

   endif

#ifndef SW_DYNAMICS
   if (neststruct%nestupdate /= 3 .and. neststruct%nestupdate /= 8) then

      if (conv_theta) then

         if (neststruct%child_proc) then
            !pt is potential temperature on the nested grid, but actual
            !temperature on the coarse grid. Compute actual temperature
            !on the nested grid, then gather.
            allocate(t_nest(isd:ied,jsd:jed,1:npz))
!$OMP parallel do default(none) shared(npz,js,je,is,ie,t_nest,pt,pkz,zvir,q,sphum)
            do k=1,npz
               do j=js,je
                  do i=is,ie
                     t_nest(i,j,k) = pt(i,j,k)*pkz(i,j,k)/(1.+zvir*q(i,j,k,sphum))
                  enddo
               enddo
            enddo
            deallocate(t_nest)
         endif

            call update_coarse_grid(parent_grid%pt, &
                 t_nest, neststruct%nest_domain, &
                 neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
                 isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
                 neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
                 npx, npy, npz, 0, 0, &
                 neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)
      else

            call update_coarse_grid(parent_grid%pt, &
              pt, neststruct%nest_domain, &
              neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
              isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
              neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
              npx, npy, npz, 0, 0, &
              neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)

      endif !conv_theta

      call mpp_sync!self

      if (.not. flagstruct%hydrostatic) then

            call update_coarse_grid(parent_grid%w, w, neststruct%nest_domain, &
                 neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
                 isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
                 neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
                 npx, npy, npz, 0, 0, &
                 neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)
            !Updating for delz not yet implemented; may be problematic
!!$            call update_coarse_grid(parent_grid%delz, delz, neststruct%nest_domain, &
!!$                 neststruct%ind_update_h, &
!!$                 isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, npz, 0, 0, &
!!$                 neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc)

         call mpp_sync!self

      end if
      
   end if !Neststruct%nestupdate /= 3

#endif

      call update_coarse_grid(parent_grid%u, u, neststruct%nest_domain, &
           neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
           isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
           neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
           npx, npy, npz, 0, 1, &
           neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)

      call update_coarse_grid(parent_grid%v, v, neststruct%nest_domain, &
           neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
           isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
           neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
           npx, npy, npz, 1, 0, &
           neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)

   call mpp_sync!self


#ifndef SW_DYNAMICS
   if (neststruct%nestupdate >= 5 .and. npz > 4) then

      !Use PS0 from nested grid, NOT the full delp. Also we assume the same number of levels on both grids.
      !PS0 should be initially set to be ps so that this routine does NOTHING outside of the update region

      !Re-compute nested (AND COARSE) grid ps

      allocate(ps0(isd_p:ied_p,jsd_p:jed_p))
      if (neststruct%parent_proc) then

         parent_grid%ps = parent_grid%ptop
!This loop appears to cause problems with OMP
!$OMP parallel do default(none) shared(npz,jsd_p,jed_p,isd_p,ied_p,parent_grid)
         do j=jsd_p,jed_p
            do k=1,npz
               do i=isd_p,ied_p
                  parent_grid%ps(i,j) = parent_grid%ps(i,j) + &
                       parent_grid%delp(i,j,k)
               end do
            end do
         end do

         ps0 = parent_grid%ps
      endif

      if (neststruct%child_proc) then

         ps = ptop
!$OMP parallel do default(none) shared(npz,jsd,jed,isd,ied,ps,delp)
         do j=jsd,jed
            do k=1,npz
               do i=isd,ied
                  ps(i,j) = ps(i,j) + delp(i,j,k)
               end do
            end do
         end do
      endif

      call update_coarse_grid(ps0, ps, neststruct%nest_domain, &
              neststruct%ind_update_h, gridstruct%dx, gridstruct%dy, gridstruct%area, &
              isd_p, ied_p, jsd_p, jed_p, isd, ied, jsd, jed, &
              neststruct%isu, neststruct%ieu, neststruct%jsu, neststruct%jeu, &
              npx, npy, 0, 0, &
              neststruct%refinement, neststruct%nestupdate, upoff, 0, neststruct%parent_proc, neststruct%child_proc, parent_grid)

      !!! The mpp version of update_coarse_grid does not return a consistent value of ps
      !!! across PEs, as it does not go into the haloes of a given coarse-grid PE. This
      !!! update_domains call takes care of the problem.

   if (neststruct%parent_proc) then
     call mpp_update_domains(parent_grid%ps, parent_grid%domain, complete=.false.)
     call mpp_update_domains(ps0, parent_grid%domain, complete=.true.)
   endif


      call mpp_sync!self

      if (parent_grid%tile == neststruct%parent_tile) then 

         if (neststruct%parent_proc) then

         !comment out if statement to always remap theta instead of t in the remap-update.
         !(In LtE typically we use remap_t = .true.: remapping t is better (except in
         !idealized simulations with a background uniform theta) since near the top
         !boundary theta is exponential, which is hard to accurately interpolate with a spline
         if (.not. parent_grid%flagstruct%remap_t) then
!$OMP parallel do default(none) shared(npz,jsc_p,jec_p,isc_p,iec_p,parent_grid,zvir,sphum)
            do k=1,npz
               do j=jsc_p,jec_p
                  do i=isc_p,iec_p
                     parent_grid%pt(i,j,k) = &
                          parent_grid%pt(i,j,k)/parent_grid%pkz(i,j,k)*&
                          (1.+zvir*parent_grid%q(i,j,k,sphum))
                  end do
               end do
            end do
         end if
         call update_remap_tq(npz, parent_grid%ak, parent_grid%bk, &
              parent_grid%ps, parent_grid%delp, &
              parent_grid%pt, parent_grid%q, npz, ps0, zvir, parent_grid%ptop, ncnst, &
              isc_p, iec_p, jsc_p, jec_p, isd_p, ied_p, jsd_p, jed_p, .false. ) !neststruct%nestupdate < 7)
         if (.not. parent_grid%flagstruct%remap_t) then
!$OMP parallel do default(none) shared(npz,jsc_p,jec_p,isc_p,iec_p,parent_grid,zvir,sphum)
            do k=1,npz
               do j=jsc_p,jec_p
                  do i=isc_p,iec_p
                     parent_grid%pt(i,j,k) = &
                          parent_grid%pt(i,j,k)*parent_grid%pkz(i,j,k) / &
                          (1.+zvir*parent_grid%q(i,j,k,sphum))
                  end do
               end do
            end do
         end if

         call update_remap_uv(npz, parent_grid%ak, parent_grid%bk, &
              parent_grid%ps, &
              parent_grid%u, &
              parent_grid%v, npz, ps0, &
              isc_p, iec_p, jsc_p, jec_p, isd_p, ied_p, jsd_p, jed_p, parent_grid%ptop)

         endif !neststruct%parent_proc

      end if

      if (allocated(ps0)) deallocate(ps0)

   end if

#endif

 end subroutine twoway_nest_update

 subroutine level_sum(q, area, domain, bd, npz, L_sum)

    integer, intent(IN) :: npz
    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(in) :: area(   bd%isd:bd%ied  ,bd%jsd:bd%jed)
    real, intent(in) ::    q(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
    real, intent(OUT) :: L_sum( npz ) 
    type(domain2d), intent(IN) :: domain
   
    integer :: i, j, k, n
    real :: qA!(bd%is:bd%ie, bd%js:bd%je)

    do k=1,npz
       qA = 0.
       do j=bd%js,bd%je
       do i=bd%is,bd%ie
          !qA(i,j) = q(i,j,k)*area(i,j)
          qA = qA + q(i,j,k)*area(i,j)
       enddo
       enddo
       call mp_reduce_sum(qA)
       L_sum(k) = qA
!       L_sum(k) = mpp_global_sum(domain, qA, flags=BITWISE_EXACT_SUM)
!       L_sum(k) = mpp_global_sum(domain, qA, flags=BITWISE_EFP_SUM) ! doesn't work??
    enddo

 end subroutine level_sum


 subroutine after_twoway_nest_update(npx, npy, npz, ng, ncnst,   &
                        u, v, w, delz, pt, delp, q,              &
                        ps, pe, pk, peln, pkz, phis, ua, va,     &
                        ptop, gridstruct, flagstruct,            &
                        domain, bd)

   type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(IN) :: ptop

    integer, intent(IN) :: ng, npx, npy, npz
    integer, intent(IN) :: ncnst

    real, intent(inout), dimension(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) :: u ! D grid zonal wind (m/s)
    real, intent(inout), dimension(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) :: v ! D grid meridional wind (m/s)
    real, intent(inout) :: w(   bd%isd:        ,bd%jsd:        ,1: )  !  W (m/s)
    real, intent(inout) :: pt(  bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! temperature (K)
    real, intent(inout) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! pressure thickness (pascal)
    real, intent(inout) :: q(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, ncnst) ! specific humidity and constituents
    real, intent(inout) :: delz(bd%isd:        ,bd%jsd:        ,1: )   ! delta-height (m); non-hydrostatic only

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:    
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real, intent(inout) :: ps  (bd%isd:bd%ied  ,bd%jsd:bd%jed)           ! Surface pressure (pascal)
    real, intent(inout) :: pe  (bd%is-1:bd%ie+1, npz+1,bd%js-1:bd%je+1)  ! edge pressure (pascal)
    real, intent(inout) :: pk  (bd%is:bd%ie,bd%js:bd%je, npz+1)          ! pe**cappa
    real, intent(inout) :: peln(bd%is:bd%ie,npz+1,bd%js:bd%je)           ! ln(pe)
    real, intent(inout) :: pkz (bd%is:bd%ie,bd%js:bd%je,npz)             ! finite-volume mean pk
    
!-----------------------------------------------------------------------
! Others:
!-----------------------------------------------------------------------
    real, intent(inout) :: phis(bd%isd:bd%ied,bd%jsd:bd%jed)       ! Surface geopotential (g*Z_surf)

    real, intent(inout), dimension(bd%isd:bd%ied ,bd%jsd:bd%jed ,npz):: ua, va
    type(fv_grid_type), intent(IN) :: gridstruct
    type(fv_flags_type), intent(IN) :: flagstruct
    type(domain2d), intent(INOUT) :: domain


    integer :: is,  ie,  js,  je
    integer :: isd, ied, jsd, jed
    
    is  = bd%is
    ie  = bd%ie
    js  = bd%js
    je  = bd%je
    isd = bd%isd
    ied = bd%ied
    jsd = bd%jsd
    jed = bd%jed

    call cubed_to_latlon(u, v, ua, va, &
         gridstruct, npx, npy, npz, &
         1, gridstruct%grid_type, domain, &
         gridstruct%nested, flagstruct%c2l_ord, bd)

#ifndef SW_DYNAMICS

   !To get coarse grid pkz, etc right after a two-way update so
   !that it is consistent across a restart:
   !(should only be called after doing such an update)

    !! CLEANUP: move to twoway_nest_update??
   call p_var(npz, is, ie, js, je, ptop, ptop_min,  &
        delp, delz, &
        pt, ps, &
        pe, peln,   &
        pk,   pkz, kappa, &
        q, ng, flagstruct%ncnst,  gridstruct%area_64, 0.,  &
        .false.,  .false., & !mountain argument not used
        flagstruct%moist_phys,  flagstruct%hydrostatic, &
        flagstruct%nwat, domain, .false.)

#endif

 end subroutine after_twoway_nest_update

 !Routines for remapping (interpolated) nested-grid data to the coarse-grid's vertical coordinate.

 !This does not yet do anything for the tracers
 subroutine update_remap_tq( npz, ak,  bk,  ps, delp,  t,  q,  &
                      kmd, ps0, zvir, ptop, nq, &
                      is, ie, js, je, isd, ied, jsd, jed, do_q)
  integer, intent(in):: npz, kmd, nq
  real,    intent(in):: zvir, ptop
  real,    intent(in):: ak(npz+1), bk(npz+1)
  real,    intent(in), dimension(isd:ied,jsd:jed):: ps0
  real,    intent(in), dimension(isd:ied,jsd:jed):: ps
  real, intent(in), dimension(isd:ied,jsd:jed,npz):: delp
  real,    intent(inout), dimension(isd:ied,jsd:jed,npz):: t
  real,    intent(inout), dimension(isd:ied,jsd:jed,npz,nq):: q
  integer,  intent(in) ::  is, ie, js, je, isd, ied, jsd, jed
  logical,   intent(in) :: do_q
! local:
  real, dimension(is:ie,kmd):: tp, qp
  real, dimension(is:ie,kmd+1):: pe0, pn0
  real, dimension(is:ie,npz):: qn1
  real, dimension(is:ie,npz+1):: pe1, pn1
  integer i,j,k,iq

!$OMP parallel do default(none) shared(js,je,kmd,is,ie,ak,bk,ps0,q,npz,ptop,do_q,t,ps,nq) &
!$OMP          private(pe0,pn0,pe1,pn1,qp,tp,qn1)
  do 5000 j=js,je

     do k=1,kmd+1
        do i=is,ie
           pe0(i,k) = ak(k) + bk(k)*ps0(i,j)
           pn0(i,k) = log(pe0(i,k))
       enddo
     enddo 
     do k=1,kmd+1
        do i=is,ie
           pe1(i,k) = ak(k) + bk(k)*ps(i,j)
           pn1(i,k) = log(pe1(i,k))
       enddo
     enddo 
     if (do_q) then
        do iq=1,nq
        do k=1,kmd
        do i=is,ie
           qp(i,k) = q(i,j,k,iq)
        enddo
        enddo
        call mappm(kmd, pe0, qp, npz, pe1,  qn1, is,ie, 0, 11, ptop)
        do k=1,npz
           do i=is,ie
              q(i,j,k,iq) = qn1(i,k)
           enddo
        enddo
        enddo
     endif

   do k=1,kmd
      do i=is,ie
         tp(i,k) = t(i,j,k)
      enddo
   enddo
   call mappm(kmd, pn0, tp, npz, pn1, qn1, is,ie, 1, 8, ptop)

        do k=1,npz
           do i=is,ie
              t(i,j,k) = qn1(i,k)
           enddo
        enddo

5000 continue

 end subroutine update_remap_tq

 !remap_uv as-is remaps only a-grid velocities. A new routine has been written to handle staggered grids.
 subroutine update_remap_uv(npz, ak, bk, ps, u, v, kmd, ps0, &
                      is, ie, js, je, isd, ied, jsd, jed, ptop)
  integer, intent(in):: npz
  real,    intent(in):: ak(npz+1), bk(npz+1)
  real,    intent(in):: ps(isd:ied,jsd:jed)
  real,    intent(inout), dimension(isd:ied,jsd:jed+1,npz):: u
  real,    intent(inout), dimension(isd:ied+1,jsd:jed,npz):: v
!
  integer, intent(in):: kmd
  real,    intent(IN) :: ptop
  real,    intent(in):: ps0(isd:ied,jsd:jed)
  integer,  intent(in) ::  is, ie, js, je, isd, ied, jsd, jed
!
! local:
  real, dimension(is:ie+1,kmd+1):: pe0
  real, dimension(is:ie+1,npz+1):: pe1
  real, dimension(is:ie+1,kmd):: qt
  real, dimension(is:ie+1,npz):: qn1
  integer i,j,k

!------
! map u
!------
!$OMP parallel do default(none) shared(js,je,kmd,is,ie,ak,bk,ps,ps0,npz,u,ptop) &
!$OMP          private(pe0,pe1,qt,qn1)
  do j=js,je+1
!------
! Data
!------
     do k=1,kmd+1
       do i=is,ie
          pe0(i,k) = ak(k) + bk(k)*0.5*(ps0(i,j)+ps0(i,j-1))
       enddo
     enddo
!------
! Model
!------
     do k=1,kmd+1
        do i=is,ie
          pe1(i,k) = ak(k) + bk(k)*0.5*(ps(i,j)+ps(i,j-1))
       enddo
     enddo
!------
!Do map
!------
     qt = 0.
      do k=1,kmd
         do i=is,ie
            qt(i,k) = u(i,j,k)
         enddo
      enddo
      qn1 = 0.
      call mappm(kmd, pe0(is:ie,:), qt(is:ie,:), npz, pe1(is:ie,:), qn1(is:ie,:), is,ie, -1, 8, ptop)
      do k=1,npz
         do i=is,ie
            u(i,j,k) = qn1(i,k)
         enddo
      enddo

   end do

!------
! map v
!------
!$OMP parallel do default(none) shared(js,je,kmd,is,ie,ak,bk,ps,ps0,npz,v,ptop) &
!$OMP          private(pe0,pe1,qt,qn1)
   do j=js,je
!------
! Data
!------
     do k=1,kmd+1
        do i=is,ie+1
          pe0(i,k) = ak(k) + bk(k)*0.5*(ps0(i,j)+ps0(i-1,j))
       enddo
     enddo
!------
! Model
!------
     do k=1,kmd+1
        do i=is,ie+1
          pe1(i,k) = ak(k) + bk(k)*0.5*(ps(i,j)+ps(i-1,j))
       enddo
     enddo
!------
!Do map
!------
     qt = 0.
      do k=1,kmd
         do i=is,ie+1
            qt(i,k) = v(i,j,k)
         enddo
      enddo
      qn1 = 0.
      call mappm(kmd, pe0(is:ie+1,:), qt(is:ie+1,:), npz, pe1(is:ie+1,:), qn1(is:ie+1,:), is,ie+1, -1, 8, ptop)
      do k=1,npz
         do i=is,ie+1
            v(i,j,k) = qn1(i,k)
         enddo
      enddo
   end do

 end subroutine update_remap_uv


end module fv_nesting_mod
