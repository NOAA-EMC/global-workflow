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
module dyn_core_mod

  use constants_mod,      only: rdgas, radius, cp_air
  use mpp_mod,            only: mpp_pe 
  use mpp_domains_mod,    only: CGRID_NE, DGRID_NE, mpp_get_boundary, mpp_update_domains,  &
                                domain2d
  use mpp_parameter_mod,  only: CORNER
  use fv_mp_mod,          only: is_master
  use fv_mp_mod,          only: start_group_halo_update, complete_group_halo_update
  use fv_mp_mod,          only: group_halo_update_type
  use sw_core_mod,        only: c_sw, d_sw
  use a2b_edge_mod,       only: a2b_ord2, a2b_ord4
  use nh_core_mod,        only: Riem_Solver3, Riem_Solver_C, update_dz_c, update_dz_d, nest_halo_nh
  use tp_core_mod,        only: copy_corners
  use fv_timing_mod,      only: timing_on, timing_off
  use fv_diagnostics_mod, only: prt_maxmin, fv_time, prt_mxm
#ifdef ROT3
  use fv_update_phys_mod, only: update_dwinds_phys
#endif
#if defined (ADA_NUDGE)
  use fv_ada_nudge_mod,   only: breed_slp_inline_ada
#else
  use fv_nwp_nudge_mod,   only: breed_slp_inline, do_adiabatic_init
#endif
  use diag_manager_mod,   only: send_data
  use fv_arrays_mod,      only: fv_grid_type, fv_flags_type, fv_nest_type, fv_diag_type, fv_grid_bounds_type
  use constants_mod,      only: R_GRID

  use boundary_mod,         only: extrapolation_BC,  nested_grid_BC_apply_intT

#ifdef SW_DYNAMICS
  use test_cases_mod,      only: test_case, case9_forcing1, case9_forcing2
#endif

implicit none
private

public :: dyn_core, del2_cubed, init_ijk_mem

  real :: ptk, peln1, rgrav
  real :: d3_damp
  real, allocatable, dimension(:,:,:) ::  ut, vt, crx, cry, xfx, yfx, divgd, &
                                          zh, du, dv, pkc, delpc, pk3, ptc, gz
  real(kind=R_GRID), parameter :: cnst_0p20=0.20d0

!---- version number -----
  character(len=128) :: version = '$Id$'
  character(len=128) :: tagname = '$Name$'

contains

!-----------------------------------------------------------------------
!     dyn_core :: FV Lagrangian dynamics driver
!-----------------------------------------------------------------------
 
 subroutine dyn_core(npx, npy, npz, ng, sphum, nq, bdt, n_split, zvir, cp, akap, cappa, grav, hydrostatic,  &
                     u,  v,  w, delz, pt, q, delp, pe, pk, phis, ws, omga, ptop, pfull, ua, va, & 
                     uc, vc, mfx, mfy, cx, cy, pkz, peln, q_con, ak, bk, &
                     ks, gridstruct, flagstruct, neststruct, idiag, bd, domain, &
                     init_step, i_pack, end_step, time_total)
    integer, intent(IN) :: npx
    integer, intent(IN) :: npy
    integer, intent(IN) :: npz
    integer, intent(IN) :: ng, nq, sphum
    integer, intent(IN) :: n_split
    real   , intent(IN) :: bdt
    real   , intent(IN) :: zvir, cp, akap, grav
    real   , intent(IN) :: ptop
    logical, intent(IN) :: hydrostatic
    logical, intent(IN) :: init_step, end_step
    real, intent(in) :: pfull(npz)
    real, intent(in),     dimension(npz+1) :: ak, bk
    integer, intent(IN) :: ks
    type(group_halo_update_type), intent(inout) :: i_pack(*)
    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(inout), dimension(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz):: u  ! D grid zonal wind (m/s)
    real, intent(inout), dimension(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz):: v  ! D grid meridional wind (m/s)
    real, intent(inout) :: w(   bd%isd:,bd%jsd:,1:)  ! vertical vel. (m/s)
    real, intent(inout) ::  delz(bd%isd:,bd%jsd:,1:)  ! delta-height (m, negative)
    real, intent(inout) :: cappa(bd%isd:,bd%jsd:,1:) ! moist kappa
    real, intent(inout) :: pt(  bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! temperature (K)
    real, intent(inout) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! pressure thickness (pascal)
    real, intent(inout) :: q(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, nq)  ! 
    real, intent(in), optional:: time_total  ! total time (seconds) since start

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:    
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real, intent(inout):: phis(bd%isd:bd%ied,bd%jsd:bd%jed)      ! Surface geopotential (g*Z_surf)
    real, intent(inout):: pe(bd%is-1:bd%ie+1, npz+1,bd%js-1:bd%je+1)  ! edge pressure (pascal)
    real, intent(inout):: peln(bd%is:bd%ie,npz+1,bd%js:bd%je)          ! ln(pe)
    real, intent(inout):: pk(bd%is:bd%ie,bd%js:bd%je, npz+1)        ! pe**kappa

!-----------------------------------------------------------------------
! Others:
    real,    parameter:: near0 = 1.E-8
    real,    parameter:: huge_r = 1.E40
!-----------------------------------------------------------------------
    real, intent(out  ):: ws(bd%is:bd%ie,bd%js:bd%je)        ! w at surface
    real, intent(inout):: omga(bd%isd:bd%ied,bd%jsd:bd%jed,npz)    ! Vertical pressure velocity (pa/s)
    real, intent(inout):: uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)  ! (uc, vc) are mostly used as the C grid winds
    real, intent(inout):: vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
    real, intent(inout), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,npz):: ua, va
    real, intent(inout):: q_con(bd%isd:, bd%jsd:, 1:)

! The Flux capacitors: accumulated Mass flux arrays
    real, intent(inout)::  mfx(bd%is:bd%ie+1, bd%js:bd%je,   npz)
    real, intent(inout)::  mfy(bd%is:bd%ie  , bd%js:bd%je+1, npz)
! Accumulated Courant number arrays
    real, intent(inout)::  cx(bd%is:bd%ie+1, bd%jsd:bd%jed, npz)
    real, intent(inout)::  cy(bd%isd:bd%ied ,bd%js:bd%je+1, npz)
    real, intent(inout),dimension(bd%is:bd%ie,bd%js:bd%je,npz):: pkz

    type(fv_grid_type),  intent(INOUT), target :: gridstruct
    type(fv_flags_type), intent(IN),    target :: flagstruct
    type(fv_nest_type),  intent(INOUT)         :: neststruct
    type(fv_diag_type),  intent(IN)            :: idiag
    type(domain2d),      intent(INOUT)         :: domain

    real, allocatable, dimension(:,:,:):: pem, heat_source
! Auto 1D & 2D arrays:
    real, dimension(bd%isd:bd%ied,bd%jsd:bd%jed):: ws3, z_rat
    real:: dp_ref(npz)
    real:: zs(bd%isd:bd%ied,bd%jsd:bd%jed)        ! surface height (m)
    real:: p1d(bd%is:bd%ie)
    real:: om2d(bd%is:bd%ie,npz)
    real wbuffer(npy+2,npz)
    real ebuffer(npy+2,npz)
    real nbuffer(npx+2,npz)
    real sbuffer(npx+2,npz)
! ----   For external mode:
    real divg2(bd%is:bd%ie+1,bd%js:bd%je+1)
    real wk(bd%isd:bd%ied,bd%jsd:bd%jed)
    real fz(bd%is: bd%ie+1,bd%js: bd%je+1)
    real heat_s(bd%is:bd%ie,bd%js:bd%je)
    real damp_vt(npz+1)
    integer nord_v(npz+1)
!-------------------------------------
    integer :: hord_m, hord_v, hord_t, hord_p
    integer :: nord_k, nord_w, nord_t
    integer :: ms
!---------------------------------------
    integer :: i,j,k, it, iq, n_con, nf_ke
    integer :: iep1, jep1
    real    :: beta, beta_d, d_con_k, damp_w, damp_t, kgb, cv_air
    real    :: dt, dt2, rdt
    real    :: d2_divg
    real    :: k1k, rdg, mk1
    logical :: last_step, remap_step
    logical used
    real :: split_timestep_bc

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

#ifdef SW_DYNAMICS
    peln1 = 0.
#else
    peln1 = log(ptop)
#endif
    ptk = ptop ** akap
    dt  = bdt / real(n_split)
    dt2 = 0.5*dt
    rdt = 1.0/dt
    ms = max(1, flagstruct%m_split/2)
    beta = flagstruct%beta
    rdg = -rdgas / grav
    cv_air = cp_air - rdgas

! Indexes:
    iep1 = ie + 1
    jep1 = je + 1

    if ( .not.hydrostatic ) then

         rgrav = 1.0/grav
           k1k =  akap / (1.-akap)    ! rg/Cv=0.4

!$OMP parallel do default(none) shared(npz,dp_ref,ak,bk)
       do k=1,npz
          dp_ref(k) = ak(k+1)-ak(k) + (bk(k+1)-bk(k))*1.E5  
       enddo

!$OMP parallel do default(none) shared(isd,ied,jsd,jed,zs,phis,rgrav)
       do j=jsd,jed
          do i=isd,ied
             zs(i,j) = phis(i,j) * rgrav
          enddo
       enddo
    endif

      if ( init_step ) then  ! Start of the big dynamic time stepping

           allocate(    gz(isd:ied, jsd:jed ,npz+1) )
             call init_ijk_mem(isd,ied, jsd,jed, npz+1, gz, huge_r)
           allocate(   pkc(isd:ied, jsd:jed ,npz+1) )
           allocate(   ptc(isd:ied, jsd:jed ,npz ) )
           allocate( crx(is :ie+1, jsd:jed,  npz) )
           allocate( xfx(is :ie+1, jsd:jed,  npz) )
           allocate( cry(isd:ied,  js :je+1, npz) )
           allocate( yfx(isd:ied,  js :je+1, npz) )
           allocate( divgd(isd:ied+1,jsd:jed+1,npz) )
           allocate( delpc(isd:ied, jsd:jed  ,npz  ) )
!                    call init_ijk_mem(isd,ied, jsd,jed, npz, delpc, 0.)
           allocate( ut(isd:ied, jsd:jed, npz) )
!                    call init_ijk_mem(isd,ied, jsd,jed, npz, ut, 0.)
           allocate( vt(isd:ied, jsd:jed, npz) )
!                    call init_ijk_mem(isd,ied, jsd,jed, npz, vt, 0.)

          if ( .not. hydrostatic ) then
               allocate( zh(isd:ied, jsd:jed, npz+1) )
!              call init_ijk_mem(isd,ied, jsd,jed, npz+1, zh, huge_r )
               allocate ( pk3(isd:ied,jsd:jed,npz+1) )
               call init_ijk_mem(isd,ied, jsd,jed, npz+1, pk3, huge_r )
          endif
          if ( beta > near0 ) then
               allocate( du(isd:ied,  jsd:jed+1,npz) )
               call init_ijk_mem(isd,ied,   jsd,jed+1, npz, du, 0.)
               allocate( dv(isd:ied+1,jsd:jed,  npz) )
               call init_ijk_mem(isd,ied+1, jsd,jed  , npz, dv, 0.)
          endif
      endif    ! end init_step

! Empty the "flux capacitors"
    call init_ijk_mem(is, ie+1, js,  je,   npz, mfx, 0.)
    call init_ijk_mem(is, ie  , js,  je+1, npz, mfy, 0.)
    call init_ijk_mem(is, ie+1, jsd, jed,  npz, cx, 0.)
    call init_ijk_mem(isd, ied, js,  je+1, npz, cy, 0.)

    if ( flagstruct%d_con > 1.0E-5 ) then
         allocate( heat_source(isd:ied, jsd:jed, npz) )
         call init_ijk_mem(isd, ied, jsd, jed, npz, heat_source, 0.)
    endif

    if ( flagstruct%convert_ke .or. flagstruct%vtdm4> 1.E-3 ) then
         n_con = npz
    else
         if ( flagstruct%d2_bg_k1 < 1.E-3 ) then
              n_con = 0
         else
              if ( flagstruct%d2_bg_k2 < 1.E-3 ) then
                   n_con = 1
              else
                   n_con = 2
              endif
         endif
    endif



!-----------------------------------------------------
  do it=1,n_split
!-----------------------------------------------------
#ifdef ROT3
     call start_group_halo_update(i_pack(8), u, v, domain, gridtype=DGRID_NE)
#endif
     if ( flagstruct%breed_vortex_inline .or. it==n_split ) then
          remap_step = .true.
     else
          remap_step = .false.
     endif

     if ( flagstruct%fv_debug ) then
          if(is_master()) write(*,*) 'n_split loop, it=', it
          if ( .not. flagstruct%hydrostatic )    &
          call prt_mxm('delz',  delz, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
     endif

     if (gridstruct%nested) then
        !First split timestep has split_timestep_BC = n_split*k_split
        !   to do time-extrapolation on BCs.
        split_timestep_bc = real(n_split*flagstruct%k_split+neststruct%nest_timestep)
     endif

     if ( nq > 0 ) then
                                    call timing_on('COMM_TOTAL')
                                        call timing_on('COMM_TRACER')
         if ( flagstruct%inline_q ) then
                      call start_group_halo_update(i_pack(10), q, domain)
         endif
                                       call timing_off('COMM_TRACER')
                                   call timing_off('COMM_TOTAL')
     endif

     if ( .not. hydrostatic ) then
                             call timing_on('COMM_TOTAL')
         call start_group_halo_update(i_pack(7), w, domain)
                             call timing_off('COMM_TOTAL')

      if ( it==1 ) then
		if (gridstruct%nested) then
!$OMP parallel do default(none) shared(isd,ied,jsd,jed,npz,gz,zs,delz)
         do j=jsd,jed
            do i=isd,ied
               gz(i,j,npz+1) = zs(i,j)
            enddo
            do k=npz,1,-1
               do i=isd,ied
                  gz(i,j,k) = gz(i,j,k+1) - delz(i,j,k)
               enddo
            enddo
         enddo
		else
!$OMP parallel do default(none) shared(is,ie,js,je,npz,gz,zs,delz)
         do j=js,je
            do i=is,ie
               gz(i,j,npz+1) = zs(i,j)
            enddo
            do k=npz,1,-1
               do i=is,ie
                  gz(i,j,k) = gz(i,j,k+1) - delz(i,j,k)
               enddo
            enddo
         enddo
	    endif
                             call timing_on('COMM_TOTAL')
         call start_group_halo_update(i_pack(5), gz,  domain)
                             call timing_off('COMM_TOTAL')
      endif

     endif


#ifdef SW_DYNAMICS
     if (test_case>1) then
#ifdef USE_OLD
     if (test_case==9) call case9_forcing1(phis, time_total)
#endif
#endif

     if ( it==1 ) then
                                       call timing_on('COMM_TOTAL')
          call complete_group_halo_update(i_pack(1), domain)
                                      call timing_off('COMM_TOTAL')
          beta_d = 0.
     else
          beta_d = beta
     endif

     if ( it==n_split .and. end_step ) then
       if ( flagstruct%use_old_omega ) then
          allocate ( pem(is-1:ie+1,npz+1,js-1:je+1) )
!$OMP parallel do default(none) shared(is,ie,js,je,npz,pem,delp,ptop)
         do j=js-1,je+1
            do i=is-1,ie+1
               pem(i,1,j) = ptop
            enddo
            do k=1,npz
               do i=is-1,ie+1
                  pem(i,k+1,j) = pem(i,k,j) + delp(i,j,k)
               enddo
            enddo
         enddo
       endif
          last_step = .true.
     else
          last_step = .false.
     endif
       
                                                     call timing_on('COMM_TOTAL')
     call complete_group_halo_update(i_pack(8), domain)
     if( .not. hydrostatic )  &
          call complete_group_halo_update(i_pack(7), domain)
                                                     call timing_off('COMM_TOTAL')

                                                     call timing_on('c_sw')
!$OMP parallel do default(none) shared(npz,isd,jsd,delpc,delp,ptc,pt,u,v,w,uc,vc,ua,va, &
!$OMP                                  omga,ut,vt,divgd,flagstruct,dt2,hydrostatic,bd,  &
!$OMP                                  gridstruct)
      do k=1,npz
         call c_sw(delpc(isd,jsd,k), delp(isd,jsd,k),  ptc(isd,jsd,k),    &
                      pt(isd,jsd,k),    u(isd,jsd,k),    v(isd,jsd,k),    &
                       w(isd:,jsd:,k),   uc(isd,jsd,k),   vc(isd,jsd,k),    &
                      ua(isd,jsd,k),   va(isd,jsd,k), omga(isd,jsd,k),    &
                      ut(isd,jsd,k),   vt(isd,jsd,k), divgd(isd,jsd,k),   &
                      flagstruct%nord,   dt2,  hydrostatic,  .true., bd,  &
                      gridstruct, flagstruct)
      enddo
                                                     call timing_off('c_sw')
      if ( flagstruct%nord > 0 ) then
                                                   call timing_on('COMM_TOTAL')
          call start_group_halo_update(i_pack(3), divgd, domain, position=CORNER)
                                                  call timing_off('COMM_TOTAL')
      endif

      if (gridstruct%nested) then
         call nested_grid_BC_apply_intT(delpc, &
              0, 0, npx, npy, npz, bd, split_timestep_BC+0.5, real(n_split*flagstruct%k_split), &
              neststruct%delp_BC, bctype=neststruct%nestbctype)
#ifndef SW_DYNAMICS
         call nested_grid_BC_apply_intT(ptc, &
              0, 0, npx, npy, npz, bd, split_timestep_BC+0.5, real(n_split*flagstruct%k_split), &
              neststruct%pt_BC, bctype=neststruct%nestbctype )
#endif
      endif
      if ( hydrostatic ) then
           call geopk(ptop, pe, peln, delpc, pkc, gz, phis, ptc, q_con, pkz, npz, akap, .true., &
                      gridstruct%nested, .false., npx, npy, flagstruct%a2b_ord, bd)
      else
#ifndef SW_DYNAMICS
           if ( it == 1 ) then

                                      call timing_on('COMM_TOTAL')
              call complete_group_halo_update(i_pack(5), domain)
                                     call timing_off('COMM_TOTAL')

!$OMP parallel do default(none) shared(isd,ied,jsd,jed,npz,zh,gz)
           do k=1,npz+1
              do j=jsd,jed
                 do i=isd,ied
! Save edge heights for update_dz_d
                    zh(i,j,k) = gz(i,j,k)
                 enddo
              enddo
           enddo

        else 
!$OMP parallel do default(none) shared(isd,ied,jsd,jed,npz,zh,gz)
           do k=1, npz+1
              do j=jsd,jed
                 do i=isd,ied
                    gz(i,j,k) = zh(i,j,k)
                 enddo
              enddo
           enddo
        endif
                                            call timing_on('UPDATE_DZ_C')
         call update_dz_c(is, ie, js, je, npz, ng, dt2, dp_ref, zs, gridstruct%area, ut, vt, gz, ws3, &
             npx, npy, gridstruct%sw_corner, gridstruct%se_corner, &
             gridstruct%ne_corner, gridstruct%nw_corner, bd, gridstruct%grid_type)
                                            call timing_off('UPDATE_DZ_C')

                                               call timing_on('Riem_Solver')
           call Riem_Solver_C( ms, dt2,   is,  ie,   js,   je,   npz,   ng,   &
                               akap, cappa,  cp,  ptop, phis, omga, ptc,  &
                               q_con,  delpc, gz,  pkc, ws3, flagstruct%p_fac, &
                                flagstruct%a_imp, flagstruct%scale_z )
                                               call timing_off('Riem_Solver')

           if (gridstruct%nested) then
                 call nested_grid_BC_apply_intT(delz, &
                      0, 0, npx, npy, npz, bd, split_timestep_BC+0.5, real(n_split*flagstruct%k_split), &
                neststruct%delz_BC, bctype=neststruct%nestbctype )


              !Compute gz/pkc
              !NOTE: nominally only need to compute quantities one out in the halo for p_grad_c
              !(instead of entire halo)
           call nest_halo_nh(ptop, grav, akap, cp, delpc, delz, ptc, phis, &
#ifdef USE_COND
                q_con, &
#ifdef MOIST_CAPPA
                cappa, &
#endif
#endif
                pkc, gz, pk3, &
                npx, npy, npz, gridstruct%nested, .false., .false., .false., bd)

           endif

#endif SW_DYNAMICS

      endif   ! end hydro check

      call p_grad_c(dt2, npz, delpc, pkc, gz, uc, vc, bd, gridstruct%rdxc, gridstruct%rdyc, hydrostatic)

                                                                   call timing_on('COMM_TOTAL')
      call start_group_halo_update(i_pack(9), uc, vc, domain, gridtype=CGRID_NE)
                                                     call timing_off('COMM_TOTAL')
#ifdef SW_DYNAMICS
#ifdef USE_OLD
      if (test_case==9) call case9_forcing2(phis)
#endif
      endif !test_case>1
#endif

                                                                   call timing_on('COMM_TOTAL')
    if (flagstruct%inline_q .and. nq>0) call complete_group_halo_update(i_pack(10), domain)
    if (flagstruct%nord > 0) call complete_group_halo_update(i_pack(3), domain)
                             call complete_group_halo_update(i_pack(9), domain)

                                                                   call timing_off('COMM_TOTAL')
      if (gridstruct%nested) then
         !On a nested grid we have to do SOMETHING with uc and vc in
         ! the boundary halo, particularly at the corners of the
         ! domain and of each processor element. We must either
         ! apply an interpolated BC, or extrapolate into the
         ! boundary halo
         ! NOTE: 
         !The update_domains calls for uc and vc need to go BEFORE the BCs to ensure cross-restart
         !bitwise-consistent solutions when doing the spatial extrapolation; should not make a
         !difference for interpolated BCs from the coarse grid.


            call nested_grid_BC_apply_intT(vc, &
                 0, 1, npx, npy, npz, bd, split_timestep_bc+0.5, real(n_split*flagstruct%k_split), & 
            neststruct%vc_BC, bctype=neststruct%nestbctype )
            call nested_grid_BC_apply_intT(uc, &
                 1, 0, npx, npy, npz, bd, split_timestep_bc+0.5, real(n_split*flagstruct%k_split), &
            neststruct%uc_BC, bctype=neststruct%nestbctype )

       !QUESTION: What to do with divgd in nested halo?
            call nested_grid_BC_apply_intT(divgd, &
                 1, 1, npx, npy, npz, bd, split_timestep_bc, real(n_split*flagstruct%k_split), &
            neststruct%divg_BC, bctype=neststruct%nestbctype )
!!$            if (is == 1 .and. js == 1) then
!!$               do j=jsd,5
!!$                  write(mpp_pe()+2000,*) j, divg(isd:5,j,1)
!!$            endif

      end if

    if ( gridstruct%nested .and. flagstruct%inline_q ) then
            do iq=1,nq
                  call nested_grid_BC_apply_intT(q(isd:ied,jsd:jed,:,iq), &
                       0, 0, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
               neststruct%q_BC(iq), bctype=neststruct%nestbctype )
            end do
      endif

                                                     call timing_on('d_sw')
!$OMP parallel do default(none) shared(npz,flagstruct,nord_v,pfull,damp_vt,hydrostatic,last_step, &
!$OMP                                  is,ie,js,je,isd,ied,jsd,jed,omga,delp,gridstruct,npx,npy,  &
!$OMP                                  ng,zh,vt,ptc,pt,u,v,w,uc,vc,ua,va,divgd,mfx,mfy,cx,cy,     &
!$OMP                                  crx,cry,xfx,yfx,q_con,zvir,sphum,nq,q,dt,bd,rdt,iep1,jep1, &
!$OMP                                  heat_source)                                               &
!$OMP                          private(nord_k, nord_w, nord_t, damp_w, damp_t, d2_divg,   &
!$OMP                          d_con_k,kgb, hord_m, hord_v, hord_t, hord_p, wk, heat_s, z_rat)
    do k=1,npz
       hord_m = flagstruct%hord_mt
       hord_t = flagstruct%hord_tm
       hord_v = flagstruct%hord_vt
       hord_p = flagstruct%hord_dp
       nord_k = flagstruct%nord
       if ( k==npz ) then
          kgb = flagstruct%ke_bg
       else
          kgb = 0.
       endif
       nord_v(k) = min(2, flagstruct%nord)
!      d2_divg = min(0.20, flagstruct%d2_bg*(1.-3.*tanh(0.1*log(pfull(k)/pfull(npz)))))
       d2_divg = min(0.20, flagstruct%d2_bg)

       if ( flagstruct%do_vort_damp ) then
            damp_vt(k) = flagstruct%vtdm4     ! for delp, delz, and vorticity
       else
            damp_vt(k) = 0.
       endif

       nord_w = nord_v(k)
       nord_t = nord_v(k)
       damp_w = damp_vt(k)
       damp_t = damp_vt(k)
       d_con_k = flagstruct%d_con

       if ( npz==1 .or. flagstruct%n_sponge<0 ) then
           d2_divg = flagstruct%d2_bg
       else
! Sponge layers with del-2 damping on divergence, vorticity, w, z, and air mass (delp).
! no special damping of potential temperature in sponge layers
              if ( k==1 ) then
! Divergence damping:
                   nord_k=0; d2_divg = max(0.01, flagstruct%d2_bg, flagstruct%d2_bg_k1)
! Vertical velocity:
                   nord_w=0; damp_w = d2_divg
                   if ( flagstruct%do_vort_damp ) then
! damping on delp and vorticity:
                        nord_v(k)=0; 
#ifndef HIWPP
                        damp_vt(k) = 0.5*d2_divg
#endif
                   endif
                   d_con_k = 0.
              elseif ( k==2 .and. flagstruct%d2_bg_k2>0.01 ) then
                   nord_k=0; d2_divg = max(flagstruct%d2_bg, flagstruct%d2_bg_k2)
                   nord_w=0; damp_w = d2_divg
                   if ( flagstruct%do_vort_damp ) then
                        nord_v(k)=0; 
#ifndef HIWPP
                        damp_vt(k) = 0.5*d2_divg
#endif
                   endif
                   d_con_k = 0.
              elseif ( k==3 .and. flagstruct%d2_bg_k2>0.05 ) then
                   nord_k=0;  d2_divg = max(flagstruct%d2_bg, 0.2*flagstruct%d2_bg_k2)
                   nord_w=0;  damp_w = d2_divg
                   d_con_k = 0.
              endif
       endif

       if( hydrostatic .and. (.not.flagstruct%use_old_omega) .and. last_step ) then
! Average horizontal "convergence" to cell center
            do j=js,je
               do i=is,ie
                  omga(i,j,k) = delp(i,j,k)
               enddo
            enddo
       endif

!--- external mode divergence damping ---
       if ( flagstruct%d_ext > 0. )  &
            call a2b_ord2(delp(isd,jsd,k), wk, gridstruct, npx, npy, is,    &
                          ie, js, je, ng, .false.)

       if ( .not.hydrostatic .and. flagstruct%do_f3d ) then
! Correction factor for 3D Coriolis force
         do j=jsd,jed
            do i=isd,ied
               z_rat(i,j) = 1. + (zh(i,j,k)+zh(i,j,k+1))/radius
            enddo
         enddo
       endif
       call d_sw(vt(isd,jsd,k), delp(isd,jsd,k), ptc(isd,jsd,k),  pt(isd,jsd,k),      &
                  u(isd,jsd,k),    v(isd,jsd,k),   w(isd:,jsd:,k),  uc(isd,jsd,k),      &
                  vc(isd,jsd,k),   ua(isd,jsd,k),  va(isd,jsd,k), divgd(isd,jsd,k),   &
                  mfx(is, js, k),  mfy(is, js, k),  cx(is, jsd,k),  cy(isd,js, k),    &
                  crx(is, jsd,k),  cry(isd,js, k), xfx(is, jsd,k), yfx(isd,js, k),    &
#ifdef USE_COND
                  q_con(isd:,jsd:,k),  z_rat(isd,jsd),  &
#else
                  q_con(isd:,jsd:,1),  z_rat(isd,jsd),  &
#endif
                  kgb, heat_s, zvir, sphum, nq,  q,  k,  npz, flagstruct%inline_q,  dt,  &
                  flagstruct%hord_tr, hord_m, hord_v, hord_t, hord_p,    &
                  nord_k, nord_v(k), nord_w, nord_t, flagstruct%dddmp, d2_divg, flagstruct%d4_bg,  &
                  damp_vt(k), damp_w, damp_t, d_con_k, hydrostatic, gridstruct, flagstruct, bd)

       if( hydrostatic .and. (.not.flagstruct%use_old_omega) .and. last_step ) then
! Average horizontal "convergence" to cell center
            do j=js,je
               do i=is,ie
                  omga(i,j,k) = omga(i,j,k)*(xfx(i,j,k)-xfx(i+1,j,k)+yfx(i,j,k)-yfx(i,j+1,k))*gridstruct%rarea(i,j)*rdt
               enddo
            enddo
       endif

       if ( flagstruct%d_ext > 0. ) then
            do j=js,jep1
               do i=is,iep1
                  ptc(i,j,k) = wk(i,j)    ! delp at cell corners
               enddo
            enddo
       endif
       if ( flagstruct%d_con > 1.0E-5 ) then
! Average horizontal "convergence" to cell center
            do j=js,je
               do i=is,ie
                  heat_source(i,j,k) = heat_source(i,j,k) + heat_s(i,j)
               enddo
            enddo
       endif
    enddo           ! end openMP k-loop

                                                     call timing_off('d_sw')

    if( flagstruct%fill_dp ) call mix_dp(hydrostatic, w, delp, pt, npz, ak, bk, .false., flagstruct%fv_debug, bd)

                                                             call timing_on('COMM_TOTAL')
    call start_group_halo_update(i_pack(1), delp, domain, complete=.false.)
    call start_group_halo_update(i_pack(1), pt,   domain, complete=.true.)
#ifdef USE_COND
    call start_group_halo_update(i_pack(11), q_con, domain)
#endif
                                                             call timing_off('COMM_TOTAL')

    if ( flagstruct%d_ext > 0. ) then
          d2_divg = flagstruct%d_ext * gridstruct%da_min_c
!$OMP parallel do default(none) shared(is,iep1,js,jep1,npz,wk,ptc,divg2,vt,d2_divg)
          do j=js,jep1
              do i=is,iep1
                    wk(i,j) = ptc(i,j,1)
                 divg2(i,j) = wk(i,j)*vt(i,j,1)
              enddo
              do k=2,npz
                 do i=is,iep1
                       wk(i,j) =    wk(i,j) + ptc(i,j,k)
                    divg2(i,j) = divg2(i,j) + ptc(i,j,k)*vt(i,j,k)
                 enddo
              enddo
              do i=is,iep1
                 divg2(i,j) = d2_divg*divg2(i,j)/wk(i,j)
              enddo
          enddo
    else
        divg2(:,:) = 0.
    endif

                                       call timing_on('COMM_TOTAL')
     call complete_group_halo_update(i_pack(1), domain)
#ifdef USE_COND
     call complete_group_halo_update(i_pack(11), domain)
#endif
                                       call timing_off('COMM_TOTAL')
    if ( flagstruct%fv_debug ) then
         if ( .not. flagstruct%hydrostatic )    &
         call prt_mxm('delz',  delz, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
    endif

    !Want to move this block into the hydro/nonhydro branch above and merge the two if structures
    if (gridstruct%nested) then
       call nested_grid_BC_apply_intT(delp, &
            0, 0, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
            neststruct%delp_BC, bctype=neststruct%nestbctype )

#ifndef SW_DYNAMICS

       call nested_grid_BC_apply_intT(pt, &
            0, 0, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
            neststruct%pt_BC, bctype=neststruct%nestbctype  )

#ifdef USE_COND
       call nested_grid_BC_apply_intT(q_con, &
            0, 0, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
            neststruct%q_con_BC, bctype=neststruct%nestbctype  )       
#endif

#endif

    end if
     if ( hydrostatic ) then
          call geopk(ptop, pe, peln, delp, pkc, gz, phis, pt, q_con, pkz, npz, akap, .false., &
                     gridstruct%nested, .true., npx, npy, flagstruct%a2b_ord, bd)
       else
#ifndef SW_DYNAMICS
                                            call timing_on('UPDATE_DZ')
        call update_dz_d(nord_v, damp_vt, flagstruct%hord_tm, is, ie, js, je, npz, ng, npx, npy, gridstruct%area,  &
                         gridstruct%rarea, dp_ref, zs, zh, crx, cry, xfx, yfx, delz, ws, rdt, gridstruct, bd)
                                            call timing_off('UPDATE_DZ')
    if ( flagstruct%fv_debug ) then
         if ( .not. flagstruct%hydrostatic )    &
         call prt_mxm('delz updated',  delz, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
    endif

        if (idiag%id_ws>0 .and. last_step) then
!           call prt_maxmin('WS', ws, is, ie, js, je, 0, 1, 1., master)
            used=send_data(idiag%id_ws, ws, fv_time)
        endif



                             

                                                         call timing_on('Riem_Solver')
        call Riem_Solver3(flagstruct%m_split, dt,  is,  ie,   js,   je, npz, ng,     &
                         isd, ied, jsd, jed, &
                         akap, cappa, cp,  ptop, zs, q_con, w, delz, pt, delp, zh,   &
                         pe, pkc, pk3, pk, peln, ws, &
                         flagstruct%scale_z, flagstruct%p_fac, flagstruct%a_imp, &
                         flagstruct%use_logp, remap_step, beta<-0.1)
                                                         call timing_off('Riem_Solver')
                                       call timing_on('COMM_TOTAL')
        if ( gridstruct%square_domain ) then
          call start_group_halo_update(i_pack(4), zh ,  domain)
          call start_group_halo_update(i_pack(5), pkc,  domain, whalo=2, ehalo=2, shalo=2, nhalo=2)
        else
          call start_group_halo_update(i_pack(4), zh ,  domain, complete=.false.)
          call start_group_halo_update(i_pack(4), pkc,  domain, complete=.true.)
        endif
                                       call timing_off('COMM_TOTAL')
        if ( remap_step )  &
        call pe_halo(is, ie, js, je, isd, ied, jsd, jed, npz, ptop, pe, delp)

        if ( flagstruct%use_logp ) then
             call pln_halo(is, ie, js, je, isd, ied, jsd, jed, npz, ptop, pk3, delp)
        else
             call pk3_halo(is, ie, js, je, isd, ied, jsd, jed, npz, ptop, akap, pk3, delp)
        endif
       if (gridstruct%nested) then
          call nested_grid_BC_apply_intT(delz, &
               0, 0, npx, npy, npz, bd, split_timestep_BC+1., real(n_split*flagstruct%k_split), &
               neststruct%delz_BC, bctype=neststruct%nestbctype  )
          
          !Compute gz/pkc/pk3; note that now pkc should be nonhydro pert'n pressure
          call nest_halo_nh(ptop, grav, akap, cp, delp, delz, pt, phis, &
#ifdef USE_COND
               q_con, &
#ifdef MOIST_CAPPA
               cappa, &
#endif
#endif
               pkc, gz, pk3, npx, npy, npz, gridstruct%nested, .true., .true., .true., bd)

       endif
        call timing_on('COMM_TOTAL')
        call complete_group_halo_update(i_pack(4), domain)
        call timing_off('COMM_TOTAL')
!$OMP parallel do default(none) shared(is,ie,js,je,npz,gz,zh,grav)
        do k=1,npz+1
           do j=js-2,je+2
              do i=is-2,ie+2
                 gz(i,j,k) = zh(i,j,k)*grav
              enddo
           enddo
        enddo
        if ( gridstruct%square_domain ) then
           call timing_on('COMM_TOTAL')
        call complete_group_halo_update(i_pack(5), domain)
                                       call timing_off('COMM_TOTAL')
	    endif
#endif SW_DYNAMICS
     endif    ! end hydro check

#ifdef SW_DYNAMICS
      if (test_case > 1) then
#else
      if ( remap_step .and. hydrostatic ) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,pk,pkc)
           do k=1,npz+1
              do j=js,je
                 do i=is,ie
                    pk(i,j,k) = pkc(i,j,k)
                 enddo
              enddo
           enddo
      endif
#endif

!----------------------------
! Compute pressure gradient:
!----------------------------
                                       call timing_on('PG_D')
    if ( hydrostatic ) then
       if ( beta > 0. ) then
          call grad1_p_update(divg2, u, v, pkc, gz, dt, ng, gridstruct, bd, npx, npy, npz, ptop, beta_d, flagstruct%a2b_ord)
       else
          call one_grad_p(u, v, pkc, gz, divg2, delp, dt, ng, gridstruct, bd, npx, npy, npz, ptop, hydrostatic, flagstruct%a2b_ord, flagstruct%d_ext)
       endif

    else


       if ( beta > 0. ) then
          call split_p_grad( u, v, pkc, gz, delp, pk3, beta_d, dt, ng, gridstruct, bd, npx, npy, npz, flagstruct%use_logp)
       elseif ( beta < -0.1 ) then
         call one_grad_p(u, v, pkc, gz, divg2, delp, dt, ng, gridstruct, bd, npx, npy, npz, ptop, hydrostatic, flagstruct%a2b_ord, flagstruct%d_ext)
       else
          call nh_p_grad(u, v, pkc, gz, delp, pk3, dt, ng, gridstruct, bd, npx, npy, npz, flagstruct%use_logp)
       endif

#ifdef ROT3
       if ( flagstruct%do_f3d ) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,ua,gridstruct,w,va,isd,ied,jsd,jed)
           do k=1,npz
              do j=js,je
                 do i=is,ie
                    ua(i,j,k) = -gridstruct%w00(i,j)*w(i,j,k)
                 enddo
              enddo
              do j=jsd,jed
                 do i=isd,ied
                    va(i,j,k) = 0.
                 enddo
              enddo
           enddo
           call mpp_update_domains(ua, domain, complete=.true.)
           call update_dwinds_phys(is, ie, js, je, isd, ied, jsd, jed, dt, ua, va, u, v, gridstruct, npx, npy, npz, domain)
       endif
#endif
   endif
                                       call timing_off('PG_D')

!-------------------------------------------------------------------------------------------------------
    if ( flagstruct%breed_vortex_inline ) then
        if ( .not. hydrostatic ) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,pkz,cappa,rdg,delp,delz,pt,k1k)
           do k=1,npz
              do j=js,je
                 do i=is,ie
! Note: pt at this stage is Theta_m
#ifdef MOIST_CAPPA
                    pkz(i,j,k) = exp(cappa(i,j,k)/(1.-cappa(i,j,k))*log(rdg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)) )
#else
                    pkz(i,j,k) = exp( k1k*log(rdg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)) )
#endif
                 enddo
              enddo
           enddo
        endif
#if defined (ADA_NUDGE)
         call breed_slp_inline_ada( it, dt, npz, ak, bk, phis, pe, pk, peln, pkz,     &
                                delp, u, v, pt, q, flagstruct%nwat, zvir, gridstruct, ks, domain, bd )
#else
         call breed_slp_inline( it, dt, npz, ak, bk, phis, pe, pk, peln, pkz, delp, u, v, pt, q,    &
                                flagstruct%nwat, zvir, gridstruct, ks, domain, bd, hydrostatic )
#endif
    endif
!-------------------------------------------------------------------------------------------------------

                                                     call timing_on('COMM_TOTAL')
    if( it==n_split .and. gridstruct%grid_type<4 .and. .not. gridstruct%nested) then
! Prevent accumulation of rounding errors at overlapped domain edges:
       call mpp_get_boundary(u, v, domain, ebuffery=ebuffer,  &
                             nbufferx=nbuffer, gridtype=DGRID_NE )
!$OMP parallel do default(none) shared(is,ie,js,je,npz,u,nbuffer,v,ebuffer)
          do k=1,npz
             do i=is,ie
                u(i,je+1,k) = nbuffer(i-is+1,k)
             enddo
             do j=js,je
                v(ie+1,j,k) = ebuffer(j-js+1,k)
             enddo
          enddo

    endif

#ifndef ROT3
    if ( it/=n_split)   &
         call start_group_halo_update(i_pack(8), u, v, domain, gridtype=DGRID_NE)
#endif
                                                     call timing_off('COMM_TOTAL')

#ifdef SW_DYNAMICS
    endif
#endif
      if ( gridstruct%nested ) then
         neststruct%nest_timestep = neststruct%nest_timestep + 1
      endif

#ifdef SW_DYNAMICS
#else
    if ( hydrostatic .and. last_step ) then
      if ( flagstruct%use_old_omega ) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,omga,pe,pem,rdt)
         do k=1,npz
            do j=js,je
               do i=is,ie
                  omga(i,j,k) = (pe(i,k+1,j) - pem(i,k+1,j)) * rdt
               enddo
            enddo
         enddo
!------------------------------
! Compute the "advective term"
!------------------------------
         call adv_pe(ua, va, pem, omga, gridstruct, bd, npx, npy,  npz, ng)
      else
!$OMP parallel do default(none) shared(is,ie,js,je,npz,omga) private(om2d)
         do j=js,je
            do k=1,npz
               do i=is,ie
                  om2d(i,k) = omga(i,j,k)
               enddo
            enddo
            do k=2,npz
               do i=is,ie
                  om2d(i,k) = om2d(i,k-1) + omga(i,j,k)
               enddo
            enddo
            do k=2,npz
               do i=is,ie
                  omga(i,j,k) = om2d(i,k)
               enddo
            enddo
         enddo
      endif
      if (idiag%id_ws>0 .and. hydrostatic) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,ws,delz,delp,omga)
          do j=js,je
             do i=is,ie
                ws(i,j) = delz(i,j,npz)/delp(i,j,npz) * omga(i,j,npz)
             enddo
          enddo
          used=send_data(idiag%id_ws, ws, fv_time)
      endif
    endif
#endif

    if (gridstruct%nested) then



#ifndef SW_DYNAMICS
         if (.not. hydrostatic) then
               call nested_grid_BC_apply_intT(w, &
               0, 0, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
               neststruct%w_BC, bctype=neststruct%nestbctype  )
       end if
#endif SW_DYNAMICS
            call nested_grid_BC_apply_intT(u, &
            0, 1, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
            neststruct%u_BC, bctype=neststruct%nestbctype  )
            call nested_grid_BC_apply_intT(v, &
            1, 0, npx, npy, npz, bd, split_timestep_BC+1, real(n_split*flagstruct%k_split), &
            neststruct%v_BC, bctype=neststruct%nestbctype )

      end if

!-----------------------------------------------------
  enddo   ! time split loop
!-----------------------------------------------------
    if ( nq > 0 .and. .not. flagstruct%inline_q ) then
       call timing_on('COMM_TOTAL')
       call timing_on('COMM_TRACER')
       call start_group_halo_update(i_pack(10), q, domain)
       call timing_off('COMM_TRACER')
       call timing_off('COMM_TOTAL')
     endif

  if ( flagstruct%fv_debug ) then
       if(is_master()) write(*,*) 'End of n_split loop'
  endif


  if ( n_con/=0 .and. flagstruct%d_con > 1.e-5 ) then
       nf_ke = min(3, flagstruct%nord+1)
       call del2_cubed(heat_source, cnst_0p20*gridstruct%da_min, gridstruct, domain, npx, npy, npz, nf_ke, bd)

! Note: pt here is cp*(Virtual_Temperature/pkz)
    if ( hydrostatic ) then
!
! del(Cp*T) = - del(KE)
!
!$OMP parallel do default(none) shared(is,ie,js,je,n_con,pt,heat_source,delp,pkz)
       do j=js,je
          do k=1,n_con
             do i=is,ie
                pt(i,j,k) = pt(i,j,k) + heat_source(i,j,k)/(cp_air*delp(i,j,k)*pkz(i,j,k))
             enddo
          enddo
       enddo
    else
!$OMP parallel do default(none) shared(is,ie,js,je,n_con,pkz,cappa,rdg,delp,delz,pt, &
!$OMP                                  heat_source,k1k,akap,cv_air) &
!$OMP                          private(mk1)
       do j=js,je
          do k=1,n_con    ! n_con is usually less than 3; not good as outer openMP loop
             do i=is,ie
#ifdef MOIST_CAPPA
                mk1 = cappa(i,j,k)/(1.-cappa(i,j,k))
                pkz(i,j,k) = exp( mk1*log(rdg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)) )
#else
                pkz(i,j,k) = exp( k1k*log(rdg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)) )
#endif
                pt(i,j,k) = pt(i,j,k) + heat_source(i,j,k)/(cv_air*delp(i,j,k)*pkz(i,j,k))
             enddo
          enddo
       enddo
    endif
    endif
    if (allocated(heat_source)) deallocate( heat_source ) !If ncon == 0 but d_con > 1.e-5, this would not be deallocated in earlier versions of the code


  if ( end_step ) then
    deallocate(    gz )
    deallocate(   ptc )
    deallocate(   crx )
    deallocate(   xfx )
    deallocate(   cry )
    deallocate(   yfx )
    deallocate( divgd )
    deallocate(   pkc )
    deallocate( delpc )

    if( allocated(ut))   deallocate( ut )
    if( allocated(vt))   deallocate( vt )
    if ( allocated (du) ) deallocate( du )
    if ( allocated (dv) ) deallocate( dv )
    if ( .not. hydrostatic ) then
         deallocate( zh )
         if( allocated(pk3) )   deallocate ( pk3 )
    endif

  endif
  if( allocated(pem) )   deallocate ( pem )

if ( flagstruct%fv_debug ) then
   if(is_master()) write(*,*) 'End of dyn_core'
endif

end subroutine dyn_core

subroutine pk3_halo(is, ie, js, je, isd, ied, jsd, jed, npz, ptop, akap, pk3, delp)
integer, intent(in):: is, ie, js, je, isd, ied, jsd, jed, npz
real, intent(in):: ptop, akap
real, intent(in   ), dimension(isd:ied,jsd:jed,npz):: delp
real, intent(inout), dimension(isd:ied,jsd:jed,npz+1):: pk3
! Local:
real:: pei(isd:ied)
real:: pej(jsd:jed)
integer:: i,j,k

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,npz,ptop,delp,pk3,akap) &
!$OMP                          private(pei)
  do j=js,je
     pei(is-2) = ptop
     pei(is-1) = ptop
     do k=1,npz
        pei(is-2) = pei(is-2) + delp(is-2,j,k)
        pei(is-1) = pei(is-1) + delp(is-1,j,k)
        pk3(is-2,j,k+1) = exp(akap*log(pei(is-2)))
        pk3(is-1,j,k+1) = exp(akap*log(pei(is-1)))
     enddo
     pei(ie+1) = ptop
     pei(ie+2) = ptop
     do k=1,npz
        pei(ie+1) = pei(ie+1) + delp(ie+1,j,k)
        pei(ie+2) = pei(ie+2) + delp(ie+2,j,k)
        pk3(ie+1,j,k+1) = exp(akap*log(pei(ie+1)))
        pk3(ie+2,j,k+1) = exp(akap*log(pei(ie+2)))
     enddo
  enddo

!$OMP parallel do default(none) shared(is,ie,js,je,npz,ptop,delp,pk3,akap) &
!$OMP                          private(pej)
  do i=is-2,ie+2
     pej(js-2) = ptop
     pej(js-1) = ptop
     do k=1,npz
        pej(js-2) = pej(js-2) + delp(i,js-2,k)
        pej(js-1) = pej(js-1) + delp(i,js-1,k)
        pk3(i,js-2,k+1) = exp(akap*log(pej(js-2)))
        pk3(i,js-1,k+1) = exp(akap*log(pej(js-1)))
     enddo
     pej(je+1) = ptop
     pej(je+2) = ptop
     do k=1,npz
        pej(je+1) = pej(je+1) + delp(i,je+1,k)
        pej(je+2) = pej(je+2) + delp(i,je+2,k)
        pk3(i,je+1,k+1) = exp(akap*log(pej(je+1)))
        pk3(i,je+2,k+1) = exp(akap*log(pej(je+2)))
     enddo
  enddo

end subroutine pk3_halo

subroutine pln_halo(is, ie, js, je, isd, ied, jsd, jed, npz, ptop, pk3, delp)
integer, intent(in):: is, ie, js, je, isd, ied, jsd, jed, npz
real, intent(in):: ptop
real, intent(in   ), dimension(isd:ied,jsd:jed,npz):: delp
real, intent(inout), dimension(isd:ied,jsd:jed,npz+1):: pk3
! Local:
real:: pet
integer:: i,j,k

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,npz,ptop,delp,pk3) &
!$OMP                          private(pet)
  do j=js,je
     do i=is-2,is-1
        pet = ptop
        do k=1,npz
           pet = pet + delp(i,j,k)
           pk3(i,j,k+1) = log(pet)
        enddo
     enddo
     do i=ie+1,ie+2
        pet = ptop
        do k=1,npz
           pet = pet + delp(i,j,k)
           pk3(i,j,k+1) = log(pet)
        enddo
     enddo
  enddo

!$OMP parallel do default(none) shared(is,ie,js,je,npz,ptop,delp,pk3) &
!$OMP                          private(pet)
  do i=is-2,ie+2
     do j=js-2,js-1
        pet = ptop
        do k=1,npz
           pet = pet + delp(i,j,k)
           pk3(i,j,k+1) = log(pet)
        enddo
     enddo
     do j=je+1,je+2
        pet = ptop
        do k=1,npz
           pet = pet + delp(i,j,k)
           pk3(i,j,k+1) = log(pet)
        enddo
     enddo
  enddo

end subroutine pln_halo

subroutine pe_halo(is, ie, js, je, isd, ied, jsd, jed, npz, ptop, pe, delp)
integer, intent(in):: is, ie, js, je, isd, ied, jsd, jed, npz
real, intent(in):: ptop
real, intent(in   ), dimension(isd:ied,jsd:jed,npz):: delp
real, intent(inout), dimension(is-1:ie+1,npz+1,js-1:je+1):: pe
! Local:
integer:: i,j,k

!$OMP parallel do default(none) shared(is,ie,js,je,npz,pe,delp,ptop)
  do j=js,je
     pe(is-1,1,j) = ptop
     pe(ie+1,1,j) = ptop
     do k=1,npz
        pe(is-1,k+1,j) = pe(is-1,k,j) + delp(is-1,j,k)
        pe(ie+1,k+1,j) = pe(ie+1,k,j) + delp(ie+1,j,k)
     enddo
  enddo

!$OMP parallel do default(none) shared(is,ie,js,je,npz,pe,delp,ptop)
  do i=is-1,ie+1
     pe(i,1,js-1) = ptop
     pe(i,1,je+1) = ptop
     do k=1,npz
        pe(i,k+1,js-1) = pe(i,k,js-1) + delp(i,js-1,k)
        pe(i,k+1,je+1) = pe(i,k,je+1) + delp(i,je+1,k)
     enddo
  enddo

end subroutine pe_halo


subroutine adv_pe(ua, va, pem, om, gridstruct, bd, npx, npy, npz, ng)

integer, intent(in) :: npx, npy, npz, ng
type(fv_grid_bounds_type), intent(IN) :: bd
! Contra-variant wind components:
real, intent(in), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,npz):: ua, va
! Pressure at edges:
real, intent(in) :: pem(bd%is-1:bd%ie+1,1:npz+1,bd%js-1:bd%je+1)
real, intent(inout) :: om(bd%isd:bd%ied,bd%jsd:bd%jed,npz)
type(fv_grid_type), intent(INOUT), target :: gridstruct

! Local:
real, dimension(bd%is:bd%ie,bd%js:bd%je):: up, vp
real v3(3,bd%is:bd%ie,bd%js:bd%je)

real pin(bd%isd:bd%ied,bd%jsd:bd%jed)
real  pb(bd%isd:bd%ied,bd%jsd:bd%jed)

real grad(3,bd%is:bd%ie,bd%js:bd%je)
real pdx(3,bd%is:bd%ie,bd%js:bd%je+1)
real pdy(3,bd%is:bd%ie+1,bd%js:bd%je)

integer :: i,j,k, n
integer :: is,  ie,  js,  je

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je

!$OMP parallel do default(none) shared(is,ie,js,je,npz,ua,va,gridstruct,pem,npx,npy,ng,om) &
!$OMP                          private(n, pdx, pdy, pin, pb, up, vp, grad, v3)
do k=1,npz
   if ( k==npz ) then
      do j=js,je
         do i=is,ie
            up(i,j) = ua(i,j,npz)
            vp(i,j) = va(i,j,npz)
         enddo
      enddo
   else
      do j=js,je
         do i=is,ie
            up(i,j) = 0.5*(ua(i,j,k)+ua(i,j,k+1))
            vp(i,j) = 0.5*(va(i,j,k)+va(i,j,k+1))
         enddo
      enddo
   endif

   ! Compute Vect wind:
   do j=js,je
      do i=is,ie
         do n=1,3
            v3(n,i,j) = up(i,j)*gridstruct%ec1(n,i,j) + vp(i,j)*gridstruct%ec2(n,i,j) 
         enddo
      enddo
   enddo

   do j=js-1,je+1
      do i=is-1,ie+1
         pin(i,j) = pem(i,k+1,j)
      enddo
   enddo

   ! Compute pe at 4 cell corners:
   call a2b_ord2(pin, pb, gridstruct, npx, npy, is, ie, js, je, ng)


   do j=js,je+1
      do i=is,ie
         do n=1,3
            pdx(n,i,j) = (pb(i,j)+pb(i+1,j))*gridstruct%dx(i,j)*gridstruct%en1(n,i,j)
         enddo
      enddo
   enddo
   do j=js,je
      do i=is,ie+1
         do n=1,3
            pdy(n,i,j) = (pb(i,j)+pb(i,j+1))*gridstruct%dy(i,j)*gridstruct%en2(n,i,j)
         enddo
      enddo
   enddo

   ! Compute grad (pe) by Green's theorem
   do j=js,je
      do i=is,ie
         do n=1,3
            grad(n,i,j) = pdx(n,i,j+1) - pdx(n,i,j) - pdy(n,i,j) + pdy(n,i+1,j)
         enddo
      enddo
   enddo

   ! Compute inner product: V3 * grad (pe)
   do j=js,je
      do i=is,ie
         om(i,j,k) = om(i,j,k) + 0.5*gridstruct%rarea(i,j)*(v3(1,i,j)*grad(1,i,j) +   &
              v3(2,i,j)*grad(2,i,j) + v3(3,i,j)*grad(3,i,j))
      enddo
   enddo
enddo

end subroutine adv_pe




subroutine p_grad_c(dt2, npz, delpc, pkc, gz, uc, vc, bd, rdxc, rdyc, hydrostatic)

integer, intent(in):: npz
real,    intent(in):: dt2
type(fv_grid_bounds_type), intent(IN) :: bd
real, intent(in), dimension(bd%isd:, bd%jsd: ,:  ):: delpc
! pkc is pe**cappa     if hydrostatic
! pkc is full pressure if non-hydrostatic
real, intent(in), dimension(bd%isd:bd%ied, bd%jsd:bd%jed ,npz+1):: pkc, gz
real, intent(inout):: uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
real, intent(inout):: vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
real, intent(IN) :: rdxc(bd%isd:bd%ied+1,bd%jsd:bd%jed+1)
real, intent(IN) :: rdyc(bd%isd:bd%ied  ,bd%jsd:bd%jed)
logical, intent(in):: hydrostatic
! Local:
real:: wk(bd%is-1:bd%ie+1,bd%js-1:bd%je+1)
integer:: i,j,k

integer :: is,  ie,  js,  je

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je

!$OMP parallel do default(none) shared(is,ie,js,je,npz,hydrostatic,pkc,delpc,uc,dt2,rdxc,gz,vc,rdyc) &
!$OMP                          private(wk)
do k=1,npz

   if ( hydrostatic ) then
      do j=js-1,je+1
         do i=is-1,ie+1
            wk(i,j) = pkc(i,j,k+1) - pkc(i,j,k)
         enddo
      enddo
   else
      do j=js-1,je+1
         do i=is-1,ie+1
            wk(i,j) = delpc(i,j,k)
         enddo
      enddo
   endif

   do j=js,je
      do i=is,ie+1
         uc(i,j,k) = uc(i,j,k) + dt2*rdxc(i,j) / (wk(i-1,j)+wk(i,j)) *   &
              ( (gz(i-1,j,k+1)-gz(i,j,k  ))*(pkc(i,j,k+1)-pkc(i-1,j,k))  &
              + (gz(i-1,j,k) - gz(i,j,k+1))*(pkc(i-1,j,k+1)-pkc(i,j,k)) )
      enddo
   enddo
   do j=js,je+1
      do i=is,ie
         vc(i,j,k) = vc(i,j,k) + dt2*rdyc(i,j) / (wk(i,j-1)+wk(i,j)) *   &
              ( (gz(i,j-1,k+1)-gz(i,j,k  ))*(pkc(i,j,k+1)-pkc(i,j-1,k))  &
              + (gz(i,j-1,k) - gz(i,j,k+1))*(pkc(i,j-1,k+1)-pkc(i,j,k)) )
      enddo
   enddo
enddo

end subroutine p_grad_c


subroutine nh_p_grad(u, v, pp, gz, delp, pk, dt, ng, gridstruct, bd, npx, npy, npz, use_logp)
integer, intent(IN) :: ng, npx, npy, npz
real,    intent(IN) :: dt
logical, intent(in) :: use_logp
type(fv_grid_bounds_type), intent(IN) :: bd
real, intent(inout) ::  delp(bd%isd:bd%ied, bd%jsd:bd%jed, npz)
real, intent(inout) ::    pp(bd%isd:bd%ied, bd%jsd:bd%jed, npz+1)  ! perturbation pressure
real, intent(inout) ::    pk(bd%isd:bd%ied, bd%jsd:bd%jed, npz+1)  ! p**kappa
real, intent(inout) ::    gz(bd%isd:bd%ied, bd%jsd:bd%jed, npz+1)  ! g * h
real, intent(inout) ::     u(bd%isd:bd%ied,  bd%jsd:bd%jed+1,npz) 
real, intent(inout) ::     v(bd%isd:bd%ied+1,bd%jsd:bd%jed,  npz)
    type(fv_grid_type), intent(INOUT), target :: gridstruct
! Local:
real wk1(bd%isd:bd%ied, bd%jsd:bd%jed)
real  wk(bd%is: bd%ie+1,bd%js: bd%je+1)
real du1, dv1, top_value
integer i,j,k
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
      
if ( use_logp ) then
   top_value = peln1
else
   top_value = ptk
endif

!Remember that not all compilers set pp to zero by default
!$OMP parallel do default(none) shared(is,ie,js,je,pp,pk,top_value)
do j=js,je+1
   do i=is,ie+1
      pp(i,j,1) = 0.
      pk(i,j,1) = top_value
   enddo
enddo

!$OMP parallel do default(none) shared(isd,jsd,npz,pp,gridstruct,npx,npy,is,ie,js,je,ng,pk,gz) &
!$OMP                          private(wk1)
do k=1,npz+1
   if ( k/=1 ) then
      call a2b_ord4(pp(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
      call a2b_ord4(pk(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   endif
   call a2b_ord4( gz(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
enddo

!$OMP parallel do default(none) shared(is,ie,js,je,npz,delp,gridstruct,npx,npy,ng,isd,jsd, &
!$OMP                                  pk,dt,gz,u,pp,v) &
!$OMP                          private(wk1, wk, du1, dv1)
do k=1,npz
   call a2b_ord4(delp(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng)
   do j=js,je+1
      do i=is,ie+1
         wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
      enddo
   enddo
   do j=js,je+1
      do i=is,ie
         ! hydrostatic contributions from past time-step already added in the "beta" part
         ! Current gradient from "hydrostatic" components:
         du1 = dt / (wk(i,j)+wk(i+1,j)) *   &
               ( (gz(i,j,k+1)-gz(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) +  &
                 (gz(i,j,k)-gz(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k)) )
#ifdef GAS_HYDRO_P
         dul = (1.-0.5*(q_con(i,j-1,k)+q_con(i,j,k)))*du
#endif
         ! Non-hydrostatic contribution
         u(i,j,k) = (u(i,j,k) + du1 + dt/(wk1(i,j)+wk1(i+1,j)) *  &
                    ((gz(i,j,k+1)-gz(i+1,j,k))*(pp(i+1,j,k+1)-pp(i,j,k))    &
              + (gz(i,j,k)-gz(i+1,j,k+1))*(pp(i,j,k+1)-pp(i+1,j,k))))*gridstruct%rdx(i,j)
      enddo
   enddo
   do j=js,je
      do i=is,ie+1
         ! Current gradient from "hydrostatic" components:
         dv1 = dt / (wk(i,j)+wk(i,j+1)) *   &
              ((gz(i,j,k+1)-gz(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) +  &
              (gz(i,j,k)-gz(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k)))
#ifdef GAS_HYDRO_P
         dvl = (1.-0.5*(q_con(i-1,j,k)+q_con(i,j,k)))*dv
#endif
         ! Non-hydrostatic contribution
         v(i,j,k) = (v(i,j,k) + dv1 + dt/(wk1(i,j)+wk1(i,j+1)) *  &
                    ((gz(i,j,k+1)-gz(i,j+1,k))*(pp(i,j+1,k+1)-pp(i,j,k))   &
              + (gz(i,j,k)-gz(i,j+1,k+1))*(pp(i,j,k+1)-pp(i,j+1,k))))*gridstruct%rdy(i,j)
      enddo
   enddo

enddo    ! end k-loop
end subroutine nh_p_grad


subroutine split_p_grad( u, v, pp, gz, delp, pk, beta, dt, ng, gridstruct, bd, npx, npy, npz, use_logp)
integer, intent(IN) :: ng, npx, npy, npz
real,    intent(IN) :: beta, dt
logical, intent(in):: use_logp
type(fv_grid_bounds_type), intent(IN) :: bd
real, intent(inout) ::  delp(bd%isd:bd%ied, bd%jsd:bd%jed, npz)
real, intent(inout) ::    pp(bd%isd:bd%ied, bd%jsd:bd%jed, npz+1)  ! perturbation pressure
real, intent(inout) ::    pk(bd%isd:bd%ied, bd%jsd:bd%jed, npz+1)  ! p**kappa
real, intent(inout) ::    gz(bd%isd:bd%ied, bd%jsd:bd%jed, npz+1)  ! g * h
! real, intent(inout) ::    du(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) 
! real, intent(inout) ::    dv(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
real, intent(inout) ::     u(bd%isd:bd%ied,  bd%jsd:bd%jed+1,npz) 
real, intent(inout) ::     v(bd%isd:bd%ied+1,bd%jsd:bd%jed,  npz)
type(fv_grid_type), intent(INOUT), target :: gridstruct
! Local:
real wk1(bd%isd:bd%ied, bd%jsd:bd%jed)
real  wk(bd%is: bd%ie+1,bd%js: bd%je+1)
real  alpha, top_value
integer i,j,k
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
      
if ( use_logp ) then
   top_value = peln1
else
   top_value = ptk
endif

alpha = 1. - beta

!$OMP parallel do default(none) shared(is,ie,js,je,pp,pk,top_value)
do j=js,je+1
   do i=is,ie+1
      pp(i,j,1) = 0.
      pk(i,j,1) = top_value
   enddo
enddo

!$OMP parallel do default(none) shared(isd,jsd,npz,pp,gridstruct,npx,npy,is,ie,js,je,ng,pk,gz) &
!$OMP                          private(wk1)
do k=1,npz+1
   if ( k/=1 ) then
      call a2b_ord4(pp(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
      call a2b_ord4(pk(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   endif
   call a2b_ord4( gz(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
enddo

!$OMP parallel do default(none) shared(is,ie,js,je,isd,jsd,npz,delp,gridstruct,npx,npy,ng, &
!$OMP                                  pk,u,beta,du,dt,gz,alpha,pp,v,dv) &
!$OMP                          private(wk1, wk)
do k=1,npz
   call a2b_ord4(delp(isd,jsd,k), wk1, gridstruct, npx, npy, is, ie, js, je, ng)

   do j=js,je+1
      do i=is,ie+1
         wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
      enddo
   enddo

   do j=js,je+1
      do i=is,ie
         u(i,j,k) = u(i,j,k) + beta*du(i,j,k)
         ! hydrostatic contributions from past time-step already added in the "beta" part
         ! Current gradient from "hydrostatic" components:
         !---------------------------------------------------------------------------------
         du(i,j,k) =  dt / (wk(i,j)+wk(i+1,j)) *   &
              ((gz(i,j,k+1)-gz(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) +  &
              (gz(i,j,k)-gz(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k)))
#ifdef GAS_HYDRO_P
         du = (1.-0.5*(q_con(i,j-1,k)+q_con(i,j,k)))*du
#endif
         !---------------------------------------------------------------------------------
         ! Non-hydrostatic contribution
         u(i,j,k) = (u(i,j,k) + alpha*du(i,j,k) + dt/(wk1(i,j)+wk1(i+1,j)) *  &
                    ((gz(i,j,k+1)-gz(i+1,j,k))*(pp(i+1,j,k+1)-pp(i,j,k))    &
              + (gz(i,j,k)-gz(i+1,j,k+1))*(pp(i,j,k+1)-pp(i+1,j,k))))*gridstruct%rdx(i,j)
      enddo
   enddo
   do j=js,je
      do i=is,ie+1
         v(i,j,k) = v(i,j,k) + beta*dv(i,j,k)
         ! Current gradient from "hydrostatic" components:
         !---------------------------------------------------------------------------------
         dv(i,j,k) = dt / (wk(i,j)+wk(i,j+1)) *   &
              ((gz(i,j,k+1)-gz(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) +  &
              (gz(i,j,k)-gz(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k)))
#ifdef GAS_HYDRO_P
         dv = (1.-0.5*(q_con(i-1,j,k)+q_con(i,j,k)))*dv
#endif
         !---------------------------------------------------------------------------------
         ! Non-hydrostatic contribution
         v(i,j,k) = (v(i,j,k) + alpha*dv(i,j,k) + dt/(wk1(i,j)+wk1(i,j+1)) *  &
                    ((gz(i,j,k+1)-gz(i,j+1,k))*(pp(i,j+1,k+1)-pp(i,j,k))   &
              + (gz(i,j,k)-gz(i,j+1,k+1))*(pp(i,j,k+1)-pp(i,j+1,k))))*gridstruct%rdy(i,j)
      enddo
   enddo

enddo    ! end k-loop


end subroutine split_p_grad



subroutine one_grad_p(u, v, pk, gz, divg2, delp, dt, ng, gridstruct, bd, npx, npy, npz,  &
   ptop, hydrostatic, a2b_ord, d_ext)  

integer, intent(IN) :: ng, npx, npy, npz, a2b_ord
real,    intent(IN) :: dt, ptop, d_ext
logical, intent(in) :: hydrostatic
type(fv_grid_bounds_type), intent(IN) :: bd
real,    intent(in) :: divg2(bd%is:bd%ie+1,bd%js:bd%je+1)
real, intent(inout) ::    pk(bd%isd:bd%ied,  bd%jsd:bd%jed  ,npz+1)
real, intent(inout) ::    gz(bd%isd:bd%ied,  bd%jsd:bd%jed  ,npz+1)
real, intent(inout) ::  delp(bd%isd:bd%ied,  bd%jsd:bd%jed  ,npz)
real, intent(inout) ::     u(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) 
real, intent(inout) ::     v(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
type(fv_grid_type), intent(INOUT), target :: gridstruct
! Local:
real, dimension(bd%isd:bd%ied,bd%jsd:bd%jed):: wk
real:: wk1(bd%is:bd%ie+1,bd%js:bd%je+1)
real:: wk2(bd%is:bd%ie,bd%js:bd%je+1)
real top_value
integer i,j,k

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

if ( hydrostatic ) then
   ! pk is pe**kappa if hydrostatic
   top_value = ptk
else
   ! pk is full pressure if non-hydrostatic
   top_value = ptop
endif

!$OMP parallel do default(none) shared(is,ie,js,je,pk,top_value)
do j=js,je+1
   do i=is,ie+1
      pk(i,j,1) = top_value
   enddo
enddo

!$OMP parallel do default(none) shared(npz,isd,jsd,pk,gridstruct,npx,npy,is,ie,js,je,ng,a2b_ord) &
!$OMP                          private(wk)
do k=2,npz+1
   if ( a2b_ord==4 ) then
      call a2b_ord4(pk(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   else
      call a2b_ord2(pk(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   endif
enddo

!$OMP parallel do default(none) shared(npz,isd,jsd,gz,gridstruct,npx,npy,is,ie,js,je,ng,a2b_ord) &
!$OMP                          private(wk)
do k=1,npz+1
   if ( a2b_ord==4 ) then
      call a2b_ord4( gz(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   else
      call a2b_ord2( gz(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   endif
enddo

if ( d_ext > 0. ) then

   !$OMP parallel do default(none) shared(is,ie,js,je,wk2,divg2)
   do j=js,je+1
      do i=is,ie
         wk2(i,j) = divg2(i,j)-divg2(i+1,j)
      enddo
   enddo

   !$OMP parallel do default(none) shared(is,ie,js,je,wk1,divg2)
   do j=js,je
      do i=is,ie+1
         wk1(i,j) = divg2(i,j)-divg2(i,j+1)
      enddo
   enddo

else

   !$OMP parallel do default(none) shared(is,ie,js,je,wk1,wk2)
   do j=js,je+1
      do i=is,ie
         wk2(i,j) = 0.
      enddo
      do i=is,ie+1
         wk1(i,j) = 0.
      enddo
   enddo

endif

!$OMP parallel do default(none) shared(is,ie,js,je,npz,pk,delp,hydrostatic,a2b_ord,gridstruct, &
!$OMP                                  npx,npy,isd,jsd,ng,u,v,wk2,dt,gz,wk1) &
!$OMP                          private(wk)
do k=1,npz

   if ( hydrostatic ) then
      do j=js,je+1
         do i=is,ie+1
            wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
         enddo
      enddo
   else
      if ( a2b_ord==4 ) then
         call a2b_ord4(delp(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng)
      else
         call a2b_ord2(delp(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng)
      endif
   endif

   do j=js,je+1
      do i=is,ie
         u(i,j,k) = gridstruct%rdx(i,j)*(wk2(i,j)+u(i,j,k) + dt/(wk(i,j)+wk(i+1,j)) * &
                                 ((gz(i,j,k+1)-gz(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) &
                                + (gz(i,j,k)-gz(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k))))
      enddo
   enddo
   do j=js,je
      do i=is,ie+1
         v(i,j,k) = gridstruct%rdy(i,j)*(wk1(i,j)+v(i,j,k) + dt/(wk(i,j)+wk(i,j+1)) * &
                                 ((gz(i,j,k+1)-gz(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) &
                                + (gz(i,j,k)-gz(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k))))
      enddo
   enddo
enddo    ! end k-loop

end subroutine one_grad_p


subroutine grad1_p_update(divg2, u, v, pk, gz, dt, ng, gridstruct, bd, npx, npy, npz, ptop, beta, a2b_ord)

integer, intent(in) :: ng, npx, npy, npz, a2b_ord
real,    intent(in) :: dt, ptop, beta
type(fv_grid_bounds_type), intent(IN) :: bd
real, intent(in):: divg2(bd%is:bd%ie+1,bd%js:bd%je+1)
real, intent(inout) ::    pk(bd%isd:bd%ied,  bd%jsd:bd%jed  ,npz+1)
real, intent(inout) ::    gz(bd%isd:bd%ied,  bd%jsd:bd%jed  ,npz+1)
real, intent(inout) ::     u(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) 
real, intent(inout) ::     v(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
type(fv_grid_type), intent(INOUT), target :: gridstruct

! Local:
real:: wk(bd%isd:bd%ied,bd%jsd:bd%jed)
real top_value, alpha
integer i,j,k

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd  = bd%isd
      ied  = bd%ied
      jsd  = bd%jsd
      jed  = bd%jed

alpha = 1. - beta

! pk is pe**kappa if hydrostatic
top_value = ptk

!$OMP parallel do default(none) shared(is,ie,js,je,pk,top_value)
do j=js,je+1
   do i=is,ie+1
      pk(i,j,1) = top_value
   enddo
enddo
!$OMP parallel do default(none) shared(npz,isd,jsd,pk,gridstruct,npx,npy,is,ie,js,je,ng,a2b_ord) &
!$OMP                          private(wk)
do k=2,npz+1
   if ( a2b_ord==4 ) then
      call a2b_ord4(pk(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   else
      call a2b_ord2(pk(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   endif
enddo

!$OMP parallel do default(none) shared(npz,isd,jsd,gz,gridstruct,npx,npy,is,ie,js,je,ng,a2b_ord) &
!$OMP                          private(wk)
do k=1,npz+1
   if ( a2b_ord==4 ) then
      call a2b_ord4( gz(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   else
      call a2b_ord2( gz(isd,jsd,k), wk, gridstruct, npx, npy, is, ie, js, je, ng, .true.)
   endif
enddo

!$OMP parallel do default(none) shared(npz,is,ie,js,je,pk,u,beta,gz,divg2,alpha, &
!$OMP                                  gridstruct,v,dt,du,dv) &          
!$OMP                          private(wk)
do k=1,npz

   do j=js,je+1
      do i=is,ie+1
         wk(i,j) = pk(i,j,k+1) - pk(i,j,k)
      enddo
   enddo

   do j=js,je+1
      do i=is,ie
         u(i,j,k) = u(i,j,k) + beta*du(i,j,k)
         du(i,j,k) = dt/(wk(i,j)+wk(i+1,j)) *  &
              ((gz(i,j,k+1)-gz(i+1,j,k))*(pk(i+1,j,k+1)-pk(i,j,k)) &
              + (gz(i,j,k)-gz(i+1,j,k+1))*(pk(i,j,k+1)-pk(i+1,j,k)))
         u(i,j,k) = (u(i,j,k) + divg2(i,j)-divg2(i+1,j) + alpha*du(i,j,k))*gridstruct%rdx(i,j)
      enddo
   enddo
   do j=js,je
      do i=is,ie+1
         v(i,j,k) = v(i,j,k) + beta*dv(i,j,k)
         dv(i,j,k) = dt/(wk(i,j)+wk(i,j+1)) *  &
              ((gz(i,j,k+1)-gz(i,j+1,k))*(pk(i,j+1,k+1)-pk(i,j,k)) &
              + (gz(i,j,k)-gz(i,j+1,k+1))*(pk(i,j,k+1)-pk(i,j+1,k)))
         v(i,j,k) = (v(i,j,k) + divg2(i,j)-divg2(i,j+1) + alpha*dv(i,j,k))*gridstruct%rdy(i,j)
      enddo
   enddo
enddo    ! end k-loop

end subroutine grad1_p_update


subroutine mix_dp(hydrostatic, w, delp, pt, km, ak, bk, CG, fv_debug, bd)
integer, intent(IN) :: km
real   , intent(IN) :: ak(km+1), bk(km+1)
type(fv_grid_bounds_type), intent(IN) :: bd
real, intent(INOUT), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,km):: pt, delp
real, intent(INOUT), dimension(bd%isd:,bd%jsd:,1:):: w
logical, intent(IN) :: hydrostatic, CG, fv_debug
! Local:
real dp, dpmin
integer i, j, k, ip
integer ifirst, ilast
integer jfirst, jlast

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


if ( CG ) then
   ifirst = is-1; ilast = ie+1
   jfirst = js-1; jlast = je+1
else
   ifirst = is; ilast = ie
   jfirst = js; jlast = je
endif


!$OMP parallel do default(none) shared(jfirst,jlast,km,ifirst,ilast,delp,ak,bk,pt, &
!$OMP                                  hydrostatic,w,fv_debug) &
!$OMP                          private(ip, dpmin, dp)
do 1000 j=jfirst,jlast

   ip = 0

   do k=1, km-1
      dpmin = 0.01 * ( ak(k+1)-ak(k) + (bk(k+1)-bk(k))*1.E5 )
      do i=ifirst, ilast
         if(delp(i,j,k) < dpmin) then
            if (fv_debug) write(*,*) 'Mix_dp: ', i, j, k, mpp_pe(), delp(i,j,k), pt(i,j,k)
            ! Remap from below and mix pt
            dp = dpmin - delp(i,j,k)
            pt(i,j,k) = (pt(i,j,k)*delp(i,j,k) + pt(i,j,k+1)*dp) / dpmin
            if ( .not.hydrostatic ) w(i,j,k) = (w(i,j,k)*delp(i,j,k) + w(i,j,k+1)*dp) / dpmin
            delp(i,j,k) = dpmin
            delp(i,j,k+1) = delp(i,j,k+1) - dp
            ip = ip + 1
         endif
      enddo
   enddo

   ! Bottom (k=km):
   dpmin = 0.01 * ( ak(km+1)-ak(km) + (bk(km+1)-bk(km))*1.E5 )
   do i=ifirst, ilast
      if(delp(i,j,km) < dpmin) then
         if (fv_debug) write(*,*) 'Mix_dp: ', i, j, km, mpp_pe(), delp(i,j,km), pt(i,j,km)
         ! Remap from above and mix pt
         dp = dpmin - delp(i,j,km)
         pt(i,j,km) = (pt(i,j,km)*delp(i,j,km) + pt(i,j,km-1)*dp)/dpmin
         if ( .not.hydrostatic ) w(i,j,km) = (w(i,j,km)*delp(i,j,km) + w(i,j,km-1)*dp) / dpmin
         delp(i,j,km) = dpmin
         delp(i,j,km-1) = delp(i,j,km-1) - dp
         ip = ip + 1
      endif
   enddo
   if ( fv_debug .and. ip/=0 ) write(*,*) 'Warning: Mix_dp', mpp_pe(), j, ip 
   !      if ( ip/=0 ) write(*,*) 'Warning: Mix_dp', mpp_pe(), j, ip 
1000 continue

 end subroutine  mix_dp


 subroutine geopk(ptop, pe, peln, delp, pk, gz, hs, pt, q_con, pkz, km, akap, CG, nested, computehalo, npx, npy, a2b_ord, bd)

   integer, intent(IN) :: km, npx, npy, a2b_ord
   real   , intent(IN) :: akap, ptop
   type(fv_grid_bounds_type), intent(IN) :: bd
   real   , intent(IN) :: hs(bd%isd:bd%ied,bd%jsd:bd%jed)
   real, intent(IN), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,km):: pt, delp
   real, intent(IN), dimension(bd%isd:,bd%jsd:,1:):: q_con
   logical, intent(IN) :: CG, nested, computehalo
   ! !OUTPUT PARAMETERS
   real, intent(OUT), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,km+1):: gz, pk
   real, intent(OUT) :: pe(bd%is-1:bd%ie+1,km+1,bd%js-1:bd%je+1)
   real, intent(out) :: peln(bd%is:bd%ie,km+1,bd%js:bd%je)          ! ln(pe)
   real, intent(out) :: pkz(bd%is:bd%ie,bd%js:bd%je,km)
   ! !DESCRIPTION:
   !    Calculates geopotential and pressure to the kappa.
   ! Local:
   real peg(bd%isd:bd%ied,km+1)
   real pkg(bd%isd:bd%ied,km+1)
   real p1d(bd%isd:bd%ied)
   real logp(bd%isd:bd%ied)
   integer i, j, k
   integer ifirst, ilast
   integer jfirst, jlast

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

   if ( (.not. CG .and. a2b_ord==4) .or. (nested .and. .not. CG) ) then   ! D-Grid
      ifirst = is-2; ilast = ie+2
      jfirst = js-2; jlast = je+2
   else
      ifirst = is-1; ilast = ie+1
      jfirst = js-1; jlast = je+1
   endif

   if (nested .and. computehalo) then
      if (is == 1)     ifirst = isd
      if (ie == npx-1) ilast  = ied
      if (js == 1)     jfirst = jsd
      if (je == npy-1) jlast  = jed
   end if

!$OMP parallel do default(none) shared(jfirst,jlast,ifirst,ilast,pk,km,gz,hs,ptop,ptk, &
!$OMP                                  js,je,is,ie,peln,peln1,pe,delp,akap,pt,CG,pkz,q_con) &
!$OMP                          private(peg, pkg, p1d, logp)
   do 2000 j=jfirst,jlast

      do i=ifirst, ilast
         p1d(i) = ptop
         pk(i,j,1) = ptk
         gz(i,j,km+1) = hs(i,j)
#ifdef USE_COND
         peg(i,1) = ptop
         pkg(i,1) = ptk
#endif
      enddo

#ifndef SW_DYNAMICS
      if( j>=js .and. j<=je) then
         do i=is,ie
            peln(i,1,j) = peln1
         enddo
      endif
#endif

      if( j>(js-2) .and. j<(je+2) ) then
         do i=max(ifirst,is-1), min(ilast,ie+1) 
            pe(i,1,j) = ptop
         enddo
      endif

      ! Top down
      do k=2,km+1
         do i=ifirst, ilast
            p1d(i)  = p1d(i) + delp(i,j,k-1)
            logp(i) = log(p1d(i))
            pk(i,j,k) = exp( akap*logp(i) ) 
#ifdef USE_COND
            peg(i,k) = peg(i,k-1) + delp(i,j,k-1)*(1.-q_con(i,j,k-1))
            pkg(i,k) = exp( akap*log(peg(i,k)) )
#endif
         enddo

         if( j>(js-2) .and. j<(je+2) ) then
            do i=max(ifirst,is-1), min(ilast,ie+1) 
               pe(i,k,j) = p1d(i)
            enddo
            if( j>=js .and. j<=je) then
               do i=is,ie
                  peln(i,k,j) = logp(i)
               enddo
            endif
         endif

      enddo

      ! Bottom up
      do k=km,1,-1
         do i=ifirst, ilast
#ifdef SW_DYNAMICS
            gz(i,j,k) = gz(i,j,k+1) + pt(i,j,k)*(pk(i,j,k+1)-pk(i,j,k))
#else
#ifdef USE_COND
            gz(i,j,k) = gz(i,j,k+1) + cp_air*pt(i,j,k)*(pkg(i,k+1)-pkg(i,k))
#else
            gz(i,j,k) = gz(i,j,k+1) + cp_air*pt(i,j,k)*(pk(i,j,k+1)-pk(i,j,k))
#endif
#endif
         enddo
      enddo

      if ( .not. CG .and. j .ge. js .and. j .le. je ) then
         do k=1,km
            do i=is,ie
               pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(akap*(peln(i,k+1,j)-peln(i,k,j)))
            enddo
         enddo
      endif

2000  continue
 end subroutine geopk


 subroutine del2_cubed(q, cd, gridstruct, domain, npx, npy, km, nmax, bd)
      !---------------------------------------------------------------
      ! This routine is for filtering the omega field for the physics
      !---------------------------------------------------------------
      integer, intent(in):: npx, npy, km, nmax
      real(kind=R_GRID),    intent(in):: cd            ! cd = K * da_min;   0 < K < 0.25
      type(fv_grid_bounds_type), intent(IN) :: bd
      real, intent(inout):: q(bd%isd:bd%ied,bd%jsd:bd%jed,km)
      type(fv_grid_type), intent(IN), target :: gridstruct
      type(domain2d), intent(INOUT) :: domain
      real, parameter:: r3  = 1./3.
      real :: fx(bd%isd:bd%ied+1,bd%jsd:bd%jed), fy(bd%isd:bd%ied,bd%jsd:bd%jed+1)
      real :: q2(bd%isd:bd%ied,bd%jsd:bd%jed)
      integer i,j,k, n, nt, ntimes
      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      !Local routine pointers
!     real, pointer, dimension(:,:) :: rarea
!     real, pointer, dimension(:,:) :: del6_u, del6_v
!     logical, pointer :: sw_corner, se_corner, ne_corner, nw_corner

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

!     rarea => gridstruct%rarea
!     del6_u => gridstruct%del6_u
!     del6_v => gridstruct%del6_v
      
!     sw_corner => gridstruct%sw_corner
!     nw_corner => gridstruct%nw_corner
!     se_corner => gridstruct%se_corner
!     ne_corner => gridstruct%ne_corner

      ntimes = min(3, nmax)

      call timing_on('COMM_TOTAL')
      call mpp_update_domains(q, domain, complete=.true.)
      call timing_off('COMM_TOTAL')


      do n=1,ntimes
         nt = ntimes - n

!$OMP parallel do default(none) shared(km,q,is,ie,js,je,npx,npy, &
!$OMP                                  nt,isd,jsd,gridstruct,bd, &
!$OMP                                  cd) &
!$OMP                          private(fx, fy)
         do k=1,km

            if ( gridstruct%sw_corner ) then
               q(1,1,k) = (q(1,1,k)+q(0,1,k)+q(1,0,k)) * r3
               q(0,1,k) =  q(1,1,k)
               q(1,0,k) =  q(1,1,k)
            endif
            if ( gridstruct%se_corner ) then
               q(ie, 1,k) = (q(ie,1,k)+q(npx,1,k)+q(ie,0,k)) * r3
               q(npx,1,k) =  q(ie,1,k)
               q(ie, 0,k) =  q(ie,1,k)
            endif
            if ( gridstruct%ne_corner ) then
               q(ie, je,k) = (q(ie,je,k)+q(npx,je,k)+q(ie,npy,k)) * r3
               q(npx,je,k) =  q(ie,je,k)
               q(ie,npy,k) =  q(ie,je,k)
            endif
            if ( gridstruct%nw_corner ) then
               q(1, je,k) = (q(1,je,k)+q(0,je,k)+q(1,npy,k)) * r3
               q(0, je,k) =  q(1,je,k)
               q(1,npy,k) =  q(1,je,k)
            endif

            if(nt>0) call copy_corners(q(isd,jsd,k), npx, npy, 1, gridstruct%nested, bd, &
                 gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner )
            do j=js-nt,je+nt
               do i=is-nt,ie+1+nt
#ifdef USE_SG
                  fx(i,j) = gridstruct%dy(i,j)*gridstruct%sina_u(i,j)*(q(i-1,j,k)-q(i,j,k))*gridstruct%rdxc(i,j)
#else
                  fx(i,j) = gridstruct%del6_v(i,j)*(q(i-1,j,k)-q(i,j,k))
#endif
               enddo
            enddo

            if(nt>0) call copy_corners(q(isd,jsd,k), npx, npy, 2, gridstruct%nested, bd, &
                 gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)
            do j=js-nt,je+1+nt
               do i=is-nt,ie+nt
#ifdef USE_SG
                  fy(i,j) = gridstruct%dx(i,j)*gridstruct%sina_v(i,j)*(q(i,j-1,k)-q(i,j,k))*gridstruct%rdyc(i,j)
#else
                  fy(i,j) = gridstruct%del6_u(i,j)*(q(i,j-1,k)-q(i,j,k))
#endif
               enddo
            enddo

            do j=js-nt,je+nt
               do i=is-nt,ie+nt
                  q(i,j,k) = q(i,j,k) + cd*gridstruct%rarea(i,j)*(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))
               enddo
            enddo
         enddo
      enddo

 end subroutine del2_cubed

 subroutine init_ijk_mem(i1, i2, j1, j2, km, array, var)
      integer, intent(in):: i1, i2, j1, j2, km
      real, intent(inout):: array(i1:i2,j1:j2,km)
      real, intent(in):: var
      integer:: i, j, k

!$OMP parallel do default(none) shared(i1,i2,j1,j2,km,array,var)
      do k=1,km
         do j=j1,j2
            do i=i1,i2
               array(i,j,k) = var
            enddo
         enddo
      enddo

 end subroutine init_ijk_mem


end module dyn_core_mod
