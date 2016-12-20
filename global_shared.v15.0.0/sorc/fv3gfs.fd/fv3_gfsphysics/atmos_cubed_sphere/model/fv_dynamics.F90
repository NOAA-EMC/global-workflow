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
module fv_dynamics_mod
   use constants_mod,       only: grav, pi=>pi_8, radius, hlv, rdgas, omega, rvgas, cp_vapor
   use dyn_core_mod,        only: dyn_core, del2_cubed, init_ijk_mem
   use fv_mapz_mod,         only: compute_total_energy, Lagrangian_to_Eulerian, moist_cv, moist_cp
   use fv_tracer2d_mod,     only: tracer_2d, tracer_2d_1L, tracer_2d_nested
   use fv_grid_utils_mod,   only: cubed_to_latlon, c2l_ord2, g_sum
   use fv_mp_mod,           only: is_master
   use fv_mp_mod,           only: group_halo_update_type
   use fv_mp_mod,           only: start_group_halo_update, complete_group_halo_update
   use fv_timing_mod,       only: timing_on, timing_off
   use diag_manager_mod,    only: send_data
   use fv_diagnostics_mod,  only: fv_time, prt_mxm, range_check, prt_minmax
   use mpp_domains_mod,     only: DGRID_NE, CGRID_NE, mpp_update_domains, domain2D
   use mpp_mod,             only: mpp_pe
   use field_manager_mod,   only: MODEL_ATMOS
   use tracer_manager_mod,  only: get_tracer_index
   use fv_sg_mod,           only: neg_adj3
   use fv_nesting_mod,      only: setup_nested_grid_BCs
   use boundary_mod,        only: nested_grid_BC_apply_intT
   use fv_arrays_mod,       only: fv_grid_type, fv_flags_type, fv_atmos_type, fv_nest_type, fv_diag_type, fv_grid_bounds_type
   use fv_nwp_nudge_mod,    only: do_adiabatic_init

implicit none
   logical :: RF_initialized = .false.
   logical :: pt_initialized = .false.
   logical :: bad_range = .false.
   real, allocatable ::  rf(:)
   integer :: kmax=1
   real :: agrav
#ifdef HIWPP
   real, allocatable:: u00(:,:,:), v00(:,:,:)
#endif
private
public :: fv_dynamics

!---- version number -----
   character(len=128) :: version = '$Id$'
   character(len=128) :: tagname = '$Name$'

contains

!-----------------------------------------------------------------------
!     fv_dynamics :: FV dynamical core driver
!-----------------------------------------------------------------------
 
  subroutine fv_dynamics(npx, npy, npz, nq_tot,  ng, bdt, consv_te, fill,               &
                        reproduce_sum, kappa, cp_air, zvir, ptop, ks, ncnst, n_split,     &
                        q_split, u, v, w, delz, hydrostatic, pt, delp, q,   &
                        ps, pe, pk, peln, pkz, phis, q_con, omga, ua, va, uc, vc,          &
                        ak, bk, mfx, mfy, cx, cy, ze0, hybrid_z, &
                        gridstruct, flagstruct, neststruct, idiag, bd, &
                        parent_grid, domain, time_total)

    real, intent(IN) :: bdt  ! Large time-step
    real, intent(IN) :: consv_te
    real, intent(IN) :: kappa, cp_air
    real, intent(IN) :: zvir, ptop
    real, intent(IN), optional :: time_total

    integer, intent(IN) :: npx
    integer, intent(IN) :: npy
    integer, intent(IN) :: npz
    integer, intent(IN) :: nq_tot             ! transported tracers
    integer, intent(IN) :: ng
    integer, intent(IN) :: ks
    integer, intent(IN) :: ncnst
    integer, intent(IN) :: n_split        ! small-step horizontal dynamics
    integer, intent(IN) :: q_split        ! tracer
    logical, intent(IN) :: fill
    logical, intent(IN) :: reproduce_sum
    logical, intent(IN) :: hydrostatic
    logical, intent(IN) :: hybrid_z       ! Using hybrid_z for remapping

    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(inout), dimension(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) :: u ! D grid zonal wind (m/s)
    real, intent(inout), dimension(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) :: v ! D grid meridional wind (m/s)
    real, intent(inout) :: w(   bd%isd:  ,bd%jsd:  ,1:)  !  W (m/s)
    real, intent(inout) :: pt(  bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! temperature (K)
    real, intent(inout) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)  ! pressure thickness (pascal)
    real, intent(inout) :: q(   bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, ncnst) ! specific humidity and constituents
    real, intent(inout) :: delz(bd%isd:,bd%jsd:,1:)   ! delta-height (m); non-hydrostatic only
    real, intent(inout) ::  ze0(bd%is:, bd%js: ,1:) ! height at edges (m); non-hydrostatic

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:    
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real, intent(inout) :: ps  (bd%isd:bd%ied  ,bd%jsd:bd%jed)           ! Surface pressure (pascal)
    real, intent(inout) :: pe  (bd%is-1:bd%ie+1, npz+1,bd%js-1:bd%je+1)  ! edge pressure (pascal)
    real, intent(inout) :: pk  (bd%is:bd%ie,bd%js:bd%je, npz+1)          ! pe**kappa
    real, intent(inout) :: peln(bd%is:bd%ie,npz+1,bd%js:bd%je)           ! ln(pe)
    real, intent(inout) :: pkz (bd%is:bd%ie,bd%js:bd%je,npz)             ! finite-volume mean pk
    real, intent(inout):: q_con(bd%isd:, bd%jsd:, 1:)
    
!-----------------------------------------------------------------------
! Others:
!-----------------------------------------------------------------------
    real, intent(inout) :: phis(bd%isd:bd%ied,bd%jsd:bd%jed)       ! Surface geopotential (g*Z_surf)
    real, intent(inout) :: omga(bd%isd:bd%ied,bd%jsd:bd%jed,npz)   ! Vertical pressure velocity (pa/s)
    real, intent(inout) :: uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz) ! (uc,vc) mostly used as the C grid winds
    real, intent(inout) :: vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)

    real, intent(inout), dimension(bd%isd:bd%ied ,bd%jsd:bd%jed ,npz):: ua, va
    real, intent(in),    dimension(npz+1):: ak, bk

! Accumulated Mass flux arrays: the "Flux Capacitor"
    real, intent(inout) ::  mfx(bd%is:bd%ie+1, bd%js:bd%je,   npz)
    real, intent(inout) ::  mfy(bd%is:bd%ie  , bd%js:bd%je+1, npz)
! Accumulated Courant number arrays
    real, intent(inout) ::  cx(bd%is:bd%ie+1, bd%jsd:bd%jed, npz)
    real, intent(inout) ::  cy(bd%isd:bd%ied ,bd%js:bd%je+1, npz)

    type(fv_grid_type),  intent(inout), target :: gridstruct
    type(fv_flags_type), intent(INOUT) :: flagstruct
    type(fv_nest_type),  intent(INOUT) :: neststruct
    type(domain2d), intent(INOUT) :: domain
    type(fv_atmos_type), intent(INOUT) :: parent_grid
    type(fv_diag_type), intent(IN) :: idiag

! Local Arrays
      real:: ws(bd%is:bd%ie,bd%js:bd%je)
      real:: te_2d(bd%is:bd%ie,bd%js:bd%je)
      real::   teq(bd%is:bd%ie,bd%js:bd%je)
      real:: ps2(bd%isd:bd%ied,bd%jsd:bd%jed)
      real:: m_fac(bd%is:bd%ie,bd%js:bd%je)
      real:: pfull(npz)
      real, dimension(bd%is:bd%ie):: cvm
      real, allocatable :: dp1(:,:,:), dtdt_m(:,:,:), cappa(:,:,:)
      real:: akap, rdg, ph1, ph2, mdt, gam, amdt, u0
      integer:: kord_tracer(ncnst)
      integer :: i,j,k, n, iq, n_map, nq, nwat, k_split
      integer :: sphum, liq_wat = -999, ice_wat = -999      ! GFDL physics
      integer :: rainwat = -999, snowwat = -999, graupel = -999, cld_amt = -999
      integer :: theta_d = -999
      logical used, last_step, do_omega
      integer, parameter :: max_packs=12
      type(group_halo_update_type), save :: i_pack(max_packs)
      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed
      real :: dt2

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

!     cv_air =  cp_air - rdgas
      agrav = 1. / grav
        dt2 = 0.5*bdt
      k_split = flagstruct%k_split
      nwat = flagstruct%nwat
      nq = nq_tot - flagstruct%dnats
      rdg = -rdgas * agrav
      allocate ( dp1(isd:ied, jsd:jed, 1:npz) )
      
      
#ifdef MOIST_CAPPA
      allocate ( cappa(isd:ied,jsd:jed,npz) )
      call init_ijk_mem(isd,ied, jsd,jed, npz, cappa, 0.)
#else
      allocate ( cappa(isd:isd,jsd:jsd,1) )
      cappa = 0.
#endif
      !We call this BEFORE converting pt to virtual potential temperature, 
      !since we interpolate on (regular) temperature rather than theta.
      if (gridstruct%nested .or. ANY(neststruct%child_grids)) then
                                           call timing_on('NEST_BCs')
         call setup_nested_grid_BCs(npx, npy, npz, zvir, ncnst, &
              u, v, w, pt, delp, delz, q, uc, vc, pkz, &
              neststruct%nested, flagstruct%inline_q, flagstruct%make_nh, ng, &
              gridstruct, flagstruct, neststruct, &
              neststruct%nest_timestep, neststruct%tracer_nest_timestep, &
              domain, bd, nwat)

#ifndef SW_DYNAMICS
         if (gridstruct%nested) then
          !Correct halo values have now been set up for BCs; we can go ahead and apply them too...
            call nested_grid_BC_apply_intT(pt, &
                 0, 0, npx, npy, npz, bd, 1., 1., &
                 neststruct%pt_BC, bctype=neststruct%nestbctype  )
#ifdef USE_COND
            call nested_grid_BC_apply_intT(q_con, &
                 0, 0, npx, npy, npz, bd, 1., 1., &
                 neststruct%q_con_BC, bctype=neststruct%nestbctype  )            
#ifdef MOIST_CAPPA
            call nested_grid_BC_apply_intT(cappa, &
                 0, 0, npx, npy, npz, bd, 1., 1., &
                 neststruct%cappa_BC, bctype=neststruct%nestbctype  )            
#endif
#endif
         endif
#endif
                                           call timing_off('NEST_BCs')
      endif


      if ( flagstruct%no_dycore ) then
         if ( nwat.eq.2 .and. (.not.hydrostatic) ) then
            sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
         endif
         goto 911
      endif

      if ( nwat==0 ) then
             sphum = 1
           cld_amt = -1   ! to cause trouble if (mis)used
      else
             sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
           liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
           ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
           rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
           snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
           graupel = get_tracer_index (MODEL_ATMOS, 'graupel')
           cld_amt = get_tracer_index (MODEL_ATMOS, 'cld_amt')
      endif

      theta_d = get_tracer_index (MODEL_ATMOS, 'theta_d')

#ifdef SW_DYNAMICS
      akap  = 1.
      pfull(1) = 0.5*flagstruct%p_ref
#else
      akap  = kappa

!$OMP parallel do default(none) shared(npz,ak,bk,flagstruct,pfull) &
!$OMP                          private(ph1, ph2)
      do k=1,npz
         ph1 = ak(k  ) + bk(k  )*flagstruct%p_ref
         ph2 = ak(k+1) + bk(k+1)*flagstruct%p_ref
         pfull(k) = (ph2 - ph1) / log(ph2/ph1)
      enddo

    if ( hydrostatic ) then
!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,dp1,zvir,nwat,q,q_con,sphum,liq_wat, &
!$OMP      rainwat,ice_wat,snowwat,graupel) private(cvm)
      do k=1,npz
         do j=js,je
#ifdef USE_COND
             call moist_cp(is,ie,isd,ied,jsd,jed, npz, j, k, nwat, sphum, liq_wat, rainwat,    &
                           ice_wat, snowwat, graupel, q, q_con(is:ie,j,k), cvm)
#endif
            do i=is,ie
               dp1(i,j,k) = zvir*q(i,j,k,sphum)
            enddo
         enddo
      enddo
    else
!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,npz,dp1,zvir,q,q_con,sphum,liq_wat, &
!$OMP                                  rainwat,ice_wat,snowwat,graupel,pkz,flagstruct, & 
!$OMP                                  cappa,kappa,rdg,delp,pt,delz,nwat)              &
!$OMP                          private(cvm)
       do k=1,npz
          if ( flagstruct%moist_phys ) then
          do j=js,je
#ifdef MOIST_CAPPA
             call moist_cv(is,ie,isd,ied,jsd,jed, npz, j, k, nwat, sphum, liq_wat, rainwat,    &
                           ice_wat, snowwat, graupel, q, q_con(is:ie,j,k), cvm)
#endif
             do i=is,ie
                dp1(i,j,k) = zvir*q(i,j,k,sphum)
#ifdef MOIST_CAPPA
               cappa(i,j,k) = rdgas/(rdgas + cvm(i)/(1.+dp1(i,j,k)))
               pkz(i,j,k) = exp(cappa(i,j,k)*log(rdg*delp(i,j,k)*pt(i,j,k)*    &
                            (1.+dp1(i,j,k))*(1.-q_con(i,j,k))/delz(i,j,k)) )
#else
               pkz(i,j,k) = exp( kappa*log(rdg*delp(i,j,k)*pt(i,j,k)*    &
                            (1.+dp1(i,j,k))/delz(i,j,k)) )
! Using dry pressure for the definition of the virtual potential temperature
!              pkz(i,j,k) = exp( kappa*log(rdg*delp(i,j,k)*pt(i,j,k)*    &
!                                      (1.-q(i,j,k,sphum))/delz(i,j,k)) )
#endif
             enddo
          enddo
          else
            do j=js,je
               do i=is,ie
                  dp1(i,j,k) = 0.
                  pkz(i,j,k) = exp(kappa*log(rdg*delp(i,j,k)*pt(i,j,k)/delz(i,j,k)))
               enddo
            enddo
          endif
       enddo
    endif

      if ( flagstruct%fv_debug ) then
#ifdef MOIST_CAPPA
         call prt_mxm('cappa', cappa, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
#endif
         call prt_mxm('PS',        ps, is, ie, js, je, ng,   1, 0.01, gridstruct%area_64, domain)
         call prt_mxm('T_dyn_b',   pt, is, ie, js, je, ng, npz, 1.,   gridstruct%area_64, domain)
         if ( .not. hydrostatic) call prt_mxm('delz',    delz, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
         call prt_mxm('delp_b ', delp, is, ie, js, je, ng, npz, 0.01, gridstruct%area_64, domain)
         call prt_mxm('pk_b',    pk, is, ie, js, je, 0, npz+1, 1.,gridstruct%area_64, domain)
         call prt_mxm('pkz_b',   pkz,is, ie, js, je, 0, npz,   1.,gridstruct%area_64, domain)
      endif

!---------------------
! Compute Total Energy
!---------------------
      if ( consv_te > 0.  .and. (.not.do_adiabatic_init) ) then
           call compute_total_energy(is, ie, js, je, isd, ied, jsd, jed, npz,        &
                                     u, v, w, delz, pt, delp, q, dp1, pe, peln, phis, &
                                     gridstruct%rsin2, gridstruct%cosa_s, &
                                     zvir, cp_air, rdgas, hlv, te_2d, ua, va, teq,        &
                                     flagstruct%moist_phys, nwat, sphum, liq_wat, rainwat,   &
                                     ice_wat, snowwat, graupel, hydrostatic, idiag%id_te)
           if( idiag%id_te>0 ) then
               used = send_data(idiag%id_te, teq, fv_time)
!              te_den=1.E-9*g_sum(teq, is, ie, js, je, ng, area, 0)/(grav*4.*pi*radius**2)
!              if(is_master())  write(*,*) 'Total Energy Density (Giga J/m**2)=',te_den
           endif
      endif

      if( (flagstruct%consv_am.or.idiag%id_amdt>0) .and. (.not.do_adiabatic_init) ) then
          call compute_aam(npz, is, ie, js, je, isd, ied, jsd, jed, gridstruct, bd,   &
                           ptop, ua, va, u, v, delp, teq, ps2, m_fac)
      endif

      if( flagstruct%tau > 0. ) then
        if ( gridstruct%grid_type<4 ) then
             call Rayleigh_Super(abs(bdt), npx, npy, npz, ks, pfull, phis, flagstruct%tau, u, v, w, pt,  &
                  ua, va, delz, gridstruct%agrid, cp_air, rdgas, ptop, hydrostatic, (.not. neststruct%nested), flagstruct%rf_cutoff, gridstruct, domain, bd)
        else
             call Rayleigh_Friction(abs(bdt), npx, npy, npz, ks, pfull, flagstruct%tau, u, v, w, pt,  &
                  ua, va, delz, cp_air, rdgas, ptop, hydrostatic, .true., flagstruct%rf_cutoff, gridstruct, domain, bd)
        endif
      endif

#endif

#ifndef SW_DYNAMICS
! Convert pt to virtual potential temperature on the first timestep
  if ( flagstruct%adiabatic .and. flagstruct%kord_tm>0 ) then
     if ( .not.pt_initialized )then
!$OMP parallel do default(none) shared(theta_d,is,ie,js,je,npz,pt,pkz,q)
       do k=1,npz
          do j=js,je
             do i=is,ie
                pt(i,j,k) = pt(i,j,k)/pkz(i,j,k)
             enddo
          enddo
          if ( theta_d>0 ) then
             do j=js,je
                do i=is,ie
                   q(i,j,k,theta_d) = pt(i,j,k)
                enddo
             enddo
          endif
       enddo
       pt_initialized = .true.
     endif
  else
!$OMP parallel do default(none) shared(is,ie,js,je,npz,pt,dp1,pkz,q_con)
  do k=1,npz
     do j=js,je
        do i=is,ie
#ifdef USE_COND
           pt(i,j,k) = pt(i,j,k)*(1.+dp1(i,j,k))*(1.-q_con(i,j,k))/pkz(i,j,k)
#else
           pt(i,j,k) = pt(i,j,k)*(1.+dp1(i,j,k))/pkz(i,j,k)
#endif
        enddo
     enddo
  enddo
  endif
#endif

  last_step = .false.
  mdt = bdt / real(k_split)

  if ( idiag%id_mdt > 0 .and. (.not. do_adiabatic_init) ) then
       allocate ( dtdt_m(is:ie,js:je,npz) )
!$OMP parallel do default(none) shared(is,ie,js,je,npz,dtdt_m)
       do k=1,npz
          do j=js,je
             do i=is,ie
                dtdt_m(i,j,k) = 0.
             enddo
          enddo
       enddo
  endif


                                                  call timing_on('FV_DYN_LOOP')
  do n_map=1, k_split   ! first level of time-split
                                           call timing_on('COMM_TOTAL')
#ifdef USE_COND
      call start_group_halo_update(i_pack(11), q_con, domain)
#ifdef MOIST_CAPPA
      call start_group_halo_update(i_pack(12), cappa, domain)
#endif
#endif
      call start_group_halo_update(i_pack(1), delp, domain, complete=.false.)
      call start_group_halo_update(i_pack(1), pt,   domain, complete=.true.)
#ifndef ROT3
      call start_group_halo_update(i_pack(8), u, v, domain, gridtype=DGRID_NE)
#endif
                                           call timing_off('COMM_TOTAL')
!$OMP parallel do default(none) shared(isd,ied,jsd,jed,npz,dp1,delp)
      do k=1,npz
         do j=jsd,jed
            do i=isd,ied
               dp1(i,j,k) = delp(i,j,k)
            enddo
         enddo
      enddo

      if ( n_map==k_split ) last_step = .true.

#ifdef USE_COND
                                           call timing_on('COMM_TOTAL')
     call complete_group_halo_update(i_pack(11), domain)
#ifdef MOIST_CAPPA
     call complete_group_halo_update(i_pack(12), domain)
#endif
                                           call timing_off('COMM_TOTAL')
#endif

                                           call timing_on('DYN_CORE')
      call dyn_core(npx, npy, npz, ng, sphum, nq, mdt, n_split, zvir, cp_air, akap, cappa, grav, hydrostatic, &
                    u, v, w, delz, pt, q, delp, pe, pk, phis, ws, omga, ptop, pfull, ua, va,           & 
                    uc, vc, mfx, mfy, cx, cy, pkz, peln, q_con, ak, bk, ks, &
                    gridstruct, flagstruct, neststruct, idiag, bd, &
                    domain, n_map==1, i_pack, last_step, time_total)
                                           call timing_off('DYN_CORE')


#ifdef SW_DYNAMICS
!$OMP parallel do default(none) shared(is,ie,js,je,delp,agrav)
      do j=js,je
         do i=is,ie
            ps(i,j) = delp(i,j,1) * agrav
         enddo
      enddo
#else
      if( .not. flagstruct%inline_q .and. nq /= 0 ) then    
!--------------------------------------------------------
! Perform large-time-step scalar transport using the accumulated CFL and
! mass fluxes
                                              call timing_on('tracer_2d')
       !!! CLEANUP: merge these two calls?
       if (gridstruct%nested) then
         call tracer_2d_nested(q, dp1, mfx, mfy, cx, cy, gridstruct, bd, domain, npx, npy, npz, nq,    &
                        flagstruct%hord_tr, q_split, mdt, idiag%id_divg, i_pack(10), &
                        flagstruct%nord_tr, flagstruct%trdm2, &
                        k_split, neststruct, parent_grid)          
       else
         if ( flagstruct%z_tracer ) then
         call tracer_2d_1L(q, dp1, mfx, mfy, cx, cy, gridstruct, bd, domain, npx, npy, npz, nq,    &
                        flagstruct%hord_tr, q_split, mdt, idiag%id_divg, i_pack(10), &
                        flagstruct%nord_tr, flagstruct%trdm2)
         else
         call tracer_2d(q, dp1, mfx, mfy, cx, cy, gridstruct, bd, domain, npx, npy, npz, nq,    &
                        flagstruct%hord_tr, q_split, mdt, idiag%id_divg, i_pack(10), &
                        flagstruct%nord_tr, flagstruct%trdm2)
         endif
       endif
                                             call timing_off('tracer_2d')

     if ( flagstruct%moist_phys ) then
                                                  call timing_on('Fill2D')
      if ( liq_wat > 0 )  &
       call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,liq_wat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
      if ( nwat > 2 ) then       ! FV3_GFS nwat=2; nq=4
       if ( rainwat > 0 )  &
        call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,rainwat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
       if ( ice_wat > 0  )  &
        call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,ice_wat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
       if ( snowwat > 0 )  &
        call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,snowwat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
       if ( graupel > 0 )  &
        call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,graupel), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
      endif
                                                  call timing_off('Fill2D')
     endif

         if( last_step .and. idiag%id_divg>0 ) then
             used = send_data(idiag%id_divg, dp1, fv_time) 
             if(flagstruct%fv_debug) call prt_mxm('divg',  dp1, is, ie, js, je, 0, npz, 1.,gridstruct%area_64, domain)
         endif
      endif

      if ( npz > 4 ) then
!------------------------------------------------------------------------
! Peroform vertical remapping from Lagrangian control-volume to
! the Eulerian coordinate as specified by the routine set_eta.
! Note that this finite-volume dycore is otherwise independent of the vertical
! Eulerian coordinate.
!------------------------------------------------------------------------

         do iq=1,nq
                                kord_tracer(iq) = flagstruct%kord_tr
            if ( iq==cld_amt )  kord_tracer(iq) = 9      ! monotonic
         enddo

         do_omega = hydrostatic .and. last_step
                                                  call timing_on('Remapping')
#ifdef AVEC_TIMERS
                                                  call avec_timer_start(6)
#endif

         call Lagrangian_to_Eulerian(last_step, consv_te, ps, pe, delp,          &
                     pkz, pk, mdt, bdt, npz, is,ie,js,je, isd,ied,jsd,jed,       &
                     nq, nwat, sphum, q_con, u,  v, w, delz, pt, q, phis,    &
                     zvir, cp_air, akap, cappa, flagstruct%kord_mt, flagstruct%kord_wz, &
                     kord_tracer, flagstruct%kord_tm, peln, te_2d,               &
                     ng, ua, va, omga, dp1, ws, fill, reproduce_sum,             &
                     idiag%id_mdt>0, dtdt_m, ptop, ak, bk, gridstruct, domain, ze0,  &
                     flagstruct%do_sat_adj, hydrostatic, hybrid_z, do_omega,         &
                     flagstruct%adiabatic, do_adiabatic_init)

#ifdef AVEC_TIMERS
                                                  call avec_timer_stop(6)
#endif
                                                  call timing_off('Remapping')
#ifdef MOIST_CAPPA
         if ( neststruct%nested .and. .not. last_step) then
            call nested_grid_BC_apply_intT(cappa, &
                 0, 0, npx, npy, npz, bd, real(n_map+1), real(k_split), &
                 neststruct%cappa_BC, bctype=neststruct%nestbctype  )
         endif
#endif

         if( last_step )  then
            if( .not. hydrostatic ) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,omga,delp,delz,w)
               do k=1,npz
                  do j=js,je
                     do i=is,ie
                        omga(i,j,k) = delp(i,j,k)/delz(i,j,k)*w(i,j,k)
                     enddo
                  enddo
               enddo
            endif
!--------------------------
! Filter omega for physics:
!--------------------------
            if(flagstruct%nf_omega>0)    &
            call del2_cubed(omga, 0.18*gridstruct%da_min, gridstruct, domain, npx, npy, npz, flagstruct%nf_omega, bd)
         endif
      end if
#endif
  enddo    ! n_map loop
                                                  call timing_off('FV_DYN_LOOP')
  if ( idiag%id_mdt > 0 .and. (.not.do_adiabatic_init) ) then
! Output temperature tendency due to inline moist physics:
!$OMP parallel do default(none) shared(is,ie,js,je,npz,dtdt_m,bdt)
       do k=1,npz
          do j=js,je
             do i=is,ie
                dtdt_m(i,j,k) = dtdt_m(i,j,k) / bdt
             enddo
          enddo
       enddo
       used = send_data(idiag%id_mdt, dtdt_m, fv_time)
       deallocate ( dtdt_m )
  endif

  if( nwat==6 ) then
      call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,sphum  ), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
      call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,liq_wat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
      call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,rainwat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
     if (cld_amt > 0) then
      call neg_adj3(is, ie, js, je, ng, npz,        &
                    flagstruct%hydrostatic,         &
                    peln, delz,                     &
                    pt, delp, q(isd,jsd,1,sphum),   &
                              q(isd,jsd,1,liq_wat), &
                              q(isd,jsd,1,rainwat), &
                              q(isd,jsd,1,ice_wat), &
                              q(isd,jsd,1,snowwat), &
                              q(isd,jsd,1,graupel), &
                              q(isd,jsd,1,cld_amt), flagstruct%check_negative)
     else
        call neg_adj3(is, ie, js, je, ng, npz,        &
                      flagstruct%hydrostatic,         &
                      peln, delz,                     &
                      pt, delp, q(isd,jsd,1,sphum),   &
                                q(isd,jsd,1,liq_wat), &
                                q(isd,jsd,1,rainwat), &
                                q(isd,jsd,1,ice_wat), &
                                q(isd,jsd,1,snowwat), &
                                q(isd,jsd,1,graupel), check_negative=flagstruct%check_negative)
     endif
      call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,ice_wat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
      call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,snowwat), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
      call fill2D(is, ie, js, je, ng, npz, q(isd,jsd,1,graupel), delp, gridstruct%area, domain, neststruct%nested, npx, npy)
     if ( flagstruct%fv_debug ) then
       call prt_mxm('T_dyn_a3',    pt, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
       call prt_mxm('SPHUM_dyn',   q(isd,jsd,1,sphum  ), is, ie, js, je, ng, npz, 1.,gridstruct%area_64, domain)
       call prt_mxm('liq_wat_dyn', q(isd,jsd,1,liq_wat), is, ie, js, je, ng, npz, 1.,gridstruct%area_64, domain)
       call prt_mxm('rainwat_dyn', q(isd,jsd,1,rainwat), is, ie, js, je, ng, npz, 1.,gridstruct%area_64, domain)
       call prt_mxm('ice_wat_dyn', q(isd,jsd,1,ice_wat), is, ie, js, je, ng, npz, 1.,gridstruct%area_64, domain)
       call prt_mxm('snowwat_dyn', q(isd,jsd,1,snowwat), is, ie, js, je, ng, npz, 1.,gridstruct%area_64, domain)
       call prt_mxm('graupel_dyn', q(isd,jsd,1,graupel), is, ie, js, je, ng, npz, 1.,gridstruct%area_64, domain)
     endif
  endif

  if( (flagstruct%consv_am.or.idiag%id_amdt>0.or.idiag%id_aam>0) .and. (.not.do_adiabatic_init)  ) then
      call compute_aam(npz, is, ie, js, je, isd, ied, jsd, jed, gridstruct, bd,   &
                       ptop, ua, va, u, v, delp, te_2d, ps, m_fac)
      if( idiag%id_aam>0 ) then
          used = send_data(idiag%id_aam, te_2d, fv_time)
          if ( prt_minmax ) then
             gam = g_sum( domain, te_2d, is, ie, js, je, ng, gridstruct%area_64, 0) 
             if( is_master() ) write(6,*) 'Total AAM =', gam
          endif
      endif
  endif

  if( (flagstruct%consv_am.or.idiag%id_amdt>0) .and. (.not.do_adiabatic_init)  ) then
!$OMP parallel do default(none) shared(is,ie,js,je,te_2d,teq,dt2,ps2,ps,idiag) 
      do j=js,je
         do i=is,ie
! Note: the mountain torque computation contains also numerical error
! The numerical error is mostly from the zonal gradient of the terrain (zxg)
            te_2d(i,j) = te_2d(i,j)-teq(i,j) + dt2*(ps2(i,j)+ps(i,j))*idiag%zxg(i,j)
         enddo
      enddo
      if( idiag%id_amdt>0 ) used = send_data(idiag%id_amdt, te_2d/bdt, fv_time)

      if ( flagstruct%consv_am .or. prt_minmax ) then
         amdt = g_sum( domain, te_2d, is, ie, js, je, ng, gridstruct%area_64, 0, reproduce=.true.) 
         u0 = -radius*amdt/g_sum( domain, m_fac, is, ie, js, je, ng, gridstruct%area_64, 0,reproduce=.true.)
         if(is_master() .and. prt_minmax)         &
         write(6,*) 'Dynamic AM tendency (Hadleys)=', amdt/(bdt*1.e18), 'del-u (per day)=', u0*86400./bdt
      endif

    if( flagstruct%consv_am ) then
!$OMP parallel do default(none) shared(is,ie,js,je,m_fac,u0,gridstruct)
      do j=js,je
         do i=is,ie
            m_fac(i,j) = u0*cos(gridstruct%agrid(i,j,2))
         enddo
      enddo
!$OMP parallel do default(none) shared(is,ie,js,je,npz,hydrostatic,pt,m_fac,ua,cp_air, &
!$OMP                                  u,u0,gridstruct,v )
      do k=1,npz
      do j=js,je+1
         do i=is,ie
            u(i,j,k) = u(i,j,k) + u0*gridstruct%l2c_u(i,j)
         enddo
      enddo
      do j=js,je
         do i=is,ie+1
            v(i,j,k) = v(i,j,k) + u0*gridstruct%l2c_v(i,j)
         enddo
      enddo
      enddo
    endif   !  consv_am
  endif

911  call cubed_to_latlon(u, v, ua, va, gridstruct, &
          npx, npy, npz, 1, gridstruct%grid_type, domain, gridstruct%nested, flagstruct%c2l_ord, bd)

  deallocate(dp1)
  deallocate(cappa)

     if ( flagstruct%fv_debug ) then
       call prt_mxm('UA', ua, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
       call prt_mxm('VA', va, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
       call prt_mxm('TA', pt, is, ie, js, je, ng, npz, 1., gridstruct%area_64, domain)
     endif

  if ( flagstruct%range_warn ) then
       call range_check('UA_dyn', ua, is, ie, js, je, ng, npz, gridstruct%agrid,   &
                         -220., 260., bad_range)
       call range_check('VA_dyn', ua, is, ie, js, je, ng, npz, gridstruct%agrid,   &
                         -220., 260., bad_range)
#ifndef SW_DYNAMICS
       call range_check('TA_dyn', pt, is, ie, js, je, ng, npz, gridstruct%agrid,   &
#ifdef HIWPP
                         130., 335., bad_range)
#else
                         160., 335., bad_range)
#endif
       if ( .not. hydrostatic ) &
       call range_check('W_dyn', w, is, ie, js, je, ng, npz, gridstruct%agrid,   &
                         -50., 100., bad_range)
#endif

  endif

  end subroutine fv_dynamics


 subroutine Rayleigh_Super(dt, npx, npy, npz, ks, pm, phis, tau, u, v, w, pt,  &
                           ua, va, delz, agrid, cp, rg, ptop, hydrostatic, conserve, rf_cutoff, gridstruct, domain, bd)
    real, intent(in):: dt
    real, intent(in):: tau              ! time scale (days)
    real, intent(in):: cp, rg, ptop, rf_cutoff
    real, intent(in),  dimension(npz):: pm
    integer, intent(in):: npx, npy, npz, ks
    logical, intent(in):: hydrostatic
    logical, intent(in):: conserve
    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(inout):: u(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) ! D grid zonal wind (m/s)
    real, intent(inout):: v(bd%isd:bd%ied+1,bd%jsd:bd%jed,npz) ! D grid meridional wind (m/s)
    real, intent(inout)::  w(bd%isd:      ,bd%jsd:      ,1: ) ! cell center vertical wind (m/s)
    real, intent(inout):: pt(bd%isd:bd%ied,bd%jsd:bd%jed,npz) ! temp
    real, intent(inout):: ua(bd%isd:bd%ied,bd%jsd:bd%jed,npz) ! 
    real, intent(inout):: va(bd%isd:bd%ied,bd%jsd:bd%jed,npz) ! 
    real, intent(inout):: delz(bd%isd:    ,bd%jsd:      ,1: ) ! delta-height (m); non-hydrostatic only
    real,   intent(in) :: agrid(bd%isd:bd%ied,  bd%jsd:bd%jed,2)
    real, intent(in) :: phis(bd%isd:bd%ied,bd%jsd:bd%jed)     ! Surface geopotential (g*Z_surf)
    type(fv_grid_type), intent(IN) :: gridstruct
    type(domain2d), intent(INOUT) :: domain
!
    real, allocatable ::  u2f(:,:,:)
    real, parameter:: u0   = 60.   ! scaling velocity
    real, parameter:: sday = 86400.
    real rcv, tau0
    integer i, j, k

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

    rcv = 1. / (cp - rg)

     if ( .not. RF_initialized ) then
#ifdef HIWPP
          allocate ( u00(is:ie,  js:je+1,npz) )
          allocate ( v00(is:ie+1,js:je  ,npz) )
!$OMP parallel do default(none) shared(is,ie,js,je,npz,u00,u,v00,v)
          do k=1,npz
             do j=js,je+1
                do i=is,ie
                   u00(i,j,k) = u(i,j,k)
                enddo
             enddo
             do j=js,je
                do i=is,ie+1
                   v00(i,j,k) = v(i,j,k)
                enddo
             enddo
          enddo
#endif
#ifdef SMALL_EARTH
          tau0 = tau
#else
          tau0 = tau * sday
#endif
          allocate( rf(npz) )
          rf(:) = 0.

          if( is_master() ) write(6,*) 'Rayleigh friction E-folding time (days):'
          do k=1, npz
             if ( pm(k) < rf_cutoff ) then
                  rf(k) = dt/tau0*sin(0.5*pi*log(rf_cutoff/pm(k))/log(rf_cutoff/ptop))**2
                  if( is_master() ) write(6,*) k, 0.01*pm(k), dt/(rf(k)*sday)
                  kmax = k
             else
                  exit
             endif
          enddo
          RF_initialized = .true.
     endif

    call c2l_ord2(u, v, ua, va, gridstruct, npz, gridstruct%grid_type, bd, gridstruct%nested)

    allocate( u2f(isd:ied,jsd:jed,kmax) )

    if (gridstruct%nested) then
!$OMP parallel do default(none) shared(is,ie,js,je,kmax,pm,rf_cutoff,hydrostatic,ua,va,agrid, &
!$OMP                                  u2f,rf,w)
    do k=1,kmax
       if ( pm(k) < rf_cutoff ) then
          do j=js-1,je+1
             if ( hydrostatic ) then
                do i=is-1,ie+1
                   if ( sqrt(ua(i,j,1)**2+va(i,j,1)**2)>25.*cos(agrid(i,j,2)) )  then
                      u2f(i,j,k) = 1./(1.+rf(k)*sqrt(ua(i,j,k)**2+va(i,j,k)**2)/u0)
                   else
                      u2f(i,j,k) = 1.
                   endif
                enddo
             else
                do i=is-1,ie+1
                   if ( sqrt(ua(i,j,1)**2+va(i,j,1)**2)>25.*cos(agrid(i,j,2)) .or. abs(w(i,j,1))>0.05 )  then
                      !!! DEBUG CODE
                      !print*, i,j,k, ua(i,j,k), va(i,j,k), w(i,j,k)
                      !!! END DEBUG CODE
                      u2f(i,j,k) = 1./(1.+rf(k)*sqrt(ua(i,j,k)**2+va(i,j,k)**2+w(i,j,k)**2)/u0)
                   else
                      u2f(i,j,k) = 1.
                   endif
                enddo
             endif
          enddo
       endif ! p check
    enddo

    else
!$OMP parallel do default(none) shared(is,ie,js,je,kmax,pm,rf_cutoff,hydrostatic,ua,va,agrid, &
!$OMP                                  u2f,rf,w)
    do k=1,kmax
       if ( pm(k) < rf_cutoff ) then
       do j=js,je
        if ( hydrostatic ) then
          do i=is,ie
             if ( sqrt(ua(i,j,1)**2+va(i,j,1)**2)>25.*cos(agrid(i,j,2)) )  then
                  u2f(i,j,k) = 1./(1.+rf(k)*sqrt(ua(i,j,k)**2+va(i,j,k)**2)/u0)
             else
                  u2f(i,j,k) = 1.
             endif
          enddo
        else
          do i=is,ie
             if ( sqrt(ua(i,j,1)**2+va(i,j,1)**2)>25.*cos(agrid(i,j,2)) .or. abs(w(i,j,1))>0.05 )  then
                  u2f(i,j,k) = 1./(1.+rf(k)*sqrt(ua(i,j,k)**2+va(i,j,k)**2+w(i,j,k)**2)/u0)
             else
                  u2f(i,j,k) = 1.
             endif
          enddo
        endif
       enddo
       endif ! p check
    enddo
    endif
                                        call timing_on('COMM_TOTAL')
    call mpp_update_domains(u2f, domain)
                                        call timing_off('COMM_TOTAL')

!$OMP parallel do default(none) shared(is,ie,js,je,kmax,pm,rf_cutoff,w,rf,u,v, &
#ifdef HIWPP
!$OMP                                  u00,v00, &
#endif
!$OMP                                  conserve,hydrostatic,pt,ua,va,u2f,cp,rg,ptop,rcv)
     do k=1,kmax
        if ( pm(k) < rf_cutoff ) then
#ifdef HIWPP
           if (.not. hydrostatic) then
              do j=js,je
                 do i=is,ie
                    w(i,j,k) = w(i,j,k)/(1.+rf(k))
                 enddo
              enddo
           endif
             do j=js,je+1
                do i=is,ie
                   u(i,j,k) = (u(i,j,k)+rf(k)*u00(i,j,k))/(1.+rf(k))
                enddo
             enddo
             do j=js,je
                do i=is,ie+1
                   v(i,j,k) = (v(i,j,k)+rf(k)*v00(i,j,k))/(1.+rf(k))
                enddo
             enddo
#else
! Add heat so as to conserve TE
          if ( conserve ) then
             if ( hydrostatic ) then
               do j=js,je
                  do i=is,ie
                     pt(i,j,k) = pt(i,j,k) + 0.5*(ua(i,j,k)**2+va(i,j,k)**2)*(1.-u2f(i,j,k)**2)/(cp-rg*ptop/pm(k))
                  enddo
               enddo
             else
               do j=js,je
                  do i=is,ie
                     pt(i,j,k) = pt(i,j,k) + 0.5*(ua(i,j,k)**2+va(i,j,k)**2+w(i,j,k)**2)*(1.-u2f(i,j,k)**2)*rcv
                  enddo
               enddo
             endif
          endif
             do j=js,je+1
                do i=is,ie
                   u(i,j,k) = 0.5*(u2f(i,j-1,k)+u2f(i,j,k))*u(i,j,k)
                enddo
             enddo
             do j=js,je
                do i=is,ie+1
                   v(i,j,k) = 0.5*(u2f(i-1,j,k)+u2f(i,j,k))*v(i,j,k)
                enddo
             enddo
          if ( .not. hydrostatic ) then
             do j=js,je
                do i=is,ie
                   w(i,j,k) = u2f(i,j,k)*w(i,j,k)
                enddo
             enddo
          endif
#endif
        endif
     enddo

     deallocate ( u2f )

 end subroutine Rayleigh_Super


 subroutine Rayleigh_Friction(dt, npx, npy, npz, ks, pm, tau, u, v, w, pt,  &
                              ua, va, delz, cp, rg, ptop, hydrostatic, conserve, &
                              rf_cutoff, gridstruct, domain, bd)
    real, intent(in):: dt
    real, intent(in):: tau              ! time scale (days)
    real, intent(in):: cp, rg, ptop, rf_cutoff
    real, intent(in),  dimension(npz):: pm
    integer, intent(in):: npx, npy, npz, ks
    logical, intent(in):: hydrostatic
    logical, intent(in):: conserve
    type(fv_grid_bounds_type), intent(IN) :: bd
    real, intent(inout):: u(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz) ! D grid zonal wind (m/s)
    real, intent(inout):: v(bd%isd:bd%ied+1,bd%jsd:bd%jed,npz) ! D grid meridional wind (m/s)
    real, intent(inout)::  w(bd%isd:      ,bd%jsd:      ,1: ) ! cell center vertical wind (m/s)
    real, intent(inout):: pt(bd%isd:bd%ied,bd%jsd:bd%jed,npz) ! temp
    real, intent(inout):: ua(bd%isd:bd%ied,bd%jsd:bd%jed,npz) ! 
    real, intent(inout):: va(bd%isd:bd%ied,bd%jsd:bd%jed,npz) ! 
    real, intent(inout):: delz(bd%isd:    ,bd%jsd:      ,1: ) ! delta-height (m); non-hydrostatic only
    type(fv_grid_type), intent(IN) :: gridstruct
    type(domain2d), intent(INOUT) :: domain
! local:
    real, allocatable ::  u2f(:,:,:)
    real, parameter:: sday = 86400.
    real, parameter:: u000 = 4900.   ! scaling velocity  **2
    real  rcv
    integer i, j, k

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


    rcv = 1. / (cp - rg)

    if ( .not. RF_initialized ) then
          allocate( rf(npz) )
          if( is_master() ) write(6,*) 'Rayleigh friction E-folding time (days):'
          do k=1, npz
             if ( pm(k) < rf_cutoff ) then
                  rf(k) = dt/(tau*sday)*sin(0.5*pi*log(rf_cutoff/pm(k))/log(rf_cutoff/ptop))**2
                  if( is_master() ) write(6,*) k, 0.01*pm(k), dt/(rf(k)*sday)
                  kmax = k
             else
                  exit
             endif
          enddo
          RF_initialized = .true.
    endif

    allocate( u2f(isd:ied,jsd:jed,kmax) )

    call c2l_ord2(u, v, ua, va, gridstruct, npz, gridstruct%grid_type, bd, gridstruct%nested)

!$OMP parallel do default(none) shared(is,ie,js,je,kmax,u2f,hydrostatic,ua,va,w)
    do k=1,kmax
        if ( hydrostatic ) then
           do j=js,je
              do i=is,ie
                 u2f(i,j,k) = ua(i,j,k)**2 + va(i,j,k)**2
              enddo
           enddo
        else
           do j=js,je
              do i=is,ie
                 u2f(i,j,k) = ua(i,j,k)**2 + va(i,j,k)**2 + w(i,j,k)**2
              enddo
           enddo
        endif
    enddo

                                        call timing_on('COMM_TOTAL')
    call mpp_update_domains(u2f, domain)
                                        call timing_off('COMM_TOTAL')

!$OMP parallel do default(none) shared(is,ie,js,je,kmax,conserve,hydrostatic,pt,u2f,cp,rg, &
!$OMP                                  ptop,pm,rf,delz,rcv,u,v,w)
     do k=1,kmax

        if ( conserve ) then
           if ( hydrostatic ) then
             do j=js,je
                do i=is,ie
                   pt(i,j,k) = pt(i,j,k) + 0.5*u2f(i,j,k)/(cp-rg*ptop/pm(k))      &
                             * ( 1. - 1./(1.+rf(k)*sqrt(u2f(i,j,k)/u000))**2 )
                enddo
             enddo
           else
             do j=js,je
                do i=is,ie
                   delz(i,j,k) = delz(i,j,k) / pt(i,j,k)
                   pt(i,j,k) = pt(i,j,k) + 0.5*u2f(i,j,k) * rcv      &
                             * ( 1. - 1./(1.+rf(k)*sqrt(u2f(i,j,k)/u000))**2 )
                   delz(i,j,k) = delz(i,j,k) * pt(i,j,k)
                enddo
             enddo
           endif
        endif

        do j=js-1,je+1
           do i=is-1,ie+1
              u2f(i,j,k) = rf(k)*sqrt(u2f(i,j,k)/u000)
           enddo
        enddo

        do j=js,je+1
           do i=is,ie
              u(i,j,k) = u(i,j,k) / (1.+0.5*(u2f(i,j-1,k)+u2f(i,j,k)))
           enddo
        enddo
        do j=js,je
           do i=is,ie+1
              v(i,j,k) = v(i,j,k) / (1.+0.5*(u2f(i-1,j,k)+u2f(i,j,k)))
           enddo
        enddo

        if ( .not. hydrostatic ) then
              do j=js,je
                 do i=is,ie
                    w(i,j,k) = w(i,j,k) / (1.+u2f(i,j,k))
                 enddo
              enddo
        endif

     enddo

     deallocate ( u2f )

 end subroutine Rayleigh_Friction

 subroutine compute_aam(npz, is, ie, js, je, isd, ied, jsd, jed, gridstruct, bd,   &
                        ptop, ua, va, u, v, delp, aam, ps, m_fac)
! Compute vertically (mass) integrated Atmospheric Angular Momentum
    integer, intent(in):: npz
    integer, intent(in):: is,  ie,  js,  je
    integer, intent(in):: isd, ied, jsd, jed
    real, intent(in):: ptop
    real, intent(inout):: u(isd:ied  ,jsd:jed+1,npz) ! D grid zonal wind (m/s)
    real, intent(inout):: v(isd:ied+1,jsd:jed,npz) ! D grid meridional wind (m/s)
    real, intent(inout):: delp(isd:ied,jsd:jed,npz)
    real, intent(inout), dimension(isd:ied,jsd:jed, npz):: ua, va
    real, intent(out):: aam(is:ie,js:je)
    real, intent(out):: m_fac(is:ie,js:je)
    real, intent(out):: ps(isd:ied,jsd:jed)
    type(fv_grid_bounds_type), intent(IN) :: bd
    type(fv_grid_type), intent(IN) :: gridstruct
! local:
    real, dimension(is:ie):: r1, r2, dm
    integer i, j, k

    call c2l_ord2(u, v, ua, va, gridstruct, npz, gridstruct%grid_type, bd, gridstruct%nested)
    
!$OMP parallel do default(none) shared(is,ie,js,je,npz,gridstruct,aam,m_fac,ps,ptop,delp,agrav,ua) &
!$OMP                          private(r1, r2, dm)
  do j=js,je
     do i=is,ie
        r1(i) = radius*cos(gridstruct%agrid(i,j,2))
        r2(i) = r1(i)*r1(i)
        aam(i,j) = 0.
        m_fac(i,j) = 0.
        ps(i,j) = ptop
     enddo
     do k=1,npz
        do i=is,ie
           dm(i) = delp(i,j,k)
           ps(i,j) = ps(i,j) + dm(i)
           dm(i) = dm(i)*agrav
           aam(i,j) = aam(i,j) + (r2(i)*omega + r1(i)*ua(i,j,k)) * dm(i)
           m_fac(i,j) = m_fac(i,j) + dm(i)*r2(i)
        enddo
     enddo
  enddo

 end subroutine compute_aam

 subroutine fill2D(is, ie, js, je, ng, km, q, delp, area, domain, nested, npx, npy)
! This is a diffusive type filling algorithm
 type(domain2d), intent(INOUT) :: domain
 integer, intent(in):: is, ie, js, je, ng, km, npx, npy
 logical, intent(IN):: nested
 real, intent(in):: area(is-ng:ie+ng, js-ng:je+ng)
 real, intent(in):: delp(is-ng:ie+ng, js-ng:je+ng, km)
 real, intent(inout):: q(is-ng:ie+ng, js-ng:je+ng, km)
!
 real, dimension(is-ng:ie+ng, js-ng:je+ng,km):: qt
 real, dimension(is:ie+1, js:je):: fx
 real, dimension(is:ie, js:je+1):: fy
 real, parameter:: dif = 0.25
 integer:: i, j, k
 integer :: is1, ie1, js1, je1

 if (nested) then
    if (is == 1) then
       is1 = is-1
    else
       is1 = is
    endif
    if (ie == npx-1) then
       ie1 = ie+1
    else
       ie1 = ie
    endif
    if (js == 1) then
       js1 = js-1
    else
       js1 = js
    endif
    if (je == npy-1) then
       je1 = je+1
    else
       je1 = je
    endif
 else
    is1 = is
    ie1 = ie
    js1 = js
    je1 = je
 endif

!$OMP parallel do default(shared)
 do k=1, km
    do j=js1,je1
       do i=is1,ie1
          qt(i,j,k) = q(i,j,k)*delp(i,j,k)*area(i,j)
       enddo
    enddo
 enddo
 call mpp_update_domains(qt, domain, whalo=1, ehalo=1, shalo=1, nhalo=1)

!$OMP parallel do default(shared) private(fx,fy)
 do k=1, km
    fx(:,:) = 0.
    do j=js,je
       do i=is,ie+1
          if( qt(i-1,j,k)*qt(i,j,k)<0. ) fx(i,j) = qt(i-1,j,k) - qt(i,j,k)
       enddo
    enddo
    fy(:,:) = 0.
    do j=js,je+1
       do i=is,ie
          if( qt(i,j-1,k)*qt(i,j,k)<0. ) fy(i,j) = qt(i,j-1,k) - qt(i,j,k)
       enddo
    enddo
    do j=js,je
       do i=is,ie
          q(i,j,k) = q(i,j,k)+dif*(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))/(delp(i,j,k)*area(i,j))
       enddo
    enddo
 enddo

 end subroutine fill2D
end module fv_dynamics_mod
