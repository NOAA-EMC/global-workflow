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
module fv_update_phys_mod

  use constants_mod,      only: kappa, rdgas, rvgas, grav, cp_air, cp_vapor, pi=>pi_8, radius, R_GRID
  use field_manager_mod,  only: MODEL_ATMOS
  use mpp_domains_mod,    only: mpp_update_domains, domain2d
  use mpp_parameter_mod,  only: AGRID_PARAM=>AGRID
  use mpp_mod,            only: FATAL, mpp_error
  use mpp_mod,            only: mpp_error, NOTE, WARNING
  use time_manager_mod,   only: time_type
  use tracer_manager_mod, only: get_tracer_index, adjust_mass
  use fv_mp_mod,          only: start_group_halo_update, complete_group_halo_update
  use fv_mp_mod,          only: group_halo_update_type
  use fv_arrays_mod,      only: fv_flags_type, fv_nest_type
  use boundary_mod,       only: nested_grid_BC
  use boundary_mod,       only: extrapolation_BC
  use fv_eta_mod,         only: get_eta_level
  use fv_timing_mod,      only: timing_on, timing_off
  use fv_diagnostics_mod, only: prt_maxmin
  use fv_mapz_mod,        only: moist_cv, moist_cp
  use fv_nwp_nudge_mod,   only: fv_nwp_nudge
  use fv_arrays_mod,      only: fv_grid_type, fv_nest_type, fv_grid_bounds_type
#ifdef GFS_PHYS
  use fv_grid_utils_mod,  only: cubed_to_latlon
#endif

  implicit none

  public :: fv_update_phys, del2_phys
#ifdef ROT3
  public :: update_dwinds_phys
#endif

!---- version number -----
  character(len=128) :: version = '$Id$'
  character(len=128) :: tagname = '$Name$'
  real,parameter:: con_cp  = cp_air

  contains

  subroutine fv_update_phys ( dt, is, ie, js, je, isd, ied, jsd, jed, ng, nq,     &
                              u, v, w, delp, pt, q, qdiag, ua, va, ps, pe,  peln, pk, pkz,  &
                              ak, bk, phis, u_srf, v_srf, ts, delz, hydrostatic,  &
                              u_dt, v_dt, t_dt, moist_phys, Time, nudge,    &
                              gridstruct, lona, lata, npx, npy, npz, flagstruct,  &
                              neststruct, bd, domain, ptop, q_dt)
    real, intent(in)   :: dt, ptop
    integer, intent(in):: is,  ie,  js,  je, ng
    integer, intent(in):: isd, ied, jsd, jed
    integer, intent(in):: nq            ! tracers modified by physics 
                                        ! ncnst is the total nmber of tracers
    logical, intent(in):: moist_phys
    logical, intent(in):: hydrostatic
    logical, intent(in):: nudge

    type (time_type), intent(in) :: Time

    real, intent(in), dimension(npz+1):: ak, bk
    real, intent(in) :: phis(isd:ied,jsd:jed)
    real, intent(inout):: delz(isd:,jsd:,1:)

! optional arguments for atmospheric nudging
    real, intent(in), dimension(isd:ied,jsd:jed), optional ::   &
                                lona, lata   ! A-grid (physics) lon and lat

! Winds on lat-lon grid:
    real, intent(inout), dimension(isd:ied,jsd:jed,npz):: ua, va
    real, intent(inout), dimension(isd:   ,jsd:   ,1: ):: w

! Tendencies from Physics:
    real, intent(inout), dimension(isd:ied,jsd:jed,npz):: u_dt, v_dt
    real, intent(inout):: t_dt(is:ie,js:je,npz)
    real, intent(inout), optional :: q_dt(is:ie,js:je,npz,nq)

! Saved Bottom winds for GFDL Physics Interface
    real, intent(out), dimension(is:ie,js:je):: u_srf, v_srf, ts

    type(fv_flags_type) :: flagstruct
    type(fv_grid_bounds_type), intent(IN)  :: bd
    type(domain2d), intent(INOUT) :: domain

    real, intent(inout):: u(isd:ied  ,jsd:jed+1,npz)  ! D grid zonal wind (m/s)
    real, intent(inout):: v(isd:ied+1,jsd:jed  ,npz)  ! D grid meridional wind (m/s)
    real, intent(inout), dimension(isd:ied,jsd:jed,npz):: pt, delp
    real, intent(inout):: q(isd:ied,jsd:jed,npz,nq)   ! specific humidity and constituents
    real, intent(inout):: qdiag(isd:ied,jsd:jed,npz,nq+1:flagstruct%ncnst) ! diagnostic tracers

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:    
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real, intent(inout):: ps  (isd:ied  ,jsd:jed)           ! Surface pressure (pascal)
    real, intent(inout):: pe  (is-1:ie+1, npz+1,js-1:je+1)  ! edge pressure (pascal)
    real, intent(inout):: pk  (is:ie,js:je  , npz+1)        ! pe**cappa
    real, intent(inout):: peln(is:ie,npz+1,js:je)           ! ln(pe)
    real, intent(inout):: pkz (is:ie,js:je,npz)             ! finite-volume mean pk
    real, parameter:: tice = 273.16

    type(fv_grid_type) :: gridstruct
    type(fv_nest_type) :: neststruct

    integer, intent(IN) :: npx, npy, npz

!***********
! Haloe Data
!***********
    real, parameter::    q1_h2o = 2.2E-6
    real, parameter::    q7_h2o = 3.8E-6
    real, parameter::  q100_h2o = 3.8E-6
    real, parameter:: q1000_h2o = 3.1E-6
    real, parameter:: q2000_h2o = 2.8E-6
    real, parameter:: q3000_h2o = 3.0E-6

! Local arrays:
    real  ps_dt(is:ie,js:je)
    real  cvm(is:ie), qc(is:ie)
    real  phalf(npz+1), pfull(npz)

    type(group_halo_update_type), save :: i_pack(2)
    integer  i, j, k, m, n, nwat
    integer  sphum, liq_wat, ice_wat, cld_amt   ! GFDL AM physics
    integer  rainwat, snowwat, graupel          ! Lin Micro-physics
    integer  w_diff                             ! w-tracer for PBL diffusion
    real:: qstar, dbk, rdg, zvir, p_fac, cv_air, gama_dt

    real, dimension(1,1,1) :: parent_u_dt, parent_v_dt ! dummy variables for nesting

    cv_air = cp_air - rdgas ! = rdgas * (7/2-1) = 2.5*rdgas=717.68

    rdg = -rdgas / grav

    nwat = flagstruct%nwat

    if ( moist_phys .or. nwat/=0 ) then
           zvir = rvgas/rdgas - 1.
    else
           zvir = 0.
    endif

    sphum   = get_tracer_index (MODEL_ATMOS, 'sphum')
    liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
    ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
    rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
    snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
    graupel = get_tracer_index (MODEL_ATMOS, 'graupel')
    cld_amt = get_tracer_index (MODEL_ATMOS, 'cld_amt')

    if ( .not. hydrostatic ) then
        w_diff = get_tracer_index (MODEL_ATMOS, 'w_diff')
!       if ( w_diff<8 ) call mpp_error(FATAL,'W_tracer index error')
    else
        w_diff = 0
    endif

    if ( .not. hydrostatic .and. .not. flagstruct%phys_hydrostatic .and. nwat == 0 ) then
       gama_dt = dt*cp_air/cv_air
    else
       gama_dt = -1.e24
    endif

    if ( flagstruct%fv_debug ) then
       call prt_maxmin('delp_b_update', delp, is, ie, js,  je, ng, npz, 0.01)
       if (present(q_dt)) then
       do m=1,nq
          call prt_maxmin('q_dt', q_dt(is,js,1,m), is, ie, js, je, 0, npz, 1.)
       enddo
       endif
       call prt_maxmin('u_dt', u_dt, is, ie, js,  je, ng, npz, 1.)
       call prt_maxmin('v_dt', v_dt, is, ie, js,  je, ng, npz, 1.)
       call prt_maxmin('T_dt', t_dt, is, ie, js,  je, 0, npz, 1.)
    endif

    call get_eta_level(npz, 1.0E5, pfull, phalf, ak, bk)

!$OMP parallel do default(none) shared(is,ie,js,je,npz,flagstruct,pfull,q_dt,sphum,q,qdiag, &
!$OMP                                  nq,w_diff,dt,nwat,liq_wat,rainwat,ice_wat,snowwat,   &
!$OMP                                  graupel,delp,cld_amt,hydrostatic,pt,t_dt,delz,       &
!$OMP                                  gama_dt,cv_air,ua,u_dt,va,v_dt,isd,ied,jsd,jed)      &
!$OMP                          private(cvm, qc, qstar, ps_dt, p_fac)
    do k=1, npz

       if (present(q_dt)) then

       if (flagstruct%tau_h2o<0.0 .and. pfull(k) < 100.E2 ) then
! Wipe the stratosphere clean:
! This should only be used for initialization from a bad model state
           p_fac = -flagstruct%tau_h2o*86400.
           do j=js,je
              do i=is,ie
                 q_dt(i,j,k,sphum) = q_dt(i,j,k,sphum) + (3.E-6-q(i,j,k,sphum))/p_fac
              enddo
           enddo
       elseif ( flagstruct%tau_h2o>0.0 .and. pfull(k) < 3000. ) then
! Do idealized Ch4 chemistry

           if ( pfull(k) < 1. ) then
               qstar = q1_h2o
               p_fac = 0.2 * flagstruct%tau_h2o*86400.
           elseif ( pfull(k) <   7. .and. pfull(k) >=    1. ) then
               qstar = q1_h2o + (q7_h2o-q1_h2o)*log(pfull(k)/1.)/log(7.)
               p_fac = 0.3 * flagstruct%tau_h2o*86400.
           elseif ( pfull(k) <  100. .and. pfull(k) >=    7. ) then
               qstar = q7_h2o + (q100_h2o-q7_h2o)*log(pfull(k)/7.)/log(100./7.)
               p_fac = 0.4 * flagstruct%tau_h2o*86400.
           elseif ( pfull(k) < 1000. .and. pfull(k) >=  100. ) then
               qstar = q100_h2o + (q1000_h2o-q100_h2o)*log(pfull(k)/1.E2)/log(10.)
               p_fac = 0.5 * flagstruct%tau_h2o*86400.
           elseif ( pfull(k) < 2000. .and. pfull(k) >= 1000. ) then
               qstar = q1000_h2o + (q2000_h2o-q1000_h2o)*log(pfull(k)/1.E3)/log(2.)
               p_fac = 0.75 * flagstruct%tau_h2o*86400.
           else
               qstar = q3000_h2o
               p_fac = flagstruct%tau_h2o*86400.
           endif

           do j=js,je
              do i=is,ie
                 q_dt(i,j,k,sphum) = q_dt(i,j,k,sphum) + (qstar-q(i,j,k,sphum))/p_fac
              enddo
           enddo
       endif

!----------------
! Update tracers:
!----------------
       do m=1,nq
          if( m /= w_diff ) then 
          do j=js,je
             do i=is,ie
                q(i,j,k,m) = q(i,j,k,m) + dt*q_dt(i,j,k,m)
             enddo
          enddo
          endif
       enddo

!--------------------------------------------------------
! Adjust total air mass due to changes in water substance
!--------------------------------------------------------

      if ( nwat==6 ) then
! micro-physics with 6 water substances
        do j=js,je
           do i=is,ie
              ps_dt(i,j)  = 1. + dt * ( q_dt(i,j,k,sphum  ) +    &
                                        q_dt(i,j,k,liq_wat) +    &
                                        q_dt(i,j,k,rainwat) +    &
                                        q_dt(i,j,k,ice_wat) +    &
                                        q_dt(i,j,k,snowwat) +    &
                                        q_dt(i,j,k,graupel) )
              delp(i,j,k) = delp(i,j,k) * ps_dt(i,j)
           enddo
        enddo
      elseif ( nwat==4 ) then
! micro-physics with fake ice
        do j=js,je
           do i=is,ie
              ps_dt(i,j)  = 1. + dt * ( q_dt(i,j,k,sphum  ) +    &
                                        q_dt(i,j,k,liq_wat) +    &
                                        q_dt(i,j,k,rainwat) )
              delp(i,j,k) = delp(i,j,k) * ps_dt(i,j)
           enddo
        enddo
      elseif( nwat==3 ) then
! GFDL AM2/3 phys (cloud water + cloud ice)
        do j=js,je
           do i=is,ie
               ps_dt(i,j) = 1. + dt*(q_dt(i,j,k,sphum  ) +    &
                                     q_dt(i,j,k,liq_wat) +    &
                                     q_dt(i,j,k,ice_wat) )
              delp(i,j,k) = delp(i,j,k) * ps_dt(i,j)
           enddo
        enddo
      elseif( nwat==2 ) then
! NGGPS-GFS: the total condensate is "liq_wat"
        do j=js,je
           do i=is,ie
               ps_dt(i,j) = 1. + dt*(q_dt(i,j,k,sphum) + q_dt(i,j,k,liq_wat))
              delp(i,j,k) = delp(i,j,k) * ps_dt(i,j)
           enddo
        enddo
      elseif ( nwat>0 ) then
        do j=js,je
           do i=is,ie
              ps_dt(i,j)  = 1. + dt*sum(q_dt(i,j,k,1:nwat))
              delp(i,j,k) = delp(i,j,k) * ps_dt(i,j)
           enddo
        enddo
      endif

!-----------------------------------------
! Adjust mass mixing ratio of all tracers 
!-----------------------------------------
      if ( nwat /=0 ) then
        do m=1,flagstruct%ncnst
!-- check to query field_table to determine if tracer needs mass adjustment
          if( m /= cld_amt .and. m /= w_diff .and. adjust_mass(MODEL_ATMOS,m)) then 
            if (m <= nq)  then
              q(is:ie,js:je,k,m) = q(is:ie,js:je,k,m) / ps_dt(is:ie,js:je)
            else
              qdiag(is:ie,js:je,k,m) = qdiag(is:ie,js:je,k,m) / ps_dt(is:ie,js:je)
            endif
          endif
        enddo
      endif

      endif ! present(q_dt)

      if ( hydrostatic ) then
         do j=js,je
            call moist_cp(is,ie,isd,ied,jsd,jed, npz, j, k, nwat, sphum, liq_wat, rainwat,    &
                          ice_wat, snowwat, graupel, q, qc, cvm, pt(is:ie,j,k) )
            do i=is,ie
!!!            pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*dt
               pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*dt*con_cp/cvm(i)
            enddo
          enddo
       else
         if ( flagstruct%phys_hydrostatic ) then
! Constant pressure
             do j=js,je
                call moist_cp(is,ie,isd,ied,jsd,jed, npz, j, k, nwat, sphum, liq_wat, rainwat,    &
                              ice_wat, snowwat, graupel, q, qc, cvm, pt(is:ie,j,k) )
                do i=is,ie
                   delz(i,j,k) = delz(i,j,k) / pt(i,j,k)
!!!                pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*dt
                   pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*dt*con_cp/cvm(i)
                   delz(i,j,k) = delz(i,j,k) * pt(i,j,k)
                enddo
             enddo
         else
            !NOTE: only works for either no physics or Lin MP
            if (nwat == 0) then
               do j=js,je
                  do i=is,ie
                     pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*gama_dt
                  enddo
               enddo
            else
               do j=js,je
                  call moist_cv(is,ie,isd,ied,jsd,jed, npz, j, k, nwat, sphum, liq_wat, rainwat,    &
                                ice_wat, snowwat, graupel, q, qc, cvm, pt(is:ie,j,k))
                  do i=is,ie
!!!                  pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*dt*con_cp/cv_air
                     pt(i,j,k) = pt(i,j,k) + t_dt(i,j,k)*dt*con_cp/cvm(i)
                  enddo
               enddo
            endif
         endif
      endif

#ifndef GFS_PHYS
      do j=js,je
         do i=is,ie
            ua(i,j,k) = ua(i,j,k) + dt*u_dt(i,j,k)
            va(i,j,k) = va(i,j,k) + dt*v_dt(i,j,k)
         enddo
      enddo
#endif

   enddo ! k-loop

! [delp, (ua, va), pt, q] updated. Perform nudging if requested
!------- nudging of atmospheric variables toward specified data --------

    ps_dt(:,:) = 0.

    if ( nudge ) then
! All fields will be updated except winds; wind tendencies added
!$OMP parallel do default(none) shared(is,ie,js,je,npz,pe,delp,ps)
        do j=js,je
         do k=2,npz+1                                                                             
          do i=is,ie
            pe(i,k,j) = pe(i,k-1,j) + delp(i,j,k-1)
          enddo
         enddo
         do i=is,ie
           ps(i,j) = pe(i,npz+1,j)
         enddo
        enddo
  endif         ! end nudging       

  if ( .not.flagstruct%dwind_2d ) then

                                                            call timing_on('COMM_TOTAL')
      if ( gridstruct%square_domain ) then                  
           call start_group_halo_update(i_pack(1), u_dt, domain, whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.false.)
           call start_group_halo_update(i_pack(1), v_dt, domain, whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
      else
           call start_group_halo_update(i_pack(1), u_dt, domain, complete=.false.)
           call start_group_halo_update(i_pack(1), v_dt, domain, complete=.true.)
      endif                
                                                           call timing_off('COMM_TOTAL')
  endif

!----------------------------------------
! Update pe, peln, pkz, and surface winds
!----------------------------------------
  if ( flagstruct%fv_debug ) then
       call prt_maxmin('PS_b_update',     ps, is, ie, js,  je, ng,   1, 0.01)
       call prt_maxmin('delp_a_update', delp, is, ie, js,  je, ng, npz, 0.01)
  endif

!$OMP parallel do default(none) shared(is,ie,js,je,npz,pe,delp,peln,pk,ps,u_srf,v_srf, &
!$OMP                                  ua,va,pkz,hydrostatic)
   do j=js,je
      do k=2,npz+1                                                                             
         do i=is,ie
              pe(i,k,j) = pe(i,k-1,j) + delp(i,j,k-1)
            peln(i,k,j) = log( pe(i,k,j) )
              pk(i,j,k) = exp( kappa*peln(i,k,j) )
         enddo
      enddo

      do i=is,ie
            ps(i,j) = pe(i,npz+1,j)
         u_srf(i,j) = ua(i,j,npz)
         v_srf(i,j) = va(i,j,npz)
      enddo

      if ( hydrostatic ) then
         do k=1,npz
            do i=is,ie
               pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
            enddo
         enddo
      endif
   enddo      ! j-loop

                                                    call timing_on(' Update_dwinds')
  if ( flagstruct%dwind_2d ) then
    call update2d_dwinds_phys(is, ie, js, je, isd, ied, jsd, jed, dt, u_dt, v_dt, u, v, gridstruct, &
         npx,npy,npz,domain)
  else

     !I have not seen dwind_2d be used for anything; so we will only handle nesting assuming dwind_2d == .false.

    call timing_on('COMM_TOTAL')

    call complete_group_halo_update(i_pack(1), domain)

    if (size(neststruct%child_grids) > 1) then
       if (gridstruct%nested) then
          call nested_grid_BC(u_dt, parent_u_dt, neststruct%nest_domain, neststruct%ind_h, neststruct%wt_h, 0, 0, &
               npx, npy, npz, bd, 1, npx-1, 1, npy-1)
          call nested_grid_BC(v_dt, parent_v_dt, neststruct%nest_domain, neststruct%ind_h, neststruct%wt_h, 0, 0, &
               npx, npy, npz, bd, 1, npx-1, 1, npy-1)
       endif
       do n=1,size(neststruct%child_grids)
          if (neststruct%child_grids(n)) then
             call nested_grid_BC(u_dt, neststruct%nest_domain_all(n), 0, 0)
             call nested_grid_BC(v_dt, neststruct%nest_domain_all(n), 0, 0)
          endif
       enddo
    endif

    call timing_off('COMM_TOTAL')
    call update_dwinds_phys(is, ie, js, je, isd, ied, jsd, jed, dt, u_dt, v_dt, u, v, gridstruct, npx, npy, npz, domain)
 endif
                                                    call timing_off(' Update_dwinds')
#ifdef GFS_PHYS
    call cubed_to_latlon(u, v, ua, va, gridstruct, &
         npx, npy, npz, 1, gridstruct%grid_type, domain, gridstruct%nested, flagstruct%c2l_ord, bd)
#endif

  if ( flagstruct%fv_debug ) then
       call prt_maxmin('PS_a_update', ps, is, ie, js, je, ng,   1, 0.01)
  endif

  end subroutine fv_update_phys


  subroutine del2_phys(qdt, delp, gridstruct, cd, npx, npy, km, is, ie, js, je, &
                       isd, ied, jsd, jed, ngc, domain)
! This routine is for filtering the physics tendency
   integer, intent(in):: npx, npy, km
   integer, intent(in):: is, ie, js, je, isd, ied, jsd, jed, ngc
   real,    intent(in):: cd            ! cd = K * da_min;   0 < K < 0.25
   real, intent(in   ):: delp(isd:ied,jsd:jed,km)
   real, intent(inout):: qdt(is-ngc:ie+ngc,js-ngc:je+ngc,km)
   type(fv_grid_type), intent(IN), target :: gridstruct
   type(domain2d), intent(INOUT) :: domain
   
   real, pointer, dimension(:,:) :: rarea, dx, dy, sina_u, sina_v, rdxc, rdyc
   real, pointer, dimension(:,:,:) :: sin_sg
!
   real :: q(isd:ied,jsd:jed,km)
   real :: fx(is:ie+1,js:je), fy(is:ie,js:je+1)
   real :: mask(is:ie+1,js:je+1)
   real :: f1(is:ie+1), f2(js:je+1)
   real :: damp
   integer i,j,k

   rarea  => gridstruct%rarea
   dx     => gridstruct%dx
   dy     => gridstruct%dy
   sina_u => gridstruct%sina_u
   sina_v => gridstruct%sina_v
   rdxc   => gridstruct%rdxc
   rdyc   => gridstruct%rdyc
   sin_sg => gridstruct%sin_sg

! Applying mask to cd, the damping coefficient?
   damp = 0.25 * cd * gridstruct%da_min

! Mask defined at corners

!$OMP parallel do default(none) shared(is,ie,f1,npx)
   do i=is,ie+1
      f1(i) = (1. - sin(real(i-1)/real(npx-1)*pi))**2
   enddo

!$OMP parallel do default(none) shared(is,ie,js,je,f1,f2,npy,mask,damp)
   do j=js,je+1
      f2(j) = (1. - sin(real(j-1)/real(npy-1)*pi))**2
      do i=is,ie+1
         mask(i,j) = damp * (f1(i) + f2(j))
      enddo
   enddo

! mass weighted tendency from physics is filtered

!$OMP parallel do default(none) shared(is,ie,js,je,km,q,qdt,delp)
   do k=1,km
      do j=js,je
         do i=is,ie
            q(i,j,k) = qdt(i,j,k)*delp(i,j,k)
         enddo
      enddo
   enddo
                     call timing_on('COMM_TOTAL')
   call mpp_update_domains(q, domain, complete=.true.)
                     call timing_off('COMM_TOTAL')

!$OMP parallel do default(none) shared(is,ie,js,je,km,mask,dy,sina_u,q,rdxc,gridstruct, &
!$OMP                                  sin_sg,npx,dx,npy,rdyc,sina_v,qdt,rarea,delp)    &
!$OMP                          private(fx, fy)
   do k=1,km
      do j=js,je
         do i=is,ie+1
            fx(i,j) = &
                 (mask(i,j)+mask(i,j+1))*dy(i,j)*sina_u(i,j)* &
                 (q(i-1,j,k)-q(i,j,k))*rdxc(i,j)
         enddo
         if (is == 1 .and. .not. gridstruct%nested)   fx(i,j) = &
              (mask(is,j)+mask(is,j+1))*dy(is,j)*(q(is-1,j,k)-q(is,j,k))*rdxc(is,j)* &
            0.5*(sin_sg(1,j,1) + sin_sg(0,j,3))
         if (ie+1==npx .and. .not. gridstruct%nested) fx(i,j) = &
              (mask(ie+1,j)+mask(ie+1,j+1))*dy(ie+1,j)*(q(ie,j,k)-q(ie+1,j,k))*rdxc(ie+1,j)* & 
            0.5*(sin_sg(npx,j,1) + sin_sg(npx-1,j,3))
      enddo
      do j=js,je+1
         if ((j == 1 .OR. j == npy) .and. .not. gridstruct%nested) then
            do i=is,ie
               fy(i,j) = (mask(i,j)+mask(i+1,j))*dx(i,j)*&
                    (q(i,j-1,k)-q(i,j,k))*rdyc(i,j) &
                    *0.5*(sin_sg(i,j,2) + sin_sg(i,j-1,4) )
            enddo
         else
            do i=is,ie
               fy(i,j) = (mask(i,j)+mask(i+1,j))*dx(i,j)*sina_v(i,j)*&
                    (q(i,j-1,k)-q(i,j,k))*rdyc(i,j)
            enddo
         end if
      enddo
      do j=js,je
         do i=is,ie
            qdt(i,j,k) = qdt(i,j,k) + rarea(i,j)*(fx(i,j)-fx(i+1,j)+fy(i,j)-fy(i,j+1))/delp(i,j,k)
         enddo
      enddo
   enddo

  end subroutine del2_phys


  subroutine update_dwinds_phys(is, ie, js, je, isd, ied, jsd, jed, dt, u_dt, v_dt, u, v, gridstruct, npx, npy, npz, domain)

! Purpose; Transform wind tendencies on A grid to D grid for the final update
 
  integer, intent(in):: is,  ie,  js,  je
  integer, intent(in):: isd, ied, jsd, jed
  integer, intent(IN) :: npx,npy, npz
  real,    intent(in):: dt
  real, intent(inout):: u(isd:ied,  jsd:jed+1,npz)
  real, intent(inout):: v(isd:ied+1,jsd:jed  ,npz)
  real, intent(inout), dimension(isd:ied,jsd:jed,npz):: u_dt, v_dt
  type(fv_grid_type), intent(IN), target :: gridstruct
  type(domain2d), intent(INOUT) :: domain

! local:
  real v3(is-1:ie+1,js-1:je+1,3)
  real ue(is-1:ie+1,js:je+1,3)    ! 3D winds at edges
  real ve(is:ie+1,js-1:je+1,  3)    ! 3D winds at edges
  real, dimension(is:ie):: ut1, ut2, ut3
  real, dimension(js:je):: vt1, vt2, vt3
  real dt5, gratio
  integer i, j, k, m, im2, jm2

  real(kind=R_GRID), pointer, dimension(:,:,:) :: vlon, vlat
  real(kind=R_GRID), pointer, dimension(:,:,:,:) :: es, ew
  real(kind=R_GRID), pointer, dimension(:) :: edge_vect_w, edge_vect_e, edge_vect_s, edge_vect_n

  es   => gridstruct%es
  ew   => gridstruct%ew
  vlon => gridstruct%vlon
  vlat => gridstruct%vlat

  edge_vect_w => gridstruct%edge_vect_w
  edge_vect_e => gridstruct%edge_vect_e
  edge_vect_s => gridstruct%edge_vect_s
  edge_vect_n => gridstruct%edge_vect_n

    dt5 = 0.5 * dt
    im2 = (npx-1)/2
    jm2 = (npy-1)/2

!$OMP parallel do default(none) shared(is,ie,js,je,npz,gridstruct,u,dt5,u_dt,v,v_dt,  &
!$OMP                                  vlon,vlat,jm2,edge_vect_w,npx,edge_vect_e,im2, &
!$OMP                                  edge_vect_s,npy,edge_vect_n,es,ew)             &
!$OMP                          private(ut1, ut2, ut3, vt1, vt2, vt3, ue, ve, v3)
    do k=1, npz

     if ( gridstruct%grid_type > 3 ) then    ! Local & one tile configurations

       do j=js,je+1
          do i=is,ie
             u(i,j,k) = u(i,j,k) + dt5*(u_dt(i,j-1,k) + u_dt(i,j,k))
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             v(i,j,k) = v(i,j,k) + dt5*(v_dt(i-1,j,k) + v_dt(i,j,k))
          enddo
       enddo

     else
! Compute 3D wind tendency on A grid
       do j=js-1,je+1
          do i=is-1,ie+1
             v3(i,j,1) = u_dt(i,j,k)*vlon(i,j,1) + v_dt(i,j,k)*vlat(i,j,1)
             v3(i,j,2) = u_dt(i,j,k)*vlon(i,j,2) + v_dt(i,j,k)*vlat(i,j,2)
             v3(i,j,3) = u_dt(i,j,k)*vlon(i,j,3) + v_dt(i,j,k)*vlat(i,j,3)
          enddo
       enddo

! Interpolate to cell edges
       do j=js,je+1
          do i=is-1,ie+1
             ue(i,j,1) = v3(i,j-1,1) + v3(i,j,1)
             ue(i,j,2) = v3(i,j-1,2) + v3(i,j,2)
             ue(i,j,3) = v3(i,j-1,3) + v3(i,j,3)
          enddo
       enddo

       do j=js-1,je+1
          do i=is,ie+1
             ve(i,j,1) = v3(i-1,j,1) + v3(i,j,1)
             ve(i,j,2) = v3(i-1,j,2) + v3(i,j,2)
             ve(i,j,3) = v3(i-1,j,3) + v3(i,j,3)
          enddo
       enddo

! --- E_W edges (for v-wind):
     if ( is==1 .and. .not. gridstruct%nested ) then
       i = 1
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_w(j)*ve(i,j-1,1)+(1.-edge_vect_w(j))*ve(i,j,1)
             vt2(j) = edge_vect_w(j)*ve(i,j-1,2)+(1.-edge_vect_w(j))*ve(i,j,2)
             vt3(j) = edge_vect_w(j)*ve(i,j-1,3)+(1.-edge_vect_w(j))*ve(i,j,3)
        else
             vt1(j) = edge_vect_w(j)*ve(i,j+1,1)+(1.-edge_vect_w(j))*ve(i,j,1)
             vt2(j) = edge_vect_w(j)*ve(i,j+1,2)+(1.-edge_vect_w(j))*ve(i,j,2)
             vt3(j) = edge_vect_w(j)*ve(i,j+1,3)+(1.-edge_vect_w(j))*ve(i,j,3)
        endif
       enddo
       do j=js,je
          ve(i,j,1) = vt1(j)
          ve(i,j,2) = vt2(j)
          ve(i,j,3) = vt3(j)
       enddo
     endif
     if ( (ie+1)==npx .and. .not. gridstruct%nested ) then
       i = npx
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_e(j)*ve(i,j-1,1)+(1.-edge_vect_e(j))*ve(i,j,1)
             vt2(j) = edge_vect_e(j)*ve(i,j-1,2)+(1.-edge_vect_e(j))*ve(i,j,2)
             vt3(j) = edge_vect_e(j)*ve(i,j-1,3)+(1.-edge_vect_e(j))*ve(i,j,3)
        else
             vt1(j) = edge_vect_e(j)*ve(i,j+1,1)+(1.-edge_vect_e(j))*ve(i,j,1)
             vt2(j) = edge_vect_e(j)*ve(i,j+1,2)+(1.-edge_vect_e(j))*ve(i,j,2)
             vt3(j) = edge_vect_e(j)*ve(i,j+1,3)+(1.-edge_vect_e(j))*ve(i,j,3)
        endif
       enddo
       do j=js,je
          ve(i,j,1) = vt1(j)
          ve(i,j,2) = vt2(j)
          ve(i,j,3) = vt3(j)
       enddo
     endif
! N-S edges (for u-wind):
     if ( js==1  .and. .not. gridstruct%nested) then
       j = 1
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_s(i)*ue(i-1,j,1)+(1.-edge_vect_s(i))*ue(i,j,1)
             ut2(i) = edge_vect_s(i)*ue(i-1,j,2)+(1.-edge_vect_s(i))*ue(i,j,2)
             ut3(i) = edge_vect_s(i)*ue(i-1,j,3)+(1.-edge_vect_s(i))*ue(i,j,3)
        else
             ut1(i) = edge_vect_s(i)*ue(i+1,j,1)+(1.-edge_vect_s(i))*ue(i,j,1)
             ut2(i) = edge_vect_s(i)*ue(i+1,j,2)+(1.-edge_vect_s(i))*ue(i,j,2)
             ut3(i) = edge_vect_s(i)*ue(i+1,j,3)+(1.-edge_vect_s(i))*ue(i,j,3)
        endif
       enddo
       do i=is,ie
          ue(i,j,1) = ut1(i)
          ue(i,j,2) = ut2(i)
          ue(i,j,3) = ut3(i)
       enddo
     endif
     if ( (je+1)==npy  .and. .not. gridstruct%nested) then
       j = npy
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_n(i)*ue(i-1,j,1)+(1.-edge_vect_n(i))*ue(i,j,1)
             ut2(i) = edge_vect_n(i)*ue(i-1,j,2)+(1.-edge_vect_n(i))*ue(i,j,2)
             ut3(i) = edge_vect_n(i)*ue(i-1,j,3)+(1.-edge_vect_n(i))*ue(i,j,3)
        else
             ut1(i) = edge_vect_n(i)*ue(i+1,j,1)+(1.-edge_vect_n(i))*ue(i,j,1)
             ut2(i) = edge_vect_n(i)*ue(i+1,j,2)+(1.-edge_vect_n(i))*ue(i,j,2)
             ut3(i) = edge_vect_n(i)*ue(i+1,j,3)+(1.-edge_vect_n(i))*ue(i,j,3)
        endif
       enddo
       do i=is,ie
          ue(i,j,1) = ut1(i)
          ue(i,j,2) = ut2(i)
          ue(i,j,3) = ut3(i)
       enddo
     endif
       do j=js,je+1
          do i=is,ie
             u(i,j,k) = u(i,j,k) + dt5*( ue(i,j,1)*es(1,i,j,1) +  &
                                         ue(i,j,2)*es(2,i,j,1) +  &
                                         ue(i,j,3)*es(3,i,j,1) )
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             v(i,j,k) = v(i,j,k) + dt5*( ve(i,j,1)*ew(1,i,j,2) +  &
                                         ve(i,j,2)*ew(2,i,j,2) +  &
                                         ve(i,j,3)*ew(3,i,j,2) )
          enddo
       enddo
! Update:
      endif   ! end grid_type
 
    enddo         ! k-loop

  end subroutine update_dwinds_phys 


  subroutine update2d_dwinds_phys(is, ie, js, je, isd, ied, jsd, jed, dt, u_dt, v_dt, u, v, gridstruct, npx, npy, npz, domain)

! Purpose; Transform wind tendencies on A grid to D grid for the final update

  integer, intent(in):: is,  ie,  js,  je
  integer, intent(in):: isd, ied, jsd, jed
  real,    intent(in):: dt
  real, intent(inout):: u(isd:ied,  jsd:jed+1,npz)
  real, intent(inout):: v(isd:ied+1,jsd:jed  ,npz)
  real, intent(inout), dimension(isd:ied,jsd:jed,npz):: u_dt, v_dt
  type(fv_grid_type), intent(IN), target :: gridstruct
  integer, intent(IN) :: npx,npy, npz
  type(domain2d), intent(INOUT) :: domain

! local:
  real ut(isd:ied,jsd:jed)
  real:: dt5, gratio
  integer i, j, k

  real(kind=R_GRID), pointer, dimension(:,:,:) :: vlon, vlat
  real(kind=R_GRID), pointer, dimension(:,:,:,:) :: es, ew
  real(kind=R_GRID), pointer, dimension(:) :: edge_vect_w, edge_vect_e, edge_vect_s, edge_vect_n
  real, pointer, dimension(:,:) :: z11, z12, z21, z22, dya, dxa

  es   => gridstruct%es
  ew   => gridstruct%ew
  vlon => gridstruct%vlon
  vlat => gridstruct%vlat

  edge_vect_w => gridstruct%edge_vect_w
  edge_vect_e => gridstruct%edge_vect_e
  edge_vect_s => gridstruct%edge_vect_s
  edge_vect_n => gridstruct%edge_vect_n

  z11 => gridstruct%z11
  z21 => gridstruct%z21
  z12 => gridstruct%z12
  z22 => gridstruct%z22

  dxa => gridstruct%dxa
  dya => gridstruct%dya

! Transform wind tendency on A grid to local "co-variant" components:

!$OMP parallel do default(none) shared(is,ie,js,je,npz,z11,u_dt,z12,v_dt,z21,z22) &
!$OMP                          private(ut)
    do k=1,npz
       do j=js,je
          do i=is,ie
                 ut(i,j) = z11(i,j)*u_dt(i,j,k) + z12(i,j)*v_dt(i,j,k)
             v_dt(i,j,k) = z21(i,j)*u_dt(i,j,k) + z22(i,j)*v_dt(i,j,k)
             u_dt(i,j,k) = ut(i,j)
          enddo
       enddo
    enddo
! (u_dt,v_dt) are now on local coordinate system
       call timing_on('COMM_TOTAL')
  call mpp_update_domains(u_dt, v_dt, domain, gridtype=AGRID_PARAM)
       call timing_off('COMM_TOTAL')

    dt5 = 0.5 * dt

!$OMP parallel do default(none) shared(is,ie,js,je,npz,gridstruct,u,dt5,u_dt,v,v_dt, &
!$OMP                                  dya,npy,dxa,npx)                              &
!$OMP                          private(gratio)
    do k=1, npz

     if ( gridstruct%grid_type > 3 .or. gridstruct%nested) then    ! Local & one tile configurations

       do j=js,je+1
          do i=is,ie
             u(i,j,k) = u(i,j,k) + dt5*(u_dt(i,j-1,k) + u_dt(i,j,k))
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             v(i,j,k) = v(i,j,k) + dt5*(v_dt(i-1,j,k) + v_dt(i,j,k))
          enddo
       enddo

     else

!--------
! u-wind
!--------
! Edges:
    if ( js==1 ) then
       do i=is,ie
          gratio = dya(i,2) / dya(i,1)
          u(i,1,k) = u(i,1,k) + dt5*((2.+gratio)*(u_dt(i,0,k)+u_dt(i,1,k))  &
                   -(u_dt(i,-1,k)+u_dt(i,2,k)))/(1.+gratio)
       enddo
    endif

! Interior
    do j=max(2,js),min(npy-1,je+1)
       do i=is,ie
          u(i,j,k) = u(i,j,k) + dt5*(u_dt(i,j-1,k)+u_dt(i,j,k))
       enddo
    enddo

    if ( (je+1)==npy ) then
       do i=is,ie
          gratio = dya(i,npy-2) / dya(i,npy-1)
          u(i,npy,k) = u(i,npy,k) + dt5*((2.+gratio)*(u_dt(i,npy-1,k)+u_dt(i,npy,k)) &
                     -(u_dt(i,npy-2,k)+u_dt(i,npy+1,k)))/(1.+gratio)
       enddo
    endif

!--------
! v-wind
!--------
! West Edges:
    if ( is==1 ) then
       do j=js,je
          gratio = dxa(2,j) / dxa(1,j)
          v(1,j,k) = v(1,j,k) + dt5*((2.+gratio)*(v_dt(0,j,k)+v_dt(1,j,k)) &
                   -(v_dt(-1,j,k)+v_dt(2,j,k)))/(1.+gratio)
       enddo
    endif

! Interior
    do j=js,je
       do i=max(2,is),min(npx-1,ie+1)
          v(i,j,k) = v(i,j,k) + dt5*(v_dt(i-1,j,k)+v_dt(i,j,k))
       enddo
    enddo

! East Edges:
    if ( (ie+1)==npx ) then
       do j=js,je
          gratio = dxa(npx-2,j) / dxa(npx-1,j)
          v(npx,j,k) = v(npx,j,k) + dt5*((2.+gratio)*(v_dt(npx-1,j,k)+v_dt(npx,j,k)) &
                     -(v_dt(npx-2,j,k)+v_dt(npx+1,j,k)))/(1.+gratio)
       enddo
    endif

    endif   ! end grid_type

    enddo         ! k-loop

  end subroutine update2d_dwinds_phys

end module fv_update_phys_mod
