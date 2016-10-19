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
! SJL: Apr 12, 2012
! This revision may actually produce rounding level differences due to the elimination of KS to compute
! pressure level for remapping.
module fv_mapz_mod

  use constants_mod,     only: radius, pi=>pi_8, rvgas, rdgas, grav, hlv, hlf, cp_air, cp_vapor
  use tracer_manager_mod,only: get_tracer_index
  use field_manager_mod, only: MODEL_ATMOS
  use fv_grid_utils_mod, only: g_sum, ptop_min
  use fv_fill_mod,       only: fillz
  use mpp_domains_mod,   only: mpp_update_domains, domain2d
  use mpp_mod,           only: FATAL, mpp_error, get_unit, mpp_root_pe, mpp_pe
  use fv_arrays_mod,     only: fv_grid_type
  use fv_timing_mod,     only: timing_on, timing_off
  use lin_cld_microphys_mod, only: sat_adj2
  use fv_mp_mod,         only: is_master

  implicit none
  real, parameter:: consv_min= 0.001   ! below which no correction applies
  real, parameter:: t_min= 184.   ! below which applies stricter constraint
  real, parameter:: r3 = 1./3., r23 = 2./3., r12 = 1./12.
  real, parameter:: cv_vap = 3.*rvgas  ! 1384.5
  real, parameter:: cv_air =  cp_air - rdgas ! = rdgas * (7/2-1) = 2.5*rdgas=717.68
  real, parameter:: c_ice = 2106.           ! heat capacity of ice at 0.C
  real, parameter:: c_liq = 4.1855e+3    ! GFS
!  real, parameter:: c_con = c_ice  ! Heat capacity of the GFS condensate
  real, parameter:: c_con = cv_vap  ! Heat capacity of the GFS condensate; set to cv_vap (no condensate effect) 
  real(kind=4) :: E_Flux = 0.
  private

  public compute_total_energy, Lagrangian_to_Eulerian, moist_cv, moist_cp,   &
         rst_remap, mappm, E_Flux

!---- version number -----
  character(len=128) :: version = '$Id$'
  character(len=128) :: tagname = '$Name$'

contains

 subroutine Lagrangian_to_Eulerian(last_step, consv, ps, pe, delp, pkz, pk,   &
                                   mdt, pdt, km, is,ie,js,je, isd,ied,jsd,jed,       &
                      nq, nwat, sphum, q_con, u, v, w, delz, pt, q, hs, r_vir, cp,  &
                      akap, cappa, kord_mt, kord_wz, kord_tr, kord_tm,  peln, te0_2d,        &
                      ng, ua, va, omga, te, ws, fill, reproduce_sum, out_dt, dtdt,      &
                      ptop, ak, bk, gridstruct, domain, ze0, do_sat_adj, &
                      hydrostatic, hybrid_z, do_omega, adiabatic, do_adiabatic_init)
  logical, intent(in):: last_step
  real,    intent(in):: mdt                   ! remap time step
  real,    intent(in):: pdt                   ! phys time step
  integer, intent(in):: km
  integer, intent(in):: nq                    ! number of tracers (including h2o)
  integer, intent(in):: nwat
  integer, intent(in):: sphum                 ! index for water vapor (specific humidity)
  integer, intent(in):: ng
  integer, intent(in):: is,ie,isd,ied         ! starting & ending X-Dir index
  integer, intent(in):: js,je,jsd,jed         ! starting & ending Y-Dir index
  integer, intent(in):: kord_mt               ! Mapping order for the vector winds
  integer, intent(in):: kord_wz               ! Mapping order/option for w
  integer, intent(in):: kord_tr(nq)           ! Mapping order for tracers
  integer, intent(in):: kord_tm               ! Mapping order for thermodynamics

  real, intent(in):: consv                 ! factor for TE conservation
  real, intent(in):: r_vir
  real, intent(in):: cp
  real, intent(in):: akap
  real, intent(in):: hs(isd:ied,jsd:jed)  ! surface geopotential
  real, intent(inout):: te0_2d(is:ie,js:je)
  real, intent(in):: ws(is:ie,js:je)

  logical, intent(in):: do_sat_adj
  logical, intent(in):: fill                  ! fill negative tracers
  logical, intent(in):: reproduce_sum
  logical, intent(in):: do_omega, adiabatic, do_adiabatic_init
  real, intent(in) :: ptop
  real, intent(in) :: ak(km+1)
  real, intent(in) :: bk(km+1)
  type(fv_grid_type), intent(IN), target :: gridstruct
  type(domain2d), intent(INOUT) :: domain

! !INPUT/OUTPUT
  real, intent(inout):: pk(is:ie,js:je,km+1) ! pe to the kappa
  real, intent(inout):: q(isd:ied,jsd:jed,km,*)
  real, intent(inout):: delp(isd:ied,jsd:jed,km) ! pressure thickness
  real, intent(inout)::  pe(is-1:ie+1,km+1,js-1:je+1) ! pressure at layer edges
  real, intent(inout):: ps(isd:ied,jsd:jed)      ! surface pressure
  real, intent(inout):: ze0(is:,js:,1:)    ! Specified height at edges (m)

! u-wind will be ghosted one latitude to the north upon exit
  real, intent(inout)::  u(isd:ied  ,jsd:jed+1,km)   ! u-wind (m/s)
  real, intent(inout)::  v(isd:ied+1,jsd:jed  ,km)   ! v-wind (m/s)
  real, intent(inout)::  w(isd:     ,jsd:     ,1:)   ! vertical velocity (m/s)
  real, intent(inout):: pt(isd:ied  ,jsd:jed  ,km)   ! cp*virtual potential temperature 
                                                     ! as input; output: temperature
  real, intent(inout), dimension(isd:,jsd:,1:)::delz, q_con, cappa
  logical, intent(in):: hydrostatic
  logical, intent(in):: hybrid_z
  logical, intent(in):: out_dt

  real, intent(inout)::   ua(isd:ied,jsd:jed,km)   ! u-wind (m/s) on physics grid
  real, intent(inout)::   va(isd:ied,jsd:jed,km)   ! v-wind (m/s) on physics grid
  real, intent(inout):: omga(isd:ied,jsd:jed,km)   ! vertical press. velocity (pascal/sec)
  real, intent(inout)::   peln(is:ie,km+1,js:je)     ! log(pe)
  real, intent(inout)::   dtdt(is:ie,js:je,km)
  real, intent(out)::    pkz(is:ie,js:je,km)       ! layer-mean pk for converting t to pt
  real, intent(out)::     te(isd:ied,jsd:jed,km)

! !DESCRIPTION:
!
! !REVISION HISTORY:
! SJL 03.11.04: Initial version for partial remapping
!
!-----------------------------------------------------------------------
  integer :: i,j,k 
! real q_source(is:ie,js:je,nq)    ! numerical tracer source from surface
                                   ! in case fillz is not sufficient
  real, dimension(is:ie,js:je):: te_2d, zsum0, zsum1, dpeln
  real, dimension(is:ie,km)  :: q2, dp2, deng
  real, dimension(is:ie,km+1):: ze1, ze2, pe1, pe2, pk1, pk2, pn2, phis
     real  pe0(is:ie+1,km+1)
     real  pe3(is:ie+1,km+1)
     real, dimension(is:ie):: gz, cvm, qv
     real dz1(km)
     real rcp, rg, tmp, tpe, rrg, bkh, dtmp, dlnp, ztop, z_rat
     real k1k
     integer nt, liq_wat, ice_wat, rainwat, snowwat, cld_amt, graupel, iq, n, kp, k_next

        k1k = rdgas/cv_air   ! akap / (1.-akap) = rg/Cv=0.4
         rg = rdgas
        rcp = 1./ cp
        rrg = -rdgas/grav

           liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
           ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')
           rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
           snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
           graupel = get_tracer_index (MODEL_ATMOS, 'graupel')
           cld_amt = get_tracer_index (MODEL_ATMOS, 'cld_amt')

!$OMP parallel do default(none) shared(is,ie,js,je,km,pe,ptop,kord_tm,hydrostatic, &
!$OMP                                  pt,pk,rg,peln,q,nwat,liq_wat,rainwat,ice_wat,snowwat,    &
!$OMP                                  graupel,q_con,sphum,cappa,r_vir,rcp,k1k,delp, &
!$OMP                                  delz,akap,pkz,te,u,v,ps, gridstruct, last_step, &
!$OMP                                  ze0,ak,bk,nq,isd,ied,jsd,jed,kord_tr,fill, adiabatic, &
!$OMP                                  hs,w,ws,kord_wz,do_omega,omga,rrg,kord_mt,ua)    &
!$OMP                          private(qv,gz,cvm,dz1,z_rat,kp,k_next,bkh,deng,dp2,   &
!$OMP                                  pe0,pe1,pe2,pe3,pk1,pk2,pn2,phis,q2,ze1,ze2,ztop)
  do 1000 j=js,je+1

     do k=1,km+1
        do i=is,ie
           pe1(i,k) = pe(i,k,j)
        enddo
     enddo

     do i=is,ie
        pe2(i,   1) = ptop
        pe2(i,km+1) = pe(i,km+1,j)
     enddo

  if ( j /= (je+1) ) then
       if ( kord_tm < 0 ) then
! Note: pt at this stage is Theta_v
             if ( hydrostatic ) then
! Transform virtual pt to virtual Temp
             do k=1,km
                   do i=is,ie
                      pt(i,j,k) = pt(i,j,k)*(pk(i,j,k+1)-pk(i,j,k))/(akap*(peln(i,k+1,j)-peln(i,k,j)))
                   enddo
             enddo
             else
! Transform "density pt" to "density temp"
               do k=1,km
#ifdef MOIST_CAPPA
               if ( nwat==2 ) then
                  do i=is,ie
                            qv(i) = max(0., q(i,j,k,sphum)) 
                     q_con(i,j,k) = max(0., q(i,j,k,liq_wat))
                     cvm(i) = (1.-qv(i))*cv_air + qv(i)*cv_vap
                     cappa(i,j,k) = rdgas / ( rdgas + cvm(i)/(1.+r_vir*qv(i)) )
                     pt(i,j,k) = pt(i,j,k)*exp(cappa(i,j,k)/(1.-cappa(i,j,k))*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
                  enddo
               else
                  call moist_cv(is,ie,isd,ied,jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                                ice_wat, snowwat, graupel, q, gz, cvm)
                  do i=is,ie
                     q_con(i,j,k) = gz(i)
                     cappa(i,j,k) = rdgas / ( rdgas + cvm(i)/(1.+r_vir*q(i,j,k,sphum)) )
                     pt(i,j,k) = pt(i,j,k)*exp(cappa(i,j,k)/(1.-cappa(i,j,k))*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
                  enddo
               endif
#else
                  do i=is,ie
                     pt(i,j,k) = pt(i,j,k)*exp(k1k*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
! Using dry pressure for the definition of the virtual potential temperature
!                    pt(i,j,k) = pt(i,j,k)*exp(k1k*log(rrg*(1.-q(i,j,k,sphum))*delp(i,j,k)/delz(i,j,k)*    &
!                                              pt(i,j,k)/(1.+r_vir*q(i,j,k,sphum))))
                  enddo
#endif
               enddo
             endif         ! hydro test
       elseif ( hydrostatic ) then
           call pkez(km, is, ie, js, je, j, pe, pk, akap, peln, pkz, ptop)
! Compute cp*T + KE
           do k=1,km
                 do i=is,ie
                    te(i,j,k) = 0.25*gridstruct%rsin2(i,j)*(u(i,j,k)**2+u(i,j+1,k)**2 +  &
                                                 v(i,j,k)**2+v(i+1,j,k)**2 -  &
                               (u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*gridstruct%cosa_s(i,j))  &
                              + cp_air*pt(i,j,k)*pkz(i,j,k)
                 enddo
           enddo
       endif

     if ( .not. hydrostatic ) then
           do k=1,km
              do i=is,ie
                 delz(i,j,k) = -delz(i,j,k) / delp(i,j,k) ! ="specific volume"/grav
              enddo
           enddo
      endif

! update ps
      do i=is,ie
         ps(i,j) = pe1(i,km+1)
      enddo
!
! Hybrid sigma-P coordinate:
!
        do k=2,km
           do i=is,ie
              pe2(i,k) = ak(k) + bk(k)*pe(i,km+1,j)
           enddo
        enddo
        do k=1,km
           do i=is,ie
              dp2(i,k) = pe2(i,k+1) - pe2(i,k)
           enddo
        enddo

!------------
! update delp
!------------
      do k=1,km
         do i=is,ie
            delp(i,j,k) = dp2(i,k)
         enddo
      enddo

!------------------
! Compute p**Kappa
!------------------
   do k=1,km+1
      do i=is,ie
         pk1(i,k) = pk(i,j,k)
      enddo
   enddo

   do i=is,ie
      pn2(i,   1) = peln(i,   1,j)
      pn2(i,km+1) = peln(i,km+1,j)
      pk2(i,   1) = pk1(i,   1)
      pk2(i,km+1) = pk1(i,km+1)
   enddo

   do k=2,km
      do i=is,ie
         pn2(i,k) = log(pe2(i,k))
         pk2(i,k) = exp(akap*pn2(i,k))
      enddo
   enddo

   if ( kord_tm<0 ) then
!----------------------------------
! Map t using logp 
!----------------------------------
         call map_scalar(km,  peln(is,1,j),  pt, gz,   &
                         km,  pn2,           pt,              &
                         is, ie, j, isd, ied, jsd, jed, 1, abs(kord_tm), t_min)
   else
! Map pt using pe
         call map1_ppm (km,  pe1,  pt,  gz,       &
                        km,  pe2,  pt,                  &
                        is, ie, j, isd, ied, jsd, jed, 1, abs(kord_tm))
   endif

!----------------
! Map constituents
!----------------
      if( nq > 5 ) then
           call mapn_tracer(nq, km, pe1, pe2, q, dp2, kord_tr, j,     &
                            is, ie, isd, ied, jsd, jed, 0., fill)
      elseif ( nq > 0 ) then
! Remap one tracer at a time
         do iq=1,nq
             call map1_q2(km, pe1, q(isd,jsd,1,iq),     &
                          km, pe2, q2, dp2,             &
                          is, ie, 0, kord_tr(iq), j, isd, ied, jsd, jed, 0.)
            if (fill) call fillz(ie-is+1, km, 1, q2, dp2)
            do k=1,km
               do i=is,ie
                  q(i,j,k,iq) = q2(i,k)
               enddo
            enddo
         enddo
      endif

   if ( .not. hydrostatic ) then
! Remap vertical wind:
        call map1_ppm (km,   pe1,  w,  ws(is,j),   &
                       km,   pe2,  w,              &
                       is, ie, j, isd, ied, jsd, jed, -2, kord_wz)
! Remap delz for hybrid sigma-p coordinate
        call map1_ppm (km,   pe1, delz,  gz,   &
                       km,   pe2, delz,              &
                       is, ie, j, isd,  ied,  jsd,  jed,  1, abs(kord_tm))
        do k=1,km
           do i=is,ie
              delz(i,j,k) = -delz(i,j,k)*dp2(i,k)
           enddo
        enddo
   endif

!----------
! Update pk
!----------
   do k=1,km+1
      do i=is,ie
         pk(i,j,k) = pk2(i,k)
      enddo
   enddo

!----------------
   if ( do_omega ) then
! Start do_omega
! Copy omega field to pe3
      do i=is,ie
         pe3(i,1) = 0.
      enddo
      do k=2,km+1
         do i=is,ie
            pe3(i,k) = omga(i,j,k-1)
         enddo
      enddo
   endif

   do k=1,km+1
      do i=is,ie
          pe0(i,k)   = peln(i,k,j)
         peln(i,k,j) =  pn2(i,k)
      enddo
   enddo

!------------
! Compute pkz
!------------
   if ( hydrostatic ) then
      do k=1,km
         do i=is,ie
            pkz(i,j,k) = (pk2(i,k+1)-pk2(i,k))/(akap*(peln(i,k+1,j)-peln(i,k,j)))
         enddo
      enddo
   else
! Note: pt at this stage is T_v or T_m
         do k=1,km
#ifdef MOIST_CAPPA
         if ( nwat==2 ) then
            do i=is,ie
                      qv(i) = max(0., q(i,j,k,sphum)) 
               q_con(i,j,k) = max(0., q(i,j,k,liq_wat))
               cvm(i) = (1.-qv(i))*cv_air + qv(i)*cv_vap
               cappa(i,j,k) = rdgas / ( rdgas + cvm(i)/(1.+r_vir*qv(i)) )
               pkz(i,j,k) = exp(cappa(i,j,k)*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
            enddo
         else
            call moist_cv(is,ie,isd,ied,jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                          ice_wat, snowwat, graupel, q, gz, cvm)
            do i=is,ie
               q_con(i,j,k) = gz(i)
               cappa(i,j,k) = rdgas / ( rdgas + cvm(i)/(1.+r_vir*q(i,j,k,sphum)) )
               pkz(i,j,k) = exp(cappa(i,j,k)*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
            enddo
         endif    ! nwat test
#else
         if ( kord_tm < 0 ) then
           do i=is,ie
              pkz(i,j,k) = exp(akap*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
! Using dry pressure for the definition of the virtual potential temperature
!             pkz(i,j,k) = exp(akap*log(rrg*(1.-q(i,j,k,sphum))*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)/(1.+r_vir*q(i,j,k,sphum))))
           enddo
         else
           do i=is,ie
              pkz(i,j,k) = exp(k1k*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
! Using dry pressure for the definition of the virtual potential temperature
!             pkz(i,j,k) = exp(k1k*log(rrg*(1.-q(i,j,k,sphum))*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)/(1.+r_vir*q(i,j,k,sphum))))
           enddo
           if ( last_step .and. (.not.adiabatic) ) then
              do i=is,ie
                 pt(i,j,k) = pt(i,j,k)*pkz(i,j,k)
              enddo
           endif
         endif
#endif
         enddo
   endif

! Interpolate omega/pe3 (defined at pe0) to remapped cell center (dp2)
   if ( do_omega ) then
   do k=1,km
      do i=is,ie
         dp2(i,k) = 0.5*(peln(i,k,j) + peln(i,k+1,j))
      enddo
   enddo
   do i=is,ie
       k_next = 1
       do n=1,km
          kp = k_next
          do k=kp,km
             if( dp2(i,n) <= pe0(i,k+1) .and. dp2(i,n) >= pe0(i,k) ) then
                 omga(i,j,n) = pe3(i,k)  +  (pe3(i,k+1) - pe3(i,k)) *    &
                       (dp2(i,n)-pe0(i,k)) / (pe0(i,k+1)-pe0(i,k) )
                 k_next = k
                 exit
             endif
          enddo
       enddo
   enddo
   endif     ! end do_omega

  endif !(j < je+1)

      do i=is,ie+1
         pe0(i,1) = pe(i,1,j)
      enddo
!------
! map u
!------
      do k=2,km+1
         do i=is,ie
            pe0(i,k) = 0.5*(pe(i,k,j-1)+pe1(i,k))
         enddo
      enddo

      do k=1,km+1
         bkh = 0.5*bk(k)
         do i=is,ie
            pe3(i,k) = ak(k) + bkh*(pe(i,km+1,j-1)+pe1(i,km+1))
         enddo
      enddo

      call map1_ppm( km, pe0(is:ie,:),   u,   gz,   &
                     km, pe3(is:ie,:),   u,               &
                     is, ie, j, isd, ied, jsd, jed+1, -1, kord_mt)

   if (j < je+1) then
!------
! map v
!------
       do i=is,ie+1
          pe3(i,1) = ak(1)
       enddo

       do k=2,km+1
          bkh = 0.5*bk(k)
          do i=is,ie+1
             pe0(i,k) =         0.5*(pe(i-1,k,   j)+pe(i,k,   j))
             pe3(i,k) = ak(k) + bkh*(pe(i-1,km+1,j)+pe(i,km+1,j))
          enddo
       enddo

       call map1_ppm (km, pe0,  v, gz,    &
                      km, pe3,  v, is, ie+1,    &
                      j, isd, ied+1, jsd, jed, -1, kord_mt)
   endif ! (j < je+1)

     do k=1,km
        do i=is,ie
           ua(i,j,k) = pe2(i,k+1)
        enddo
     enddo

1000  continue

!$OMP parallel default(none) shared(is,ie,js,je,km,ptop,u,v,pe,ua,isd,ied,jsd,jed,kord_mt, &
!$OMP                               te_2d,te,delp,hydrostatic,hs,rg,pt,peln, adiabatic, &
!$OMP                               cp,delz,nwat,rainwat,liq_wat,ice_wat,snowwat,     &
!$OMP                               graupel,q_con,r_vir,sphum,w,pk,pkz,last_step,consv, &
!$OMP                               do_adiabatic_init,zsum1,zsum0,te0_2d,domain,   &
!$OMP                               ng,gridstruct,E_Flux,pdt,dtmp,reproduce_sum,q, &
!$OMP                               mdt,cld_amt,cappa,dtdt,out_dt,rrg,akap,do_sat_adj,kord_tm) &
!$OMP                       private(pe0,pe1,pe2,pe3,qv,cvm,gz,phis,tpe,tmp, dlnp,dpeln)

!$OMP do
  do k=2,km
     do j=js,je
        do i=is,ie
           pe(i,k,j) = ua(i,j,k-1)
        enddo
     enddo
  enddo

dtmp = 0.
if( last_step .and. (.not.do_adiabatic_init)  ) then

  if ( consv > consv_min ) then

!$OMP do
    do j=js,je
       if ( hydrostatic ) then
            do i=is,ie
               gz(i) = hs(i,j)
               do k=1,km
                  gz(i) = gz(i) + rg*pt(i,j,k)*(peln(i,k+1,j)-peln(i,k,j))
               enddo
            enddo
            do i=is,ie
               te_2d(i,j) = pe(i,km+1,j)*hs(i,j) - pe(i,1,j)*gz(i)
            enddo

            do k=1,km
            do i=is,ie
               te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*(cp*pt(i,j,k) +   &
                            0.25*gridstruct%rsin2(i,j)*(u(i,j,k)**2+u(i,j+1,k)**2 +  &
                                                        v(i,j,k)**2+v(i+1,j,k)**2 -  &
                           (u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*gridstruct%cosa_s(i,j)))
            enddo
            enddo
       else
           do i=is,ie
              te_2d(i,j) = 0.
              phis(i,km+1) = hs(i,j)
           enddo
           do k=km,1,-1
              do i=is,ie
                 phis(i,k) = phis(i,k+1) - grav*delz(i,j,k)
              enddo
           enddo

           do k=1,km
#ifdef MOIST_CAPPA
           if ( nwat==2 ) then
              do i=is,ie
                 qv(i) = max(0., q(i,j,k,sphum)) 
                 gz(i) = max(0., q(i,j,k,liq_wat))
                 cvm(i) = (1.-qv(i))*cv_air + qv(i)*cv_vap
              enddo
           else
              call moist_cv(is,ie,isd,ied,jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                            ice_wat, snowwat, graupel, q, gz, cvm)
           endif
              do i=is,ie
! KE using 3D winds:
              q_con(i,j,k) = gz(i)
              te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*(cvm(i)*pt(i,j,k)/((1.+r_vir*q(i,j,k,sphum))*(1.-gz(i))) + &
                              0.5*(phis(i,k)+phis(i,k+1) + w(i,j,k)**2 + 0.5*gridstruct%rsin2(i,j)*( &
                              u(i,j,k)**2+u(i,j+1,k)**2 + v(i,j,k)**2+v(i+1,j,k)**2 -  &
                             (u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*gridstruct%cosa_s(i,j))))
              enddo
#else
              do i=is,ie
                 te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*(cv_air*pt(i,j,k)/(1.+r_vir*q(i,j,k,sphum)) + &
                                 0.5*(phis(i,k)+phis(i,k+1) + w(i,j,k)**2 + 0.5*gridstruct%rsin2(i,j)*( &
                                 u(i,j,k)**2+u(i,j+1,k)**2 + v(i,j,k)**2+v(i+1,j,k)**2 -  &
                                (u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*gridstruct%cosa_s(i,j))))
              enddo
#endif
           enddo   ! k-loop
       endif  ! end non-hydro

         do i=is,ie
            te_2d(i,j) = te0_2d(i,j) - te_2d(i,j)
            zsum1(i,j) = pkz(i,j,1)*delp(i,j,1)
         enddo
         do k=2,km
            do i=is,ie
               zsum1(i,j) = zsum1(i,j) + pkz(i,j,k)*delp(i,j,k)
            enddo
         enddo
         if ( hydrostatic ) then
            do i=is,ie
               zsum0(i,j) = ptop*(pk(i,j,1)-pk(i,j,km+1)) + zsum1(i,j)
            enddo
         endif

    enddo   ! j-loop

!$OMP single
         tpe = consv*g_sum(domain, te_2d, is, ie, js, je, ng, gridstruct%area_64, 0, reproduce=.true.)
      E_Flux = tpe / (grav*pdt*4.*pi*radius**2)    ! unit: W/m**2
                                                   ! Note pdt is "phys" time step
      if ( hydrostatic ) then
           dtmp = tpe / (cp*g_sum(domain, zsum0,  is, ie, js, je, ng, gridstruct%area_64, 0, reproduce=.true.))
      else
           dtmp = tpe / (cv_air*g_sum(domain, zsum1, is, ie, js, je, ng, gridstruct%area_64, 0, reproduce=.true.))
      endif
!$OMP end single

  elseif ( consv < -consv_min ) then

!$OMP do
      do j=js,je
         do i=is,ie
            zsum1(i,j) = pkz(i,j,1)*delp(i,j,1)
         enddo
         do k=2,km
            do i=is,ie
               zsum1(i,j) = zsum1(i,j) + pkz(i,j,k)*delp(i,j,k)
            enddo
         enddo
         if ( hydrostatic ) then
            do i=is,ie
               zsum0(i,j) = ptop*(pk(i,j,1)-pk(i,j,km+1)) + zsum1(i,j)
            enddo
         endif
      enddo

      E_Flux = consv
!$OMP single
      if ( hydrostatic ) then
           dtmp = E_flux*(grav*pdt*4.*pi*radius**2) /    &
                 (cp*g_sum(domain, zsum0,  is, ie, js, je, ng, gridstruct%area_64, 0, reproduce=.true.))
      else
           dtmp = E_flux*(grav*pdt*4.*pi*radius**2) /    &
                 (cv_air*g_sum(domain, zsum1,  is, ie, js, je, ng, gridstruct%area_64, 0, reproduce=.true.))
      endif
!$OMP end single
  endif        ! end consv check
endif        ! end last_step check

! Note: pt at this stage is T_v
  if ( .not. do_adiabatic_init ) then
#ifdef FAST_LIN_MP
! Notes: must set consv=0
                                           call timing_on('FAST_LIN_MP')
       call lin_fast_mp(u, v, w, pt, q, sphum, liq_wat, rainwat, ice_wat, snowwat, graupel, cld_amt,  & 
                        delp, delz, gridstruct, mdt, lprec, fprec, f_land,      &
                        hydrostatic, phys_hydrostatic, time)
                                           call timing_off('FAST_LIN_MP')
#else
       if ( do_sat_adj ) then
                                           call timing_on('sat_adj2')
!$OMP do
           do k=1,km
              do j=js,je
                 do i=is,ie
                    dpeln(i,j) = peln(i,k+1,j) - peln(i,k,j)
                 enddo
              enddo
              call sat_adj2(mdt, is, ie, js, je, ng, hydrostatic, consv>consv_min, &
                            te(is,js,k), q(isd,jsd,k,sphum), q(isd,jsd,k,liq_wat),   &
                            q(isd,jsd,k,ice_wat), q(isd,jsd,k,rainwat),    &
                            q(isd,jsd,k,snowwat), q(isd,jsd,k,graupel), q(isd,jsd,k,cld_amt), gridstruct%area(isd,jsd), &
                            dpeln, delz(isd:,jsd:,k), pt(isd,jsd,k), delp(isd,jsd,k),           &
                            q_con(isd:,jsd:,k), &
#ifdef MOIST_CAPPA
                            cappa(isd:,jsd:,k), &
#endif
                            dtdt(is,js,k), out_dt, last_step)
              if ( .not. hydrostatic  ) then
                 do j=js,je
                    do i=is,ie
#ifdef MOIST_CAPPA
                       pkz(i,j,k) = exp(cappa(i,j,k)*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
#else
                       pkz(i,j,k) = exp(akap*log(rrg*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)))
! Using dry pressure for the definition of the virtual potential temperature
!                      pkz(i,j,k) = exp(akap*log(rrg*(1.-q(i,j,k,sphum))*delp(i,j,k)/delz(i,j,k)*pt(i,j,k)/(1.+r_vir*q(i,j,k,sphum))))
#endif
                    enddo
                 enddo
              endif
           enddo    ! OpenMP k-loop

           if ( consv > consv_min ) then
!$OMP do
                do j=js,je
                   do k=1,km
                      do i=is,ie
                         te0_2d(i,j) = te0_2d(i,j) + te(i,j,k)
                      enddo
                   enddo
                enddo
           endif
                                           call timing_off('sat_adj2')
       endif  ! do_sat_adj
#endif
  endif   ! .not. do_adiabatic_init


  if ( last_step ) then
       ! Output temperature if last_step
!$OMP do
        do k=1,km
           do j=js,je
#ifdef USE_COND
              if ( nwat==2 ) then
                 do i=is,ie
                    gz(i) = max(0., q(i,j,k,liq_wat))
                    qv(i) = max(0., q(i,j,k,sphum)) 
                    pt(i,j,k) = (pt(i,j,k)+dtmp*pkz(i,j,k)) / ((1.+r_vir*qv(i))*(1.-gz(i)))
                 enddo
              elseif ( nwat==6 ) then
                 do i=is,ie
                    gz(i) = q(i,j,k,liq_wat)+q(i,j,k,rainwat)+q(i,j,k,ice_wat)+q(i,j,k,snowwat)+q(i,j,k,graupel)
                    pt(i,j,k) = (pt(i,j,k)+dtmp*pkz(i,j,k))/((1.+r_vir*q(i,j,k,sphum))*(1.-gz(i)))
                 enddo
              else
                 call moist_cv(is,ie,isd,ied,jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                               ice_wat, snowwat, graupel, q, gz, cvm)
                 do i=is,ie
                    pt(i,j,k) = (pt(i,j,k)+dtmp*pkz(i,j,k)) / ((1.+r_vir*q(i,j,k,sphum))*(1.-gz(i)))
                 enddo
              endif
#else
              if ( .not. adiabatic ) then
                 do i=is,ie
                    pt(i,j,k) = (pt(i,j,k)+dtmp*pkz(i,j,k)) / (1.+r_vir*q(i,j,k,sphum))
                 enddo
              endif
#endif
           enddo   ! j-loop
        enddo  ! k-loop
  else  ! not last_step
    if ( kord_tm < 0 ) then
!$OMP do
       do k=1,km
          do j=js,je
             do i=is,ie
                pt(i,j,k) = pt(i,j,k)/pkz(i,j,k)
             enddo
          enddo
       enddo
    endif
  endif
!$OMP end parallel

 end subroutine Lagrangian_to_Eulerian


 subroutine compute_total_energy(is, ie, js, je, isd, ied, jsd, jed, km,       &
                                 u, v, w, delz, pt, delp, q, qc, pe, peln, hs, &
                                 rsin2_l, cosa_s_l, &
                                 r_vir,  cp, rg, hlv, te_2d, ua, va, teq, &
                                 moist_phys, nwat, sphum, liq_wat, rainwat, ice_wat, snowwat, graupel, hydrostatic, id_te)
!------------------------------------------------------
! Compute vertically integrated total energy per column
!------------------------------------------------------
! !INPUT PARAMETERS:
   integer,  intent(in):: km, is, ie, js, je, isd, ied, jsd, jed, id_te
   integer,  intent(in):: sphum, liq_wat, ice_wat, rainwat, snowwat, graupel, nwat
   real, intent(inout), dimension(isd:ied,jsd:jed,km):: ua, va
   real, intent(in), dimension(isd:ied,jsd:jed,km):: pt, delp
   real, intent(in), dimension(isd:ied,jsd:jed,km,*):: q
   real, intent(in), dimension(isd:ied,jsd:jed,km):: qc
   real, intent(inout)::  u(isd:ied,  jsd:jed+1,km)
   real, intent(inout)::  v(isd:ied+1,jsd:jed,  km)
   real, intent(in)::  w(isd:,jsd:,1:)   ! vertical velocity (m/s)
   real, intent(in):: delz(isd:,jsd:,1:)
   real, intent(in):: hs(isd:ied,jsd:jed)  ! surface geopotential
   real, intent(in)::   pe(is-1:ie+1,km+1,js-1:je+1) ! pressure at layer edges
   real, intent(in):: peln(is:ie,km+1,js:je)  ! log(pe)
   real, intent(in):: cp, rg, r_vir, hlv
   real, intent(in) :: rsin2_l(isd:ied, jsd:jed)
   real, intent(in) :: cosa_s_l(isd:ied, jsd:jed)
   logical, intent(in):: moist_phys, hydrostatic
! Output:
   real, intent(out):: te_2d(is:ie,js:je)   ! vertically integrated TE
   real, intent(out)::   teq(is:ie,js:je)   ! Moist TE
! Local
   real, dimension(is:ie,km):: tv
   real  phiz(is:ie,km+1)
   real  cvm(is:ie), qd(is:ie)
   integer i, j, k

!----------------------
! Output lat-lon winds:
!----------------------
!  call cubed_to_latlon(u, v, ua, va, dx, dy, rdxa, rdya, km, flagstruct%c2l_ord)

!$OMP parallel do default(none) shared(is,ie,js,je,isd,ied,jsd,jed,km,hydrostatic,hs,pt,qc,rg,peln,te_2d, &
!$OMP                                  pe,delp,cp,rsin2_l,u,v,cosa_s_l,delz,moist_phys,w, &
!$OMP                                  q,nwat,liq_wat,rainwat,ice_wat,snowwat,graupel,sphum)   &
!$OMP                          private(phiz, tv, cvm, qd)
  do j=js,je

     if ( hydrostatic ) then

        do i=is,ie
           phiz(i,km+1) = hs(i,j)
        enddo
        do k=km,1,-1
           do i=is,ie
                tv(i,k) = pt(i,j,k)*(1.+qc(i,j,k))
              phiz(i,k) = phiz(i,k+1) + rg*tv(i,k)*(peln(i,k+1,j)-peln(i,k,j))
           enddo
        enddo

        do i=is,ie
           te_2d(i,j) = pe(i,km+1,j)*phiz(i,km+1) - pe(i,1,j)*phiz(i,1)
        enddo

        do k=1,km
           do i=is,ie
              te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*(cp*tv(i,k) +            &
                           0.25*rsin2_l(i,j)*(u(i,j,k)**2+u(i,j+1,k)**2 +      &
                                            v(i,j,k)**2+v(i+1,j,k)**2 -      &
                       (u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*cosa_s_l(i,j)))
           enddo
        enddo

     else
!-----------------
! Non-hydrostatic:
!-----------------
     do i=is,ie
        phiz(i,km+1) = hs(i,j)
        do k=km,1,-1
           phiz(i,k) = phiz(i,k+1) - grav*delz(i,j,k)
        enddo
     enddo
     do i=is,ie
        te_2d(i,j) = 0.
     enddo
     if ( moist_phys ) then
     do k=1,km
#ifdef MOIST_CAPPA
        call moist_cv(is,ie,isd,ied,jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                      ice_wat, snowwat, graupel, q, qd, cvm)
#endif
        do i=is,ie
#ifdef MOIST_CAPPA
           te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*( cvm(i)*pt(i,j,k) +  &
#else
           te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*( cv_air*pt(i,j,k) +  &
#endif
                        0.5*(phiz(i,k)+phiz(i,k+1)+w(i,j,k)**2+0.5*rsin2_l(i,j)*(u(i,j,k)**2+u(i,j+1,k)**2 +  &
                        v(i,j,k)**2+v(i+1,j,k)**2-(u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*cosa_s_l(i,j))))
        enddo
     enddo
     else
       do k=1,km
          do i=is,ie
             te_2d(i,j) = te_2d(i,j) + delp(i,j,k)*( cv_air*pt(i,j,k) +  &
                          0.5*(phiz(i,k)+phiz(i,k+1)+w(i,j,k)**2+0.5*rsin2_l(i,j)*(u(i,j,k)**2+u(i,j+1,k)**2 +  &
                          v(i,j,k)**2+v(i+1,j,k)**2-(u(i,j,k)+u(i,j+1,k))*(v(i,j,k)+v(i+1,j,k))*cosa_s_l(i,j))))
          enddo
       enddo
     endif
     endif
  enddo

!-------------------------------------
! Doganostics computation for moist TE
!-------------------------------------
  if( id_te>0 ) then
!$OMP parallel do default(none) shared(is,ie,js,je,teq,te_2d,moist_phys,km,hlv,sphum,q,delp)
      do j=js,je
         do i=is,ie
            teq(i,j) = te_2d(i,j)
         enddo
         if ( moist_phys ) then
           do k=1,km
              do i=is,ie
                 teq(i,j) = teq(i,j) + hlv*q(i,j,k,sphum)*delp(i,j,k)
              enddo
           enddo
         endif
      enddo
   endif

  end subroutine compute_total_energy


  subroutine pkez(km, ifirst, ilast, jfirst, jlast, j, &
                  pe, pk, akap, peln, pkz, ptop)

! !INPUT PARAMETERS:
   integer, intent(in):: km, j
   integer, intent(in):: ifirst, ilast        ! Latitude strip
   integer, intent(in):: jfirst, jlast        ! Latitude strip
   real, intent(in):: akap
   real, intent(in):: pe(ifirst-1:ilast+1,km+1,jfirst-1:jlast+1)
   real, intent(in):: pk(ifirst:ilast,jfirst:jlast,km+1)
   real, intent(IN):: ptop
! !OUTPUT
   real, intent(out):: pkz(ifirst:ilast,jfirst:jlast,km)
   real, intent(inout):: peln(ifirst:ilast, km+1, jfirst:jlast)   ! log (pe)
! Local
   real pk2(ifirst:ilast, km+1)
   real pek
   real lnp
   real ak1
   integer i, k

   ak1 = (akap + 1.) / akap

        pek = pk(ifirst,j,1)
        do i=ifirst, ilast
           pk2(i,1) = pek
        enddo

        do k=2,km+1
           do i=ifirst, ilast
!             peln(i,k,j) =  log(pe(i,k,j))
              pk2(i,k) =  pk(i,j,k)
           enddo
        enddo

!---- GFDL modification
       if( ptop < ptop_min ) then
           do i=ifirst, ilast
               peln(i,1,j) = peln(i,2,j) - ak1
           enddo
       else
           lnp = log( ptop )
           do i=ifirst, ilast
              peln(i,1,j) = lnp
           enddo
       endif
!---- GFDL modification

       do k=1,km
          do i=ifirst, ilast
             pkz(i,j,k) = (pk2(i,k+1) - pk2(i,k) )  /  &
                          (akap*(peln(i,k+1,j) - peln(i,k,j)) )
          enddo
       enddo

 end subroutine pkez



 subroutine remap_z(km, pe1, q1, kn, pe2, q2, i1, i2, iv, kord)

! !INPUT PARAMETERS:
      integer, intent(in) :: i1                ! Starting longitude
      integer, intent(in) :: i2                ! Finishing longitude
      integer, intent(in) :: kord              ! Method order
      integer, intent(in) :: km                ! Original vertical dimension
      integer, intent(in) :: kn                ! Target vertical dimension
      integer, intent(in) :: iv

      real, intent(in) ::  pe1(i1:i2,km+1)     ! height at layer edges 
                                               ! (from model top to bottom surface)
      real, intent(in) ::  pe2(i1:i2,kn+1)     ! hieght at layer edges 
                                               ! (from model top to bottom surface)
      real, intent(in) ::  q1(i1:i2,km)        ! Field input

! !INPUT/OUTPUT PARAMETERS:
      real, intent(inout)::  q2(i1:i2,kn)      ! Field output

! !LOCAL VARIABLES:
      real   qs(i1:i2)
      real  dp1(  i1:i2,km)
      real   q4(4,i1:i2,km)
      real   pl, pr, qsum, delp, esl
      integer i, k, l, m, k0

      do k=1,km
         do i=i1,i2
             dp1(i,k) = pe1(i,k+1) - pe1(i,k)      ! negative
            q4(1,i,k) = q1(i,k)
         enddo
      enddo

! Compute vertical subgrid distribution
   if ( kord >7 ) then
        call  cs_profile( qs, q4, dp1, km, i1, i2, iv, kord )
   else
        call ppm_profile( q4, dp1, km, i1, i2, iv, kord )
   endif

! Mapping
      do 1000 i=i1,i2
         k0 = 1
      do 555 k=1,kn
      do 100 l=k0,km
! locate the top edge: pe2(i,k)
      if(pe2(i,k) <= pe1(i,l) .and. pe2(i,k) >= pe1(i,l+1)) then
         pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
         if(pe2(i,k+1) >= pe1(i,l+1)) then
! entire new grid is within the original grid
            pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
            q2(i,k) = q4(2,i,l) + 0.5*(q4(4,i,l)+q4(3,i,l)-q4(2,i,l))  &
                       *(pr+pl)-q4(4,i,l)*r3*(pr*(pr+pl)+pl**2)
               k0 = l
               goto 555
          else
! Fractional area...
            qsum = (pe1(i,l+1)-pe2(i,k))*(q4(2,i,l)+0.5*(q4(4,i,l)+   &
                    q4(3,i,l)-q4(2,i,l))*(1.+pl)-q4(4,i,l)*           &
                     (r3*(1.+pl*(1.+pl))))
              do m=l+1,km
! locate the bottom edge: pe2(i,k+1)
                 if(pe2(i,k+1) < pe1(i,m+1) ) then
! Whole layer..
                    qsum = qsum + dp1(i,m)*q4(1,i,m)
                 else
                    delp = pe2(i,k+1)-pe1(i,m)
                    esl = delp / dp1(i,m)
                    qsum = qsum + delp*(q4(2,i,m)+0.5*esl*               &
                         (q4(3,i,m)-q4(2,i,m)+q4(4,i,m)*(1.-r23*esl)))
                    k0 = m
                 goto 123
                 endif
              enddo
              goto 123
           endif
      endif
100   continue
123   q2(i,k) = qsum / ( pe2(i,k+1) - pe2(i,k) )
555   continue
1000  continue

 end subroutine remap_z

 subroutine map_scalar( km,   pe1,    q1,   qs,           &
                        kn,   pe2,    q2,   i1, i2,       &
                         j,  ibeg, iend, jbeg, jend, iv,  kord, q_min)
! iv=1
 integer, intent(in) :: i1                ! Starting longitude
 integer, intent(in) :: i2                ! Finishing longitude
 integer, intent(in) :: iv                ! Mode: 0 == constituents  1 == temp
                                          !       2 == remap temp with cs scheme
 integer, intent(in) :: kord              ! Method order
 integer, intent(in) :: j                 ! Current latitude
 integer, intent(in) :: ibeg, iend, jbeg, jend
 integer, intent(in) :: km                ! Original vertical dimension
 integer, intent(in) :: kn                ! Target vertical dimension
 real, intent(in) ::   qs(i1:i2)       ! bottom BC
 real, intent(in) ::  pe1(i1:i2,km+1)  ! pressure at layer edges 
                                       ! (from model top to bottom surface)
                                       ! in the original vertical coordinate
 real, intent(in) ::  pe2(i1:i2,kn+1)  ! pressure at layer edges 
                                       ! (from model top to bottom surface)
                                       ! in the new vertical coordinate
 real, intent(in) ::    q1(ibeg:iend,jbeg:jend,km) ! Field input
! !INPUT/OUTPUT PARAMETERS:
 real, intent(inout)::  q2(ibeg:iend,jbeg:jend,kn) ! Field output
 real, intent(in):: q_min

! !DESCRIPTION:
! IV = 0: constituents
! pe1: pressure at layer edges (from model top to bottom surface)
!      in the original vertical coordinate
! pe2: pressure at layer edges (from model top to bottom surface)
!      in the new vertical coordinate
! !LOCAL VARIABLES:
   real    dp1(i1:i2,km)
   real   q4(4,i1:i2,km)
   real    pl, pr, qsum, dp, esl
   integer i, k, l, m, k0

   do k=1,km
      do i=i1,i2
         dp1(i,k) = pe1(i,k+1) - pe1(i,k)
         q4(1,i,k) = q1(i,j,k)
      enddo
   enddo

! Compute vertical subgrid distribution
   if ( kord >7 ) then
        call scalar_profile( qs, q4, dp1, km, i1, i2, iv, kord, q_min )
   else
        call ppm_profile( q4, dp1, km, i1, i2, iv, kord )
   endif

  do i=i1,i2
     k0 = 1
     do 555 k=1,kn
      do l=k0,km
! locate the top edge: pe2(i,k)
      if( pe2(i,k) >= pe1(i,l) .and. pe2(i,k) <= pe1(i,l+1) ) then
         pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
         if( pe2(i,k+1) <= pe1(i,l+1) ) then
! entire new grid is within the original grid
            pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
            q2(i,j,k) = q4(2,i,l) + 0.5*(q4(4,i,l)+q4(3,i,l)-q4(2,i,l))  &
                       *(pr+pl)-q4(4,i,l)*r3*(pr*(pr+pl)+pl**2)
               k0 = l
               goto 555
         else
! Fractional area...
            qsum = (pe1(i,l+1)-pe2(i,k))*(q4(2,i,l)+0.5*(q4(4,i,l)+   &
                    q4(3,i,l)-q4(2,i,l))*(1.+pl)-q4(4,i,l)*           &
                     (r3*(1.+pl*(1.+pl))))
              do m=l+1,km
! locate the bottom edge: pe2(i,k+1)
                 if( pe2(i,k+1) > pe1(i,m+1) ) then
! Whole layer
                     qsum = qsum + dp1(i,m)*q4(1,i,m)
                 else
                     dp = pe2(i,k+1)-pe1(i,m)
                     esl = dp / dp1(i,m)
                     qsum = qsum + dp*(q4(2,i,m)+0.5*esl*               &
                           (q4(3,i,m)-q4(2,i,m)+q4(4,i,m)*(1.-r23*esl)))
                     k0 = m
                     goto 123
                 endif
              enddo
              goto 123
         endif
      endif
      enddo
123   q2(i,j,k) = qsum / ( pe2(i,k+1) - pe2(i,k) )
555   continue
  enddo

 end subroutine map_scalar


 subroutine map1_ppm( km,   pe1,    q1,   qs,           &
                      kn,   pe2,    q2,   i1, i2,       &
                      j,    ibeg, iend, jbeg, jend, iv,  kord)
 integer, intent(in) :: i1                ! Starting longitude
 integer, intent(in) :: i2                ! Finishing longitude
 integer, intent(in) :: iv                ! Mode: 0 == constituents  1 == ???
                                          !       2 == remap temp with cs scheme
 integer, intent(in) :: kord              ! Method order
 integer, intent(in) :: j                 ! Current latitude
 integer, intent(in) :: ibeg, iend, jbeg, jend
 integer, intent(in) :: km                ! Original vertical dimension
 integer, intent(in) :: kn                ! Target vertical dimension
 real, intent(in) ::   qs(i1:i2)       ! bottom BC
 real, intent(in) ::  pe1(i1:i2,km+1)  ! pressure at layer edges 
                                       ! (from model top to bottom surface)
                                       ! in the original vertical coordinate
 real, intent(in) ::  pe2(i1:i2,kn+1)  ! pressure at layer edges 
                                       ! (from model top to bottom surface)
                                       ! in the new vertical coordinate
 real, intent(in) ::    q1(ibeg:iend,jbeg:jend,km) ! Field input
! !INPUT/OUTPUT PARAMETERS:
 real, intent(inout)::  q2(ibeg:iend,jbeg:jend,kn) ! Field output

! !DESCRIPTION:
! IV = 0: constituents
! pe1: pressure at layer edges (from model top to bottom surface)
!      in the original vertical coordinate
! pe2: pressure at layer edges (from model top to bottom surface)
!      in the new vertical coordinate
! !LOCAL VARIABLES:
   real    dp1(i1:i2,km)
   real   q4(4,i1:i2,km)
   real    pl, pr, qsum, dp, esl
   integer i, k, l, m, k0

   do k=1,km
      do i=i1,i2
         dp1(i,k) = pe1(i,k+1) - pe1(i,k)
         q4(1,i,k) = q1(i,j,k)
      enddo
   enddo

! Compute vertical subgrid distribution
   if ( kord >7 ) then
        call  cs_profile( qs, q4, dp1, km, i1, i2, iv, kord )
   else
        call ppm_profile( q4, dp1, km, i1, i2, iv, kord )
   endif

  do i=i1,i2
     k0 = 1
     do 555 k=1,kn
      do l=k0,km
! locate the top edge: pe2(i,k)
      if( pe2(i,k) >= pe1(i,l) .and. pe2(i,k) <= pe1(i,l+1) ) then
         pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
         if( pe2(i,k+1) <= pe1(i,l+1) ) then
! entire new grid is within the original grid
            pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
            q2(i,j,k) = q4(2,i,l) + 0.5*(q4(4,i,l)+q4(3,i,l)-q4(2,i,l))  &
                       *(pr+pl)-q4(4,i,l)*r3*(pr*(pr+pl)+pl**2)
               k0 = l
               goto 555
         else
! Fractional area...
            qsum = (pe1(i,l+1)-pe2(i,k))*(q4(2,i,l)+0.5*(q4(4,i,l)+   &
                    q4(3,i,l)-q4(2,i,l))*(1.+pl)-q4(4,i,l)*           &
                     (r3*(1.+pl*(1.+pl))))
              do m=l+1,km
! locate the bottom edge: pe2(i,k+1)
                 if( pe2(i,k+1) > pe1(i,m+1) ) then
! Whole layer
                     qsum = qsum + dp1(i,m)*q4(1,i,m)
                 else
                     dp = pe2(i,k+1)-pe1(i,m)
                     esl = dp / dp1(i,m)
                     qsum = qsum + dp*(q4(2,i,m)+0.5*esl*               &
                           (q4(3,i,m)-q4(2,i,m)+q4(4,i,m)*(1.-r23*esl)))
                     k0 = m
                     goto 123
                 endif
              enddo
              goto 123
         endif
      endif
      enddo
123   q2(i,j,k) = qsum / ( pe2(i,k+1) - pe2(i,k) )
555   continue
  enddo

 end subroutine map1_ppm


 subroutine mapn_tracer(nq, km, pe1, pe2, q1, dp2, kord, j,     &
                        i1, i2, isd, ied, jsd, jed, q_min, fill)
! !INPUT PARAMETERS:
      integer, intent(in):: km                ! vertical dimension
      integer, intent(in):: j, nq, i1, i2
      integer, intent(in):: isd, ied, jsd, jed
      integer, intent(in):: kord(nq)
      real, intent(in)::  pe1(i1:i2,km+1)     ! pressure at layer edges 
                                              ! (from model top to bottom surface)
                                              ! in the original vertical coordinate
      real, intent(in)::  pe2(i1:i2,km+1)     ! pressure at layer edges 
                                              ! (from model top to bottom surface)
                                              ! in the new vertical coordinate
      real, intent(in)::  dp2(i1:i2,km)
      real, intent(in)::  q_min
      logical, intent(in):: fill
      real, intent(inout):: q1(isd:ied,jsd:jed,km,nq) ! Field input
! !LOCAL VARIABLES:
      real:: q4(4,i1:i2,km,nq)
      real:: q2(i1:i2,km,nq) ! Field output
      real:: qsum(nq)
      real:: dp1(i1:i2,km)
      real:: qs(i1:i2)
      real:: pl, pr, dp, esl, fac1, fac2
      integer:: i, k, l, m, k0, iq

      do k=1,km
         do i=i1,i2
            dp1(i,k) = pe1(i,k+1) - pe1(i,k)
         enddo
      enddo

      do iq=1,nq
         do k=1,km
            do i=i1,i2
               q4(1,i,k,iq) = q1(i,j,k,iq)
            enddo
         enddo
         call scalar_profile( qs, q4(1,i1,1,iq), dp1, km, i1, i2, 0, kord(iq), q_min )
      enddo

! Mapping
      do 1000 i=i1,i2
         k0 = 1
      do 555 k=1,km
      do 100 l=k0,km
! locate the top edge: pe2(i,k)
      if(pe2(i,k) >= pe1(i,l) .and. pe2(i,k) <= pe1(i,l+1)) then
         pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
         if(pe2(i,k+1) <= pe1(i,l+1)) then
! entire new grid is within the original grid
            pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
            fac1 = pr + pl
            fac2 = r3*(pr*fac1 + pl*pl) 
            fac1 = 0.5*fac1
            do iq=1,nq
               q2(i,k,iq) = q4(2,i,l,iq) + (q4(4,i,l,iq)+q4(3,i,l,iq)-q4(2,i,l,iq))*fac1  &
                                         -  q4(4,i,l,iq)*fac2
            enddo
            k0 = l
            goto 555
          else
! Fractional area...
            dp = pe1(i,l+1) - pe2(i,k)
            fac1 = 1. + pl
            fac2 = r3*(1.+pl*fac1)
            fac1 = 0.5*fac1
            do iq=1,nq
               qsum(iq) = dp*(q4(2,i,l,iq) + (q4(4,i,l,iq)+   &
                              q4(3,i,l,iq) - q4(2,i,l,iq))*fac1 - q4(4,i,l,iq)*fac2)
            enddo
            do m=l+1,km
! locate the bottom edge: pe2(i,k+1)
               if(pe2(i,k+1) > pe1(i,m+1) ) then
                                                   ! Whole layer..
                  do iq=1,nq
                     qsum(iq) = qsum(iq) + dp1(i,m)*q4(1,i,m,iq)
                  enddo
               else
                  dp = pe2(i,k+1)-pe1(i,m)
                  esl = dp / dp1(i,m)
                  fac1 = 0.5*esl
                  fac2 = 1.-r23*esl
                  do iq=1,nq
                     qsum(iq) = qsum(iq) + dp*( q4(2,i,m,iq) + fac1*(         &
                                q4(3,i,m,iq)-q4(2,i,m,iq)+q4(4,i,m,iq)*fac2 ) )
                  enddo
                  k0 = m
                  goto 123
               endif
            enddo
            goto 123
          endif
      endif
100   continue
123   continue
      do iq=1,nq
         q2(i,k,iq) = qsum(iq) / dp2(i,k)
      enddo
555   continue
1000  continue

  if (fill) call fillz(i2-i1+1, km, nq, q2, dp2)

  do iq=1,nq
!    if (fill) call fillz(i2-i1+1, km, 1, q2(i1,1,iq), dp2)
     do k=1,km
        do i=i1,i2
           q1(i,j,k,iq) = q2(i,k,iq)
        enddo
     enddo
  enddo

 end subroutine mapn_tracer


 subroutine map1_q2(km,   pe1,   q1,            &
                    kn,   pe2,   q2,   dp2,     &
                    i1,   i2,    iv,   kord, j, &
                    ibeg, iend, jbeg, jend, q_min )


! !INPUT PARAMETERS:
      integer, intent(in) :: j
      integer, intent(in) :: i1, i2
      integer, intent(in) :: ibeg, iend, jbeg, jend
      integer, intent(in) :: iv                ! Mode: 0 ==  constituents 1 == ???
      integer, intent(in) :: kord
      integer, intent(in) :: km                ! Original vertical dimension
      integer, intent(in) :: kn                ! Target vertical dimension

      real, intent(in) ::  pe1(i1:i2,km+1)     ! pressure at layer edges 
                                               ! (from model top to bottom surface)
                                               ! in the original vertical coordinate
      real, intent(in) ::  pe2(i1:i2,kn+1)     ! pressure at layer edges 
                                               ! (from model top to bottom surface)
                                               ! in the new vertical coordinate
      real, intent(in) ::  q1(ibeg:iend,jbeg:jend,km) ! Field input
      real, intent(in) ::  dp2(i1:i2,kn)
      real, intent(in) ::  q_min
! !INPUT/OUTPUT PARAMETERS:
      real, intent(inout):: q2(i1:i2,kn) ! Field output
! !LOCAL VARIABLES:
      real   qs(i1:i2)
      real   dp1(i1:i2,km)
      real   q4(4,i1:i2,km)
      real   pl, pr, qsum, dp, esl

      integer i, k, l, m, k0

      do k=1,km
         do i=i1,i2
             dp1(i,k) = pe1(i,k+1) - pe1(i,k)
            q4(1,i,k) = q1(i,j,k)
         enddo
      enddo

! Compute vertical subgrid distribution
   if ( kord >7 ) then
        call  scalar_profile( qs, q4, dp1, km, i1, i2, iv, kord, q_min )
   else
        call ppm_profile( q4, dp1, km, i1, i2, iv, kord )
   endif

! Mapping
      do 1000 i=i1,i2
         k0 = 1
      do 555 k=1,kn
      do 100 l=k0,km
! locate the top edge: pe2(i,k)
      if(pe2(i,k) >= pe1(i,l) .and. pe2(i,k) <= pe1(i,l+1)) then
         pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
         if(pe2(i,k+1) <= pe1(i,l+1)) then
! entire new grid is within the original grid
            pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
            q2(i,k) = q4(2,i,l) + 0.5*(q4(4,i,l)+q4(3,i,l)-q4(2,i,l))  &
                       *(pr+pl)-q4(4,i,l)*r3*(pr*(pr+pl)+pl**2)
               k0 = l
               goto 555
          else
! Fractional area...
            qsum = (pe1(i,l+1)-pe2(i,k))*(q4(2,i,l)+0.5*(q4(4,i,l)+   &
                    q4(3,i,l)-q4(2,i,l))*(1.+pl)-q4(4,i,l)*           &
                     (r3*(1.+pl*(1.+pl))))
              do m=l+1,km
! locate the bottom edge: pe2(i,k+1)
                 if(pe2(i,k+1) > pe1(i,m+1) ) then
                                                   ! Whole layer..
                    qsum = qsum + dp1(i,m)*q4(1,i,m)
                 else
                     dp = pe2(i,k+1)-pe1(i,m)
                    esl = dp / dp1(i,m)
                   qsum = qsum + dp*(q4(2,i,m)+0.5*esl*               &
                       (q4(3,i,m)-q4(2,i,m)+q4(4,i,m)*(1.-r23*esl)))
                   k0 = m
                   goto 123
                 endif
              enddo
              goto 123
          endif
      endif
100   continue
123   q2(i,k) = qsum / dp2(i,k)
555   continue
1000  continue

 end subroutine map1_q2



 subroutine remap_2d(km,   pe1,   q1,        &
                     kn,   pe2,   q2,        &
                     i1,   i2,    iv,   kord)
   integer, intent(in):: i1, i2
   integer, intent(in):: iv               ! Mode: 0 ==  constituents 1 ==others
   integer, intent(in):: kord
   integer, intent(in):: km               ! Original vertical dimension
   integer, intent(in):: kn               ! Target vertical dimension
   real, intent(in):: pe1(i1:i2,km+1)     ! pressure at layer edges 
                                          ! (from model top to bottom surface)
                                          ! in the original vertical coordinate
   real, intent(in):: pe2(i1:i2,kn+1)     ! pressure at layer edges 
                                          ! (from model top to bottom surface)
                                          ! in the new vertical coordinate
   real, intent(in) :: q1(i1:i2,km) ! Field input
   real, intent(out):: q2(i1:i2,kn) ! Field output
! !LOCAL VARIABLES:
   real   qs(i1:i2)
   real   dp1(i1:i2,km)
   real   q4(4,i1:i2,km)
   real   pl, pr, qsum, dp, esl
   integer i, k, l, m, k0

   do k=1,km
      do i=i1,i2
          dp1(i,k) = pe1(i,k+1) - pe1(i,k)
         q4(1,i,k) = q1(i,k)
      enddo
   enddo

! Compute vertical subgrid distribution
   if ( kord >7 ) then
        call  cs_profile( qs, q4, dp1, km, i1, i2, iv, kord )
   else
        call ppm_profile( q4, dp1, km, i1, i2, iv, kord )
   endif

   do i=i1,i2
      k0 = 1
      do 555 k=1,kn
#ifdef OLD_TOP_EDGE
         if( pe2(i,k+1) <= pe1(i,1) ) then
! Entire grid above old ptop
             q2(i,k) = q4(2,i,1)
         elseif( pe2(i,k) < pe1(i,1) .and. pe2(i,k+1)>pe1(i,1) ) then
! Partially above old ptop:
             q2(i,k) = q1(i,1)
#else
         if( pe2(i,k) <= pe1(i,1) ) then
! above old ptop:
             q2(i,k) = q1(i,1)
#endif
         else
           do l=k0,km
! locate the top edge: pe2(i,k)
           if( pe2(i,k) >= pe1(i,l) .and. pe2(i,k) <= pe1(i,l+1) ) then
               pl = (pe2(i,k)-pe1(i,l)) / dp1(i,l)
               if(pe2(i,k+1) <= pe1(i,l+1)) then
! entire new grid is within the original grid
                  pr = (pe2(i,k+1)-pe1(i,l)) / dp1(i,l)
                  q2(i,k) = q4(2,i,l) + 0.5*(q4(4,i,l)+q4(3,i,l)-q4(2,i,l))  &
                          *(pr+pl)-q4(4,i,l)*r3*(pr*(pr+pl)+pl**2)
                  k0 = l
                  goto 555
               else
! Fractional area...
                 qsum = (pe1(i,l+1)-pe2(i,k))*(q4(2,i,l)+0.5*(q4(4,i,l)+   &
                         q4(3,i,l)-q4(2,i,l))*(1.+pl)-q4(4,i,l)*           &
                        (r3*(1.+pl*(1.+pl))))
                 do m=l+1,km
! locate the bottom edge: pe2(i,k+1)
                    if(pe2(i,k+1) > pe1(i,m+1) ) then
                                                   ! Whole layer..
                       qsum = qsum + dp1(i,m)*q4(1,i,m)
                    else
                       dp = pe2(i,k+1)-pe1(i,m)
                      esl = dp / dp1(i,m)
                      qsum = qsum + dp*(q4(2,i,m)+0.5*esl*               &
                            (q4(3,i,m)-q4(2,i,m)+q4(4,i,m)*(1.-r23*esl)))
                      k0 = m
                      goto 123
                    endif
                 enddo
                 goto 123
               endif
           endif
           enddo
123        q2(i,k) = qsum / ( pe2(i,k+1) - pe2(i,k) )
         endif
555   continue
   enddo

 end subroutine remap_2d


 subroutine scalar_profile(qs, a4, delp, km, i1, i2, iv, kord, qmin)
! Optimized vertical profile reconstruction:
! Latest: Apr 2008 S.-J. Lin, NOAA/GFDL
 integer, intent(in):: i1, i2
 integer, intent(in):: km      ! vertical dimension
 integer, intent(in):: iv      ! iv =-1: winds
                               ! iv = 0: positive definite scalars
                               ! iv = 1: others
 integer, intent(in):: kord
 real, intent(in)   ::   qs(i1:i2)
 real, intent(in)   :: delp(i1:i2,km)     ! layer pressure thickness
 real, intent(inout):: a4(4,i1:i2,km)     ! Interpolated values
 real, intent(in):: qmin
!-----------------------------------------------------------------------
 logical:: extm(i1:i2,km) 
 real  gam(i1:i2,km)
 real    q(i1:i2,km+1)
 real   d4(i1:i2)
 real   bet, a_bot, grat 
 real   pmp_1, lac_1, pmp_2, lac_2
 integer i, k, im

 if ( iv .eq. -2 ) then
      do i=i1,i2
         gam(i,2) = 0.5
           q(i,1) = 1.5*a4(1,i,1)
      enddo
      do k=2,km-1
         do i=i1, i2
                  grat = delp(i,k-1) / delp(i,k)
                   bet =  2. + grat + grat - gam(i,k)
                q(i,k) = (3.*(a4(1,i,k-1)+a4(1,i,k)) - q(i,k-1))/bet
            gam(i,k+1) = grat / bet
         enddo
      enddo
      do i=i1,i2
            grat = delp(i,km-1) / delp(i,km) 
         q(i,km) = (3.*(a4(1,i,km-1)+a4(1,i,km)) - grat*qs(i) - q(i,km-1)) /  &
                   (2. + grat + grat - gam(i,km))
         q(i,km+1) = qs(i)
      enddo
      do k=km-1,1,-1
        do i=i1,i2
           q(i,k) = q(i,k) - gam(i,k+1)*q(i,k+1)
        enddo
      enddo
 else
  do i=i1,i2
         grat = delp(i,2) / delp(i,1)   ! grid ratio
          bet = grat*(grat+0.5)
       q(i,1) = ( (grat+grat)*(grat+1.)*a4(1,i,1) + a4(1,i,2) ) / bet
     gam(i,1) = ( 1. + grat*(grat+1.5) ) / bet
  enddo

  do k=2,km
     do i=i1,i2
           d4(i) = delp(i,k-1) / delp(i,k)
             bet =  2. + d4(i) + d4(i) - gam(i,k-1)
          q(i,k) = ( 3.*(a4(1,i,k-1)+d4(i)*a4(1,i,k)) - q(i,k-1) )/bet
        gam(i,k) = d4(i) / bet
     enddo
  enddo
 
  do i=i1,i2
         a_bot = 1. + d4(i)*(d4(i)+1.5)
     q(i,km+1) = (2.*d4(i)*(d4(i)+1.)*a4(1,i,km)+a4(1,i,km-1)-a_bot*q(i,km))  &
               / ( d4(i)*(d4(i)+0.5) - a_bot*gam(i,km) )
  enddo

  do k=km,1,-1
     do i=i1,i2
        q(i,k) = q(i,k) - gam(i,k)*q(i,k+1)
     enddo
  enddo
 endif

!----- Perfectly linear scheme --------------------------------
 if ( abs(kord) > 16 ) then
  do k=1,km
     do i=i1,i2
        a4(2,i,k) = q(i,k  )
        a4(3,i,k) = q(i,k+1)
        a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
     enddo
  enddo
  return
 endif
!----- Perfectly linear scheme --------------------------------
!------------------
! Apply constraints
!------------------
  im = i2 - i1 + 1

! Apply *large-scale* constraints 
  do i=i1,i2
     q(i,2) = min( q(i,2), max(a4(1,i,1), a4(1,i,2)) )
     q(i,2) = max( q(i,2), min(a4(1,i,1), a4(1,i,2)) )
  enddo

  do k=2,km
     do i=i1,i2
        gam(i,k) = a4(1,i,k) - a4(1,i,k-1)
     enddo
  enddo

! Interior:
  do k=3,km-1
     do i=i1,i2
        if ( gam(i,k-1)*gam(i,k+1)>0. ) then
! Apply large-scale constraint to ALL fields if not local max/min
             q(i,k) = min( q(i,k), max(a4(1,i,k-1),a4(1,i,k)) )
             q(i,k) = max( q(i,k), min(a4(1,i,k-1),a4(1,i,k)) )
        else
          if ( gam(i,k-1) > 0. ) then
! There exists a local max
               q(i,k) = max(q(i,k), min(a4(1,i,k-1),a4(1,i,k)))
          else
! There exists a local min
                 q(i,k) = min(q(i,k), max(a4(1,i,k-1),a4(1,i,k)))
               if ( iv==0 ) q(i,k) = max(0., q(i,k))
          endif
        endif
     enddo
  enddo

! Bottom:
  do i=i1,i2
     q(i,km) = min( q(i,km), max(a4(1,i,km-1), a4(1,i,km)) )
     q(i,km) = max( q(i,km), min(a4(1,i,km-1), a4(1,i,km)) )
  enddo

  do k=1,km
     do i=i1,i2
        a4(2,i,k) = q(i,k  )
        a4(3,i,k) = q(i,k+1)
     enddo
  enddo

  do k=2,km-1
     do i=i1,i2
        if ( gam(i,k)*gam(i,k+1) > 0.0 ) then
             extm(i,k) = .false. 
        else
             extm(i,k) = .true.
        endif
     enddo
  enddo

!---------------------------
! Apply subgrid constraints:
!---------------------------
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
! Top 2 and bottom 2 layers always use monotonic mapping

  if ( iv==0 ) then
     do i=i1,i2
        a4(2,i,1) = max(0., a4(2,i,1))
     enddo
  elseif ( iv==-1 ) then 
      do i=i1,i2
         if ( a4(2,i,1)*a4(1,i,1) <= 0. ) a4(2,i,1) = 0.
      enddo
  elseif ( iv==2 ) then
     do i=i1,i2
        a4(2,i,1) = a4(1,i,1)
        a4(3,i,1) = a4(1,i,1)
        a4(4,i,1) = 0.
     enddo
  endif

  if ( iv/=2 ) then
     do i=i1,i2
        a4(4,i,1) = 3.*(2.*a4(1,i,1) - (a4(2,i,1)+a4(3,i,1)))
     enddo
     call cs_limiters(im, extm(i1,1), a4(1,i1,1), 1)
  endif

! k=2
   do i=i1,i2
      a4(4,i,2) = 3.*(2.*a4(1,i,2) - (a4(2,i,2)+a4(3,i,2)))
   enddo
   call cs_limiters(im, extm(i1,2), a4(1,i1,2), 2)

!-------------------------------------
! Huynh's 2nd constraint for interior:
!-------------------------------------
  do k=3,km-2
     if ( abs(kord)<9 ) then
       do i=i1,i2
! Left  edges
          pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
          lac_1 = pmp_1 + 1.5*gam(i,k+2)
          a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),   &
                                         max(a4(1,i,k), pmp_1, lac_1) )
! Right edges
          pmp_2 = a4(1,i,k) + 2.*gam(i,k)
          lac_2 = pmp_2 - 1.5*gam(i,k-1)
          a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),    &
                                         max(a4(1,i,k), pmp_2, lac_2) )

          a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
       enddo

     elseif ( abs(kord)==9 ) then
       do i=i1,i2
          if ( extm(i,k) .and. (extm(i,k-1).or.extm(i,k+1).or.a4(1,i,k)<qmin) ) then
! grid-scale 2-delta-z wave detected
               a4(2,i,k) = a4(1,i,k)
               a4(3,i,k) = a4(1,i,k)
               a4(4,i,k) = 0.
          else
            a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
! Check within the smooth region if subgrid profile is non-monotonic
            if( abs(a4(4,i,k)) > abs(a4(2,i,k)-a4(3,i,k)) ) then
                  pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                  lac_1 = pmp_1 + 1.5*gam(i,k+2)
              a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),  &
                                             max(a4(1,i,k), pmp_1, lac_1) )
                  pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                  lac_2 = pmp_2 - 1.5*gam(i,k-1)
              a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),  &
                                             max(a4(1,i,k), pmp_2, lac_2) )
              a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
            endif
          endif
       enddo
     elseif ( abs(kord)==10 ) then
       do i=i1,i2
          if( extm(i,k) ) then
              if( a4(1,i,k)<qmin .or. extm(i,k-1) .or. extm(i,k+1) ) then
! grid-scale 2-delta-z wave detected; or q is too small -> ehance vertical mixing
                   a4(2,i,k) = a4(1,i,k)
                   a4(3,i,k) = a4(1,i,k)
                   a4(4,i,k) = 0.
              else
! True local extremum
                a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
              endif
          else        ! not a local extremum
            a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
! Check within the smooth region if subgrid profile is non-monotonic
            if( abs(a4(4,i,k)) > abs(a4(2,i,k)-a4(3,i,k)) ) then
                  pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                  lac_1 = pmp_1 + 1.5*gam(i,k+2)
              a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),  &
                                             max(a4(1,i,k), pmp_1, lac_1) )
                  pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                  lac_2 = pmp_2 - 1.5*gam(i,k-1)
              a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),  &
                                             max(a4(1,i,k), pmp_2, lac_2) )
              a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
            endif
          endif
       enddo
     elseif ( abs(kord)==12 ) then
       do i=i1,i2
          if( extm(i,k) ) then
              a4(2,i,k) = a4(1,i,k)
              a4(3,i,k) = a4(1,i,k)
              a4(4,i,k) = 0.
          else        ! not a local extremum
            a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
! Check within the smooth region if subgrid profile is non-monotonic
            if( abs(a4(4,i,k)) > abs(a4(2,i,k)-a4(3,i,k)) ) then
                  pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                  lac_1 = pmp_1 + 1.5*gam(i,k+2)
              a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),  &
                                             max(a4(1,i,k), pmp_1, lac_1) )
                  pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                  lac_2 = pmp_2 - 1.5*gam(i,k-1)
              a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),  &
                                             max(a4(1,i,k), pmp_2, lac_2) )
              a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
            endif
          endif
       enddo
     elseif ( abs(kord)==13 ) then
       do i=i1,i2
          if( extm(i,k) ) then
             if ( extm(i,k-1) .and. extm(i,k+1) ) then
! grid-scale 2-delta-z wave detected
                 a4(2,i,k) = a4(1,i,k)
                 a4(3,i,k) = a4(1,i,k)
                 a4(4,i,k) = 0.
             else
                 ! Left  edges
                 pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                 lac_1 = pmp_1 + 1.5*gam(i,k+2)
                 a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),   &
                                     max(a4(1,i,k), pmp_1, lac_1) )
                 ! Right edges
                 pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                 lac_2 = pmp_2 - 1.5*gam(i,k-1)
                 a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),    &
                                     max(a4(1,i,k), pmp_2, lac_2) )
                 a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
             endif
          else
             a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
          endif
       enddo
     elseif ( abs(kord)==14 ) then
       do i=i1,i2
          a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
       enddo
     else      ! kord = 11, 12, 13
       do i=i1,i2
         if ( extm(i,k) .and. (extm(i,k-1).or.extm(i,k+1).or.a4(1,i,k)<qmin) ) then
! Noisy region:
              a4(2,i,k) = a4(1,i,k)
              a4(3,i,k) = a4(1,i,k)
              a4(4,i,k) = 0.
         else
              a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         endif
       enddo
     endif

! Additional constraint to ensure positivity
     if ( iv==0 ) call cs_limiters(im, extm(i1,k), a4(1,i1,k), 0)

  enddo      ! k-loop

!----------------------------------
! Bottom layer subgrid constraints:
!----------------------------------
  if ( iv==0 ) then
     do i=i1,i2
        a4(3,i,km) = max(0., a4(3,i,km))
     enddo
  elseif ( iv .eq. -1 ) then 
      do i=i1,i2
         if ( a4(3,i,km)*a4(1,i,km) <= 0. )  a4(3,i,km) = 0.
      enddo
  endif

  do k=km-1,km
     do i=i1,i2
        a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
     enddo
     if(k==(km-1)) call cs_limiters(im, extm(i1,k), a4(1,i1,k), 2)
     if(k== km   ) call cs_limiters(im, extm(i1,k), a4(1,i1,k), 1)
  enddo

 end subroutine scalar_profile


 subroutine cs_profile(qs, a4, delp, km, i1, i2, iv, kord)
! Optimized vertical profile reconstruction:
! Latest: Apr 2008 S.-J. Lin, NOAA/GFDL
 integer, intent(in):: i1, i2
 integer, intent(in):: km      ! vertical dimension
 integer, intent(in):: iv      ! iv =-1: winds
                               ! iv = 0: positive definite scalars
                               ! iv = 1: others
 integer, intent(in):: kord
 real, intent(in)   ::   qs(i1:i2)
 real, intent(in)   :: delp(i1:i2,km)     ! layer pressure thickness
 real, intent(inout):: a4(4,i1:i2,km)     ! Interpolated values
!-----------------------------------------------------------------------
 logical:: extm(i1:i2,km) 
 real  gam(i1:i2,km)
 real    q(i1:i2,km+1)
 real   d4(i1:i2)
 real   bet, a_bot, grat 
 real   pmp_1, lac_1, pmp_2, lac_2
 integer i, k, im

 if ( iv .eq. -2 ) then
      do i=i1,i2
         gam(i,2) = 0.5
           q(i,1) = 1.5*a4(1,i,1)
      enddo
      do k=2,km-1
         do i=i1, i2
                  grat = delp(i,k-1) / delp(i,k)
                   bet =  2. + grat + grat - gam(i,k)
                q(i,k) = (3.*(a4(1,i,k-1)+a4(1,i,k)) - q(i,k-1))/bet
            gam(i,k+1) = grat / bet
         enddo
      enddo
      do i=i1,i2
            grat = delp(i,km-1) / delp(i,km) 
         q(i,km) = (3.*(a4(1,i,km-1)+a4(1,i,km)) - grat*qs(i) - q(i,km-1)) /  &
                   (2. + grat + grat - gam(i,km))
         q(i,km+1) = qs(i)
      enddo
      do k=km-1,1,-1
        do i=i1,i2
           q(i,k) = q(i,k) - gam(i,k+1)*q(i,k+1)
        enddo
      enddo
 else
  do i=i1,i2
         grat = delp(i,2) / delp(i,1)   ! grid ratio
          bet = grat*(grat+0.5)
       q(i,1) = ( (grat+grat)*(grat+1.)*a4(1,i,1) + a4(1,i,2) ) / bet
     gam(i,1) = ( 1. + grat*(grat+1.5) ) / bet
  enddo

  do k=2,km
     do i=i1,i2
           d4(i) = delp(i,k-1) / delp(i,k)
             bet =  2. + d4(i) + d4(i) - gam(i,k-1)
          q(i,k) = ( 3.*(a4(1,i,k-1)+d4(i)*a4(1,i,k)) - q(i,k-1) )/bet
        gam(i,k) = d4(i) / bet
     enddo
  enddo
 
  do i=i1,i2
         a_bot = 1. + d4(i)*(d4(i)+1.5)
     q(i,km+1) = (2.*d4(i)*(d4(i)+1.)*a4(1,i,km)+a4(1,i,km-1)-a_bot*q(i,km))  &
               / ( d4(i)*(d4(i)+0.5) - a_bot*gam(i,km) )
  enddo

  do k=km,1,-1
     do i=i1,i2
        q(i,k) = q(i,k) - gam(i,k)*q(i,k+1)
     enddo
  enddo
 endif
!----- Perfectly linear scheme --------------------------------
 if ( abs(kord) > 16 ) then
  do k=1,km
     do i=i1,i2
        a4(2,i,k) = q(i,k  )
        a4(3,i,k) = q(i,k+1)
        a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
     enddo
  enddo
  return
 endif
!----- Perfectly linear scheme --------------------------------

!------------------
! Apply constraints
!------------------
  im = i2 - i1 + 1

! Apply *large-scale* constraints 
  do i=i1,i2
     q(i,2) = min( q(i,2), max(a4(1,i,1), a4(1,i,2)) )
     q(i,2) = max( q(i,2), min(a4(1,i,1), a4(1,i,2)) )
  enddo

  do k=2,km
     do i=i1,i2
        gam(i,k) = a4(1,i,k) - a4(1,i,k-1)
     enddo
  enddo

! Interior:
  do k=3,km-1
     do i=i1,i2
        if ( gam(i,k-1)*gam(i,k+1)>0. ) then
! Apply large-scale constraint to ALL fields if not local max/min
             q(i,k) = min( q(i,k), max(a4(1,i,k-1),a4(1,i,k)) )
             q(i,k) = max( q(i,k), min(a4(1,i,k-1),a4(1,i,k)) )
        else
          if ( gam(i,k-1) > 0. ) then
! There exists a local max
               q(i,k) = max(q(i,k), min(a4(1,i,k-1),a4(1,i,k)))
          else
! There exists a local min
                 q(i,k) = min(q(i,k), max(a4(1,i,k-1),a4(1,i,k)))
               if ( iv==0 ) q(i,k) = max(0., q(i,k))
          endif
        endif
     enddo
  enddo

! Bottom:
  do i=i1,i2
     q(i,km) = min( q(i,km), max(a4(1,i,km-1), a4(1,i,km)) )
     q(i,km) = max( q(i,km), min(a4(1,i,km-1), a4(1,i,km)) )
  enddo

  do k=1,km
     do i=i1,i2
        a4(2,i,k) = q(i,k  )
        a4(3,i,k) = q(i,k+1)
     enddo
  enddo

  do k=2,km-1
     do i=i1,i2
        if ( gam(i,k)*gam(i,k+1) > 0.0 ) then
             extm(i,k) = .false. 
        else
             extm(i,k) = .true.
        endif
     enddo
  enddo

!---------------------------
! Apply subgrid constraints:
!---------------------------
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
! Top 2 and bottom 2 layers always use monotonic mapping

  if ( iv==0 ) then
     do i=i1,i2
        a4(2,i,1) = max(0., a4(2,i,1))
     enddo
  elseif ( iv==-1 ) then 
      do i=i1,i2
         if ( a4(2,i,1)*a4(1,i,1) <= 0. ) a4(2,i,1) = 0.
      enddo
  elseif ( iv==2 ) then
     do i=i1,i2
        a4(2,i,1) = a4(1,i,1)
        a4(3,i,1) = a4(1,i,1)
        a4(4,i,1) = 0.
     enddo
  endif

  if ( iv/=2 ) then
     do i=i1,i2
        a4(4,i,1) = 3.*(2.*a4(1,i,1) - (a4(2,i,1)+a4(3,i,1)))
     enddo
     call cs_limiters(im, extm(i1,1), a4(1,i1,1), 1)
  endif

! k=2
   do i=i1,i2
      a4(4,i,2) = 3.*(2.*a4(1,i,2) - (a4(2,i,2)+a4(3,i,2)))
   enddo
   call cs_limiters(im, extm(i1,2), a4(1,i1,2), 2)

!-------------------------------------
! Huynh's 2nd constraint for interior:
!-------------------------------------
  do k=3,km-2
     if ( abs(kord)<9 ) then
       do i=i1,i2
! Left  edges
          pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
          lac_1 = pmp_1 + 1.5*gam(i,k+2)
          a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),   &
                                         max(a4(1,i,k), pmp_1, lac_1) )
! Right edges
          pmp_2 = a4(1,i,k) + 2.*gam(i,k)
          lac_2 = pmp_2 - 1.5*gam(i,k-1)
          a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),    &
                                         max(a4(1,i,k), pmp_2, lac_2) )

          a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
       enddo

     elseif ( abs(kord)==9 ) then
       do i=i1,i2
          if ( extm(i,k) .and. (extm(i,k-1).or.extm(i,k+1)) ) then  ! c90_mp122
! grid-scale 2-delta-z wave detected
               a4(2,i,k) = a4(1,i,k)
               a4(3,i,k) = a4(1,i,k)
               a4(4,i,k) = 0.
          else
            a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
! Check within the smooth region if subgrid profile is non-monotonic
            if( abs(a4(4,i,k)) > abs(a4(2,i,k)-a4(3,i,k)) ) then
                  pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                  lac_1 = pmp_1 + 1.5*gam(i,k+2)
              a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),  &
                                             max(a4(1,i,k), pmp_1, lac_1) )
                  pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                  lac_2 = pmp_2 - 1.5*gam(i,k-1)
              a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),  &
                                             max(a4(1,i,k), pmp_2, lac_2) )
              a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
            endif
          endif
       enddo
     elseif ( abs(kord)==10 ) then
       do i=i1,i2
          if( extm(i,k) ) then
              if( extm(i,k-1) .or. extm(i,k+1) ) then
! grid-scale 2-delta-z wave detected
                   a4(2,i,k) = a4(1,i,k)
                   a4(3,i,k) = a4(1,i,k)
                   a4(4,i,k) = 0.
              else
! True local extremum
                a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
              endif
          else        ! not a local extremum
            a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
! Check within the smooth region if subgrid profile is non-monotonic
            if( abs(a4(4,i,k)) > abs(a4(2,i,k)-a4(3,i,k)) ) then
                  pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                  lac_1 = pmp_1 + 1.5*gam(i,k+2)
              a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),  &
                                             max(a4(1,i,k), pmp_1, lac_1) )
                  pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                  lac_2 = pmp_2 - 1.5*gam(i,k-1)
              a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),  &
                                             max(a4(1,i,k), pmp_2, lac_2) )
              a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
            endif
          endif
       enddo
     elseif ( abs(kord)==12 ) then
       do i=i1,i2
          if( extm(i,k) ) then
! grid-scale 2-delta-z wave detected
              a4(2,i,k) = a4(1,i,k)
              a4(3,i,k) = a4(1,i,k)
              a4(4,i,k) = 0.
          else        ! not a local extremum
            a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
! Check within the smooth region if subgrid profile is non-monotonic
            if( abs(a4(4,i,k)) > abs(a4(2,i,k)-a4(3,i,k)) ) then
                  pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                  lac_1 = pmp_1 + 1.5*gam(i,k+2)
              a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),  &
                                             max(a4(1,i,k), pmp_1, lac_1) )
                  pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                  lac_2 = pmp_2 - 1.5*gam(i,k-1)
              a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),  &
                                             max(a4(1,i,k), pmp_2, lac_2) )
              a4(4,i,k) = 6.*a4(1,i,k) - 3.*(a4(2,i,k)+a4(3,i,k))
            endif
          endif
       enddo
     elseif ( abs(kord)==13 ) then
       do i=i1,i2
          if( extm(i,k) ) then
             if ( extm(i,k-1) .and. extm(i,k+1) ) then
! grid-scale 2-delta-z wave detected
                 a4(2,i,k) = a4(1,i,k)
                 a4(3,i,k) = a4(1,i,k)
                 a4(4,i,k) = 0.
             else
                 ! Left  edges
                 pmp_1 = a4(1,i,k) - 2.*gam(i,k+1)
                 lac_1 = pmp_1 + 1.5*gam(i,k+2)
                 a4(2,i,k) = min(max(a4(2,i,k), min(a4(1,i,k), pmp_1, lac_1)),   &
                                     max(a4(1,i,k), pmp_1, lac_1) )
                 ! Right edges
                 pmp_2 = a4(1,i,k) + 2.*gam(i,k)
                 lac_2 = pmp_2 - 1.5*gam(i,k-1)
                 a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), pmp_2, lac_2)),    &
                                     max(a4(1,i,k), pmp_2, lac_2) )
                 a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
             endif
          else
             a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
          endif
       enddo
     elseif ( abs(kord)==14 ) then
       do i=i1,i2
          a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
       enddo
     else      ! kord = 11, 12, 13
       do i=i1,i2
         if ( extm(i,k) .and. (extm(i,k-1) .or. extm(i,k+1)) ) then
! Noisy region:
              a4(2,i,k) = a4(1,i,k)
              a4(3,i,k) = a4(1,i,k)
              a4(4,i,k) = 0.
         else
              a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         endif
       enddo
     endif

! Additional constraint to ensure positivity
     if ( iv==0 ) call cs_limiters(im, extm(i1,k), a4(1,i1,k), 0)

  enddo      ! k-loop

!----------------------------------
! Bottom layer subgrid constraints:
!----------------------------------
  if ( iv==0 ) then
     do i=i1,i2
        a4(3,i,km) = max(0., a4(3,i,km))
     enddo
  elseif ( iv .eq. -1 ) then 
      do i=i1,i2
         if ( a4(3,i,km)*a4(1,i,km) <= 0. )  a4(3,i,km) = 0.
      enddo
  endif

  do k=km-1,km
     do i=i1,i2
        a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
     enddo
     if(k==(km-1)) call cs_limiters(im, extm(i1,k), a4(1,i1,k), 2)
     if(k== km   ) call cs_limiters(im, extm(i1,k), a4(1,i1,k), 1)
  enddo

 end subroutine cs_profile


 subroutine cs_limiters(im, extm, a4, iv)
 integer, intent(in) :: im
 integer, intent(in) :: iv
 logical, intent(in) :: extm(im)
 real , intent(inout) :: a4(4,im)   ! PPM array
! !LOCAL VARIABLES:
 real  da1, da2, a6da
 integer i

 if ( iv==0 ) then
! Positive definite constraint
    do i=1,im
    if( a4(1,i)<=0.) then
        a4(2,i) = a4(1,i)
        a4(3,i) = a4(1,i)
        a4(4,i) = 0.
    else
      if( abs(a4(3,i)-a4(2,i)) < -a4(4,i) ) then
         if( (a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12) < 0. ) then
! local minimum is negative
             if( a4(1,i)<a4(3,i) .and. a4(1,i)<a4(2,i) ) then
                 a4(3,i) = a4(1,i)
                 a4(2,i) = a4(1,i)
                 a4(4,i) = 0.
             elseif( a4(3,i) > a4(2,i) ) then
                 a4(4,i) = 3.*(a4(2,i)-a4(1,i))
                 a4(3,i) = a4(2,i) - a4(4,i)
             else
                 a4(4,i) = 3.*(a4(3,i)-a4(1,i))
                 a4(2,i) = a4(3,i) - a4(4,i)
             endif
         endif
      endif
    endif
    enddo
 elseif ( iv==1 ) then
    do i=1,im
      if( (a4(1,i)-a4(2,i))*(a4(1,i)-a4(3,i))>=0. ) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da < -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da > da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
    enddo
 else
! Standard PPM constraint
    do i=1,im
      if( extm(i) ) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da < -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da > da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
    enddo
 endif
 end subroutine cs_limiters



 subroutine ppm_profile(a4, delp, km, i1, i2, iv, kord)

! !INPUT PARAMETERS:
 integer, intent(in):: iv      ! iv =-1: winds
                               ! iv = 0: positive definite scalars
                               ! iv = 1: others
                               ! iv = 2: temp (if remap_t) and w (iv=-2)
 integer, intent(in):: i1      ! Starting longitude
 integer, intent(in):: i2      ! Finishing longitude
 integer, intent(in):: km      ! vertical dimension
 integer, intent(in):: kord    ! Order (or more accurately method no.):
                               ! 
 real , intent(in):: delp(i1:i2,km)     ! layer pressure thickness

! !INPUT/OUTPUT PARAMETERS:
 real , intent(inout):: a4(4,i1:i2,km)  ! Interpolated values

! DESCRIPTION:
!
!   Perform the piecewise parabolic reconstruction
! 
! !REVISION HISTORY: 
! S.-J. Lin   revised at GFDL 2007
!-----------------------------------------------------------------------
! local arrays:
      real    dc(i1:i2,km)
      real    h2(i1:i2,km)
      real  delq(i1:i2,km)
      real   df2(i1:i2,km)
      real    d4(i1:i2,km)

! local scalars:
      integer i, k, km1, lmt, it
      real  fac
      real  a1, a2, c1, c2, c3, d1, d2
      real  qm, dq, lac, qmp, pmp

      km1 = km - 1
       it = i2 - i1 + 1

      do k=2,km
         do i=i1,i2
            delq(i,k-1) =   a4(1,i,k) - a4(1,i,k-1)
              d4(i,k  ) = delp(i,k-1) + delp(i,k)
         enddo
      enddo

      do k=2,km1
         do i=i1,i2
                 c1  = (delp(i,k-1)+0.5*delp(i,k))/d4(i,k+1)
                 c2  = (delp(i,k+1)+0.5*delp(i,k))/d4(i,k)
            df2(i,k) = delp(i,k)*(c1*delq(i,k) + c2*delq(i,k-1)) /      &
                                    (d4(i,k)+delp(i,k+1))
            dc(i,k) = sign( min(abs(df2(i,k)),              &
                            max(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))-a4(1,i,k),  &
                  a4(1,i,k)-min(a4(1,i,k-1),a4(1,i,k),a4(1,i,k+1))), df2(i,k) )
         enddo
      enddo

!-----------------------------------------------------------
! 4th order interpolation of the provisional cell edge value
!-----------------------------------------------------------

      do k=3,km1
         do i=i1,i2
            c1 = delq(i,k-1)*delp(i,k-1) / d4(i,k)
            a1 = d4(i,k-1) / (d4(i,k) + delp(i,k-1))
            a2 = d4(i,k+1) / (d4(i,k) + delp(i,k))
            a4(2,i,k) = a4(1,i,k-1) + c1 + 2./(d4(i,k-1)+d4(i,k+1)) *    &
                      ( delp(i,k)*(c1*(a1 - a2)+a2*dc(i,k-1)) -          &
                        delp(i,k-1)*a1*dc(i,k  ) )
         enddo
      enddo

!     if(km>8 .and. kord>4) call steepz(i1, i2, km, a4, df2, dc, delq, delp, d4)

! Area preserving cubic with 2nd deriv. = 0 at the boundaries
! Top
      do i=i1,i2
         d1 = delp(i,1)
         d2 = delp(i,2)
         qm = (d2*a4(1,i,1)+d1*a4(1,i,2)) / (d1+d2)
         dq = 2.*(a4(1,i,2)-a4(1,i,1)) / (d1+d2)
         c1 = 4.*(a4(2,i,3)-qm-d2*dq) / ( d2*(2.*d2*d2+d1*(d2+3.*d1)) )
         c3 = dq - 0.5*c1*(d2*(5.*d1+d2)-3.*d1*d1)
         a4(2,i,2) = qm - 0.25*c1*d1*d2*(d2+3.*d1)
! Top edge:
!-------------------------------------------------------
         a4(2,i,1) = d1*(2.*c1*d1**2-c3) + a4(2,i,2)
!-------------------------------------------------------
!        a4(2,i,1) = (12./7.)*a4(1,i,1)-(13./14.)*a4(1,i,2)+(3./14.)*a4(1,i,3)
!-------------------------------------------------------
! No over- and undershoot condition
         a4(2,i,2) = max( a4(2,i,2), min(a4(1,i,1), a4(1,i,2)) )
         a4(2,i,2) = min( a4(2,i,2), max(a4(1,i,1), a4(1,i,2)) )
         dc(i,1) =  0.5*(a4(2,i,2) - a4(1,i,1))
      enddo

! Enforce monotonicity  within the top layer

      if( iv==0 ) then
         do i=i1,i2
            a4(2,i,1) = max(0., a4(2,i,1))
            a4(2,i,2) = max(0., a4(2,i,2))
         enddo 
      elseif( iv==-1 ) then
         do i=i1,i2
            if ( a4(2,i,1)*a4(1,i,1) <= 0. ) a4(2,i,1) = 0.
         enddo
      elseif( abs(iv)==2 ) then
         do i=i1,i2
            a4(2,i,1) = a4(1,i,1)
            a4(3,i,1) = a4(1,i,1)
         enddo
      endif

! Bottom
! Area preserving cubic with 2nd deriv. = 0 at the surface
      do i=i1,i2
         d1 = delp(i,km)
         d2 = delp(i,km1)
         qm = (d2*a4(1,i,km)+d1*a4(1,i,km1)) / (d1+d2)
         dq = 2.*(a4(1,i,km1)-a4(1,i,km)) / (d1+d2)
         c1 = (a4(2,i,km1)-qm-d2*dq) / (d2*(2.*d2*d2+d1*(d2+3.*d1)))
         c3 = dq - 2.0*c1*(d2*(5.*d1+d2)-3.*d1*d1)
         a4(2,i,km) = qm - c1*d1*d2*(d2+3.*d1)
! Bottom edge:
!-----------------------------------------------------
         a4(3,i,km) = d1*(8.*c1*d1**2-c3) + a4(2,i,km)
!        dc(i,km) = 0.5*(a4(3,i,km) - a4(1,i,km))
!-----------------------------------------------------
!        a4(3,i,km) = (12./7.)*a4(1,i,km)-(13./14.)*a4(1,i,km-1)+(3./14.)*a4(1,i,km-2)
! No over- and under-shoot condition
         a4(2,i,km) = max( a4(2,i,km), min(a4(1,i,km), a4(1,i,km1)) )
         a4(2,i,km) = min( a4(2,i,km), max(a4(1,i,km), a4(1,i,km1)) )
         dc(i,km) = 0.5*(a4(1,i,km) - a4(2,i,km))
      enddo


! Enforce constraint on the "slope" at the surface

#ifdef BOT_MONO
      do i=i1,i2
         a4(4,i,km) = 0
         if( a4(3,i,km) * a4(1,i,km) <= 0. ) a4(3,i,km) = 0.
         d1 = a4(1,i,km) - a4(2,i,km)
         d2 = a4(3,i,km) - a4(1,i,km)
         if ( d1*d2 < 0. ) then
              a4(2,i,km) = a4(1,i,km)
              a4(3,i,km) = a4(1,i,km)
         else
              dq = sign(min(abs(d1),abs(d2),0.5*abs(delq(i,km-1))), d1)
              a4(2,i,km) = a4(1,i,km) - dq
              a4(3,i,km) = a4(1,i,km) + dq
         endif
      enddo
#else
      if( iv==0 ) then
          do i=i1,i2
             a4(2,i,km) = max(0.,a4(2,i,km))
             a4(3,i,km) = max(0.,a4(3,i,km))
          enddo
      elseif( iv<0 ) then
          do i=i1,i2
             if( a4(1,i,km)*a4(3,i,km) <= 0. )  a4(3,i,km) = 0.
          enddo
      endif
#endif

   do k=1,km1
      do i=i1,i2
         a4(3,i,k) = a4(2,i,k+1)
      enddo
   enddo

!-----------------------------------------------------------
! f(s) = AL + s*[(AR-AL) + A6*(1-s)]         ( 0 <= s  <= 1 )
!-----------------------------------------------------------
! Top 2 and bottom 2 layers always use monotonic mapping
      do k=1,2
         do i=i1,i2
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call ppm_limiters(dc(i1,k), a4(1,i1,k), it, 0)
      enddo

      if(kord >= 7) then
!-----------------------
! Huynh's 2nd constraint
!-----------------------
      do k=2,km1
         do i=i1,i2
! Method#1
!           h2(i,k) = delq(i,k) - delq(i,k-1)
! Method#2 - better
            h2(i,k) = 2.*(dc(i,k+1)/delp(i,k+1) - dc(i,k-1)/delp(i,k-1))  &
                     / ( delp(i,k)+0.5*(delp(i,k-1)+delp(i,k+1)) )        &
                     * delp(i,k)**2 
! Method#3
!!!            h2(i,k) = dc(i,k+1) - dc(i,k-1)
         enddo
      enddo

      fac = 1.5           ! original quasi-monotone

      do k=3,km-2
        do i=i1,i2
! Right edges
!        qmp   = a4(1,i,k) + 2.0*delq(i,k-1)
!        lac   = a4(1,i,k) + fac*h2(i,k-1) + 0.5*delq(i,k-1)
!
         pmp   = 2.*dc(i,k)
         qmp   = a4(1,i,k) + pmp
         lac   = a4(1,i,k) + fac*h2(i,k-1) + dc(i,k)
         a4(3,i,k) = min(max(a4(3,i,k), min(a4(1,i,k), qmp, lac)),    &
                                        max(a4(1,i,k), qmp, lac) )
! Left  edges
!        qmp   = a4(1,i,k) - 2.0*delq(i,k)
!        lac   = a4(1,i,k) + fac*h2(i,k+1) - 0.5*delq(i,k)
!
         qmp   = a4(1,i,k) - pmp
         lac   = a4(1,i,k) + fac*h2(i,k+1) - dc(i,k)
         a4(2,i,k) = min(max(a4(2,i,k),  min(a4(1,i,k), qmp, lac)),   &
                     max(a4(1,i,k), qmp, lac))
!-------------
! Recompute A6
!-------------
         a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
        enddo
! Additional constraint to ensure positivity when kord=7
         if (iv == 0 .and. kord >= 6 )                      &
             call ppm_limiters(dc(i1,k), a4(1,i1,k), it, 2)
      enddo

      else

         lmt = kord - 3
         lmt = max(0, lmt)
         if (iv == 0) lmt = min(2, lmt)

         do k=3,km-2
            if( kord /= 4) then
              do i=i1,i2
                 a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
              enddo
            endif
            if(kord/=6) call ppm_limiters(dc(i1,k), a4(1,i1,k), it, lmt)
         enddo
      endif

      do k=km1,km
         do i=i1,i2
            a4(4,i,k) = 3.*(2.*a4(1,i,k) - (a4(2,i,k)+a4(3,i,k)))
         enddo
         call ppm_limiters(dc(i1,k), a4(1,i1,k), it, 0)
      enddo

 end subroutine ppm_profile


 subroutine ppm_limiters(dm, a4, itot, lmt)

! !INPUT PARAMETERS:
      real , intent(in):: dm(*)     ! the linear slope
      integer, intent(in) :: itot      ! Total Longitudes
      integer, intent(in) :: lmt       ! 0: Standard PPM constraint
                                       ! 1: Improved full monotonicity constraint (Lin)
                                       ! 2: Positive definite constraint
                                       ! 3: do nothing (return immediately)
! !INPUT/OUTPUT PARAMETERS:
      real , intent(inout) :: a4(4,*)   ! PPM array
                                           ! AA <-- a4(1,i)
                                           ! AL <-- a4(2,i)
                                           ! AR <-- a4(3,i)
                                           ! A6 <-- a4(4,i)
! !LOCAL VARIABLES:
      real  qmp
      real  da1, da2, a6da
      real  fmin
      integer i

! Developer: S.-J. Lin

      if ( lmt == 3 ) return

      if(lmt == 0) then
! Standard PPM constraint
      do i=1,itot
      if(dm(i) == 0.) then
         a4(2,i) = a4(1,i)
         a4(3,i) = a4(1,i)
         a4(4,i) = 0.
      else
         da1  = a4(3,i) - a4(2,i)
         da2  = da1**2
         a6da = a4(4,i)*da1
         if(a6da < -da2) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         elseif(a6da > da2) then
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
      endif
      enddo

      elseif (lmt == 1) then

! Improved full monotonicity constraint (Lin 2004)
! Note: no need to provide first guess of A6 <-- a4(4,i)
      do i=1, itot
           qmp = 2.*dm(i)
         a4(2,i) = a4(1,i)-sign(min(abs(qmp),abs(a4(2,i)-a4(1,i))), qmp)
         a4(3,i) = a4(1,i)+sign(min(abs(qmp),abs(a4(3,i)-a4(1,i))), qmp)
         a4(4,i) = 3.*( 2.*a4(1,i) - (a4(2,i)+a4(3,i)) )
      enddo

      elseif (lmt == 2) then

! Positive definite constraint
      do i=1,itot
      if( abs(a4(3,i)-a4(2,i)) < -a4(4,i) ) then
      fmin = a4(1,i)+0.25*(a4(3,i)-a4(2,i))**2/a4(4,i)+a4(4,i)*r12
         if( fmin < 0. ) then
         if(a4(1,i)<a4(3,i) .and. a4(1,i)<a4(2,i)) then
            a4(3,i) = a4(1,i)
            a4(2,i) = a4(1,i)
            a4(4,i) = 0.
         elseif(a4(3,i) > a4(2,i)) then
            a4(4,i) = 3.*(a4(2,i)-a4(1,i))
            a4(3,i) = a4(2,i) - a4(4,i)
         else
            a4(4,i) = 3.*(a4(3,i)-a4(1,i))
            a4(2,i) = a4(3,i) - a4(4,i)
         endif
         endif
      endif
      enddo

      endif

 end subroutine ppm_limiters



 subroutine steepz(i1, i2, km, a4, df2, dm, dq, dp, d4)
 integer, intent(in) :: km, i1, i2
   real , intent(in) ::  dp(i1:i2,km)       ! grid size
   real , intent(in) ::  dq(i1:i2,km)       ! backward diff of q
   real , intent(in) ::  d4(i1:i2,km)       ! backward sum:  dp(k)+ dp(k-1) 
   real , intent(in) :: df2(i1:i2,km)       ! first guess mismatch
   real , intent(in) ::  dm(i1:i2,km)       ! monotonic mismatch
! !INPUT/OUTPUT PARAMETERS:
      real , intent(inout) ::  a4(4,i1:i2,km)  ! first guess/steepened
! !LOCAL VARIABLES:
      integer i, k
      real  alfa(i1:i2,km)
      real     f(i1:i2,km)
      real   rat(i1:i2,km)
      real   dg2

! Compute ratio of dq/dp
      do k=2,km
         do i=i1,i2
            rat(i,k) = dq(i,k-1) / d4(i,k)
         enddo
      enddo

! Compute F
      do k=2,km-1
         do i=i1,i2
            f(i,k) =   (rat(i,k+1) - rat(i,k))                          &
                     / ( dp(i,k-1)+dp(i,k)+dp(i,k+1) )
         enddo
      enddo

      do k=3,km-2
         do i=i1,i2
         if(f(i,k+1)*f(i,k-1)<0. .and. df2(i,k)/=0.) then
            dg2 = (f(i,k+1)-f(i,k-1))*((dp(i,k+1)-dp(i,k-1))**2          &
                   + d4(i,k)*d4(i,k+1) )
            alfa(i,k) = max(0., min(0.5, -0.1875*dg2/df2(i,k)))
         else
            alfa(i,k) = 0.
         endif
         enddo
      enddo

      do k=4,km-2
         do i=i1,i2
            a4(2,i,k) = (1.-alfa(i,k-1)-alfa(i,k)) * a4(2,i,k) +         &
                        alfa(i,k-1)*(a4(1,i,k)-dm(i,k))    +             &
                        alfa(i,k)*(a4(1,i,k-1)+dm(i,k-1))
         enddo
      enddo

 end subroutine steepz



 subroutine rst_remap(km, kn, is,ie,js,je, isd,ied,jsd,jed, nq, ntp, &
                      delp_r, u_r, v_r, w_r, delz_r, pt_r, q_r, qdiag_r, &
                      delp,   u,   v,   w,   delz,   pt,   q,   qdiag,   &
                      ak_r, bk_r, ptop, ak, bk, hydrostatic, make_nh, &
                      domain, square_domain)
!------------------------------------
! Assuming hybrid sigma-P coordinate:
!------------------------------------
! !INPUT PARAMETERS:
  integer, intent(in):: km                    ! Restart z-dimension
  integer, intent(in):: kn                    ! Run time dimension
  integer, intent(in):: nq, ntp               ! number of tracers (including h2o)
  integer, intent(in):: is,ie,isd,ied         ! starting & ending X-Dir index
  integer, intent(in):: js,je,jsd,jed         ! starting & ending Y-Dir index
  logical, intent(in):: hydrostatic, make_nh, square_domain
  real, intent(IN) :: ptop
  real, intent(in) :: ak_r(km+1)
  real, intent(in) :: bk_r(km+1)
  real, intent(in) :: ak(kn+1)
  real, intent(in) :: bk(kn+1)
  real, intent(in):: delp_r(is:ie,js:je,km) ! pressure thickness
  real, intent(in)::   u_r(is:ie,  js:je+1,km)   ! u-wind (m/s)
  real, intent(in)::   v_r(is:ie+1,js:je  ,km)   ! v-wind (m/s)
  real, intent(inout)::  pt_r(is:ie,js:je,km)
  real, intent(in)::   w_r(is:ie,js:je,km)
  real, intent(in)::   q_r(is:ie,js:je,km,1:ntp)
  real, intent(in)::   qdiag_r(is:ie,js:je,km,ntp+1:nq)
  real, intent(inout)::delz_r(is:ie,js:je,km)
  type(domain2d), intent(INOUT) :: domain
! Output:
  real, intent(out):: delp(isd:ied,jsd:jed,kn) ! pressure thickness
  real, intent(out)::  u(isd:ied  ,jsd:jed+1,kn)   ! u-wind (m/s)
  real, intent(out)::  v(isd:ied+1,jsd:jed  ,kn)   ! v-wind (m/s)
  real, intent(out)::  w(isd:     ,jsd:     ,1:)   ! vertical velocity (m/s)
  real, intent(out):: pt(isd:ied  ,jsd:jed  ,kn)   ! temperature
  real, intent(out):: q(isd:ied,jsd:jed,kn,1:ntp)
  real, intent(out):: qdiag(isd:ied,jsd:jed,kn,ntp+1:nq)
  real, intent(out):: delz(isd:,jsd:,1:)   ! delta-height (m)
!-----------------------------------------------------------------------
  real r_vir, rgrav
  real ps(isd:ied,jsd:jed)  ! surface pressure
  real  pe1(is:ie,km+1)
  real  pe2(is:ie,kn+1)
  real  pv1(is:ie+1,km+1)
  real  pv2(is:ie+1,kn+1)

  integer i,j,k , iq
  integer, parameter:: kord=4

#ifdef HYDRO_DELZ_REMAP
  if (is_master() .and. .not. hydrostatic) then
     print*, ''
     print*, ' REMAPPING IC: INITIALIZING DELZ WITH HYDROSTATIC STATE  '
     print*, ''
  endif
#endif

#ifdef HYDRO_DELZ_EXTRAP
  if (is_master() .and. .not. hydrostatic) then
     print*, ''
     print*, ' REMAPPING IC: INITIALIZING DELZ WITH HYDROSTATIC STATE ABOVE INPUT MODEL TOP  '
     print*, ''
  endif
#endif

#ifdef ZERO_W_EXTRAP
  if (is_master() .and. .not. hydrostatic) then
     print*, ''
     print*, ' REMAPPING IC: INITIALIZING W TO ZERO ABOVE INPUT MODEL TOP  '
     print*, ''
  endif
#endif

  r_vir = rvgas/rdgas - 1.
  rgrav = 1./grav

!$OMP parallel do default(none) shared(is,ie,js,je,ps,ak_r)
  do j=js,je
     do i=is,ie
        ps(i,j) = ak_r(1)
     enddo
  enddo

! this OpenMP do-loop setup cannot work in it's current form....
!$OMP parallel do default(none) shared(is,ie,js,je,km,ps,delp_r)
  do j=js,je
     do k=1,km
        do i=is,ie
           ps(i,j) = ps(i,j) + delp_r(i,j,k)
        enddo
     enddo
  enddo

! only one cell is needed
  if ( square_domain ) then
      call mpp_update_domains(ps, domain,  whalo=1, ehalo=1, shalo=1, nhalo=1, complete=.true.)
  else
      call mpp_update_domains(ps, domain, complete=.true.)
  endif

! Compute virtual Temp
!$OMP parallel do default(none) shared(is,ie,js,je,km,pt_r,r_vir,q_r)
  do k=1,km
     do j=js,je
        do i=is,ie
           pt_r(i,j,k) = pt_r(i,j,k) * (1.+r_vir*q_r(i,j,k,1))
        enddo
     enddo
  enddo

!$OMP parallel do default(none) shared(is,ie,js,je,km,ak_r,bk_r,ps,kn,ak,bk,u_r,u,delp, &
!$OMP                                  ntp,nq,hydrostatic,make_nh,w_r,w,delz_r,delp_r,delz, &
!$OMP                                  pt_r,pt,v_r,v,q,q_r,qdiag,qdiag_r) &
!$OMP                          private(pe1,  pe2, pv1, pv2)
  do 1000 j=js,je+1
!------
! map u
!------
     do k=1,km+1
        do i=is,ie
           pe1(i,k) = ak_r(k) + 0.5*bk_r(k)*(ps(i,j-1)+ps(i,j))
        enddo
     enddo

     do k=1,kn+1
        do i=is,ie
           pe2(i,k) = ak(k) + 0.5*bk(k)*(ps(i,j-1)+ps(i,j))
        enddo
     enddo

     call remap_2d(km, pe1, u_r(is:ie,j:j,1:km),       &
                   kn, pe2,   u(is:ie,j:j,1:kn),       &
                   is, ie, -1, kord)

  if ( j /= (je+1) )  then 

!---------------
! Hybrid sigma-p
!---------------
     do k=1,km+1
        do i=is,ie
           pe1(i,k) = ak_r(k) + bk_r(k)*ps(i,j)
        enddo
     enddo

     do k=1,kn+1
        do i=is,ie
           pe2(i,k) =   ak(k) + bk(k)*ps(i,j)
        enddo
     enddo

!-------------
! Compute delp
!-------------
      do k=1,kn
         do i=is,ie
            delp(i,j,k) = pe2(i,k+1) - pe2(i,k)
         enddo
      enddo

!----------------
! Map constituents
!----------------
      if( nq /= 0 ) then
          do iq=1,ntp
             call remap_2d(km, pe1, q_r(is:ie,j:j,1:km,iq:iq),  &
                           kn, pe2,   q(is:ie,j:j,1:kn,iq:iq),  &
                           is, ie, 0, kord)
          enddo
          do iq=ntp+1,nq
             call remap_2d(km, pe1, qdiag_r(is:ie,j:j,1:km,iq:iq),  &
                           kn, pe2,   qdiag(is:ie,j:j,1:kn,iq:iq),  &
                           is, ie, 0, kord)
          enddo
      endif

      if ( .not. hydrostatic .and. .not. make_nh) then
! Remap vertical wind:
         call remap_2d(km, pe1, w_r(is:ie,j:j,1:km),       &
                       kn, pe2,   w(is:ie,j:j,1:kn),       &
                       is, ie, -1, kord)

#ifdef ZERO_W_EXTRAP
       do k=1,kn
       do i=is,ie
          if (pe2(i,k) < pe1(i,1)) then
             w(i,j,k) = 0.
          endif
       enddo
       enddo         
#endif

#ifndef HYDRO_DELZ_REMAP
! Remap delz for hybrid sigma-p coordinate
         do k=1,km
            do i=is,ie
               delz_r(i,j,k) = -delz_r(i,j,k)/delp_r(i,j,k) ! ="specific volume"/grav
            enddo
         enddo
         call remap_2d(km, pe1, delz_r(is:ie,j:j,1:km),       &
                       kn, pe2,   delz(is:ie,j:j,1:kn),       &
                       is, ie, 1, kord)
         do k=1,kn
            do i=is,ie
               delz(i,j,k) = -delz(i,j,k)*delp(i,j,k)
            enddo
         enddo
#endif
      endif

! Geopotential conserving remap of virtual temperature:
       do k=1,km+1
          do i=is,ie
             pe1(i,k) = log(pe1(i,k))
          enddo
       enddo
       do k=1,kn+1
          do i=is,ie
             pe2(i,k) = log(pe2(i,k))
          enddo
       enddo

       call remap_2d(km, pe1, pt_r(is:ie,j:j,1:km),       &
                     kn, pe2,   pt(is:ie,j:j,1:kn),       &
                     is, ie, 1, kord)

#ifdef HYDRO_DELZ_REMAP
       !initialize delz from the hydrostatic state
       do k=1,kn
       do i=is,ie
          delz(i,j,k) = (rdgas*rgrav)*pt(i,j,k)*(pe2(i,k)-pe2(i,k+1))
       enddo
       enddo
#endif
#ifdef HYDRO_DELZ_EXTRAP
       !initialize delz from the hydrostatic state
       do k=1,kn
       do i=is,ie
          if (pe2(i,k) < pe1(i,1)) then
             delz(i,j,k) = (rdgas*rgrav)*pt(i,j,k)*(pe2(i,k)-pe2(i,k+1))
          endif
       enddo
       enddo
#endif
!------
! map v
!------
       do k=1,km+1
          do i=is,ie+1
             pv1(i,k) = ak_r(k) + 0.5*bk_r(k)*(ps(i-1,j)+ps(i,j))
          enddo
       enddo
       do k=1,kn+1
          do i=is,ie+1
             pv2(i,k) = ak(k) + 0.5*bk(k)*(ps(i-1,j)+ps(i,j))
          enddo
       enddo

       call remap_2d(km, pv1, v_r(is:ie+1,j:j,1:km),       &
                     kn, pv2,   v(is:ie+1,j:j,1:kn),       &
                     is, ie+1, -1, kord)

  endif !(j < je+1)
1000  continue

!$OMP parallel do default(none) shared(is,ie,js,je,kn,pt,r_vir,q)
  do k=1,kn
     do j=js,je
        do i=is,ie
           pt(i,j,k) = pt(i,j,k) / (1.+r_vir*q(i,j,k,1))
        enddo
     enddo   
  enddo

 end subroutine rst_remap



 subroutine mappm(km, pe1, q1, kn, pe2, q2, i1, i2, iv, kord, ptop)

! IV = 0: constituents
! IV = 1: potential temp
! IV =-1: winds
 
! Mass flux preserving mapping: q1(im,km) -> q2(im,kn)
 
! pe1: pressure at layer edges (from model top to bottom surface)
!      in the original vertical coordinate
! pe2: pressure at layer edges (from model top to bottom surface)
!      in the new vertical coordinate

 integer, intent(in):: i1, i2, km, kn, kord, iv
 real, intent(in ):: pe1(i1:i2,km+1), pe2(i1:i2,kn+1)
 real, intent(in )::  q1(i1:i2,km)
 real, intent(out)::  q2(i1:i2,kn)
 real, intent(IN) :: ptop
! local
      real  qs(i1:i2)
      real dp1(i1:i2,km)
      real a4(4,i1:i2,km)
      integer i, k, l
      integer k0, k1
      real pl, pr, tt, delp, qsum, dpsum, esl

      do k=1,km
         do i=i1,i2
             dp1(i,k) = pe1(i,k+1) - pe1(i,k)
            a4(1,i,k) = q1(i,k)
         enddo
      enddo

      if ( kord >7 ) then
           call  cs_profile( qs, a4, dp1, km, i1, i2, iv, kord )
      else
           call ppm_profile( a4, dp1, km, i1, i2, iv, kord )
      endif

!------------------------------------
! Lowest layer: constant distribution
!------------------------------------
#ifdef NGGPS_SUBMITTED
      do i=i1,i2
         a4(2,i,km) = q1(i,km)
         a4(3,i,km) = q1(i,km)
         a4(4,i,km) = 0.
      enddo
#endif

      do 5555 i=i1,i2
         k0 = 1
      do 555 k=1,kn

         if(pe2(i,k) .le. pe1(i,1)) then
! above old ptop
            q2(i,k) = q1(i,1)
         elseif(pe2(i,k) .ge. pe1(i,km+1)) then
! Entire grid below old ps
#ifdef NGGPS_SUBMITTED
            q2(i,k) = a4(3,i,km)   ! this is not good.
#else
            q2(i,k) = q1(i,km)
#endif
         else

         do 45 L=k0,km
! locate the top edge at pe2(i,k)
         if( pe2(i,k) .ge. pe1(i,L) .and.        &
             pe2(i,k) .le. pe1(i,L+1)    ) then
             k0 = L
             PL = (pe2(i,k)-pe1(i,L)) / dp1(i,L)
             if(pe2(i,k+1) .le. pe1(i,L+1)) then

! entire new grid is within the original grid
               PR = (pe2(i,k+1)-pe1(i,L)) / dp1(i,L)
               TT = r3*(PR*(PR+PL)+PL**2)
               q2(i,k) = a4(2,i,L) + 0.5*(a4(4,i,L)+a4(3,i,L)  &
                       - a4(2,i,L))*(PR+PL) - a4(4,i,L)*TT
              goto 555
             else
! Fractional area...
              delp = pe1(i,L+1) - pe2(i,k)
              TT   = r3*(1.+PL*(1.+PL))
              qsum = delp*(a4(2,i,L)+0.5*(a4(4,i,L)+            &
                     a4(3,i,L)-a4(2,i,L))*(1.+PL)-a4(4,i,L)*TT)
              dpsum = delp
              k1 = L + 1
             goto 111
             endif
         endif
45       continue

111      continue
         do 55 L=k1,km
         if( pe2(i,k+1) .gt. pe1(i,L+1) ) then

! Whole layer..

            qsum  =  qsum + dp1(i,L)*q1(i,L)
            dpsum = dpsum + dp1(i,L)
         else
           delp = pe2(i,k+1)-pe1(i,L)
           esl  = delp / dp1(i,L)
           qsum = qsum + delp * (a4(2,i,L)+0.5*esl*            &
                 (a4(3,i,L)-a4(2,i,L)+a4(4,i,L)*(1.-r23*esl)) )
          dpsum = dpsum + delp
           k0 = L
           goto 123
         endif
55       continue
        delp = pe2(i,k+1) - pe1(i,km+1)
        if(delp > 0.) then
! Extended below old ps
#ifdef NGGPS_SUBMITTED
           qsum = qsum + delp * a4(3,i,km)    ! not good.
#else
           qsum = qsum + delp * q1(i,km)
#endif
          dpsum = dpsum + delp
        endif
123     q2(i,k) = qsum / dpsum
      endif
555   continue
5555  continue

 end subroutine mappm


 subroutine moist_cv(is,ie, isd,ied, jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                     ice_wat, snowwat, graupel, q, qd, cvm, t1)
  integer, intent(in):: is, ie, isd,ied, jsd,jed, km, nwat, j, k
  integer, intent(in):: sphum, liq_wat, rainwat, ice_wat, snowwat, graupel
  real, intent(in), dimension(isd:ied,jsd:jed,km,nwat):: q
  real, intent(out), dimension(is:ie):: cvm, qd
  real, intent(in), optional:: t1(is:ie)
!
  real, parameter:: tice = 273.16
  real, parameter:: t_i0 = 15.
  real, dimension(is:ie):: qv, ql, qs
  integer:: i

  select case (nwat)

   case(2)
     if ( present(t1) ) then  ! Special case for GFS physics
        do i=is,ie
           qd(i) = max(0., q(i,j,k,liq_wat))
           if ( t1(i) > tice ) then
                qs(i) = 0.
           elseif ( t1(i) < tice-t_i0 ) then
                qs(i) = qd(i)
           else
                qs(i) = qd(i)*(tice-t1(i))/t_i0
           endif
           ql(i) = qd(i) - qs(i)
           qv(i) = max(0.,q(i,j,k,sphum))
           cvm(i) = (1.-(qv(i)+qd(i)))*cv_air + qv(i)*cv_vap + ql(i)*c_liq + qs(i)*c_ice
        enddo
     else
        do i=is,ie
           qv(i) = max(0.,q(i,j,k,sphum))
           qs(i) = max(0.,q(i,j,k,liq_wat))
           qd(i) = qs(i)
           cvm(i) = (1.-qv(i))*cv_air + qv(i)*cv_vap
        enddo
     endif
  case (3)
     do i=is,ie
        qv(i) = q(i,j,k,sphum)
        ql(i) = q(i,j,k,liq_wat) 
        qs(i) = q(i,j,k,ice_wat)
        qd(i) = ql(i) + qs(i)
        cvm(i) = (1.-(qv(i)+qd(i)))*cv_air + qv(i)*cv_vap + ql(i)*c_liq + qs(i)*c_ice
     enddo
  case(4)              ! K_warm_rain with fake ice
     do i=is,ie 
        qv(i) = q(i,j,k,sphum)
        qd(i) = q(i,j,k,liq_wat) + q(i,j,k,rainwat)
        cvm(i) = (1.-(qv(i)+qd(i)))*cv_air + qv(i)*cv_vap + qd(i)*c_liq
     enddo

  case(6)
     do i=is,ie 
        qv(i) = q(i,j,k,sphum)
        ql(i) = q(i,j,k,liq_wat) + q(i,j,k,rainwat) 
        qs(i) = q(i,j,k,ice_wat) + q(i,j,k,snowwat) + q(i,j,k,graupel)
        qd(i) = ql(i) + qs(i)
        cvm(i) = (1.-(qv(i)+qd(i)))*cv_air + qv(i)*cv_vap + ql(i)*c_liq + qs(i)*c_ice
     enddo
  case default
     do i=is,ie 
         qd(i) = 0.
        cvm(i) = cv_air
     enddo
 end select

 end subroutine moist_cv

 subroutine moist_cp(is,ie, isd,ied, jsd,jed, km, j, k, nwat, sphum, liq_wat, rainwat,    &
                     ice_wat, snowwat, graupel, q, qd, cpm, t1)

  integer, intent(in):: is, ie, isd,ied, jsd,jed, km, nwat, j, k
  integer, intent(in):: sphum, liq_wat, rainwat, ice_wat, snowwat, graupel
  real, intent(in), dimension(isd:ied,jsd:jed,km,nwat):: q
  real, intent(out), dimension(is:ie):: cpm, qd
  real, intent(in), optional:: t1(is:ie)
!
  real, parameter:: tice = 273.16
  real, parameter:: t_i0 = 15.
  real, dimension(is:ie):: qv, ql, qs
  integer:: i

  select case (nwat)

  case(2)
     if ( present(t1) ) then  ! Special case for GFS physics
        do i=is,ie
           qd(i) = max(0., q(i,j,k,liq_wat))
           if ( t1(i) > tice ) then
                qs(i) = 0.
           elseif ( t1(i) < tice-t_i0 ) then
                qs(i) = qd(i)
           else
                qs(i) = qd(i)*(tice-t1(i))/t_i0
           endif
           ql(i) = qd(i) - qs(i)
           qv(i) = max(0.,q(i,j,k,sphum))
           cpm(i) = (1.-(qv(i)+qd(i)))*cp_air + qv(i)*cp_vapor + ql(i)*c_liq + qs(i)*c_ice
        enddo
     else
     do i=is,ie
        qv(i) = max(0.,q(i,j,k,sphum))
        qs(i) = max(0.,q(i,j,k,liq_wat))
        qd(i) = qs(i)
        cpm(i) = (1.-qv(i))*cp_air + qv(i)*cp_vapor
     enddo
     endif

  case(3)
     do i=is,ie
        qv(i) = q(i,j,k,sphum)
        ql(i) = q(i,j,k,liq_wat) 
        qs(i) = q(i,j,k,ice_wat)
        qd(i) = ql(i) + qs(i)
        cpm(i) = (1.-(qv(i)+qd(i)))*cp_air + qv(i)*cp_vapor + ql(i)*c_liq + qs(i)*c_ice
     enddo
  case(4)    ! K_warm_rain scheme with fake ice
     do i=is,ie
        qv(i) = q(i,j,k,sphum)
        qd(i) = q(i,j,k,liq_wat) + q(i,j,k,rainwat)
        cpm(i) = (1.-(qv(i)+qd(i)))*cp_air + qv(i)*cp_vapor + qd(i)*c_liq
     enddo

  case(6)
     do i=is,ie 
        qv(i) = q(i,j,k,sphum)
        ql(i) = q(i,j,k,liq_wat) + q(i,j,k,rainwat) 
        qs(i) = q(i,j,k,ice_wat) + q(i,j,k,snowwat) + q(i,j,k,graupel)
        qd(i) = ql(i) + qs(i)
        cpm(i) = (1.-(qv(i)+qd(i)))*cp_air + qv(i)*cp_vapor + ql(i)*c_liq + qs(i)*c_ice
     enddo
  case default
     do i=is,ie 
        qd(i) = 0.
        cpm(i) = cp_air
     enddo
  end select

 end subroutine moist_cp

end module fv_mapz_mod
