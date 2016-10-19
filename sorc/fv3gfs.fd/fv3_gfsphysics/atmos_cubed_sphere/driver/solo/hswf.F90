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

module hswf_mod

 use constants_mod,      only: grav, rdgas, cp_air, RADIAN, kappa, radius, pi
 use fv_grid_utils_mod,  only: g_sum
 use mpp_domains_mod,    only: mpp_update_domains, domain2d
 use time_manager_mod,   only: time_type, get_date, get_time
 use diag_manager_mod,   only: send_data
 use fv_timing_mod,      only: timing_on, timing_off

      implicit none
!-----------------------------------------------------------------------
      private
      public :: Held_Suarez_Tend, age_of_air

!---- version number -----
      character(len=128) :: version = '$Id: $'
      character(len=128) :: tagname = '$Name: $'

contains

!-----------------------------------------------------------------------

 subroutine Held_Suarez_Tend(npx, npy, npz, is, ie, js, je, ng, nq,   &
                              u, v, pt, q, pe, delp, peln, pkz, pdt,  &
                              ua, va, u_dt, v_dt, t_dt, q_dt, agrid,  &
                              delz, phis, hydrostatic, ak, bk, ks,    &
                              strat, rayf, master, Time, time_total)

      integer, INTENT(IN   ) :: npx, npy, npz
      integer, INTENT(IN   ) :: is, ie, js, je, ng, nq
      logical, intent(IN)    :: hydrostatic
      real   , INTENT(IN   ) :: phis(is-ng:ie+ng,js-ng:je+ng)
      real   , INTENT(IN   ) :: delz(is-ng:ie+ng,js-ng:je+ng,npz)
      real   , INTENT(IN) ::  pkz(is  :ie     ,js   :je     ,1:npz)

      real   , INTENT(INOUT) ::    u(is-ng:ie+  ng,js-ng:je+1+ng,npz)
      real   , INTENT(INOUT) ::    v(is-ng:ie+1+ng,js-ng:je+  ng,npz)
      real   , INTENT(INOUT) ::   pt(is-ng:ie+  ng,js-ng:je+  ng,npz)
      real   , INTENT(INOUT) :: delp(is-ng:ie+  ng,js-ng:je+  ng,npz)
      real   , INTENT(INOUT) ::    q(is-ng:ie+  ng,js-ng:je+  ng,npz, nq)
      real   , INTENT(INOUT) ::   pe(is-1:ie+1 ,1:npz+1,js-1:je+1)
      real   , INTENT(INOUT) :: peln(is  :ie     ,1:npz+1,js   :je     )

      real   , INTENT(INOUT) ::   ua(is-ng:ie+ng,js-ng:je+ng,npz)
      real   , INTENT(INOUT) ::   va(is-ng:ie+ng,js-ng:je+ng,npz)

! Tendencies:
      real, INTENT(INOUT):: u_dt(is-ng:ie+ng,js-ng:je+ng,npz)
      real, INTENT(INOUT):: v_dt(is-ng:ie+ng,js-ng:je+ng,npz)
      real, INTENT(INOUT):: t_dt(is:ie,js:je,npz)
      real, INTENT(INOUT):: q_dt(is:ie,js:je,npz,nq)


      real   , INTENT(IN   ) :: agrid(is-ng:ie+ng,js-ng:je+ng, 2)
      real   , INTENT(IN   ) :: ak(npz+1), bk(npz+1)
      integer, INTENT(IN   ) :: ks

      real   , INTENT(IN   ) :: pdt
      logical, INTENT(IN   ) :: strat, rayf, master

      type(time_type), intent(in) :: Time
      real, INTENT(IN), optional:: time_total

! Local
      real, dimension(is:ie,npz):: teq, pl
      real, dimension(is:ie):: u1, v1
      integer  i,j,k
      integer  seconds, days
      real  ty, tz, akap 
      real  p0, t0, sday, rkv, rka, rks, rkt, sigb, rsgb
      real  tmp, solar_ang, solar_rate
      real  ap0k, algpk
      real  tey, tez, fac, pw, sigl
      real  h0, dz
      real  dt_tropic
      real  rmr, rms
      real  relx, tau
      real  t_st, t_ms
      real  rdt, f1
      real rad_ratio, kf_day

      ty = 60.0
      tz = 10.0             ! Original value from H-S was 10.
      akap = 2./7.

      p0 = 100000.
      t0 = 200.
      h0 = 7.
      sday = 24.*3600.
      rdt = 1. / pdt

!--------------------------
      rad_ratio = radius / 6371.0e3

      kf_day = sday * rad_ratio
      rkv = pdt / kf_day
      rka = pdt / (40.*kf_day)
      rks = pdt / (4.0*kf_day)

! For strat-mesosphere
      t_ms = 10.*rad_ratio
      t_st = 40.*rad_ratio

      tau = (t_st - t_ms) / log(100.)
      rms = pdt/(t_ms*sday)
      rmr =  1./(1.+rms)

      sigb = 0.7
      rsgb = 1./(1.-sigb)
      ap0k = 1./p0**akap
      algpk = log(ap0k)

! Temperature forcing...
!$OMP parallel do default(none) shared(is,ie,js,je,npz,delp,peln,ap0k,ty,agrid,tz,akap, &
!$OMP                                  strat,h0,t_dt,pt,rms,rmr,rdt,t_ms,tau,pdt,sday,pe, &
!$OMP                                  sigb,rsgb,pkz,algpk,t0,rka,rks,rkv,u_dt,ua,v_dt,va) &
!$OMP                          private(pl, teq, tey, tez, dz, relx, dt_tropic, sigl, f1, rkt,tmp,u1,v1)
     do j=js,je
        do k=1,npz
           do i=is,ie
              pl(i,k) = delp(i,j,k) / ( peln(i,k+1,j)-peln(i,k,j))
           enddo
        enddo
        do k=npz,1,-1
           do i=is,ie
              tey = ap0k*( 315.0 - ty*SIN(agrid(i,j,2))*SIN(agrid(i,j,2)) )
              tez =  tz*( ap0k/akap )*COS(agrid(i,j,2))*COS(agrid(i,j,2)) 
              if (strat .and. pl(i,k) <= 1.E2)  then
! Mesosphere: defined as the region above 1 mb
                  dz = h0 * log(pl(i,k+1)/pl(i,k))
                  dt_tropic = -2.25*COS(agrid(i,j,2)) * dz
                  teq(i,k) = teq(i,k+1) + dt_tropic
                  t_dt(i,j,k) = t_dt(i,j,k) + ((pt(i,j,k)+rms*teq(i,k))*rmr - pt(i,j,k))*rdt
! Stratosphere:
              elseif (strat .and. pl(i,k)>1.E2 .and. pl(i,k)<=100.E2 ) then
                  dz = h0 * log(pl(i,k+1)/pl(i,k))
! Lapse rate above tropic stratopause is 2.25 deg/km
! Relaxation time is t_st days at 100 mb (as H-S) and gradually
! decreases to t_ms Days at and above the stratopause
                  relx =  t_ms + tau*log(0.01*pl(i,k))
                  relx = pdt/(relx*sday)
                  dt_tropic = 2.25*COS(agrid(i,j,2)) * dz
                  teq(i,k)  =  teq(i,k+1) + dt_tropic
                  t_dt(i,j,k) = t_dt(i,j,k) + relx*(teq(i,k)-pt(i,j,k))/(1.+relx) * rdt
              else
! Troposphere: standard Held-Suarez
                  sigl = pl(i,k)/pe(i,npz+1,j)
                  f1 = max(0., (sigl-sigb) * rsgb )
                  teq(i,k) = tey - tez*(log(pkz(i,j,k))+algpk)
                  teq(i,k) = max(t0, teq(i,k)*pkz(i,j,k))
                  rkt = rka + (rks-rka)*f1*(COS(agrid(i,j,2))**4.0)
                  t_dt(i,j,k) = t_dt(i,j,k) + rkt*(teq(i,k)-pt(i,j,k))/(1.+rkt) * rdt
                                                       ! Bottom friction:
                  sigl = pl(i,k) / pe(i,npz+1,j)
                  sigl = (sigl-sigb)*rsgb * rkv
                  if (sigl > 0.) then
                      tmp = sigl / (1.+sigl) * rdt
                      u1(i) = ua(i,j,k) + u_dt(i,j,k)
                      v1(i) = va(i,j,k) + v_dt(i,j,k)
                      u_dt(i,j,k) = u_dt(i,j,k) - u1(i)*tmp
                      v_dt(i,j,k) = v_dt(i,j,k) - v1(i)*tmp
                  endif
              endif
           enddo     !i-loop
        enddo     !k-loop
     enddo     !j-loop

#ifdef DO_AGE
      if( nq/=0 )     &
      call age_of_air(is, ie, js, je, npz, ng, time_total, pe, q(is-ng,js-ng,1,nq))
#endif

 end subroutine Held_Suarez_Tend

 subroutine age_of_air(is, ie, js, je, km, ng, time, pe, q)

      integer is, ie, js, je
      integer km
      integer ng

! q is the age tracer
! Need to be converted to mixing ratio (mass of tracer / dry_air-mass)
! Ignore this inconsistency for now.

      real, intent(inout):: pe(is-1:ie+1, km+1, js-1:je+1)
      real, intent(in):: time        ! accumulated time since init
      real, intent(inout):: q(is-ng:ie+ng,js-ng:je+ng,km)

! Local
      integer i, j, k
      real p_source      ! source level (pa)
      real ascale
      real tiny
      parameter ( tiny = 1.e-6 )
      parameter ( p_source = 75000. )
      parameter ( ascale = 5.e-6 / 60. )

!$OMP parallel do default(none) shared(is,ie,js,je,km,time,q,pe)
      do k=1,km
        do j=js,je
            do i=is,ie
               if( time < tiny ) then
                   q(i,j,k) = 0.
               elseif( pe(i,k,j) >= p_source ) then
                   q(i,j,k) = ascale * time
               endif
            enddo
        enddo          ! j-loop
      enddo             ! k-loop

 end subroutine age_of_air

end module hswf_mod
