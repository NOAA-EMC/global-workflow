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

!>@brief The module 'nh_core' peforms non-hydrostatic computations.
!@author S. J. Lin, NOAA/GFDL
!>@todo Include moisture effect in pt

module nh_core_mod

! Modules Included:
! <table>
! <tr>
!     <th>Module Name</th>
!     <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>constants_mod</td>
!     <td>rdgas, cp_air, grav</td>
!   </tr>
!   <tr>
!     <td>nh_utils_mod</td>
!     <td>update_dz_c, update_dz_d, nest_halo_nh, sim3p0_solver, rim_2d,
!         sim_solver, sim1_solver, sim3_solver</td>
!   </tr>
!   <tr>
!     <td>tp_core_mod</td>
!     <td>fv_tp_2d</td>
!   </tr>
! </table>

   use constants_mod,     only: rdgas, cp_air, grav
   use tp_core_mod,       only: fv_tp_2d
   use nh_utils_mod,      only: update_dz_c, update_dz_d, nest_halo_nh
   use nh_utils_mod,      only: sim_solver, sim1_solver, sim3_solver
   use nh_utils_mod,      only: sim3p0_solver, rim_2d
   use nh_utils_mod,      only: Riem_Solver_c

   implicit none
   private

   public Riem_Solver3, Riem_Solver_c, update_dz_c, update_dz_d, nest_halo_nh
   real, parameter:: r3 = 1./3.

CONTAINS 

  subroutine Riem_Solver3(ms, dt,   is,   ie,   js, je, km, ng,    &
                          isd, ied, jsd, jed, akap, cappa, cp,     &
                          ptop, zs, q_con, w,  delz, pt,  &
                          delp, zh, pe, ppe, pk3, pk, peln, &
                          ws, scale_m,  p_fac, a_imp, &
                          use_logp, last_call, fp_out)
!--------------------------------------------
! !OUTPUT PARAMETERS
! Ouput: gz: grav*height at edges
!        pe: full     hydrostatic pressure
!       ppe: non-hydrostatic pressure perturbation
!--------------------------------------------
   integer, intent(in):: ms, is, ie, js, je, km, ng
   integer, intent(in):: isd, ied, jsd, jed
   real, intent(in):: dt         !< the BIG horizontal Lagrangian time step
   real, intent(in):: akap, cp, ptop, p_fac, a_imp, scale_m
   real, intent(in):: zs(isd:ied,jsd:jed)
   logical, intent(in):: last_call, use_logp, fp_out
   real, intent(in):: ws(is:ie,js:je)
   real, intent(in), dimension(isd:,jsd:,1:):: q_con, cappa
   real, intent(in), dimension(isd:ied,jsd:jed,km):: delp, pt
   real, intent(inout), dimension(isd:ied,jsd:jed,km+1):: zh
   real, intent(inout), dimension(isd:ied,jsd:jed,km):: w
   real, intent(inout):: pe(is-1:ie+1,km+1,js-1:je+1)
   real, intent(out):: peln(is:ie,km+1,js:je)          !< ln(pe)
   real, intent(out), dimension(isd:ied,jsd:jed,km+1):: ppe
   real, intent(out):: delz(is-ng:ie+ng,js-ng:je+ng,km)
   real, intent(out):: pk(is:ie,js:je,km+1)
   real, intent(out):: pk3(isd:ied,jsd:jed,km+1)
! Local:
  real, dimension(is:ie,km):: dm, dz2, pm2, w2, gm2, cp2
  real, dimension(is:ie,km+1)::pem, pe2, peln2, peg, pelng
  real gama, rgrav, ptk, peln1
  integer i, j, k

    gama = 1./(1.-akap)
   rgrav = 1./grav
   peln1 = log(ptop)
     ptk = exp(akap*peln1)

!$OMP parallel do default(none) shared(is,ie,js,je,km,delp,ptop,peln1,pk3,ptk,akap,rgrav,zh,pt, &
!$OMP                                  w,a_imp,dt,gama,ws,p_fac,scale_m,ms,delz,last_call,  &
!$OMP                                  peln,pk,fp_out,ppe,use_logp,zs,pe,cappa,q_con )          &
!$OMP                          private(cp2, gm2, dm, dz2, pm2, pem, peg, pelng, pe2, peln2, w2)
   do 2000 j=js, je

      do k=1,km
         do i=is, ie
            dm(i,k) = delp(i,j,k)
#ifdef MOIST_CAPPA
            cp2(i,k) = cappa(i,j,k)
#endif
         enddo
      enddo

      do i=is,ie
         pem(i,1) = ptop
         peln2(i,1) = peln1
         pk3(i,j,1) = ptk
#ifdef USE_COND
         peg(i,1) = ptop
         pelng(i,1) = peln1
#endif
      enddo
      do k=2,km+1
         do i=is,ie
            pem(i,k) = pem(i,k-1) + dm(i,k-1)
            peln2(i,k) = log(pem(i,k))
#ifdef USE_COND
! Excluding contribution from condensates:
! peln used during remap; pk3 used only for p_grad
            peg(i,k) = peg(i,k-1) + dm(i,k-1)*(1.-q_con(i,j,k-1))
            pelng(i,k) = log(peg(i,k))
#endif
            pk3(i,j,k) = exp(akap*peln2(i,k))
         enddo
      enddo

      do k=1,km
         do i=is, ie
#ifdef USE_COND
            pm2(i,k) = (peg(i,k+1)-peg(i,k))/(pelng(i,k+1)-pelng(i,k))

#ifdef MOIST_CAPPA
            gm2(i,k) = 1. / (1.-cp2(i,k))
#endif

#else
            pm2(i,k) = dm(i,k)/(peln2(i,k+1)-peln2(i,k))
#endif
             dm(i,k) = dm(i,k) * rgrav
            dz2(i,k) = zh(i,j,k+1) - zh(i,j,k)
             w2(i,k) = w(i,j,k)
         enddo
      enddo

      if ( a_imp < -0.999 ) then
           call SIM3p0_solver(dt, is, ie, km, rdgas, gama, akap, pe2, dm,  &
                              pem, w2, dz2, pt(is:ie,j,1:km), ws(is,j), p_fac, scale_m )
      elseif ( a_imp < -0.5 ) then
           call SIM3_solver(dt, is, ie, km, rdgas, gama, akap, pe2, dm,   &
                        pem, w2, dz2, pt(is:ie,j,1:km), ws(is,j), abs(a_imp), p_fac, scale_m)
      elseif ( a_imp <= 0.5 ) then
           call RIM_2D(ms, dt, is, ie, km, rdgas, gama, gm2, pe2,   &
                       dm, pm2, w2, dz2, pt(is:ie,j,1:km), ws(is,j), .false.)
      elseif ( a_imp > 0.999 ) then
           call SIM1_solver(dt, is, ie, km, rdgas, gama, gm2, cp2, akap, pe2, dm,   &
                            pm2, pem, w2, dz2, pt(is:ie,j,1:km), ws(is,j), p_fac)
      else
           call SIM_solver(dt, is, ie, km, rdgas, gama, gm2, cp2, akap, pe2, dm,  &
                           pm2, pem, w2, dz2, pt(is:ie,j,1:km), ws(is,j), &
                           a_imp, p_fac, scale_m)
      endif

      do k=1, km
         do i=is, ie
            w(i,j,k) = w2(i,k)
            delz(i,j,k) = dz2(i,k)
         enddo
      enddo

      if ( last_call ) then
           do k=1,km+1
              do i=is,ie
                 peln(i,k,j) = peln2(i,k)
                   pk(i,j,k) = pk3(i,j,k)
                   pe(i,k,j) = pem(i,k)
              enddo
           enddo
      endif

      if( fp_out ) then
         do k=1,km+1
         do i=is, ie
            ppe(i,j,k) = pe2(i,k) + pem(i,k)
         enddo
         enddo
      else
         do k=1,km+1
         do i=is, ie
            ppe(i,j,k) = pe2(i,k)
         enddo
         enddo
      endif

      if ( use_logp ) then
         do k=2,km+1
         do i=is, ie
            pk3(i,j,k) = peln2(i,k)
         enddo
         enddo
      endif

      do i=is, ie
         zh(i,j,km+1) = zs(i,j)
      enddo
      do k=km,1,-1
         do i=is, ie
            zh(i,j,k) = zh(i,j,k+1) - dz2(i,k)
         enddo
      enddo

2000  continue

  end subroutine Riem_Solver3

end module nh_core_mod
