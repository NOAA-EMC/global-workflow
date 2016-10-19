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

module nh_core_mod

  use fms_mod, only: error_mesg, FATAL
  use fv_arrays_mod,  only: fv_grid_type, fv_grid_bounds_type


  implicit none
  private

  public Riem_Solver3, Riem_Solver_c, update_dz_c, update_dz_d, nest_halo_nh

!---- version number -----
  character(len=128) :: version = '$Id$'
  character(len=128) :: tagname = '$Name$'

contains

  subroutine update_dz_c(is, ie, js, je, km, ng, dt, dp0, zs, area, ut, vt, gz, ws, &
       npx, npy, sw_corner, se_corner, ne_corner, nw_corner, bd, grid_type)
! !INPUT PARAMETERS:
   type(fv_grid_bounds_type), intent(IN) :: bd
   integer, intent(in):: is, ie, js, je, ng, km, npx, npy, grid_type
   logical, intent(IN):: sw_corner, se_corner, ne_corner, nw_corner
   real, intent(in):: dt
   real, intent(in):: dp0(km)
   real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng,km):: ut, vt
   real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng):: area
   real, intent(inout):: gz(is-ng:ie+ng,js-ng:je+ng,km+1)
   real, intent(in   ):: zs(is-ng:ie+ng, js-ng:je+ng)
   real, intent(  out):: ws(is-ng:ie+ng, js-ng:je+ng)

   call error_mesg('update_dz_c','The null version of update_dz_c should not be called.',FATAL)

  end subroutine update_dz_c



  subroutine update_dz_d(ndif, damp, hord, is, ie, js, je, km, ng, npx, npy, area, rarea,   &
                         dp0, zs, zh, crx, cry, xfx, yfx, delz, ws, rdt, gridstruct, bd)
   type(fv_grid_bounds_type), intent(IN) :: bd
   integer, intent(in):: is, ie, js, je, ng, km, npx, npy
   integer, intent(in):: hord
   real, intent(in)   :: rdt
   real, intent(in)   :: dp0(km)
   real, intent(in)   :: area(is-ng:ie+ng,js-ng:je+ng)
   real, intent(in)   :: rarea(is-ng:ie+ng,js-ng:je+ng)
   real,    intent(inout):: damp(km+1)
   integer, intent(inout):: ndif(km+1)
   real, intent(in   ) ::  zs(is-ng:ie+ng,js-ng:je+ng)
   real, intent(inout) ::  zh(is-ng:ie+ng,js-ng:je+ng,km+1)
   real, intent(  out) ::delz(is-ng:ie+ng,js-ng:je+ng,km)
   real, intent(inout), dimension(is:ie+1,js-ng:je+ng,km):: crx, xfx
   real, intent(inout), dimension(is-ng:ie+ng,js:je+1,km):: cry, yfx
   real, intent(inout)   :: ws(is:ie,js:je)
   type(fv_grid_type), intent(IN), target :: gridstruct

   call error_mesg('update_dz_d','The null version of update_dz_d should not be called.',FATAL)

  end subroutine update_dz_d




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
   real, intent(in):: dt         ! the BIG horizontal Lagrangian time step
   real, intent(in):: akap, cp, ptop, p_fac, a_imp, scale_m
   real, intent(in):: zs(isd:ied,jsd:jed)
   logical, intent(in):: last_call, use_logp, fp_out
   real, intent(in):: ws(is:ie,js:je)
   real, intent(in), dimension(isd:,jsd:,1:):: q_con, cappa
   real, intent(in), dimension(isd:ied,jsd:jed,km):: delp, pt
   real, intent(inout), dimension(isd:ied,jsd:jed,km+1):: zh
   real, intent(inout), dimension(isd:ied,jsd:jed,km):: w
   real, intent(inout):: pe(is-1:ie+1,km+1,js-1:je+1)
   real, intent(out):: peln(is:ie,km+1,js:je)          ! ln(pe)
   real, intent(out), dimension(isd:ied,jsd:jed,km+1):: ppe
   real, intent(out):: delz(is-ng:ie+ng,js-ng:je+ng,km)
   real, intent(out):: pk(is:ie,js:je,km+1)
   real, intent(out):: pk3(isd:ied,jsd:jed,km+1)

   call error_mesg('Riem_Solver3','The null version of Riem_Solver3 should not be called.',FATAL)

  end subroutine Riem_Solver3



  subroutine Riem_Solver_c(ms,   dt,  is,   ie,   js, je, km,   ng,  &
                           akap, cappa, cp,  ptop, hs, w3,  pt, q_con, &
                           delp, gz,  pef,  ws, p_fac, a_imp, scale_m)
   integer, intent(in):: is, ie, js, je, ng, km
   integer, intent(in):: ms
   real, intent(in):: dt,  akap, cp, ptop, p_fac, a_imp, scale_m
   real, intent(in):: ws(is-ng:ie+ng,js-ng:je+ng)
   real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng,km):: pt, delp
   real, intent(in), dimension(is-ng:,js-ng:,1:):: q_con, cappa
   real, intent(in)::   hs(is-ng:ie+ng,js-ng:je+ng)
   real, intent(in), dimension(is-ng:ie+ng,js-ng:je+ng,km):: w3
! OUTPUT PARAMETERS
   real, intent(inout), dimension(is-ng:ie+ng,js-ng:je+ng,km+1):: gz
   real, intent(  out), dimension(is-ng:ie+ng,js-ng:je+ng,km+1):: pef

   call error_mesg('Riem_Solver_c','The null version of Riem_Solver_c should not be called.',FATAL)

  end subroutine Riem_Solver_c



  subroutine nest_halo_nh(ptop, grav, kappa, cp, delp, delz, pt, phis, pkc, gz, pk3, &
                           npx, npy, npz, nested, pkc_pertn, computepk3, fullhalo, bd)
   integer, intent(in) :: npx, npy, npz
   logical, intent(in) :: pkc_pertn, computepk3, fullhalo, nested
   real, intent(in) :: ptop, kappa, cp, grav
   type(fv_grid_bounds_type), intent(IN) :: bd
   real, intent(in) :: phis(bd%isd:bd%ied,bd%jsd:bd%jed)
   real, intent(in),  dimension(bd%isd:bd%ied,bd%jsd:bd%jed,npz):: pt, delp, delz
   real, intent(inout), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,npz+1):: gz, pkc, pk3

   call error_mesg('nest_halo_nh','The null version of nest_halo_nh should not be called.',FATAL)

  end subroutine nest_halo_nh

end module nh_core_mod
