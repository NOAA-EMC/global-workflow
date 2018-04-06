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

!>@brief The module 'tp_core' is a collection of routines to support FV transport.
!>@details The module contains the scalar advection scheme and PPM operators. 
module tp_core_mod

! Modules Included:
!
! <table>
!   <tr>
!     <th>Module Name</th>
!    <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>fv_mp_mod</td>
!     <td>ng</td>
!   </tr>
!   <tr>
!     <td>fv_grid_utils_mod</td>
!     <td>big_number</td>
!   </tr>
!   <tr>
!     <td>fv_arrays_mod</td>
!     <td>fv_grid_type, fv_grid_bounds_type</td>
!   </tr>
!   <tr>
!     <td>field_manager_mod</td>
!     <td>fm_path_name_len, fm_string_len, fm_exists, fm_get_index, fm_new_list, fm_get_current_list,
!         fm_change_list, fm_field_name_len, fm_type_name_len, fm_dump_list, fm_loop_over_list</td>
!   </tr>
! </table>

 use fv_mp_mod,         only: ng 
 use fv_grid_utils_mod, only: big_number
 use fv_arrays_mod,     only: fv_grid_type, fv_grid_bounds_type

 implicit none

 private
 public fv_tp_2d, pert_ppm, copy_corners

 real, parameter:: ppm_fac = 1.5   !< nonlinear scheme limiter: between 1 and 2
 real, parameter:: r3 = 1./3.
 real, parameter:: near_zero = 1.E-25
 real, parameter:: ppm_limiter = 2.0

#ifdef WAVE_FORM
! Suresh & Huynh scheme 2.2 (purtabation form)
! The wave-form is more diffusive than scheme 2.1
 real, parameter:: b1 =   0.0375
 real, parameter:: b2 =  -7./30.
 real, parameter:: b3 =  -23./120.
 real, parameter:: b4 =  13./30.
 real, parameter:: b5 = -11./240.
#else
! scheme 2.1: perturbation form
 real, parameter:: b1 =   1./30.
 real, parameter:: b2 = -13./60.
 real, parameter:: b3 = -13./60.
 real, parameter:: b4 =  0.45
 real, parameter:: b5 = -0.05
#endif
 real, parameter:: t11 = 27./28., t12 = -13./28., t13=3./7.
 real, parameter:: s11 = 11./14., s14 = 4./7.,    s15=3./14.
!----------------------------------------------------
! volume-conserving cubic with 2nd drv=0 at end point:
!----------------------------------------------------
! Non-monotonic
  real, parameter:: c1 = -2./14.
  real, parameter:: c2 = 11./14.
  real, parameter:: c3 =  5./14.
!----------------------
! PPM volume mean form:
!----------------------
  real, parameter:: p1 =  7./12.     ! 0.58333333
  real, parameter:: p2 = -1./12.
!   q(i+0.5) = p1*(q(i-1)+q(i)) + p2*(q(i-2)+q(i+1))
! integer:: is, ie, js, je, isd, ied, jsd, jed

!
!EOP
!-----------------------------------------------------------------------

contains

!>@brief The subroutine 'fv_tp_2d' contains the FV advection scheme
!! \cite putman2007finite \cite lin1996multiflux. 
!>@details It performs 1 time step of the forward advection.
 subroutine fv_tp_2d(q, crx, cry, npx, npy, hord, fx, fy, xfx, yfx,  &
                     gridstruct, bd, ra_x, ra_y, lim_fac, mfx, mfy, mass, nord, damp_c)
   type(fv_grid_bounds_type), intent(IN) :: bd
   integer, intent(in):: npx, npy
   integer, intent(in)::hord

   real, intent(in)::  crx(bd%is:bd%ie+1,bd%jsd:bd%jed)  
   real, intent(in)::  xfx(bd%is:bd%ie+1,bd%jsd:bd%jed)  
   real, intent(in)::  cry(bd%isd:bd%ied,bd%js:bd%je+1 )  
   real, intent(in)::  yfx(bd%isd:bd%ied,bd%js:bd%je+1 )  
   real, intent(in):: ra_x(bd%is:bd%ie,bd%jsd:bd%jed)
   real, intent(in):: ra_y(bd%isd:bd%ied,bd%js:bd%je)
   real, intent(inout):: q(bd%isd:bd%ied,bd%jsd:bd%jed)  !< transported scalar
   real, intent(out)::fx(bd%is:bd%ie+1 ,bd%js:bd%je)    !< Flux in x ( E )
   real, intent(out)::fy(bd%is:bd%ie,   bd%js:bd%je+1 ) !< Flux in y ( N )

   type(fv_grid_type), intent(IN), target :: gridstruct

   real, intent(in):: lim_fac
! optional Arguments:
   real, OPTIONAL, intent(in):: mfx(bd%is:bd%ie+1,bd%js:bd%je  ) !< Mass Flux X-Dir
   real, OPTIONAL, intent(in):: mfy(bd%is:bd%ie  ,bd%js:bd%je+1)  !< Mass Flux Y-Dir
   real, OPTIONAL, intent(in):: mass(bd%isd:bd%ied,bd%jsd:bd%jed)
   real, OPTIONAL, intent(in):: damp_c
   integer, OPTIONAL, intent(in):: nord !< order of divergence damping
! Local:
   integer ord_ou, ord_in
   real q_i(bd%isd:bd%ied,bd%js:bd%je)
   real q_j(bd%is:bd%ie,bd%jsd:bd%jed)
   real   fx2(bd%is:bd%ie+1,bd%jsd:bd%jed)
   real   fy2(bd%isd:bd%ied,bd%js:bd%je+1)
   real   fyy(bd%isd:bd%ied,bd%js:bd%je+1)
   real   fx1(bd%is:bd%ie+1)
   real   damp
   integer i, j

   integer:: is, ie, js, je, isd, ied, jsd, jed

   is  = bd%is
   ie  = bd%ie
   js  = bd%js
   je  = bd%je
   isd = bd%isd
   ied = bd%ied
   jsd = bd%jsd
   jed = bd%jed

   if ( hord == 10 ) then
        ord_in = 8
   else
        ord_in = hord
   endif
   ord_ou = hord

   if (.not. gridstruct%nested) call copy_corners(q, npx, npy, 2, gridstruct%nested, bd, &
                                gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)

   call yppm(fy2, q, cry, ord_in, isd,ied,isd,ied, js,je,jsd,jed, npx,npy, gridstruct%dya, gridstruct%nested, gridstruct%grid_type, lim_fac)

   do j=js,je+1
      do i=isd,ied
         fyy(i,j) = yfx(i,j) * fy2(i,j) 
      enddo
   enddo
   do j=js,je
      do i=isd,ied
         q_i(i,j) = (q(i,j)*gridstruct%area(i,j) + fyy(i,j)-fyy(i,j+1))/ra_y(i,j)
      enddo
   enddo

   call xppm(fx, q_i, crx(is,js), ord_ou, is,ie,isd,ied, js,je,jsd,jed, npx,npy, gridstruct%dxa, gridstruct%nested, gridstruct%grid_type, lim_fac)

  if (.not. gridstruct%nested) call copy_corners(q, npx, npy, 1, gridstruct%nested, bd, &
                               gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)

  call xppm(fx2, q, crx, ord_in, is,ie,isd,ied, jsd,jed,jsd,jed, npx,npy, gridstruct%dxa, gridstruct%nested, gridstruct%grid_type, lim_fac)

  do j=jsd,jed
     do i=is,ie+1
        fx1(i) =  xfx(i,j) * fx2(i,j)
     enddo
     do i=is,ie
        q_j(i,j) = (q(i,j)*gridstruct%area(i,j) + fx1(i)-fx1(i+1))/ra_x(i,j)
     enddo
  enddo

  call yppm(fy, q_j, cry, ord_ou, is,ie,isd,ied, js,je,jsd,jed, npx, npy, gridstruct%dya, gridstruct%nested, gridstruct%grid_type, lim_fac)

!----------------
! Flux averaging:
!----------------

   if ( present(mfx) .and. present(mfy) ) then
!---------------------------------
! For transport of pt and tracers
!---------------------------------
      do j=js,je
         do i=is,ie+1
            fx(i,j) = 0.5*(fx(i,j) + fx2(i,j)) * mfx(i,j)
         enddo
      enddo
      do j=js,je+1
         do i=is,ie
            fy(i,j) = 0.5*(fy(i,j) + fy2(i,j)) * mfy(i,j)
         enddo
      enddo
      if ( present(nord) .and. present(damp_c) .and. present(mass) ) then
        if ( damp_c > 1.e-4 ) then
           damp = (damp_c * gridstruct%da_min)**(nord+1)
           call deln_flux(nord, is,ie,js,je, npx, npy, damp, q, fx, fy, gridstruct, bd, mass )
        endif
      endif
   else
!---------------------------------
! For transport of delp, vorticity
!---------------------------------
      do j=js,je
         do i=is,ie+1
            fx(i,j) = 0.5*(fx(i,j) + fx2(i,j)) * xfx(i,j)
         enddo
      enddo
      do j=js,je+1
         do i=is,ie
            fy(i,j) = 0.5*(fy(i,j) + fy2(i,j)) * yfx(i,j)
         enddo
      enddo
      if ( present(nord) .and. present(damp_c) ) then
           if ( damp_c > 1.E-4 ) then
                damp = (damp_c * gridstruct%da_min)**(nord+1)
                call deln_flux(nord, is,ie,js,je, npx, npy, damp, q, fx, fy, gridstruct, bd)
           endif
      endif
   endif

 end subroutine fv_tp_2d

 !Weird arguments are because this routine is called in a lot of
 !places outside of tp_core, sometimes very deeply nested in the call tree.
 subroutine copy_corners(q, npx, npy, dir, nested, bd, &
                         sw_corner, se_corner, nw_corner, ne_corner)
 type(fv_grid_bounds_type), intent(IN) :: bd
 integer, intent(in):: npx, npy, dir
 real, intent(inout):: q(bd%isd:bd%ied,bd%jsd:bd%jed)
 logical, intent(IN) :: nested, sw_corner, se_corner, nw_corner, ne_corner
 integer  i,j

 if (nested) return

 if ( dir == 1 ) then
! XDir:
    if ( sw_corner ) then
         do j=1-ng,0
            do i=1-ng,0
               q(i,j) = q(j,1-i)
            enddo
         enddo
    endif
    if ( se_corner ) then
         do j=1-ng,0
            do i=npx,npx+ng-1
               q(i,j) = q(npy-j,i-npx+1)
            enddo
         enddo
    endif
    if ( ne_corner ) then
         do j=npy,npy+ng-1
            do i=npx,npx+ng-1
               q(i,j) = q(j,2*npx-1-i)
            enddo
         enddo
    endif
    if ( nw_corner ) then
         do j=npy,npy+ng-1
            do i=1-ng,0
               q(i,j) = q(npy-j,i-1+npx)
            enddo
         enddo
    endif

 elseif ( dir == 2 ) then
! YDir:

    if ( sw_corner ) then
         do j=1-ng,0
            do i=1-ng,0
               q(i,j) = q(1-j,i)
            enddo
         enddo
    endif
    if ( se_corner ) then
         do j=1-ng,0
            do i=npx,npx+ng-1
               q(i,j) = q(npy+j-1,npx-i)
            enddo
         enddo
    endif
    if ( ne_corner ) then
         do j=npy,npy+ng-1
            do i=npx,npx+ng-1
               q(i,j) = q(2*npy-1-j,i)
            enddo
         enddo
    endif
    if ( nw_corner ) then
         do j=npy,npy+ng-1
            do i=1-ng,0
               q(i,j) = q(j+1-npx,npy-i)
            enddo
         enddo
    endif

 endif
      
 end subroutine copy_corners

 subroutine xppm(flux, q, c, iord, is,ie,isd,ied, jfirst,jlast,jsd,jed, npx, npy, dxa, nested, grid_type, lim_fac)
 integer, INTENT(IN) :: is, ie, isd, ied, jsd, jed
 integer, INTENT(IN) :: jfirst, jlast  !< compute domain
 integer, INTENT(IN) :: iord
 integer, INTENT(IN) :: npx, npy
 real   , INTENT(IN) :: q(isd:ied,jfirst:jlast)
 real   , INTENT(IN) :: c(is:ie+1,jfirst:jlast) !< Courant N (like FLUX)
 real   , intent(IN) :: dxa(isd:ied,jsd:jed)
 logical, intent(IN) :: nested
 integer, intent(IN) :: grid_type
 real   , intent(IN) :: lim_fac
!OUTPUT PARAMETERS:
 real  , INTENT(OUT) :: flux(is:ie+1,jfirst:jlast) !< Flux
! Local
 real, dimension(is-1:ie+1):: bl, br, b0
 real:: q1(isd:ied)
 real, dimension(is:ie+1):: fx0, fx1, xt1
 logical, dimension(is-1:ie+1):: smt5, smt6
 logical, dimension(is:ie+1):: hi5, hi6
 real  al(is-1:ie+2)
 real  dm(is-2:ie+2)
 real  dq(is-3:ie+2)
 integer:: i, j, ie3, is1, ie1, mord
 real:: x0, x1, xt, qtmp, pmp_1, lac_1, pmp_2, lac_2

 if ( .not. nested .and. grid_type<3 ) then
    is1 = max(3,is-1);  ie3 = min(npx-2,ie+2)
                        ie1 = min(npx-3,ie+1)
 else
    is1 = is-1;         ie3 = ie+2
                        ie1 = ie+1
 end if

 mord = abs(iord)

 do 666 j=jfirst,jlast

    do i=isd, ied
       q1(i) = q(i,j)
    enddo

 if ( iord < 8 ) then
! ord = 2: perfectly linear ppm scheme
! Diffusivity: ord2 < ord5 < ord3 < ord4 < ord6 

   do i=is1, ie3
      al(i) = p1*(q1(i-1)+q1(i)) + p2*(q1(i-2)+q1(i+1))
   enddo

   if ( .not.nested .and. grid_type<3 ) then
     if ( is==1 ) then
       al(0) = c1*q1(-2) + c2*q1(-1) + c3*q1(0)
       al(1) = 0.5*(((2.*dxa(0,j)+dxa(-1,j))*q1(0)-dxa(0,j)*q1(-1))/(dxa(-1,j)+dxa(0,j)) &
             +      ((2.*dxa(1,j)+dxa( 2,j))*q1(1)-dxa(1,j)*q1( 2))/(dxa(1, j)+dxa(2,j)))
       al(2) = c3*q1(1) + c2*q1(2) +c1*q1(3)
     endif
     if ( (ie+1)==npx ) then
       al(npx-1) = c1*q1(npx-3) + c2*q1(npx-2) + c3*q1(npx-1)
       al(npx) = 0.5*(((2.*dxa(npx-1,j)+dxa(npx-2,j))*q1(npx-1)-dxa(npx-1,j)*q1(npx-2))/(dxa(npx-2,j)+dxa(npx-1,j)) &
               +      ((2.*dxa(npx,  j)+dxa(npx+1,j))*q1(npx  )-dxa(npx,  j)*q1(npx+1))/(dxa(npx,  j)+dxa(npx+1,j)))
       al(npx+1) = c3*q1(npx) + c2*q1(npx+1) + c1*q1(npx+2)
     endif
   endif

   if ( iord<0 ) then
       do i=is-1, ie+2
          al(i) = max(0., al(i))
       enddo
   endif

   if ( mord==1 ) then  ! perfectly linear scheme
        do i=is-1,ie+1
           bl(i) = al(i)   - q1(i)
           br(i) = al(i+1) - q1(i)
           b0(i) = bl(i) + br(i)
           smt5(i) = abs(lim_fac*b0(i)) < abs(bl(i)-br(i))
        enddo
!DEC$ VECTOR ALWAYS
      do i=is,ie+1
         if ( c(i,j) > 0. ) then
             fx1(i) = (1.-c(i,j))*(br(i-1) - c(i,j)*b0(i-1))
             flux(i,j) = q1(i-1)
         else
             fx1(i) = (1.+c(i,j))*(bl(i) + c(i,j)*b0(i))
             flux(i,j) = q1(i)
         endif
         if (smt5(i-1).or.smt5(i)) flux(i,j) = flux(i,j) + fx1(i) 
      enddo

   elseif ( mord==2 ) then  ! perfectly linear scheme

!DEC$ VECTOR ALWAYS
      do i=is,ie+1
         xt = c(i,j)
         if ( xt > 0. ) then
              qtmp = q1(i-1)
              flux(i,j) = qtmp + (1.-xt)*(al(i)-qtmp-xt*(al(i-1)+al(i)-(qtmp+qtmp)))  
         else
              qtmp = q1(i)
              flux(i,j) = qtmp + (1.+xt)*(al(i)-qtmp+xt*(al(i)+al(i+1)-(qtmp+qtmp)))  
         endif
!        x0 = sign(dim(xt, 0.), 1.)
!        x1 = sign(dim(0., xt), 1.)
!        flux(i,j) = x0*(q1(i-1)+(1.-xt)*(al(i)-qtmp-xt*(al(i-1)+al(i)-(qtmp+qtmp))))     &
!                  + x1*(q1(i)  +(1.+xt)*(al(i)-qtmp+xt*(al(i)+al(i+1)-(qtmp+qtmp))))
      enddo

   elseif ( mord==3 ) then

        do i=is-1,ie+1
           bl(i) = al(i)   - q1(i)
           br(i) = al(i+1) - q1(i)
           b0(i) = bl(i) + br(i)
              x0 = abs(b0(i))
              xt = abs(bl(i)-br(i))
           smt5(i) =    x0 < xt
           smt6(i) = 3.*x0 < xt
        enddo
        do i=is,ie+1
           fx1(i) = 0.
           xt1(i) = c(i,j)
           hi5(i) = smt5(i-1) .and. smt5(i)   ! more diffusive
           hi6(i) = smt6(i-1) .or.  smt6(i)
        enddo
        do i=is,ie+1
           if ( xt1(i) > 0. ) then
                if ( hi6(i) ) then
                   fx1(i) = br(i-1) - xt1(i)*b0(i-1)
                elseif ( hi5(i) ) then   ! 2nd order, piece-wise linear
                   fx1(i) = sign(min(abs(bl(i-1)),abs(br(i-1))), br(i-1))
                endif
                flux(i,j) = q1(i-1) + (1.-xt1(i))*fx1(i)
           else
                if ( hi6(i) ) then
                   fx1(i) = bl(i) + xt1(i)*b0(i)
                elseif ( hi5(i) ) then   ! 2nd order, piece-wise linear
                   fx1(i) = sign(min(abs(bl(i)), abs(br(i))), bl(i))
                endif
                flux(i,j) = q1(i) + (1.+xt1(i))*fx1(i)
           endif
        enddo

   elseif ( mord==4 ) then

        do i=is-1,ie+1
           bl(i) = al(i)   - q1(i)
           br(i) = al(i+1) - q1(i)
           b0(i) = bl(i) + br(i)
              x0 = abs(b0(i))
              xt = abs(bl(i)-br(i))
           smt5(i) =    x0 < xt
           smt6(i) = 3.*x0 < xt
        enddo
        do i=is,ie+1
           xt1(i) = c(i,j)
           hi5(i) = smt5(i-1) .and. smt5(i)   ! more diffusive
           hi6(i) = smt6(i-1) .or.  smt6(i)
           hi5(i) = hi5(i) .or. hi6(i)
        enddo
!DEC$ VECTOR ALWAYS
        do i=is,ie+1
           if ( xt1(i) > 0. ) then
               fx1(i) = (1.-xt1(i))*(br(i-1) - xt1(i)*b0(i-1))
               flux(i,j) = q1(i-1)
           else
               fx1(i) = (1.+xt1(i))*(bl(i) + xt1(i)*b0(i))
               flux(i,j) = q1(i)
           endif
           if ( hi5(i) ) flux(i,j) = flux(i,j) + fx1(i) 
        enddo

   else

      if ( mord==5 ) then
        do i=is-1,ie+1
           bl(i) = al(i)   - q1(i)
           br(i) = al(i+1) - q1(i)
           b0(i) = bl(i) + br(i)
           smt5(i) = bl(i)*br(i) < 0.
        enddo
      else
        do i=is-1,ie+1
           bl(i) = al(i)   - q1(i)
           br(i) = al(i+1) - q1(i)
           b0(i) = bl(i) + br(i)
           smt5(i) = 3.*abs(b0(i)) < abs(bl(i)-br(i))
        enddo
      endif

!DEC$ VECTOR ALWAYS
      do i=is,ie+1
         if ( c(i,j) > 0. ) then
              fx1(i) = (1.-c(i,j))*(br(i-1) - c(i,j)*b0(i-1))
              flux(i,j) = q1(i-1)
         else
              fx1(i) = (1.+c(i,j))*(bl(i) + c(i,j)*b0(i))
              flux(i,j) = q1(i)
         endif
         if (smt5(i-1).or.smt5(i)) flux(i,j) = flux(i,j) + fx1(i) 
      enddo

   endif
   goto 666

 else

! Monotonic constraints:
! ord = 8: PPM with Lin's PPM fast monotone constraint
! ord = 10: PPM with Lin's modification of Huynh 2nd constraint
! ord = 13: 10 plus positive definite constraint

    do i=is-2,ie+2
          xt = 0.25*(q1(i+1) - q1(i-1))
       dm(i) = sign(min(abs(xt), max(q1(i-1), q1(i), q1(i+1)) - q1(i),  &
                         q1(i) - min(q1(i-1), q1(i), q1(i+1))), xt)
    enddo
    do i=is1,ie1+1
       al(i) = 0.5*(q1(i-1)+q1(i)) + r3*(dm(i-1)-dm(i))
    enddo

    if ( iord==8 ) then
       do i=is1, ie1
          xt = 2.*dm(i)
          bl(i) = -sign(min(abs(xt), abs(al(i  )-q1(i))), xt)
          br(i) =  sign(min(abs(xt), abs(al(i+1)-q1(i))), xt)
       enddo
    elseif ( iord==11 ) then
! This is emulation of 2nd van Leer scheme using PPM codes
       do i=is1, ie1
          xt = ppm_fac*dm(i)
          bl(i) = -sign(min(abs(xt), abs(al(i  )-q1(i))), xt)
          br(i) =  sign(min(abs(xt), abs(al(i+1)-q1(i))), xt)
       enddo
    else
       do i=is1-2, ie1+1
          dq(i) = 2.*(q1(i+1) - q1(i))
       enddo
       do i=is1, ie1
          bl(i) = al(i  ) - q1(i)
          br(i) = al(i+1) - q1(i)
          if ( abs(dm(i-1))+abs(dm(i))+abs(dm(i+1)) < near_zero ) then
                   bl(i) = 0.
                   br(i) = 0.
          elseif( abs(3.*(bl(i)+br(i))) > abs(bl(i)-br(i)) ) then
                   pmp_2 = dq(i-1)
                   lac_2 = pmp_2 - 0.75*dq(i-2)
                   br(i) = min( max(0., pmp_2, lac_2), max(br(i), min(0., pmp_2, lac_2)) )
                   pmp_1 = -dq(i)
                   lac_1 = pmp_1 + 0.75*dq(i+1)
                   bl(i) = min( max(0., pmp_1, lac_1), max(bl(i), min(0., pmp_1, lac_1)) )
          endif
       enddo
    endif
! Positive definite constraint:
    if(iord==9 .or. iord==13) call pert_ppm(ie1-is1+1, q1(is1), bl(is1), br(is1), 0)

    if (.not. nested .and. grid_type<3) then
      if ( is==1 ) then
         bl(0) = s14*dm(-1) + s11*(q1(-1)-q1(0))

         xt = 0.5*(((2.*dxa(0,j)+dxa(-1,j))*q1(0)-dxa(0,j)*q1(-1))/(dxa(-1,j)+dxa(0,j)) &
            +      ((2.*dxa(1,j)+dxa( 2,j))*q1(1)-dxa(1,j)*q1( 2))/(dxa(1, j)+dxa(2,j)))
!        if ( iord==8 .or. iord==10 ) then
            xt = max(xt, min(q1(-1),q1(0),q1(1),q1(2)))
            xt = min(xt, max(q1(-1),q1(0),q1(1),q1(2)))
!        endif
         br(0) = xt - q1(0)
         bl(1) = xt - q1(1)
         xt = s15*q1(1) + s11*q1(2) - s14*dm(2)
         br(1) = xt - q1(1)
         bl(2) = xt - q1(2)

         br(2) = al(3) - q1(2)
         call pert_ppm(3, q1(0), bl(0), br(0), 1)
      endif
      if ( (ie+1)==npx ) then
         bl(npx-2) = al(npx-2) - q1(npx-2)

         xt = s15*q1(npx-1) + s11*q1(npx-2) + s14*dm(npx-2)
         br(npx-2) = xt - q1(npx-2)
         bl(npx-1) = xt - q1(npx-1)

         xt = 0.5*(((2.*dxa(npx-1,j)+dxa(npx-2,j))*q1(npx-1)-dxa(npx-1,j)*q1(npx-2))/(dxa(npx-2,j)+dxa(npx-1,j)) &
            +      ((2.*dxa(npx,  j)+dxa(npx+1,j))*q1(npx  )-dxa(npx,  j)*q1(npx+1))/(dxa(npx,  j)+dxa(npx+1,j)))
!        if ( iord==8 .or. iord==10 ) then
            xt = max(xt, min(q1(npx-2),q1(npx-1),q1(npx),q1(npx+1)))
            xt = min(xt, max(q1(npx-2),q1(npx-1),q1(npx),q1(npx+1)))
!        endif
         br(npx-1) = xt - q1(npx-1)
         bl(npx  ) = xt - q1(npx  )

         br(npx) = s11*(q1(npx+1)-q1(npx)) - s14*dm(npx+1)
         call pert_ppm(3, q1(npx-2), bl(npx-2), br(npx-2), 1)
      endif
    endif

  endif

  do i=is,ie+1
     if( c(i,j)>0. ) then
         flux(i,j) = q1(i-1) + (1.-c(i,j))*(br(i-1)-c(i,j)*(bl(i-1)+br(i-1)))
     else
         flux(i,j) = q1(i  ) + (1.+c(i,j))*(bl(i  )+c(i,j)*(bl(i)+br(i)))
     endif
  enddo

666   continue

 end subroutine xppm


 subroutine yppm(flux, q, c, jord, ifirst,ilast, isd,ied, js,je,jsd,jed, npx, npy, dya, nested, grid_type, lim_fac)
 integer, INTENT(IN) :: ifirst,ilast    !< Compute domain
 integer, INTENT(IN) :: isd,ied, js,je,jsd,jed
 integer, INTENT(IN) :: jord
 integer, INTENT(IN) :: npx, npy
 real   , INTENT(IN) :: q(ifirst:ilast,jsd:jed)
 real   , intent(in) :: c(isd:ied,js:je+1 )  !< Courant number
 real   , INTENT(OUT):: flux(ifirst:ilast,js:je+1)   !<  Flux
 real   , intent(IN) :: dya(isd:ied,jsd:jed)
 logical, intent(IN) :: nested
 integer, intent(IN) :: grid_type
 real   , intent(IN) :: lim_fac
! Local:
 real:: dm(ifirst:ilast,js-2:je+2)
 real:: al(ifirst:ilast,js-1:je+2)
 real, dimension(ifirst:ilast,js-1:je+1):: bl, br, b0
 real:: dq(ifirst:ilast,js-3:je+2)
 real,    dimension(ifirst:ilast):: fx0, fx1, xt1
 logical, dimension(ifirst:ilast,js-1:je+1):: smt5, smt6
 logical, dimension(ifirst:ilast):: hi5, hi6
 real:: x0, xt, qtmp, pmp_1, lac_1, pmp_2, lac_2, r1
 integer:: i, j, js1, je3, je1, mord

   if ( .not.nested .and. grid_type < 3 ) then
! Cubed-sphere:
      js1 = max(3,js-1); je3 = min(npy-2,je+2)
                         je1 = min(npy-3,je+1)
   else
! Nested grid OR Doubly periodic domain:
      js1 = js-1;        je3 = je+2
                         je1 = je+1
   endif

 mord = abs(jord)

if ( jord < 8 ) then

   do j=js1, je3
      do i=ifirst,ilast
         al(i,j) = p1*(q(i,j-1)+q(i,j)) + p2*(q(i,j-2)+q(i,j+1))
      enddo
   enddo

   if ( .not. nested .and. grid_type<3 ) then
      if( js==1 ) then
        do i=ifirst,ilast
           al(i,0) = c1*q(i,-2) + c2*q(i,-1) + c3*q(i,0)
           al(i,1) = 0.5*(((2.*dya(i,0)+dya(i,-1))*q(i,0)-dya(i,0)*q(i,-1))/(dya(i,-1)+dya(i,0))   &
                   +      ((2.*dya(i,1)+dya(i,2))*q(i,1)-dya(i,1)*q(i,2))/(dya(i,1)+dya(i,2)))
           al(i,2) = c3*q(i,1) + c2*q(i,2) + c1*q(i,3)
        enddo
      endif
      if( (je+1)==npy ) then
        do i=ifirst,ilast
         al(i,npy-1) = c1*q(i,npy-3) + c2*q(i,npy-2) + c3*q(i,npy-1)
         al(i,npy) = 0.5*(((2.*dya(i,npy-1)+dya(i,npy-2))*q(i,npy-1)-dya(i,npy-1)*q(i,npy-2))/(dya(i,npy-2)+dya(i,npy-1))  &
                   +      ((2.*dya(i,npy)+dya(i,npy+1))*q(i,npy)-dya(i,npy)*q(i,npy+1))/(dya(i,npy)+dya(i,npy+1)))
         al(i,npy+1) = c3*q(i,npy) + c2*q(i,npy+1) + c1*q(i,npy+2)
        enddo
      endif
   endif

   if ( jord<0 ) then
      do j=js-1, je+2
         do i=ifirst,ilast
            al(i,j) = max(0., al(i,j))
         enddo
      enddo
   endif

   if ( mord==1 ) then
       do j=js-1,je+1
          do i=ifirst,ilast
             bl(i,j) = al(i,j  ) - q(i,j)
             br(i,j) = al(i,j+1) - q(i,j)
             b0(i,j) = bl(i,j) + br(i,j)
             smt5(i,j) = abs(lim_fac*b0(i,j)) < abs(bl(i,j)-br(i,j))
          enddo
       enddo
       do j=js,je+1
!DEC$ VECTOR ALWAYS
          do i=ifirst,ilast
             if ( c(i,j) > 0. ) then
                  fx1(i) = (1.-c(i,j))*(br(i,j-1) - c(i,j)*b0(i,j-1))
                  flux(i,j) = q(i,j-1)
             else
                  fx1(i) = (1.+c(i,j))*(bl(i,j) + c(i,j)*b0(i,j))
                  flux(i,j) = q(i,j)
             endif
             if (smt5(i,j-1).or.smt5(i,j)) flux(i,j) = flux(i,j) + fx1(i) 
          enddo
       enddo

   elseif ( mord==2 ) then   ! Perfectly linear scheme
! Diffusivity: ord2 < ord5 < ord3 < ord4 < ord6  < ord7

      do j=js,je+1
!DEC$ VECTOR ALWAYS
         do i=ifirst,ilast
            xt = c(i,j)
            if ( xt > 0. ) then
                 qtmp = q(i,j-1)
                 flux(i,j) = qtmp + (1.-xt)*(al(i,j)-qtmp-xt*(al(i,j-1)+al(i,j)-(qtmp+qtmp)))
            else
                 qtmp = q(i,j)
                 flux(i,j) = qtmp + (1.+xt)*(al(i,j)-qtmp+xt*(al(i,j)+al(i,j+1)-(qtmp+qtmp)))
            endif
         enddo
      enddo

   elseif ( mord==3 ) then

        do j=js-1,je+1
           do i=ifirst,ilast
              bl(i,j) = al(i,j  ) - q(i,j)
              br(i,j) = al(i,j+1) - q(i,j)
              b0(i,j) = bl(i,j) + br(i,j)
                   x0 = abs(b0(i,j)) 
                   xt = abs(bl(i,j)-br(i,j))
              smt5(i,j) =    x0 < xt
              smt6(i,j) = 3.*x0 < xt
           enddo
        enddo
        do j=js,je+1
           do i=ifirst,ilast
              fx1(i) = 0.
              xt1(i) = c(i,j)
              hi5(i) = smt5(i,j-1) .and. smt5(i,j)
              hi6(i) = smt6(i,j-1) .or.  smt6(i,j)
           enddo
           do i=ifirst,ilast
              if ( xt1(i) > 0. ) then
                   if( hi6(i) ) then
                       fx1(i) = br(i,j-1) - xt1(i)*b0(i,j-1)
                   elseif ( hi5(i) ) then ! both up-downwind sides are noisy; 2nd order, piece-wise linear
                       fx1(i) = sign(min(abs(bl(i,j-1)),abs(br(i,j-1))),br(i,j-1))
                   endif
                   flux(i,j) = q(i,j-1) + (1.-xt1(i))*fx1(i)
              else
                   if( hi6(i) ) then
                       fx1(i) = bl(i,j) + xt1(i)*b0(i,j)
                   elseif ( hi5(i) ) then ! both up-downwind sides are noisy; 2nd order, piece-wise linear
                       fx1(i) = sign(min(abs(bl(i,j)),abs(br(i,j))), bl(i,j))
                   endif
                   flux(i,j) = q(i,j) + (1.+xt1(i))*fx1(i)
              endif
           enddo
        enddo

   elseif ( mord==4 ) then

        do j=js-1,je+1
           do i=ifirst,ilast
              bl(i,j) = al(i,j  ) - q(i,j)
              br(i,j) = al(i,j+1) - q(i,j)
              b0(i,j) = bl(i,j) + br(i,j)
                   x0 = abs(b0(i,j)) 
                   xt = abs(bl(i,j)-br(i,j))
              smt5(i,j) =    x0 < xt
              smt6(i,j) = 3.*x0 < xt
           enddo
        enddo
        do j=js,je+1
           do i=ifirst,ilast
              xt1(i) = c(i,j)
              hi5(i) = smt5(i,j-1) .and. smt5(i,j)
              hi6(i) = smt6(i,j-1) .or.  smt6(i,j)
              hi5(i) = hi5(i) .or. hi6(i)
           enddo
!DEC$ VECTOR ALWAYS
           do i=ifirst,ilast
                if ( xt1(i) > 0. ) then
                     fx1(i) = (1.-xt1(i))*(br(i,j-1) - xt1(i)*b0(i,j-1))
                     flux(i,j) = q(i,j-1)
                else
                     fx1(i) = (1.+xt1(i))*(bl(i,j) + xt1(i)*b0(i,j))
                     flux(i,j) = q(i,j)
                endif
                if ( hi5(i) ) flux(i,j) = flux(i,j) + fx1(i) 
           enddo
        enddo

   else  ! mord=5,6,7
       if ( mord==5 ) then
          do j=js-1,je+1
             do i=ifirst,ilast
                bl(i,j) = al(i,j  ) - q(i,j)
                br(i,j) = al(i,j+1) - q(i,j)
                b0(i,j) = bl(i,j) + br(i,j)
                smt5(i,j) = bl(i,j)*br(i,j) < 0.
             enddo
          enddo
       else
          do j=js-1,je+1
             do i=ifirst,ilast
                bl(i,j) = al(i,j  ) - q(i,j)
                br(i,j) = al(i,j+1) - q(i,j)
                b0(i,j) = bl(i,j) + br(i,j)
                smt5(i,j) = 3.*abs(b0(i,j)) < abs(bl(i,j)-br(i,j))
             enddo
          enddo
       endif

       do j=js,je+1
!DEC$ VECTOR ALWAYS
          do i=ifirst,ilast
             if ( c(i,j) > 0. ) then
                  fx1(i) = (1.-c(i,j))*(br(i,j-1) - c(i,j)*b0(i,j-1))
                  flux(i,j) = q(i,j-1)
             else
                  fx1(i) = (1.+c(i,j))*(bl(i,j) + c(i,j)*b0(i,j))
                  flux(i,j) = q(i,j)
             endif
             if (smt5(i,j-1).or.smt5(i,j)) flux(i,j) = flux(i,j) + fx1(i) 
          enddo
       enddo

   endif
   return

else
! Monotonic constraints:
! ord = 8: PPM with Lin's PPM fast monotone constraint
! ord > 8: PPM with Lin's modification of Huynh 2nd constraint
 
  do j=js-2,je+2
     do i=ifirst,ilast
             xt = 0.25*(q(i,j+1) - q(i,j-1))
        dm(i,j) = sign(min(abs(xt), max(q(i,j-1), q(i,j), q(i,j+1)) - q(i,j),   &
                           q(i,j) - min(q(i,j-1), q(i,j), q(i,j+1))), xt)
     enddo
  enddo
  do j=js1,je1+1
     do i=ifirst,ilast
        al(i,j) = 0.5*(q(i,j-1)+q(i,j)) + r3*(dm(i,j-1) - dm(i,j))
     enddo
  enddo

  if ( jord==8 ) then
       do j=js1,je1
          do i=ifirst,ilast
             xt = 2.*dm(i,j)
             bl(i,j) = -sign(min(abs(xt), abs(al(i,j)-q(i,j))),   xt)
             br(i,j) =  sign(min(abs(xt), abs(al(i,j+1)-q(i,j))), xt)
          enddo
       enddo
  elseif ( jord==11 ) then
       do j=js1,je1
          do i=ifirst,ilast
             xt = ppm_fac*dm(i,j)
             bl(i,j) = -sign(min(abs(xt), abs(al(i,j)-q(i,j))),   xt)
             br(i,j) =  sign(min(abs(xt), abs(al(i,j+1)-q(i,j))), xt)
          enddo
       enddo
  else
       do j=js1-2,je1+1
          do i=ifirst,ilast
             dq(i,j) = 2.*(q(i,j+1) - q(i,j))
          enddo
       enddo
       do j=js1,je1
          do i=ifirst,ilast
             bl(i,j) = al(i,j  ) - q(i,j)
             br(i,j) = al(i,j+1) - q(i,j)
             if ( abs(dm(i,j-1))+abs(dm(i,j))+abs(dm(i,j+1)) < near_zero ) then
                  bl(i,j) = 0.
                  br(i,j) = 0.
             elseif( abs(3.*(bl(i,j)+br(i,j))) > abs(bl(i,j)-br(i,j)) ) then
                  pmp_2 = dq(i,j-1)
                  lac_2 = pmp_2 - 0.75*dq(i,j-2)
                  br(i,j) = min(max(0.,pmp_2,lac_2), max(br(i,j), min(0.,pmp_2,lac_2)))
                  pmp_1 = -dq(i,j) 
                  lac_1 = pmp_1 + 0.75*dq(i,j+1)
                  bl(i,j) = min(max(0.,pmp_1,lac_1), max(bl(i,j), min(0.,pmp_1,lac_1)))
             endif
          enddo
       enddo
  endif
  if ( jord==9 .or. jord==13 ) then
! Positive definite constraint:
     do j=js1,je1
        call pert_ppm(ilast-ifirst+1, q(ifirst,j), bl(ifirst,j), br(ifirst,j), 0)
     enddo
  endif

  if (.not. nested .and. grid_type<3) then
    if( js==1 ) then
      do i=ifirst,ilast
         bl(i,0) = s14*dm(i,-1) + s11*(q(i,-1)-q(i,0))

         xt = 0.5*(((2.*dya(i,0)+dya(i,-1))*q(i,0)-dya(i,0)*q(i,-1))/(dya(i,-1)+dya(i,0))   &
            +      ((2.*dya(i,1)+dya(i,2))*q(i,1)-dya(i,1)*q(i,2))/(dya(i,1)+dya(i,2)))
!        if ( jord==8 .or. jord==10 ) then
            xt = max(xt, min(q(i,-1),q(i,0),q(i,1),q(i,2)))
            xt = min(xt, max(q(i,-1),q(i,0),q(i,1),q(i,2)))
!        endif
         br(i,0) = xt - q(i,0)
         bl(i,1) = xt - q(i,1)

         xt = s15*q(i,1) + s11*q(i,2) - s14*dm(i,2)
         br(i,1) = xt - q(i,1)
         bl(i,2) = xt - q(i,2)

         br(i,2) = al(i,3) - q(i,2)
      enddo
      call pert_ppm(3*(ilast-ifirst+1), q(ifirst,0), bl(ifirst,0), br(ifirst,0), 1)
    endif
    if( (je+1)==npy ) then
      do i=ifirst,ilast
         bl(i,npy-2) = al(i,npy-2) - q(i,npy-2)

         xt = s15*q(i,npy-1) + s11*q(i,npy-2) + s14*dm(i,npy-2)
         br(i,npy-2) = xt - q(i,npy-2)
         bl(i,npy-1) = xt - q(i,npy-1)

         xt = 0.5*(((2.*dya(i,npy-1)+dya(i,npy-2))*q(i,npy-1)-dya(i,npy-1)*q(i,npy-2))/(dya(i,npy-2)+dya(i,npy-1))  &
            +      ((2.*dya(i,npy)+dya(i,npy+1))*q(i,npy)-dya(i,npy)*q(i,npy+1))/(dya(i,npy)+dya(i,npy+1)))
!        if ( jord==8 .or. jord==10 ) then
            xt = max(xt, min(q(i,npy-2),q(i,npy-1),q(i,npy),q(i,npy+1)))
            xt = min(xt, max(q(i,npy-2),q(i,npy-1),q(i,npy),q(i,npy+1)))
!        endif
         br(i,npy-1) = xt - q(i,npy-1)
         bl(i,npy  ) = xt - q(i,npy)

         br(i,npy) = s11*(q(i,npy+1)-q(i,npy)) - s14*dm(i,npy+1)
     enddo
     call pert_ppm(3*(ilast-ifirst+1), q(ifirst,npy-2), bl(ifirst,npy-2), br(ifirst,npy-2), 1)
    endif
 end if

endif

  do j=js,je+1
     do i=ifirst,ilast
        if( c(i,j)>0. ) then
           flux(i,j) = q(i,j-1) + (1.-c(i,j))*(br(i,j-1)-c(i,j)*(bl(i,j-1)+br(i,j-1)))
        else
           flux(i,j) = q(i,j  ) + (1.+c(i,j))*(bl(i,j  )+c(i,j)*(bl(i,j)+br(i,j)))
        endif
     enddo
  enddo

 end subroutine yppm



 subroutine mp_ghost_ew(im, jm, km, nq, ifirst, ilast, jfirst, jlast, &
                              kfirst, klast, ng_w, ng_e, ng_s, ng_n, q_ghst, q)
!
! !INPUT PARAMETERS:
      integer, intent(in):: im, jm, km, nq
      integer, intent(in):: ifirst, ilast
      integer, intent(in):: jfirst, jlast
      integer, intent(in):: kfirst, klast
      integer, intent(in):: ng_e      !< eastern  zones to ghost
      integer, intent(in):: ng_w      !< western  zones to ghost
      integer, intent(in):: ng_s      !< southern zones to ghost
      integer, intent(in):: ng_n      !< northern zones to ghost
      real, intent(inout):: q_ghst(ifirst-ng_w:ilast+ng_e,jfirst-ng_s:jlast+ng_n,kfirst:klast,nq)
      real, optional, intent(in):: q(ifirst:ilast,jfirst:jlast,kfirst:klast,nq)
!
! !DESCRIPTION:
!
!     Ghost 4d east/west 
!
! !REVISION HISTORY:
!    2005.08.22   Putman
!
!EOP
!------------------------------------------------------------------------------
!BOC
      integer :: i,j,k,n

      if (present(q)) then
         q_ghst(ifirst:ilast,jfirst:jlast,kfirst:klast,1:nq) = &
              q(ifirst:ilast,jfirst:jlast,kfirst:klast,1:nq)
      endif

!      Assume Periodicity in X-dir and not overlapping
      do n=1,nq
         do k=kfirst,klast
            do j=jfirst-ng_s,jlast+ng_n
               do i=1, ng_w
                  q_ghst(ifirst-i,j,k,n) = q_ghst(ilast-i+1,j,k,n)
               enddo
               do i=1, ng_e
                  q_ghst(ilast+i,j,k,n) = q_ghst(ifirst+i-1,j,k,n)
               enddo
            enddo
         enddo
      enddo

 end subroutine mp_ghost_ew



 subroutine pert_ppm(im, a0, al, ar, iv)
 integer, intent(in):: im
 integer, intent(in):: iv
 real, intent(in)   :: a0(im)
 real, intent(inout):: al(im), ar(im)
! Local:
 real a4, da1, da2, a6da, fmin
 integer i
 real, parameter:: r12 = 1./12.

!-----------------------------------
! Optimized PPM in perturbation form:
!-----------------------------------

 if ( iv==0 ) then
! Positive definite constraint
    do i=1,im
     if ( a0(i) <= 0. ) then
          al(i) = 0.
          ar(i) = 0.
     else
        a4 = -3.*(ar(i) + al(i))
       da1 =      ar(i) - al(i)
      if( abs(da1) < -a4 ) then
         fmin = a0(i) + 0.25/a4*da1**2 + a4*r12
         if( fmin < 0. ) then
             if( ar(i)>0. .and. al(i)>0. ) then
                 ar(i) = 0.
                 al(i) = 0.
             elseif( da1 > 0. ) then
                 ar(i) = -2.*al(i)
             else
                 al(i) = -2.*ar(i)
             endif
         endif
      endif
     endif
    enddo
 else
! Standard PPM constraint
    do i=1,im
       if ( al(i)*ar(i) < 0. ) then
            da1 = al(i) - ar(i)
            da2 = da1**2
            a6da = 3.*(al(i)+ar(i))*da1
! abs(a6da) > da2 --> 3.*abs(al+ar) > abs(al-ar)
            if( a6da < -da2 ) then
                ar(i) = -2.*al(i)
            elseif( a6da > da2 ) then
                al(i) = -2.*ar(i)
            endif
       else
! effect of dm=0 included here
            al(i) = 0.
            ar(i) = 0.
       endif
  enddo
 endif

 end subroutine pert_ppm


 subroutine deln_flux(nord,is,ie,js,je, npx, npy, damp, q, fx, fy, gridstruct, bd, mass )
!> Del-n damping for the cell-mean values (A grid)
!------------------
!> nord = 0:   del-2
!> nord = 1:   del-4
!> nord = 2:   del-6
!> nord = 3:   del-8 --> requires more ghosting than current
!------------------
   type(fv_grid_bounds_type), intent(IN) :: bd
   integer, intent(in):: nord            !< del-n
   integer, intent(in):: is,ie,js,je, npx, npy
   real, intent(in):: damp
   real, intent(in):: q(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng)  ! q ghosted on input
   type(fv_grid_type), intent(IN), target :: gridstruct
   real, optional, intent(in):: mass(bd%isd:bd%ied, bd%jsd:bd%jed)  ! q ghosted on input
! diffusive fluxes:
   real, intent(inout):: fx(bd%is:bd%ie+1,bd%js:bd%je), fy(bd%is:bd%ie,bd%js:bd%je+1)
! local:
   real fx2(bd%isd:bd%ied+1,bd%jsd:bd%jed), fy2(bd%isd:bd%ied,bd%jsd:bd%jed+1)
   real d2(bd%isd:bd%ied,bd%jsd:bd%jed)
   real damp2
   integer i,j, n, nt, i1, i2, j1, j2

#ifdef USE_SG
   real, pointer, dimension(:,:)   :: dx, dy, rdxc, rdyc
   real, pointer, dimension(:,:,:) :: sin_sg
   dx       => gridstruct%dx     
   dy       => gridstruct%dy     
   rdxc     => gridstruct%rdxc   
   rdyc     => gridstruct%rdyc   
   sin_sg   => gridstruct%sin_sg 
#endif

   i1 = is-1-nord;    i2 = ie+1+nord
   j1 = js-1-nord;    j2 = je+1+nord

   if ( .not. present(mass) ) then
     do j=j1, j2
        do i=i1,i2
           d2(i,j) = damp*q(i,j)
        enddo
     enddo
   else
     do j=j1, j2
        do i=i1,i2
           d2(i,j) = q(i,j)
        enddo
     enddo
   endif

   if( nord>0 ) call copy_corners(d2, npx, npy, 1, gridstruct%nested, bd, &
      gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)

   do j=js-nord,je+nord
      do i=is-nord,ie+nord+1
#ifdef USE_SG
         fx2(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*(d2(i-1,j)-d2(i,j))*rdxc(i,j)
#else
         fx2(i,j) = gridstruct%del6_v(i,j)*(d2(i-1,j)-d2(i,j))
#endif
      enddo
   enddo

   if( nord>0 ) call copy_corners(d2, npx, npy, 2, gridstruct%nested, bd, &
      gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)
   do j=js-nord,je+nord+1
         do i=is-nord,ie+nord
#ifdef USE_SG
            fy2(i,j) = 0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))*dx(i,j)*(d2(i,j-1)-d2(i,j))*rdyc(i,j)
#else
            fy2(i,j) = gridstruct%del6_u(i,j)*(d2(i,j-1)-d2(i,j))
#endif
         enddo
   enddo

   if ( nord>0 ) then

!----------
! high-order
!----------

   do n=1, nord

      nt = nord-n

      do j=js-nt-1,je+nt+1
         do i=is-nt-1,ie+nt+1
            d2(i,j) = (fx2(i,j)-fx2(i+1,j)+fy2(i,j)-fy2(i,j+1))*gridstruct%rarea(i,j)
         enddo
      enddo

      call copy_corners(d2, npx, npy, 1, gridstruct%nested, bd, &
           gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)
      do j=js-nt,je+nt
         do i=is-nt,ie+nt+1
#ifdef USE_SG
            fx2(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*(d2(i,j)-d2(i-1,j))*rdxc(i,j)
#else
            fx2(i,j) = gridstruct%del6_v(i,j)*(d2(i,j)-d2(i-1,j))
#endif
         enddo
      enddo

      call copy_corners(d2, npx, npy, 2, gridstruct%nested, bd, &
           gridstruct%sw_corner, gridstruct%se_corner, gridstruct%nw_corner, gridstruct%ne_corner)
      do j=js-nt,je+nt+1
            do i=is-nt,ie+nt
#ifdef USE_SG
               fy2(i,j) = 0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))*dx(i,j)*(d2(i,j)-d2(i,j-1))*rdyc(i,j)
#else
               fy2(i,j) = gridstruct%del6_u(i,j)*(d2(i,j)-d2(i,j-1))
#endif
            enddo
      enddo
   enddo

   endif

!---------------------------------------------
! Add the diffusive fluxes to the flux arrays:
!---------------------------------------------

   if ( present(mass) ) then
! Apply mass weighting to diffusive fluxes:
        damp2 = 0.5*damp
        do j=js,je
           do i=is,ie+1
              fx(i,j) = fx(i,j) + damp2*(mass(i-1,j)+mass(i,j))*fx2(i,j)
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              fy(i,j) = fy(i,j) + damp2*(mass(i,j-1)+mass(i,j))*fy2(i,j)
           enddo
        enddo
   else
        do j=js,je
           do i=is,ie+1
              fx(i,j) = fx(i,j) + fx2(i,j)
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              fy(i,j) = fy(i,j) + fy2(i,j)
           enddo
        enddo
   endif

 end subroutine deln_flux


end module tp_core_mod
