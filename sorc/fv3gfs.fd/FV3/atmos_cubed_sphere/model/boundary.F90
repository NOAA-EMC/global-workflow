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

!>@brief The module 'boundary' contains utility routines for grid nesting
!! and boundary conditions.

module boundary_mod

! Modules Included:
! <table>
! <tr>
!    <th>Module Name</th>
!     <th>Functions Included</th>
!  </tr>
!   <tr>
!     <td>constants_mod</td>
!     <td>grav</td>
!   <tr>
!     <td>fv_arrays_mod</td>
!     <td>fv_atmos_type, fv_nest_BC_type_3D, fv_grid_bounds_type</td>
!   </tr>
!   <tr>
!     <td>fv_mp_mod</td>
!     <td>ng, isc,jsc,iec,jec, isd,jsd,ied,jed, is,js,ie,je, is_master, mp_bcst</td>
!   </tr>
!   <tr>
!     <td>fv_timing_mod</td>
!     <td>timing_on, timing_off</td>
!   </tr>
!   <tr>
!     <td>mpp_mod/td>
!     <td>mpp_error, FATAL, mpp_sum, mpp_sync, mpp_npes, mpp_broadcast, WARNING, mpp_pe,
!         mpp_send, mpp_recv</td>
!   </tr>
!   <tr>
!     <td>mpp_domains_mod/td>
!     <td>mpp_get_compute_domain, mpp_get_data_domain, mpp_get_global_domain,
!         ENTER, CORNER, NORTH, EAST,nest_domain_type, WEST, SOUTH, 
!         mpp_get_C2F_index, mpp_update_nest_fine,mpp_global_field, mpp_get_pelist
!         mpp_get_F2C_index, mpp_update_nest_coarse</td>
!   </tr>
! </table>

  use fv_mp_mod,         only: ng, isc,jsc,iec,jec, isd,jsd,ied,jed, is,js,ie,je, is_master
  use constants_mod,     only: grav

  use mpp_domains_mod,    only: mpp_get_compute_domain, mpp_get_data_domain, mpp_get_global_domain
  use mpp_domains_mod,    only: CENTER, CORNER, NORTH, EAST
  use mpp_domains_mod,    only: mpp_global_field, mpp_get_pelist
  use mpp_mod,            only: mpp_error, FATAL, mpp_sum, mpp_sync, mpp_npes, mpp_broadcast, WARNING, mpp_pe

  use fv_mp_mod,          only: mp_bcst
  use fv_arrays_mod,      only: fv_atmos_type, fv_nest_BC_type_3D, fv_grid_bounds_type
  use mpp_mod,            only: mpp_send, mpp_recv
  use fv_timing_mod,      only: timing_on, timing_off
  use mpp_domains_mod, only : nest_domain_type, WEST, SOUTH
  use mpp_domains_mod, only : mpp_get_C2F_index, mpp_update_nest_fine
  use mpp_domains_mod, only : mpp_get_F2C_index, mpp_update_nest_coarse
  !use mpp_domains_mod, only : mpp_get_domain_shift

  implicit none
  public extrapolation_BC
  public nested_grid_bc, update_coarse_grid
  public fill_nested_grid, nested_grid_BC_apply_intT
  public nested_grid_BC_send, nested_grid_BC_recv, nested_grid_BC_save_proc

!>@briefThe interface 'nested_grid_BC' includes subroutines 'nested_grid_BC_2d' and 'nested_grid_BC_3d' 
!! that fetch coarse-grid data, interpolate it to nested-grid boundary cells,
!! apply the interpolated data directly to the boundary halo cells without saving the datatype.
  interface nested_grid_BC 
     module procedure nested_grid_BC_2d
     module procedure nested_grid_BC_mpp
     module procedure nested_grid_BC_mpp_send
     module procedure nested_grid_BC_2D_mpp
     module procedure nested_grid_BC_3d
  end interface

!>@brief The interface 'fill_nested_grid' includes subroutines 'fill_nested_grid_2d' and 'fill_nested_grid_3d' 
!! that fill nested-grid data with interpolated data from the coarse grid.
!>@details This is one method to create a new nested grid, and may be useful when cold-starting.
  interface fill_nested_grid
     module procedure fill_nested_grid_2d
     module procedure fill_nested_grid_3d
  end interface

!>@brief The interface'update_coarse_grid_mpp'contains subroutines that
!! fetch data from the nested grid and 
!! interpolate it to the coarse grid using the method described by
!! \cite harris2013two.
  interface update_coarse_grid
     module procedure update_coarse_grid_mpp
     module procedure update_coarse_grid_mpp_2d
  end interface

contains

!>@brief The subroutine 'extrapolation_BC' performs linear extrapolation into the halo region.
!Not to be confused with extrapolated-in-time nested BCs
  subroutine extrapolation_BC(q, istag, jstag, npx, npy, bd, pd_in, debug_in)

    type(fv_grid_bounds_type), intent(IN) :: bd
    integer, intent(in) :: istag, jstag, npx, npy
    real, intent(inout), dimension(bd%isd:bd%ied+istag, bd%jsd:bd%jed+jstag) :: q
    logical, intent(in), OPTIONAL :: pd_in, debug_in

    integer :: i,j, istart, iend, jstart, jend
    logical :: pd, debug

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

    istart = max(isd, 1)
    iend = min(ied,npx-1)
    jstart = max(jsd, 1)
    jend = min(jed,npy-1)

    !Positive-definite extrapolation: shift from linear extrapolation to zero-gradient when the extrapolated value turns negative.
    if (present(pd_in)) then
       pd    = pd_in
    else
       pd = .false.
    end if

    if (present(debug_in)) then
       debug = debug_in
    else
       debug = .false.
    end if
    
    if (is == 1) then

       if (pd) then

          do j = jstart,jend+jstag
          do i = 0,isd,-1

             if  (real(i) <= 1. - q(1,j)/(q(2,j) - q(1,j) + 1.e-12) .and. q(1,j) < q(2,j)) then
                q(i,j) = q(i+1,j)
             else
                q(i,j) = real(2-i)*q(1,j) - real(1-i)*q(2,j)
             end if

          end do
          end do

       else

          do j = jstart,jend+jstag
          do i = 0,isd,-1

             q(i,j) = real(2-i)*q(1,j) - real(1-i)*q(2,j)

          end do
          end do

       end if
       
    end if

    if (js == 1) then

       if (pd) then

          do j = 0,jsd,-1
          do i = istart,iend+istag

             if  (real(j) <= 1. - q(i,1)/(q(i,2) - q(i,1) + 1.e-12) .and. q(i,1) < q(i,2)) then
                q(i,j) = q(i,j+1)
             else
                q(i,j) = real(2-j)*q(i,1) - real(1-j)*q(i,2)
             end if

          end do
          end do

       else

          do j = 0,jsd,-1
          do i = istart,iend+istag

             q(i,j) = real(2-j)*q(i,1) - real(1-j)*q(i,2)

          end do
          end do

       end if
       
    end if

    if (ie == npx - 1) then

       if (pd) then

          do j=jstart,jend+jstag
          do i=ie+1+istag,ied+istag
             
             if (real(i) >= ie+istag + q(ie+istag,j)/(q(ie+istag-1,j)-q(ie+istag,j)+1.e-12) .and. &
                  q(ie+istag,j) < q(ie+istag-1,j)) then
                q(i,j) = q(i-1,j)
             else
                q(i,j) = real(i - (ie+istag-1))*q(ie+istag,j) + real((ie+istag) - i)*q(ie+istag-1,j)
             end if

          end do
          end do

       else

          do j=jstart,jend+jstag
          do i=ie+1+istag,ied+istag

             q(i,j) = real(i - (ie+istag-1))*q(ie+istag,j) + real((ie+istag) - i)*q(ie+istag-1,j)

          end do
          end do

       end if

    end if

    if (je == npy - 1) then

       if (pd) then

          do j=je+1+jstag,jed+jstag
          do i=istart,iend+istag

             if (real(j) >= je+jstag + q(i,je+jstag)/(q(i,je+jstag-1)-q(i,je+jstag)+1.e-12) .and. &
                  q(i,je+jstag-1) > q(i,je+jstag)) then
                q(i,j) = q(i,j-1)
             else
                q(i,j) = real(j - (je+jstag-1))*q(i,je+jstag) + real((je+jstag) - j)*q(i,je+jstag-1)
             end if

          end do
          end do

       else

          do j=je+1+jstag,jed+jstag
          do i=istart,iend+istag

             q(i,j) = real(j - (je+jstag-1))*q(i,je+jstag) + real((je+jstag) - j)*q(i,je+jstag-1)

          end do
          end do

       end if

    end if


    !CORNERS: Average of extrapolations

    if (is == 1 .and. js == 1) then

       if (pd) then

          do j=0,jsd,-1
          do i=0,isd,-1

             if (real(i) <= 1. - q(1,j)/(q(2,j) - q(1,j) + 1.e-12) .and. q(2,j) > q(1,j)) then
                q(i,j) = 0.5*q(i+1,j)
             else
                q(i,j) = 0.5*( real(2-i)*q(1,j) - real(1-i)*q(2,j) )
             end if
             
             if  (real(j) <= 1. - q(i,1)/(q(i,2) - q(i,1) + 1.e-12) .and. q(i,2) > q(i,1)) then
                q(i,j) = q(i,j) + 0.5*q(i,j+1)

             else
                q(i,j) = q(i,j) + 0.5*(real(2-j)*q(i,1) - real(1-j)*q(i,2))
             end if

          end do
          end do

       else

          do j=jsd,0
          do i=isd,0
                 
             q(i,j) = 0.5*( real(2-i)*q(1,j) - real(1-i)*q(2,j) ) + &
                  0.5*( real(2-j)*q(i,1) - real(1-j)*q(i,2) )
             
          end do
          end do

       end if

    end if

    if (is == 1 .and. je == npy-1) then

       if (pd) then

          do j=je+1+jstag,jed+jstag
          do i=0,isd,-1

             if (real(i) <= 1. - q(1,j)/(q(2,j) - q(1,j) + 1.e-12) .and. q(2,j) > q(1,j)) then
                q(i,j) = 0.5*q(i+1,j)
             else
                q(i,j) = 0.5*( real(2-i)*q(1,j) - real(1-i)*q(2,j) )
             end if

             !'Unary plus' removed to appease IBM compiler
             !if (real(j) >= je+jstag - q(i,je+jstag)/(q(i,je+jstag-1)-q(i,je+jstag)+1.e-12) .and. &
             if (real(j) >= je+jstag - q(i,je+jstag)/(q(i,je+jstag-1)-q(i,je+jstag)+1.e-12) .and. &
                  q(i,je+jstag-1) > q(i,je+jstag) ) then
                q(i,j) = q(i,j) + 0.5*q(i,j-1)
             else
                q(i,j) = q(i,j) + 0.5*( real(j - (je+jstag-1))*q(i,je+jstag) + real((je+jstag) - j)*q(i,je+jstag-1) )
             end if
                 
          end do
          end do

       else

          do j=je+1+jstag,jed+jstag
          do i=isd,0
                 
             q(i,j) = 0.5*( real(2-i)*q(1,j) - real(1-i)*q(2,j) ) + &
                      0.5*( real(j - (je+jstag-1))*q(i,je+jstag) + real((je+jstag) - j)*q(i,je+jstag-1) )
          
          end do
          end do

       end if

    end if

    if (ie == npx-1 .and. je == npy-1) then

       if (pd) then

          do j=je+1+jstag,jed+jstag
          do i=ie+1+istag,ied+istag
                 
             
             if (real(i) >= ie+istag + q(ie+istag,j)/(q(ie+istag-1,j)-q(ie+istag,j)+1.e-12) .and. &
                  q(ie+istag-1,j) > q(ie+istag,j)) then
                q(i,j) = 0.5*q(i-1,j)
             else
                q(i,j) = 0.5*(real(i - (ie+istag-1))*q(ie+istag,j) + real((ie+istag) - i)*q(ie+istag-1,j))
             end if

             if (real(j) >= je+jstag + q(i,je+jstag)/(q(i,je+jstag-1)-q(i,je+jstag)+1.e-12) .and. &
                  q(i,je+jstag-1) > q(i,je+jstag)) then
                q(i,j) = q(i,j) + 0.5*q(i,j-1)
             else
                q(i,j) = q(i,j) + 0.5*( real(j - (je+jstag-1))*q(i,je+jstag) + real((je+jstag) - j)*q(i,je+jstag-1) )
             end if
          
          end do
          end do

       else

          do j=je+1+jstag,jed+jstag
          do i=ie+1+istag,ied+istag
                 
             q(i,j) = 0.5*( real(i - (ie+istag-1))*q(ie+istag,j) + real((ie+istag) - i)*q(ie+istag-1,j) ) + &
                      0.5*( real(j - (je+jstag-1))*q(i,je+jstag) + real((je+jstag) - j)*q(i,je+jstag-1) )
          
          end do
          end do

       end if

    end if

    if (ie == npx-1 .and. js == 1) then

       if (pd) then

          do j=0,jsd,-1
          do i=ie+1+istag,ied+istag
                 
             
             if (real(i) >= ie+istag + q(ie+istag,j)/(q(ie+istag-1,j)-q(ie+istag,j)+1.e-12) .and. &
                  q(ie+istag-1,j) > q(ie+istag,j)) then
                q(i,j) = 0.5*q(i-1,j)
             else
                q(i,j) = 0.5*(real(i - (ie+istag-1))*q(ie+istag,j) + real((ie+istag) - i)*q(ie+istag-1,j))
             end if
             
             if  (real(j) <= 1. - q(i,1)/(q(i,2) - q(i,1) + 1.e-12) .and. &
                  q(i,2) > q(i,1)) then
                q(i,j) = q(i,j) + 0.5*q(i,j+1)
             else
                q(i,j) = q(i,j) + 0.5*(real(2-j)*q(i,1) - real(1-j)*q(i,2))
             end if
          
          end do
          end do


       else

          do j=jsd,0
          do i=ie+1+istag,ied+istag
                 
             q(i,j) = 0.5*( real(i - (ie+istag-1))*q(ie+istag,j) + real((ie+istag) - i)*q(ie+istag-1,j) ) + &
                      0.5*( real(2-j)*q(i,1) - real(1-j)*q(i,2) )
          
          end do
          end do

       end if

    end if


  end subroutine extrapolation_BC

  subroutine fill_nested_grid_2D(var_nest, var_coarse, ind, wt, istag, jstag,  &
      isg, ieg, jsg, jeg, bd, istart_in, iend_in, jstart_in, jend_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag), intent(INOUT) :: var_nest
   real, dimension(isg:ieg+istag,jsg:jeg+jstag), intent(IN) :: var_coarse 
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, isg, ieg, jsg, jeg
   integer, intent(IN), OPTIONAL :: istart_in, iend_in, jstart_in, jend_in

   integer :: i,j, ic, jc
   integer :: istart, iend, jstart, jend

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

   if (present(istart_in)) then
      istart = istart_in
   else
      istart = isd
   end if
   if (present(iend_in)) then
      iend = iend_in+istag
   else
      iend = ied+istag
   end if

   if (present(jstart_in)) then
      jstart = jstart_in
   else
      jstart = jsd
   end if
   if (present(jend_in)) then
      jend = jend_in+jstag
   else
      jend = jed+jstag
   end if

   do j=jstart,jend
   do i=istart,iend

      ic = ind(i,j,1)
      jc = ind(i,j,2)

      var_nest(i,j) = &
           wt(i,j,1)*var_coarse(ic,  jc) +  &
           wt(i,j,2)*var_coarse(ic,  jc+1) +  &
           wt(i,j,3)*var_coarse(ic+1,jc+1) +  &
           wt(i,j,4)*var_coarse(ic+1,jc) 

   end do
   end do

 end subroutine fill_nested_grid_2D
 
  subroutine fill_nested_grid_3D(var_nest, var_coarse, ind, wt, istag, jstag,  &
      isg, ieg, jsg, jeg, npz, bd, istart_in, iend_in, jstart_in, jend_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,npz), intent(INOUT) :: var_nest
   real, dimension(isg:ieg+istag,jsg:jeg+jstag,npz), intent(IN) :: var_coarse
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, isg, ieg, jsg, jeg, npz
   integer, intent(IN), OPTIONAL :: istart_in, iend_in, jstart_in, jend_in

   integer :: i,j, ic, jc, k
   integer :: istart, iend, jstart, jend

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

   if (present(istart_in)) then
      istart = istart_in
   else
      istart = isd
   end if
   if (present(iend_in)) then
      iend = iend_in+istag
   else
      iend = ied+istag
   end if

   if (present(jstart_in)) then
      jstart = jstart_in
   else
      jstart = jsd
   end if
   if (present(jend_in)) then
      jend = jend_in+jstag
   else
      jend = jed+jstag
   end if

   do k=1,npz

   do j=jstart,jend
   do i=istart,iend

      ic = ind(i,j,1)
      jc = ind(i,j,2)

      var_nest(i,j,k) = &
           wt(i,j,1)*var_coarse(ic,  jc,  k) +  &
           wt(i,j,2)*var_coarse(ic,  jc+1,k) +  &
           wt(i,j,3)*var_coarse(ic+1,jc+1,k) +  &
           wt(i,j,4)*var_coarse(ic+1,jc,  k) 

   end do
   end do

   end do

 end subroutine fill_nested_grid_3D
 
 subroutine nested_grid_BC_mpp(var_nest, var_coarse, nest_domain, ind, wt, istag, jstag, &
      npx, npy, npz, bd, isg, ieg, jsg, jeg, nstep_in, nsplit_in, proc_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,npz), intent(INOUT) :: var_nest
   real, dimension(isg:ieg+istag,jsg:jeg+jstag,npz), intent(IN) :: var_coarse
   type(nest_domain_type), intent(INOUT) :: nest_domain
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, npx, npy, npz, isg, ieg, jsg, jeg
   integer, intent(IN), OPTIONAL :: nstep_in, nsplit_in
   logical, intent(IN), OPTIONAL :: proc_in

   integer                      :: isw_f, iew_f, jsw_f, jew_f, isw_c, iew_c, jsw_c, jew_c
   integer                      :: ise_f, iee_f, jse_f, jee_f, ise_c, iee_c, jse_c, jee_c
   integer                      :: iss_f, ies_f, jss_f, jes_f, iss_c, ies_c, jss_c, jes_c
   integer                      :: isn_f, ien_f, jsn_f, jen_f, isn_c, ien_c, jsn_c, jen_c
   real,    allocatable         :: wbuffer(:,:,:)
   real,    allocatable         :: ebuffer(:,:,:)
   real,    allocatable         :: sbuffer(:,:,:)
   real,    allocatable         :: nbuffer(:,:,:)

   integer :: i,j, ic, jc, istart, iend, k

   integer :: position
   logical :: process

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

   if (PRESENT(proc_in)) then
      process = proc_in
   else
      process = .true.
   endif

   if (istag == 1 .and. jstag == 1) then
      position = CORNER
   else if (istag == 0 .and. jstag == 1) then
      position = NORTH
   else if (istag == 1 .and. jstag == 0) then
      position = EAST
   else
      position = CENTER
   end if

   call mpp_get_C2F_index(nest_domain, isw_f, iew_f, jsw_f, jew_f, isw_c, iew_c, jsw_c, jew_c, &
        WEST,  position=position)
   call mpp_get_C2F_index(nest_domain, ise_f, iee_f, jse_f, jee_f, ise_c, iee_c, jse_c, jee_c, &
        EAST,  position=position)
   call mpp_get_C2F_index(nest_domain, iss_f, ies_f, jss_f, jes_f, iss_c, ies_c, jss_c, jes_c, &
        SOUTH,  position=position)
   call mpp_get_C2F_index(nest_domain, isn_f, ien_f, jsn_f, jen_f, isn_c, ien_c, jsn_c, jen_c, &
        NORTH,  position=position)

   if( iew_c .GE. isw_c .AND. jew_c .GE. jsw_c ) then
      allocate(wbuffer(isw_c:iew_c, jsw_c:jew_c,npz))
   else
      allocate(wbuffer(1,1,1))
   endif
   wbuffer = 0

   if( iee_c .GE. ise_c .AND. jee_c .GE. jse_c ) then
      allocate(ebuffer(ise_c:iee_c, jse_c:jee_c,npz))
   else
      allocate(ebuffer(1,1,1))
   endif
   ebuffer = 0

   if( ies_c .GE. iss_c .AND. jes_c .GE. jss_c ) then
      allocate(sbuffer(iss_c:ies_c, jss_c:jes_c,npz))
   else
      allocate(sbuffer(1,1,1))
   endif
   sbuffer = 0

   if( ien_c .GE. isn_c .AND. jen_c .GE. jsn_c ) then
      allocate(nbuffer(isn_c:ien_c, jsn_c:jen_c,npz))
   else
      allocate(nbuffer(1,1,1))
   endif
   nbuffer = 0


       call timing_on ('COMM_TOTAL')
   call mpp_update_nest_fine(var_coarse, nest_domain, wbuffer, sbuffer, ebuffer, nbuffer,  position=position)
       call timing_off('COMM_TOTAL')

   if (process) then

   if (is == 1) then
      do k=1,npz
      do j=jsd,jed+jstag
         do i=isd,0

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*wbuffer(ic,  jc,  k) +  &
                 wt(i,j,2)*wbuffer(ic,  jc+1,k) +  &
                 wt(i,j,3)*wbuffer(ic+1,jc+1,k) +  &
                 wt(i,j,4)*wbuffer(ic+1,jc,  k) 

         end do
      end do
      end do
   end if

   if (js == 1) then

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

      do k=1,npz
      do j=jsd,0
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*sbuffer(ic,  jc,  k) +  &
                 wt(i,j,2)*sbuffer(ic,  jc+1,k) +  &
                 wt(i,j,3)*sbuffer(ic+1,jc+1,k) +  &
                 wt(i,j,4)*sbuffer(ic+1,jc,  k) 

         end do
      end do
      end do
   end if


   if (ie == npx-1) then
      do k=1,npz
      do j=jsd,jed+jstag
         do i=npx+istag,ied+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*ebuffer(ic,  jc,  k) +  &
                 wt(i,j,2)*ebuffer(ic,  jc+1,k) +  &
                 wt(i,j,3)*ebuffer(ic+1,jc+1,k) +  &
                 wt(i,j,4)*ebuffer(ic+1,jc,  k) 

         end do
      end do
      end do
   end if

   if (je == npy-1) then

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

      do k=1,npz
      do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*nbuffer(ic,  jc,  k) +  &
                 wt(i,j,2)*nbuffer(ic,  jc+1,k) +  &
                 wt(i,j,3)*nbuffer(ic+1,jc+1,k) +  &
                 wt(i,j,4)*nbuffer(ic+1,jc,  k) 

         end do
      end do
      end do
   end if

   endif !process

   deallocate(wbuffer, ebuffer, sbuffer, nbuffer)

 end subroutine nested_grid_BC_mpp

 subroutine nested_grid_BC_mpp_send(var_coarse, nest_domain, istag, jstag)

   real, dimension(:,:,:), intent(IN) :: var_coarse
   type(nest_domain_type), intent(INOUT) :: nest_domain
   integer, intent(IN) :: istag, jstag

   real,    allocatable         :: wbuffer(:,:,:)
   real,    allocatable         :: ebuffer(:,:,:)
   real,    allocatable         :: sbuffer(:,:,:)
   real,    allocatable         :: nbuffer(:,:,:)

   integer :: i,j, ic, jc, istart, iend, k

   integer :: position


   if (istag == 1 .and. jstag == 1) then
      position = CORNER
   else if (istag == 0 .and. jstag == 1) then
      position = NORTH
   else if (istag == 1 .and. jstag == 0) then
      position = EAST
   else
      position = CENTER
   end if


      allocate(wbuffer(1,1,1))

      allocate(ebuffer(1,1,1))

      allocate(sbuffer(1,1,1))

      allocate(nbuffer(1,1,1))


       call timing_on ('COMM_TOTAL')
   call mpp_update_nest_fine(var_coarse, nest_domain, wbuffer, sbuffer, ebuffer, nbuffer,  position=position)
       call timing_off('COMM_TOTAL')


   deallocate(wbuffer, ebuffer, sbuffer, nbuffer)

 end subroutine nested_grid_BC_mpp_send

 subroutine nested_grid_BC_2D_mpp(var_nest, var_coarse, nest_domain, ind, wt, istag, jstag, &
      npx, npy, bd, isg, ieg, jsg, jeg, nstep_in, nsplit_in, proc_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag), intent(INOUT) :: var_nest
   real, dimension(isg:ieg+istag,jsg:jeg+jstag), intent(IN) :: var_coarse
   type(nest_domain_type), intent(INOUT) :: nest_domain
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, npx, npy, isg, ieg, jsg, jeg
   integer, intent(IN), OPTIONAL :: nstep_in, nsplit_in
   logical, intent(IN), OPTIONAL :: proc_in

   integer                      :: isw_f, iew_f, jsw_f, jew_f, isw_c, iew_c, jsw_c, jew_c
   integer                      :: ise_f, iee_f, jse_f, jee_f, ise_c, iee_c, jse_c, jee_c
   integer                      :: iss_f, ies_f, jss_f, jes_f, iss_c, ies_c, jss_c, jes_c
   integer                      :: isn_f, ien_f, jsn_f, jen_f, isn_c, ien_c, jsn_c, jen_c
   real,    allocatable         :: wbuffer(:,:)
   real,    allocatable         :: ebuffer(:,:)
   real,    allocatable         :: sbuffer(:,:)
   real,    allocatable         :: nbuffer(:,:)

   integer :: i,j, ic, jc, istart, iend, k

   integer :: position
   logical :: process

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

   if (PRESENT(proc_in)) then
      process = proc_in
   else
      process = .true.
   endif

   if (istag == 1 .and. jstag == 1) then
      position = CORNER
   else if (istag == 0 .and. jstag == 1) then
      position = NORTH
   else if (istag == 1 .and. jstag == 0) then
      position = EAST
   else
      position = CENTER
   end if

   call mpp_get_C2F_index(nest_domain, isw_f, iew_f, jsw_f, jew_f, isw_c, iew_c, jsw_c, jew_c, &
        WEST,  position=position)
   call mpp_get_C2F_index(nest_domain, ise_f, iee_f, jse_f, jee_f, ise_c, iee_c, jse_c, jee_c, &
        EAST,  position=position)
   call mpp_get_C2F_index(nest_domain, iss_f, ies_f, jss_f, jes_f, iss_c, ies_c, jss_c, jes_c, &
        SOUTH,  position=position)
   call mpp_get_C2F_index(nest_domain, isn_f, ien_f, jsn_f, jen_f, isn_c, ien_c, jsn_c, jen_c, &
        NORTH,  position=position)

   if( iew_c .GE. isw_c .AND. jew_c .GE. jsw_c ) then
      allocate(wbuffer(isw_c:iew_c, jsw_c:jew_c))
   else
      allocate(wbuffer(1,1))
   endif
   wbuffer = 0

   if( iee_c .GE. ise_c .AND. jee_c .GE. jse_c ) then
      allocate(ebuffer(ise_c:iee_c, jse_c:jee_c))
   else
      allocate(ebuffer(1,1))
   endif
   ebuffer = 0

   if( ies_c .GE. iss_c .AND. jes_c .GE. jss_c ) then
      allocate(sbuffer(iss_c:ies_c, jss_c:jes_c))
   else
      allocate(sbuffer(1,1))
   endif
   sbuffer = 0

   if( ien_c .GE. isn_c .AND. jen_c .GE. jsn_c ) then
      allocate(nbuffer(isn_c:ien_c, jsn_c:jen_c))
   else
      allocate(nbuffer(1,1))
   endif
   nbuffer = 0

       call timing_on ('COMM_TOTAL')
   call mpp_update_nest_fine(var_coarse, nest_domain, wbuffer, sbuffer, ebuffer, nbuffer,  position=position)
       call timing_off('COMM_TOTAL')

   if (process) then

   if (is == 1) then
      do j=jsd,jed+jstag
         do i=isd,0

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*wbuffer(ic,  jc) +  &
                 wt(i,j,2)*wbuffer(ic,  jc+1) +  &
                 wt(i,j,3)*wbuffer(ic+1,jc+1) +  &
                 wt(i,j,4)*wbuffer(ic+1,jc) 

         end do
      end do
   end if

   if (js == 1) then

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

      do j=jsd,0
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*sbuffer(ic,  jc) +  &
                 wt(i,j,2)*sbuffer(ic,  jc+1) +  &
                 wt(i,j,3)*sbuffer(ic+1,jc+1) +  &
                 wt(i,j,4)*sbuffer(ic+1,jc) 

         end do
      end do
   end if


   if (ie == npx-1) then
      do j=jsd,jed+jstag
         do i=npx+istag,ied+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*ebuffer(ic,  jc) +  &
                 wt(i,j,2)*ebuffer(ic,  jc+1) +  &
                 wt(i,j,3)*ebuffer(ic+1,jc+1) +  &
                 wt(i,j,4)*ebuffer(ic+1,jc) 

         end do
      end do
   end if

   if (je == npy-1) then

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

      do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*nbuffer(ic,  jc) +  &
                 wt(i,j,2)*nbuffer(ic,  jc+1) +  &
                 wt(i,j,3)*nbuffer(ic+1,jc+1) +  &
                 wt(i,j,4)*nbuffer(ic+1,jc) 

         end do
      end do
   end if

   endif !process

   deallocate(wbuffer, ebuffer, sbuffer, nbuffer)

 end subroutine nested_grid_BC_2D_mpp

 subroutine nested_grid_BC_2D(var_nest, var_coarse, ind, wt, istag, jstag, &
      npx, npy, bd, isg, ieg, jsg, jeg, nstep_in, nsplit_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag), intent(INOUT) :: var_nest
   real, dimension(isg:ieg+istag,jsg:jeg+jstag), intent(IN) :: var_coarse
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, npx, npy, isg, ieg, jsg, jeg
   integer, intent(IN), OPTIONAL :: nstep_in, nsplit_in

   integer :: nstep, nsplit

   integer :: i,j, ic, jc, istart, iend

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

   if ( .not. present(nstep_in) .or. .not. present(nsplit_in) ) then
      nstep = 1
      nsplit = 2
   else
      nstep = nstep_in
      nsplit = nsplit_in
   end if

   if (is == 1) then
      do j=jsd,jed+jstag
         do i=isd,0

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*var_coarse(ic,  jc) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc) 

         end do
      end do
   end if

   if (js == 1) then

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

      do j=jsd,0
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*var_coarse(ic,  jc) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc) 

         end do
      end do
   end if


   if (ie == npx-1) then
      do j=jsd,jed+jstag
         do i=npx+istag,ied+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*var_coarse(ic,  jc) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc) 

         end do
      end do
   end if

   if (je == npy-1) then

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


      do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j) = &
                 wt(i,j,1)*var_coarse(ic,  jc) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc) 

         end do
      end do
   end if



 end subroutine nested_grid_BC_2D

 subroutine nested_grid_BC_3D(var_nest, var_coarse, ind, wt, istag, jstag, &
      npx, npy, npz, bd, isg, ieg, jsg, jeg, nstep_in, nsplit_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,npz), intent(INOUT) :: var_nest
   real, dimension(isg:ieg+istag,jsg:jeg+jstag,npz), intent(IN) :: var_coarse
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, npx, npy, isg, ieg, jsg, jeg, npz
   integer, intent(IN), OPTIONAL :: nstep_in, nsplit_in

   integer :: nstep, nsplit

   integer :: i,j, ic, jc, istart, iend, k

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

   if ( .not. present(nstep_in) .or. .not. present(nsplit_in) ) then
      nstep = 1
      nsplit = 2
   else
      nstep = nstep_in
      nsplit = nsplit_in
   end if

   if (is == 1) then
      do k=1,npz
      do j=jsd,jed+jstag
         do i=isd,0

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*var_coarse(ic,  jc,  k) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1,k) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1,k) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc,  k) 

         end do
      end do
      end do
   end if

   if (js == 1) then

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

      do k=1,npz
      do j=jsd,0
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*var_coarse(ic,  jc,  k) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1,k) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1,k) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc,  k) 

         end do
      end do
      end do
   end if


   if (ie == npx-1) then
      do k=1,npz
      do j=jsd,jed+jstag
         do i=npx+istag,ied+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*var_coarse(ic,  jc,  k) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1,k) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1,k) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc,  k) 

         end do
      end do
      end do
   end if

   if (je == npy-1) then

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

      do k=1,npz
      do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)

            var_nest(i,j,k) = &
                 wt(i,j,1)*var_coarse(ic,  jc,  k) +  &
                 wt(i,j,2)*var_coarse(ic,  jc+1,k) +  &
                 wt(i,j,3)*var_coarse(ic+1,jc+1,k) +  &
                 wt(i,j,4)*var_coarse(ic+1,jc,  k) 

         end do
      end do
      end do
   end if



 end subroutine nested_grid_BC_3D

!>@brief The subroutine 'nested_grid_BC_send' sends coarse-grid data to create boundary conditions.
 subroutine nested_grid_BC_send(var_coarse, nest_domain, istag, jstag)

   real, dimension(:,:,:), intent(IN) :: var_coarse
   type(nest_domain_type), intent(INOUT) :: nest_domain
   integer, intent(IN) :: istag, jstag

   integer                      :: position

   real :: wbuffer(1,1,1)
   real :: ebuffer(1,1,1)
   real :: sbuffer(1,1,1)
   real :: nbuffer(1,1,1)


   if (istag == 1 .and. jstag == 1) then
      position = CORNER
   else if (istag == 0 .and. jstag == 1) then
      position = NORTH
   else if (istag == 1 .and. jstag == 0) then
      position = EAST
   else
      position = CENTER
   end if

       call timing_on ('COMM_TOTAL')
   call mpp_update_nest_fine(var_coarse, nest_domain, wbuffer, sbuffer, ebuffer, nbuffer,  position=position)
       call timing_off('COMM_TOTAL')

 end subroutine nested_grid_BC_send

!>@briefThe subroutine 'nested_grid_BC_recv' receives coarse-grid data to create boundary conditions.
 subroutine nested_grid_BC_recv(nest_domain, istag, jstag, npz, &
      bd, nest_BC_buffers)

   type(fv_grid_bounds_type), intent(IN) :: bd
   type(nest_domain_type), intent(INOUT) :: nest_domain
   integer, intent(IN) :: istag, jstag, npz

   type(fv_nest_BC_type_3d), intent(INOUT), target :: nest_BC_buffers
   
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,npz) :: var_coarse_dummy

   integer                      :: position

   integer                      :: isw_f, iew_f, jsw_f, jew_f, isw_c, iew_c, jsw_c, jew_c
   integer                      :: ise_f, iee_f, jse_f, jee_f, ise_c, iee_c, jse_c, jee_c
   integer                      :: iss_f, ies_f, jss_f, jes_f, iss_c, ies_c, jss_c, jes_c
   integer                      :: isn_f, ien_f, jsn_f, jen_f, isn_c, ien_c, jsn_c, jen_c

   integer :: i,j, k

   if (istag == 1 .and. jstag == 1) then
      position = CORNER
   else if (istag == 0 .and. jstag == 1) then
      position = NORTH
   else if (istag == 1 .and. jstag == 0) then
      position = EAST
   else
      position = CENTER
   end if

   if (.not. allocated(nest_BC_buffers%west_t1) ) then

      call mpp_get_C2F_index(nest_domain, isw_f, iew_f, jsw_f, jew_f, isw_c, iew_c, jsw_c, jew_c, &
           WEST,  position=position)
      call mpp_get_C2F_index(nest_domain, ise_f, iee_f, jse_f, jee_f, ise_c, iee_c, jse_c, jee_c, &
           EAST,  position=position)
      call mpp_get_C2F_index(nest_domain, iss_f, ies_f, jss_f, jes_f, iss_c, ies_c, jss_c, jes_c, &
           SOUTH,  position=position)
      call mpp_get_C2F_index(nest_domain, isn_f, ien_f, jsn_f, jen_f, isn_c, ien_c, jsn_c, jen_c, &
           NORTH,  position=position)

      if( iew_c .GE. isw_c .AND. jew_c .GE. jsw_c ) then
         If (.not. allocated(nest_BC_buffers%west_t1)) allocate(nest_BC_buffers%west_t1(isw_c:iew_c, jsw_c:jew_c,npz))
         !compatible with first touch principle
         do k=1,npz
         do j=jsw_c,jew_c
         do i=isw_c,iew_c
            nest_BC_buffers%west_t1(i,j,k) = 0.
         enddo
         enddo
         enddo         
      else
         allocate(nest_BC_buffers%west_t1(1,1,1))
         nest_BC_buffers%west_t1(1,1,1) = 0.
      endif

      if( iee_c .GE. ise_c .AND. jee_c .GE. jse_c ) then
         If (.not. allocated(nest_BC_buffers%east_t1)) allocate(nest_BC_buffers%east_t1(ise_c:iee_c, jse_c:jee_c,npz))
         do k=1,npz
         do j=jse_c,jee_c
         do i=ise_c,iee_c
            nest_BC_buffers%east_t1(i,j,k) = 0.
         enddo
         enddo
         enddo
      else
         allocate(nest_BC_buffers%east_t1(1,1,1))
         nest_BC_buffers%east_t1(1,1,1) = 0.
      endif

      if( ies_c .GE. iss_c .AND. jes_c .GE. jss_c ) then
         If (.not. allocated(nest_BC_buffers%south_t1)) allocate(nest_BC_buffers%south_t1(iss_c:ies_c, jss_c:jes_c,npz))
         do k=1,npz
         do j=jss_c,jes_c
         do i=iss_c,ies_c
            nest_BC_buffers%south_t1(i,j,k) = 0.
         enddo
         enddo
         enddo
      else
         allocate(nest_BC_buffers%south_t1(1,1,1))
         nest_BC_buffers%south_t1(1,1,1) = 0.
      endif

      if( ien_c .GE. isn_c .AND. jen_c .GE. jsn_c ) then
         If (.not. allocated(nest_BC_buffers%north_t1)) allocate(nest_BC_buffers%north_t1(isn_c:ien_c, jsn_c:jen_c,npz))
         do k=1,npz
         do j=jsn_c,jen_c
         do i=isn_c,ien_c
            nest_BC_buffers%north_t1(i,j,k) = 0.
         enddo
         enddo
         enddo
      else
         allocate(nest_BC_buffers%north_t1(1,1,1))
         nest_BC_buffers%north_t1(1,1,1) = 0
      endif

   endif

       call timing_on ('COMM_TOTAL')
   call mpp_update_nest_fine(var_coarse_dummy, nest_domain, nest_BC_buffers%west_t1, nest_BC_buffers%south_t1, nest_BC_buffers%east_t1, nest_BC_buffers%north_t1,  position=position)
       call timing_off('COMM_TOTAL')

 end subroutine nested_grid_BC_recv

!>@brief The subroutine 'nested_grid_BC_save_proc' saves data received by 'nested_grid_BC_recv' 
!! into the datatype 'fv_nest_BC_type'.
 subroutine nested_grid_BC_save_proc(nest_domain, ind, wt, istag, jstag, &
      npx, npy, npz, bd, nest_BC, nest_BC_buffers, pd_in)

   type(fv_grid_bounds_type), intent(IN) :: bd
   type(nest_domain_type), intent(INOUT) :: nest_domain
   integer, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,2), intent(IN) :: ind
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,4), intent(IN) :: wt
   integer, intent(IN) :: istag, jstag, npx, npy, npz
   logical, intent(IN), OPTIONAL :: pd_in

   !!NOTE: if declaring an ALLOCATABLE array with intent(OUT), the resulting dummy array
   !!      will NOT be allocated! This goes for allocatable members of derived types as well.
   type(fv_nest_BC_type_3d), intent(INOUT), target :: nest_BC, nest_BC_buffers
   
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag,npz) :: var_coarse_dummy

   real, dimension(:,:,:), pointer :: var_east, var_west, var_south, var_north
   real, dimension(:,:,:), pointer :: buf_east, buf_west, buf_south, buf_north

   integer                      :: position


   integer :: i,j, k, ic, jc, istart, iend
   logical :: process, pd = .false.

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


   if (present(pd_in)) then
      pd = pd_in
   else
      pd = .false.
   endif


      var_east  => nest_BC%east_t1
      var_west  => nest_BC%west_t1
      var_north => nest_BC%north_t1
      var_south => nest_BC%south_t1

   buf_east  => nest_BC_buffers%east_t1
   buf_west  => nest_BC_buffers%west_t1
   buf_north => nest_BC_buffers%north_t1
   buf_south => nest_BC_buffers%south_t1
   ! ?buffer has uninterpolated coarse-grid data; need to perform interpolation ourselves
   !To do this more securely, instead of using is/etc we could use the fine-grid indices defined above
   if (is == 1  ) then

!$NO-MP parallel do default(none) shared(npz,isd,ied,jsd,jed,jstag,ind,var_west,wt,buf_west) private(ic,jc)
      do k=1,npz
      do j=jsd,jed+jstag
         do i=isd,0

            ic = ind(i,j,1)
            jc = ind(i,j,2)


            var_west(i,j,k) = &
                 wt(i,j,1)*buf_west(ic,  jc,k) +  &
                 wt(i,j,2)*buf_west(ic,  jc+1,k) +  &
                 wt(i,j,3)*buf_west(ic+1,jc+1,k) +  &
                 wt(i,j,4)*buf_west(ic+1,jc,k) 

         end do
      end do
      end do

      if (pd) then
!$NO-MP parallel do default(none) shared(npz,jsd,jed,jstag,isd,var_west,nest_BC)
         do k=1,npz
         do j=jsd,jed+jstag
         do i=isd,0

            var_west(i,j,k) = max(var_west(i,j,k), 0.5*nest_BC%west_t0(i,j,k))
         end do
         end do
         end do         
      endif

   end if

   if (js == 1  ) then

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

!$NO-MP parallel do default(none) shared(npz,istart,iend,jsd,jed,istag,ind,var_south,wt,buf_south) private(ic,jc)
      do k=1,npz
      do j=jsd,0
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)


            var_south(i,j,k) = &
                 wt(i,j,1)*buf_south(ic,  jc,k) +  &
                 wt(i,j,2)*buf_south(ic,  jc+1,k) +  &
                 wt(i,j,3)*buf_south(ic+1,jc+1,k) +  &
                 wt(i,j,4)*buf_south(ic+1,jc,k) 

         end do
      end do
      end do

      if (pd) then
!$NO-MP parallel do default(none) shared(npz,jsd,jed,istart,iend,istag,var_south,nest_BC)
         do k=1,npz
         do j=jsd,0
         do i=istart,iend+istag

            var_south(i,j,k) = max(var_south(i,j,k), 0.5*nest_BC%south_t0(i,j,k))

         end do
         end do
         end do         
      endif

   end if


   if (ie == npx-1 ) then

!$NO-MP parallel do default(none) shared(npx,npz,isd,ied,jsd,jed,istag,jstag,ind,var_east,wt,buf_east) private(ic,jc)
      do k=1,npz
      do j=jsd,jed+jstag
         do i=npx+istag,ied+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)


            var_east(i,j,k) = &
                 wt(i,j,1)*buf_east(ic,  jc,k) +  &
                 wt(i,j,2)*buf_east(ic,  jc+1,k) +  &
                 wt(i,j,3)*buf_east(ic+1,jc+1,k) +  &
                 wt(i,j,4)*buf_east(ic+1,jc,k) 

         end do
      end do
      end do

      if (pd) then
!$NO-MP parallel do default(none) shared(npx,npz,jsd,jed,istag,jstag,ied,var_east,nest_BC)
         do k=1,npz
         do j=jsd,jed+jstag
         do i=npx+istag,ied+istag

            var_east(i,j,k) = max(var_east(i,j,k), 0.5*nest_BC%east_t0(i,j,k))

         end do
         end do
         end do         
      endif

   end if

   if (je == npy-1 ) then

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

!$NO-MP parallel do default(none) shared(npy,npz,istart,iend,jsd,jed,istag,jstag,ind,var_north,wt,buf_north) private(ic,jc)
      do k=1,npz
      do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            ic = ind(i,j,1)
            jc = ind(i,j,2)


            var_north(i,j,k) = &
                 wt(i,j,1)*buf_north(ic,  jc,k) +  &
                 wt(i,j,2)*buf_north(ic,  jc+1,k) +  &
                 wt(i,j,3)*buf_north(ic+1,jc+1,k) +  &
                 wt(i,j,4)*buf_north(ic+1,jc,k) 

         end do
      end do
      end do

      if (pd) then
!$NO-MP parallel do default(none) shared(npy,npz,jsd,jed,istart,iend,istag,jstag,ied,var_north,nest_BC)
         do k=1,npz
         do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            var_north(i,j,k) = max(var_north(i,j,k), 0.5*nest_BC%north_t0(i,j,k))

         end do
         end do
         end do         
      endif

   end if

 end subroutine nested_grid_BC_save_proc


  ! A NOTE ON BCTYPE: currently only an interpolation BC is implemented, 
  ! bctype >= 2 currently correspond
  ! to a flux BC on the tracers ONLY, which is implemented in fv_tracer.

!>@brief The subroutine 'nested_grid_BC_apply_intT' performs linear interpolation or 
!! extrapolation in time for saved BC data, then applies the interlpolated
!! data to nested-grid boundary cells.
 subroutine nested_grid_BC_apply_intT(var_nest, istag, jstag, &
      npx, npy, npz, bd, step, split, &
      BC, bctype)

   type(fv_grid_bounds_type), intent(IN) :: bd
   real, dimension(bd%isd:bd%ied+istag,bd%jsd:bd%jed+jstag, npz), intent(INOUT) :: var_nest
   integer, intent(IN) :: istag, jstag, npx, npy, npz
   real, intent(IN) :: split, step
   integer, intent(IN) :: bctype
   
   type(fv_nest_BC_type_3D), intent(IN), target :: BC
   real, pointer, dimension(:,:,:) :: var_t0, var_t1

   integer :: i,j, istart, iend, k
   real :: denom

   logical, save :: printdiag = .true.

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

   denom = 1./split
   if (is == 1  ) then
      var_t0 => BC%west_t0
      var_t1 => BC%west_t1
      do k=1,npz
      do j=jsd,jed+jstag
      do i=isd,0
            var_nest(i,j,k) = (var_t0(i,j,k)*(split-step) + step*var_t1(i,j,k))*denom
      end do

         end do
      end do
   end if

   if (js == 1  ) then

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

      var_t0 => BC%south_t0
      var_t1 => BC%south_t1
      do k=1,npz
      do j=jsd,0
         do i=istart,iend+istag

            var_nest(i,j,k) = (var_t0(i,j,k)*(split-step) + step*var_t1(i,j,k))*denom
      end do
      end do
      end do
   end if


   if (ie == npx-1 ) then
      var_t0 => BC%east_t0
      var_t1 => BC%east_t1
      do k=1,npz
      do j=jsd,jed+jstag
         do i=npx+istag,ied+istag
            var_nest(i,j,k) = (var_t0(i,j,k)*(split-step) + step*var_t1(i,j,k))*denom

         end do
      end do
      end do

   end if

   if (je == npy-1 ) then

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

      var_t0 => BC%north_t0
      var_t1 => BC%north_t1
      do k=1,npz
      do j=npy+jstag,jed+jstag
         do i=istart,iend+istag

            var_nest(i,j,k) = (var_t0(i,j,k)*(split-step) + step*var_t1(i,j,k))*denom

         end do
         end do
      end do

   end if


 end subroutine nested_grid_BC_apply_intT

 subroutine update_coarse_grid_mpp_2d(var_coarse, var_nest, nest_domain, ind_update, dx, dy, area, &
      isd_p, ied_p, jsd_p, jed_p, is_n, ie_n, js_n, je_n, isu, ieu, jsu, jeu, npx, npy, &
      istag, jstag, r, nestupdate, upoff, nsponge, parent_proc, child_proc, parent_grid)

   integer, intent(IN) :: isd_p, ied_p, jsd_p, jed_p, is_n, ie_n, js_n, je_n
   integer, intent(IN) :: isu, ieu, jsu, jeu
   integer, intent(IN) :: istag, jstag, r, nestupdate, upoff, nsponge
   integer, intent(IN) :: ind_update(isd_p:ied_p+1,jsd_p:jed_p+1,2)
   integer, intent(IN) :: npx, npy
   real, intent(IN)    :: var_nest(is_n:ie_n+istag,js_n:je_n+jstag)
   real, intent(INOUT) :: var_coarse(isd_p:ied_p+istag,jsd_p:jed_p+jstag)
   real, intent(IN)    :: dx(isd:ied,jsd:jed+1)
   real, intent(IN)    :: dy(isd:ied+1,jsd:jed)
   real, intent(IN)    :: area(isd:ied,jsd:jed)
   logical, intent(IN) :: parent_proc, child_proc
   type(fv_atmos_type), intent(INOUT) :: parent_grid
   type(nest_domain_type), intent(INOUT) :: nest_domain

   real :: var_nest_3d(is_n:ie_n+istag,js_n:je_n+jstag,1)
   real :: var_coarse_3d(isd_p:ied_p+istag,jsd_p:jed_p+jstag,1)

   if (child_proc .and. size(var_nest) > 1) var_nest_3d(is_n:ie_n+istag,js_n:je_n+jstag,1) = var_nest(is_n:ie_n+istag,js_n:je_n+jstag)
   if (parent_proc .and. size(var_coarse) > 1) var_coarse_3d(isd_p:ied_p+istag,jsd_p:jed_p,1) = var_coarse(isd_p:ied_p+istag,jsd_p:jed_p+jstag)

   call update_coarse_grid_mpp(var_coarse_3d, var_nest_3d, &
        nest_domain, ind_update, dx, dy, area, &
        isd_p, ied_p, jsd_p, jed_p, is_n, ie_n, js_n, je_n, &
        isu, ieu, jsu, jeu, npx, npy, 1, &
        istag, jstag, r, nestupdate, upoff, nsponge, &
        parent_proc, child_proc, parent_grid)

   if (size(var_coarse) > 1 .and. parent_proc) var_coarse(isd_p:ied_p+istag,jsd_p:jed_p+jstag) = var_coarse_3d(isd_p:ied_p+istag,jsd_p:jed_p,1)

 end subroutine update_coarse_grid_mpp_2d


  subroutine update_coarse_grid_mpp(var_coarse, var_nest, nest_domain, ind_update, dx, dy, area, &
      isd_p, ied_p, jsd_p, jed_p, is_n, ie_n, js_n, je_n, &
      isu, ieu, jsu, jeu, npx, npy, npz, &
      istag, jstag, r, nestupdate, upoff, nsponge, &
      parent_proc, child_proc, parent_grid)

   !This routine assumes the coarse and nested grids are properly
   ! aligned, and that in particular for odd refinement ratios all
   ! coarse-grid cells (faces) coincide with nested-grid cells (faces)

   integer, intent(IN) :: isd_p, ied_p, jsd_p, jed_p, is_n, ie_n, js_n, je_n
   integer, intent(IN) :: isu, ieu, jsu, jeu
   integer, intent(IN) :: istag, jstag, npx, npy, npz, r, nestupdate, upoff, nsponge
   integer, intent(IN) :: ind_update(isd_p:ied_p+1,jsd_p:jed_p+1,2)
   real, intent(IN)    :: var_nest(is_n:ie_n+istag,js_n:je_n+jstag,npz)
   real, intent(INOUT) :: var_coarse(isd_p:ied_p+istag,jsd_p:jed_p+jstag,npz)
   real, intent(IN)    :: area(isd:ied,jsd:jed)
   real, intent(IN)    :: dx(isd:ied,jsd:jed+1)
   real, intent(IN)    :: dy(isd:ied+1,jsd:jed)
   logical, intent(IN) :: parent_proc, child_proc
   type(fv_atmos_type), intent(INOUT) :: parent_grid
   type(nest_domain_type), intent(INOUT) :: nest_domain

   integer :: in, jn, ini, jnj, s, qr
   integer :: is_c, ie_c, js_c, je_c, is_f, ie_f, js_f, je_f
   integer :: istart, istop, jstart, jstop, ishift, jshift, j, i, k
   real :: val
   real, allocatable, dimension(:,:,:) :: nest_dat
   real ::  var_nest_send(is_n:ie_n+istag,js_n:je_n+jstag,npz)
   integer :: position

   if (istag == 1 .and. jstag == 1) then
      position = CORNER
   else if (istag == 0 .and. jstag == 1) then
      position = NORTH
   else if (istag == 1 .and. jstag == 0) then
      position = EAST
   else
      position = CENTER
   end if

   call mpp_get_F2C_index(nest_domain, is_c, ie_c, js_c, je_c, is_f, ie_f, js_f, je_f, position=position)
   if (ie_f > is_f .and. je_f > js_f) then
      allocate(nest_dat (is_f:ie_f, js_f:je_f,npz))
   else
      allocate(nest_dat(1,1,1))
   endif
   nest_dat = -600

   if (child_proc) then
!! IF an area average (for istag == jstag == 0) or a linear average then multiply in the areas before sending data
   if (istag == 0 .and. jstag == 0) then
      select case (nestupdate)
      case (1,2,6,7,8)
         
!$NO-MP parallel do default(none) shared(npz,js_n,je_n,is_n,ie_n,var_nest_send,var_nest,area)
         do k=1,npz
         do j=js_n,je_n
         do i=is_n,ie_n

            var_nest_send(i,j,k) = var_nest(i,j,k)*area(i,j)

         end do
         end do
         end do

      end select
   else if (istag == 0 .and. jstag > 0) then

      select case (nestupdate) 
      case (1,6,7,8)

!$NO-MP parallel do default(none) shared(npz,js_n,je_n,is_n,ie_n,var_nest_send,var_nest,dx)
         do k=1,npz
         do j=js_n,je_n+1
         do i=is_n,ie_n


            var_nest_send(i,j,k) = var_nest(i,j,k)*dx(i,j)
            
         end do
         end do
         end do

      case default

         call mpp_error(FATAL, 'nestupdate type not implemented')

      end select

   else if (istag > 0 .and. jstag == 0) then
      select case (nestupdate) 

      case (1,6,7,8)   !averaging update; in-line average for face-averaged values instead of areal average

!$NO-MP parallel do default(none) shared(npz,js_n,je_n,is_n,ie_n,var_nest_send,var_nest,dy)
         do k=1,npz
         do j=js_n,je_n
         do i=is_n,ie_n+1

            var_nest_send(i,j,k) = var_nest(i,j,k)*dy(i,j)

         end do
         end do
         end do

      case default

         call mpp_error(FATAL, 'nestupdate type not implemented')

      end select

   else
      
      call mpp_error(FATAL, "Cannot have both nonzero istag and jstag.")

   endif
   endif

      call timing_on('COMM_TOTAL')
   call mpp_update_nest_coarse(var_nest_send, nest_domain, nest_dat, position=position)
      call timing_off('COMM_TOTAL')

   s = r/2 !rounds down (since r > 0)
   qr = r*upoff + nsponge - s

   if (parent_proc .and. .not. (ieu < isu .or. jeu < jsu)) then
   if (istag == 0 .and. jstag == 0) then

      select case (nestupdate) 
      case (1,2,6,7,8) ! 1 = Conserving update on all variables; 2 = conserving update for cell-centered values; 6 = conserving remap-update

!$NO-MP parallel do default(none) shared(npz,jsu,jeu,isu,ieu,ind_update,nest_dat,parent_grid,var_coarse,r) &
!$NO-MP          private(in,jn,val)
         do k=1,npz
         do j=jsu,jeu
         do i=isu,ieu

            in = ind_update(i,j,1)
            jn = ind_update(i,j,2)

!!$            if (in < max(1+qr,is_f) .or. in > min(npx-1-qr-r+1,ie_f) .or. &
!!$                 jn < max(1+qr,js_f) .or. jn > min(npy-1-qr-r+1,je_f)) then
!!$               write(mpp_pe()+3000,'(A, 14I6)') 'SKIP: ', i, j, in, jn, 1+qr, is_f, ie_f, js_f, je_f, npy-1-qr-r+1, isu, ieu, jsu, jeu
!!$               cycle
!!$            endif

            val = 0.
            do jnj=jn,jn+r-1
               do ini=in,in+r-1
                  val = val + nest_dat(ini,jnj,k)
               end do
            end do            

            !var_coarse(i,j,k) = val/r**2.

            !!! CLEANUP: Couldn't rarea and rdx and rdy be built into the weight arrays?
            !!!    Two-way updates do not yet have weights, tho
            var_coarse(i,j,k) = val*parent_grid%gridstruct%rarea(i,j)

         end do
         end do
         end do


      case default

         call mpp_error(FATAL, 'nestupdate type not implemented')


      end select

   else if (istag == 0 .and. jstag > 0) then


      select case (nestupdate) 
      case (1,6,7,8)

!$NO-MP parallel do default(none) shared(npz,jsu,jeu,isu,ieu,ind_update,nest_dat,parent_grid,var_coarse,r) &
!$NO-MP          private(in,jn,val)
         do k=1,npz
         do j=jsu,jeu+1
         do i=isu,ieu

            in = ind_update(i,j,1)
            jn = ind_update(i,j,2)

!!$            if (in < max(1+qr,is_f) .or. in > min(npx-1-qr-r+1,ie_f) .or. &
!!$                 jn < max(1+qr+s,js_f) .or. jn > min(npy-1-qr-s+1,je_f)) then
!!$               write(mpp_pe()+3000,'(A, 14I)') 'SKIP u: ', i, j, in, jn, 1+qr, is_f, ie_f, js_f, je_f, npy-1-qr-s+1, isu, ieu, jsu, jeu
!!$               cycle
!!$            endif

            val = 0.
               do ini=in,in+r-1
                  val = val + nest_dat(ini,jn,k)
               end do

!            var_coarse(i,j,k) = val/r
            var_coarse(i,j,k) = val*parent_grid%gridstruct%rdx(i,j)

         end do
         end do
         end do

      case default

         call mpp_error(FATAL, 'nestupdate type not implemented')

      end select

   else if (istag > 0 .and. jstag == 0) then

      select case (nestupdate) 
      case (1,6,7,8)   !averaging update; in-line average for face-averaged values instead of areal average

!$NO-MP parallel do default(none) shared(npz,jsu,jeu,isu,ieu,ind_update,nest_dat,parent_grid,var_coarse,r) &
!$NO-MP          private(in,jn,val)
         do k=1,npz
         do j=jsu,jeu
         do i=isu,ieu+1

            in = ind_update(i,j,1)
            jn = ind_update(i,j,2)

!!$            if (in < max(1+qr+s,is_f) .or. in > min(npx-1-qr-s+1,ie_f) .or. &
!!$                 jn < max(1+qr,js_f) .or. jn > min(npy-1-qr-r+1,je_f)) then
!!$               write(mpp_pe()+3000,'(A, 14I6)') 'SKIP v: ', i, j, in, jn, 1+qr, is_f, ie_f, js_f, je_f, npx-1-qr-s+1, isu, ieu, jsu, jeu
!!$               cycle
!!$            endif

            val = 0.
            do jnj=jn,jn+r-1
                  val = val + nest_dat(in,jnj,k)
            end do

!            var_coarse(i,j,k) = val/r
            var_coarse(i,j,k) = val*parent_grid%gridstruct%rdy(i,j)

         end do
         end do
         end do

      case default

         call mpp_error(FATAL, 'nestupdate type not implemented')

      end select

   end if


   endif
   deallocate(nest_dat)
   
 end subroutine update_coarse_grid_mpp


   
end module boundary_mod
