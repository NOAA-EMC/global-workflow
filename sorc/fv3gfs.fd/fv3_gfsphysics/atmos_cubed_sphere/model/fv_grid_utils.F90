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
 module fv_grid_utils_mod
 
#include <fms_platform.h>
 use constants_mod,   only: omega, pi=>pi_8, cnst_radius=>radius, R_GRID
 use mpp_mod,         only: FATAL, mpp_error, WARNING
 use external_sst_mod, only: i_sst, j_sst, sst_ncep, sst_anom
 use mpp_domains_mod, only: mpp_update_domains, DGRID_NE, mpp_global_sum
 use mpp_domains_mod, only: BITWISE_EXACT_SUM, domain2d, BITWISE_EFP_SUM
 use mpp_parameter_mod, only: AGRID_PARAM=>AGRID, CGRID_NE_PARAM=>CGRID_NE
 use mpp_parameter_mod, only: CORNER, SCALAR_PAIR

 use fv_arrays_mod,   only: fv_atmos_type, fv_grid_type, fv_grid_bounds_type
 use fv_eta_mod,      only: set_eta
 use fv_mp_mod,       only: ng, is_master
 use fv_mp_mod,       only: mp_reduce_sum, mp_reduce_min, mp_reduce_max
 use fv_mp_mod,       only: fill_corners, XDir, YDir
 use fv_timing_mod,   only: timing_on, timing_off

 implicit none
 private
 logical:: symm_grid
#ifdef NO_QUAD_PRECISION
! 64-bit precision (kind=8)
 integer, parameter:: f_p = selected_real_kind(15)
#else
! Higher precision (kind=16) for grid geometrical factors:
 integer, parameter:: f_p = selected_real_kind(20)
#endif
 real, parameter::  big_number=1.d8
 real, parameter:: tiny_number=1.d-8

 real(kind=R_GRID) :: radius=cnst_radius

 real, parameter:: ptop_min=1.d-8

 public f_p 
 public ptop_min, big_number !CLEANUP: OK to keep since they are constants?
 public cos_angle
 public latlon2xyz, gnomonic_grids, &
        global_mx, unit_vect_latlon,  &
        cubed_to_latlon, c2l_ord2, g_sum, global_qsum, great_circle_dist,  &
        v_prod, get_unit_vect2, project_sphere_v
 public mid_pt_sphere,  mid_pt_cart, vect_cross, grid_utils_init, grid_utils_end, &
        spherical_angle, cell_center2, get_area, inner_prod, fill_ghost, direct_transform,  &
        make_eta_level, expand_cell, cart_to_latlon, intp_great_circle, normalize_vect, &
        dist2side_latlon, spherical_linear_interpolation, get_latlon_vector
 public symm_grid

 INTERFACE fill_ghost
#ifdef OVERLOAD_R4
   MODULE PROCEDURE fill_ghost_r4
#endif
   MODULE PROCEDURE fill_ghost_r8
 END INTERFACE

!---- version number -----
 character(len=128) :: version = '$Id$'
 character(len=128) :: tagname = '$Name$'

 contains

   subroutine grid_utils_init(Atm, npx, npy, npz, non_ortho, grid_type, c2l_order)
! Initialize 2D memory and geometrical factors
      type(fv_atmos_type), intent(inout), target :: Atm
      logical, intent(in):: non_ortho
      integer, intent(in):: npx, npy, npz
      integer, intent(in):: grid_type, c2l_order
!
! Super (composite) grid:
 
!     9---4---8
!     |       |
!     1   5   3
!     |       |
!     6---2---7
 
      real(kind=R_GRID) grid3(3,Atm%bd%isd:Atm%bd%ied+1,Atm%bd%jsd:Atm%bd%jed+1)
      real(kind=R_GRID) p1(3), p2(3), p3(3), p4(3), pp(3), ex(3), ey(3), e1(3), e2(3)
      real(kind=R_GRID) pp1(2), pp2(2), pp3(2)
      real(kind=R_GRID) sin2, tmp1, tmp2
      integer i, j, k, n, ip

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      !Local pointers
      real(kind=R_GRID), pointer, dimension(:,:,:) :: agrid, grid
      real(kind=R_GRID), pointer, dimension(:,:) :: area, area_c
      real(kind=R_GRID), pointer, dimension(:,:) :: sina, cosa, dx, dy, dxc, dyc, dxa, dya
      real, pointer, dimension(:,:) :: del6_u, del6_v
      real, pointer, dimension(:,:) :: divg_u, divg_v
      real, pointer, dimension(:,:) :: cosa_u, cosa_v, cosa_s
      real, pointer, dimension(:,:) :: sina_u, sina_v
      real, pointer, dimension(:,:) :: rsin_u, rsin_v
      real, pointer, dimension(:,:) :: rsina, rsin2
      real, pointer, dimension(:,:,:) :: sin_sg, cos_sg
      real(kind=R_GRID), pointer, dimension(:,:,:) :: ee1, ee2, ec1, ec2
      real(kind=R_GRID), pointer, dimension(:,:,:,:) :: ew, es
      real(kind=R_GRID), pointer, dimension(:,:,:) :: en1, en2
!     real(kind=R_GRID), pointer, dimension(:,:) :: eww, ess
      logical, pointer :: sw_corner, se_corner, ne_corner, nw_corner

      is  = Atm%bd%is
      ie  = Atm%bd%ie
      js  = Atm%bd%js
      je  = Atm%bd%je
      isd = Atm%bd%isd
      ied = Atm%bd%ied
      jsd = Atm%bd%jsd
      jed = Atm%bd%jed

!--- pointers to higher-order precision quantities
      agrid => Atm%gridstruct%agrid_64
      grid  => Atm%gridstruct%grid_64
      area    => Atm%gridstruct%area_64
      area_c  => Atm%gridstruct%area_c_64
      dx     => Atm%gridstruct%dx_64
      dy     => Atm%gridstruct%dy_64
      dxc    => Atm%gridstruct%dxc_64
      dyc    => Atm%gridstruct%dyc_64
      dxa    => Atm%gridstruct%dxa_64
      dya    => Atm%gridstruct%dya_64
      sina   => Atm%gridstruct%sina_64
      cosa   => Atm%gridstruct%cosa_64

      divg_u => Atm%gridstruct%divg_u
      divg_v => Atm%gridstruct%divg_v

      del6_u => Atm%gridstruct%del6_u
      del6_v => Atm%gridstruct%del6_v

      cosa_u => Atm%gridstruct%cosa_u
      cosa_v => Atm%gridstruct%cosa_v
      cosa_s => Atm%gridstruct%cosa_s
      sina_u => Atm%gridstruct%sina_u
      sina_v => Atm%gridstruct%sina_v
      rsin_u => Atm%gridstruct%rsin_u
      rsin_v => Atm%gridstruct%rsin_v
      rsina => Atm%gridstruct%rsina
      rsin2 => Atm%gridstruct%rsin2
      ee1 => Atm%gridstruct%ee1
      ee2 => Atm%gridstruct%ee2
      ec1 => Atm%gridstruct%ec1
      ec2 => Atm%gridstruct%ec2
      ew => Atm%gridstruct%ew
      es => Atm%gridstruct%es
      sin_sg => Atm%gridstruct%sin_sg
      cos_sg => Atm%gridstruct%cos_sg
      en1 => Atm%gridstruct%en1
      en2 => Atm%gridstruct%en2
!     eww => Atm%gridstruct%eww
!     ess => Atm%gridstruct%ess

      sw_corner                     => Atm%gridstruct%sw_corner
      se_corner                     => Atm%gridstruct%se_corner
      ne_corner                     => Atm%gridstruct%ne_corner
      nw_corner                     => Atm%gridstruct%nw_corner

      if ( Atm%flagstruct%do_schmidt .and. abs(Atm%flagstruct%stretch_fac-1.) > 1.E-5 ) then
           Atm%gridstruct%stretched_grid = .true.
           symm_grid = .false.
      else
      Atm%gridstruct%stretched_grid = .false.
           symm_grid = .true.
      endif

      if ( npz == 1 ) then
           Atm%ak(1) = 0.
           Atm%ak(2) = 0.
           Atm%bk(1) = 0.
           Atm%bk(2) = 1.
           Atm%ptop  = 0.
           Atm%ks    = 0
      elseif ( .not. Atm%flagstruct%hybrid_z ) then
! Initialize (ak,bk) for cold start; overwritten with restart file
           call set_eta(npz, Atm%ks, Atm%ptop, Atm%ak, Atm%bk)
           if ( is_master() ) then
              write(*,*) 'Grid_init', npz, Atm%ks, Atm%ptop
              tmp1 = Atm%ak(Atm%ks+1)
              do k=Atm%ks+1,npz
                 tmp1 = max(tmp1, (Atm%ak(k)-Atm%ak(k+1))/max(1.E-9, (Atm%bk(k+1)-Atm%bk(k))) )
              enddo
              write(*,*) 'Hybrid Sigma-P: minimum allowable surface pressure (hpa)=', tmp1/100.
              if ( tmp1 > 420.E2 ) write(*,*) 'Warning: the chosen setting in set_eta can cause instability'
           endif
      endif

! NCEP analysis available from amip-Interp (allocate if needed)
#ifndef DYCORE_SOLO
      if (.not. allocated(sst_ncep)) allocate (sst_ncep(i_sst,j_sst))
      if (.not. allocated(sst_anom)) allocate (sst_anom(i_sst,j_sst))
#endif


      cos_sg(:,:,:) =  big_number
      sin_sg(:,:,:) = tiny_number

      sw_corner = .false.
      se_corner = .false.
      ne_corner = .false.
      nw_corner = .false.

      if (grid_type < 3 .and. .not. Atm%neststruct%nested) then
         if (       is==1 .and.  js==1 )      sw_corner = .true.
         if ( (ie+1)==npx .and.  js==1 )      se_corner = .true.
         if ( (ie+1)==npx .and. (je+1)==npy ) ne_corner = .true.
         if (       is==1 .and. (je+1)==npy ) nw_corner = .true.
      endif

  if ( sw_corner ) then
       tmp1 = great_circle_dist(grid(1,1,1:2), agrid(1,1,1:2))
       tmp2 = great_circle_dist(grid(1,1,1:2), agrid(2,2,1:2))
       write(*,*) 'Corner interpolation coefficient=', tmp2/(tmp2-tmp1)
  endif

  if (grid_type < 3) then
     if ( .not. Atm%neststruct%nested ) then
     call fill_corners(grid(:,:,1), npx, npy, FILL=XDir, BGRID=.true.)
     call fill_corners(grid(:,:,2), npx, npy, FILL=XDir, BGRID=.true.)
     end if

     do j=jsd,jed+1
        do i=isd,ied+1
           call latlon2xyz(grid(i,j,1:2), grid3(1,i,j))
        enddo
     enddo


     call get_center_vect( npx, npy, grid3, ec1, ec2, Atm%bd )

! Fill arbitrary values in the non-existing corner regions:
     if (.not. Atm%neststruct%nested) then
     do k=1,3
        call fill_ghost(ec1(k,:,:), npx, npy, big_number, Atm%bd)
        call fill_ghost(ec2(k,:,:), npx, npy, big_number, Atm%bd)
     enddo
     end if


     do j=jsd,jed
        do i=isd+1,ied
        if ( ( (i<1   .and. j<1  ) .or. (i>npx .and. j<1  ) .or.  &
             (i>npx .and. j>(npy-1)) .or. (i<1   .and. j>(npy-1)) )  .and. .not. Atm%neststruct%nested) then
             ew(1:3,i,j,1:2) = 0.
        else
           call mid_pt_cart( grid(i,j,1:2), grid(i,j+1,1:2), pp)
           if (i==1 .and. .not. Atm%neststruct%nested) then
              call latlon2xyz( agrid(i,j,1:2), p1)
              call vect_cross(p2, pp, p1)
           elseif(i==npx .and. .not. Atm%neststruct%nested) then
              call latlon2xyz( agrid(i-1,j,1:2), p1)
              call vect_cross(p2, p1, pp)
           else
              call latlon2xyz( agrid(i-1,j,1:2), p3)
              call latlon2xyz( agrid(i,  j,1:2), p1)
              call vect_cross(p2, p3, p1)
           endif
           call vect_cross(ew(1:3,i,j,1), p2, pp)
           call normalize_vect(ew(1:3,i,j,1))
!---
           call vect_cross(p1, grid3(1,i,j), grid3(1,i,j+1))
           call vect_cross(ew(1:3,i,j,2), p1, pp)
           call normalize_vect(ew(1:3,i,j,2))
        endif
        enddo
     enddo

     do j=jsd+1,jed
        do i=isd,ied
        if ( ( (i<1   .and. j<1  ) .or. (i>(npx-1) .and. j<1  ) .or.  &
               (i>(npx-1) .and. j>npy) .or. (i<1   .and. j>npy) ) .and. .not. Atm%neststruct%nested) then
             es(1:3,i,j,1:2) = 0.
        else
           call mid_pt_cart(grid(i,j,1:2), grid(i+1,j,1:2), pp)
           if (j==1 .and. .not. Atm%neststruct%nested) then
              call latlon2xyz( agrid(i,j,1:2), p1)
              call vect_cross(p2, pp, p1)
           elseif (j==npy .and. .not. Atm%neststruct%nested) then
              call latlon2xyz( agrid(i,j-1,1:2), p1)
              call vect_cross(p2, p1, pp)
           else 
              call latlon2xyz( agrid(i,j  ,1:2), p1)
              call latlon2xyz( agrid(i,j-1,1:2), p3)
              call vect_cross(p2, p3, p1)
           endif
           call vect_cross(es(1:3,i,j,2), p2, pp)
           call normalize_vect(es(1:3,i,j,2))
!---
           call vect_cross(p3, grid3(1,i,j), grid3(1,i+1,j))
           call vect_cross(es(1:3,i,j,1), p3, pp)
           call normalize_vect(es(1:3,i,j,1))
        endif
        enddo
     enddo

!     9---4---8
!     |       |
!     1   5   3
!     |       |
!     6---2---7

      do j=jsd,jed
         do i=isd,ied
! Testing using spherical formular: exact if coordinate lines are along great circles
! SW corner:
            cos_sg(i,j,6) = cos_angle( grid3(1,i,j), grid3(1,i+1,j), grid3(1,i,j+1) )
! SE corner:
            cos_sg(i,j,7) = -cos_angle( grid3(1,i+1,j), grid3(1,i,j), grid3(1,i+1,j+1) )
! NE corner:
            cos_sg(i,j,8) = cos_angle( grid3(1,i+1,j+1), grid3(1,i+1,j), grid3(1,i,j+1) )
! NW corner:
            cos_sg(i,j,9) = -cos_angle( grid3(1,i,j+1), grid3(1,i,j), grid3(1,i+1,j+1) )
! Mid-points by averaging:
!!!         cos_sg(i,j,1) = 0.5*( cos_sg(i,j,6) + cos_sg(i,j,9) ) 
!!!         cos_sg(i,j,2) = 0.5*( cos_sg(i,j,6) + cos_sg(i,j,7) ) 
!!!         cos_sg(i,j,3) = 0.5*( cos_sg(i,j,7) + cos_sg(i,j,8) ) 
!!!         cos_sg(i,j,4) = 0.5*( cos_sg(i,j,8) + cos_sg(i,j,9) ) 
!!!!!       cos_sg(i,j,5) = 0.25*(cos_sg(i,j,6)+cos_sg(i,j,7)+cos_sg(i,j,8)+cos_sg(i,j,9)) 
! No averaging -----
            call latlon2xyz(agrid(i,j,1:2), p3)   ! righ-hand system consistent with grid3
               call mid_pt3_cart(grid3(1,i,j), grid3(1,i,j+1), p1)
            cos_sg(i,j,1) = cos_angle( p1, p3, grid3(1,i,j+1) )
               call mid_pt3_cart(grid3(1,i,j), grid3(1,i+1,j), p1)
            cos_sg(i,j,2) = cos_angle( p1, grid3(1,i+1,j), p3 )
               call mid_pt3_cart(grid3(1,i+1,j), grid3(1,i+1,j+1), p1)
            cos_sg(i,j,3) = cos_angle( p1, p3, grid3(1,i+1,j) )
               call mid_pt3_cart(grid3(1,i,j+1), grid3(1,i+1,j+1), p1)
            cos_sg(i,j,4) = cos_angle( p1, grid3(1,i,j+1), p3 )
! Center point:
! Using center_vect: [ec1, ec2]
            cos_sg(i,j,5) = inner_prod( ec1(1:3,i,j), ec2(1:3,i,j) )
         enddo
      enddo

      do ip=1,9
         do j=jsd,jed
            do i=isd,ied
               sin_sg(i,j,ip) = min(1.0, sqrt( max(0., 1.-cos_sg(i,j,ip)**2) ) )
            enddo
         enddo
      enddo

! -------------------------------
! For transport operation
! -------------------------------
      if (.not. Atm%neststruct%nested) then
      if ( sw_corner ) then
           do i=-2,0
              sin_sg(0,i,3) = sin_sg(i,1,2) 
              sin_sg(i,0,4) = sin_sg(1,i,1) 
           enddo
      endif
      if ( nw_corner ) then
           do i=npy,npy+2
              sin_sg(0,i,3) = sin_sg(npy-i,npy-1,4) 
           enddo
           do i=-2,0
              sin_sg(i,npy,2) = sin_sg(1,npx+i,1) 
           enddo
      endif
      if ( se_corner ) then
           do j=-2,0
              sin_sg(npx,j,1) = sin_sg(npx-j,1,2) 
           enddo
           do i=npx,npx+2
              sin_sg(i,0,4) = sin_sg(npx-1,npx-i,3) 
           enddo
      endif
      if ( ne_corner ) then
           do i=npy,npy+2
              sin_sg(npx,i,1) = sin_sg(i,npy-1,4) 
              sin_sg(i,npy,2) = sin_sg(npx-1,i,3) 
           enddo
        endif
     endif

! For AAM correction:
     do j=js,je
        do i=is,ie+1
           pp1(:) = grid(i  ,j ,1:2)
           pp2(:) = grid(i,j+1 ,1:2)
           call mid_pt_sphere(pp1, pp2, pp3)
           call get_unit_vect2(pp1, pp2, e2)
           call get_latlon_vector(pp3, ex, ey)
           Atm%gridstruct%l2c_v(i,j) = cos(pp3(2)) * inner_prod(e2, ex)
        enddo
     enddo
     do j=js,je+1
        do i=is,ie
           pp1(:) = grid(i,  j,1:2)
           pp2(:) = grid(i+1,j,1:2)
           call mid_pt_sphere(pp1, pp2, pp3)
           call get_unit_vect2(pp1, pp2, e1)
           call get_latlon_vector(pp3, ex, ey)
           Atm%gridstruct%l2c_u(i,j) = cos(pp3(2)) * inner_prod(e1, ex)
        enddo
     enddo

   else
     cos_sg(:,:,:) = 0.
     sin_sg(:,:,:) = 1.

     ec1(1,:,:)=1.
     ec1(2,:,:)=0.
     ec1(3,:,:)=0.

     ec2(1,:,:)=0.
     ec2(2,:,:)=1.
     ec2(3,:,:)=0.

     ew(1,:,:,1)=1.
     ew(2,:,:,1)=0.
     ew(3,:,:,1)=0.
                                   
     ew(1,:,:,2)=0.
     ew(2,:,:,2)=1.
     ew(3,:,:,2)=0.

     es(1,:,:,1)=1.
     es(2,:,:,1)=0.
     es(3,:,:,1)=0.
                                   
     es(1,:,:,2)=0.
     es(2,:,:,2)=1.
     es(3,:,:,2)=0.
  endif

   if ( non_ortho ) then
           cosa_u = big_number
           cosa_v = big_number
           cosa_s = big_number
           sina_u = big_number
           sina_v = big_number
           rsin_u = big_number
           rsin_v = big_number
           rsina  = big_number
           rsin2  = big_number
           cosa = big_number
           sina = big_number

        do j=js,je+1
           do i=is,ie+1
! unit vect in X-dir: ee1
              if (i==1 .and. .not. Atm%neststruct%nested) then
                  call vect_cross(pp, grid3(1,i,  j), grid3(1,i+1,j))
              elseif(i==npx .and. .not. Atm%neststruct%nested) then
                  call vect_cross(pp, grid3(1,i-1,j), grid3(1,i,  j))
              else
                  call vect_cross(pp, grid3(1,i-1,j), grid3(1,i+1,j))
              endif
              call vect_cross(ee1(1:3,i,j), pp, grid3(1:3,i,j))
              call normalize_vect( ee1(1:3,i,j) )

! unit vect in Y-dir: ee2
              if (j==1 .and. .not. Atm%neststruct%nested) then
                  call vect_cross(pp, grid3(1:3,i,j  ), grid3(1:3,i,j+1))
              elseif(j==npy .and. .not. Atm%neststruct%nested) then
                  call vect_cross(pp, grid3(1:3,i,j-1), grid3(1:3,i,j  ))
              else
                  call vect_cross(pp, grid3(1:3,i,j-1), grid3(1:3,i,j+1))
              endif
              call vect_cross(ee2(1:3,i,j), pp, grid3(1:3,i,j))
              call normalize_vect( ee2(1:3,i,j) )

! symmetrical grid
#ifdef TEST_FP
              tmp1 = inner_prod(ee1(1:3,i,j), ee2(1:3,i,j))
              cosa(i,j) = sign(min(1., abs(tmp1)), tmp1)
              sina(i,j) = sqrt(max(0.,1. -cosa(i,j)**2))
#else
              cosa(i,j) = 0.5*(cos_sg(i-1,j-1,8)+cos_sg(i,j,6))
              sina(i,j) = 0.5*(sin_sg(i-1,j-1,8)+sin_sg(i,j,6))
#endif
           enddo
        enddo

!     9---4---8
!     |       |
!     1   5   3
!     |       |
!     6---2---7
      do j=jsd,jed
         do i=isd+1,ied
            cosa_u(i,j) = 0.5*(cos_sg(i-1,j,3)+cos_sg(i,j,1))
            sina_u(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))
!           rsin_u(i,j) =  1. / sina_u(i,j)**2
            rsin_u(i,j) =  1. / max(tiny_number, sina_u(i,j)**2)
         enddo
      enddo
      do j=jsd+1,jed
         do i=isd,ied
            cosa_v(i,j) = 0.5*(cos_sg(i,j-1,4)+cos_sg(i,j,2))
            sina_v(i,j) = 0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))
!           rsin_v(i,j) =  1. / sina_v(i,j)**2
            rsin_v(i,j) =  1. / max(tiny_number, sina_v(i,j)**2)
         enddo
      enddo
     
      do j=jsd,jed
         do i=isd,ied
            cosa_s(i,j) = cos_sg(i,j,5)
!           rsin2(i,j) = 1. / sin_sg(i,j,5)**2
            rsin2(i,j) = 1. / max(tiny_number, sin_sg(i,j,5)**2)
         enddo
      enddo
! Force the model to fail if incorrect corner values are to be used:
      if (.not. Atm%neststruct%nested) then
         call fill_ghost(cosa_s, npx, npy,  big_number, Atm%bd)
      end if
!------------------------------------
! Set special sin values at edges:
!------------------------------------
      do j=js,je+1
         do i=is,ie+1
            if ( i==npx .and. j==npy .and. .not. Atm%neststruct%nested) then
            else if ( ( i==1 .or. i==npx .or. j==1 .or. j==npy ) .and. .not. Atm%neststruct%nested ) then
                 rsina(i,j) = big_number
            else
!                rsina(i,j) = 1. / sina(i,j)**2
                 rsina(i,j) = 1. / max(tiny_number, sina(i,j)**2)
            endif
         enddo
      enddo

      do j=jsd,jed
         do i=is,ie+1
            if ( (i==1 .or. i==npx)  .and. .not. Atm%neststruct%nested ) then
!                rsin_u(i,j) = 1. / sina_u(i,j)
                 rsin_u(i,j) = 1. / sign(max(tiny_number,abs(sina_u(i,j))), sina_u(i,j))
            endif
         enddo
      enddo

      do j=js,je+1
         do i=isd,ied
            if ( (j==1 .or. j==npy) .and. .not. Atm%neststruct%nested ) then
!                rsin_v(i,j) = 1. / sina_v(i,j)
                 rsin_v(i,j) = 1. / sign(max(tiny_number,abs(sina_v(i,j))), sina_v(i,j))
            endif
         enddo
      enddo

      !EXPLANATION HERE: calling fill_ghost overwrites **SOME** of the sin_sg values along the outward-facing edge of a tile in the corners, which is incorrect. What we will do is call fill_ghost and then fill in the appropriate values

      if (.not. Atm%neststruct%nested) then
     do k=1,9
        call fill_ghost(sin_sg(:,:,k), npx, npy, tiny_number, Atm%bd)  ! this will cause NAN if used
        call fill_ghost(cos_sg(:,:,k), npx, npy, big_number, Atm%bd)
     enddo
     end if

! -------------------------------
! For transport operation
! -------------------------------
      if ( sw_corner ) then
           do i=0,-2,-1
              sin_sg(0,i,3) = sin_sg(i,1,2) 
              sin_sg(i,0,4) = sin_sg(1,i,1) 
              cos_sg(0,i,3) = cos_sg(i,1,2) 
              cos_sg(i,0,4) = cos_sg(1,i,1) 
!!!           cos_sg(0,i,7) = cos_sg(i,1,6)
!!!           cos_sg(0,i,8) = cos_sg(i,1,7)
!!!           cos_sg(i,0,8) = cos_sg(1,i,9)
!!!           cos_sg(i,0,9) = cos_sg(1,i,6)
           enddo
!!!        cos_sg(0,0,8) = 0.5*(cos_sg(0,1,7)+cos_sg(1,0,9))
           
      endif
      if ( nw_corner ) then
           do i=npy,npy+2
              sin_sg(0,i,3) = sin_sg(npy-i,npy-1,4) 
              cos_sg(0,i,3) = cos_sg(npy-i,npy-1,4) 
!!!           cos_sg(0,i,7) = cos_sg(npy-i,npy-1,8)
!!!           cos_sg(0,i,8) = cos_sg(npy-i,npy-1,9)
           enddo
           do i=0,-2,-1
              sin_sg(i,npy,2) = sin_sg(1,npy-i,1) 
              cos_sg(i,npy,2) = cos_sg(1,npy-i,1) 
!!!           cos_sg(i,npy,6) = cos_sg(1,npy-i,9)
!!!           cos_sg(i,npy,7) = cos_sg(1,npy-i,6)
           enddo
!!!        cos_sg(0,npy,7) = 0.5*(cos_sg(1,npy,6)+cos_sg(0,npy-1,8))
      endif
      if ( se_corner ) then
           do j=0,-2,-1
              sin_sg(npx,j,1) = sin_sg(npx-j,1,2) 
              cos_sg(npx,j,1) = cos_sg(npx-j,1,2) 
!!!           cos_sg(npx,j,6) = cos_sg(npx-j,1,7) 
!!!           cos_sg(npx,j,9) = cos_sg(npx-j,1,6) 
           enddo
           do i=npx,npx+2
              sin_sg(i,0,4) = sin_sg(npx-1,npx-i,3) 
              cos_sg(i,0,4) = cos_sg(npx-1,npx-i,3) 
!!!           cos_sg(i,0,9) = cos_sg(npx-1,npx-i,8) 
!!!           cos_sg(i,0,8) = cos_sg(npx-1,npx-i,7) 
           enddo
!!!        cos_sg(npx,0,9) = 0.5*(cos_sg(npx,1,6)+cos_sg(npx-1,0,8))
      endif
      if ( ne_corner ) then
         do i=0,2
            sin_sg(npx,npy+i,1) = sin_sg(npx+i,npy-1,4)
            sin_sg(npx+i,npy,2) = sin_sg(npx-1,npy+i,3)
            cos_sg(npx,npy+i,1) = cos_sg(npx+i,npy-1,4)
!!!         cos_sg(npx,npy+i,6) = cos_sg(npx+i,npy-1,9)
!!!         cos_sg(npx,npy+i,9) = cos_sg(npx+i,npy-1,8)
            cos_sg(npx+i,npy,2) = cos_sg(npx-1,npy+i,3)
!!!         cos_sg(npx+i,npy,6) = cos_sg(npx-1,npy+i,7)
!!!         cos_sg(npx+i,npy,7) = cos_sg(npx-1,npy+i,8)
         end do
!!!      cos_sg(npx,npy,6) = 0.5*(cos_sg(npx-1,npy,7)+cos_sg(npx,npy-1,9))
      endif     

   else
           sina = 1.
           cosa = 0.
           rsina  = 1.
           rsin2  = 1.
           sina_u = 1.
           sina_v = 1.
           cosa_u = 0.        
           cosa_v = 0.        
           cosa_s = 0.        
           rsin_u = 1.
           rsin_v = 1.
   endif

   if ( grid_type < 3 ) then

#ifdef USE_NORM_VECT
!-------------------------------------------------------------
! Make normal vect at face edges after consines are computed:
!-------------------------------------------------------------
! for old d2a2c_vect routines
      if (.not. Atm%neststruct%nested) then
         do j=js-1,je+1
            if ( is==1 ) then
               i=1
               call vect_cross(ew(1,i,j,1), grid3(1,i,j+1), grid3(1,i,j)) 
               call normalize_vect( ew(1,i,j,1) )
            endif
            if ( (ie+1)==npx ) then
               i=npx
               call vect_cross(ew(1,i,j,1), grid3(1,i,j+1), grid3(1,i,j)) 
               call normalize_vect( ew(1,i,j,1) )
            endif
         enddo

         if ( js==1 ) then
            j=1
            do i=is-1,ie+1
               call vect_cross(es(1,i,j,2), grid3(1,i,j),grid3(1,i+1,j)) 
               call normalize_vect( es(1,i,j,2) )
            enddo
         endif
         if ( (je+1)==npy ) then
            j=npy
            do i=is-1,ie+1
               call vect_cross(es(1,i,j,2), grid3(1,i,j),grid3(1,i+1,j)) 
               call normalize_vect( es(1,i,j,2) )
            enddo
         endif
      endif
#endif

! For omega computation:
! Unit vectors:
     do j=js,je+1
        do i=is,ie
           call vect_cross(en1(1:3,i,j), grid3(1,i,j), grid3(1,i+1,j))
           call normalize_vect( en1(1:3,i,j) )
        enddo
     enddo
     do j=js,je
        do i=is,ie+1
           call vect_cross(en2(1:3,i,j), grid3(1,i,j+1), grid3(1,i,j)) 
           call normalize_vect( en2(1:3,i,j) )
        enddo
     enddo
!-------------------------------------------------------------
! Make unit vectors for the coordinate extension:
!-------------------------------------------------------------
  endif
 
  do j=jsd,jed+1
     if ((j==1 .OR. j==npy) .and. .not. Atm%neststruct%nested) then
        do i=isd,ied
           divg_u(i,j) = 0.5*(sin_sg(i,j,2)+sin_sg(i,j-1,4))*dyc(i,j)/dx(i,j)
           del6_u(i,j) = 0.5*(sin_sg(i,j,2)+sin_sg(i,j-1,4))*dx(i,j)/dyc(i,j)
        enddo
     else
        do i=isd,ied
           divg_u(i,j) = sina_v(i,j)*dyc(i,j)/dx(i,j)
           del6_u(i,j) = sina_v(i,j)*dx(i,j)/dyc(i,j)
        enddo
     end if
  enddo
  do j=jsd,jed
     do i=isd,ied+1
        divg_v(i,j) = sina_u(i,j)*dxc(i,j)/dy(i,j)
        del6_v(i,j) = sina_u(i,j)*dy(i,j)/dxc(i,j)
     enddo
     if (is == 1 .and. .not. Atm%neststruct%nested) then
         divg_v(is,j) = 0.5*(sin_sg(1,j,1)+sin_sg(0,j,3))*dxc(is,j)/dy(is,j)
         del6_v(is,j) = 0.5*(sin_sg(1,j,1)+sin_sg(0,j,3))*dy(is,j)/dxc(is,j)
     endif
     if (ie+1 == npx .and. .not. Atm%neststruct%nested) then
         divg_v(ie+1,j) = 0.5*(sin_sg(npx,j,1)+sin_sg(npx-1,j,3))*dxc(ie+1,j)/dy(ie+1,j)
         del6_v(ie+1,j) = 0.5*(sin_sg(npx,j,1)+sin_sg(npx-1,j,3))*dy(ie+1,j)/dxc(ie+1,j)
     endif
  enddo

! Initialize cubed_sphere to lat-lon transformation:
     call init_cubed_to_latlon( Atm%gridstruct, Atm%flagstruct%hydrostatic, agrid, grid_type, c2l_order, Atm%bd )

     call global_mx(area, ng, Atm%gridstruct%da_min, Atm%gridstruct%da_max, Atm%bd)
     if( is_master() ) write(*,*) 'da_max/da_min=', Atm%gridstruct%da_max/Atm%gridstruct%da_min

     call global_mx_c(area_c(is:ie,js:je), is, ie, js, je, Atm%gridstruct%da_min_c, Atm%gridstruct%da_max_c)

     if( is_master() ) write(*,*) 'da_max_c/da_min_c=', Atm%gridstruct%da_max_c/Atm%gridstruct%da_min_c

!------------------------------------------------
! Initialization for interpolation at face edges
!------------------------------------------------
! A->B scalar:
     if (grid_type < 3 .and. .not. Atm%neststruct%nested) then
        call mpp_update_domains(divg_v, divg_u, Atm%domain, flags=SCALAR_PAIR,      &
                                gridtype=CGRID_NE_PARAM, complete=.true.)
        call mpp_update_domains(del6_v, del6_u, Atm%domain, flags=SCALAR_PAIR,      &
                                gridtype=CGRID_NE_PARAM, complete=.true.)
        call edge_factors (Atm%gridstruct%edge_s, Atm%gridstruct%edge_n, Atm%gridstruct%edge_w, &
             Atm%gridstruct%edge_e, non_ortho, grid, agrid, npx, npy, Atm%bd)
        call efactor_a2c_v(Atm%gridstruct%edge_vect_s, Atm%gridstruct%edge_vect_n, &
             Atm%gridstruct%edge_vect_w, Atm%gridstruct%edge_vect_e, &
             non_ortho, grid, agrid, npx, npy, Atm%neststruct%nested, Atm%bd)
!       call extend_cube_s(non_ortho, grid, agrid, npx, npy, .false., Atm%neststruct%nested)
!       call van2d_init(grid, agrid, npx, npy)
     else

        Atm%gridstruct%edge_s = big_number
        Atm%gridstruct%edge_n = big_number
        Atm%gridstruct%edge_w = big_number
        Atm%gridstruct%edge_e = big_number

        Atm%gridstruct%edge_vect_s = big_number
        Atm%gridstruct%edge_vect_n = big_number
        Atm%gridstruct%edge_vect_w = big_number
        Atm%gridstruct%edge_vect_e = big_number

     endif

!32-bit versions of the data
      Atm%gridstruct%grid   = Atm%gridstruct%grid_64
      Atm%gridstruct%agrid  = Atm%gridstruct%agrid_64
      Atm%gridstruct%area   = Atm%gridstruct%area_64
      Atm%gridstruct%area_c = Atm%gridstruct%area_c_64
      Atm%gridstruct%dx     = Atm%gridstruct%dx_64
      Atm%gridstruct%dy     = Atm%gridstruct%dy_64
      Atm%gridstruct%dxa    = Atm%gridstruct%dxa_64
      Atm%gridstruct%dya    = Atm%gridstruct%dya_64
      Atm%gridstruct%dxc    = Atm%gridstruct%dxc_64
      Atm%gridstruct%dyc    = Atm%gridstruct%dyc_64
      Atm%gridstruct%cosa   = Atm%gridstruct%cosa_64
      Atm%gridstruct%sina   = Atm%gridstruct%sina_64

!--- deallocate the higher-order gridstruct arrays
!rab      deallocate ( Atm%gridstruct%grid_64 )
!rab      deallocate ( Atm%gridstruct%agrid_64 )
!rab      deallocate ( Atm%gridstruct%area_64 )
      deallocate ( Atm%gridstruct%area_c_64 )
!rab      deallocate ( Atm%gridstruct%dx_64 )
!rab      deallocate ( Atm%gridstruct%dy_64 )
      deallocate ( Atm%gridstruct%dxa_64 )
      deallocate ( Atm%gridstruct%dya_64 )
      deallocate ( Atm%gridstruct%dxc_64 )
      deallocate ( Atm%gridstruct%dyc_64 )
      deallocate ( Atm%gridstruct%cosa_64 )
      deallocate ( Atm%gridstruct%sina_64 )

      nullify(agrid)
      nullify(grid)
      nullify(area)
      nullify(area_c)
      nullify(dx)
      nullify(dy)
      nullify(dxc)
      nullify(dyc)
      nullify(dxa)
      nullify(dya)
      nullify(sina)
      nullify(cosa)
      nullify(divg_u)
      nullify(divg_v)

      nullify(del6_u)
      nullify(del6_v)

      nullify(cosa_u)
      nullify(cosa_v)
      nullify(cosa_s)
      nullify(sina_u)
      nullify(sina_v)
      nullify(rsin_u)
      nullify(rsin_v)
      nullify(rsina)
      nullify(rsin2)
      nullify(ee1)
      nullify(ee2)
      nullify(ec1)
      nullify(ec2)
      nullify(ew)
      nullify(es)
      nullify(sin_sg)
      nullify(cos_sg)
      nullify(en1)
      nullify(en2)
      nullify(sw_corner)
      nullify(se_corner)
      nullify(ne_corner)
      nullify(nw_corner)

  end subroutine grid_utils_init

 
  subroutine grid_utils_end
 
! deallocate sst_ncep (if allocated)
#ifndef DYCORE_SOLO
      if (allocated(sst_ncep)) deallocate( sst_ncep )
      if (allocated(sst_anom)) deallocate( sst_anom )
#endif
  end subroutine grid_utils_end

  subroutine direct_transform(c, i1, i2, j1, j2, lon_p, lat_p, n, lon, lat)
!
! This is a direct transformation of the standard (symmetrical) cubic grid
! to a locally enhanced high-res grid on the sphere; it is an application
! of the Schmidt transformation at the south pole followed by a 
! pole_shift_to_target (rotation) operation
!
    real(kind=R_GRID),    intent(in):: c              ! Stretching factor
    real(kind=R_GRID),    intent(in):: lon_p, lat_p   ! center location of the target face, radian
    integer, intent(in):: n              ! grid face number
    integer, intent(in):: i1, i2, j1, j2
!  0 <= lon <= 2*pi ;    -pi/2 <= lat <= pi/2
    real(kind=R_GRID), intent(inout), dimension(i1:i2,j1:j2):: lon, lat
!
    real(f_p):: lat_t, sin_p, cos_p, sin_lat, cos_lat, sin_o, p2, two_pi
    real(f_p):: c2p1, c2m1
    integer:: i, j

    p2 = 0.5d0*pi
    two_pi = 2.d0*pi

    if( is_master() .and. n==1 ) then
        write(*,*) n, 'Schmidt transformation: stretching factor=', c, ' center=', lon_p, lat_p
    endif

    c2p1 = 1.d0 + c*c
    c2m1 = 1.d0 - c*c

    sin_p = sin(lat_p)
    cos_p = cos(lat_p)

    do j=j1,j2
       do i=i1,i2
          if ( abs(c2m1) > 1.d-7 ) then
               sin_lat = sin(lat(i,j)) 
               lat_t = asin( (c2m1+c2p1*sin_lat)/(c2p1+c2m1*sin_lat) )
          else         ! no stretching
               lat_t = lat(i,j)
          endif
          sin_lat = sin(lat_t) 
          cos_lat = cos(lat_t) 
            sin_o = -(sin_p*sin_lat + cos_p*cos_lat*cos(lon(i,j)))
          if ( (1.-abs(sin_o)) < 1.d-7 ) then    ! poles
               lon(i,j) = 0.d0
               lat(i,j) = sign( p2, sin_o )
          else
               lat(i,j) = asin( sin_o )
               lon(i,j) = lon_p + atan2( -cos_lat*sin(lon(i,j)),   &
                          -sin_lat*cos_p+cos_lat*sin_p*cos(lon(i,j)))
               if ( lon(i,j) < 0.d0 ) then
                    lon(i,j) = lon(i,j) + two_pi
               elseif( lon(i,j) >= two_pi ) then
                    lon(i,j) = lon(i,j) - two_pi
               endif
          endif
       enddo
    enddo

  end subroutine direct_transform


  real function inner_prod(v1, v2)
       real(kind=R_GRID),intent(in):: v1(3), v2(3)
       real (f_p) :: vp1(3), vp2(3), prod16
       integer k
      
         do k=1,3
            vp1(k) = real(v1(k),kind=f_p)
            vp2(k) = real(v2(k),kind=f_p)
         enddo
         prod16 = vp1(1)*vp2(1) + vp1(2)*vp2(2) + vp1(3)*vp2(3)
         inner_prod = prod16

  end function inner_prod


 subroutine efactor_a2c_v(edge_vect_s, edge_vect_n, edge_vect_w, edge_vect_e, non_ortho, grid, agrid, npx, npy, nested, bd)
!
! Initialization of interpolation factors at face edges
! for interpolating vectors from A to C grid
!
 type(fv_grid_bounds_type), intent(IN) :: bd
 real(kind=R_GRID),    intent(INOUT), dimension(bd%isd:bd%ied) :: edge_vect_s, edge_vect_n
 real(kind=R_GRID),    intent(INOUT), dimension(bd%jsd:bd%jed) :: edge_vect_w, edge_vect_e
 logical, intent(in):: non_ortho, nested
 real(kind=R_GRID),    intent(in)::  grid(bd%isd:bd%ied+1,bd%jsd:bd%jed+1,2)
 real(kind=R_GRID),    intent(in):: agrid(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,2)
 integer, intent(in):: npx, npy

 real(kind=R_GRID) px(2,bd%isd:bd%ied+1),  py(2,bd%jsd:bd%jed+1)
 real(kind=R_GRID) p1(2,bd%isd:bd%ied+1),  p2(2,bd%jsd:bd%jed+1)       ! mid-point
 real(kind=R_GRID) d1, d2
 integer i, j
 integer im2, jm2
 
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


  if ( .not. non_ortho ) then
     edge_vect_s = 0.
     edge_vect_n = 0.
     edge_vect_w = 0.
     edge_vect_e = 0.
  else
     edge_vect_s = big_number
     edge_vect_n = big_number
     edge_vect_w = big_number
     edge_vect_e = big_number

     if ( npx /= npy .and. .not. nested) call mpp_error(FATAL, 'efactor_a2c_v: npx /= npy')
     if ( (npx/2)*2 == npx ) call mpp_error(FATAL, 'efactor_a2c_v: npx/npy is not an odd number')

     im2 = (npx-1)/2
     jm2 = (npy-1)/2

 if ( is==1 ) then
    i=1
    do j=js-2,je+2
       call mid_pt_sphere(agrid(i-1,j,1:2), agrid(i,j,  1:2), py(1,j))
       call mid_pt_sphere( grid(i,  j,1:2),  grid(i,j+1,1:2), p2(1,j))
    enddo

! west edge:
!------------------------------------------------------------------
! v_sw(j) = (1.-edge_vect_w(j)) * p(j) + edge_vect_w(j) * p(j+1)
!------------------------------------------------------------------
    do j=js-1,je+1
       if ( j<=jm2 ) then
            d1 = great_circle_dist( py(1,j  ), p2(1,j) )
            d2 = great_circle_dist( py(1,j+1), p2(1,j) )
            edge_vect_w(j) = d1 / ( d1 + d2 )
       else
            d2 = great_circle_dist( py(1,j-1), p2(1,j) )
            d1 = great_circle_dist( py(1,j  ), p2(1,j) )
            edge_vect_w(j) = d1 / ( d2 + d1 )
       endif
    enddo
    if ( js==1 ) then
         edge_vect_w(0) = edge_vect_w(1)
    endif
    if ( (je+1)==npy ) then
         edge_vect_w(npy) = edge_vect_w(je)
    endif
    do j=js-1,je+1
!      if ( is_master() ) write(*,*) j, edge_vect_w(j)
    enddo
 endif

 if ( (ie+1)==npx ) then
    i=npx
    do j=jsd,jed
       call mid_pt_sphere(agrid(i-1,j,1:2), agrid(i,j,  1:2), py(1,j))
       call mid_pt_sphere( grid(i,  j,1:2),  grid(i,j+1,1:2), p2(1,j))
    enddo

    do j=js-1,je+1
       if ( j<=jm2 ) then
            d1 = great_circle_dist( py(1,j  ), p2(1,j) )
            d2 = great_circle_dist( py(1,j+1), p2(1,j) )
            edge_vect_e(j) = d1 / ( d1 + d2 )
       else
            d2 = great_circle_dist( py(1,j-1), p2(1,j) )
            d1 = great_circle_dist( py(1,j  ), p2(1,j) )
            edge_vect_e(j) = d1 / ( d2 + d1 )
       endif
    enddo
    if ( js==1 ) then
         edge_vect_e(0) = edge_vect_e(1)
    endif
    if ( (je+1)==npy ) then
         edge_vect_e(npy) = edge_vect_e(je)
    endif
    do j=js-1,je+1
!      if ( is_master() ) write(*,*) j, edge_vect_e(j)
    enddo
 endif

 if ( js==1 ) then
    j=1
    do i=isd,ied
       call mid_pt_sphere(agrid(i,j-1,1:2), agrid(i,  j,1:2), px(1,i))
       call mid_pt_sphere( grid(i,j,  1:2),  grid(i+1,j,1:2), p1(1,i))
    enddo
! south_west edge:
!------------------------------------------------------------------
! v_s(i) = (1.-edge_vect_s(i)) * p(i) + edge_vect_s(i) * p(i+1)
!------------------------------------------------------------------
    do i=is-1,ie+1
       if ( i<=im2 ) then
            d1 = great_circle_dist( px(1,i  ), p1(1,i) )
            d2 = great_circle_dist( px(1,i+1), p1(1,i) )
            edge_vect_s(i) = d1 / ( d1 + d2 )
       else
            d2 = great_circle_dist( px(1,i-1), p1(1,i) )
            d1 = great_circle_dist( px(1,i  ), p1(1,i) )
            edge_vect_s(i) = d1 / ( d2 + d1 )
       endif
    enddo
    if ( is==1 ) then
         edge_vect_s(0) = edge_vect_s(1)
    endif
    if ( (ie+1)==npx ) then
         edge_vect_s(npx) = edge_vect_s(ie)
    endif
    do i=is-1,ie+1
!      if ( is_master() ) write(*,*) i, edge_vect_s(i)
    enddo
 endif


 if ( (je+1)==npy ) then
! v_n(i) = (1.-edge_vect_n(i)) * p(i) + edge_vect_n(i) * p(i+1)
    j=npy
    do i=isd,ied
       call mid_pt_sphere(agrid(i,j-1,1:2), agrid(i,  j,1:2), px(1,i))
       call mid_pt_sphere( grid(i,j,  1:2),  grid(i+1,j,1:2), p1(1,i))
    enddo

    do i=is-1,ie+1
       if ( i<=im2 ) then
            d1 = great_circle_dist( px(1,i  ), p1(1,i) )
            d2 = great_circle_dist( px(1,i+1), p1(1,i) )
            edge_vect_n(i) = d1 / ( d1 + d2 )
       else
            d2 = great_circle_dist( px(1,i-1), p1(1,i) )
            d1 = great_circle_dist( px(1,i  ), p1(1,i) )
            edge_vect_n(i) = d1 / ( d2 + d1 )
       endif
    enddo
    if ( is==1 ) then
         edge_vect_n(0) = edge_vect_n(1)
    endif
    if ( (ie+1)==npx ) then
         edge_vect_n(npx) = edge_vect_n(ie)
    endif
    do i=is-1,ie+1
!      if ( is_master() ) write(*,*) i, edge_vect_n(i)
    enddo
 endif

 endif

 end subroutine efactor_a2c_v

! Sets up edge_?
 subroutine edge_factors(edge_s, edge_n, edge_w, edge_e, non_ortho, grid, agrid, npx, npy, bd)
!
! Initialization of interpolation factors at face edges
! for interpolation from A to B grid
!
 type(fv_grid_bounds_type), intent(IN) :: bd
 real(kind=R_GRID),    intent(INOUT), dimension(npx) :: edge_s, edge_n
 real(kind=R_GRID),    intent(INOUT), dimension(npy) :: edge_w, edge_e
 logical, intent(in):: non_ortho
 real(kind=R_GRID),    intent(in)::  grid(bd%isd:bd%ied+1,bd%jsd:bd%jed+1,2)
 real(kind=R_GRID),    intent(in):: agrid(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,2)
 integer, intent(in):: npx, npy

 real(kind=R_GRID) px(2,npx), py(2,npy)
 real(kind=R_GRID) d1, d2
 integer i, j

  integer :: is,  ie,  js,  je
  integer :: isd, ied, jsd, jed

  is  = bd%is
  ie  = bd%ie
  js  = bd%js
  je  = bd%je
  isd = bd%isd
  ied = bd%ied
  jsd = bd%jsd


  if ( .not. non_ortho ) then
     edge_s = 0.5d0
     edge_n = 0.5d0
     edge_w = 0.5d0
     edge_e = 0.5d0
  else
     edge_s = big_number
     edge_n = big_number
     edge_w = big_number
     edge_e = big_number
 
! west edge:
!----------------------------------------------------------
! p_west(j) = (1.-edge_w(j)) * p(j) + edge_w(j) * p(j-1)
!----------------------------------------------------------
 if ( is==1 ) then
    i=1
    do j=max(1,js-1), min(npy-1,je+1)
       call mid_pt_sphere(agrid(i-1,j,1:2), agrid(i,j,1:2), py(1,j))
    enddo
    do j=max(2,js), min(npy-1,je+1)
       d1 = great_circle_dist( py(1,j-1), grid(i,j,1:2) )
       d2 = great_circle_dist( py(1,j  ), grid(i,j,1:2) )
       edge_w(j) = d2 / ( d1 + d2 )
    enddo
 endif

! east edge:
!----------------------------------------------------------
! p_east(j) = (1.-edge_e(j)) * p(j) + edge_e(j) * p(j-1)
!----------------------------------------------------------
 if ( (ie+1)==npx ) then
    i=npx
    do j=max(1,js-1), min(npy-1,je+1)
       call mid_pt_sphere(agrid(i-1,j,1:2), agrid(i,j,1:2), py(1,j))
    enddo
    do j=max(2,js), min(npy-1,je+1)
       d1 = great_circle_dist( py(1,j-1), grid(i,j,1:2) )
       d2 = great_circle_dist( py(1,j  ), grid(i,j,1:2) )
       edge_e(j) = d2 / ( d1 + d2 )
! Check rounding difference:
!      if(is_master()) write(*,*) j, edge_w(j) - edge_e(j)
    enddo
 endif


! south edge:
!----------------------------------------------------------
! p_south(j) = (1.-edge_s(i)) * p(i) + edge_s(i) * p(i-1)
!----------------------------------------------------------
 if ( js==1 ) then
    j=1
    do i=max(1,is-1), min(npx-1,ie+1)
       call mid_pt_sphere(agrid(i,j-1,1:2), agrid(i,j,1:2), px(1,i))
    enddo
    do i=max(2,is), min(npx-1,ie+1)
       d1 = great_circle_dist( px(1,i-1), grid(i,j,1:2) )
       d2 = great_circle_dist( px(1,i  ), grid(i,j,1:2) )
       edge_s(i) = d2 / ( d1 + d2 )
    enddo
 endif

! North edge:
!----------------------------------------------------------
! p_north(j) = (1.-edge_n(i)) * p(i) + edge_n(i) * p(i-1)
!----------------------------------------------------------
 if ( (je+1)==npy ) then
    j=npy
    do i=max(1,is-1), min(npx-1,ie+1)
       call mid_pt_sphere(agrid(i,j-1,1:2), agrid(i,j,1:2), px(1,i))
    enddo
    do i=max(2,is), min(npx-1,ie+1)
       d1 = great_circle_dist( px(1,i-1), grid(i,j,1:2) )
       d2 = great_circle_dist( px(1,i  ), grid(i,j,1:2) )
       edge_n(i) = d2 / ( d1 + d2 )
!      if(is_master()) write(*,*) i, edge_s(i), edge_n(i)-edge_s(i)
    enddo
 endif
 endif

 end subroutine edge_factors


 subroutine gnomonic_grids(grid_type, im, lon, lat)
 integer, intent(in):: im, grid_type
 real(kind=R_GRID), intent(out):: lon(im+1,im+1)
 real(kind=R_GRID), intent(out):: lat(im+1,im+1)
 integer i, j

  if(grid_type==0) call gnomonic_ed(  im, lon, lat)
  if(grid_type==1) call gnomonic_dist(im, lon, lat)
  if(grid_type==2) call gnomonic_angl(im, lon, lat)


  if(grid_type<3) then
     call symm_ed(im, lon, lat)
     do j=1,im+1
        do i=1,im+1
           lon(i,j) = lon(i,j) - pi
        enddo
     enddo
!    call van2_init(lon, lat, im+1, im+1)
  endif

 end subroutine gnomonic_grids

 subroutine gnomonic_ed(im, lamda, theta)
!-----------------------------------------------------
! Equal distance along the 4 edges of the cubed sphere
!-----------------------------------------------------
! Properties: 
!            * defined by intersections of great circles
!            * max(dx,dy; global) / min(dx,dy; global) = sqrt(2) = 1.4142
!            * Max(aspect ratio) = 1.06089
!            * the N-S coordinate curves are const longitude on the 4 faces with equator 
! For C2000: (dx_min, dx_max) = (3.921, 5.545)    in km unit
! This is the grid of choice for global cloud resolving

 integer, intent(in):: im
 real(kind=R_GRID), intent(out):: lamda(im+1,im+1)
 real(kind=R_GRID), intent(out):: theta(im+1,im+1)

! Local:
 real(kind=R_GRID) pp(3,im+1,im+1)
 real(kind=R_GRID) p1(2), p2(2)
 real(f_p):: rsq3, alpha, delx, dely
 integer i, j, k

  rsq3 = 1.d0/sqrt(3.d0) 
 alpha = asin( rsq3 )

! Ranges:
! lamda = [0.75*pi, 1.25*pi]
! theta = [-alpha, alpha]

    dely = 2.d0*alpha / real(im,kind=f_p)

! Define East-West edges:
 do j=1,im+1
    lamda(1,   j) = 0.75d0*pi                  ! West edge
    lamda(im+1,j) = 1.25d0*pi                  ! East edge
    theta(1,   j) = -alpha + dely*real(j-1,kind=f_p)  ! West edge
    theta(im+1,j) = theta(1,j)               ! East edge
 enddo

! Get North-South edges by symmetry:

 do i=2,im
    call mirror_latlon(lamda(1,1), theta(1,1), lamda(im+1,im+1), theta(im+1,im+1), &
                       lamda(1,i), theta(1,i), lamda(i,1),       theta(i,      1) )
    lamda(i,im+1) =  lamda(i,1)
    theta(i,im+1) = -theta(i,1)
 enddo

! Set 4 corners:
    call latlon2xyz2(lamda(1    ,  1), theta(1,      1), pp(1,   1,   1))
    call latlon2xyz2(lamda(im+1,   1), theta(im+1,   1), pp(1,im+1,   1))
    call latlon2xyz2(lamda(1,   im+1), theta(1,   im+1), pp(1,   1,im+1))
    call latlon2xyz2(lamda(im+1,im+1), theta(im+1,im+1), pp(1,im+1,im+1))

! Map edges on the sphere back to cube:
! Intersections at x=-rsq3

 i=1
 do j=2,im
    call latlon2xyz2(lamda(i,j), theta(i,j), pp(1,i,j))
    pp(2,i,j) = -pp(2,i,j)*rsq3/pp(1,i,j)
    pp(3,i,j) = -pp(3,i,j)*rsq3/pp(1,i,j)
 enddo

 j=1
 do i=2,im
    call latlon2xyz2(lamda(i,j), theta(i,j), pp(1,i,1))
    pp(2,i,1) = -pp(2,i,1)*rsq3/pp(1,i,1)
    pp(3,i,1) = -pp(3,i,1)*rsq3/pp(1,i,1)
 enddo

 do j=1,im+1
    do i=1,im+1
       pp(1,i,j) = -rsq3
    enddo
 enddo

 do j=2,im+1
    do i=2,im+1
! Copy y-z face of the cube along j=1
       pp(2,i,j) = pp(2,i,1)
! Copy along i=1
       pp(3,i,j) = pp(3,1,j)
    enddo
 enddo

 call cart_to_latlon( (im+1)*(im+1), pp, lamda, theta)

! Compute great-circle-distance "resolution" along the face edge:
 if ( is_master() ) then
      p1(1) = lamda(1,1);    p1(2) = theta(1,1)
      p2(1) = lamda(2,1);    p2(2) = theta(2,1)
      write(*,*) 'Gird distance at face edge (km)=',great_circle_dist( p1, p2, radius )   ! earth radius is assumed
 endif

 end subroutine gnomonic_ed

 subroutine gnomonic_ed_limited(im, in, nghost, lL, lR, uL, uR, lamda, theta)
   
   !This routine creates a limited-area equidistant gnomonic grid with
   !corners given by lL (lower-left), lR (lower-right), uL (upper-left),
   !and uR (upper-right) with im by in cells. lamda and theta are the 
   !latitude-longitude coordinates of the corners of the cells.

   !This formulation assumes the coordinates given are on the
   ! 'prototypical equatorial panel' given by gnomonic_ed. The
   ! resulting gnomonic limited area grid can then be translated and
   ! /or scaled to its appropriate location on another panel if so
   ! desired.

   integer, intent(IN) :: im, in, nghost
   real(kind=R_GRID), intent(IN), dimension(2) :: lL, lR, uL, uR
   real(kind=R_GRID), intent(OUT) :: lamda(1-nghost:im+1+nghost,1-nghost:in+1+nghost)
   real(kind=R_GRID), intent(OUT) :: theta(1-nghost:im+1+nghost,1-nghost:in+1+nghost)

   ! Local:
   real(kind=R_GRID) pp(3,1-nghost:im+1+nghost,1-nghost:in+1+nghost)
   real(kind=R_GRID) p1(2), p2(2)
   real(f_p):: rsq3, alpha, delx, dely
   integer i, j, k, irefl
   
   rsq3 = 1.d0/sqrt(3.d0) 
   alpha = asin( rsq3 )

   lamda(1,1) = lL(1);         theta(1,1) = lL(2)
   lamda(im+1,1) = lR(1);      theta(im+1,1) = lR(2)
   lamda(1,in+1) = uL(1);      theta(1,in+1) = uL(2)
   lamda(im+1,in+1) = uR(1);   theta(im+1,in+1) = uR(2)

   !Since meridians are great circles, grid spacing is equidistant in
   !lat-lon space along the east and west edges of the grid
   dely = (uL(2) - lL(2))/in
   do j=2,in+1+nghost
      theta(1,j) = theta(1,j-1) + dely
      theta(in+1,j) = theta(in+1,j-1) + dely
      lamda(1,j) = lamda(1,1)
      lamda(in+1,j) = lamda(in+1,1)
   end do
   do j=0,1-nghost,-1
      theta(1,j) = theta(1,j+1) - dely
      theta(in+1,j) = theta(in+1,j+1) - dely
      lamda(1,j) = lamda(1,1)
      lamda(in+1,j) = lamda(in+1,1)
   end do

   lamda(1,:) = lamda(1,1)
   lamda(in+1,:) = lamda(in+1,1)

   !Here, instead of performing a reflection (as in gnomonic_ed) to get the north and south
   !edges we interpolate along the great circle connecting the upper (or lower) two corners.
   do i=1-nghost,im+1+nghost

      if (i == 1) cycle

      call spherical_linear_interpolation(real(i-1,kind=R_GRID)/real(im,kind=R_GRID), &
           (/lamda(1,1),theta(1,1)/), (/lamda(im+1,1),theta(im+1,1)/), p1 )
      call spherical_linear_interpolation(real(i-1,kind=R_GRID)/real(im,kind=R_GRID), &
           (/lamda(1,in+1),theta(1,in+1)/), (/lamda(im+1,in+1),theta(im+1,in+1)/), p2 )

      lamda(i,1) = p1(1); theta(i,1) = p1(2)
      lamda(i,in+1) = p2(1); theta(i,in+1) = p2(2)

   end do

   !Get cartesian coordinates and project onto the cube face with x = -rsq3
   
   i=1
   do j=1-nghost,in+1+nghost
      call latlon2xyz2(lamda(i,j), theta(i,j), pp(1,i,j))
      pp(2,i,j) = -pp(2,i,j)*rsq3/pp(1,i,j)
      pp(3,i,j) = -pp(3,i,j)*rsq3/pp(1,i,j)
   enddo

   j=1
   do i=1-nghost,im+1+nghost
      call latlon2xyz2(lamda(i,j), theta(i,j), pp(1,i,1))
      pp(2,i,1) = -pp(2,i,1)*rsq3/pp(1,i,1)
      pp(3,i,1) = -pp(3,i,1)*rsq3/pp(1,i,1)
   enddo

   !We are now on the cube.

   do j=1-nghost,in+1+nghost
      do i=1-nghost,im+1+nghost
         pp(1,i,j) = -rsq3
      enddo
   enddo

   do j=1-nghost,in+1+nghost
      do i=1-nghost,im+1+nghost
         ! Copy y-z face of the cube along j=1
         pp(2,i,j) = pp(2,i,1)
         ! Copy along i=1
         pp(3,i,j) = pp(3,1,j)
      enddo
   enddo

   call cart_to_latlon( (im+1+2*nghost)*(in+1+2*nghost), &
        pp(:,1-nghost:im+1+nghost,1-nghost:in+1+nghost), &
        lamda(1-nghost:im+1+nghost,1-nghost:in+1+nghost), &
        theta(1-nghost:im+1+nghost,1-nghost:in+1+nghost))
   !call cart_to_latlon( (im+1)*(in+1), pp(:,1:im+1,1:in+1), lamda(1:im+1,1:in+1), theta(1:im+1,1:in+1))
   
   ! Compute great-circle-distance "resolution" along the face edge:
   if ( is_master() ) then
      p1(1) = lamda(1,1);    p1(2) = theta(1,1)
      p2(1) = lamda(2,1);    p2(2) = theta(2,1)
      write(*,*) 'Grid x-distance at face edge (km)=',great_circle_dist( p1, p2, radius )   ! earth radius is assumed
      p2(1) = lamda(1,2);    p2(2) = theta(1,2)
      write(*,*) 'Grid y-distance at face edge (km)=',great_circle_dist( p1, p2, radius )   ! earth radius is assumed
      !print*, 'dtheta = ', dely
      !print*, 'dlambda = ', lamda(2,1) - lamda(1,1)
   endif


 end subroutine gnomonic_ed_limited


 subroutine gnomonic_angl(im, lamda, theta)
! This is the commonly known equi-angular grid
 integer im
 real(kind=R_GRID) lamda(im+1,im+1)
 real(kind=R_GRID) theta(im+1,im+1)
 real(kind=R_GRID) p(3,im+1,im+1)
! Local
 real(kind=R_GRID) rsq3, xf, y0, z0, y, x, z, ds
 real(kind=R_GRID) dy, dz
 integer j,k
 real(kind=R_GRID) dp

 dp = 0.5d0*pi/real(im,kind=R_GRID)

 rsq3 = 1.d0/sqrt(3.d0) 
 do k=1,im+1
    do j=1,im+1
       p(1,j,k) =-rsq3               ! constant
       p(2,j,k) =-rsq3*tan(-0.25d0*pi+(j-1)*dp)
       p(3,j,k) = rsq3*tan(-0.25d0*pi+(k-1)*dp)
    enddo
 enddo

 call cart_to_latlon( (im+1)*(im+1), p, lamda, theta)

 end subroutine gnomonic_angl

 subroutine gnomonic_dist(im, lamda, theta)
! This is the commonly known equi-distance grid
 integer im
 real(kind=R_GRID) lamda(im+1,im+1)
 real(kind=R_GRID) theta(im+1,im+1)
 real(kind=R_GRID) p(3,im+1,im+1)
! Local
 real(kind=R_GRID) rsq3, xf, y0, z0, y, x, z, ds
 real(kind=R_GRID) dy, dz
 integer j,k

! Face-2

 rsq3 = 1.d0/sqrt(3.d0) 
 xf = -rsq3
 y0 =  rsq3;  dy = -2.d0*rsq3/im 
 z0 = -rsq3;  dz =  2.d0*rsq3/im

 do k=1,im+1
    do j=1,im+1
       p(1,j,k) = xf
       p(2,j,k) = y0 + (j-1)*dy
       p(3,j,k) = z0 + (k-1)*dz
    enddo
 enddo
 call cart_to_latlon( (im+1)*(im+1), p, lamda, theta)

 end subroutine gnomonic_dist

 subroutine symm_ed(im, lamda, theta)
! Make grid symmetrical to i=im/2+1
 integer im
 real(kind=R_GRID) lamda(im+1,im+1)
 real(kind=R_GRID) theta(im+1,im+1)
 integer i,j,ip,jp
 real(kind=R_GRID) avg

 do j=2,im+1
    do i=2,im
       lamda(i,j) = lamda(i,1)
    enddo
 enddo

 do j=1,im+1
    do i=1,im/2
       ip = im + 2 - i
       avg = 0.5d0*(lamda(i,j)-lamda(ip,j))
       lamda(i, j) = avg + pi
       lamda(ip,j) = pi - avg 
       avg = 0.5d0*(theta(i,j)+theta(ip,j))
       theta(i, j) = avg
       theta(ip,j) = avg
    enddo
 enddo

! Make grid symmetrical to j=im/2+1
 do j=1,im/2
       jp = im + 2 - j
    do i=2,im
       avg = 0.5d0*(lamda(i,j)+lamda(i,jp))
       lamda(i, j) = avg
       lamda(i,jp) = avg
       avg = 0.5d0*(theta(i,j)-theta(i,jp))
       theta(i, j) =  avg
       theta(i,jp) = -avg
    enddo
 enddo

 end subroutine symm_ed

 subroutine latlon2xyz2(lon, lat, p3)
 real(kind=R_GRID), intent(in):: lon, lat
 real(kind=R_GRID), intent(out):: p3(3)
 real(kind=R_GRID) e(2)

    e(1) = lon;    e(2) = lat
    call latlon2xyz(e, p3)

 end subroutine latlon2xyz2


 subroutine latlon2xyz(p, e, id)
!
! Routine to map (lon, lat) to (x,y,z)
!
 real(kind=R_GRID), intent(in) :: p(2)
 real(kind=R_GRID), intent(out):: e(3)
 integer, optional, intent(in):: id   ! id=0 do nothing; id=1, right_hand

 integer n
 real (f_p):: q(2)
 real (f_p):: e1, e2, e3

    do n=1,2
       q(n) = p(n)
    enddo

    e1 = cos(q(2)) * cos(q(1))
    e2 = cos(q(2)) * sin(q(1))
    e3 = sin(q(2))
!-----------------------------------
! Truncate to the desired precision:
!-----------------------------------
    e(1) = e1
    e(2) = e2
    e(3) = e3

 end subroutine latlon2xyz


 subroutine mirror_xyz(p1, p2, p0, p)

! Given the "mirror" as defined by p1(x1, y1, z1), p2(x2, y2, z2), and center 
! of the sphere, compute the mirror image of p0(x0, y0, z0) as p(x, y, z)

!-------------------------------------------------------------------------------
! for k=1,2,3 (x,y,z)
!
! p(k) = p0(k) - 2 * [p0(k) .dot. NB(k)] * NB(k)
!
! where 
!       NB(k) = p1(k) .cross. p2(k)         ---- direction of NB is imaterial
!       the normal unit vector to the "mirror" plane
!-------------------------------------------------------------------------------

 real(kind=R_GRID), intent(in) :: p1(3), p2(3), p0(3)
 real(kind=R_GRID), intent(out):: p(3)
!
 real(kind=R_GRID):: x1, y1, z1, x2, y2, z2, x0, y0, z0
 real(kind=R_GRID) nb(3)
 real(kind=R_GRID) pdot
 integer k

 call vect_cross(nb, p1, p2)
    pdot = sqrt(nb(1)**2+nb(2)**2+nb(3)**2)
 do k=1,3
    nb(k) = nb(k) / pdot
 enddo

 pdot = p0(1)*nb(1) + p0(2)*nb(2) + p0(3)*nb(3)
 do k=1,3
    p(k) = p0(k) - 2.d0*pdot*nb(k)
 enddo

 end subroutine mirror_xyz 


 subroutine mirror_latlon(lon1, lat1, lon2, lat2, lon0, lat0, lon3, lat3)
!
! Given the "mirror" as defined by (lon1, lat1), (lon2, lat2), and center 
! of the sphere, compute the mirror image of (lon0, lat0) as  (lon3, lat3)

 real(kind=R_GRID), intent(in):: lon1, lat1, lon2, lat2, lon0, lat0
 real(kind=R_GRID), intent(out):: lon3, lat3
!
 real(kind=R_GRID) p0(3), p1(3), p2(3), nb(3), pp(3), sp(2)
 real(kind=R_GRID) pdot
 integer k

 call latlon2xyz2(lon0, lat0, p0)
 call latlon2xyz2(lon1, lat1, p1)
 call latlon2xyz2(lon2, lat2, p2)
 call vect_cross(nb, p1, p2)

 pdot = sqrt(nb(1)**2+nb(2)**2+nb(3)**2)
 do k=1,3
    nb(k) = nb(k) / pdot
 enddo

 pdot = p0(1)*nb(1) + p0(2)*nb(2) + p0(3)*nb(3)
 do k=1,3
    pp(k) = p0(k) - 2.d0*pdot*nb(k)
 enddo

 call cart_to_latlon(1, pp, sp(1), sp(2))
 lon3 = sp(1)
 lat3 = sp(2)

 end subroutine  mirror_latlon


 subroutine cart_to_latlon(np, q, xs, ys)
! vector version of cart_to_latlon1
  integer, intent(in):: np
  real(kind=R_GRID), intent(inout):: q(3,np)
  real(kind=R_GRID), intent(inout):: xs(np), ys(np)
! local
  real(kind=R_GRID), parameter:: esl=1.d-10
  real (f_p):: p(3)
  real (f_p):: dist, lat, lon
  integer i,k

  do i=1,np
     do k=1,3
        p(k) = q(k,i)
     enddo
     dist = sqrt(p(1)**2 + p(2)**2 + p(3)**2)
     do k=1,3
        p(k) = p(k) / dist
     enddo

     if ( (abs(p(1))+abs(p(2)))  < esl ) then
          lon = real(0.,kind=f_p)
     else
          lon = atan2( p(2), p(1) )   ! range [-pi,pi]
     endif

     if ( lon < 0.) lon = real(2.,kind=f_p)*pi + lon
! RIGHT_HAND system:
     lat = asin(p(3))
     
     xs(i) = lon
     ys(i) = lat
! q Normalized:
     do k=1,3
        q(k,i) = p(k)
     enddo
  enddo

 end  subroutine cart_to_latlon



 subroutine vect_cross(e, p1, p2)
 real(kind=R_GRID), intent(in) :: p1(3), p2(3)
 real(kind=R_GRID), intent(out):: e(3)
!
! Perform cross products of 3D vectors: e = P1 X P2
!
      e(1) = p1(2)*p2(3) - p1(3)*p2(2)
      e(2) = p1(3)*p2(1) - p1(1)*p2(3)
      e(3) = p1(1)*p2(2) - p1(2)*p2(1)

 end subroutine vect_cross



 subroutine get_center_vect( npx, npy, pp, u1, u2, bd )
   type(fv_grid_bounds_type), intent(IN) :: bd
    integer, intent(in):: npx, npy
    real(kind=R_GRID), intent(in) :: pp(3,bd%isd:bd%ied+1,bd%jsd:bd%jed+1)
    real(kind=R_GRID), intent(out):: u1(3,bd%isd:bd%ied,  bd%jsd:bd%jed)
    real(kind=R_GRID), intent(out):: u2(3,bd%isd:bd%ied,  bd%jsd:bd%jed)
! Local:
    integer i,j,k
    real(kind=R_GRID) p1(3), p2(3), pc(3), p3(3)

    integer :: isd, ied, jsd, jed

      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

    do j=jsd,jed
       do i=isd,ied
        if ( (i<1       .and. j<1  )     .or. (i>(npx-1) .and. j<1) .or.  &
             (i>(npx-1) .and. j>(npy-1)) .or. (i<1       .and. j>(npy-1))) then
             u1(1:3,i,j) = 0.d0
             u2(1:3,i,j) = 0.d0
        else
#ifdef OLD_VECT
          do k=1,3
             u1(k,i,j) = pp(k,i+1,j)+pp(k,i+1,j+1) - pp(k,i,j)-pp(k,i,j+1)
             u2(k,i,j) = pp(k,i,j+1)+pp(k,i+1,j+1) - pp(k,i,j)-pp(k,i+1,j)
          enddo
          call normalize_vect( u1(1,i,j) )
          call normalize_vect( u2(1,i,j) )
#else
          call cell_center3(pp(1,i,j), pp(1,i+1,j), pp(1,i,j+1), pp(1,i+1,j+1), pc)
! e1:
          call mid_pt3_cart(pp(1,i,j),   pp(1,i,j+1),   p1)
          call mid_pt3_cart(pp(1,i+1,j), pp(1,i+1,j+1), p2)
          call vect_cross(p3, p2, p1)
          call vect_cross(u1(1,i,j), pc, p3)
          call normalize_vect( u1(1,i,j) )
! e2:
          call mid_pt3_cart(pp(1,i,j),   pp(1,i+1,j),   p1)
          call mid_pt3_cart(pp(1,i,j+1), pp(1,i+1,j+1), p2)
          call vect_cross(p3, p2, p1)
          call vect_cross(u2(1,i,j), pc, p3)
          call normalize_vect( u2(1,i,j) )
#endif
        endif
       enddo
    enddo

 end subroutine get_center_vect


 subroutine get_unit_vect2( e1, e2, uc )
   real(kind=R_GRID), intent(in) :: e1(2), e2(2)
   real(kind=R_GRID), intent(out):: uc(3) ! unit vector e1--->e2
! Local:
   real(kind=R_GRID), dimension(3):: pc, p1, p2, p3

! RIGHT_HAND system:
   call latlon2xyz(e1, p1)
   call latlon2xyz(e2, p2)

   call mid_pt3_cart(p1, p2,  pc)
   call vect_cross(p3, p2, p1)
   call vect_cross(uc, pc, p3)
   call normalize_vect( uc )

 end subroutine get_unit_vect2

 subroutine get_unit_vect3( p1, p2, uc )
   real(kind=R_GRID), intent(in) :: p1(3), p2(3)
   real(kind=R_GRID), intent(out):: uc(3)
! Local:
   real(kind=R_GRID), dimension(3):: pc, p3

   call mid_pt3_cart(p1, p2,  pc)
   call vect_cross(p3, p2, p1)
   call vect_cross(uc, pc, p3)
   call normalize_vect( uc )

 end subroutine get_unit_vect3



 subroutine normalize_vect(e)
!                              Make e an unit vector
 real(kind=R_GRID), intent(inout):: e(3)
 real(f_p):: pdot
 integer k

    pdot = e(1)**2 + e(2)**2 + e(3)**2
    pdot = sqrt( pdot ) 

    do k=1,3
       e(k) = e(k) / pdot
    enddo

 end subroutine normalize_vect


 subroutine intp_great_circle(beta, p1, p2, x_o, y_o)
 real(kind=R_GRID), intent(in)::  beta    ! [0,1]
 real(kind=R_GRID), intent(in)::  p1(2), p2(2)
 real(kind=R_GRID), intent(out):: x_o, y_o     ! between p1 and p2 along GC
!------------------------------------------
    real(kind=R_GRID):: pm(2)
    real(kind=R_GRID):: e1(3), e2(3), e3(3)
    real(kind=R_GRID):: s1, s2, s3, dd, alpha

      call latlon2xyz(p1, e1)
      call latlon2xyz(p2, e2)

       alpha = 1.d0 - beta

       s1 = alpha*e1(1) + beta*e2(1)
       s2 = alpha*e1(2) + beta*e2(2)
       s3 = alpha*e1(3) + beta*e2(3)

       dd = sqrt( s1**2 + s2**2 + s3**2 )

       e3(1) = s1 / dd
       e3(2) = s2 / dd
       e3(3) = s3 / dd

      call cart_to_latlon(1, e3, pm(1), pm(2))

      x_o = pm(1)
      y_o = pm(2)

 end subroutine intp_great_circle

 subroutine spherical_linear_interpolation(beta, p1, p2, pb)

   !This formula interpolates along the great circle connecting points p1 and p2. This formula is taken from http://en.wikipedia.org/wiki/Slerp and is attributed to Glenn Davis based on a concept by Ken Shoemake.

 real(kind=R_GRID), intent(in)::  beta    ! [0,1]
 real(kind=R_GRID), intent(in)::  p1(2), p2(2)
 real(kind=R_GRID), intent(out):: pb(2)   ! between p1 and p2 along GC
!------------------------------------------
 real(kind=R_GRID):: pm(2)
 real(kind=R_GRID):: e1(3), e2(3), eb(3)
 real(kind=R_GRID):: dd, alpha, omg
 
 if ( abs(p1(1) - p2(1)) < 1.d-8 .and. abs(p1(2) - p2(2)) < 1.d-8) then
    call mpp_error(WARNING, 'spherical_linear_interpolation was passed two colocated points.')
    pb = p1
    return
 end if

 call latlon2xyz(p1, e1)
 call latlon2xyz(p2, e2)

 dd = sqrt( e1(1)**2 + e1(2)**2 + e1(3)**2 )
 
 e1(1) = e1(1) / dd
 e1(2) = e1(2) / dd
 e1(3) = e1(3) / dd

 dd = sqrt( e2(1)**2 + e2(2)**2 + e2(3)**2 )
 
 e2(1) = e2(1) / dd
 e2(2) = e2(2) / dd
 e2(3) = e2(3) / dd

 alpha = 1.d0 - beta

 omg = acos( e1(1)*e2(1) + e1(2)*e2(2) + e1(3)*e2(3) )

 if ( abs(omg) < 1.d-5 ) then
    print*, 'spherical_linear_interpolation: ', omg, p1, p2
    call mpp_error(FATAL, 'spherical_linear_interpolation: interpolation not well defined between antipodal points')
 end if

 eb(1) = sin( beta*omg )*e2(1) + sin(alpha*omg)*e1(1)
 eb(2) = sin( beta*omg )*e2(2) + sin(alpha*omg)*e1(2)
 eb(3) = sin( beta*omg )*e2(3) + sin(alpha*omg)*e1(3)

 eb(1) = eb(1) / sin(omg)
 eb(2) = eb(2) / sin(omg)
 eb(3) = eb(3) / sin(omg)

 call cart_to_latlon(1, eb, pb(1), pb(2))

 end subroutine spherical_linear_interpolation

 subroutine mid_pt_sphere(p1, p2, pm)
      real(kind=R_GRID) , intent(IN)  :: p1(2), p2(2)
      real(kind=R_GRID) , intent(OUT) :: pm(2)
!------------------------------------------
      real(kind=R_GRID) e1(3), e2(3), e3(3)

      call latlon2xyz(p1, e1)
      call latlon2xyz(p2, e2)
      call mid_pt3_cart(e1, e2, e3)
      call cart_to_latlon(1, e3, pm(1), pm(2))

 end subroutine mid_pt_sphere



 subroutine mid_pt3_cart(p1, p2, e)
       real(kind=R_GRID), intent(IN)  :: p1(3), p2(3)
       real(kind=R_GRID), intent(OUT) :: e(3)
!
       real (f_p):: q1(3), q2(3)
       real (f_p):: dd, e1, e2, e3
       integer k

       do k=1,3
          q1(k) = p1(k)
          q2(k) = p2(k)
       enddo

       e1 = q1(1) + q2(1)
       e2 = q1(2) + q2(2)
       e3 = q1(3) + q2(3)

       dd = sqrt( e1**2 + e2**2 + e3**2 )
       e1 = e1 / dd
       e2 = e2 / dd
       e3 = e3 / dd

       e(1) = e1
       e(2) = e2
       e(3) = e3

 end subroutine mid_pt3_cart



 subroutine mid_pt_cart(p1, p2, e3)
    real(kind=R_GRID), intent(IN)  :: p1(2), p2(2)
    real(kind=R_GRID), intent(OUT) :: e3(3)
!-------------------------------------
    real(kind=R_GRID) e1(3), e2(3)

    call latlon2xyz(p1, e1)
    call latlon2xyz(p2, e2)
    call mid_pt3_cart(e1, e2, e3)

 end subroutine mid_pt_cart



 real function great_circle_dist( q1, q2, radius )
      real(kind=R_GRID), intent(IN)           :: q1(2), q2(2)
      real(kind=R_GRID), intent(IN), optional :: radius
 
      real (f_p):: p1(2), p2(2)
      real (f_p):: beta
      integer n

      do n=1,2
         p1(n) = q1(n)
         p2(n) = q2(n)
      enddo

      beta = asin( sqrt( sin((p1(2)-p2(2))/2.)**2 + cos(p1(2))*cos(p2(2))*   &
                         sin((p1(1)-p2(1))/2.)**2 ) ) * 2.

      if ( present(radius) ) then
           great_circle_dist = radius * beta
      else
           great_circle_dist = beta   ! Returns the angle
      endif

  end function great_circle_dist


  function great_circle_dist_cart(v1, v2, radius)
    !------------------------------------------------------------------!
    ! date:    July 2006                                               !
    ! version: 0.1                                                     !
    !                                                                  !
    ! calculate normalized great circle distance between v1 and v2     ! 
    !------------------------------------------------------------------!
    real(kind=R_GRID) :: great_circle_dist_cart
    real(kind=R_GRID), dimension(3), intent(in) :: v1, v2
    real(kind=R_GRID), intent(IN), optional :: radius
    real(kind=R_GRID) :: norm

    norm = (v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3))                  &
                *(v2(1)*v2(1)+v2(2)*v2(2)+v2(3)*v2(3))
    
    !if (norm <= 0.) print*, 'negative norm: ', norm, v1, v2

    great_circle_dist_cart=(v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3))                  &
           /sqrt(norm)
    great_circle_dist_cart = sign(min(1.,abs(great_circle_dist_cart)),great_circle_dist_cart)
    great_circle_dist_cart=acos(great_circle_dist_cart)

      if ( present(radius) ) then
           great_circle_dist_cart = radius * great_circle_dist_cart
      endif


  end function great_circle_dist_cart



 subroutine intersect(a1,a2,b1,b2,radius,x_inter,local_a,local_b)
  !--------------------------------------------------------------------!
  ! date:    July 2006                                                 !
  ! version: 0.1                                                       !
  !                                                                    !
  ! calculate intersection of two great circles                        !
  !--------------------------------------------------------------------!
    !------------------------------------------------------------------!
    ! calculate intersection of two great circles                      !
    !                                                                  !
    ! input:                                                           !
    ! a1, a2,  -   pairs of points on sphere in cartesian coordinates  !
    ! b1, b2       defining great circles                              !
    ! radius   -   radius of the sphere                                !
    !                                                                  !
    ! output:                                                          !
    ! x_inter  -   nearest intersection point of the great circles     !
    ! local_a  -   true if x1 between (a1, a2)                         !
    ! local_b  -   true if x1 between (b1, b2)                         !
    !------------------------------------------------------------------!
    real(kind=R_GRID), dimension(3), intent(in)  :: a1, a2, b1, b2
    real(kind=R_GRID), intent(in) :: radius
    real(kind=R_GRID), dimension(3), intent(out) :: x_inter
    logical, intent(out) :: local_a,local_b
    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!
    real(kind=R_GRID) :: a2_xy, b1_xy, b2_xy, a2_xz, b1_xz, b2_xz,                   &
            b1_xyz, b2_xyz, length
    !------------------------------------------------------------------!
    ! calculate intersection point                                     !
    !------------------------------------------------------------------!
    a2_xy=a2(1)*a1(2)-a2(2)*a1(1)
    b1_xy=b1(1)*a1(2)-b1(2)*a1(1)
    b2_xy=b2(1)*a1(2)-b2(2)*a1(1)

    a2_xz=a2(1)*a1(3)-a2(3)*a1(1)
    b1_xz=b1(1)*a1(3)-b1(3)*a1(1)
    b2_xz=b2(1)*a1(3)-b2(3)*a1(1)

    b1_xyz=b1_xy*a2_xz-b1_xz*a2_xy
    b2_xyz=b2_xy*a2_xz-b2_xz*a2_xy

    if (b1_xyz==0.0d0) then
       x_inter(:)=b1(:)
    elseif (b2_xyz==0.0d0) then
       x_inter(:)=b2(:)
    else
       x_inter(:)=b2(:)-b1(:)*b2_xyz/b1_xyz
       length=sqrt(x_inter(1)*x_inter(1)+x_inter(2)*x_inter(2)+x_inter(3)*x_inter(3))
       x_inter(:)=radius/length*x_inter(:)
    endif
    !------------------------------------------------------------------!
    ! check if intersection is between pairs of points on sphere       !
    !------------------------------------------------------------------!
    call get_nearest()
    call check_local(a1,a2,local_a)
    call check_local(b1,b2,local_b)

  contains
    !------------------------------------------------------------------!
    subroutine get_nearest()
      real(kind=R_GRID), dimension(3) :: center, dx
      real(kind=R_GRID) :: dist1,dist2

      center(:)=0.25*(a1(:)+a2(:)+b1(:)+b2(:))
      dx(:)=+x_inter(:)-center(:)
      dist1=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)
      dx(:)=-x_inter(:)-center(:)
      dist2=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)

      if (dist2<dist1) x_inter(:)=-x_inter(:)

    end subroutine get_nearest
    !------------------------------------------------------------------!
    subroutine check_local(x1,x2,local)
      real(kind=R_GRID), dimension(3), intent(in) :: x1,x2
      logical, intent(out) :: local

      real(kind=R_GRID), dimension(3) :: dx
      real(kind=R_GRID) :: dist, dist1, dist2

      dx(:)=x1(:)-x2(:)
      dist=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)
    
      dx(:)=x1(:)-x_inter(:)
      dist1=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)
      dx(:)=x2(:)-x_inter(:)
      dist2=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)

      if (dist1<=dist .and. dist2<=dist) then
         local=.true.
      else
         local=.false.
      endif
      
    end subroutine check_local
    !------------------------------------------------------------------!
  end subroutine intersect

 subroutine intersect_cross(a1,a2,b1,b2,radius,x_inter,local_a,local_b)
    !------------------------------------------------------------------!
    ! calculate intersection of two great circles                      !
    !                                                                  !
    ! input:                                                           !
    ! a1, a2,  -   pairs of points on sphere in cartesian coordinates  !
    ! b1, b2       defining great circles                              !
    ! radius   -   radius of the sphere                                !
    !                                                                  !
    ! output:                                                          !
    ! x_inter  -   nearest intersection point of the great circles     !
    ! local_a  -   true if x1 between (a1, a2)                         !
    ! local_b  -   true if x1 between (b1, b2)                         !
    !------------------------------------------------------------------!
    real(kind=R_GRID), dimension(3), intent(in)  :: a1, a2, b1, b2
    real(kind=R_GRID), intent(in) :: radius
    real(kind=R_GRID), dimension(3), intent(out) :: x_inter
    logical, intent(out) :: local_a,local_b
    real(kind=R_GRID), dimension(3) :: v1, v2

    !A great circle is the intersection of a plane through the center
    ! of the sphere with the sphere. That plane is specified by a
    ! vector v1, which is the cross product of any two vectors lying
    ! in the plane; here, we use position vectors, which are unit
    ! vectors lying in the plane and rooted at the center of the
    ! sphere. 
    !The intersection of two great circles is where the the
    ! intersection of the planes, a line, itself intersects the
    ! sphere. Since the planes are defined by perpendicular vectors
    ! v1, v2 respectively, the intersecting line is perpendicular
    ! to both v1 and v2, and so lies along the cross product of v1
    ! and v2.
    !The two intersection points of the great circles is therefore +/- v1 x v2.
    call vect_cross(v1, a1, a2)
    call vect_cross(v2, b1, b2)

    v1 = v1/sqrt(v1(1)**2 + v1(2)**2 + v1(3)**2)
    v2 = v2/sqrt(v2(1)**2 + v2(2)**2 + v2(3)**2)
    call vect_cross(x_inter, v1, v2)

    !Normalize
    x_inter = x_inter/sqrt(x_inter(1)**2 + x_inter(2)**2 + x_inter(3)**2)

    ! check if intersection is between pairs of points on sphere 
    call get_nearest()
    call check_local(a1,a2,local_a)
    call check_local(b1,b2,local_b)

  contains
    subroutine get_nearest()
      real(kind=R_GRID), dimension(3) :: center, dx
      real(kind=R_GRID) :: dist1,dist2

      center(:)=0.25*(a1(:)+a2(:)+b1(:)+b2(:))
      dx(:)=+x_inter(:)-center(:)
      dist1=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)
      dx(:)=-x_inter(:)-center(:)
      dist2=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)

      if (dist2<dist1) x_inter(:)=-x_inter(:)

    end subroutine get_nearest

    subroutine check_local(x1,x2,local)
      real(kind=R_GRID), dimension(3), intent(in) :: x1,x2
      logical, intent(out) :: local

      real(kind=R_GRID), dimension(3) :: dx
      real(kind=R_GRID) :: dist, dist1, dist2

      dx(:)=x1(:)-x2(:)
      dist=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)
    
      dx(:)=x1(:)-x_inter(:)
      dist1=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)
      dx(:)=x2(:)-x_inter(:)
      dist2=dx(1)*dx(1)+dx(2)*dx(2)+dx(3)*dx(3)

      if (dist1<=dist .and. dist2<=dist) then
         local=.true.
      else
         local=.false.
      endif
      
    end subroutine check_local
    !------------------------------------------------------------------!
  end subroutine intersect_cross



  subroutine unit_vect_latlon(pp, elon, elat)
      real(kind=R_GRID), intent(IN)  :: pp(2)
      real(kind=R_GRID), intent(OUT) :: elon(3), elat(3)

      real (f_p):: lon, lat
      real (f_p):: sin_lon, cos_lon, sin_lat, cos_lat

      lon = pp(1)
      lat = pp(2)

      sin_lon = sin(lon)
      cos_lon = cos(lon)
      sin_lat = sin(lat)
      cos_lat = cos(lat)

      elon(1) = -sin_lon
      elon(2) =  cos_lon
      elon(3) =  0.d0

      elat(1) = -sin_lat*cos_lon
      elat(2) = -sin_lat*sin_lon
      elat(3) =  cos_lat

  end subroutine unit_vect_latlon



  real(kind=R_GRID) function v_prod(v1, v2)
  real(kind=R_GRID) v1(3), v2(3)

       v_prod = v1(1)*v2(1) + v1(2)*v2(2) + v1(3)*v2(3)

  end function v_prod


  subroutine init_cubed_to_latlon( gridstruct, hydrostatic, agrid, grid_type, ord, bd )
  type(fv_grid_bounds_type), intent(IN) :: bd
  logical, intent(in):: hydrostatic
  real(kind=R_GRID),    intent(in) :: agrid(bd%isd:bd%ied,bd%jsd:bd%jed,2)
  integer, intent(in) :: grid_type
  integer, intent(in) :: ord
  type(fv_grid_type), intent(INOUT), target :: gridstruct
  integer i, j

  integer :: is,  ie,  js,  je

  !Local pointers
  real, pointer, dimension(:,:) :: a11, a12, a21, a22
  real, pointer, dimension(:,:) :: z11, z12, z21, z22
  real(kind=R_GRID), pointer, dimension(:,:,:) :: vlon, vlat
  real(kind=R_GRID), pointer, dimension(:,:,:) :: ee1, ee2, ec1, ec2

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je

  if ( grid_type < 4 ) then

     vlon   => gridstruct%vlon
     vlat   => gridstruct%vlat
     a11    => gridstruct%a11
     a12    => gridstruct%a12
     a21    => gridstruct%a21
     a22    => gridstruct%a22
     z11    => gridstruct%z11
     z12    => gridstruct%z12
     z21    => gridstruct%z21
     z22    => gridstruct%z22
     ee1    => gridstruct%ee1
     ee2    => gridstruct%ee2
     ec1    => gridstruct%ec1
     ec2    => gridstruct%ec2

     do j=js-2,je+2
        do i=is-2,ie+2
           call unit_vect_latlon(agrid(i,j,1:2), vlon(i,j,1:3), vlat(i,j,1:3))
        enddo
     enddo

     do j=js-1,je+1
        do i=is-1,ie+1
           z11(i,j) =  v_prod(ec1(1:3,i,j), vlon(i,j,1:3))
           z12(i,j) =  v_prod(ec1(1:3,i,j), vlat(i,j,1:3))
           z21(i,j) =  v_prod(ec2(1:3,i,j), vlon(i,j,1:3))
           z22(i,j) =  v_prod(ec2(1:3,i,j), vlat(i,j,1:3))
!-------------------------------------------------------------------------
           a11(i,j) =  0.5d0*z22(i,j) / gridstruct%sin_sg(i,j,5)
           a12(i,j) = -0.5d0*z12(i,j) / gridstruct%sin_sg(i,j,5)
           a21(i,j) = -0.5d0*z21(i,j) / gridstruct%sin_sg(i,j,5)
           a22(i,j) =  0.5d0*z11(i,j) / gridstruct%sin_sg(i,j,5)
! For 3D Coriolis force
!          if(.not.hydrostatic) gridstruct%w00(i,j) = 2.d0*omega*cos(agrid(i,j,2))
        enddo
     enddo
  endif

  end subroutine init_cubed_to_latlon


 subroutine cubed_to_latlon(u, v, ua, va, gridstruct, npx, npy, km, mode, grid_type, domain, nested, c2l_ord, bd)
 type(fv_grid_bounds_type), intent(IN) :: bd 
 integer, intent(in) :: km, npx, npy, grid_type, c2l_ord
 integer, intent(in) :: mode   ! update if present
 type(fv_grid_type), intent(IN) :: gridstruct
 real, intent(inout):: u(bd%isd:bd%ied,bd%jsd:bd%jed+1,km)
 real, intent(inout):: v(bd%isd:bd%ied+1,bd%jsd:bd%jed,km)
 real, intent(out):: ua(bd%isd:bd%ied, bd%jsd:bd%jed,km)
 real, intent(out):: va(bd%isd:bd%ied, bd%jsd:bd%jed,km)
 type(domain2d), intent(INOUT) :: domain
 logical, intent(IN) :: nested

 if ( c2l_ord == 2 ) then
      call c2l_ord2(u, v, ua, va, gridstruct, km, grid_type, bd, .false.)
 else
      call c2l_ord4(u, v, ua, va, gridstruct, npx, npy, km, grid_type, domain, nested, mode, bd)
 endif

 end subroutine cubed_to_latlon


 subroutine c2l_ord4(u, v, ua, va, gridstruct, npx, npy, km, grid_type, domain, nested, mode, bd)

 type(fv_grid_bounds_type), intent(IN) :: bd
  integer, intent(in) :: km, npx, npy, grid_type
  integer, intent(in):: mode   ! update if present
 type(fv_grid_type), intent(IN), target :: gridstruct
  real, intent(inout):: u(bd%isd:bd%ied,bd%jsd:bd%jed+1,km)
  real, intent(inout):: v(bd%isd:bd%ied+1,bd%jsd:bd%jed,km)
  real, intent(out)::  ua(bd%isd:bd%ied, bd%jsd:bd%jed,km)
  real, intent(out)::  va(bd%isd:bd%ied, bd%jsd:bd%jed,km)
  type(domain2d), intent(INOUT) :: domain
  logical, intent(IN) :: nested
! Local 
! 4-pt Lagrange interpolation
  real :: a1 =  0.5625
  real :: a2 = -0.0625
  real :: c1 =  1.125
  real :: c2 = -0.125
  real utmp(bd%is:bd%ie,  bd%js:bd%je+1)
  real vtmp(bd%is:bd%ie+1,bd%js:bd%je)
  real wu(bd%is:bd%ie,  bd%js:bd%je+1)
  real wv(bd%is:bd%ie+1,bd%js:bd%je)
  integer i, j, k

  integer :: is,  ie,  js,  je


  is  = bd%is
  ie  = bd%ie
  js  = bd%js
  je  = bd%je

  if ( mode > 0 ) then
                                   call timing_on('COMM_TOTAL')
       call mpp_update_domains(u, v, domain, gridtype=DGRID_NE)
                                  call timing_off('COMM_TOTAL')
  endif

!$OMP parallel do default(none) shared(is,ie,js,je,km,npx,npy,grid_type,nested,c2,c1, &
!$OMP                                  u,v,gridstruct,ua,va,a1,a2)         &
!$OMP                          private(utmp, vtmp, wu, wv)
 do k=1,km
   if ( grid_type < 4 ) then
    if (nested) then
     do j=max(1,js),min(npy-1,je)
        do i=max(1,is),min(npx-1,ie)
           utmp(i,j) = c2*(u(i,j-1,k)+u(i,j+2,k)) + c1*(u(i,j,k)+u(i,j+1,k))
           vtmp(i,j) = c2*(v(i-1,j,k)+v(i+2,j,k)) + c1*(v(i,j,k)+v(i+1,j,k))
        enddo
     enddo
   else
     do j=max(2,js),min(npy-2,je)
        do i=max(2,is),min(npx-2,ie)
           utmp(i,j) = c2*(u(i,j-1,k)+u(i,j+2,k)) + c1*(u(i,j,k)+u(i,j+1,k))
           vtmp(i,j) = c2*(v(i-1,j,k)+v(i+2,j,k)) + c1*(v(i,j,k)+v(i+1,j,k))
        enddo
     enddo

    if ( js==1  ) then
         do i=is,ie+1
            wv(i,1) = v(i,1,k)*gridstruct%dy(i,1)
         enddo
         do i=is,ie
            vtmp(i,1) = 2.*(wv(i,1) + wv(i+1,1)) / (gridstruct%dy(i,1)+gridstruct%dy(i+1,1))
            utmp(i,1) = 2.*(u(i,1,k)*gridstruct%dx(i,1) + u(i,2,k)*gridstruct%dx(i,2))   &
                         / (         gridstruct%dx(i,1) +          gridstruct%dx(i,2))
!!!         vtmp(i,1) = (wv(i,1) + wv(i+1,1)) * gridstruct%rdya(i,1)
!!!         utmp(i,1) = (u(i,1,k)*gridstruct%dx(i,1) + u(i,2,k)*gridstruct%dx(i,2)) * gridstruct%rdxa(i,1)
         enddo
    endif

    if ( (je+1)==npy   ) then
         j = npy-1
         do i=is,ie+1
            wv(i,j) = v(i,j,k)*gridstruct%dy(i,j)
         enddo
         do i=is,ie
            vtmp(i,j) = 2.*(wv(i,j) + wv(i+1,j)) / (gridstruct%dy(i,j)+gridstruct%dy(i+1,j))
            utmp(i,j) = 2.*(u(i,j,k)*gridstruct%dx(i,j) + u(i,j+1,k)*gridstruct%dx(i,j+1))   &
                         / (         gridstruct%dx(i,j) +            gridstruct%dx(i,j+1))
!!!         vtmp(i,j) = (wv(i,j) + wv(i+1,j)) * gridstruct%rdya(i,j)
!!!         utmp(i,j) = (u(i,j,k)*gridstruct%dx(i,j) + u(i,j+1,k)*gridstruct%dx(i,j+1)) * gridstruct%rdxa(i,j)
         enddo
    endif

    if ( is==1 ) then
      i = 1
      do j=js,je
         wv(1,j) = v(1,j,k)*gridstruct%dy(1,j)
         wv(2,j) = v(2,j,k)*gridstruct%dy(2,j)
      enddo
      do j=js,je+1
         wu(i,j) = u(i,j,k)*gridstruct%dx(i,j)
      enddo
      do j=js,je
         utmp(i,j) = 2.*(wu(i,j) + wu(i,j+1))/(gridstruct%dx(i,j)+gridstruct%dx(i,j+1))
         vtmp(i,j) = 2.*(wv(1,j) + wv(2,j  ))/(gridstruct%dy(1,j)+gridstruct%dy(2,j))
!!!      utmp(i,j) = (wu(i,j) + wu(i,  j+1)) * gridstruct%rdxa(i,j)
!!!      vtmp(i,j) = (wv(i,j) + wv(i+1,j  )) * gridstruct%rdya(i,j)
      enddo
    endif

    if ( (ie+1)==npx) then
      i = npx-1
      do j=js,je
         wv(i,  j) = v(i,  j,k)*gridstruct%dy(i,  j)
         wv(i+1,j) = v(i+1,j,k)*gridstruct%dy(i+1,j)
      enddo
      do j=js,je+1
         wu(i,j) = u(i,j,k)*gridstruct%dx(i,j)
      enddo
      do j=js,je
         utmp(i,j) = 2.*(wu(i,j) + wu(i,  j+1))/(gridstruct%dx(i,j)+gridstruct%dx(i,j+1))
         vtmp(i,j) = 2.*(wv(i,j) + wv(i+1,j  ))/(gridstruct%dy(i,j)+gridstruct%dy(i+1,j))
!!!      utmp(i,j) = (wu(i,j) + wu(i,  j+1)) * gridstruct%rdxa(i,j)
!!!      vtmp(i,j) = (wv(i,j) + wv(i+1,j  )) * gridstruct%rdya(i,j)
      enddo
    endif

 endif !nested

 !Transform local a-grid winds into latitude-longitude coordinates
     do j=js,je
        do i=is,ie
           ua(i,j,k) = gridstruct%a11(i,j)*utmp(i,j) + gridstruct%a12(i,j)*vtmp(i,j)
           va(i,j,k) = gridstruct%a21(i,j)*utmp(i,j) + gridstruct%a22(i,j)*vtmp(i,j)
        enddo
     enddo
   else
! Simple Cartesian Geometry:
     do j=js,je
        do i=is,ie
           ua(i,j,k) = a2*(u(i,j-1,k)+u(i,j+2,k)) + a1*(u(i,j,k)+u(i,j+1,k))
           va(i,j,k) = a2*(v(i-1,j,k)+v(i+2,j,k)) + a1*(v(i,j,k)+v(i+1,j,k))
        enddo
     enddo
   endif
 enddo
 end subroutine c2l_ord4

 subroutine c2l_ord2(u, v, ua, va, gridstruct, km, grid_type, bd, do_halo)
 type(fv_grid_bounds_type), intent(IN) :: bd
  integer, intent(in) :: km, grid_type
  real, intent(in) ::  u(bd%isd:bd%ied,bd%jsd:bd%jed+1,km)
  real, intent(in) ::  v(bd%isd:bd%ied+1,bd%jsd:bd%jed,km)
 type(fv_grid_type), intent(IN), target :: gridstruct
 logical, intent(in) :: do_halo
!
  real, intent(out):: ua(bd%isd:bd%ied, bd%jsd:bd%jed,km)
  real, intent(out):: va(bd%isd:bd%ied, bd%jsd:bd%jed,km)
!--------------------------------------------------------------
! Local 
  real wu(bd%is-1:bd%ie+1,  bd%js-1:bd%je+2)
  real wv(bd%is-1:bd%ie+2,  bd%js-1:bd%je+1)
  real u1(bd%is-1:bd%ie+1), v1(bd%is-1:bd%ie+1)
  integer i, j, k
  integer :: is,  ie,  js,  je

  real, dimension(:,:), pointer :: a11, a12, a21, a22
  real, dimension(:,:), pointer :: dx, dy, rdxa, rdya

  a11 => gridstruct%a11
  a12 => gridstruct%a12
  a21 => gridstruct%a21
  a22 => gridstruct%a22

  dx   => gridstruct%dx
  dy   => gridstruct%dy
  rdxa => gridstruct%rdxa
  rdya => gridstruct%rdya

  if (do_halo) then
     is  = bd%is-1
     ie  = bd%ie+1
     js  = bd%js-1
     je  = bd%je+1
  else
     is  = bd%is
     ie  = bd%ie
     js  = bd%js
     je  = bd%je
  endif

!$OMP parallel do default(none) shared(is,ie,js,je,km,grid_type,u,dx,v,dy,ua,va,a11,a12,a21,a22) &
!$OMP                          private(u1, v1, wu, wv)
  do k=1,km
     if ( grid_type < 4 ) then
       do j=js,je+1
          do i=is,ie
             wu(i,j) = u(i,j,k)*dx(i,j)
          enddo
       enddo
       do j=js,je
          do i=is,ie+1
             wv(i,j) = v(i,j,k)*dy(i,j)
          enddo
       enddo

       do j=js,je
          do i=is,ie
! Co-variant to Co-variant "vorticity-conserving" interpolation
             u1(i) = 2.*(wu(i,j) + wu(i,j+1)) / (dx(i,j)+dx(i,j+1))
             v1(i) = 2.*(wv(i,j) + wv(i+1,j)) / (dy(i,j)+dy(i+1,j))
!!!          u1(i) = (wu(i,j) + wu(i,j+1)) * rdxa(i,j)
!!!          v1(i) = (wv(i,j) + wv(i+1,j)) * rdya(i,j)
! Cubed (cell center co-variant winds) to lat-lon:
             ua(i,j,k) = a11(i,j)*u1(i) + a12(i,j)*v1(i)
             va(i,j,k) = a21(i,j)*u1(i) + a22(i,j)*v1(i)
          enddo
       enddo
     else
! 2nd order:
       do j=js,je
          do i=is,ie
             ua(i,j,k) = 0.5*(u(i,j,k)+u(i,  j+1,k))
             va(i,j,k) = 0.5*(v(i,j,k)+v(i+1,j,  k))
          enddo
       enddo
     endif
  enddo

 end subroutine c2l_ord2


 subroutine expand_cell(q1, q2, q3, q4, a1, a2, a3, a4, fac)
! Util for land model (for BW)
!
!        4----3
!        |  . |
!        1----2
!
      real(kind=R_GRID), intent(in):: q1(2), q2(2), q3(2), q4(2)
      real(kind=R_GRID), intent(in):: fac    ! expansion factor: outside: > 1
                                ! fac = 1: qq1 returns q1
                                ! fac = 0: qq1 returns the center position
      real(kind=R_GRID), intent(out):: a1(2), a2(2), a3(2), a4(2)
! Local
      real(kind=R_GRID) qq1(3), qq2(3), qq3(3), qq4(3)
      real(kind=R_GRID) p1(3), p2(3), p3(3), p4(3)
      real(kind=R_GRID) ec(3)
      real(f_p):: dd, d1, d2, d3, d4
      integer k

! Transform to (x,y,z)
      call latlon2xyz(q1, p1)
      call latlon2xyz(q2, p2)
      call latlon2xyz(q3, p3)
      call latlon2xyz(q4, p4)

! Get center position:
      do k=1,3
         ec(k) = p1(k) + p2(k) + p3(k) + p4(k)
      enddo
      dd = sqrt( ec(1)**2 + ec(2)**2 + ec(3)**2 )

      do k=1,3
         ec(k) = ec(k) / dd   ! cell center position
      enddo

! Perform the "extrapolation" in 3D (x-y-z) 
      do k=1,3
         qq1(k) = ec(k) + fac*(p1(k)-ec(k)) 
         qq2(k) = ec(k) + fac*(p2(k)-ec(k)) 
         qq3(k) = ec(k) + fac*(p3(k)-ec(k)) 
         qq4(k) = ec(k) + fac*(p4(k)-ec(k)) 
      enddo

!--------------------------------------------------------
! Force the points to be on the sphere via normalization
!--------------------------------------------------------
      d1 = sqrt( qq1(1)**2 + qq1(2)**2 + qq1(3)**2 )
      d2 = sqrt( qq2(1)**2 + qq2(2)**2 + qq2(3)**2 )
      d3 = sqrt( qq3(1)**2 + qq3(2)**2 + qq3(3)**2 )
      d4 = sqrt( qq4(1)**2 + qq4(2)**2 + qq4(3)**2 )
      do k=1,3
         qq1(k) = qq1(k) / d1
         qq2(k) = qq2(k) / d2
         qq3(k) = qq3(k) / d3
         qq4(k) = qq4(k) / d4
      enddo

!----------------------------------------
! Transform back to lat-lon coordinates:
!----------------------------------------

      call cart_to_latlon(1, qq1, a1(1), a1(2))
      call cart_to_latlon(1, qq2, a2(1), a2(2))
      call cart_to_latlon(1, qq3, a3(1), a3(2))
      call cart_to_latlon(1, qq4, a4(1), a4(2))

 end subroutine expand_cell


 subroutine cell_center2(q1, q2, q3, q4, e2)
      real(kind=R_GRID) , intent(in ) :: q1(2), q2(2), q3(2), q4(2)
      real(kind=R_GRID) , intent(out) :: e2(2)
! Local
      real(kind=R_GRID) p1(3), p2(3), p3(3), p4(3)
      real(kind=R_GRID) ec(3)
      real(kind=R_GRID) dd
      integer k

      call latlon2xyz(q1, p1)
      call latlon2xyz(q2, p2)
      call latlon2xyz(q3, p3)
      call latlon2xyz(q4, p4)

      do k=1,3
         ec(k) = p1(k) + p2(k) + p3(k) + p4(k)
      enddo
      dd = sqrt( ec(1)**2 + ec(2)**2 + ec(3)**2 )

      do k=1,3
         ec(k) = ec(k) / dd
      enddo

      call cart_to_latlon(1, ec, e2(1), e2(2))

 end subroutine cell_center2


 subroutine cell_center3(p1, p2, p3, p4, ec)
! Get center position of a cell
         real(kind=R_GRID) , intent(IN)  :: p1(3), p2(3), p3(3), p4(3)
         real(kind=R_GRID) , intent(OUT) :: ec(3)
! Local
         real (kind=R_GRID)dd
         integer k

         do k=1,3
            ec(k) = p1(k) + p2(k) + p3(k) + p4(k)
         enddo
         dd = sqrt( ec(1)**2 + ec(2)**2 + ec(3)**2 )

         do k=1,3
            ec(k) = ec(k) / dd
         enddo

 end subroutine cell_center3



 real(kind=R_GRID) function get_area(p1, p4, p2, p3, radius)
!-----------------------------------------------
 real(kind=R_GRID), intent(in), dimension(2):: p1, p2, p3, p4
 real(kind=R_GRID), intent(in), optional:: radius
!-----------------------------------------------
 real(kind=R_GRID) e1(3), e2(3), e3(3)
 real(kind=R_GRID) ang1, ang2, ang3, ang4

! S-W: 1
       call latlon2xyz(p1, e1)   ! p1
       call latlon2xyz(p2, e2)   ! p2
       call latlon2xyz(p4, e3)   ! p4
       ang1 = spherical_angle(e1, e2, e3)
!----
! S-E: 2
!----
       call latlon2xyz(p2, e1)
       call latlon2xyz(p3, e2)
       call latlon2xyz(p1, e3)
       ang2 = spherical_angle(e1, e2, e3)
!----
! N-E: 3
!----
       call latlon2xyz(p3, e1)
       call latlon2xyz(p4, e2)
       call latlon2xyz(p2, e3)
       ang3 = spherical_angle(e1, e2, e3)
!----
! N-W: 4
!----
       call latlon2xyz(p4, e1)
       call latlon2xyz(p3, e2)
       call latlon2xyz(p1, e3)
       ang4 = spherical_angle(e1, e2, e3)

       if ( present(radius) ) then
            get_area = (ang1 + ang2 + ang3 + ang4 - 2.*pi) * radius**2
       else
            get_area = ang1 + ang2 + ang3 + ang4 - 2.*pi
       endif

 end function get_area


  function dist2side(v1, v2, point)
    !------------------------------------------------------------------!
    ! calculate shortest normalized distance on sphere                 !
    ! from point to straight line defined by v1 and v2                 !
    ! This version uses cartesian coordinates.                         !
    ! date:    Feb 2007                                                !
    ! version: 0.1                                                     !
    !------------------------------------------------------------------!
    real(kind=R_GRID) :: dist2side
    real(kind=R_GRID), dimension(3), intent(in) :: v1, v2, point

    real(kind=R_GRID) :: angle, side

    angle = spherical_angle(v1, v2, point)
    side = great_circle_dist_cart(v1, point)
    dist2side = asin(sin(side)*sin(angle))

  end function dist2side

  function dist2side_latlon(v1,v2,point)
    !Version of dist2side that takes points in latitude-longitude coordinates

    real(kind=R_GRID) :: dist2side_latlon
    real(kind=R_GRID), dimension(2), intent(in) :: v1, v2, point

    real(kind=R_GRID),dimension(3) :: c1, c2, cpoint

    real(kind=R_GRID) :: angle,side

    !no version of spherical angle for lat-lon coords
    call latlon2xyz(v1,c1)
    call latlon2xyz(v2,c2)
    call latlon2xyz(point,cpoint)
    angle = spherical_angle(c1,c2,cpoint)

    side = great_circle_dist(v1,point)

    dist2side_latlon = asin(sin(side)*sin(angle))

    !!dist2side_latlon = dist2side(c1,c2,cpoint)

  end function dist2side_latlon



 real(kind=R_GRID) function spherical_angle(p1, p2, p3)
 
!           p3
!         /
!        /
!       p1 ---> angle
!         \
!          \
!           p2

 real(kind=R_GRID) p1(3), p2(3), p3(3)

 real (f_p):: e1(3), e2(3), e3(3)
 real (f_p):: px, py, pz
 real (f_p):: qx, qy, qz
 real (f_p):: angle, ddd
 integer n

  do n=1,3
     e1(n) = p1(n)
     e2(n) = p2(n)
     e3(n) = p3(n)
  enddo

!-------------------------------------------------------------------
! Page 41, Silverman's book on Vector Algebra; spherical trigonmetry
!-------------------------------------------------------------------
! Vector P:
   px = e1(2)*e2(3) - e1(3)*e2(2) 
   py = e1(3)*e2(1) - e1(1)*e2(3) 
   pz = e1(1)*e2(2) - e1(2)*e2(1) 
! Vector Q:
   qx = e1(2)*e3(3) - e1(3)*e3(2) 
   qy = e1(3)*e3(1) - e1(1)*e3(3) 
   qz = e1(1)*e3(2) - e1(2)*e3(1) 

   ddd = (px*px+py*py+pz*pz)*(qx*qx+qy*qy+qz*qz)

   if ( ddd <= 0.0d0 ) then
        angle = 0.d0
   else
        ddd = (px*qx+py*qy+pz*qz) / sqrt(ddd)
        if ( abs(ddd)>1.d0) then
             angle = 2.d0*atan(1.0)    ! 0.5*pi
           !FIX (lmh) to correctly handle co-linear points (angle near pi or 0)
           if (ddd < 0.d0) then
              angle = 4.d0*atan(1.0d0) !should be pi
           else
              angle = 0.d0 
           end if
        else
             angle = acos( ddd )
        endif
   endif

   spherical_angle = angle

 end function spherical_angle


 real(kind=R_GRID) function cos_angle(p1, p2, p3)
! As spherical_angle, but returns the cos(angle)
!       p3
!       ^  
!       |  
!       | 
!       p1 ---> p2
!
 real(kind=R_GRID), intent(in):: p1(3), p2(3), p3(3)

 real (f_p):: e1(3), e2(3), e3(3)
 real (f_p):: px, py, pz
 real (f_p):: qx, qy, qz
 real (f_p):: angle, ddd
 integer n

  do n=1,3
     e1(n) = p1(n)
     e2(n) = p2(n)
     e3(n) = p3(n)
  enddo

!-------------------------------------------------------------------
! Page 41, Silverman's book on Vector Algebra; spherical trigonmetry
!-------------------------------------------------------------------
! Vector P:= e1 X e2
   px = e1(2)*e2(3) - e1(3)*e2(2) 
   py = e1(3)*e2(1) - e1(1)*e2(3) 
   pz = e1(1)*e2(2) - e1(2)*e2(1) 

! Vector Q: e1 X e3
   qx = e1(2)*e3(3) - e1(3)*e3(2) 
   qy = e1(3)*e3(1) - e1(1)*e3(3) 
   qz = e1(1)*e3(2) - e1(2)*e3(1) 

! ddd = sqrt[ (P*P) (Q*Q) ]
   ddd = sqrt( (px**2+py**2+pz**2)*(qx**2+qy**2+qz**2) )
   if ( ddd > 0.d0 ) then
        angle = (px*qx+py*qy+pz*qz) / ddd 
   else
        angle = 1.d0
   endif
   cos_angle = angle

 end function cos_angle



 real function g_sum(domain, p, ifirst, ilast, jfirst, jlast, ngc, area, mode, reproduce)
! Fast version of globalsum 
      integer, intent(IN) :: ifirst, ilast
      integer, intent(IN) :: jfirst, jlast, ngc
      integer, intent(IN) :: mode  ! if ==1 divided by area
      logical, intent(in), optional :: reproduce
      real, intent(IN) :: p(ifirst:ilast,jfirst:jlast)      ! field to be summed
      real(kind=R_GRID), intent(IN) :: area(ifirst-ngc:ilast+ngc,jfirst-ngc:jlast+ngc)
      type(domain2d), intent(IN) :: domain
      integer :: i,j
      real gsum
      logical, SAVE :: g_sum_initialized = .false.
      real(kind=R_GRID), SAVE :: global_area
      real :: tmp(ifirst:ilast,jfirst:jlast) 
        
      if ( .not. g_sum_initialized ) then
         global_area = mpp_global_sum(domain, area, flags=BITWISE_EFP_SUM)
         if ( is_master() ) write(*,*) 'Global Area=',global_area
         g_sum_initialized = .true.
      end if
 
!-------------------------
! FMS global sum algorithm:
!-------------------------
      if ( present(reproduce) ) then
         if (reproduce) then
            gsum = mpp_global_sum(domain, p(:,:)*area(ifirst:ilast,jfirst:jlast), &
                                  flags=BITWISE_EFP_SUM)
         else
            gsum = mpp_global_sum(domain, p(:,:)*area(ifirst:ilast,jfirst:jlast))
         endif
      else
!-------------------------
! Quick local sum algorithm
!-------------------------
         gsum = 0.
         do j=jfirst,jlast
            do i=ifirst,ilast
               gsum = gsum + p(i,j)*area(i,j)
            enddo
         enddo
         call mp_reduce_sum(gsum)
      endif

      if ( mode==1 ) then
           g_sum = gsum / global_area
      else
           g_sum = gsum
      endif

 end function g_sum


 real function global_qsum(p, ifirst, ilast, jfirst, jlast)
! quick global sum without area weighting
      integer, intent(IN) :: ifirst, ilast
      integer, intent(IN) :: jfirst, jlast
      real, intent(IN) :: p(ifirst:ilast,jfirst:jlast)      ! field to be summed
      integer :: i,j
      real gsum
         
      gsum = 0.
      do j=jfirst,jlast
         do i=ifirst,ilast
            gsum = gsum + p(i,j)
         enddo
      enddo
      call mp_reduce_sum(gsum)

      global_qsum  = gsum

 end function global_qsum


 subroutine global_mx(q, n_g, qmin, qmax, bd)

   type(fv_grid_bounds_type), intent(IN) :: bd
     integer, intent(in):: n_g
     real(kind=R_GRID), intent(in)::q(bd%is-n_g:bd%ie+n_g, bd%js-n_g:bd%je+n_g)
     real(kind=R_GRID), intent(out):: qmin, qmax
     integer i,j

     integer :: is,  ie,  js,  je

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je

      qmin = q(is,js)
      qmax = qmin
      do j=js,je
         do i=is,ie
            qmin = min(qmin, q(i,j))
            qmax = max(qmax, q(i,j))
         enddo
      enddo
      call mp_reduce_min(qmin)
      call mp_reduce_max(qmax)

 end subroutine global_mx

 subroutine global_mx_c(q, i1, i2, j1, j2, qmin, qmax)
! For computing global max/min at cell Corners
     integer, intent(in):: i1, i2, j1, j2
     real(kind=R_GRID), intent(in)   :: q(i1:i2,j1:j2)
     real(kind=R_GRID), intent(out)  :: qmin, qmax
     integer i,j

      qmin = q(i1,j1)
      qmax = qmin
      do j=j1,j2
         do i=i1,i2
            qmin = min(qmin, q(i,j))
            qmax = max(qmax, q(i,j))
         enddo
      enddo
      call mp_reduce_min(qmin)
      call mp_reduce_max(qmax)

 end subroutine global_mx_c


#ifdef OVERLOAD_R4
  subroutine fill_ghost_r4(q, npx, npy, value, bd)

 type(fv_grid_bounds_type), intent(IN) :: bd
  real(kind=4), intent(inout):: q(bd%isd:bd%ied,bd%jsd:bd%jed)
  integer, intent(in):: npx, npy
  real, intent(in):: value
  integer i,j

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
 
     do j=jsd,jed
        do i=isd,ied
           if ( (i<1 .and. j<1) ) then
                q(i,j) = value
           endif
           if ( i>(npx-1) .and. j<1 ) then
                q(i,j) = value
           endif
           if ( i>(npx-1) .and. j>(npy-1) ) then
                q(i,j) = value
           endif
           if ( i<1 .and. j>(npy-1) ) then
                q(i,j) = value
           endif
        enddo
     enddo

  end subroutine fill_ghost_r4
#endif

  subroutine fill_ghost_r8(q, npx, npy, value, bd)

 type(fv_grid_bounds_type), intent(IN) :: bd
  real(kind=R_GRID), intent(inout):: q(bd%isd:bd%ied,bd%jsd:bd%jed)
  integer, intent(in):: npx, npy
  real, intent(in):: value
  integer i,j

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
  
     do j=jsd,jed
        do i=isd,ied
           if ( (i<1 .and. j<1) ) then
                q(i,j) = value
           endif
           if ( i>(npx-1) .and. j<1 ) then
                q(i,j) = value
           endif
           if ( i>(npx-1) .and. j>(npy-1) ) then
                q(i,j) = value
           endif
           if ( i<1 .and. j>(npy-1) ) then
                q(i,j) = value
           endif
        enddo
     enddo

  end subroutine fill_ghost_r8



 subroutine make_eta_level(km, pe, area, kks, ak, bk, ptop, domain, bd)
 type(fv_grid_bounds_type), intent(IN) :: bd
  integer, intent(in ):: km
  integer, intent(out):: kks
  real(kind=R_GRID), intent(in):: area(bd%isd:bd%ied,bd%jsd:bd%jed)
  real, intent(INOUT) :: ptop
  real, intent(inout):: pe(bd%is-1:bd%ie+1,km+1,bd%js-1:bd%je+1)
  real, intent(out):: ak(km+1), bk(km+1)
  type(domain2d), intent(IN) :: domain
! local:
  real ph(km+1)
  real, allocatable:: pem(:,:)
  real(kind=4) :: p4
  integer k, i, j
  integer :: is,  ie,  js,  je

  is  = bd%is
  ie  = bd%ie
  js  = bd%js
  je  = bd%je

     allocate ( pem(is:ie,js:je) )

! Compute global mean values:
     do k=1,km+1
        do j=js,je
           do i=is,ie
               pem(i,j) = pe(i,k,j)
           enddo
        enddo
! Make it the same across all PEs
!       ph(k) = g_sum(pem, is, ie, js, je, ng, area, 1, .true.)
        p4 = g_sum(domain, pem, is, ie, js, je, ng, area, 1)
        ph(k) = p4
     enddo

     ptop = ph(1)
     do j=js-1,je+1
        do i=is-1,ie+1
           pe(i,1,j) = ptop 
        enddo
     enddo

! Faking a's and b's for code compatibility with hybrid sigma-p
     kks = 0
     ak(1) = ph(1)
     bk(1) = 0.
     ak(km+1) = 0.
     bk(km+1) = 1.

     do k=2,km
        bk(k) = (ph(k) - ph(1)) / (ph(km+1)-ph(1))
        ak(k) = ph(1)*(1.-bk(k))
     enddo

    if ( is_master() ) then
         write(*,*) 'Make_eta_level ...., ptop=', ptop
#ifdef PRINT_GRID
         do k=1,km+1
            write(*,*) ph(k), ak(k), bk(k)
         enddo
#endif
    endif

    deallocate ( pem )

 end subroutine make_eta_level

 subroutine invert_matrix(n, a, x)
  integer, intent (in) :: n
  integer :: i,j,k
  real(kind=R_GRID), intent (inout), dimension (n,n):: a
  real(kind=R_GRID), intent (out), dimension (n,n):: x   ! inverted maxtrix
  real(kind=R_GRID), dimension (n,n) :: b
  integer indx(n)
 
  do i = 1, n
     do j = 1, n
        b(i,j) = 0.0
     end do
  end do

  do i = 1, n
     b(i,i) = 1.0
  end do
 
  call elgs (a,n,indx)
 
  do i = 1, n-1
     do j = i+1, n
        do k = 1, n
           b(indx(j),k) = b(indx(j),k) - a(indx(j),i)*b(indx(i),k)
        end do
     end do
  end do
 
  do i = 1, n
     x(n,i) = b(indx(n),i)/a(indx(n),n)
     do j = n-1, 1, -1
        x(j,i) = b(indx(j),i)
        do k = j+1, n
           x(j,i) = x(j,i)-a(indx(j),k)*x(k,i)
        end do
        x(j,i) =  x(j,i)/a(indx(j),j)
     end do
  end do

 end subroutine invert_matrix
 

 subroutine elgs (a,n,indx)

!------------------------------------------------------------------
! subroutine to perform the partial-pivoting gaussian elimination.
! a(n,n) is the original matrix in the input and transformed matrix
! plus the pivoting element ratios below the diagonal in the output.
!------------------------------------------------------------------
 
  integer, intent (in) :: n
  integer :: i,j,k,itmp
  integer, intent (out), dimension (n) :: indx
  real(kind=R_GRID), intent (inout), dimension (n,n) :: a
!
  real(kind=R_GRID) :: c1, pie, pi1, pj
  real(kind=R_GRID), dimension (n) :: c
 
  do i = 1, n
     indx(i) = i
  end do
!
! find the rescaling factors, one from each row
!
  do i = 1, n
     c1= 0.0
     do j = 1, n
        c1 = max(c1,abs(a(i,j)))
     end do
     c(i) = c1
  end do
!
! search the pivoting (largest) element from each column
!
  do j = 1, n-1
     pi1 = 0.0
     do i = j, n
        pie = abs(a(indx(i),j))/c(indx(i))
        if (pie > pi1) then
            pi1 = pie
            k   = i
        endif
     end do
!
! interchange the rows via indx(n) to record pivoting order
!
    itmp    = indx(j)
    indx(j) = indx(k)
    indx(k) = itmp
    do i = j+1, n
       pj  = a(indx(i),j)/a(indx(j),j)
!
! record pivoting ratios below the diagonal
!
       a(indx(i),j) = pj
!
! modify other elements accordingly
!
       do k = j+1, n
          a(indx(i),k) = a(indx(i),k)-pj*a(indx(j),k)
       end do
     end do
  end do
 
 end subroutine elgs

 subroutine get_latlon_vector(pp, elon, elat)
 real(kind=R_GRID), intent(IN)  :: pp(2)
 real(kind=R_GRID), intent(OUT) :: elon(3), elat(3)

         elon(1) = -SIN(pp(1))
         elon(2) =  COS(pp(1))
         elon(3) =  0.0
         elat(1) = -SIN(pp(2))*COS(pp(1))
         elat(2) = -SIN(pp(2))*SIN(pp(1))
!!! RIGHT_HAND
         elat(3) =  COS(pp(2))
! Left-hand system needed to be consistent with rest of the codes
!        elat(3) = -COS(pp(2))

 end subroutine get_latlon_vector

 
  

 subroutine project_sphere_v( np, f, e )
!---------------------------------
 integer, intent(in):: np           ! total number of points
 real(kind=R_GRID),    intent(in):: e(3,np)      ! input position unit vector
 real(kind=R_GRID), intent(inout):: f(3,np)
! local
 real(f_p):: ap
 integer i

 do i=1,np
    ap = f(1,i)*e(1,i) + f(2,i)*e(2,i) + f(3,i)*e(3,i)
    f(1,i) = f(1,i) - ap*e(1,i)
    f(2,i) = f(2,i) - ap*e(2,i)
    f(3,i) = f(3,i) - ap*e(3,i)
 enddo

 end subroutine project_sphere_v

#ifdef TO_DO_MQ
 subroutine init_mq(phis, gridstruct, npx, npy, is, ie, js, je, ng)
    integer, intent(in):: npx, npy, is, ie, js, je, ng
    real, intent(in):: phis(is-ng:ie+ng, js-ng:je+ng)
    type(fv_grid_type), intent(IN), target :: gridstruct

! local:
    real zs(is-ng:ie+ng, js-ng:je+ng)
    real zb(is-ng:ie+ng, js-ng:je+ng)
    real pdx(3,is:ie,js:je+1)
    real pdy(3,is:ie+1,js:je)
    integer i, j, n

    real, pointer :: rarea(:,:)
    real, pointer, dimension(:,:) :: dx, dy
    real, pointer, dimension(:,:,:) :: en1, en2, agrid, vlon, vlat

    rarea => gridstruct%rarea
    dx    => gridstruct%dx
    dy    => gridstruct%dy
    en1   => gridstruct%en1
    en2   => gridstruct%en2
    agrid => gridstruct%agrid
    vlon  => gridstruct%vlon
    vlat  => gridstruct%vlat

!   do j=js,je
!      do i=is,ie
    do j=js-ng,je+ng
       do i=is-ng,ie+ng
          zs(i,j) = phis(i,j) / grav
       enddo
    enddo
!   call mpp_update_domains( zs, domain )

!   call a2b_ord2(zs, zb, gridstruct, npx, npy, is, ie, js, je, ng)
    call a2b_ord4(zs, zb, gridstruct, npx, npy, is, ie, js, je, ng)

    do j=js,je+1
       do i=is,ie
          do n=1,3
             pdx(n,i,j) = 0.5*(zb(i,j)+zb(i+1,j))*dx(i,j)*en1(n,i,j)
          enddo
       enddo
    enddo
    do j=js,je
       do i=is,ie+1
          do n=1,3
             pdy(n,i,j) = 0.5*(zb(i,j)+zb(i,j+1))*dy(i,j)*en2(n,i,j)
          enddo
       enddo
    enddo

! Compute "volume-mean" gradient by Green's theorem
    do j=js,je
       do i=is,ie
          idiag%zxg(i,j) = vlon(i,j,1)*(pdx(1,i,j+1)-pdx(1,i,j)-pdy(1,i,j)+pdy(1,i+1,j))  &
                         + vlon(i,j,2)*(pdx(2,i,j+1)-pdx(2,i,j)-pdy(2,i,j)+pdy(2,i+1,j))  &
                         + vlon(i,j,3)*(pdx(3,i,j+1)-pdx(3,i,j)-pdy(3,i,j)+pdy(3,i+1,j))
! dF/d(lamda) = radius*cos(agrid(i,j,2)) * dF/dx, F is a scalar
!                                                       ________________________
          idiag%zxg(i,j) =  idiag%zxg(i,j)*rarea(i,j) * radius*cos(agrid(i,j,2))
!                                                       ^^^^^^^^^^^^^^^^^^^^^^^^
       enddo
    enddo

 end subroutine init_mq
#endif

 end module fv_grid_utils_mod
