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

 module fv_surf_map_mod

! <table>
! <tr>
!     <th>Module Name</th>
!     <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>constants_mod</td>
!     <td>grav, radius, pi=>pi_8</td>
!   </tr>
!   <tr>
!     <td>fms_mod</td>
!     <td>file_exist, check_nml_error,open_namelist_file, close_file,
!         stdlog, mpp_pe, mpp_root_pe, FATAL, error_mesg</td>
!   </tr>
!   <tr>
!     <td>fv_arrays_mod</td>
!     <td>fv_grid_bounds_type, R_GRID</td>
!   </tr>
!   <tr>
!     <td>fv_grid_utils_mod</td>
!     <td>great_circle_dist, latlon2xyz, v_prod, normalize_vect,
!         g_sum, global_mx, vect_cross</td>
!   </tr>
!   <tr>
!     <td>fv_mp_mod</td>
!     <td>ng,mp_stop, mp_reduce_min, mp_reduce_max, is_master</td>
!   </tr>
!   <tr>
!     <td>fv_timing_mod</td>
!     <td>timing_on, timing_off</td>
!   </tr>
!   <tr>
!     <td>mpp_mod</td>
!     <td>get_unit, input_nml_file, mpp_error,
!         mpp_pe, mpp_chksum, stdout</td>
!   </tr>
!   <tr>
!     <td>mpp_domains_mod</td>
!     <td>mpp_update_domains, domain2d</td>
!   </tr>
! </table>

      use fms_mod,           only: file_exist, check_nml_error,            &
                                   open_namelist_file, close_file, stdlog, &
                                   mpp_pe, mpp_root_pe, FATAL, error_mesg
      use mpp_mod,           only: get_unit, input_nml_file, mpp_error
      use mpp_domains_mod,   only: mpp_update_domains, domain2d
      use constants_mod,     only: grav, radius, pi=>pi_8

      use fv_grid_utils_mod, only: great_circle_dist, latlon2xyz, v_prod, normalize_vect
      use fv_grid_utils_mod, only: g_sum, global_mx, vect_cross
      use fv_mp_mod,         only: ng
      use fv_mp_mod,         only: mp_stop, mp_reduce_min, mp_reduce_max, is_master
      use fv_timing_mod,     only: timing_on, timing_off
      use fv_arrays_mod,     only: fv_grid_bounds_type, R_GRID

      implicit none

      private
!-----------------------------------------------------------------------
! NAMELIST
!    Name, resolution, and format of XXmin USGS datafile
!      1min            ---------> 1.85 km
!         nlon = 10800 * 2
!         nlat =  5400 * 2
!      2min            ---------> 3.7 km
!         nlon = 10800
!         nlat =  5400
!      5min
!         nlon = 4320
!         nlat = 2160
!    surf_format:      netcdf (default)
!                      binary
! New NASA SRTM30 data: SRTM30.nc
!         nlon = 43200
!         nlat = 21600
      logical::  zs_filter = .true. 
      logical:: zero_ocean = .true.          ! if true, no diffusive flux into water/ocean area 
      integer           ::  nlon = 21600
      integer           ::  nlat = 10800
      real:: cd4 = 0.15      !< Dimensionless coeff for del-4 diffusion (with FCT)
      real:: cd2 = -1.       !< Dimensionless coeff for del-2 diffusion (-1 gives resolution-determined value)
      real:: peak_fac = 1.05 !< overshoot factor for the mountain peak
      real:: max_slope = 0.15 !< max allowable terrain slope: 1 --> 45 deg
                              !! 0.15 for C768 or lower; 0.25 C1536; 0.3 for C3072
      integer:: n_del2_weak = 12
      integer:: n_del2_strong = -1
      integer:: n_del4 = -1
      

      character(len=128)::  surf_file = "INPUT/topo1min.nc"
      character(len=6)  ::  surf_format = 'netcdf'
      logical :: namelist_read = .false.

      real(kind=R_GRID) da_min 
      real cos_grid
      character(len=3) :: grid_string = ''

      namelist /surf_map_nml/ surf_file,surf_format,nlon,nlat, zero_ocean, zs_filter, &
                    cd4, peak_fac, max_slope, n_del2_weak, n_del2_strong, cd2, n_del4
!
      real, allocatable:: zs_g(:,:), sgh_g(:,:), oro_g(:,:)

      public  sgh_g, oro_g, zs_g
      public  surfdrv
      public  del2_cubed_sphere, del4_cubed_sphere, FV3_zs_filter

      contains

      subroutine surfdrv(npx, npy, grid, agrid, area, dx, dy, dxa, dya, dxc, dyc, sin_sg, phis, &
                         stretch_fac, nested, npx_global, domain,grid_number, bd)

      implicit         none
#include <netcdf.inc>
      integer, intent(in):: npx, npy

    ! INPUT arrays
      type(fv_grid_bounds_type), intent(IN) :: bd
      real(kind=R_GRID), intent(in)::area(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng)
      real, intent(in):: dx(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng+1)
      real, intent(in):: dy(bd%is-ng:bd%ie+ng+1, bd%js-ng:bd%je+ng)
      real, intent(in), dimension(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng)::dxa, dya
      real, intent(in)::dxc(bd%is-ng:bd%ie+ng+1, bd%js-ng:bd%je+ng)
      real, intent(in)::dyc(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng+1)

      real(kind=R_GRID), intent(in):: grid(bd%is-ng:bd%ie+ng+1, bd%js-ng:bd%je+ng+1,2)
      real(kind=R_GRID), intent(in):: agrid(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng,2)
      real, intent(IN):: sin_sg(bd%isd:bd%ied,bd%jsd:bd%jed,9)
      real(kind=R_GRID), intent(IN):: stretch_fac
      logical, intent(IN) :: nested
      integer, intent(IN) :: npx_global
      type(domain2d), intent(INOUT) :: domain
      integer, intent(IN) :: grid_number

    ! OUTPUT arrays
      real, intent(out):: phis(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng)
! Local:
      real, allocatable :: z2(:,:)
! Position of edges of the box containing the original data point:
      integer          londim
      integer          latdim
      character(len=80) :: topoflnm
      real(kind=4), allocatable ::  ft(:,:), zs(:,:)
      real, allocatable :: lon1(:),  lat1(:)
      real dx1, dx2, dy1, dy2, lats, latn, r2d
      real(kind=R_GRID) da_max
      real zmean, z2mean, delg, rgrav
!     real z_sp, f_sp, z_np, f_np
      integer i, j, n, mdim
      integer igh, jt
      integer ncid, lonid, latid, ftopoid, htopoid
      integer jstart, jend, start(4), nread(4)
      integer status

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed
      real phis_coarse(bd%isd:bd%ied, bd%jsd:bd%jed)
      real wt

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed
      if (nested) then
      !Divide all by grav
         rgrav = 1./grav
         do j=jsd,jed
            do i=isd,ied
               phis(i,j) = phis(i,j)*rgrav
            enddo
         enddo
         !Save interpolated coarse-grid data for blending
         do j=jsd,jed
            do i=isd,ied
               phis_coarse(i,j) = phis(i,j)
            enddo
         enddo
      endif

      do j=js,je
      do i=is,ie
         phis(i,j) = 0.0
      enddo
      enddo

      call read_namelist

      if (grid_number > 1) write(grid_string, '(A, I1)') ' g', grid_number


!
! surface file must be in NetCDF format
!
      if ( file_exist(surf_file) ) then 
         if (surf_format == "netcdf") then

          status = nf_open (surf_file, NF_NOWRITE, ncid)
          if (status .ne. NF_NOERR) call handle_err(status)
  
          status = nf_inq_dimid (ncid, 'lon', lonid)
          if (status .ne. NF_NOERR) call handle_err(status)
          status = nf_inq_dimlen (ncid, lonid, londim)
          if (status .ne. NF_NOERR) call handle_err(status)
          nlon = londim

          status = nf_inq_dimid (ncid, 'lat', latid)
          if (status .ne. NF_NOERR) call handle_err(status)
          status = nf_inq_dimlen (ncid, latid, latdim)
          if (status .ne. NF_NOERR) call handle_err(status)
          nlat = latdim

          if ( is_master() ) then
              if ( nlon==43200 ) then
                write(*,*) 'Opening NASA datset file:', surf_file, surf_format, nlon, nlat
              else
                write(*,*) 'Opening USGS datset file:', surf_file, surf_format, nlon, nlat
              endif
          endif
  
       else
          call error_mesg ( 'surfdrv','Raw IEEE data format no longer supported !!!', FATAL )
       endif
    else
       call error_mesg ( 'surfdrv','surface file '//trim(surf_file)//' not found !', FATAL )
    endif

      allocate ( lat1(nlat+1) )
      allocate ( lon1(nlon+1) )

      r2d = 180./pi

      cos_grid = cos( 2.*pi/real(nlat) )   ! two-data_grid distance

      dx1 = 2.*pi/real(nlon)
      dy1 = pi/real(nlat)

      do i=1,nlon+1
         lon1(i) = dx1 * real(i-1)    ! between 0 2pi
      enddo

         lat1(1) = - 0.5*pi
         lat1(nlat+1) =  0.5*pi
      do j=2,nlat
         lat1(j) = -0.5*pi + dy1*(j-1)
      enddo

!-------------------------------------
! Compute raw phis and oro
!-------------------------------------
      call timing_on('map_to_cubed')

      if (surf_format == "netcdf") then

!  Find latitude strips reading data
         lats =  pi/2.
         latn = -pi/2.
         do j=js,je
            do i=is,ie
               lats = min( lats, grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
               latn = max( latn, grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
            enddo
         enddo

! Enlarge the search zone:
! To account for the curvature of the coordinates:

         !I have had trouble running c90 with 600 pes unless the search region is expanded
         ! due to failures in finding latlon points in the source data.
         !This sets a larger search region if the number of cells on a PE is too small.
         !(Alternately you can just cold start the topography using a smaller number of PEs)
         if (min(je-js+1,ie-is+1) < 15) then
            delg = max( 0.4*(latn-lats), pi/real(npx_global-1), 2.*pi/real(nlat) )
         else
            delg = max( 0.2*(latn-lats), pi/real(npx_global-1), 2.*pi/real(nlat) )
         endif
         lats = max( -0.5*pi, lats - delg )
         latn = min(  0.5*pi, latn + delg )

         jstart = 1
         do j=2,nlat
            if ( lats < lat1(j) ) then
                 jstart = j-1
                 exit
            endif
         enddo
         jstart = max(jstart-1, 1)

         jend = nlat
         do j=2,nlat
            if ( latn < lat1(j+1) ) then
                 jend = j+1
                 exit
            endif
         enddo
         jend = min(jend+1, nlat)

         jt = jend - jstart + 1
         igh = nlon/8 + nlon/(2*(npx_global-1))

         if (is_master()) write(*,*) 'Terrain dataset =', nlon, 'jt=', jt
         if (is_master()) write(*,*) 'igh (terrain ghosting)=', igh

         status = nf_inq_varid (ncid, 'ftopo', ftopoid)
         if (status .ne. NF_NOERR) call handle_err(status)
         nread = 1;   start = 1
         nread(1) = nlon
         start(2) = jstart; nread(2) = jend - jstart + 1

         allocate ( ft(-igh:nlon+igh,jt) )
         status = nf_get_vara_real (ncid, ftopoid, start, nread, ft(1:nlon,1:jt))
         if (status .ne. NF_NOERR) call handle_err(status)

         do j=1,jt
            do i=-igh,0
               ft(i,j) = ft(i+nlon,j)
            enddo
            do i=nlon+1,nlon+igh
               ft(i,j) = ft(i-nlon,j)
            enddo
         enddo

         status = nf_inq_varid (ncid, 'htopo', htopoid)
         if (status .ne. NF_NOERR) call handle_err(status)
         allocate ( zs(-igh:nlon+igh,jt) )
         status = nf_get_vara_real (ncid, htopoid, start, nread, zs(1:nlon,1:jt))
         if (status .ne. NF_NOERR) call handle_err(status)
         status = nf_close (ncid)
         if (status .ne. NF_NOERR) call handle_err(status)
! Ghost Data
         do j=1,jt
            do i=-igh,0
               zs(i,j) = zs(i+nlon,j)
            enddo
            do i=nlon+1,nlon+igh
               zs(i,j) = zs(i-nlon,j)
            enddo
         enddo

      endif

! special SP treatment:
!     if ( jstart == 1 ) then
!          call zonal_mean(nlon, zs(1,1), z_sp)
!          call zonal_mean(nlon, ft(1,1), f_sp)
!     endif

      allocate ( oro_g(isd:ied, jsd:jed) )
      allocate ( sgh_g(isd:ied, jsd:jed) )
                                                     call timing_on('map_to_cubed')
      call map_to_cubed_raw(igh, nlon, jt, lat1(jstart:jend+1), lon1, zs, ft, grid, agrid,  &
                            phis, oro_g, sgh_g, npx, npy, jstart, jend, stretch_fac, nested, npx_global, bd)
      if (is_master()) write(*,*) 'map_to_cubed_raw: master PE done'
                                                     call timing_off('map_to_cubed')

      deallocate ( zs )
      deallocate ( ft )
      deallocate ( lon1 )
      deallocate ( lat1 )

      allocate (  zs_g(is:ie, js:je) )
      allocate ( z2(is:ie,js:je) )
      do j=js,je
         do i=is,ie
            zs_g(i,j) = phis(i,j)
            z2(i,j) = phis(i,j)**2
         enddo
      enddo
!--------
! Filter:
!--------
      call global_mx(real(phis,kind=R_GRID), ng, da_min, da_max, bd)
      zmean  = g_sum(domain, zs_g(is:ie,js:je), is, ie, js, je, ng, area, 1)
      z2mean = g_sum(domain, z2(is:ie,js:je)  , is, ie, js, je, ng, area, 1)
      if ( is_master() ) then
           write(*,*) 'Before filter ZS', trim(grid_string), ' min=', da_min, ' Max=', da_max,'    Mean=',zmean
           write(*,*) '*** Mean variance', trim(grid_string), ' *** =', z2mean
      endif

      !On a nested grid blend coarse-grid and nested-grid
      ! orography near boundary.
      ! This works only on the height of topography; assume
      ! land fraction and sub-grid variance unchanged

      ! Here, we blend in the four cells nearest to the boundary.
      ! In the halo we set the value to that interpolated from the coarse
      !  grid. (Previously this was erroneously not being done, which was causing
      !  the halo to be filled with unfiltered nested-grid terrain, creating
      !  ugly edge artifacts.)
      if (nested) then
         if (is_master()) write(*,*) 'Blending nested and coarse grid topography'
         do j=jsd,jed
         do i=isd,ied
            wt = max(0.,min(1.,real(5 - min(i,j,npx-i,npy-j,5))/5. ))
            phis(i,j) = (1.-wt)*phis(i,j) + wt*phis_coarse(i,j)

         enddo
         enddo
      endif

      call global_mx(real(oro_g,kind=R_GRID), ng, da_min, da_max, bd)
      if ( is_master() ) write(*,*) 'ORO', trim(grid_string), ' min=', da_min, ' Max=', da_max

      call global_mx(area, ng, da_min, da_max, bd)
                                                    call timing_on('Terrain_filter')
! Del-2: high resolution only
      if ( zs_filter ) then
         if(is_master()) then
            write(*,*) 'Applying terrain filters. zero_ocean is', zero_ocean
         endif
         call FV3_zs_filter (bd, isd, ied, jsd, jed, npx, npy, npx_global,  &
                             stretch_fac, nested, domain, area, dxa, dya, dx, dy, dxc, dyc, grid,  &
                             agrid, sin_sg, phis, oro_g)
         call mpp_update_domains(phis, domain)
      endif          ! end terrain filter
                                                    call timing_off('Terrain_filter')

      do j=js,je
         do i=is,ie
            z2(i,j) = phis(i,j)**2
         end do
      end do

      call global_mx(real(phis,kind=R_GRID), ng, da_min, da_max, bd)
      zmean  = g_sum(domain, phis(is:ie,js:je), is, ie, js, je, ng, area, 1)
      z2mean = g_sum(domain, z2,                is, ie, js, je, ng, area, 1)
      deallocate ( z2 )

      if ( is_master() ) then
           write(*,*) 'After  filter Phis', trim(grid_string), ' min=', da_min, ' Max=', da_max, 'Mean=', zmean
           write(*,*) '*** Mean variance', trim(grid_string), ' *** =', z2mean
      endif

      !FOR NESTING: Unless we fill the outermost halo with topography
      ! interpolated from the coarse grid, the values of phi there
      ! will be wrong because they are just z instead of g*z; they
      ! have not had gravity properly multiplied in so we can get phi

      ! For now we compute phis and sgh_g on the full data domain; for
      ! nested grids this allows us to do the smoothing near the boundary
      ! without having to fill the boundary halo from the coarse grid

      !ALSO for nesting: note that we are smoothing the terrain using 
      !  the nested-grid's outer halo filled with the terrain computed
      !  directly from the input file computed here, and then
      !  replacing it with interpolated topography in fv_restart, so
      !  as to be consistent when doing the boundary condition
      !  interpolation. We would ideally replace the nested-grid
      !  halo topography BEFORE smoothing, which could be more
      !  consistent, but this would require moving calls to
      !  nested_grid_BC in this routine.

      do j=jsd,jed
         do i=isd,ied
            phis(i,j) =  grav * phis(i,j)
            if ( sgh_g(i,j) <= 0. ) then
                 sgh_g(i,j) = 0.
            else
                 sgh_g(i,j) = sqrt(sgh_g(i,j))
            endif
         end do
      end do

      call global_mx(real(sgh_g,kind=R_GRID), ng, da_min, da_max, bd)
      if ( is_master() ) write(*,*) 'Before filter SGH', trim(grid_string), ' min=', da_min, ' Max=', da_max


!-----------------------------------------------
! Filter the standard deviation of mean terrain:
!-----------------------------------------------
      call global_mx(area, ng, da_min, da_max, bd)

      if(zs_filter) call del4_cubed_sphere(npx, npy, sgh_g, area, dx, dy, dxc, dyc, sin_sg, 1, zero_ocean, oro_g, nested, domain, bd)

      call global_mx(real(sgh_g,kind=R_GRID), ng, da_min, da_max, bd)
      if ( is_master() ) write(*,*) 'After  filter SGH', trim(grid_string), ' min=', da_min, ' Max=', da_max
      do j=js,je
         do i=is,ie
            sgh_g(i,j) = max(0., sgh_g(i,j))
         enddo
      enddo

 end subroutine surfdrv

 subroutine FV3_zs_filter (bd, isd, ied, jsd, jed, npx, npy, npx_global,  &
                           stretch_fac, nested, domain, area, dxa, dya, dx, dy, dxc, dyc, grid,  &
                            agrid, sin_sg,  phis, oro )
      integer, intent(in):: isd, ied, jsd, jed, npx, npy, npx_global
      type(fv_grid_bounds_type), intent(IN) :: bd
      real(kind=R_GRID), intent(in), dimension(isd:ied,jsd:jed)::area
      real, intent(in), dimension(isd:ied,jsd:jed)::dxa, dya
      real, intent(in), dimension(isd:ied,  jsd:jed+1):: dx, dyc
      real, intent(in), dimension(isd:ied+1,jsd:jed):: dy, dxc

      real(kind=R_GRID), intent(in)::  grid(isd:ied+1, jsd:jed+1,2)
      real(kind=R_GRID), intent(in):: agrid(isd:ied,   jsd:jed,  2)
      real, intent(IN):: sin_sg(isd:ied,jsd:jed,9)
      real(kind=R_GRID), intent(IN):: stretch_fac
      logical, intent(IN) :: nested
      real, intent(inout):: phis(isd:ied,jsd,jed)
      real, intent(inout):: oro(isd:ied,jsd,jed)
      type(domain2d), intent(INOUT) :: domain
      integer mdim
      real(kind=R_GRID) da_max

      if (is_master()) print*, ' Calling FV3_zs_filter...'

      if (.not. namelist_read) call read_namelist !when calling from external_ic
      call global_mx(area, ng, da_min, da_max, bd)

      mdim = nint( real(npx_global) * min(10., stretch_fac) )

! Del-2: high resolution only
! call del2_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, sin_sg, n_del2, cd2, zero_ocean, oro, nested, domain, bd)
      if (n_del2_strong < 0) then
         if ( npx_global<=97) then
              n_del2_strong = 0
         elseif ( npx_global<=193 ) then
              n_del2_strong = 1
         else
              n_del2_strong = 2
         endif
      endif
      if (cd2 < 0.) cd2 = 0.16*da_min
! Applying strong 2-delta-filter:
      if ( n_del2_strong > 0 )   &
           call two_delta_filter(npx, npy, phis, area, dx, dy, dxa, dya, dxc, dyc, sin_sg, cd2, zero_ocean,  &
                                 .true., 0, oro, nested, domain, bd, n_del2_strong)

! MFCT Del-4:
      if (n_del4 < 0) then
         if ( mdim<=193 ) then
              n_del4 = 1
         elseif ( mdim<=1537 ) then
              n_del4 = 2
         else
              n_del4 = 3
         endif
      endif
      call del4_cubed_sphere(npx, npy, phis, area, dx, dy, dxc, dyc, sin_sg, n_del4, zero_ocean, oro, nested, domain, bd)
! Applying weak 2-delta-filter:
      cd2 = 0.12*da_min
      call two_delta_filter(npx, npy, phis, area, dx, dy, dxa, dya, dxc, dyc, sin_sg, cd2, zero_ocean,  &
                               .true., 1, oro, nested, domain, bd, n_del2_weak)


 end subroutine FV3_zs_filter


 subroutine two_delta_filter(npx, npy, q, area, dx, dy, dxa, dya, dxc, dyc, sin_sg, cd, zero_ocean,  &
                            check_slope, filter_type, oro, nested, domain, bd, ntmax)
   type(fv_grid_bounds_type), intent(IN) :: bd
   integer, intent(in):: npx, npy
   integer, intent(in):: ntmax
   integer, intent(in):: filter_type    !< 0: strong,   1: weak
   real, intent(in):: cd
! INPUT arrays
   real(kind=R_GRID), intent(in)::area(bd%isd:bd%ied,  bd%jsd:bd%jed)
   real, intent(in)::  dx(bd%isd:bd%ied,  bd%jsd:bd%jed+1)
   real, intent(in)::  dy(bd%isd:bd%ied+1,bd%jsd:bd%jed)
   real, intent(in):: dxa(bd%isd:bd%ied,  bd%jsd:bd%jed)
   real, intent(in):: dya(bd%isd:bd%ied,  bd%jsd:bd%jed)
   real, intent(in):: dxc(bd%isd:bd%ied+1,bd%jsd:bd%jed)
   real, intent(in):: dyc(bd%isd:bd%ied,  bd%jsd:bd%jed+1)
   real, intent(in):: sin_sg(bd%isd:bd%ied,bd%jsd:bd%jed,9)
   real, intent(in):: oro(bd%isd:bd%ied,  bd%jsd:bd%jed)        !< 0==water, 1==land
   logical, intent(in):: zero_ocean, check_slope
   logical, intent(in):: nested
   type(domain2d), intent(inout) :: domain
! OUTPUT arrays
   real, intent(inout):: q(bd%isd:bd%ied, bd%jsd:bd%jed)
! Local:
   real, parameter:: p1 =  7./12.
   real, parameter:: p2 = -1./12.
   real, parameter:: c1 = -2./14.
   real, parameter:: c2 = 11./14.
   real, parameter:: c3 =  5./14.
   real:: ddx(bd%is:bd%ie+1,bd%js:bd%je), ddy(bd%is:bd%ie,bd%js:bd%je+1)
   logical:: extm(bd%is-1:bd%ie+1)
   logical:: ext2(bd%is:bd%ie,bd%js-1:bd%je+1)
   real::  a1(bd%is-1:bd%ie+2)
   real::  a2(bd%is:bd%ie,bd%js-1:bd%je+2)
   real(kind=R_GRID):: a3(bd%is:bd%ie,bd%js:bd%je)
   real(kind=R_GRID):: smax, smin
   real:: m_slope, fac
   integer:: i,j, nt
   integer:: is,  ie,  js,  je
   integer:: isd, ied, jsd, jed
   integer:: is1, ie2, js1, je2

   is  = bd%is
   ie  = bd%ie
   js  = bd%js
   je  = bd%je
   isd = bd%isd
   ied = bd%ied
   jsd = bd%jsd
   jed = bd%jed

   if ( nested ) then
        is1 = is-1;         ie2 = ie+2
        js1 = js-1;         je2 = je+2
   else
        is1 = max(3,is-1);  ie2 = min(npx-2,ie+2)
        js1 = max(3,js-1);  je2 = min(npy-2,je+2)
   end if

   if ( check_slope ) then
        m_slope = max_slope
   else
        m_slope = 10.
   endif
         

   do 777 nt=1, ntmax
    call mpp_update_domains(q, domain)

! Check slope
    if ( nt==1 .and. check_slope ) then
         do j=js,je
            do i=is,ie+1
               ddx(i,j) = (q(i,j) - q(i-1,j))/dxc(i,j) 
               ddx(i,j) = abs(ddx(i,j))
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               ddy(i,j) = (q(i,j) - q(i,j-1))/dyc(i,j) 
               ddy(i,j) = abs(ddy(i,j))
            enddo
         enddo
         do j=js,je
            do i=is,ie
               a3(i,j) = max( ddx(i,j), ddx(i+1,j), ddy(i,j), ddy(i,j+1) )
            enddo
         enddo
         call global_mx(a3, 0, smin, smax, bd)
         if ( is_master() ) write(*,*) 'Before filter: Max_slope=', smax
    endif

! First step: average the corners:
  if ( .not. nested .and. nt==1 ) then
    if ( is==1 .and. js==1 ) then
         q(1,1) = (q(1,1)*area(1,1)+q(0,1)*area(0,1)+q(1,0)*area(1,0))  &
                / (       area(1,1)+       area(0,1)+       area(1,0) )
         q(0,1) =  q(1,1)
         q(1,0) =  q(1,1)
    endif
    if ( (ie+1)==npx .and. js==1 ) then
         q(ie, 1) = (q(ie,1)*area(ie,1)+q(npx,1)*area(npx,1)+q(ie,0)*area(ie,0)) &
                  / (        area(ie,1)+         area(npx,1)+        area(ie,0))
         q(npx,1) =  q(ie,1)
         q(ie, 0) =  q(ie,1)
    endif
    if ( is==1 .and. (je+1)==npy ) then
         q(1, je) = (q(1,je)*area(1,je)+q(0,je)*area(0,je)+q(1,npy)*area(1,npy))   &
                  / (        area(1,je)+        area(0,je)+         area(1,npy))
         q(0, je) =  q(1,je)
         q(1,npy) =  q(1,je)
    endif
    if ( (ie+1)==npx .and. (je+1)==npy ) then
         q(ie, je) = (q(ie,je)*area(ie,je)+q(npx,je)*area(npx,je)+q(ie,npy)*area(ie,npy))  &
                   / (         area(ie,je)+          area(npx,je)+          area(ie,npy))
         q(npx,je) =  q(ie,je)
         q(ie,npy) =  q(ie,je)
    endif
    call mpp_update_domains(q, domain)
  endif

! x-diffusive flux:
 do 333 j=js,je

    do i=is1, ie2
       a1(i) = p1*(q(i-1,j)+q(i,j)) + p2*(q(i-2,j)+q(i+1,j))
    enddo

    if ( .not. nested ) then
      if ( is==1 ) then
        a1(0) = c1*q(-2,j) + c2*q(-1,j) + c3*q(0,j)
        a1(1) = 0.5*(((2.*dxa(0,j)+dxa(-1,j))*q(0,j)-dxa(0,j)*q(-1,j))/(dxa(-1,j)+dxa(0,j)) &
              +      ((2.*dxa(1,j)+dxa( 2,j))*q(1,j)-dxa(1,j)*q( 2,j))/(dxa(1, j)+dxa(2,j)))
        a1(2) = c3*q(1,j) + c2*q(2,j) +c1*q(3,j)
      endif
      if ( (ie+1)==npx ) then
        a1(npx-1) = c1*q(npx-3,j) + c2*q(npx-2,j) + c3*q(npx-1,j)
        a1(npx) = 0.5*(((2.*dxa(npx-1,j)+dxa(npx-2,j))*q(npx-1,j)-dxa(npx-1,j)*q(npx-2,j))/(dxa(npx-2,j)+dxa(npx-1,j)) &
                +      ((2.*dxa(npx,  j)+dxa(npx+1,j))*q(npx,  j)-dxa(npx,  j)*q(npx+1,j))/(dxa(npx,  j)+dxa(npx+1,j)))
        a1(npx+1) = c3*q(npx,j) + c2*q(npx+1,j) + c1*q(npx+2,j)
      endif
    endif

    if ( filter_type == 0 ) then
       do i=is-1, ie+1
          if( abs(3.*(a1(i)+a1(i+1)-2.*q(i,j))) > abs(a1(i)-a1(i+1)) ) then
              extm(i) = .true.
          else
              extm(i) = .false.
          endif
       enddo
    else
       do i=is-1, ie+1
          if ( (a1(i)-q(i,j))*(a1(i+1)-q(i,j)) > 0. ) then
               extm(i) = .true.
          else
               extm(i) = .false.
          endif
       enddo
    endif

    do i=is,ie+1
            ddx(i,j) = (q(i-1,j)-q(i,j))/dxc(i,j)
       if ( extm(i-1).and.extm(i) ) then
            ddx(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*ddx(i,j)
       elseif ( abs(ddx(i,j)) > m_slope ) then
            fac = min(1., max(0.1,(abs(ddx(i,j))-m_slope)/m_slope ) )
            ddx(i,j) = fac*0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*ddx(i,j)
       else
            ddx(i,j) = 0.
       endif
   enddo
333   continue

! y-diffusive flux:
   do j=js1,je2
      do i=is,ie
         a2(i,j) = p1*(q(i,j-1)+q(i,j)) + p2*(q(i,j-2)+q(i,j+1))
      enddo
   enddo
   if ( .not. nested ) then
      if( js==1 ) then
        do i=is,ie
           a2(i,0) = c1*q(i,-2) + c2*q(i,-1) + c3*q(i,0)
           a2(i,1) = 0.5*(((2.*dya(i,0)+dya(i,-1))*q(i,0)-dya(i,0)*q(i,-1))/(dya(i,-1)+dya(i,0))   &
                   +      ((2.*dya(i,1)+dya(i, 2))*q(i,1)-dya(i,1)*q(i, 2))/(dya(i, 1)+dya(i,2)))
           a2(i,2) = c3*q(i,1) + c2*q(i,2) + c1*q(i,3)
        enddo
      endif
      if( (je+1)==npy ) then
        do i=is,ie
           a2(i,npy-1) = c1*q(i,npy-3) + c2*q(i,npy-2) + c3*q(i,npy-1)
           a2(i,npy) = 0.5*(((2.*dya(i,npy-1)+dya(i,npy-2))*q(i,npy-1)-dya(i,npy-1)*q(i,npy-2))/(dya(i,npy-2)+dya(i,npy-1))  &
                     +      ((2.*dya(i,npy)+dya(i,npy+1))*q(i,npy)-dya(i,npy)*q(i,npy+1))/(dya(i,npy)+dya(i,npy+1)))
           a2(i,npy+1) = c3*q(i,npy) + c2*q(i,npy+1) + c1*q(i,npy+2)
        enddo
      endif
   endif

   if ( filter_type == 0 ) then
     do j=js-1,je+1
        do i=is,ie
           if( abs(3.*(a2(i,j)+a2(i,j+1)-2.*q(i,j))) > abs(a2(i,j)-a2(i,j+1)) ) then
               ext2(i,j) = .true.
           else
               ext2(i,j) = .false.
           endif
        enddo
     enddo
   else
     do j=js-1,je+1
        do i=is,ie
           if ( (a2(i,j)-q(i,j))*(a2(i,j+1)-q(i,j)) > 0. ) then
                ext2(i,j) = .true.
           else
                ext2(i,j) = .false.
           endif
        enddo
     enddo
   endif

   do j=js,je+1
      do i=is,ie
         ddy(i,j) = (q(i,j-1)-q(i,j))/dyc(i,j)
         if ( ext2(i,j-1) .and. ext2(i,j) ) then
              ddy(i,j) = 0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))*dx(i,j)*ddy(i,j)
         elseif ( abs(ddy(i,j))>m_slope ) then
              fac = min(1., max(0.1,(abs(ddy(i,j))-m_slope)/m_slope))
              ddy(i,j) = fac*0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))*dx(i,j)*ddy(i,j)
         else
              ddy(i,j) = 0.
         endif
      enddo
   enddo

    if ( zero_ocean ) then
! Limit diffusive flux over water cells:
         do j=js,je
            do i=is,ie+1
               ddx(i,j) = max(0., min(oro(i-1,j), oro(i,j))) * ddx(i,j)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               ddy(i,j) = max(0., min(oro(i,j-1), oro(i,j))) * ddy(i,j)
            enddo
         enddo
    endif

    do j=js,je
       do i=is,ie
          q(i,j) = q(i,j) + cd/area(i,j)*(ddx(i,j)-ddx(i+1,j)+ddy(i,j)-ddy(i,j+1))
       enddo
    enddo
777 continue

! Check slope
    if ( check_slope ) then
         call mpp_update_domains(q, domain)
         do j=js,je
            do i=is,ie+1
               ddx(i,j) = (q(i,j) - q(i-1,j))/dxc(i,j) 
               ddx(i,j) = abs(ddx(i,j))
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               ddy(i,j) = (q(i,j) - q(i,j-1))/dyc(i,j) 
               ddy(i,j) = abs(ddy(i,j))
            enddo
         enddo
         do j=js,je
            do i=is,ie
               a3(i,j) = max( ddx(i,j), ddx(i+1,j), ddy(i,j), ddy(i,j+1) )
            enddo
         enddo
         call global_mx(a3, 0, smin, smax, bd)
         if ( is_master() ) write(*,*) 'After filter: Max_slope=', smax
    endif

 end subroutine two_delta_filter



 subroutine del2_cubed_sphere(npx, npy, q, area, dx, dy, dxc, dyc, sin_sg, nmax, cd, zero_ocean, oro, nested, domain, bd)
      type(fv_grid_bounds_type), intent(IN) :: bd
      integer, intent(in):: npx, npy
      integer, intent(in):: nmax
      real(kind=R_GRID), intent(in):: cd
      logical, intent(in):: zero_ocean
    ! INPUT arrays
      real(kind=R_GRID), intent(in)::area(bd%isd:bd%ied,  bd%jsd:bd%jed)
      real, intent(in)::  dx(bd%isd:bd%ied,  bd%jsd:bd%jed+1)
      real, intent(in)::  dy(bd%isd:bd%ied+1,bd%jsd:bd%jed)
      real, intent(in):: dxc(bd%isd:bd%ied+1,bd%jsd:bd%jed)
      real, intent(in):: dyc(bd%isd:bd%ied,  bd%jsd:bd%jed+1)
      real, intent(IN):: sin_sg(bd%isd:bd%ied,bd%jsd:bd%jed,9)
      real, intent(in):: oro(bd%isd:bd%ied,  bd%jsd:bd%jed)        !< 0==water, 1==land
      logical, intent(IN) :: nested
      type(domain2d), intent(INOUT) :: domain
    ! OUTPUT arrays
      real, intent(inout):: q(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng)
! Local:
      real ddx(bd%is:bd%ie+1,bd%js:bd%je), ddy(bd%is:bd%ie,bd%js:bd%je+1)
      integer i,j,n

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


      call mpp_update_domains(q,domain,whalo=ng,ehalo=ng,shalo=ng,nhalo=ng)

! First step: average the corners:
      if ( is==1 .and. js==1  .and. .not. nested) then
           q(1,1) = (q(1,1)*area(1,1)+q(0,1)*area(0,1)+q(1,0)*area(1,0))  &
                  / (       area(1,1)+       area(0,1)+       area(1,0) )
           q(0,1) =  q(1,1)
           q(1,0) =  q(1,1)
      endif
      if ( (ie+1)==npx .and. js==1  .and. .not. nested) then
           q(ie, 1) = (q(ie,1)*area(ie,1)+q(npx,1)*area(npx,1)+q(ie,0)*area(ie,0)) &
                    / (        area(ie,1)+         area(npx,1)+        area(ie,0))
           q(npx,1) =  q(ie,1)
           q(ie, 0) =  q(ie,1)
      endif
      if ( (ie+1)==npx .and. (je+1)==npy .and. .not. nested ) then
           q(ie, je) = (q(ie,je)*area(ie,je)+q(npx,je)*area(npx,je)+q(ie,npy)*area(ie,npy))  &
                     / (         area(ie,je)+          area(npx,je)+          area(ie,npy))
           q(npx,je) =  q(ie,je)
           q(ie,npy) =  q(ie,je)
      endif
      if ( is==1 .and. (je+1)==npy  .and. .not. nested) then
           q(1, je) = (q(1,je)*area(1,je)+q(0,je)*area(0,je)+q(1,npy)*area(1,npy))   &
                    / (        area(1,je)+        area(0,je)+         area(1,npy))
           q(0, je) =  q(1,je)
           q(1,npy) =  q(1,je)
      endif


      do n=1,nmax
         if( n>1 ) call mpp_update_domains(q,domain,whalo=ng,ehalo=ng,shalo=ng,nhalo=ng)
         do j=js,je
            do i=is,ie+1
               ddx(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*(q(i-1,j)-q(i,j))/dxc(i,j)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               ddy(i,j) = dx(i,j)*(q(i,j-1)-q(i,j))/dyc(i,j) &
                        *0.5*(sin_sg(i,j-1,4)+sin_sg(i,j,2))
            enddo
         enddo

         if ( zero_ocean ) then
! Limit diffusive flux over ater cells:
            do j=js,je
               do i=is,ie+1
                  ddx(i,j) = max(0., min(oro(i-1,j), oro(i,j))) * ddx(i,j)
               enddo
            enddo
            do j=js,je+1
               do i=is,ie
                  ddy(i,j) = max(0., min(oro(i,j-1), oro(i,j))) * ddy(i,j)
               enddo
            enddo
         endif

         do j=js,je
            do i=is,ie
               q(i,j) = q(i,j) + cd/area(i,j)*(ddx(i,j)-ddx(i+1,j)+ddy(i,j)-ddy(i,j+1))
            enddo
         enddo
      enddo

 end subroutine del2_cubed_sphere


 subroutine del4_cubed_sphere(npx, npy, q, area, dx, dy, dxc, dyc, sin_sg, nmax, zero_ocean, oro, nested, domain, bd)
      type(fv_grid_bounds_type), intent(IN) :: bd
      integer, intent(in):: npx, npy, nmax
      logical, intent(in):: zero_ocean
      real, intent(in):: oro(bd%isd:bd%ied,  bd%jsd:bd%jed)        !< 0==water, 1==land
      real(kind=R_GRID), intent(in)::area(bd%isd:bd%ied,  bd%jsd:bd%jed)
      real, intent(in)::  dx(bd%isd:bd%ied,  bd%jsd:bd%jed+1)
      real, intent(in)::  dy(bd%isd:bd%ied+1,bd%jsd:bd%jed)
      real, intent(in):: dxc(bd%isd:bd%ied+1,bd%jsd:bd%jed)
      real, intent(in):: dyc(bd%isd:bd%ied,  bd%jsd:bd%jed+1)
      real, intent(IN):: sin_sg(bd%isd:bd%ied,bd%jsd:bd%jed,9)
      real, intent(inout):: q(bd%is-ng:bd%ie+ng, bd%js-ng:bd%je+ng)
      logical, intent(IN) :: nested
      type(domain2d), intent(INOUT) :: domain
! diffusivity
      real :: diff(bd%is-3:bd%ie+2,bd%js-3:bd%je+2)
! diffusive fluxes: 
      real :: fx1(bd%is:bd%ie+1,bd%js:bd%je), fy1(bd%is:bd%ie,bd%js:bd%je+1)
      real :: fx2(bd%is:bd%ie+1,bd%js:bd%je), fy2(bd%is:bd%ie,bd%js:bd%je+1)
      real :: fx4(bd%is:bd%ie+1,bd%js:bd%je), fy4(bd%is:bd%ie,bd%js:bd%je+1)
      real, dimension(bd%isd:bd%ied,bd%jsd:bd%jed):: d2, win, wou 
      real, dimension(bd%is:bd%ie,bd%js:bd%je):: qlow, qmin, qmax, q0
      real, parameter:: esl = 1.E-20
      integer i,j, n

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

      !On a nested grid the haloes are not filled. Set to zero.
      d2 = 0.
      win = 0.
      wou = 0.

      do j=js-1,je+1
         do i=is-1,ie+1
            diff(i,j) = cd4*area(i,j) ! area dependency is needed for stretched grid
         enddo
     enddo

     do j=js,je
        do i=is,ie
           q0(i,j) = q(i,j)
        enddo
     enddo

  do n=1,nmax
     call mpp_update_domains(q,domain)

! First step: average the corners:
      if ( is==1 .and. js==1 .and. .not. nested) then
           q(1,1) = (q(1,1)*area(1,1)+q(0,1)*area(0,1)+q(1,0)*area(1,0))  &
                  / (       area(1,1)+       area(0,1)+       area(1,0) )
           q(0,1) = q(1,1)
           q(1,0) = q(1,1)
           q(0,0) = q(1,1)
      endif
      if ( (ie+1)==npx .and. js==1  .and. .not. nested) then
           q(ie, 1) = (q(ie,1)*area(ie,1)+q(npx,1)*area(npx,1)+q(ie,0)*area(ie,0)) &
                    / (        area(ie,1)+         area(npx,1)+        area(ie,0))
           q(npx,1) = q(ie,1)
           q(ie, 0) = q(ie,1)
           q(npx,0) = q(ie,1)
      endif
      if ( (ie+1)==npx .and. (je+1)==npy  .and. .not. nested) then
           q(ie, je) = (q(ie,je)*area(ie,je)+q(npx,je)*area(npx,je)+q(ie,npy)*area(ie,npy))  &
                     / (         area(ie,je)+          area(npx,je)+          area(ie,npy))
           q(npx, je) = q(ie,je)
           q(ie, npy) = q(ie,je)
           q(npx,npy) = q(ie,je)
      endif
      if ( is==1 .and. (je+1)==npy  .and. .not. nested) then
           q(1, je) = (q(1,je)*area(1,je)+q(0,je)*area(0,je)+q(1,npy)*area(1,npy))   &
                    / (        area(1,je)+        area(0,je)+         area(1,npy))
           q(0, je) =  q(1,je)
           q(1,npy) =  q(1,je)
           q(0,npy) =  q(1,je)
      endif

     do j=js,je
        do i=is,ie
           qmin(i,j) = min(q0(i,j),          q(i-1,j-1), q(i,j-1), q(i+1,j-1),  &
                                             q(i-1,j  ), q(i,j  ), q(i+1,j  ),  &
                                             q(i-1,j+1), q(i,j+1), q(i+1,j+1) )
           qmax(i,j) = max(peak_fac*q0(i,j), q(i-1,j-1), q(i,j-1), q(i+1,j-1),  &
                                             q(i-1,j  ), q(i,j  ), q(i+1,j  ),  &
                                             q(i-1,j+1), q(i,j+1), q(i+1,j+1) )
        enddo
     enddo

!--------------
! Compute del-2
!--------------
!     call copy_corners(q, npx, npy, 1)
      do j=js,je
         do i=is,ie+1
            fx2(i,j) = 0.25*(diff(i-1,j)+diff(i,j))*dy(i,j)*(q(i-1,j)-q(i,j))/dxc(i,j)          &
                           *(sin_sg(i,j,1)+sin_sg(i-1,j,3))
         enddo
      enddo

!     call copy_corners(q, npx, npy, 2)
      do j=js,je+1
         do i=is,ie
            fy2(i,j) = 0.25*(diff(i,j-1)+diff(i,j))*dx(i,j)*(q(i,j-1)-q(i,j))/dyc(i,j) &
                           *(sin_sg(i,j,2)+sin_sg(i,j-1,4))
         enddo
      enddo

      do j=js,je
         do i=is,ie
            d2(i,j) = (fx2(i,j)-fx2(i+1,j)+fy2(i,j)-fy2(i,j+1)) / area(i,j)
         enddo
      enddo

! qlow == low order monotonic solution
      if ( zero_ocean ) then
! Limit diffusive flux over ater cells:
            do j=js,je
               do i=is,ie+1
                  fx1(i,j) = max(0., min(oro(i-1,j), oro(i,j))) * fx2(i,j)
               enddo
            enddo
            do j=js,je+1
               do i=is,ie
                  fy1(i,j) = max(0., min(oro(i,j-1), oro(i,j))) * fy2(i,j)
               enddo
            enddo
            do j=js,je
               do i=is,ie
                  qlow(i,j) = q(i,j) + (fx1(i,j)-fx1(i+1,j)+fy1(i,j)-fy1(i,j+1)) / area(i,j)
                  d2(i,j) = diff(i,j) * d2(i,j)
               enddo
            enddo
      else
            do j=js,je
               do i=is,ie
                  qlow(i,j) =    q(i,j) + d2(i,j)
                    d2(i,j) = diff(i,j) * d2(i,j)
               enddo
            enddo
      endif

      call mpp_update_domains(d2,domain)

!---------------------
! Compute del4 fluxes:
!---------------------
!     call copy_corners(d2, npx, npy, 1)
      do j=js,je
         do i=is,ie+1
            fx4(i,j) = 0.5*(sin_sg(i-1,j,3)+sin_sg(i,j,1))*dy(i,j)*(d2(i,j)-d2(i-1,j))/dxc(i,j)-fx2(i,j)
         enddo
      enddo

!     call copy_corners(d2, npx, npy, 2)
      do j=js,je+1
         do i=is,ie
            fy4(i,j) = dx(i,j)*(d2(i,j)-d2(i,j-1))/dyc(i,j) &
                     *0.5*(sin_sg(i,j,2)+sin_sg(i,j-1,4))-fy2(i,j)
         enddo
      enddo

!----------------
! Flux limitting:
!----------------
      do j=js,je
         do i=is,ie
            win(i,j) = max(0.,fx4(i,  j)) - min(0.,fx4(i+1,j)) +   &
                       max(0.,fy4(i,  j)) - min(0.,fy4(i,j+1)) + esl
            wou(i,j) = max(0.,fx4(i+1,j)) - min(0.,fx4(i,  j)) +   &
                       max(0.,fy4(i,j+1)) - min(0.,fy4(i,  j)) + esl
            win(i,j) = max(0., qmax(i,j) - qlow(i,j)) / win(i,j)*area(i,j)
            wou(i,j) = max(0., qlow(i,j) - qmin(i,j)) / wou(i,j)*area(i,j)
         enddo
      enddo

      call mpp_update_domains(win,domain, complete=.false.)
      call mpp_update_domains(wou,domain, complete=.true.)

      do j=js,je
         do i=is,ie+1
            if ( fx4(i,j) > 0. ) then
                 fx4(i,j) = min(1., wou(i-1,j), win(i,j)) * fx4(i,j) 
            else
                 fx4(i,j) = min(1., win(i-1,j), wou(i,j)) * fx4(i,j) 
            endif
         enddo
      enddo
      do j=js,je+1
         do i=is,ie
            if ( fy4(i,j) > 0. ) then
                 fy4(i,j) = min(1., wou(i,j-1), win(i,j)) * fy4(i,j) 
            else
                 fy4(i,j) = min(1., win(i,j-1), wou(i,j)) * fy4(i,j) 
            endif
         enddo
      enddo


      if ( zero_ocean ) then
! Limit diffusive flux over ocean cells:
           do j=js,je
              do i=is,ie+1
                 fx4(i,j) = max(0., min(oro(i-1,j), oro(i,j))) * fx4(i,j)
              enddo
           enddo
           do j=js,je+1
              do i=is,ie
                 fy4(i,j) = max(0., min(oro(i,j-1), oro(i,j))) * fy4(i,j)
              enddo
           enddo
      endif

! Update:
      do j=js,je
         do i=is,ie
            q(i,j) = qlow(i,j) + (fx4(i,j)-fx4(i+1,j)+fy4(i,j)-fy4(i,j+1))/area(i,j)
         enddo
      enddo

  enddo    ! end n-loop

 end subroutine del4_cubed_sphere


 subroutine map_to_cubed_raw(igh, im, jt, lat1, lon1, zs, ft,  grid, agrid,  &
                              q2, f2, h2, npx, npy, jstart, jend, stretch_fac, &
                              nested, npx_global, bd)

! Input
      type(fv_grid_bounds_type), intent(IN) :: bd
      integer, intent(in):: igh, im, jt
      integer, intent(in):: npx, npy, npx_global
      real, intent(in):: lat1(jt+1)       !< original southern edge of the cell [-pi/2:pi/2]
      real, intent(in):: lon1(im+1)       !< original western edge of the cell [0:2*pi]
      real(kind=4), intent(in), dimension(-igh:im+igh,jt):: zs, ft
      real(kind=R_GRID), intent(in)::  grid(bd%isd:bd%ied+1, bd%jsd:bd%jed+1,2)
      real(kind=R_GRID), intent(in):: agrid(bd%isd:bd%ied,   bd%jsd:bd%jed,  2)
      integer, intent(in):: jstart, jend
      real(kind=R_GRID), intent(IN) :: stretch_fac
      logical, intent(IN) :: nested
! Output
      real, intent(out):: q2(bd%isd:bd%ied,bd%jsd:bd%jed) !< Mapped data at the target resolution
      real, intent(out):: f2(bd%isd:bd%ied,bd%jsd:bd%jed) !< oro
      real, intent(out):: h2(bd%isd:bd%ied,bd%jsd:bd%jed) !< variances of terrain
! Local
      real :: lon_g(-igh:im+igh)
      real lat_g(jt), cos_g(jt)
      real(kind=R_GRID) e2(2)
      real(kind=R_GRID) grid3(3, bd%isd:bd%ied+1, bd%jsd:bd%jed+1)
      real(kind=R_GRID), dimension(3):: p1, p2, p3, p4, pc, pp
      real(kind=R_GRID), dimension(3):: vp_12, vp_23, vp_34, vp_14
      integer i,j, np, k
      integer ii, jj, i1, i2, j1, j2, min_pts
      real th1, aa, asum,  qsum, fsum, hsum, lon_w, lon_e, lat_s, lat_n, r2d
      real qsp, fsp, hsp
      real qnp, fnp, hnp
      real delg, th0, tmp1, prod1, prod2, prod3, prod4
      integer ig_lon, jp
      integer:: lat_crit
      real pi5, pi2

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

      pi2 = pi + pi
      pi5 = 0.5 * pi
      r2d = 180./pi

      do i=1,im
         lon_g(i) = 0.5*(lon1(i)+lon1(i+1))
      enddo
      do i=-igh,0
         lon_g(i) = lon_g(i+im)
      enddo
      do i=im+1,im+igh
         lon_g(i) = lon_g(i-im)
      enddo

      do j=1,jt
         lat_g(j) = 0.5*(lat1(j)+lat1(j+1))
         cos_g(j) = cos( lat_g(j) )
      enddo

      do j=jsd,jed+1
         do i=isd,ied+1
            call latlon2xyz(real(grid(i,j,1:2),kind=R_GRID), grid3(1,i,j))
         enddo
      enddo

     if(is_master()) write(*,*) 'surf_map: Search started ....'

! stretch_fac * pi5/(npx-1)  / (pi/nlat)
    lat_crit = nint( stretch_fac*real(nlat)/real(npx_global-1) ) 
    lat_crit = min( jt, max( 4,  lat_crit ) )

    if ( jstart==1 ) then
         write(*,*) mpp_pe(), 'lat_crit=', r2d*lat_g(lat_crit)
    elseif ( jend==nlat ) then
!        write(*,*) mpp_pe(), 'lat_crit=', r2d*lat_g(jt-lat_crit+1)
    endif

!----
! SP:
!----
    iF ( jstart == 1 ) then
         asum = 0.
         qsum = 0.
         fsum = 0.
         hsum = 0.
         do j=1,lat_crit
            aa = cos_g(j)
            do i=1,im
               asum = asum + aa
               qsum = qsum + zs(i,j)*aa
               fsum = fsum + ft(i,j)*aa
            enddo
         enddo
         qsp = qsum / asum
         fsp = fsum / asum
         hsum = 0.
         np = 0
         do j=1,lat_crit
            do i=1,im 
               np = np + 1
               hsum = hsum + (qsp-zs(i,j))**2
            enddo
         enddo
         hsp = hsum / real(np)
!        write(*,*) 'SP GID, zs_sp, f_sp, hsp=', mpp_pe(), qsp, fsp, hsp
     endif
!----
! NP:
!----
    iF ( jend == nlat ) then
         asum = 0.
         qsum = 0.
         fsum = 0.
         hsum = 0.
         do jp=jend-lat_crit+1, jend
            j = jp - jstart + 1
            aa = cos_g(j)
            do i=1,im
               asum = asum + aa
               qsum = qsum + zs(i,j)*aa
               fsum = fsum + ft(i,j)*aa
            enddo
         enddo
         qnp = qsum / asum
         fnp = fsum / asum
         hsum = 0.
         np = 0
         do jp=jend-lat_crit+1, jend
            j = jp - jstart + 1
            do i=1,im 
               np = np + 1
               hsum = hsum + (qnp-zs(i,j))**2
            enddo
         enddo
         hnp = hsum / real(np)
!        write(*,*) 'NP GID, zs_np, f_np, hnp=', mpp_pe(), qnp, fnp, hnp
     endif

    min_pts = 999999
    do j=jsd,jed
       do i=isd,ied

          !Do not go into corners
          if (((i < is .and. j < js) .or. &
               (i < is .and. j > je) .or. &
               (i > ie .and. j < js) .or. &
               (i > ie .and. j > je)) .and. .not. nested) then
             q2(i,j) = 1.e25
             f2(i,j) = 1.e25
             h2(i,j) = 1.e25
             goto 4444
          end if

          if ( agrid(i,j,2) < -pi5+stretch_fac*pi5/real(npx_global-1) ) then
! SP:
               q2(i,j) = qsp
               f2(i,j) = fsp
               h2(i,j) = hsp
               goto 4444
          elseif ( agrid(i,j,2) > pi5-stretch_fac*pi5/real(npx_global-1) ) then
! NP:
               q2(i,j) = qnp
               f2(i,j) = fnp
               h2(i,j) = hnp
               goto 4444
          endif

          lat_s = min( grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
          lat_n = max( grid(i,j,2), grid(i+1,j,2), grid(i,j+1,2), grid(i+1,j+1,2), agrid(i,j,2) )
! Enlarge the search zone:
             delg = max( 0.2*(lat_n-lat_s), pi/real(npx_global-1), pi2/real(nlat) )
            lat_s = max(-pi5, lat_s - delg)
            lat_n = min( pi5, lat_n + delg)

            j1 = nint( (pi5+lat_s)/(pi/real(nlat)) ) - 1
            if ( lat_s*r2d < (-90.+90./real(npx_global-1)) ) j1 = 1
            j1 = max(jstart,  j1)

            j2 = nint( (pi5+lat_n)/(pi/real(nlat)) ) + 1
            if ( lat_n*r2d > (90.-90./real(npx_global-1))  ) j2 = nlat
            j2 = min(jend, j2)

            j1 = j1 - jstart + 1
            j2 = j2 - jstart + 1

            lon_w = min( grid(i,j,1), grid(i+1,j,1), grid(i,j+1,1), grid(i+1,j+1,1) ) 
            lon_e = max( grid(i,j,1), grid(i+1,j,1), grid(i,j+1,1), grid(i+1,j+1,1) )

            if ( (lon_e-lon_w) > pi ) then
                 i1 = -nint( (pi2-lon_e)/pi2 * real(im) ) - 1
                 i2 =  nint( lon_w/pi2 * real(im) ) + 1
            else
                 i1 = nint( lon_w/pi2 * real(im) ) - 1
                 i2 = nint( lon_e/pi2 * real(im) ) + 1
            endif

! Enlarge the search zone:
            ig_lon = max(1, (i2-i1)/8)
            i1 = max(  -igh, i1 - ig_lon)
            i2 = min(im+igh, i2 + ig_lon)

              np = 0
            qsum = 0.
            fsum = 0.
            hsum = 0.
            asum = 0.
!
!            4----------3
!           /          /
!          /    pp    /
!         /          /
!        1----------2
!
            do k=1,3
               p1(k) = grid3(k,i,  j)
               p2(k) = grid3(k,i+1,j)
               p3(k) = grid3(k,i+1,j+1)
               p4(k) = grid3(k,i,j+1)
               pc(k) = p1(k) + p2(k) + p3(k) + p4(k)
            enddo
            call normalize_vect( pc )
 
            th0 = min( v_prod(p1,p3), v_prod(p2, p4) )
            th1 = min( cos_grid, cos(0.25*acos(max(v_prod(p1,p3), v_prod(p2, p4)))))

            call  vect_cross(vp_12, p1, p2)
            call  vect_cross(vp_23, p2, p3)
            call  vect_cross(vp_34, p3, p4)
            call  vect_cross(vp_14, p1, p4)

            prod1 = v_prod(p3, vp_12)
            prod2 = v_prod(p1, vp_23)
            prod3 = v_prod(p1, vp_34)
            prod4 = v_prod(p2, vp_14)

            do jj=j1,j2
                  aa = cos_g(jj)
               e2(2) = lat_g(jj)
               do ii=i1,i2
                  e2(1) = lon_g(ii)
                  call latlon2xyz(e2, pp)
! Check two extrems:
                  tmp1 = v_prod(pp, pc)
! Points that are close to center:
                  if ( tmp1 > th1 ) goto 1111    ! inside
! check to exclude points too far away:
                  if ( tmp1 < th0 ) goto 2222    ! outside
! Check if inside the polygon
                  if ( v_prod(pp, vp_12)*prod1 < 0. ) goto 2222
                  if ( v_prod(pp, vp_23)*prod2 < 0. ) goto 2222
                  if ( v_prod(pp, vp_34)*prod3 < 0. ) goto 2222
                  if ( v_prod(pp, vp_14)*prod4 < 0. ) goto 2222
1111                np = np + 1
                  qsum = qsum + zs(ii,jj)*aa
                  fsum = fsum + ft(ii,jj)*aa
                  hsum = hsum + zs(ii,jj)**2
                  asum = asum + aa
2222              continue
               enddo
            enddo

            if ( np > 0 ) then
                 q2(i,j) = qsum / asum
                 f2(i,j) = fsum / asum
                 h2(i,j) = hsum / real(np) - q2(i,j)**2
                 min_pts = min(min_pts, np)
            else
                 write(*,*) 'min and max lat_g is ', r2d*minval(lat_g), r2d*maxval(lat_g), mpp_pe()
                 write(*,*) 'Surf_map failed for GID=', mpp_pe(), i,j, '(lon,lat)=', r2d*agrid(i,j,1),r2d*agrid(i,j,2)
                 write(*,*) '[jstart, jend]', jstart, jend
                 call mpp_error(FATAL,'Surf_map failed')
            endif
4444  continue
      enddo
    enddo

      if(is_master()) write(*,*) 'surf_map: minimum pts per cell (master PE)=', min_pts
      if ( min_pts <3 ) then
           if(is_master()) write(*,*) 'Warning: too few points used in creating the cell mean terrain !!!'
      endif

 end subroutine map_to_cubed_raw


#ifdef JUNK
 logical function inside_p4(p1, p2, p3, p4, pp, th0)

      real, intent(in):: p1(3), p2(3), p3(3), p4(3)
      real, intent(in):: pp(3)
      real, intent(in):: th0
! A * B = |A| |B| cos(angle)
! Local:
      real(kind=R_GRID) v1(3), v2(3), vp(3)
      real(kind=R_GRID) tmp1
      integer k

      inside_p4 = .false.

! 1st check: to exclude points too far away:
      do k=1,3
         vp(k) = p1(k) + p2(k) + p3(k) + p4(k)
      enddo
      call normalize_vect( vp )

      tmp1 = v_prod(pp, vp)
      if ( tmp1 < th0 ) then           ! outside
           return
      endif

!                                   1st segment: 1---2
      call  vect_cross(vp, p1, p2)
      if ( v_prod(pp, vp)*v_prod(p3, vp) < 0. ) return
!                                   2nd segment: 2---3
      call  vect_cross(vp, p2, p3)
      if ( v_prod(pp, vp)*v_prod(p1, vp) < 0. ) return
!                                   3rd segment: 3---4
      call  vect_cross(vp, p3, p4)
      if ( v_prod(pp, vp)*v_prod(p1, vp) < 0. ) return
!                                   4th segment: 1---4
      call  vect_cross(vp, p1, p4)
      if ( v_prod(pp, vp)*v_prod(p2, vp) < 0. ) return

      inside_p4 = .true.

 end function inside_p4
#endif

!>@brief The subroutine 'handle_err' returns an error when
!! it cannot find or correctly read in an external file.
 subroutine handle_err(status)
#include <netcdf.inc>
      integer          status

      if (status .ne. nf_noerr) then
        print *, nf_strerror(status)
        stop 'Stopped'
      endif

 end subroutine  handle_err

 subroutine remove_ice_sheets (lon, lat, lfrac, bd )
!---------------------------------
! Bruce Wyman's fix for Antarctic
!--------------------------------- 
      type(fv_grid_bounds_type), intent(IN) :: bd
      real(kind=R_GRID), intent(in)    :: lon(bd%isd:bd%ied,bd%jsd:bd%jed), lat(bd%isd:bd%ied,bd%jsd:bd%jed)
      real, intent(inout) :: lfrac(bd%isd:bd%ied,bd%jsd:bd%jed)

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

! lon   = longitude in radians
! lat   = latitude in radians
! lfrac = land-sea mask (land=1, sea=0)
            
      integer :: i, j
      real :: dtr, phs, phn
            
      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed
        
      dtr = acos(0.)/90.
      phs = -83.9999*dtr                                  
!     phn = -78.9999*dtr
      phn = -76.4*dtr
            
      do j = jsd, jed
         do i = isd, ied
         if ( lat(i,j) < phn ) then
                              ! replace all below this latitude
         if ( lat(i,j) < phs ) then
              lfrac(i,j) = 1.0
              cycle
         endif
                              ! replace between 270 and 360 deg
         if ( sin(lon(i,j)) < 0. .and. cos(lon(i,j)) > 0.) then
              lfrac(i,j) = 1.0
              cycle 
         endif
         endif
         enddo
      enddo
 end subroutine remove_ice_sheets

!>@brief The subroutine 'read_namelis' reads the namelist file, 
!! writes the namelist to log file, and initializes constants.
subroutine read_namelist
  integer :: unit, ierr, io
!   real    :: dtr, ght

!  read namelist

   if (namelist_read) return
#ifdef INTERNAL_FILE_NML
    read  (input_nml_file, nml=surf_map_nml, iostat=io)
    ierr = check_nml_error(io,'surf_map_nml')
#else
    unit = open_namelist_file ( )
    ierr=1 
    do while (ierr /= 0)
      read  (unit, nml=surf_map_nml, iostat=io, end=10)
      ierr = check_nml_error(io,'surf_map_nml')
    enddo
 10 call close_file (unit)
#endif

!  write version and namelist to log file

   if (mpp_pe() == mpp_root_pe()) then
     unit = stdlog()
     write (unit, nml=surf_map_nml)
   endif

   namelist_read = .true.

end subroutine read_namelist

!> The sugroutine 'zonal_mean' replaces 'p' with its zonal mean.
subroutine zonal_mean(im, p, zmean)
   integer, intent(in):: im
   real(kind=4), intent(inout):: p(im)
   real, intent(out):: zmean
   integer i

   zmean = 0.
   do i=1,im
      zmean = zmean + p(i)
   enddo
   zmean = zmean / real(im)
   do i=1,im
      p(i) = zmean
   enddo
end subroutine zonal_mean


 end module fv_surf_map_mod
