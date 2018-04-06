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

 module test_cases_mod

! <table>
! <tr>
!     <th>Module Name</th>
!     <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>constants_mod</td>
!     <td>cnst_radius=>radius, pi=>pi_8, omega, grav, kappa, rdgas, cp_air, rvgas</td>
!   </tr>
!   <tr>
!     <td>diag_manager_mod</td>
!     <td>diag_axis_init, register_diag_field,
!         register_static_field, send_data, diag_grid_init</td>
!   </tr>
!   <tr>
!     <td>field_manager_mod</td>
!     <td>MODEL_ATMOS</td>
!   </tr>
!   <tr>
!     <td>fv_arrays_mod</td>
!     <td>fv_grid_type, fv_flags_type, fv_grid_bounds_type, R_GRID</td>
!   </tr>
!   <tr>
!     <td>fv_diagnostics_mod</td>
!     <td>prt_maxmin, ppme, eqv_pot, qcly0</td>
!   </tr>
!   <tr>
!     <td>fv_grid_tools_mod</td>
!     <td>todeg, missing, spherical_to_cartesian</td>
!   </tr>
!   <tr>
!     <td>fv_eta_mod</td>
!     <td>compute_dz_L32, compute_dz_L101, set_hybrid_z,
!         gw_1d,hybrid_z_dz</td>
!   </tr>
!   <tr>
!     <td>fv_mp_mod</td>
!     <td>ng, is_master,is,js,ie,je, isd,jsd,ied,jed, 
!         domain_decomp, fill_corners, XDir, YDir, mp_stop, 
!         mp_reduce_sum, mp_reduce_max, mp_gather, mp_bcst</td>
!   </tr>
!   <tr>
!     <td>fv_sg_mod</td>
!     <td>qsmith</td>
!   </tr>
!   <tr>
!     <td>fv_surf_map_mod</td>
!     <td>surfdrv</td>
!   </tr>
!   <tr>
!     <td>init_hydro_mod</td>
!     <td>p_var, hydro_eq</td>
!   </tr>
!   <tr>
!     <td>mpp_mod</td>
!     <td>mpp_error, FATAL, mpp_root_pe, mpp_broadcast, mpp_sum,
!         mpp_pe, mpp_chksum, stdout</td>
!   </tr>
!   <tr>
!     <td>mpp_domains_mod</td>
!     <td>mpp_update_domains, domain2d</td>
!   </tr>>
!   <tr>
!     <td>mpp_parameter_mod</td>
!     <td>AGRID_PARAM=>AGRID,CGRID_NE_PARAM=>CGRID_NE,SCALAR_PAIR</td>
!   </tr>
!   <tr>
!     <td>time_manager_mod</td>
!     <td>time_type, get_date, get_time</td>
!   </tr>
!   <tr>
!     <td>tracer_manager_mod</td>
!     <td>get_tracer_index</td>
!   </tr>
! </table>

      use constants_mod,     only: cnst_radius=>radius, pi=>pi_8, omega, grav, kappa, rdgas, cp_air, rvgas
      use init_hydro_mod,    only: p_var, hydro_eq
      use fv_mp_mod,         only: ng, is_master,        &
                                   is,js,ie,je, isd,jsd,ied,jed, &
                                   domain_decomp, fill_corners, XDir, YDir, &
                                   mp_stop, mp_reduce_sum, mp_reduce_max, mp_gather, mp_bcst
      use fv_grid_utils_mod, only: cubed_to_latlon, great_circle_dist, mid_pt_sphere,    &
                                   ptop_min, inner_prod, get_latlon_vector, get_unit_vect2, &
                                   g_sum, latlon2xyz, cart_to_latlon, make_eta_level, f_p, project_sphere_v
      use fv_surf_map_mod,   only: surfdrv

      use fv_grid_tools_mod, only: todeg, missing, spherical_to_cartesian
      use fv_eta_mod,        only: compute_dz_L32, compute_dz_L101, set_hybrid_z, gw_1d,   &
                                   hybrid_z_dz

      use mpp_mod,           only: mpp_error, FATAL, mpp_root_pe, mpp_broadcast, mpp_sum
      use mpp_domains_mod,   only: mpp_update_domains, domain2d
      use mpp_parameter_mod, only: AGRID_PARAM=>AGRID,CGRID_NE_PARAM=>CGRID_NE, &
                                   SCALAR_PAIR
      use fv_sg_mod,         only: qsmith
      use fv_diagnostics_mod, only: prt_maxmin, ppme, eqv_pot, qcly0
!!! DEBUG CODE
     use mpp_mod, only: mpp_pe, mpp_chksum, stdout
!!! END DEBUG CODE
      use fv_arrays_mod,         only: fv_grid_type, fv_flags_type, fv_grid_bounds_type, R_GRID
      use tracer_manager_mod,    only: get_tracer_index
      use field_manager_mod,     only: MODEL_ATMOS
      implicit none
      private

! Test Case Number  
!                   -1 = Divergence conservation test
!                    0 = Idealized non-linear deformational flow
!                    1 = Cosine Bell advection
!                    2 = Zonal geostrophically balanced flow
!                    3 = non-rotating potential flow 
!                    4 = Tropical cyclones (merger of Rankine vortices)
!                    5 = Zonal geostrophically balanced flow over an isolated mountain
!                    6 = Rossby Wave number 4 
!                    7 = Barotropic instability
!                    !   8 = Potential flow (as in 5 but no rotation and initially at rest)
!                    8 = "Soliton" propagation twin-vortex along equator
!                    9 = Polar vortex
!                   10 = hydrostatically balanced 3D test with idealized mountain
!                   11 = Use this for cold starting the climate model with USGS terrain
!                   12 = Jablonowski & Williamson Baroclinic test case (Steady State)
!                   13 = Jablonowski & Williamson Baroclinic test case Perturbation
!                  -13 = DCMIP 2016 J&W BC Wave, with perturbation
!                   14 = Use this for cold starting the Aqua-planet model
!                   15 = Small Earth density current
!                   16 = 3D hydrostatic non-rotating Gravity waves
!                   17 = 3D hydrostatic rotating Inertial Gravity waves (case 6-3-0)
!                   18 = 3D mountain-induced Rossby wave
!                   19 = As in 15 but without rotation
!                   20 = 3D non-hydrostatic lee vortices; non-rotating (small planet)
!                   21 = 3D non-hydrostatic lee vortices; rotating     (small planet)
!                   30 = Super-Cell storm, curved hodograph, centered at OKC, no rotation
!                   31 = Super-Cell storm, curved hodograph, centered at OKC, with rotation
!                   32 = Super-Cell storm, straight hodograph, centered at OKC, no rotation
!                   33 = HIWPP Schar mountain waves, Ridge mountain (M1)
!                   34 = HIWPP Schar mountain waves, Circular mountain (M2)
!                   35 = HIWPP Schar mountain waves, Circular mountain with shear (M3)
!                   36 = HIWPP Super_Cell; no perturbation
!                   37 = HIWPP Super_Cell; with the prescribed thermal
!                   44 = Lock-exchange on the sphere; atm at rest with no mountain
!                   45 = New test
!                   51 = 3D tracer advection (deformational nondivergent flow)
!                   55 = TC 
!                  101 = 3D non-hydrostatic Large-Eddy-Simulation (LES) with hybrid_z IC

      integer :: sphum, theta_d
      real(kind=R_GRID), parameter :: radius = cnst_radius
      real(kind=R_GRID), parameter :: one = 1.d0
      integer :: test_case
      logical :: bubble_do
      real    :: alpha
      integer :: Nsolitons
      real    :: soliton_size = 750.e3, soliton_Umax = 50.

! Case 0 parameters
      real :: p0_c0 = 3.0
      real :: rgamma = 5.0
      real :: lat0 = pi/2.0 !< pi/4.8
      real :: lon0 = 0.0 !<pi-0.8

!  pi_shift moves the initial location of the cosine bell for Case 1
      real, parameter :: pi_shift = 0.0 !< 3.0*pi/4.

 ! -1:null_op, 0:All-Grids, 1:C-Grid, 2:D-Grid, 3:A-Grid, 4:A-Grid then Rotate, 5:D-Grid with unit vectors then Rotate
      integer, parameter :: initWindsCase0 =-1 
      integer, parameter :: initWindsCase1 = 1
      integer, parameter :: initWindsCase2 = 5 
      integer, parameter :: initWindsCase5 = 5
      integer, parameter :: initWindsCase6 =-1 
      integer, parameter :: initWindsCase9 =-1

      real, allocatable, dimension(:) :: pz0, zz0

      integer :: tracer_test, wind_field

     ! Ubar = initial wind speed parameter
     real   :: Ubar, Vbar
     ! gh0 = initial surface height parameter
     real   :: gh0

     !  case 9 parameters
     real  , allocatable :: case9_B(:,:)
     real   :: AofT(2)


     !  Validating fields used in statistics
     real  , allocatable :: phi0(:,:,:) !< Validating Field
     real  , allocatable :: ua0(:,:,:)  !< Validating U-Wind
     real  , allocatable :: va0(:,:,:)  !< Validating V-Windfms_io_exit, get_tile_string, &

     real  , allocatable :: gh_table(:), lats_table(:)
     logical :: gh_initialized = .false.

     !  Initial Conservation statistics ; total mass ; enstrophy ; energy
     real   :: tmass_orig !< total mass
     real   :: tvort_orig !< enstrophy (integral of total vorticity)
     real   :: tener_orig !< energy

     integer, parameter :: interpOrder = 1

      public :: pz0, zz0
      public :: test_case, bubble_do, alpha, tracer_test, wind_field, nsolitons, soliton_Umax, soliton_size
      public :: init_case, get_stats, check_courant_numbers
#ifdef NCDF_OUTPUT
      public :: output, output_ncdf
#endif
      public :: case9_forcing1, case9_forcing2, case51_forcing
      public :: init_double_periodic, init_latlon
      public :: checker_tracers

  INTERFACE mp_update_dwinds
     MODULE PROCEDURE mp_update_dwinds_2d
     MODULE PROCEDURE mp_update_dwinds_3d
  END INTERFACE

      contains

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'init_winds' initialize the winds.
      subroutine init_winds(UBar, u,v,ua,va,uc,vc, defOnGrid, npx, npy, ng, ndims, nregions, nested, gridstruct, domain, tile)
 ! defOnGrid = -1:null_op, 0:All-Grids, 1:C-Grid, 2:D-Grid, 3:A-Grid, 4:A-Grid then Rotate, 5:D-Grid with unit vectors then Rotate

      real  ,    intent(INOUT) :: UBar
      real ,      intent(INOUT) ::    u(isd:ied  ,jsd:jed+1)
      real ,      intent(INOUT) ::    v(isd:ied+1,jsd:jed  )
      real ,      intent(INOUT) ::   uc(isd:ied+1,jsd:jed  )
      real ,      intent(INOUT) ::   vc(isd:ied  ,jsd:jed+1)
      real ,      intent(INOUT) ::   ua(isd:ied  ,jsd:jed  )
      real ,      intent(INOUT) ::   va(isd:ied  ,jsd:jed  )
      integer,      intent(IN) :: defOnGrid
      integer,      intent(IN) :: npx, npy
      integer,      intent(IN) :: ng
      integer,      intent(IN) :: ndims
      integer,      intent(IN) :: nregions
      logical,      intent(IN) :: nested
      type(fv_grid_type), intent(IN), target :: gridstruct
      type(domain2d), intent(INOUT) :: domain
      integer, intent(IN)  :: tile

      real(kind=R_GRID) :: p1(2), p2(2), p3(2), p4(2), pt(2)
      real(kind=R_GRID) :: e1(3), e2(3), ex(3), ey(3)

      real   :: dist, r, r0 
      integer :: i,j,k,n
      real :: utmp, vtmp

      real :: psi_b(isd:ied+1,jsd:jed+1), psi(isd:ied,jsd:jed), psi1, psi2 
      integer :: is2, ie2, js2, je2

      real(kind=R_GRID), pointer, dimension(:,:,:)   :: agrid, grid
      real, pointer, dimension(:,:)     :: area, rarea, fC, f0
      real(kind=R_GRID), pointer, dimension(:,:,:)   :: ee1, ee2, en1, en2
      real(kind=R_GRID), pointer, dimension(:,:,:,:) :: ew, es
      real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

      logical, pointer :: cubed_sphere, latlon

      logical, pointer :: have_south_pole, have_north_pole

      integer, pointer :: ntiles_g
      real,    pointer :: acapN, acapS, globalarea

      grid => gridstruct%grid_64
      agrid=> gridstruct%agrid_64

      area  => gridstruct%area
      rarea => gridstruct%rarea

      fC    => gridstruct%fC
      f0    => gridstruct%f0

      ee1   => gridstruct%ee1
      ee2   => gridstruct%ee2
      ew    => gridstruct%ew
      es    => gridstruct%es
      en1   => gridstruct%en1
      en2   => gridstruct%en2

      dx      => gridstruct%dx
      dy      => gridstruct%dy
      dxa     => gridstruct%dxa
      dya     => gridstruct%dya
      rdxa    => gridstruct%rdxa
      rdya    => gridstruct%rdya
      dxc     => gridstruct%dxc
      dyc     => gridstruct%dyc
      
      cubed_sphere => gridstruct%cubed_sphere
      latlon       => gridstruct%latlon

      have_south_pole               => gridstruct%have_south_pole
      have_north_pole               => gridstruct%have_north_pole

      ntiles_g                      => gridstruct%ntiles_g
      acapN                         => gridstruct%acapN
      acapS                         => gridstruct%acapS
      globalarea                    => gridstruct%globalarea

      if (nested) then

         is2 = is-2
         ie2 = ie+2
         js2 = js-2
         je2 = je+2

      else

         is2 = is
         ie2 = ie
         js2 = js
         je2 = je

      end if

 200  format(i4.4,'x',i4.4,'x',i4.4,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)

      psi(:,:) = 1.e25
      psi_b(:,:) = 1.e25
      do j=jsd,jed
         do i=isd,ied
            psi(i,j) = (-1.0 * Ubar * radius *( sin(agrid(i,j,2))                  *cos(alpha) - &
                                            cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) ) )
         enddo
      enddo
      call mpp_update_domains( psi, domain )
      do j=jsd,jed+1
         do i=isd,ied+1
            psi_b(i,j) = (-1.0 * Ubar * radius *( sin(grid(i,j,2))                 *cos(alpha) - &
                                              cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) ) )
         enddo
      enddo

      if ( (cubed_sphere) .and. (defOnGrid==0) ) then
         do j=js,je+1
            do i=is,ie
               dist = dx(i,j)
               vc(i,j) = (psi_b(i+1,j)-psi_b(i,j))/dist
               if (dist==0) vc(i,j) = 0.
            enddo
         enddo
         do j=js,je
            do i=is,ie+1
               dist = dy(i,j)
               uc(i,j) = -1.0*(psi_b(i,j+1)-psi_b(i,j))/dist
               if (dist==0) uc(i,j) = 0.
            enddo
         enddo
         call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE_PARAM)
         call fill_corners(uc, vc, npx, npy, VECTOR=.true., CGRID=.true.)
         do j=js,je
            do i=is,ie+1
               dist = dxc(i,j)
               v(i,j) = (psi(i,j)-psi(i-1,j))/dist
               if (dist==0) v(i,j) = 0.
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               dist = dyc(i,j)
               u(i,j) = -1.0*(psi(i,j)-psi(i,j-1))/dist
               if (dist==0) u(i,j) = 0.
            enddo
         enddo
         call mp_update_dwinds(u, v, npx, npy, domain)
         do j=js,je
            do i=is,ie
               psi1 = 0.5*(psi(i,j)+psi(i,j-1))
               psi2 = 0.5*(psi(i,j)+psi(i,j+1))
               dist = dya(i,j)
               ua(i,j) = -1.0 * (psi2 - psi1) / (dist)
               if (dist==0) ua(i,j) = 0.
               psi1 = 0.5*(psi(i,j)+psi(i-1,j))
               psi2 = 0.5*(psi(i,j)+psi(i+1,j))
               dist = dxa(i,j)
               va(i,j) = (psi2 - psi1) / (dist)
               if (dist==0) va(i,j) = 0.
            enddo
         enddo

      elseif ( (cubed_sphere) .and. (defOnGrid==1) ) then
         do j=js,je+1
            do i=is,ie
               dist = dx(i,j)
               vc(i,j) = (psi_b(i+1,j)-psi_b(i,j))/dist
               if (dist==0) vc(i,j) = 0.
            enddo
         enddo
         do j=js,je
            do i=is,ie+1
               dist = dy(i,j)
               uc(i,j) = -1.0*(psi_b(i,j+1)-psi_b(i,j))/dist
               if (dist==0) uc(i,j) = 0.
            enddo
         enddo
         call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE_PARAM)
         call fill_corners(uc, vc, npx, npy, VECTOR=.true., CGRID=.true.)
         call ctoa(uc,vc,ua,va,dx, dy, dxc,dyc,dxa,dya,npx,npy,ng)
         call atod(ua,va,u ,v ,dxa, dya,dxc,dyc,npx,npy,ng, nested, domain)
        ! call d2a2c(npx,npy,1, is,ie, js,je, ng, u(isd,jsd),v(isd,jsd), &
        !            ua(isd,jsd),va(isd,jsd), uc(isd,jsd),vc(isd,jsd))
      elseif ( (cubed_sphere) .and. (defOnGrid==2) ) then
         do j=js2,je2
            do i=is2,ie2+1
               dist = dxc(i,j)
               v(i,j) = (psi(i,j)-psi(i-1,j))/dist
               if (dist==0) v(i,j) = 0.            
            enddo
         enddo
         do j=js2,je2+1
            do i=is2,ie2
               dist = dyc(i,j)
               u(i,j) = -1.0*(psi(i,j)-psi(i,j-1))/dist
               if (dist==0) u(i,j) = 0. 
            enddo
         enddo
         call mp_update_dwinds(u, v, npx, npy, domain)
         call dtoa( u, v,ua,va,dx,dy,dxa,dya,dxc,dyc,npx,npy,ng)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, nested, domain) 
      elseif ( (cubed_sphere) .and. (defOnGrid==3) ) then
         do j=js,je
            do i=is,ie
               psi1 = 0.5*(psi(i,j)+psi(i,j-1))
               psi2 = 0.5*(psi(i,j)+psi(i,j+1))
               dist = dya(i,j)
               ua(i,j) = -1.0 * (psi2 - psi1) / (dist)
               if (dist==0) ua(i,j) = 0.
               psi1 = 0.5*(psi(i,j)+psi(i-1,j))
               psi2 = 0.5*(psi(i,j)+psi(i+1,j))
               dist = dxa(i,j)
               va(i,j) = (psi2 - psi1) / (dist)
               if (dist==0) va(i,j) = 0.
            enddo
         enddo
         call mpp_update_domains( ua, va, domain, gridtype=AGRID_PARAM)
         call atod(ua,va, u, v,dxa, dya,dxc,dyc,npx,npy,ng, nested, domain)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, nested,domain)
      elseif ( (latlon) .or. (defOnGrid==4) ) then

         do j=js,je
            do i=is,ie
               ua(i,j) =  Ubar * ( COS(agrid(i,j,2))*COS(alpha) + &
                                     SIN(agrid(i,j,2))*COS(agrid(i,j,1))*SIN(alpha) )
               va(i,j) = -Ubar *   SIN(agrid(i,j,1))*SIN(alpha)  
               call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
               call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
               call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p3)
               call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p4)
               if (cubed_sphere) call rotate_winds(ua(i,j), va(i,j), p1,p2,p3,p4, agrid(i,j,1:2), 2, 1)

               psi1 = 0.5*(psi(i,j)+psi(i,j-1))
               psi2 = 0.5*(psi(i,j)+psi(i,j+1))
               dist = dya(i,j)
    if ( (tile==1) .and.(i==1) ) print*, ua(i,j), -1.0 * (psi2 - psi1) / (dist)

            enddo
         enddo
         call mpp_update_domains( ua, va, domain, gridtype=AGRID_PARAM)
         call atod(ua,va, u, v,dxa, dya,dxc,dyc,npx,npy,ng, nested, domain)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, nested, domain)
     elseif ( (latlon) .or. (defOnGrid==5) ) then
! SJL mods:
! v-wind:
         do j=js2,je2
            do i=is2,ie2+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, pt)
               call get_unit_vect2 (p1, p2, e2)
               call get_latlon_vector(pt, ex, ey)
               utmp =  Ubar * ( COS(pt(2))*COS(alpha) + &
                                SIN(pt(2))*COS(pt(1))*SIN(alpha) )
               vtmp = -Ubar *   SIN(pt(1))*SIN(alpha)
               v(i,j) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
            enddo
         enddo
! D grid u-wind:
         do j=js2,je2+1
            do i=is2,ie2
               p1(:) = grid(i  ,j  ,1:2)
               p2(:) = grid(i+1,j  ,1:2)
               call mid_pt_sphere(p1, p2, pt)
               call get_unit_vect2 (p1, p2, e1)
               call get_latlon_vector(pt, ex, ey)
               utmp =  Ubar * ( COS(pt(2))*COS(alpha) + &
                                SIN(pt(2))*COS(pt(1))*SIN(alpha) )
               vtmp = -Ubar *   SIN(pt(1))*SIN(alpha)
               u(i,j) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
            enddo
         enddo

         call mp_update_dwinds(u, v, npx, npy, domain)
         call dtoa( u, v,ua,va,dx,dy,dxa,dya,dxc,dyc,npx,npy,ng)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, nested, domain)
     else
         !print*, 'Choose an appropriate grid to define the winds on'
         !stop
     endif

      end subroutine init_winds
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'init_case' initialize the Williamson test cases;
!
!     init_case :: initialize the Williamson test cases:
!                  case 1 (2-D advection of a cosine bell)
!                  case 2 (Steady State Zonal Geostrophic Flow)
!                  case 5 (Steady State Zonal Geostrophic Flow over Mountain)
!                  case 6 (Rossby Wave-4 Case)
!                  case 9 (Stratospheric Vortex Breaking Case)
!
      subroutine init_case(u,v,w,pt,delp,q,phis, ps,pe,peln,pk,pkz,  uc,vc, ua,va, ak, bk,  &
                           gridstruct, flagstruct, npx, npy, npz, ng, ncnst, nwat, ndims, nregions,        &
                           dry_mass, mountain, moist_phys, hydrostatic, hybrid_z, delz, ze0, adiabatic, &
                           ks, npx_global, ptop, domain_in, tile_in, bd)
      
      type(fv_grid_bounds_type), intent(IN) :: bd
      real ,      intent(INOUT) ::    u(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
      real ,      intent(INOUT) ::    v(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
      real ,      intent(INOUT) ::    w(bd%isd:  ,bd%jsd:  ,1:)
      real ,      intent(INOUT) ::   pt(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
      real ,      intent(INOUT) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
      real ,      intent(INOUT) ::    q(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, ncnst)

      real ,      intent(INOUT) :: phis(bd%isd:bd%ied  ,bd%jsd:bd%jed  )

      real ,      intent(INOUT) ::   ps(bd%isd:bd%ied  ,bd%jsd:bd%jed  )
      real ,      intent(INOUT) ::   pe(bd%is-1:bd%ie+1,npz+1,bd%js-1:bd%je+1)
      real ,      intent(INOUT) ::   pk(bd%is:bd%ie    ,bd%js:bd%je    ,npz+1)
      real ,      intent(INOUT) :: peln(bd%is :bd%ie   ,npz+1    ,bd%js:bd%je)
      real ,      intent(INOUT) ::  pkz(bd%is:bd%ie    ,bd%js:bd%je    ,npz  )

      real ,      intent(INOUT) ::   uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
      real ,      intent(INOUT) ::   vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
      real ,      intent(INOUT) ::   ua(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
      real ,      intent(INOUT) ::   va(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
      real ,      intent(inout) :: delz(bd%isd:,bd%jsd:,1:)
      real ,      intent(inout)   ::  ze0(bd%is:,bd%js:,1:)

      real ,      intent(inout) ::   ak(npz+1)
      real ,      intent(inout) ::   bk(npz+1)

      integer,      intent(IN) :: npx, npy, npz
      integer,      intent(IN) :: ng, ncnst, nwat
      integer,      intent(IN) :: ndims
      integer,      intent(IN) :: nregions

      real,         intent(IN) :: dry_mass
      logical,      intent(IN) :: mountain
      logical,      intent(IN) :: moist_phys
      logical,      intent(IN) :: hydrostatic
      logical,      intent(IN) :: hybrid_z
      logical,      intent(IN) :: adiabatic
      integer,      intent(IN) :: ks

      type(fv_grid_type), target :: gridstruct
      type(fv_flags_type), target, intent(IN) :: flagstruct

      integer, intent(IN) :: npx_global
      integer, intent(IN), target :: tile_in
      real, intent(INOUT) :: ptop

      type(domain2d), intent(IN), target :: domain_in

      real   ::  tmp(1-ng:npx  +ng,1-ng:npy  +ng,1:nregions)
      real   :: tmp1(1   :npx     ,1   :npy     ,1:nregions)

      real(kind=R_GRID)   :: p0(2)       ! Temporary Point
      real(kind=R_GRID)   :: p1(2)      ! Temporary Point
      real(kind=R_GRID)   :: p2(2)      ! Temporary Point
      real(kind=R_GRID)   :: p3(2)      ! Temporary Point
      real(kind=R_GRID)   :: p4(2)      ! Temporary Point
      real(kind=R_GRID)   :: pa(2)      ! Temporary Point
      real(kind=R_GRID)   :: pb(2)      ! Temporary Point
      real(kind=R_GRID)   :: pcen(2)    ! Temporary Point
      real(kind=R_GRID)   :: e1(3), e2(3), e3(3), ex(3), ey(3)
      real   :: dist, r, r1, r2, r0, omg, A, B, C
      integer :: i,j,k,nreg,z,zz
      integer :: i0,j0,n0, nt
      real   :: utmp,vtmp,ftmp
      real   :: rk

      integer, parameter :: jm = 5761
      real   :: ll_phi(jm)
      real   ::   ll_u(jm)
      real   ::   ll_j(jm)
      real   ::   cose(jm)
      real   ::   sine(jm)
      real   ::   cosp(jm)
      real   :: ddeg, deg, DDP, DP, ph5
      real   :: myB, myC, yy
      integer   :: jj,jm1

      real :: Vtx, p, w_p
      real :: x1,y1,z1,x2,y2,z2,ang

      integer :: initWindsCase

      real :: dummy
      real :: ftop
      real :: v1,v2
      real :: m=1
      real :: n=1
      real :: L1_norm
      real :: L2_norm
      real :: Linf_norm
      real :: pmin, pmin1
      real :: pmax, pmax1
      real :: grad(bd%isd:bd%ied  ,bd%jsd:bd%jed,2)
      real :: div0(bd%isd:bd%ied  ,bd%jsd:bd%jed  ) 
      real :: vor0(bd%isd:bd%ied  ,bd%jsd:bd%jed  )
      real :: divg(bd%isd:bd%ied  ,bd%jsd:bd%jed  )
      real :: vort(bd%isd:bd%ied  ,bd%jsd:bd%jed  )
      real :: ztop, rgrav, p00, pturb, zmid, pk0, t00
      real :: dz1(npz), ppt(npz)
      real :: ze1(npz+1), pe1(npz+1)

      integer :: nlon,nlat
      character(len=80) :: oflnm, hgtflnm
      integer :: is2, ie2, js2, je2

     real :: psi(bd%isd:bd%ied,bd%jsd:bd%jed)
     real :: psi_b(bd%isd:bd%ied+1,bd%jsd:bd%jed+1)
     real :: psi1, psi2

! Baroclinic Test Case 12
      real :: eta(npz), eta_0, eta_s, eta_t
      real :: eta_v(npz), press, anti_rot
      real :: T_0, T_mean, delta_T, lapse_rate, n2, zeta, s0
      real :: pt1,pt2,pt3,pt4,pt5,pt6, pt7, pt8, pt9, u1, pt0
      real :: uu1, uu2, uu3, vv1, vv2, vv3
!     real wbuffer(npx+1,npz)
!     real sbuffer(npy+1,npz)
      real wbuffer(npy+2,npz)
      real sbuffer(npx+2,npz)
 
      real :: gz(bd%isd:bd%ied,bd%jsd:bd%jed,npz+1), zt, zdist
      real :: zvir

      integer :: Cl, Cl2

! Super-Cell
      real :: us0 = 30.
      real, dimension(npz):: pk1, ts1, qs1, uz1, zs1, dudz
      real:: zm, zc
      real(kind=R_GRID):: pp0(2)       ! center position

!Test case 35
      real:: cs_m3
!Test case 51
      real :: omega0, k_cell, z0, H, px
      real :: d1, d2, p1p(2), rt, s
      real :: wind_alpha, period, h0, rm, zp3(3), dz3(3), k0, lp


!Test case 55
      real, dimension(npz+1) :: pe0, gz0, ue, ve, we, pte, qe
      real :: d, cor, exppr, exppz, gamma, Ts0, q00, exponent, ztrop, height, zp, rp
      real :: qtrop, ttrop, zq1, zq2
      real :: dum, dum1, dum2, dum3, dum4, dum5, dum6, ptmp, uetmp, vetmp
      real ::   pe_u(bd%is:bd%ie,npz+1,bd%js:bd%je+1)
      real ::   pe_v(bd%is:bd%ie+1,npz+1,bd%js:bd%je)
      real ::   ps_u(bd%is:bd%ie,bd%js:bd%je+1)
      real ::   ps_v(bd%is:bd%ie+1,bd%js:bd%je)


      real :: dz, zetam

      real(kind=R_GRID), pointer, dimension(:,:,:)   :: agrid, grid
      real(kind=R_GRID), pointer, dimension(:,:)     :: area
      real, pointer, dimension(:,:)     :: rarea, fC, f0
      real(kind=R_GRID), pointer, dimension(:,:,:)   :: ee1, ee2, en1, en2
      real(kind=R_GRID), pointer, dimension(:,:,:,:) :: ew, es
      real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

      logical, pointer :: cubed_sphere, latlon

      type(domain2d), pointer :: domain
      integer, pointer :: tile

      logical, pointer :: have_south_pole, have_north_pole

      integer, pointer :: ntiles_g
      real,    pointer :: acapN, acapS, globalarea

      is  = bd%is
      ie  = bd%ie
      js  = bd%js
      je  = bd%je
      isd = bd%isd
      ied = bd%ied
      jsd = bd%jsd
      jed = bd%jed

      grid => gridstruct%grid_64
      agrid=> gridstruct%agrid_64

      area  => gridstruct%area_64
      rarea => gridstruct%rarea

      fC    => gridstruct%fC
      f0    => gridstruct%f0

      ee1   => gridstruct%ee1
      ee2   => gridstruct%ee2
      ew    => gridstruct%ew
      es    => gridstruct%es
      en1   => gridstruct%en1
      en2   => gridstruct%en2

      dx      => gridstruct%dx
      dy      => gridstruct%dy
      dxa     => gridstruct%dxa
      dya     => gridstruct%dya
      rdxa    => gridstruct%rdxa
      rdya    => gridstruct%rdya
      dxc     => gridstruct%dxc
      dyc     => gridstruct%dyc
      
      cubed_sphere => gridstruct%cubed_sphere
      latlon       => gridstruct%latlon

      domain => domain_in
      tile => tile_in

      have_south_pole               => gridstruct%have_south_pole
      have_north_pole               => gridstruct%have_north_pole

      ntiles_g                      => gridstruct%ntiles_g
      acapN                         => gridstruct%acapN
      acapS                         => gridstruct%acapS
      globalarea                    => gridstruct%globalarea

      if (gridstruct%nested) then
         is2 = isd
         ie2 = ied
         js2 = jsd
         je2 = jed
      else
         is2 = is
         ie2 = ie
         js2 = js
         je2 = je
      end if

      pe(:,:,:) = 0.0
      pt(:,:,:) = 1.0
      f0(:,:) = huge(dummy)
      fC(:,:) = huge(dummy)
      do j=jsd,jed+1
         do i=isd,ied+1
            fC(i,j) = 2.*omega*( -1.*cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) + &
                                     sin(grid(i,j,2))*cos(alpha) )
         enddo
      enddo
      do j=jsd,jed
         do i=isd,ied
            f0(i,j) = 2.*omega*( -1.*cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) + &
                                     sin(agrid(i,j,2))*cos(alpha) )
         enddo
      enddo
      call mpp_update_domains( f0, domain )
      if (cubed_sphere) call fill_corners(f0, npx, npy, YDir)

      delp(isd:is-1,jsd:js-1,1:npz)=0.
      delp(isd:is-1,je+1:jed,1:npz)=0.
      delp(ie+1:ied,jsd:js-1,1:npz)=0.
      delp(ie+1:ied,je+1:jed,1:npz)=0.

#if defined(SW_DYNAMICS)
      select case (test_case)
      case(-2)
      case(-1)
         Ubar = (2.0*pi*radius)/(12.0*86400.0)
         gh0  = 2.94e4
         phis = 0.0
         do j=js,je
            do i=is,ie
               delp(i,j,1) = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(agrid(i  ,j  ,1))*cos(agrid(i  ,j  ,2))*sin(alpha) + &
                                   sin(agrid(i  ,j  ,2))*cos(alpha) ) ** 2.0
            enddo
         enddo
         call init_winds(UBar, u,v,ua,va,uc,vc, 1, npx, npy, ng, ndims, nregions, gridstruct%nested, gridstruct, domain, tile)

! Test Divergence operator at cell centers
         do j=js,je
            do i=is,ie
               divg(i,j) = (rarea(i,j)) * ( (uc(i+1,j,1)*dy(i+1,j) - uc(i,j,1)*dy(i,j)) + &
                                            (vc(i,j+1,1)*dx(i,j+1) - vc(i,j,1)*dx(i,j)) )
      if ( (tile==1) .and. (i==1) ) write(*,200) i,j,tile, divg(i,j), uc(i,j,1), uc(i+1,j,1), vc(i,j,1), vc(i,j+1,1)
            enddo
         enddo
! Test Vorticity operator at cell centers
         do j=js,je
            do i=is,ie
               vort(i,j) = (rarea(i,j)) * ( (v(i+1,j,1)*dy(i+1,j) - v(i,j,1)*dy(i,j)) - &
                                            (u(i,j+1,1)*dx(i,j+1) - u(i,j,1)*dx(i,j)) )
           enddo
        enddo
        div0(:,:) = 1.e-20
     ! call mpp_update_domains( div0, domain )
     ! call mpp_update_domains( vor0, domain )
     ! call mpp_update_domains( divg, domain )
     ! call mpp_update_domains( vort, domain )
      call get_scalar_stats( divg, div0, npx, npy, ndims, nregions, &
                             pmin, pmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
 200  format(i4.4,'x',i4.4,'x',i4.4,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)
 201  format('          ',A,e21.14,' ',e21.14)
 202  format('          ',A,i4.4,'x',i4.4,'x',i4.4)
      if ( is_master() ) then
          write(*,*) ' Error Norms of Analytical Divergence field C-Winds initialized'
          write(*,201) 'Divergence MAX error     : ', pmax
          write(*,201) 'Divergence MIN error     : ', pmin
          write(*,201) 'Divergence L1_norm       : ', L1_norm
          write(*,201) 'Divergence L2_norm       : ', L2_norm
          write(*,201) 'Divergence Linf_norm     : ', Linf_norm
      endif 

         call init_winds(UBar, u,v,ua,va,uc,vc, 3, npx, npy, ng, ndims, nregions, gridstruct%nested, gridstruct, domain, tile)
! Test Divergence operator at cell centers
         do j=js,je
            do i=is,ie
               divg(i,j) = (rarea(i,j)) * ( (uc(i+1,j,1)*dy(i+1,j) - uc(i,j,1)*dy(i,j)) + &
                                            (vc(i,j+1,1)*dx(i,j+1) - vc(i,j,1)*dx(i,j)) )
      if ( (tile==1) .and. (i==1) ) write(*,200) i,j,tile, divg(i,j), uc(i,j,1), uc(i+1,j,1), vc(i,j,1), vc(i,j+1,1)
            enddo
         enddo
! Test Vorticity operator at cell centers
         do j=js,je
            do i=is,ie
               vort(i,j) = (rarea(i,j)) * ( (v(i+1,j,1)*dy(i+1,j) - v(i,j,1)*dy(i,j)) - &
                                            (u(i,j+1,1)*dx(i,j+1) - u(i,j,1)*dx(i,j)) )
           enddo
        enddo
        ua0 = ua
        va0 = va
        div0(:,:) = 1.e-20
      call get_scalar_stats( divg, div0, npx, npy, ndims, nregions, &
                             pmin, pmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
      if ( is_master() ) then
          write(*,*) ' Error Norms of Analytical Divergence field A-Winds initialized'
          write(*,201) 'Divergence MAX error     : ', pmax
          write(*,201) 'Divergence MIN error     : ', pmin
          write(*,201) 'Divergence L1_norm       : ', L1_norm
          write(*,201) 'Divergence L2_norm       : ', L2_norm
          write(*,201) 'Divergence Linf_norm     : ', Linf_norm
      endif

         call init_winds(UBar, u,v,ua,va,uc,vc, 2, npx, npy, ng, ndims, nregions, gridstruct%nested, gridstruct, domain, tile)
         !call d2a2c(npx,npy,1, is,ie, js,je, ng, u(isd,jsd,1),v(isd,jsd,1), &
         !           ua(isd,jsd,1),va(isd,jsd,1), uc(isd,jsd,1),vc(isd,jsd,1))
! Test Divergence operator at cell centers
         do j=js,je
            do i=is,ie
               divg(i,j) = (rarea(i,j)) * ( (uc(i+1,j,1)*dy(i+1,j) - uc(i,j,1)*dy(i,j)) + &
                                            (vc(i,j+1,1)*dx(i,j+1) - vc(i,j,1)*dx(i,j)) )
      if ( (tile==1) .and. ((i==1) .or.(i==npx-1)) ) write(*,200) i,j,tile, divg(i,j), uc(i,j,1), uc(i+1,j,1), vc(i,j,1), vc(i,j+1,1)
            enddo
         enddo
! Test Vorticity operator at cell centers
         do j=js,je
            do i=is,ie
               vort(i,j) = (rarea(i,j)) * ( (v(i+1,j,1)*dy(i+1,j) - v(i,j,1)*dy(i,j)) - &
                                            (u(i,j+1,1)*dx(i,j+1) - u(i,j,1)*dx(i,j)) )
           enddo
        enddo
        div0(:,:) = 1.e-20
      call get_scalar_stats( divg, div0, npx, npy, ndims, nregions, &
                             pmin, pmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
      if ( is_master() ) then
          write(*,*) ' Error Norms of Analytical Divergence field D-Winds initialized'
          write(*,201) 'Divergence MAX error     : ', pmax
          write(*,201) 'Divergence MIN error     : ', pmin
          write(*,201) 'Divergence L1_norm       : ', L1_norm
          write(*,201) 'Divergence L2_norm       : ', L2_norm
          write(*,201) 'Divergence Linf_norm     : ', Linf_norm
      endif

      call mp_stop()
      stop
      case(0)
         do j=jsd,jed
            do i=isd,ied

               x1 = agrid(i,j,1) 
               y1 = agrid(i,j,2)
               z1 = radius

               p = p0_c0 * cos(y1)
               Vtx = ((3.0*SQRT(2.0))/2.0) * (( 1.0/cosh(p) )**2.0) * tanh(p)
               w_p = 0.0
               if (p /= 0.0) w_p = Vtx/p 
               delp(i,j,1) = 1.0 - tanh( (p/rgamma) * sin(x1 - w_p*0.0) )
               ua(i,j,1) = w_p*(sin(lat0)*cos(agrid(i,j,2)) + cos(lat0)*cos(agrid(i,j,1) - lon0)*sin(agrid(i,j,2)))
               va(i,j,1) = w_p*cos(lat0)*sin(agrid(i,j,1) - lon0)
               ua(i,j,1) = ua(i,j,1)*radius/86400.0
               va(i,j,1) = va(i,j,1)*radius/86400.0

               call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
               call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
               call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p3)
               call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p4)      
               if (cubed_sphere) call rotate_winds(ua(i,j,1),va(i,j,1), p1,p2,p3,p4, agrid(i,j,1:2), 2, 1)

            enddo
         enddo
         call mpp_update_domains( ua, va, domain, gridtype=AGRID_PARAM)
         call atod(ua,va, u, v,dxa, dya,dxc,dyc,npx,npy,ng, gridstruct%nested, domain)
         call mp_update_dwinds(u, v, npx, npy, npz, domain)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, gridstruct%nested, domain)
         call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE_PARAM)
         call fill_corners(uc, vc, npx, npy, npz, VECTOR=.true., CGRID=.true.)
         initWindsCase=initWindsCase0
      case(1)
         Ubar = (2.0*pi*radius)/(12.0*86400.0)
         gh0  = 1.0
         phis = 0.0
         r0 = radius/3. !RADIUS radius/3.
         p1(1) = pi/2. + pi_shift
         p1(2) = 0.
         do j=jsd,jed
            do i=isd,ied
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = great_circle_dist( p1, p2, radius )
               if (r < r0) then
                  delp(i,j,1) = phis(i,j) + gh0*0.5*(1.0+cos(PI*r/r0))
               else
                  delp(i,j,1) = phis(i,j)
               endif
            enddo
         enddo
         initWindsCase=initWindsCase1
      case(2)
#ifdef TEST_TRACER
!!$         do j=js2,je2
!!$         do i=is2,ie2
!!$            q(i,j,1,:) = 1.e-3*cos(agrid(i,j,2))!*(1.+cos(agrid(i,j,1)))
!!$         enddo
!!$         enddo
         gh0  = 1.0e-6
         r0 = radius/3. !RADIUS radius/3.
         p1(2) = 35./180.*pi !0.
         p1(1) = pi/4.!pi/2.
         do j=jsd,jed
         do i=isd,ied
            p2(1) = agrid(i,j,1)
            p2(2) = agrid(i,j,2)
            r = great_circle_dist( p1, p2, radius )
            if (r < r0 .and. .not.( abs(p1(2)-p2(2)) < 1./18. .and. p2(1)-p1(1) < 5./36.)) then
               !q(i,j,k,1) = max(gh0*0.5*(1.0+cos(PI*r/r0))*exp(real(k-npz)),0.)
               q(i,j,1,1) = gh0
            else
               q(i,j,1,1) = 0.
            endif
         enddo
         enddo
#endif
         Ubar = (2.0*pi*radius)/(12.0*86400.0)
         gh0  = 2.94e4
         phis = 0.0
         do j=js2,je2
            do i=is2,ie2
!         do j=jsd,jed
!            do i=isd,ied
#ifdef FIVE_AVG
               pt5 = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(agrid(i  ,j  ,1))*cos(agrid(i  ,j  ,2))*sin(alpha) + &
                                   sin(agrid(i  ,j  ,2))*cos(alpha) ) ** 2.0
               pt1 = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(grid(i  ,j  ,1))*cos(grid(i  ,j  ,2))*sin(alpha) + &
                                   sin(grid(i  ,j  ,2))*cos(alpha) ) ** 2.0
               pt2 = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(grid(i+1,j  ,1))*cos(grid(i+1,j  ,2))*sin(alpha) + &
                                   sin(grid(i+1,j  ,2))*cos(alpha) ) ** 2.0
               pt3 = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(grid(i+1,j+1,1))*cos(grid(i+1,j+1,2))*sin(alpha) + &
                                   sin(grid(i+1,j+1,2))*cos(alpha) ) ** 2.0
               pt4 = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(grid(i,j+1,1))*cos(grid(i,j+1,2))*sin(alpha) + &
                                   sin(grid(i,j+1,2))*cos(alpha) ) ** 2.0
               delp(i,j,1) = (0.25*(pt1+pt2+pt3+pt4) + 3.*pt5) / 4.
#else
               delp(i,j,1) = gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(agrid(i  ,j  ,1))*cos(agrid(i  ,j  ,2))*sin(alpha) + &
                                   sin(agrid(i  ,j  ,2))*cos(alpha) ) ** 2.0
#endif
            enddo
         enddo
         initWindsCase=initWindsCase2
      case(3)
!----------------------------
! Non-rotating potential flow
!----------------------------
#ifdef NO_WIND
         ubar = 0.
#else
         ubar = 40.
#endif
         gh0  = 1.0e3 * grav
         phis = 0.0
         r0 = radius/3. !RADIUS radius/3.
         p1(1) = pi*1.5
         p1(2) = 0.
         do j=jsd,jed
            do i=isd,ied
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = great_circle_dist( p1, p2, radius )
               if (r < r0) then
                  delp(i,j,1) = phis(i,j) + gh0*0.5*(1.0+cos(PI*r/r0))
               else
                  delp(i,j,1) = phis(i,j)
               endif
! Add a constant:
               delp(i,j,1) = delp(i,j,1) + grav*2.e3
            enddo
         enddo

#ifdef NO_WIND
         u  = 0.;   v = 0.
         f0 = 0.;  fC = 0.
#else

         do j=js,je
            do i=is,ie+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e2)
               call get_latlon_vector(p3, ex, ey)
               utmp = ubar * cos(p3(2))
               vtmp = 0.
               v(i,j,1) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,  j,1:2)
               p2(:) = grid(i+1,j,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e1)
               call get_latlon_vector(p3, ex, ey)
               utmp = ubar * cos(p3(2))
               vtmp = 0.
               u(i,j,1) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
            enddo
         enddo

         anti_rot = -ubar/ radius
         do j=jsd,jed+1
            do i=isd,ied+1
               fC(i,j) = 2.*anti_rot*sin(grid(i,j,2))
            enddo
         enddo
         do j=jsd,jed
            do i=isd,ied
               f0(i,j) = 2.*anti_rot*sin(agrid(i,j,2))
            enddo
         enddo
#endif
         initWindsCase= -1

      case(4)

!----------------------------
! Tropical cyclones
!----------------------------
!        f0 = 0.;  fC = 0.          ! non-rotating planet setup
          u = 0.
          v = 0.
         phis = 0.0                 ! flat terrain

         ubar = 50.                 ! maxmium wind speed (m/s)
           r0 = 250.e3              ! RADIUS of the maximum wind of the Rankine vortex
          gh0 = grav * 1.e3
 
        do j=jsd,jed
           do i=isd,ied
              delp(i,j,1) = gh0
           enddo
        enddo

!       ddeg = 2.*r0/radius     ! no merger
        ddeg = 1.80*r0/radius   ! merged 

        p1(1) = pi*1.5 - ddeg
        p1(2) = pi/18.              ! 10 N
        call rankine_vortex(ubar, r0, p1, u, v, grid)

        p2(1) = pi*1.5 + ddeg
        p2(2) = pi/18.              ! 10 N
        call rankine_vortex(ubar, r0, p2, u, v, grid)

#ifndef SINGULAR_VORTEX
!-----------
! Anti-pole:
!-----------
        ubar = -ubar
        call latlon2xyz(p1, e1)
        do i=1,3
           e1(i) = -e1(i)
        enddo
        call cart_to_latlon(1, e1, p3(1), p3(2))
        call rankine_vortex(ubar, r0, p3, u, v, grid)

        call latlon2xyz(p2, e1)
        do i=1,3
           e1(i) = -e1(i)
        enddo
        call cart_to_latlon(1, e1, p4(1), p4(2))
        call rankine_vortex(ubar, r0, p4, u, v, grid)
#endif
        call mp_update_dwinds(u, v, npx, npy, npz, domain)
        initWindsCase=-1   ! do nothing

      case(5)

         Ubar = 20.        
         gh0  = 5960.*Grav
         phis = 0.0
         r0 = PI/9.
         p1(1) = PI/2.
         p1(2) = PI/6.
         do j=js2,je2
            do i=is2,ie2
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = MIN(r0*r0, (p2(1)-p1(1))*(p2(1)-p1(1)) + (p2(2)-p1(2))*(p2(2)-p1(2)) )
               r = SQRT(r)
               phis(i,j) = 2000.0*Grav*(1.0-(r/r0))
            enddo
         enddo
         do j=js2,je2
            do i=is2,ie2
               delp(i,j,1) =gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                             ( -1.*cos(agrid(i  ,j  ,1))*cos(agrid(i  ,j  ,2))*sin(alpha) + &
                                   sin(agrid(i  ,j  ,2))*cos(alpha) ) ** 2  - phis(i,j)
            enddo
         enddo
         initWindsCase=initWindsCase5
      case(6)
         gh0  = 8.E3*Grav
         R    = 4.
         omg  = 7.848E-6
         rk    = 7.848E-6
         phis = 0.0
         do j=js,je
            do i=is,ie
               A = 0.5*omg*(2.*omega+omg)*(COS(agrid(i,j,2))**2) + &
                   0.25*rk*rk*(COS(agrid(i,j,2))**(r+r)) * &
                   ( (r+1)*(COS(agrid(i,j,2))**2) + (2.*r*r-r-2.) - &
                     2.*(r*r)*COS(agrid(i,j,2))**(-2.) )
               B = (2.*(omega+omg)*rk / ((r+1)*(r+2))) * (COS(agrid(i,j,2))**r) * &
                    ( (r*r+2.*r+2.) - ((r+1.)*COS(agrid(i,j,2)))**2 )
               C = 0.25*rk*rk*(COS(agrid(i,j,2))**(2.*r)) * ( &
                   (r+1) * (COS(agrid(i,j,2))**2.) - (r+2.) )
               delp(i,j,1) =gh0 + radius*radius*(A+B*COS(r*agrid(i,j,1))+C*COS(2.*r*agrid(i,j,1)))
               delp(i,j,1) = delp(i,j,1) - phis(i,j)
            enddo
         enddo
         do j=js,je
            do i=is,ie+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e2)
               call get_latlon_vector(p3, ex, ey)
               utmp = radius*omg*cos(p3(2)) +                      &
                      radius*rk*(cos(p3(2))**(R-1))*(R*sin(p3(2))**2-cos(p3(2))**2)*cos(R*p3(1)) 
               vtmp = -radius*rk*R*sin(p3(2))*sin(R*p3(1))*cos(p3(2))**(R-1)
               v(i,j,1) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,  j,1:2)
               p2(:) = grid(i+1,j,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e1)
               call get_latlon_vector(p3, ex, ey)
               utmp = radius*omg*cos(p3(2)) +                      &
                      radius*rk*(cos(p3(2))**(R-1))*(R*sin(p3(2))**2-cos(p3(2))**2)*cos(R*p3(1)) 
               vtmp = -radius*rk*R*sin(p3(2))*sin(R*p3(1))*cos(p3(2))**(R-1)
               u(i,j,1) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
            enddo
         enddo
         call mp_update_dwinds(u, v, npx, npy, npz, domain)
         call dtoa( u, v,ua,va,dx,dy,dxa,dya,dxc,dyc,npx,npy,ng)
         !call mpp_update_domains( ua, va, domain, gridtype=AGRID_PARAM)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, gridstruct%nested, domain)
         initWindsCase=initWindsCase6
      case(7)
! Barotropically unstable jet
         gh0  = 10.E3*Grav
         phis = 0.0
         r0 = radius/12.
         p2(1) = pi/2.
         p2(2) = pi/4.
         do j=js,je
            do i=is,ie
!              ftmp = gh0
! 9-point average:
!      9  4  8
!
!      5  1  3
!          
!      6  2  7
               pt1 = gh_jet(npy, agrid(i,j,2))
               call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), pa)
               pt2 = gh_jet(npy, pa(2))
               call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), pa)
               pt3 = gh_jet(npy, pa(2))
               call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), pa)
               pt4 = gh_jet(npy, pa(2))
               call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), pa)
               pt5 = gh_jet(npy, pa(2))
               pt6 = gh_jet(npy, grid(i,  j,  2))
               pt7 = gh_jet(npy, grid(i+1,j,  2))
               pt8 = gh_jet(npy, grid(i+1,j+1,2))
               pt9 = gh_jet(npy, grid(i  ,j+1,2))
               ftmp = 0.25*pt1 + 0.125*(pt2+pt3+pt4+pt5) + 0.0625*(pt6+pt7+pt8+pt9)
#ifndef NEW_PERT
               delp(i,j,1) = ftmp + 120.*grav*cos(agrid(i,j,2)) *  &
               exp( -(3.*(agrid(i,j,1)-pi))**2 ) * exp( -(15.*(agrid(i,j,2)-pi/4.))**2 )
!              phis(i,j) = ftmp
!              delp(i,j,1) = 10.E3*grav + 120.*grav*cos(agrid(i,j,2)) *  &
!              exp( -(3.*(agrid(i,j,1)-pi))**2 ) * exp( -(15.*(agrid(i,j,2)-pi/4.))**2 )
#else
! Using great circle dist:
               p1(:) = agrid(i,j,1:2)
               delp(i,j,1) = ftmp
               r = great_circle_dist(p1, p2, radius)
               if ( r < 3.*r0 ) then
                    delp(i,j,1) = delp(i,j,1) + 1000.*grav*exp(-(r/r0)**2)
               endif
#endif
            enddo
         enddo

! v-wind:
         do j=js,je
            do i=is,ie+1
               p2(:) = grid(i,j+1,1:2)
               vv1 = u_jet(p2(2))*(ee2(2,i,j+1)*cos(p2(1)) - ee2(1,i,j+1)*sin(p2(1)))
               p1(:) = grid(i,j,1:2)
               vv3 = u_jet(p1(2))*(ee2(2,i,j)*cos(p1(1)) - ee2(1,i,j)*sin(p1(1)))
! Mid-point:
               call mid_pt_sphere(p1, p2, pa)
               vv2 = u_jet(pa(2))*(ew(2,i,j,2)*cos(pa(1)) - ew(1,i,j,2)*sin(pa(1)))
! 3-point average:
               v(i,j,1) = 0.25*(vv1 + 2.*vv2 + vv3)
!              v(i,j,1) = vv2
            enddo
         enddo
! U-wind:
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,j,1:2)
               uu1 = u_jet(p1(2))*(ee1(2,i,j)*cos(p1(1)) - ee1(1,i,j)*sin(p1(1)))
               p2(:) = grid(i+1,j,1:2)
               uu3 = u_jet(p2(2))*(ee1(2,i+1,j)*cos(p2(1)) - ee1(1,i+1,j)*sin(p2(1)))
! Mid-point:
               call mid_pt_sphere(p1, p2, pa)
               uu2 = u_jet(pa(2))*(es(2,i,j,1)*cos(pa(1)) - es(1,i,j,1)*sin(pa(1)))
! 3-point average:
               u(i,j,1) = 0.25*(uu1 + 2.*uu2 + uu3)
!              u(i,j,1) = uu2
            enddo
         enddo
         initWindsCase=initWindsCase6  ! shouldn't do anything with this
!initialize tracer with shallow-water PV 
         !Compute vorticity
         call get_vorticity(is, ie, js, je, isd, ied, jsd, jed, npz, u, v, q(is:ie,js:je,:,1), dx, dy, rarea)
         do j=jsd,jed+1
            do i=isd,ied+1
               fC(i,j) = 2.*omega*( -1.*cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) + &
                    sin(grid(i,j,2))*cos(alpha) )
            enddo
         enddo
         do j=jsd,jed
            do i=isd,ied
               f0(i,j) = 2.*omega*( -1.*cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) + &
                    sin(agrid(i,j,2))*cos(alpha) )
            enddo
         enddo
         call mpp_update_domains( f0, domain )
         if (cubed_sphere) call fill_corners(f0, npx, npy, YDir)
         do j=js,je
         do i=is,ie
            q(i,j,npz,1) = ( q(i,j,npz,1) + f0(i,j) ) / delp(i,j,npz) * 1.e6 ! PVU
            !q(i,j,npz,1) = ( q(i,j,npz,1) + f0(i,j) ) * grav / delp(i,j,npz)
         enddo
         enddo
!         call pv_entropy(is, ie, js, je, ng, npz, q(is:ie,js:je,:,2), f0, pt, pkz, delp, grav)

      case(8)
#ifdef USE_OLD
!----------------------------
! Non-rotating potential flow
!----------------------------
         gh0  = 5960.*Grav
         phis = 0.0
         r0 = PI/9.
         p1(1) = PI/2.
         p1(2) = PI/6.
         do j=js,je
            do i=is,ie
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = MIN(r0*r0, (p2(1)-p1(1))*(p2(1)-p1(1)) + (p2(2)-p1(2))*(p2(2)-p1(2)) )
               r = SQRT(r)
               phis(i,j) = 2000.0*Grav*(1.0-(r/r0))
            enddo
         enddo
         do j=js,je
            do i=is,ie
               delp(i,j,1) = gh0
            enddo
         enddo
         u  = 0.;   v = 0.
         f0 = 0.;  fC = 0.
         initWindsCase= -1
#endif
!----------------------------
! Soliton twin-vortex
!----------------------------
        if ( is_master() ) write(*,*) 'Initialzing case-8: soliton twin cycolne...'
        f0 = 0.;  fC = 0.          ! non-rotating planet setup
        phis = 0.0                 ! flat terrain
        gh0  = 5.E3*Grav
        do j=js,je
           do i=is,ie
              delp(i,j,1) = gh0
           enddo
        enddo

! Initiate the westerly-wind-burst:
        ubar = soliton_Umax
        r0   = soliton_size
!!$        ubar = 200.       ! maxmium wind speed (m/s)
!!$        r0 = 250.e3
!!$        ubar = 50.       ! maxmium wind speed (m/s)
!!$        r0 = 750.e3
! #1 1: westerly
        p0(1) = pi*0.5
        p0(2) = 0.

        do j=js,je
           do i=is,ie+1
              p1(:) = grid(i  ,j ,1:2)
              p2(:) = grid(i,j+1 ,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e2)
              call get_latlon_vector(p3, ex, ey)
              v(i,j,1) = utmp*inner_prod(e2,ex)
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              p1(:) = grid(i,  j,1:2)
              p2(:) = grid(i+1,j,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e1)
              call get_latlon_vector(p3, ex, ey)
              u(i,j,1) = utmp*inner_prod(e1,ex)
           enddo
        enddo

! #1 2: easterly
        p0(1) = p0(1) + pi
        p0(2) = 0.

        do j=js,je
           do i=is,ie+1
              p1(:) = grid(i  ,j ,1:2)
              p2(:) = grid(i,j+1 ,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e2)
              call get_latlon_vector(p3, ex, ey)
              v(i,j,1) = v(i,j,1) - utmp*inner_prod(e2,ex)
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              p1(:) = grid(i,  j,1:2)
              p2(:) = grid(i+1,j,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e1)
              call get_latlon_vector(p3, ex, ey)
              u(i,j,1) = u(i,j,1) - utmp*inner_prod(e1,ex)
           enddo
        enddo
         initWindsCase= -1

      case(9)
#ifdef USE_OLD
         jm1 = jm - 1
         DDP = PI/DBLE(jm1)
         DP  = DDP
         ll_j(1) = -0.5*PI
         do j=2,jm
            ph5  = -0.5*PI + (DBLE(j-1)-0.5)*DDP
            ll_j(j) = -0.5*PI + (DBLE(j-1)*DDP)
            sine(j) = SIN(ph5)
         enddo
         cosp( 1) =  0.
         cosp(jm) =  0.
         do j=2,jm1
            cosp(j) = (sine(j+1)-sine(j)) / DP
         enddo
         do j=2,jm
            cose(j) = 0.5 * (cosp(j-1) + cosp(j))
         enddo
         cose(1) = cose(2)
         ddeg = 180./float(jm-1)
         do j=2,jm
            deg = -90. + (float(j-1)-0.5)*ddeg
            if (deg <= 0.) then
               ll_u(j) = -10.*(deg+90.)/90.
            elseif (deg <= 60.) then
               ll_u(j) = -10. +  deg
            else
               ll_u(j) = 50. - (50./30.)* (deg - 60.)
            endif
         enddo
         ll_phi(1) = 6000. * Grav
         do j=2,jm1
            ll_phi(j)=ll_phi(j-1)  - DP*sine(j) * &
                    (radius*2.*omega + ll_u(j)/cose(j))*ll_u(j)
         enddo
         phis = 0.0
         do j=js,je
            do i=is,ie
               do jj=1,jm1
                  if ( (ll_j(jj) <= agrid(i,j,2)) .and. (agrid(i,j,2) <= ll_j(jj+1)) ) then
                     delp(i,j,1)=0.5*(ll_phi(jj)+ll_phi(jj+1))
                  endif
               enddo
            enddo
         enddo

         do j=js,je
            do i=is,ie
               if (agrid(i,j,2)*todeg <= 0.0) then
                  ua(i,j,1) = -10.*(agrid(i,j,2)*todeg + 90.)/90.
               elseif (agrid(i,j,2)*todeg <= 60.0) then
                  ua(i,j,1) = -10. + agrid(i,j,2)*todeg
               else
                  ua(i,j,1) = 50. - (50./30.)* (agrid(i,j,2)*todeg - 60.)
               endif
               va(i,j,1) = 0.0
               call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
               call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
               call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p3)
               call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p4)
               if (cubed_sphere) call rotate_winds(ua(i,j,1), va(i,j,1), p1,p2,p3,p4, agrid(i,j,1:2), 2, 1)
            enddo
         enddo

         call mpp_update_domains( ua, va, domain, gridtype=AGRID_PARAM)
         call atoc(ua,va,uc,vc,dx,dy,dxa,dya,npx,npy,ng, gridstruct%nested, domain)
         call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE_PARAM)
         call fill_corners(uc, vc, npx, npy, npz, VECTOR=.true., CGRID=.true.)
         call atod(ua,va, u, v,dxa, dya,dxc,dyc,npx,npy,ng, gridstruct%nested, domain)
         call mp_update_dwinds(u, v, npx, npy, npz, domain)
         initWindsCase=initWindsCase9


         call get_case9_B(case9_B, agrid)
         AofT(:) = 0.0
#else
!----------------------------
! Soliton twin-vortex
!----------------------------
        if ( is_master() ) write(*,*) 'Initialzing case-9: soliton cyclones...'
        f0 = 0.;  fC = 0.          ! non-rotating planet setup
        phis = 0.0                 ! flat terrain
        gh0  = 5.E3*Grav
        do j=js,je
           do i=is,ie
              delp(i,j,1) = gh0
           enddo
        enddo

! Initiate the westerly-wind-burst:
        ubar = soliton_Umax
        r0   = soliton_size
!!$        ubar = 200.       ! maxmium wind speed (m/s)
!!$        r0 = 250.e3
!!$        ubar = 50.       ! maxmium wind speed (m/s)
!!$        r0 = 750.e3
        p0(1) = pi*0.5
        p0(2) = 0.

        do j=js,je
           do i=is,ie+1
              p1(:) = grid(i  ,j ,1:2)
              p2(:) = grid(i,j+1 ,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e2)
              call get_latlon_vector(p3, ex, ey)
              v(i,j,1) = utmp*inner_prod(e2,ex)
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              p1(:) = grid(i,  j,1:2)
              p2(:) = grid(i+1,j,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e1)
              call get_latlon_vector(p3, ex, ey)
              u(i,j,1) = utmp*inner_prod(e1,ex)
           enddo
        enddo
         initWindsCase= -1
#endif
      end select
!--------------- end s-w cases --------------------------

! Copy 3D data for Shallow Water Tests
      do z=2,npz
         delp(:,:,z) = delp(:,:,1)
      enddo

      call mpp_update_domains( delp, domain )
      call mpp_update_domains( phis, domain )
      phi0  = delp

      call init_winds(UBar, u,v,ua,va,uc,vc, initWindsCase, npx, npy, ng, ndims, nregions, gridstruct%nested, gridstruct, domain, tile)
! Copy 3D data for Shallow Water Tests
      do z=2,npz
         u(:,:,z) = u(:,:,1)
         v(:,:,z) = v(:,:,1)
      enddo

      do j=js,je
         do i=is,ie
            ps(i,j) = delp(i,j,1)
         enddo
      enddo
! -------- end s-w section ----------------------------------
#else

      if (test_case==10 .or. test_case==14) then

         alpha = 0.

   ! Initialize dry atmosphere
         q(:,:,:,:) = 3.e-6
         u(:,:,:) = 0.0
         v(:,:,:) = 0.0
         if (.not.hydrostatic) w(:,:,:)= 0.0

       if ( test_case==14 ) then
! Aqua-planet case: mean SLP=1.E5
         phis = 0.0
         call hydro_eq(npz, is, ie, js, je, ps, phis, 1.E5,      &
                       delp, ak, bk, pt, delz, area, ng, .false., hydrostatic, hybrid_z, domain)
       else
! Initialize topography
         gh0  = 5960.*Grav
         phis = 0.0
         r0 = PI/9.
         p1(1) = PI/4.
         p1(2) = PI/6. + (7.5/180.0)*PI
         do j=js2,je2
            do i=is2,ie2
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = MIN(r0*r0, (p2(1)-p1(1))*(p2(1)-p1(1)) + (p2(2)-p1(2))*(p2(2)-p1(2)) )
               r = SQRT(r)
               phis(i,j) = gh0*(1.0-(r/r0))
            enddo
         enddo
         call hydro_eq(npz, is, ie, js, je, ps, phis, dry_mass,  &
                       delp, ak, bk, pt, delz, area, ng, mountain, hydrostatic, hybrid_z, domain)
       endif

      else if (test_case==11) then
       call surfdrv(npx, npy, gridstruct%grid_64, gridstruct%agrid_64,   &
                    gridstruct%area_64, dx, dy, dxa, dya, dxc, dyc, &
                    gridstruct%sin_sg, phis, &
                    flagstruct%stretch_fac, gridstruct%nested, &
                    npx_global, domain, flagstruct%grid_number, bd)
       call mpp_update_domains( phis, domain )

       if ( hybrid_z ) then
            rgrav = 1./ grav
            if( npz==32 ) then
                call compute_dz_L32( npz, ztop, dz1 )
            else
!               call mpp_error(FATAL, 'You must provide a routine for hybrid_z')
                if ( is_master() ) write(*,*) 'Using const DZ'
                ztop = 45.E3           ! assuming ptop = 100.
                dz1(1) = ztop / real(npz) 
                dz1(npz) = 0.5*dz1(1)
                do z=2,npz-1
                   dz1(z) = dz1(1)
                enddo
                dz1(1) = 2.*dz1(2)
            endif

            call set_hybrid_z(is, ie, js, je, ng, npz, ztop, dz1, rgrav,  &
                              phis, ze0, delz)
!           call prt_maxmin('ZE0', ze0,  is, ie, js, je, 0, npz, 1.E-3)
!           call prt_maxmin('DZ0', delz, is, ie, js, je, 0, npz, 1.   )
       endif

! Initialize dry atmosphere
       u = 0.
       v = 0.
       q(:,:,:,:) = 0.
       q(:,:,:,1) = 3.e-6

       call hydro_eq(npz, is, ie, js, je, ps, phis, dry_mass,  &
                     delp, ak, bk, pt, delz, area, ng, mountain, hydrostatic, hybrid_z, domain)

      else if ( (test_case==12) .or. (test_case==13) ) then

#ifdef HIWPP_TRACER
         if (is_master()) print*, 'TEST TRACER enabled for this test case'
#ifdef HIWPP
         call checker_tracers(is,ie, js,je, isd,ied, jsd,jed,  &
                              ncnst, npz, q, agrid(is:ie,js:je,1), agrid(is:ie,js:je,2), 9., 9.)
#else
              !For consistency with earlier single-grid simulations use gh0 = 1.0e-6 and p1(1) = 195.*pi/180. 
                 q(:,:,:,:) = 0.
                 gh0  = 1.0e-3
                 r0 = radius/3. !RADIUS radius/3.
                 p1(2) = 51.*pi/180.
                 p1(1) = 205.*pi/180. !231.*pi/180.
                 do k=1,npz
                 do j=jsd,jed
                 do i=isd,ied
                    p2(1) = agrid(i,j,1)
                    p2(2) = agrid(i,j,2)
                    r = great_circle_dist( p1, p2, radius )
                    if (r < r0 .and. .not.( abs(p1(2)-p2(2)) < 1./18. .and. p2(1)-p1(1) < 5./36.) .and. k > 16) then
                       q(i,j,k,1) = gh0
                    else
                       q(i,j,k,1) = 0.
                    endif
                 enddo
                 enddo
                 enddo
#endif
              
#else

         q(:,:,:,:) = 0.

#ifdef HIWPP

   cl = get_tracer_index(MODEL_ATMOS, 'cl')
   cl2 = get_tracer_index(MODEL_ATMOS, 'cl2')
   if (cl > 0 .and. cl2 > 0) then
      call terminator_tracers(is,ie,js,je,isd,ied,jsd,jed,npz, &
           q, delp,ncnst,agrid(isd:ied,jsd:jed,1),agrid(isd:ied,jsd:jed,2))
      call mpp_update_domains(q,domain)
   endif

#endif
#endif
    ! Initialize surface Pressure
         ps(:,:) = 1.e5
    ! Initialize detla-P
!$OMP parallel do default(none) shared(is,ie,js,je,npz,delp,ak,ps,bk)
         do z=1,npz
            do j=js,je
               do i=is,ie
                  delp(i,j,z) = ak(z+1)-ak(z) + ps(i,j)*(bk(z+1)-bk(z))
               enddo
            enddo
         enddo

!$OMP parallel do default(none) shared(is,ie,js,je,npz,pe,ptop,peln,pk,delp)
     do j=js,je
        do i=is, ie
           pe(i,1,j) = ptop
           peln(i,1,j) = log(ptop)
           pk(i,j,1) = ptop**kappa
        enddo
! Top down
        do k=2,npz+1
          do i=is,ie
             pe(i,k,j)  = pe(i,k-1,j) + delp(i,j,k-1)
             pk(i,j,k) = exp( kappa*log(pe(i,k,j)) )
             peln(i,k,j) = log(pe(i,k,j)) 
          enddo
        enddo
    enddo

!$OMP parallel do default(none) shared(is,ie,js,je,npz,pkz,pk,peln)
    do k=1,npz
       do j=js,je
       do i=is,ie
          pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
       enddo
       enddo
    enddo

    ! Setup ETA auxil variable
         eta_0 = 0.252
         do k=1,npz
            eta(k) = 0.5*( (ak(k)+ak(k+1))/1.e5 + bk(k)+bk(k+1) )
            eta_v(k) = (eta(k) - eta_0)*PI*0.5
         enddo

    if ( .not. adiabatic ) then
    !Set up moisture
         sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
         pcen(1) = PI/9.
         pcen(2) = 2.0*PI/9. 
!$OMP parallel do default(none) shared(sphum,is,ie,js,je,npz,pe,q,agrid,pcen,delp,peln) &
!$OMP                          private(ptmp) 
         do k=1,npz
         do j=js,je
         do i=is,ie
            !r = great_circle_dist(pcen, agrid(i,j,:), radius)
            !ptmp = 0.5*(pe(i,k,j)+pe(i,k+1,j)) - 100000.
            !q(i,j,k,1) = 0.021*exp(-(agrid(i,j,2)/pcen(2))**4.)*exp(-(ptmp/34000.)**2.)
            ptmp = delp(i,j,k)/(peln(i,k+1,j)-peln(i,k,j)) - 100000.
            q(i,j,k,sphum) = 0.021*exp(-(agrid(i,j,2)/pcen(2))**4.)*exp(-(ptmp/34000.)**2.)
! SJL:
!           q(i,j,k,sphum) = max(1.e-25, q(i,j,k,sphum))
         enddo
         enddo
         enddo
    endif

    ! Initialize winds 
         Ubar = 35.0
         r0 = 1.0
         pcen(1) = PI/9.
         pcen(2) = 2.0*PI/9. 
         if (test_case == 13) then
#ifdef ALT_PERT
             u1 = 0.0
            pt0 = 3.0
#else
             u1 = 1.0
            pt0 = 0.0
#endif
             r0 = radius/10.0
         endif

!$OMP parallel do default(none) shared(is,ie,js,je,npz,eta_v,grid,Ubar,pcen,r0,ee2,v,ee1,es,u,u1,ew) &
!$OMP                          private(utmp,r,vv1,vv3,p1,p2,vv2,uu1,uu2,uu3,pa)
         do z=1,npz
            do j=js,je
               do i=is,ie+1
                  utmp =  Ubar * COS(eta_v(z))**(3.0/2.0) * SIN(2.0*grid(i,j+1,2))**2.0
             ! Perturbation if Case==13
                  r = great_circle_dist( pcen, grid(i,j+1,1:2), radius )
                  if (-(r/r0)**2.0 > -40.0) utmp = utmp + u1*EXP(-(r/r0)**2.0) 
                  vv1 = utmp*(ee2(2,i,j+1)*cos(grid(i,j+1,1)) - ee2(1,i,j+1)*sin(grid(i,j+1,1)))

                  utmp =  Ubar * COS(eta_v(z))**(3.0/2.0) * SIN(2.0*grid(i,j,2))**2.0
             ! Perturbation if Case==13
                  r = great_circle_dist( pcen, grid(i,j,1:2), radius )
                  if (-(r/r0)**2.0 > -40.0) utmp = utmp + u1*EXP(-(r/r0)**2.0) 
                  vv3 = utmp*(ee2(2,i,j)*cos(grid(i,j,1)) - ee2(1,i,j)*sin(grid(i,j,1)))
! Mid-point:
                  p1(:) = grid(i  ,j ,1:2)
                  p2(:) = grid(i,j+1 ,1:2)
                  call mid_pt_sphere(p1, p2, pa)
                  utmp =  Ubar * COS(eta_v(z))**(3.0/2.0) * SIN(2.0*pa(2))**2.0
             ! Perturbation if Case==13
                  r = great_circle_dist( pcen, pa, radius )
                  if (-(r/r0)**2.0 > -40.0) utmp = utmp + u1*EXP(-(r/r0)**2.0) 
                  vv2 = utmp*(ew(2,i,j,2)*cos(pa(1)) - ew(1,i,j,2)*sin(pa(1)))
! 3-point average:
                  v(i,j,z) = 0.25*(vv1 + 2.*vv2 + vv3)
               enddo
            enddo
            do j=js,je+1
               do i=is,ie
                  utmp =  Ubar * COS(eta_v(z))**(3.0/2.0) * SIN(2.0*grid(i,j,2))**2.0
             ! Perturbation if Case==13
                  r = great_circle_dist( pcen, grid(i,j,1:2), radius )
                  if (-(r/r0)**2.0 > -40.0) utmp = utmp + u1*EXP(-(r/r0)**2.0)
                  uu1 = utmp*(ee1(2,i,j)*cos(grid(i,j,1)) - ee1(1,i,j)*sin(grid(i,j,1)))

                  utmp =  Ubar * COS(eta_v(z))**(3.0/2.0) * SIN(2.0*grid(i+1,j,2))**2.0
             ! Perturbation if Case==13
                  r = great_circle_dist( pcen, grid(i+1,j,1:2), radius )
                  if (-(r/r0)**2.0 > -40.0) utmp = utmp + u1*EXP(-(r/r0)**2.0)
                  uu3 = utmp*(ee1(2,i+1,j)*cos(grid(i+1,j,1)) - ee1(1,i+1,j)*sin(grid(i+1,j,1)))
! Mid-point:
                  p1(:) = grid(i  ,j  ,1:2)
                  p2(:) = grid(i+1,j  ,1:2)
                  call mid_pt_sphere(p1, p2, pa)
                  utmp =  Ubar * COS(eta_v(z))**(3.0/2.0) * SIN(2.0*pa(2))**2.0
             ! Perturbation if Case==13
                  r = great_circle_dist( pcen, pa, radius )
                  if (-(r/r0)**2.0 > -40.0) utmp = utmp + u1*EXP(-(r/r0)**2.0)
                  uu2 = utmp*(es(2,i,j,1)*cos(pa(1)) - es(1,i,j,1)*sin(pa(1)))
! 3-point average:
                  u(i,j,z) = 0.25*(uu1 + 2.*uu2 + uu3)
               enddo
            enddo
         enddo  ! z-loop

    ! Temperature
         eta_s = 1.0 ! Surface Level
         eta_t = 0.2 ! Tropopause
         T_0 = 288.0
         delta_T = 480000.0
         lapse_rate = 0.005
!$OMP parallel do default(none) shared(is,ie,js,je,npz,eta,ak,bk,T_0,lapse_rate,eta_t, &
!$OMP                                  delta_T,ptop,delp,Ubar,eta_v,agrid,grid,pcen,pt,r0) &
!$OMP                          private(T_mean,press,pt1,pt2,pt3,pt4,pt5,pt6,pt7,pt8,pt9,p1,r)
         do z=1,npz
            eta(z) = 0.5*( (ak(z)+ak(z+1))/1.e5 + bk(z)+bk(z+1) )
        !   if (is_master()) print*, z, eta
            T_mean = T_0 * eta(z)**(RDGAS*lapse_rate/Grav)
            if (eta_t > eta(z)) T_mean = T_mean + delta_T*(eta_t - eta(z))**5.0

 230  format(i4.4,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)
            press = ptop
            do zz=1,z
               press = press + delp(is,js,zz)
            enddo
            if (is_master()) write(*,230) z, eta(z), press/100., T_mean
            do j=js,je
               do i=is,ie
! A-grid cell center: i,j
                  pt1 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(agrid(i,j,2))**6.0) *(COS(agrid(i,j,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(agrid(i,j,2))**3.0)*(SIN(agrid(i,j,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
#ifndef NO_AVG13
! 9-point average: should be 2nd order accurate for a rectangular cell
!
!      9  4  8
!
!      5  1  3
!          
!      6  2  7
!
                  call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p1)
                  pt2 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p1)
                  pt3 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p1)
                  pt4 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
                  pt5 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )

                  pt6 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(grid(i,j,2))**6.0) *(COS(grid(i,j,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i,j,2))**3.0)*(SIN(grid(i,j,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  pt7 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(grid(i+1,j,2))**6.0) *(COS(grid(i+1,j,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i+1,j,2))**3.0)*(SIN(grid(i+1,j,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  pt8 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(grid(i+1,j+1,2))**6.0) *(COS(grid(i+1,j+1,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i+1,j+1,2))**3.0)*(SIN(grid(i+1,j+1,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  pt9 = T_mean + 0.75*(eta(z)*PI*Ubar/RDGAS)*SIN(eta_v(z))*SQRT(COS(eta_v(z))) * ( &
                              ( -2.0*(SIN(grid(i,j+1,2))**6.0) *(COS(grid(i,j+1,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              2.0*Ubar*COS(eta_v(z))**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i,j+1,2))**3.0)*(SIN(grid(i,j+1,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
                  pt(i,j,z) = 0.25*pt1 + 0.125*(pt2+pt3+pt4+pt5) + 0.0625*(pt6+pt7+pt8+pt9)
#else
                  pt(i,j,z) = pt1
#endif

#ifdef ALT_PERT
                  r = great_circle_dist( pcen, agrid(i,j,1:2), radius )
                  if ( (r/r0)**2 < 40. ) then
                        pt(i,j,z) = pt(i,j,z) + pt0*exp(-(r/r0)**2)
                  endif
#endif
                  
               enddo
            enddo
         enddo
         if (is_master()) print*,' '
      ! Surface Geopotential
         phis(:,:)=1.e25
!$OMP parallel do default(none) shared(is2,ie2,js2,je2,Ubar,eta_s,eta_0,agrid,grid,phis) &
!$OMP                         private(pt1,pt2,pt3,pt4,pt5,pt6,pt7,pt8,pt9,p1)
         do j=js2,je2
            do i=is2,ie2
               pt1 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                              ( -2.0*(SIN(agrid(i,j,2))**6.0) *(COS(agrid(i,j,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(agrid(i,j,2))**3.0)*(SIN(agrid(i,j,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
#ifndef NO_AVG13
! 9-point average:
!
!      9  4  8
!
!      5  1  3
!          
!      6  2  7
!
               call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p1)
               pt2 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                           ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                             Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p1)
               pt3 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                           ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                             Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p1)
               pt4 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                           ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                             Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
               pt5 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                           ( -2.0*(SIN(p1(2))**6.0) *(COS(p1(2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                             Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(p1(2))**3.0)*(SIN(p1(2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )

               pt6 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                              ( -2.0*(SIN(grid(i,j,2))**6.0) *(COS(grid(i,j,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i,j,2))**3.0)*(SIN(grid(i,j,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               pt7 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                              ( -2.0*(SIN(grid(i+1,j,2))**6.0) *(COS(grid(i+1,j,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i+1,j,2))**3.0)*(SIN(grid(i+1,j,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               pt8 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                              ( -2.0*(SIN(grid(i+1,j+1,2))**6.0) *(COS(grid(i+1,j+1,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i+1,j+1,2))**3.0)*(SIN(grid(i+1,j+1,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               pt9 = Ubar* (COS( (eta_s-eta_0)*PI/2.0 ))**(3.0/2.0) * ( &
                              ( -2.0*(SIN(grid(i,j+1,2))**6.0) *(COS(grid(i,j+1,2))**2.0 + 1.0/3.0) + 10.0/63.0 ) * &
                              Ubar*COS( (eta_s-eta_0)*PI/2.0 )**(3.0/2.0) + &
                              ( (8.0/5.0)*(COS(grid(i,j+1,2))**3.0)*(SIN(grid(i,j+1,2))**2.0 + 2.0/3.0) - PI/4.0 )*radius*omega )
               phis(i,j) = 0.25*pt1 + 0.125*(pt2+pt3+pt4+pt5) + 0.0625*(pt6+pt7+pt8+pt9)
#else
               phis(i,j) = pt1
#endif
            enddo
         enddo

         if ( .not.hydrostatic ) then
!$OMP parallel do default(none) shared(is,ie,js,je,npz,pt,delz,peln,w)
            do k=1,npz
            do j=js,je
            do i=is,ie
               w(i,j,k) = 0.
               delz(i,j,k) = rdgas/grav*pt(i,j,k)*(peln(i,k,j)-peln(i,k+1,j))
            enddo
            enddo
            enddo
         endif
            !Assume pt is virtual temperature at this point; then convert to regular temperature
         if (.not. adiabatic) then
            zvir = rvgas/rdgas - 1.
!$OMP parallel do default(none) shared(sphum,is,ie,js,je,npz,pt,zvir,q)
            do k=1,npz
            do j=js,je
            do i=is,ie
               pt(i,j,k) = pt(i,j,k)/(1. + zvir*q(i,j,k,sphum))
            enddo
            enddo
            enddo
         endif

         !Set up tracer #2 to be the initial EPV
!         call get_vorticity(is, ie, js, je, isd, ied, jsd, jed, npz, u, v, q(is:ie,js:je,:,2))
!         call pv_entropy(is, ie, js, je, ng, npz, q(is:ie,js:je,:,2), f0, pt, pkz, delp, grav)

      write(stdout(), *) 'PI:', pi
      write(stdout(), *) 'PHIS:', mpp_chksum(phis(is:ie,js:je))

      else if ( (test_case==-12) .or. (test_case==-13) ) then

         call DCMIP16_BC(delp,pt,u,v,q,w,delz, &
              is,ie,js,je,isd,ied,jsd,jed,npz,ncnst,ak,bk,ptop, &
              pk,peln,pe,pkz,gz,phis,ps,grid,agrid,hydrostatic, &
              nwat, adiabatic, test_case == -13, domain)

         write(stdout(), *) 'PHIS:', mpp_chksum(phis(is:ie,js:je))

      else if ( test_case==15 .or. test_case==19 ) then
!------------------------------------
! Non-hydrostatic 3D density current:
!------------------------------------
! C100_L64; hybrid_z = .T., make_nh = .F. ,   make_hybrid_z = .false.
! Control: npz=64;  dx = 100 m; dt = 1; n_split=10

        if ( test_case == 19 ) then
             f0(:,:) = 0.
             fC(:,:) = 0.
        endif

           phis = 0.
           u = 0.
           v = 0.
           w = 0.
          t00 = 300.
          p00 = 1.E5
          pk0 = p00**kappa
! Set up vertical coordinare with constant del-z spacing:
         ztop = 6.4E3
         ze1(    1) = ztop
         ze1(npz+1) = 0.
         do k=npz,2,-1
            ze1(k) = ze1(k+1) + ztop/real(npz)
         enddo

! Provide some room for the top layer
         ze1(1) = ztop + 1.5*ztop/real(npz)

         do j=js,je
            do i=is,ie
               ps(i,j) = p00
               pe(i,npz+1,j) = p00
               pk(i,j,npz+1) = pk0
            enddo
         enddo

         do k=npz,1,-1
            do j=js,je
               do i=is,ie
                  delz(i,j,k) = ze1(k+1) - ze1(k)
                    pk(i,j,k) = pk(i,j,k+1) + grav*delz(i,j,k)/(cp_air*t00)*pk0
                    pe(i,k,j) = pk(i,j,k)**(1./kappa)
               enddo
            enddo
         enddo

         ptop = pe(is,1,js)
         if ( is_master() ) write(*,*) 'Density curent testcase: model top (mb)=', ptop/100.

         do k=1,npz+1
            do j=js,je
               do i=is,ie
                  peln(i,k,j) = log(pe(i,k,j))
                   ze0(i,j,k) = ze1(k)
               enddo
            enddo
         enddo

         do k=1,npz
            do j=js,je
               do i=is,ie
                  pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
                 delp(i,j,k) =  pe(i,k+1,j)-pe(i,k,j)
                   pt(i,j,k) = t00/pk0   ! potential temp
                enddo
            enddo
         enddo

! Perturbation: center at 3 km from the ground
         pturb = 15.
         p1(1) = pi
         p1(2) = 0.

         do k=1,npz
#ifndef STD_BUBBLE
            r0 = 0.5*(ze1(k)+ze1(k+1)) - 3.2E3
#else
            r0 = (0.5*(ze1(k)+ze1(k+1)) - 3.0E3) / 2.E3
#endif
            do j=js,je
               do i=is,ie
! Impose perturbation in potential temperature: pturb
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
#ifndef STD_BUBBLE
               r = great_circle_dist( p1, p2, radius )
               dist = sqrt( r**2 + r0**2 ) / 3.2E3
#else
               r = great_circle_dist( p1, p2, radius ) / 4.E3
               dist = sqrt( r**2 + r0**2 )
#endif
                  if ( dist<=1. ) then
                       q(i,j,k,1) =      pk0 * pturb/pkz(i,j,k)*(cos(pi*dist)+1.)/2.
                       pt(i,j,k) = pt(i,j,k) - pturb/pkz(i,j,k)*(cos(pi*dist)+1.)/2.
                  else
                       q(i,j,k,1) = 0.
                  endif
! Transform back to temperature:
                   pt(i,j,k) = pt(i,j,k) * pkz(i,j,k)
               enddo
            enddo
          enddo

      else if ( test_case==16 ) then

! Non-rotating:
       f0(:,:) = 0.
       fC(:,:) = 0.
! Initialize dry atmosphere
       phis = 0.
       u = 0.
       v = 0.
       p00 = 1000.E2
! Set up vertical coordinare with constant del-z spacing:
       ztop = 10.E3
       call gw_1d(npz, p00, ak, bk, ptop, ztop, ppt)

       do z=1,npz+1
          pe1(z) = ak(z) + bk(z)*p00
       enddo

       ze1(npz+1) = 0.
       do z=npz,2,-1
          ze1(z) = ze1(z+1) + ztop/real(npz)
       enddo
       ze1(1) = ztop

       if ( is_master() ) write(*,*) 'Model top (pa)=', ptop

       do j=jsd,jed
          do i=isd,ied
             ps(i,j) = pe1(npz+1) 
          enddo
       enddo

       do z=1,npz+1
          do j=js,je
             do i=is,ie
                  pe(i,z,j) = pe1(z) 
                peln(i,z,j) = log(pe1(z)) 
                  pk(i,j,z) = exp(kappa*peln(i,z,j))
             enddo
          enddo
       enddo

! Horizontal shape function
       p1(1) = pi
       p1(2) = 0.
       r0 = radius / 3.
       do j=js,je
          do i=is,ie
             r = great_circle_dist( p1, agrid(i,j,1:2), radius )
             if ( r<r0 ) then
                  vort(i,j) = 0.5*(1.+cos(pi*r/r0))
             else
                  vort(i,j) = 0 
             endif
          enddo
       enddo

       q = 0.
       pk0 = p00**kappa
       pturb = 10./pk0
       do z=1,npz
          zmid = sin( 0.5*(ze1(z)+ze1(z+1))*pi/ztop )
          do j=js,je
             do i=is,ie
                 pkz(i,j,z) = (pk(i,j,z+1)-pk(i,j,z))/(kappa*(peln(i,z+1,j)-peln(i,z,j)))
                delp(i,j,z) =  pe(i,z+1,j)-pe(i,z,j)  
! Impose perturbation in potential temperature: pturb
                  pt(i,j,z) = ( ppt(z) + pturb*vort(i,j)*zmid ) * pkz(i,j,z)
                  q(i,j,z,1) = q(i,j,z,1) + vort(i,j)*zmid
             enddo
          enddo
       enddo

      elseif ( test_case==17 ) then
! Initialize dry atmosphere
       phis = 0.
       u = 0.
       v = 0.
       p00 = 1000.E2
! Set up vertical coordinare with constant del-z spacing:
       ztop = 10.E3
       call gw_1d(npz, p00, ak, bk, ptop, ztop, ppt)

       do z=1,npz+1
          pe1(z) = ak(z) + bk(z)*p00
       enddo

       ze1(npz+1) = 0.
       do z=npz,2,-1
          ze1(z) = ze1(z+1) + ztop/real(npz)
       enddo
       ze1(1) = ztop

       if ( is_master() ) write(*,*) 'Model top (pa)=', ptop

       do j=jsd,jed
          do i=isd,ied
             ps(i,j) = pe1(npz+1) 
          enddo
       enddo

       do z=1,npz+1
          do j=js,je
             do i=is,ie
                  pe(i,z,j) = pe1(z) 
                peln(i,z,j) = log(pe1(z)) 
                  pk(i,j,z) = exp(kappa*peln(i,z,j))
             enddo
          enddo
       enddo

! Horizontal shape function
       p1(1) = pi
       p1(2) = pi/4.
       r0 = radius / 3.
       do j=js,je
          do i=is,ie
             r = great_circle_dist( p1, agrid(i,j,1:2), radius )
             if ( r<r0 ) then
                  vort(i,j) = 0.5*(1.+cos(pi*r/r0))
             else
                  vort(i,j) = 0 
             endif
          enddo
       enddo

         pk0 = p00**kappa
       pturb = 10./pk0
       do z=1,npz
          zmid = sin( 0.5*(ze1(z)+ze1(z+1))*pi/ztop )
          do j=js,je
             do i=is,ie
                 pkz(i,j,z) = (pk(i,j,z+1)-pk(i,j,z))/(kappa*(peln(i,z+1,j)-peln(i,z,j)))
                delp(i,j,z) =  pe(i,z+1,j)-pe(i,z,j)  
! Impose perturbation in potential temperature: pturb
                  pt(i,j,z) = ( ppt(z) + pturb*vort(i,j)*zmid ) * pkz(i,j,z)
             enddo
          enddo
       enddo

      elseif ( test_case==18 ) then
         ubar = 20.
          pt0 = 288.
         n2 = grav**2 / (cp_air*pt0)

         pcen(1) = PI/2.
         pcen(2) = PI/6.

    ! Initialize surface Pressure
         do j=js2,je2
            do i=is2,ie2
               r = great_circle_dist( pcen, agrid(i,j,1:2), radius )
               phis(i,j) = grav*2.E3*exp( -(r/1500.E3)**2 )
               ps(i,j) = 930.E2 * exp( -radius*n2*ubar/(2.*grav*grav*kappa)*(ubar/radius+2.*omega)*   &
                                       (sin(agrid(i,j,2))**2-1.) - n2/(grav*grav*kappa)*phis(i,j))
            enddo
         enddo

      do z=1,npz
            do j=js,je
               do i=is,ie
                    pt(i,j,z) = pt0
                  delp(i,j,z) = ak(z+1)-ak(z) + ps(i,j)*(bk(z+1)-bk(z))
               enddo
            enddo
! v-wind:
         do j=js,je
            do i=is,ie+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e2)
               call get_latlon_vector(p3, ex, ey)
               utmp = ubar * cos(p3(2))
               vtmp = 0.
               v(i,j,z) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
            enddo
         enddo

! u-wind
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,  j,1:2)
               p2(:) = grid(i+1,j,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e1)
               call get_latlon_vector(p3, ex, ey)
               utmp = ubar * cos(p3(2))
               vtmp = 0.
               u(i,j,z) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
            enddo
         enddo
      enddo

      else if ( test_case==20 .or. test_case==21 ) then
!------------------------------------
! Non-hydrostatic 3D lee vortices
!------------------------------------
        f0(:,:) = 0.
        fC(:,:) = 0.

        if ( test_case == 20 ) then
             Ubar = 4.       ! u = Ubar * cos(lat)
             ftop = 2.0E3 * grav
        else
             Ubar = 8.       ! u = Ubar * cos(lat)
             ftop = 4.0E3 * grav
        endif

        w = 0.

         do j=js,je
            do i=is,ie+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e2)
               call get_latlon_vector(p3, ex, ey)
               utmp = ubar * cos(p3(2))
               vtmp = 0.
               v(i,j,1) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,  j,1:2)
               p2(:) = grid(i+1,j,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e1)
               call get_latlon_vector(p3, ex, ey)
               utmp = ubar * cos(p3(2))
               vtmp = 0.
               u(i,j,1) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
            enddo
         enddo

! copy vertically; no wind shear
        do k=2,npz
           do j=js,je+1
              do i=is,ie
                 u(i,j,k) = u(i,j,1)
              enddo
           enddo
           do j=js,je
              do i=is,ie+1
                 v(i,j,k) = v(i,j,1)
              enddo
           enddo
        enddo

! Center of the mountain:
        p1(1) = (0.5-0.125) * pi
        p1(2) = 0.
        call latlon2xyz(p1, e1)
         uu1 =  5.0E3
         uu2 = 10.0E3
         do j=js2,je2
            do i=is2,ie2
              p2(:) = agrid(i,j,1:2)
                  r = great_circle_dist( p1, p2, radius ) 
              if ( r < pi*radius ) then
                   p4(:) = p2(:) - p1(:)
                   if ( abs(p4(1)) > 1.E-12 ) then
                        zeta = asin ( p4(2) / sqrt(p4(1)**2 + p4(2)**2) ) 
                   else
                        zeta = pi/2.
                   endif
                   if ( p4(1) <= 0. ) zeta = pi - zeta
                    zeta = zeta + pi/6.
                     v1 = r/uu1 * cos( zeta )
                     v2 = r/uu2 * sin( zeta )
                   phis(i,j) = ftop / ( 1. + v1**2 + v2**2 )  
              else
                   phis(i,j) = 0.
              endif
            enddo
         enddo

       if ( hybrid_z ) then
            rgrav = 1./ grav
            if( npz==32 ) then
                call compute_dz_L32( npz, ztop, dz1 )
            elseif( npz.eq.31 .or. npz.eq.41 .or. npz.eq.51 ) then
                ztop = 16.E3
                call hybrid_z_dz(npz, dz1, ztop, 1.0)
            else
                if ( is_master() ) write(*,*) 'Using const DZ'
                ztop = 15.E3
                dz1(1) = ztop / real(npz) 
                do k=2,npz
                   dz1(k) = dz1(1)
                enddo
! Make top layer thicker
                dz1(1) = max( 1.0E3, 3.*dz1(2) )   ! min 1 km
            endif

! Re-compute ztop
             ze1(npz+1) = 0.
             do k=npz,1,-1
                ze1(k) = ze1(k+1) + dz1(k)
             enddo
             ztop = ze1(1)

            call set_hybrid_z( is, ie, js, je, ng, npz, ztop, dz1, rgrav,  &
                               phis, ze0, delz )
       else
            call mpp_error(FATAL, 'This test case is only currently setup for hybrid_z')
       endif

       do k=1,npz
          do j=js,je
             do i=is,ie
                delz(i,j,k) = ze0(i,j,k+1) - ze0(i,j,k)
             enddo
          enddo
       enddo

       p00 = 1.E5        ! mean SLP
       pk0 = p00**kappa
       t00 = 300.
       pt0 = t00/pk0
        n2 = 1.E-4
        s0 = grav*grav / (cp_air*n2) 

! For constant N2, Given z --> p
       do k=1,npz+1
          pe1(k) = p00*( (1.-s0/t00) + s0/t00*exp(-n2*ze1(k)/grav) )**(1./kappa)
       enddo

       ptop = pe1(1) 
       if ( is_master() ) write(*,*) 'Lee vortex testcase: model top (mb)=', ptop/100.

! Set up fake "sigma" coordinate 
       ak(1) = pe1(1)
       bk(1) = 0.
       do k=2,npz
          bk(k) = (pe1(k) - pe1(1)) / (pe1(npz+1)-pe1(1))  ! bk == sigma
          ak(k) =  pe1(1)*(1.-bk(k)) 
       enddo                                                
       ak(npz+1) = 0.
       bk(npz+1) = 1.

! Assuming constant N
       do k=2,npz+1
          do j=js,je
             do i=is,ie
                pk(i,j,k) = pk0 - (1.-exp(-n2/grav*ze0(i,j,k))) * (grav*grav)/(n2*cp_air*pt0)
                pe(i,k,j) = pk(i,j,k) ** (1./kappa)
                peln(i,k,j) = log(pe(i,k,j)) 
             enddo
          enddo
       enddo

       do j=js,je
          do i=is,ie
               pe(i,1,j) = ptop
             peln(i,1,j) = log(pe(i,1,j)) 
               pk(i,j,1) = pe(i,1,j) ** kappa
                 ps(i,j) = pe(i,npz+1,j)
          enddo
       enddo

       do k=1,npz
          do j=js,je
             do i=is,ie
                pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
               delp(i,j,k) =  pe(i,k+1,j)-pe(i,k,j)  
                 pt(i,j,k) =  pkz(i,j,k)*grav*delz(i,j,k) / ( cp_air*(pk(i,j,k)-pk(i,j,k+1)) )
              enddo
          enddo
      enddo

      else if (test_case == 51) then

         alpha = 0.
         t00 = 300.


         if (.not.hydrostatic) w(:,:,:)= 0.0


         select case (tracer_test)
         case (1) !DCMIP 11

         !Need to set up pressure arrays
!!$         p00 = 1.e5
!!$         ps = p00
!!$         phis = 0.

         !NOTE: since we have an isothermal atmosphere and specify constant height-thickness layers we will disregard ak and bk and specify the initial pressures in a different way

         dz = 12000./real(npz)
         
         allocate(zz0(npz+1))
         allocate(pz0(npz+1))

         zz0(1) = 12000.
         do k=2,npz
            zz0(k) = zz0(k-1) - dz
         enddo
         zz0(npz+1) = 0.

         if (is_master()) print*, 'TRACER ADVECTION TEST CASE'
         if (is_master()) print*, 'INITIAL LEVELS'
         !This gets interface pressure from input z-levels
         do k=1,npz+1
            !call test1_advection_deformation(agrid(is,js,1), agrid(is,js,2), pz0(k), zz0(k), 1, &
            !     ua(is,js,1), va(is,js,1), dum1, pt(is,js,1), phis(is,js), &
            !     ps(is,js), dum2, dum3, q(is,js,1,1), q(is,js,1,2), q(is,js,1,3), q(is,js,1,4))
            if (is_master()) write(*,*) k, pz0(k), zz0(k)
         enddo

         !Pressure
         do j=js,je
            do k=1,npz+1
            do i=is,ie
               pe(i,k,j) = pz0(k)
            enddo
            enddo
         enddo

         do k=1,npz
            ptmp = 0.5*(pz0(k) + pz0(k+1))
         do j=js,je
         do i=is,ie
            !This gets level-mean values from input pressures
            !call test1_advection_deformation(agrid(i,j,1),agrid(i,j,2),ptmp,dum,0, &
            !     ua(i,j,k), va(i,j,k), dum4, pt(i,j,k), phis(i,j), &
            !     ps(i,j), dum2, dum3, q(i,j,k,1), q(i,j,k,2), q(i,j,k,3), q(i,j,k,4))
            delp(i,j,k) = pz0(k+1)-pz0(k)
         enddo
         enddo
         enddo

         ptop = 100000.*exp(-12000.*grav/t00/rdgas)


         psi(:,:) = 1.e25
         psi_b(:,:) = 1.e25
         do j=jsd,jed
            do i=isd,ied
               psi(i,j) = (-1.0 * Ubar * radius *( sin(agrid(i,j,2))                  *cos(alpha) - &
                    cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) ) )
            enddo
         enddo
         call mpp_update_domains( psi, domain )
         do j=jsd,jed+1
            do i=isd,ied+1
               psi_b(i,j) = (-1.0 * Ubar * radius *( sin(grid(i,j,2))                 *cos(alpha) - &
                    cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) ) )
            enddo
         enddo

         k = 1
         do j=js,je+1
            do i=is,ie
               dist = dx(i,j)
               vc(i,j,k) = (psi_b(i+1,j)-psi_b(i,j))/dist
               if (dist==0) vc(i,j,k) = 0.
            enddo
         enddo
         do j=js,je
            do i=is,ie+1
               dist = dy(i,j)
               uc(i,j,k) = -1.0*(psi_b(i,j+1)-psi_b(i,j))/dist
               if (dist==0) uc(i,j,k) = 0.
            enddo
         enddo

         do j=js,je
            do i=is,ie+1
               dist = dxc(i,j)
               v(i,j,k) = (psi(i,j)-psi(i-1,j))/dist
               if (dist==0) v(i,j,k) = 0.
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               dist = dyc(i,j)
               u(i,j,k) = -1.0*(psi(i,j)-psi(i,j-1))/dist
               if (dist==0) u(i,j,k) = 0.
            enddo
         enddo

         do j=js,je
            do i=is,ie
               psi1 = 0.5*(psi(i,j)+psi(i,j-1))
               psi2 = 0.5*(psi(i,j)+psi(i,j+1))
               dist = dya(i,j)
               ua(i,j,k) = -1.0 * (psi2 - psi1) / (dist)
               if (dist==0) ua(i,j,k) = 0.
               psi1 = 0.5*(psi(i,j)+psi(i-1,j))
               psi2 = 0.5*(psi(i,j)+psi(i+1,j))
               dist = dxa(i,j)
               va(i,j,k) = (psi2 - psi1) / (dist)
               if (dist==0) va(i,j,k) = 0.
            enddo
         enddo

         do k=2,npz
            u(:,:,k) = u(:,:,1)
            v(:,:,k) = v(:,:,1)
            uc(:,:,k) = uc(:,:,1)
            vc(:,:,k) = vc(:,:,1)
            ua(:,:,k) = ua(:,:,1)
            va(:,:,k) = va(:,:,1)
         enddo

         call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE_PARAM)
         call fill_corners(uc, vc, npx, npy, npz, VECTOR=.true., CGRID=.true.)
         call mp_update_dwinds(u, v, npx, npy, npz, domain)

         case (2) !DCMIP 12

         case (3) !DCMIP 13

         case default
            call mpp_error(FATAL, 'Value of tracer_test not implemented ')
         end select
         
      else if (test_case == 52) then

         !Orography and steady-state test: DCMIP 20
   

         f0 = 0.
         fC = 0.

         u = 0.
         v = 0.

         p00 = 1.e5

         wind_field = tracer_test

         if (.not.hydrostatic) w(:,:,:)= 0.0

         !Set up ak and bk

         dz = 12000./real(npz)
         T00 = 300.
         p00 = 1.e5
         H = rdgas*T00/grav
         gamma = 0.0065
         exponent = Rdgas*gamma/grav
         px = ((t00-9000.*gamma)/t00)**(1./exponent) !p00 not multiplied in


         do k=1,npz+1
            height = 12000. - dz*real(k-1)
            if (height >= 9000. ) then
               ak(k) = p00*((t00-height*gamma)/t00)**(1./exponent)
               bk(k) = 0.
            else
               ak(k) = (((t00-height*gamma)/t00)**(1./exponent)-1.)/(px - 1.)*px*p00
               bk(k) = (((t00-height*gamma)/t00)**(1./exponent)-px)/(1.-px)
            endif
            if (is_master()) write(*,*) k, ak(k), bk(k), height, ak(k)+bk(k)*p00
         enddo

         ptop = ak(1)

         !Need to set up uniformly-spaced levels
         p1(1) = 3.*pi/2. ; p1(2) =  0.
         r0 = 0.75*pi
         zetam = pi/16.

         !Topography
         do j=js,je
         do i=is,ie
            p2(:) = agrid(i,j,1:2)
            r = great_circle_dist( p1, p2, one ) 
            if (r < r0) then
               phis(i,j) = grav*0.5*2000.*(1. + cos(pi*r/r0))*cos(pi*r/zetam)**2.
               pe(i,npz+1,j) = p00*(1.-gamma/T00*phis(i,j)/grav)**(1./exponent)
            else
               phis(i,j) = 0.
               pe(i,npz+1,j) = p00
            endif
            ps(i,j) = pe(i,npz+1,j)
         enddo
         enddo

         do j=js,je
         do k=1,npz
         do i=is,ie
            pe(i,k,j) = ak(k) + bk(k)*ps(i,j)
            gz(i,j,k) = t00/gamma*(1. - (pe(i,k,j)/p00)**exponent)
         enddo
         enddo
         enddo

         do k=1,npz
         do j=js,je
         do i=is,ie

            !call test2_steady_state_mountain(agrid(i,j,1),agrid(i,j,2),dum, dum2, 0, .true., &
            !     0.5*(ak(k)+ak(k+1)), 0.5*(bk(k)+bk(k+1)), dum3, dum4, dum5, &
            !     pt(i,j,k), phis(i,j), ps(i,j), dum6, q(i,j,k,1))
            delp(i,j,k) = pe(i,k+1,j) - pe(i,k,j)
            !Analytic point-value
!!$            ptmp = 0.5*(pe(i,k,j)+pe(i,k+1,j))
!!$            pt(i,j,k) = t00*(ptmp/p00)**exponent
            !ANalytic layer-mean
            pt(i,j,k) = -grav*t00*p00/(rdgas*gamma + grav)/delp(i,j,k) * &
                 ( (pe(i,k,j)/p00)**(exponent+1.) - (pe(i,k+1,j)/p00)**(exponent+1.)  )
            

         enddo
         enddo
         enddo

      else if ( abs(test_case)==30 .or.  abs(test_case)==31 ) then
!------------------------------------
! Super-Cell; with or with rotation
!------------------------------------
        if ( abs(test_case)==30) then
           f0(:,:) = 0.
           fC(:,:) = 0.
        endif

        zvir = rvgas/rdgas - 1.
        p00 = 1000.E2
          ps(:,:) = p00
        phis(:,:) = 0.
        do j=js,je
           do i=is,ie
                pk(i,j,1) = ptop**kappa
                pe(i,1,j) = ptop
              peln(i,1,j) = log(ptop)
           enddo
        enddo

        do k=1,npz
           do j=js,je
              do i=is,ie
                 delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
                 pe(i,k+1,j) = ak(k+1) + ps(i,j)*bk(k+1)
                 peln(i,k+1,j) = log(pe(i,k+1,j))
                   pk(i,j,k+1) = exp( kappa*peln(i,k+1,j) )
              enddo
           enddo
        enddo

        i = is
        j = js
        do k=1,npz
           pk1(k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
        enddo


        w(:,:,:) = 0.
        q(:,:,:,:) = 0.

        pp0(1) = 262.0/180.*pi   ! OKC            
        pp0(2) =  35.0/180.*pi   

        do k=1,npz
           do j=js,je
              do i=is,ie
                 pt(i,j,k)   = ts1(k)
                  q(i,j,k,1) = qs1(k)
                 delz(i,j,k) = rdgas/grav*ts1(k)*(1.+zvir*qs1(k))*(peln(i,k,j)-peln(i,k+1,j))
                enddo
             enddo
          enddo

        ze1(npz+1) = 0.
        do k=npz,1,-1
           ze1(k) = ze1(k+1) - delz(is,js,k)
        enddo

        us0 = 30.
        if (is_master()) then
           if (test_case > 0) then
              write(6,*) 'Toy supercell winds, piecewise approximation'
           else
              write(6,*) 'Toy supercell winds, tanh approximation'
           endif
        endif
        do k=1,npz

           zm = 0.5*(ze1(k)+ze1(k+1))
           ! Quarter-circle hodograph (Harris approximation)

           if (test_case > 0) then
              ! SRH = 40
              if ( zm .le. 2.e3 ) then
                 utmp = 8.*(1.-cos(pi*zm/4.e3)) 
                 vtmp = 8.*sin(pi*zm/4.e3)
              elseif (zm .le. 6.e3 ) then
                 utmp = 8. + (us0-8.)*(zm-2.e3)/4.e3
                 vtmp = 8.
              else
                 utmp = us0
                 vtmp = 8.
              endif
              ubar = utmp - 8.
              vbar = vtmp - 4.
           else
              ! SRH = 39
              utmp = 15.0*(1.+tanh(zm/2000. - 1.5))
              vtmp = 8.5*tanh(zm/1000.)
              ubar = utmp - 8.5
              vbar = vtmp - 4.25
!!$              ! SRH = 45
!!$              utmp = 16.0*(1.+tanh(zm/2000. - 1.4))
!!$              vtmp = 8.5*tanh(zm/1000.)
!!$              ubar = utmp - 10.
!!$              vbar = vtmp - 4.25
!!$              ! SRH = 27 (really)
!!$              utmp = 0.5*us0*(1.+tanh((zm-3500.)/2000.))
!!$              vtmp = 8.*tanh(zm/1000.)
!!$              ubar = utmp - 10.
!!$              vbar = vtmp - 4.
           endif

           if( is_master() ) then
              write(6,*) k, utmp, vtmp
           endif
              
           do j=js,je
              do i=is,ie+1
                 p1(:) = grid(i  ,j ,1:2)
                 p2(:) = grid(i,j+1 ,1:2)
                 call mid_pt_sphere(p1, p2, p3)
                 call get_unit_vect2(p1, p2, e2)
                 call get_latlon_vector(p3, ex, ey)
! Scaling factor is a Gaussian decay from center
                 v(i,j,k) = exp(-8.*great_circle_dist(pp0,p3,radius)/radius) *   &
                           (ubar*inner_prod(e2,ex) + vbar*inner_prod(e2,ey))
              enddo
           enddo
           do j=js,je+1
              do i=is,ie
                 p1(:) = grid(i,  j,1:2)
                 p2(:) = grid(i+1,j,1:2)
                 call mid_pt_sphere(p1, p2, p3)
                 call get_unit_vect2(p1, p2, e1)
                 call get_latlon_vector(p3, ex, ey)
! Scaling factor is a Gaussian decay from center
                 u(i,j,k) = exp(-8.*great_circle_dist(pp0,p3,radius)/radius) *   &
                           (ubar*inner_prod(e1,ex) + vbar*inner_prod(e1,ey))
              enddo
           enddo
        enddo

     call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                .true., hydrostatic, nwat, domain)

! *** Add Initial perturbation ***
        pturb = 2.
        r0 = 10.e3     ! radius
        zc = 1.4e3     ! center of bubble from surface
        do k=1, npz
           zm = 0.5*(ze1(k)+ze1(k+1))   ! center of the layer
           ptmp = ( (zm-zc)/zc ) **2
           if ( ptmp < 1. ) then
              do j=js,je
                 do i=is,ie
                    dist = ptmp + (great_circle_dist(pp0, agrid(i,j,1:2), radius)/r0)**2
                    if ( dist < 1. ) then
                         pt(i,j,k) = pt(i,j,k) + pturb*(1.-sqrt(dist))
                    endif
                 enddo
              enddo
           endif
        enddo

     elseif (test_case == 32) then

        call mpp_error(FATAL, ' test_case 32 not yet implemented')

      else if ( test_case==33 .or. test_case==34 .or. test_case==35 ) then
!------------------------------------
! HIWPP M0ountain waves tests
!------------------------------------
        f0(:,:) = 0.
        fC(:,:) = 0.

        phis(:,:) = 1.E30
          ps(:,:) = 1.E30

        zvir = 0.
        p00 = 1000.E2
        t00 = 300.
        us0 = 20.
! Vertical shear parameter for M3 case:
        if ( test_case == 35 ) then
             cs_m3 = 2.5e-4
        else
             cs_m3 = 0.
        endif

! Mountain height:
        h0 = 250.
! Mountain center
        p0(1) = 60./180. * pi
        p0(2) = 0.
! 9-point average:
!      9  4  8
!
!      5  1  3
!          
!      6  2  7
! pt = 0.25*pt1 + 0.125*(pt2+pt3+pt4+pt5) + 0.0625*(pt6+pt7+pt8+pt9)
     if ( test_case==35 ) then
        dum = -cs_m3/grav
        do j=js,je
           do i=is,ie
! temperature is function of latitude (due to vertical shear)
#ifdef USE_CELL_AVG
                    p2(2) = agrid(i,j,2)
              pt1 = exp( dum*(us0*sin(p2(2)))**2 )
                    call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
              pt2 = exp( dum*(us0*sin(p2(2)))**2 )
                    call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p2)
              pt3 = exp( dum*(us0*sin(p2(2)))**2 )
                    call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p2)
              pt4 = exp( dum*(us0*sin(p2(2)))**2 )
                    call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p2)
              pt5 = exp( dum*(us0*sin(p2(2)))**2 )
                    p2(2) = grid(i,j,2)
              pt6 = exp( dum*(us0*sin(p2(2)))**2 )
                    p2(2) = grid(i+1,j,2)
              pt7 = exp( dum*(us0*sin(p2(2)))**2 )
                    p2(2) = grid(i+1,j+1,2)
              pt8 = exp( dum*(us0*sin(p2(2)))**2 )
                    p2(2) = grid(i,j+1,2)
              pt9 = exp( dum*(us0*sin(p2(2)))**2 )
              ptmp = t00*(0.25*pt1+0.125*(pt2+pt3+pt4+pt5)+0.0625*(pt6+pt7+pt8+pt9))
#else
              ptmp = t00*exp( dum*(us0*sin(agrid(i,j,2)))**2 )
#endif
              do k=1,npz
                 pt(i,j,k) = ptmp
              enddo
           enddo
        enddo
     else
        pt(:,:,:) = t00
     endif

     if( test_case==33 ) then   
! NCAR Ridge-mountain Mods:
        do j=js,je
           do i=is,ie
#ifdef USE_CELL_AVG
                p2(1:2) = agrid(i,j,1:2)
                r = radius*(p2(1)-p0(1))
              pt1 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
                r = radius*(p2(1)-p0(1))
              pt2 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p2)
                r = radius*(p2(1)-p0(1))
              pt3 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p2)
                r = radius*(p2(1)-p0(1))
              pt4 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p2)
                r = radius*(p2(1)-p0(1))
              pt5 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                p2(1:2) = grid(i,j,1:2)
                r = radius*(p2(1)-p0(1))
              pt6 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                p2(1:2) = grid(i+1,j,1:2)
                r = radius*(p2(1)-p0(1))
              pt7 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                p2(1:2) = grid(i+1,j+1,1:2)
                r = radius*(p2(1)-p0(1))
              pt8 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                p2(1:2) = grid(i,j+1,1:2)
                r = radius*(p2(1)-p0(1))
              pt9 = cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
              phis(i,j) = grav*h0*(0.25*pt1+0.125*(pt2+pt3+pt4+pt5)+0.0625*(pt6+pt7+pt8+pt9))
#else
                p2(1:2) = agrid(i,j,1:2)
                r = radius*(p2(1)-p0(1))
              phis(i,j) = grav*h0*cos(p2(2))*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
#endif
           enddo
        enddo
     else
! Circular mountain:
        do j=js,je
           do i=is,ie
! 9-point average:
!      9  4  8
!
!      5  1  3
!          
!      6  2  7
! pt = 0.25*pt1 + 0.125*(pt2+pt3+pt4+pt5) + 0.0625*(pt6+pt7+pt8+pt9)
#ifdef USE_CELL_AVG
                   r = great_circle_dist( p0, agrid(i,j,1:2), radius ) 
                 pt1 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
                   r = great_circle_dist( p0, p2, radius ) 
                 pt2 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p2)
                   r = great_circle_dist( p0, p2, radius ) 
                 pt3 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p2)
                   r = great_circle_dist( p0, p2, radius ) 
                 pt4 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p2)
                   r = great_circle_dist( p0, p2, radius ) 
                 pt5 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   r = great_circle_dist( p0, grid(i,j,1:2), radius ) 
                 pt6 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   r = great_circle_dist( p0, grid(i+1,j,1:2), radius ) 
                 pt7 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   r = great_circle_dist( p0, grid(i+1,j+1,1:2), radius ) 
                 pt8 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
                   r = great_circle_dist( p0, grid(i,j+1,1:2), radius ) 
                 pt9 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
              phis(i,j) = grav*h0*(0.25*pt1+0.125*(pt2+pt3+pt4+pt5)+0.0625*(pt6+pt7+pt8+pt9))
#else
                   r = great_circle_dist( p0, agrid(i,j,1:2), radius ) 
                 pt1 = exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
              phis(i,j) = grav*h0*exp(-(r/5.e3)**2)*cos(pi*r/4.e3)**2
#endif
           enddo
        enddo
     endif

     do j=js,je
        do i=is,ie
! DCMIP Eq(33)
           ps(i,j) = p00*exp( -0.5*(us0*sin(agrid(i,j,2)))**2/(rdgas*t00)-phis(i,j)/(rdgas*pt(i,j,1)) )
           pe(i,1,j) = ptop
           peln(i,1,j) = log(ptop)
           pk(i,j,1) = ptop**kappa
        enddo
     enddo

        do k=2,npz+1
           do j=js,je
              do i=is,ie
                 pe(i,k,j) = ak(k) + ps(i,j)*bk(k)
               peln(i,k,j) = log(pe(i,k,j))
                 pk(i,j,k) = exp( kappa*peln(i,k,j) )
              enddo
           enddo
        enddo

        do k=1,npz
           do j=js,je
              do i=is,ie
                 delp(i,j,k) = pe(i,k+1,j) - pe(i,k,j)
                 delz(i,j,k) = rdgas/grav*pt(i,j,k)*(peln(i,k,j)-peln(i,k+1,j))
              enddo
           enddo
        enddo

! Comnpute mid-level height, using w for temp storage
        do j=js,je
           do i=is,ie
              ze1(npz+1) = phis(i,j)/grav
              do k=npz,1,-1
                 ze1(k) = ze1(k+1) - delz(i,j,k)
              enddo
              do k=1,npz
                 w(i,j,k) = 0.5*(ze1(k)+ze1(k+1))
              enddo
           enddo
        enddo
        call mpp_update_domains( w, domain )

        do k=1,npz
           do j=js,je
              do i=is,ie+1
                 p1(:) = grid(i  ,j, 1:2)
                 p2(:) = grid(i,j+1, 1:2)
                 call mid_pt_sphere(p1, p2, p3)
                 call get_unit_vect2(p1, p2, e2)
                 call get_latlon_vector(p3, ex, ey)
! Joe Klemp's mod:
                 utmp = us0*cos(p3(2))*sqrt( 1. + cs_m3*(w(i-1,j,k)+w(i,j,k)) )
                 v(i,j,k) = utmp*inner_prod(e2,ex)
              enddo
           enddo
           do j=js,je+1
              do i=is,ie
                 p1(:) = grid(i,  j, 1:2)
                 p2(:) = grid(i+1,j, 1:2)
                 call mid_pt_sphere(p1, p2, p3)
                 call get_unit_vect2(p1, p2, e1)
                 call get_latlon_vector(p3, ex, ey)
                 utmp = us0*cos(p3(2))*sqrt( 1. + cs_m3*(w(i,j-1,k)+w(i,j,k)) )
                 u(i,j,k) = utmp*inner_prod(e1,ex)
              enddo
           enddo
        enddo

     w(:,:,:) = 0.    ! reset w
     q(:,:,:,:) = 0.

     call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                .true., hydrostatic, nwat, domain)

      else if ( test_case==36 .or. test_case==37 ) then
!------------------------------------
! HIWPP Super-Cell
!------------------------------------
! HIWPP SUPER_K; 
        f0(:,:) = 0.
        fC(:,:) = 0.
        q(:,:,:,:) = 0.
        w(:,:,:) = 0.

        zvir = rvgas/rdgas - 1.
        p00 = 1000.E2
        pk0 = p00**kappa
        ps(:,:) = p00
        phis(:,:) = 0.
!
! Set up vertical layer spacing:
        ztop = 20.e3
        ze1(1) = ztop
        ze1(npz+1) = 0.
#ifndef USE_VAR_DZ
! Truly uniform setup:
        do k=npz,2,-1
           ze1(k) = ze1(k+1) + ztop/real(npz)
        enddo
#else
! Lowest layer half of the size
!       ze1(npz) = ztop / real(2*npz-1)    ! lowest layer thickness
!       zm = (ztop-ze1(npz)) / real(npz-1)
!       do k=npz,2,-1
!          ze1(k) = ze1(k+1) + zm
!       enddo
        call var_dz(npz, ztop, ze1)
#endif
        do k=1,npz
           zs1(k) = 0.5*(ze1(k)+ze1(k+1))
        enddo
!-----
! Get sounding at "equator": initial storm center
        call SuperK_Sounding(npz, pe1, p00, ze1, ts1, qs1)
! ts1 is FV's definition of potential temperature at EQ

        do k=1,npz
           ts1(k) = cp_air*ts1(k)*(1.+zvir*qs1(k)) ! cp*thelta_v
        enddo
! Initialize the fields on z-coordinate; adjust top layer mass
! Iterate then interpolate to get balanced pt & pk on the sphere
! Adjusting ptop
        call SuperK_u(npz, zs1, uz1, dudz)
        call balanced_K(npz, is, ie, js, je, ng, pe1(npz+1), ze1, ts1, qs1, uz1, dudz, pe, pk, pt,  &
                        delz, zvir, ptop, ak, bk, agrid)
        do j=js,je
           do i=is,ie
              ps(i,j) = pe(i,npz+1,j)
           enddo
        enddo

        do k=1,npz+1
           do j=js,je
              do i=is,ie
                 peln(i,k,j) = log(pe(i,k,j))
                 pk(i,j,k) = exp( kappa*peln(i,k,j) )
              enddo
           enddo
        enddo

        do k=1,npz
           do j=js,je
              do i=is,ie
                 delp(i,j,k) = pe(i,k+1,j) - pe(i,k,j)
                  pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
                 q(i,j,k,1) = qs1(k)
              enddo
           enddo
        enddo

        k = 1 ! keep the same temperature but adjust the height at the top layer
           do j=js,je
              do i=is,ie
                 delz(i,j,k) = rdgas/grav*pt(i,j,k)*(1.+zvir*qs1(k))*(peln(i,k,j)-peln(i,k+1,j))
              enddo
           enddo
! Adjust temperature; enforce constant dz except the top layer
        do k=2,npz
           do j=js,je
              do i=is,ie
                 delz(i,j,k) = ze1(k+1) - ze1(k)
                   pt(i,j,k) = delz(i,j,k)*grav/(rdgas*(1.+zvir*qs1(k))*(peln(i,k,j)-peln(i,k+1,j)))
              enddo
           enddo
        enddo

! Wind-profile:
        do k=1,npz
           do j=js,je
              do i=is,ie+1
                 p1(:) = grid(i  ,j ,1:2)
                 p2(:) = grid(i,j+1 ,1:2)
                 call mid_pt_sphere(p1, p2, p3)
                 call get_unit_vect2(p1, p2, e2)
                 call get_latlon_vector(p3, ex, ey)
                 v(i,j,k) = uz1(k)*cos(p3(2))*inner_prod(e2,ex)
              enddo
           enddo
           do j=js,je+1
              do i=is,ie
                 p1(:) = grid(i,  j,1:2)
                 p2(:) = grid(i+1,j,1:2)
                 call mid_pt_sphere(p1, p2, p3)
                 call get_unit_vect2(p1, p2, e1)
                 call get_latlon_vector(p3, ex, ey)
                 u(i,j,k) = uz1(k)*cos(p3(2))*inner_prod(e1,ex)
              enddo
           enddo
        enddo

! *** Add Initial perturbation ***
      if ( test_case == 37 ) then
        pp0(1) = pi
        pp0(2) = 0.
        if (adiabatic) then
           pturb = 10.
        else
           pturb = 3.     ! potential temperature
        endif
        r0 = 10.e3     ! radius
        zc = 1.5e3     ! center of bubble from surface
        do k=1, npz
           zm = 0.5*(ze1(k)+ze1(k+1))   ! center of the layer
           ptmp = ( (zm-zc)/zc ) **2
           if ( ptmp < 1. ) then
              do j=js,je
                 do i=is,ie
                    dist = ptmp + (great_circle_dist(pp0, agrid(i,j,1:2), radius)/r0)**2
                    dist = sqrt(dist)
                    if ( dist < 1. ) then
                         pt(i,j,k) = pt(i,j,k) + (pkz(i,j,k)/pk0)*pturb*cos(0.5*pi*dist)**2
                    endif
                 enddo
              enddo
           endif
        enddo
      endif

      else if (test_case == 44) then    ! Lock-exchange K-H instability on a very large-scale

         !Background state
         p00 = 1000.e2
         ps(:,:) = p00
         phis = 0.0
         u(:,:,:) = 0.
         v(:,:,:) = 0.
         q(:,:,:,:) = 0.

         if (adiabatic) then
             zvir = 0.
         else
             zvir = rvgas/rdgas - 1.
         endif

! Initialize delta-P
        do z=1,npz
            do j=js,je
               do i=is,ie
                  delp(i,j,z) = ak(z+1)-ak(z) + ps(i,j)*(bk(z+1)-bk(z))
               enddo
            enddo
         enddo
         
         do j=js,je
            do i=is,ie
               pe(i,1,j) = ptop
               peln(i,1,j) = log(pe(i,1,j)) 
                 pk(i,j,1) = exp(kappa*peln(i,1,j))
            enddo
            do k=2,npz+1
            do i=is,ie
                 pe(i,k,j) = pe(i,k-1,j) + delp(i,j,k-1)
               peln(i,k,j) = log(pe(i,k,j)) 
                 pk(i,j,k) = exp(kappa*peln(i,k,j))
            enddo
            enddo
         enddo

         p1(1) = pi
         p1(2) = 0.
         r0 = 1000.e3     ! hurricane size

         do k=1,npz
         do j=js,je
            do i=is,ie
               pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
               dist = great_circle_dist( p0, agrid(i,j,1:2), radius ) 
               if ( dist .le. r0 ) then
                  pt(i,j,k) = 275.
                  q(i,j,k,1) = 1.
               else
                  pt(i,j,k) = 265.
                  q(i,j,k,1) = 0.
               end if
!              pt(i,j,k) = pt(i,j,k)*pkz(i,j,k)
            enddo
         enddo
         enddo

         if (.not.hydrostatic) then
             do k=1,npz
                do j=js,je
                   do i=is,ie
                      delz(i,j,k) = rdgas*pt(i,j,k)*(1.+zvir*q(i,j,k,1))/grav*log(pe(i,k,j)/pe(i,k+1,j))
                         w(i,j,k) = 0.0
                   enddo
                enddo
             enddo
         endif

      else if (test_case == 45 .or. test_case == 46) then    ! NGGPS test?

! Background state
         f0 = 0.;  fC = 0.
         pt0 = 300.   ! potentil temperature
         p00 = 1000.e2
         ps(:,:) = p00
         phis = 0.0
         u(:,:,:) = 0.
         v(:,:,:) = 0.
         q(:,:,:,:) = 0.

         if (adiabatic) then
             zvir = 0.
         else
             zvir = rvgas/rdgas - 1.
         endif

! Initialize delta-P
        do k=1,npz
            do j=js,je
               do i=is,ie
                  delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
               enddo
            enddo
         enddo
         
         do j=js,je
            do i=is,ie
               pe(i,1,j) = ptop
               peln(i,1,j) = log(pe(i,1,j)) 
                 pk(i,j,1) = exp(kappa*peln(i,1,j))
            enddo
            do k=2,npz+1
            do i=is,ie
                 pe(i,k,j) = pe(i,k-1,j) + delp(i,j,k-1)
               peln(i,k,j) = log(pe(i,k,j)) 
                 pk(i,j,k) = exp(kappa*peln(i,k,j))
            enddo
            enddo
         enddo

! Initiate the westerly-wind-burst:
         ubar = soliton_Umax
         r0 = soliton_size 
!!$        if (test_case == 46) then
!!$           ubar = 200.
!!$           r0 = 250.e3
!!$        else
!!$           ubar = 50.       ! Initial maxmium wind speed (m/s)
!!$           r0 = 500.e3
!!$        endif
        p0(1) = pi*0.5
        p0(2) = 0.

     do k=1,npz
        do j=js,je
           do i=is,ie+1
              p1(:) = grid(i  ,j ,1:2)
              p2(:) = grid(i,j+1 ,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e2)
              call get_latlon_vector(p3, ex, ey)
              v(i,j,k) = utmp*inner_prod(e2,ex)
           enddo
        enddo
        do j=js,je+1
           do i=is,ie
              p1(:) = grid(i,  j,1:2)
              p2(:) = grid(i+1,j,1:2)
              call mid_pt_sphere(p1, p2, p3)
              r = great_circle_dist( p0, p3, radius )
              utmp = ubar*exp(-(r/r0)**2)
              call get_unit_vect2(p1, p2, e1)
              call get_latlon_vector(p3, ex, ey)
              u(i,j,k) = utmp*inner_prod(e1,ex)
           enddo
        enddo

        do j=js,je
           do i=is,ie
              pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
#ifdef USE_PT
              pt(i,j,k) = pt0/p00**kappa
! Convert back to temperature:
              pt(i,j,k) = pt(i,j,k)*pkz(i,j,k)
#else
              pt(i,j,k) = pt0
#endif
              q(i,j,k,1) = 0.
           enddo
        enddo

     enddo

#ifdef NEST_TEST
     do k=1,npz
     do j=js,je
     do i=is,ie
        q(i,j,k,:) = agrid(i,j,1)*0.180/pi
     enddo
     enddo
     enddo
#else
     call checker_tracers(is,ie, js,je, isd,ied, jsd,jed,  &
                          ncnst, npz, q, agrid(is:ie,js:je,1), agrid(is:ie,js:je,2), 9., 9.)
#endif

        if ( .not. hydrostatic ) then
            do k=1,npz
               do j=js,je
                  do i=is,ie
                     delz(i,j,k) = rdgas*pt(i,j,k)/grav*log(pe(i,k,j)/pe(i,k+1,j))
                        w(i,j,k) = 0.0
                  enddo
               enddo
            enddo
         endif
      else if (test_case == 55 .or. test_case == 56 .or. test_case == 57) then

         !Tropical cyclone test case: DCMIP 5X

         !test_case 56 initializes the environment
         ! but no vortex

         !test_case 57 uses a globally-uniform f-plane

         ! Initialize surface Pressure
         !Vortex perturbation
         p0(1) = 180. * pi / 180.
         p0(2) = 10. * pi / 180.

         if (test_case == 56) then
            dp = 0.
            rp = 1.e25
         else
         dp = 1115.
         rp = 282000.
         endif
         p00 = 101500.

         ps = p00

         do j=js,je
         do i=is,ie
            p2(:) = agrid(i,j,1:2)
            r = great_circle_dist( p0, p2, radius ) 
            ps(i,j) = p00 - dp*exp(-(r/rp)**1.5)
            phis(i,j) = 0.
         enddo
         enddo

        call prt_maxmin('PS', ps(is:ie,js:je), is, ie, js, je, 0, 1, 0.01)

         ! Initialize delta-P
         do z=1,npz
         do j=js,je
         do i=is,ie
            delp(i,j,z) = ak(z+1)-ak(z) + ps(i,j)*(bk(z+1)-bk(z))
         enddo
         enddo
         enddo
         
         !Pressure
         do j=js,je
            do i=is,ie
               pe(i,1,j) = ptop
            enddo
            do k=2,npz+1
            do i=is,ie
               pe(i,k,j) = pe(i,k-1,j) + delp(i,j,k-1)
            enddo
            enddo
         enddo

         !Pressure on v-grid and u-grid points
         do j=js,je
         do i=is,ie+1
            p2(:) = 0.5*(grid(i,j,1:2)+grid(i,j+1,1:2))
            r = great_circle_dist( p0, p2, radius ) 
            ps_v(i,j) = p00 - dp*exp(-(r/rp)**1.5)
         enddo
         enddo
         do j=js,je+1
         do i=is,ie
            p2(:) = 0.5*(grid(i,j,1:2)+grid(i+1,j,1:2))
            r = great_circle_dist( p0, p2, radius ) 
            ps_u(i,j) = p00 - dp*exp(-(r/rp)**1.5)
         enddo
         enddo
         
         !Pressure
         do j=js,je
            do i=is,ie+1
               pe_v(i,1,j) = ptop
            enddo
            do k=2,npz+1
            do i=is,ie+1
               pe_v(i,k,j) = ak(k) + ps_v(i,j)*bk(k)
            enddo
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               pe_u(i,1,j) = ptop
            enddo
            do k=2,npz+1
            do i=is,ie
               pe_u(i,k,j) = ak(k) + ps_u(i,j)*bk(k)
            enddo
            enddo
         enddo

         !Everything else
         !if (adiabatic) then
         !   zvir = 0.
         !else
            zvir = rvgas/rdgas - 1.
         !endif

         p0 = (/ pi, pi/18. /)
         
         exppr = 1.5
         exppz = 2.
         gamma = 0.007
         Ts0 = 302.15
         q00 = 0.021
         t00 = Ts0*(1.+zvir*q00)
         exponent = rdgas*gamma/grav
         ztrop = 15000.
         zp = 7000.
         dp = 1115.
         cor = 2.*omega*sin(p0(2)) !Coriolis at vortex center

         !Initialize winds separately on the D-grid
         do j=js,je
            do i=is,ie+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e2)
               call get_latlon_vector(p3, ex, ey)

               d1 = sin(p0(2))*cos(p3(2)) - cos(p0(2))*sin(p3(2))*cos(p3(1)-p0(1))
               d2 = cos(p0(2))*sin(p3(1)-p0(1))
               d = max(1.e-15,sqrt(d1**2+d2**2))

               r = great_circle_dist( p0, p3, radius ) 

               do k=1,npz
                  ptmp = 0.5*(pe_v(i,k,j)+pe_v(i,k+1,j))
                  height = (t00/gamma)*(1.-(ptmp/ps_v(i,j))**exponent)
                  if (height > ztrop) then
                     v(i,j,k) = 0.
                  else
                     utmp = 1.d0/d*(-cor*r/2.d0+sqrt((cor*r/2.d0)**(2.d0) &
                          - exppr*(r/rp)**exppr*rdgas*(t00-gamma*height) &
                          /(exppz*height*rdgas*(t00-gamma*height)/(grav*zp**exppz) &
                          +(1.d0-p00/dp*exp((r/rp)**exppr)*exp((height/zp)**exppz)))))
                     vtmp = utmp*d2
                     utmp = utmp*d1
                     
                     v(i,j,k) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)

                  endif
               enddo
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,  j,1:2)
               p2(:) = grid(i+1,j,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e1)
               call get_latlon_vector(p3, ex, ey)

               d1 = sin(p0(2))*cos(p3(2)) - cos(p0(2))*sin(p3(2))*cos(p3(1)-p0(1))
               d2 = cos(p0(2))*sin(p3(1)-p0(1))
               d = max(1.e-15,sqrt(d1**2+d2**2))

               r = great_circle_dist( p0, p3, radius ) 

               do k=1,npz
                  ptmp = 0.5*(pe_u(i,k,j)+pe_u(i,k+1,j))
                  height = (t00/gamma)*(1.-(ptmp/ps_u(i,j))**exponent)
                  if (height > ztrop) then
                     v(i,j,k) = 0.
                  else
                     utmp = 1.d0/d*(-cor*r/2.d0+sqrt((cor*r/2.d0)**(2.d0) &
                          - exppr*(r/rp)**exppr*rdgas*(t00-gamma*height) &
                          /(exppz*height*rdgas*(t00-gamma*height)/(grav*zp**exppz) &
                          +(1.d0-p00/dp*exp((r/rp)**exppr)*exp((height/zp)**exppz)))))
                     vtmp = utmp*d2
                     utmp = utmp*d1

                     u(i,j,k) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
                  endif
               enddo

            enddo
         enddo

         qtrop = 1.e-11
         ttrop = t00 - gamma*ztrop
         zq1 = 3000.
         zq2 = 8000.

         q(:,:,:,:) = 0.

         do k=1,npz
         do j=js,je
         do i=is,ie
               ptmp = 0.5*(pe(i,k,j)+pe(i,k+1,j))
               height = (t00/gamma)*(1.-(ptmp/ps(i,j))**exponent)
               if (height > ztrop) then
                  q(i,j,k,1) = qtrop
                  pt(i,j,k) = Ttrop
               else
                  q(i,j,k,1) = q00*exp(-height/zq1)*exp(-(height/zq2)**exppz)
                  p2(:) = agrid(i,j,1:2)
                  r = great_circle_dist( p0, p2, radius ) 
                  pt(i,j,k) = (T00-gamma*height)/(1.d0+zvir*q(i,j,k,1))/(1.d0+exppz*Rdgas*(T00-gamma*height)*height &
                       /(grav*zp**exppz*(1.d0-p00/dp*exp((r/rp)**exppr)*exp((height/zp)**exppz))))
               end if
         enddo
         enddo
         enddo

         !Note that this is already the moist pressure
         do j=js,je
         do i=is,ie
            ps(i,j) = pe(i,npz+1,j)
         enddo
         enddo

         if (.not.hydrostatic) then
             do k=1,npz
                do j=js,je
                   do i=is,ie
                      delz(i,j,k) = rdgas*pt(i,j,k)*(1.+zvir*q(i,j,k,1))/grav*log(pe(i,k,j)/pe(i,k+1,j))
                         w(i,j,k) = 0.0
                   enddo
                enddo
             enddo
         endif

         call dtoa(u , v , ua, va, dx,dy,dxa,dya,dxc,dyc,npx, npy, ng)

         call prt_maxmin('PS', ps(is:ie,js:je), is, ie, js, je, 0, 1, 0.01)

         if (test_case == 57) then
            do j=jsd,jed+1
               do i=isd,ied+1
                  fC(i,j) = cor
               enddo
            enddo
            do j=jsd,jed
               do i=isd,ied
                  f0(i,j) = cor
               enddo
            enddo            
         endif
         

      else if ( test_case == -55 ) then

         call DCMIP16_TC (delp, pt, u, v, q, w, delz, &
              is, ie, js, je, isd, ied, jsd, jed, npz, ncnst, &
              ak, bk, ptop, pk, peln, pe, pkz, gz, phis, &
              ps, grid, agrid, hydrostatic, nwat, adiabatic)

      else

         call mpp_error(FATAL, " test_case not defined" )

      endif !test_case

      call mpp_update_domains( phis, domain )

     ftop = g_sum(domain, phis(is:ie,js:je), is, ie, js, je, ng, area, 1)
     if(is_master()) write(*,*) 'mean terrain height (m)=', ftop/grav

! The flow is initially hydrostatic
#ifndef SUPER_K
     call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., mountain, &
                moist_phys, hydrostatic, nwat, domain, .not.hydrostatic)
#endif

#ifdef COLUMN_TRACER
      if( ncnst>1 ) q(:,:,:,2:ncnst) = 0.0
   ! Initialize a dummy Column Tracer
         pcen(1) = PI/9.
         pcen(2) = 2.0*PI/9.
         r0 = radius/10.0
         do z=1,npz
            do j=js,je
               do i=is,ie
                  p1(:) = grid(i  ,j ,1:2)
                  p2(:) = grid(i,j+1 ,1:2)
                  call mid_pt_sphere(p1, p2, pa)
                  call get_unit_vect2(p1, p2, e2)
                  call get_latlon_vector(pa, ex, ey)
             ! Perturbation Location Case==13
                  r = great_circle_dist( pcen, pa, radius )
                  if (-(r/r0)**2.0 > -40.0) q(i,j,z,1) = EXP(-(r/r0)**2.0)
               enddo
            enddo
         enddo
#endif

#endif
    call mp_update_dwinds(u, v, npx, npy, npz, domain)


    nullify(agrid)
    nullify(grid)

    nullify(area)
    nullify(rarea)

    nullify(fC)
    nullify(f0)

    nullify(dx)   
    nullify(dy)   
    nullify(dxa)  
    nullify(dya)  
    nullify(rdxa) 
    nullify(rdya) 
    nullify(dxc)  
    nullify(dyc)  

    nullify(ee1)       
    nullify(ee2)   
    nullify(ew)    
    nullify(es)    
    nullify(en1)   
    nullify(en2)   

    nullify(latlon)
    nullify(cubed_sphere)

    nullify(domain)
    nullify(tile)

    nullify(have_south_pole) 
    nullify(have_north_pole) 

    nullify(ntiles_g)        
    nullify(acapN)           
    nullify(acapS)           
    nullify(globalarea)      

  end subroutine init_case

  subroutine get_vorticity(isc, iec, jsc, jec ,isd, ied, jsd, jed, npz, u, v, vort, dx, dy, rarea)
  integer isd, ied, jsd, jed, npz
  integer isc, iec, jsc, jec
  real, intent(in)  :: u(isd:ied, jsd:jed+1, npz), v(isd:ied+1, jsd:jed, npz)
  real, intent(out) :: vort(isc:iec, jsc:jec, npz)
  real, intent(IN) :: dx(isd:ied,jsd:jed+1)
  real, intent(IN) :: dy(isd:ied+1,jsd:jed)
  real, intent(IN) :: rarea(isd:ied,jsd:jed)
! Local
  real :: utmp(isc:iec, jsc:jec+1), vtmp(isc:iec+1, jsc:jec)
  integer :: i,j,k

      do k=1,npz
         do j=jsc,jec+1
            do i=isc,iec
               utmp(i,j) = u(i,j,k)*dx(i,j)
            enddo
         enddo
         do j=jsc,jec
            do i=isc,iec+1
               vtmp(i,j) = v(i,j,k)*dy(i,j)
            enddo
         enddo

         do j=jsc,jec
            do i=isc,iec
               vort(i,j,k) = rarea(i,j)*(utmp(i,j)-utmp(i,j+1)-vtmp(i,j)+vtmp(i+1,j))
            enddo
         enddo
      enddo
      
  end subroutine get_vorticity
 
  subroutine checker_tracers(i0, i1, j0, j1, ifirst, ilast, jfirst, jlast,  &
                             nq, km, q, lon, lat, nx, ny, rn)
!--------------------------------------------------------------------
! This routine computes the checker-board tracer pattern with optional
! random pertubation (if rn/= 0)
! To get 20 (deg) by 20 (deg) checker boxes: nx=9, ny=9
! If random noises are desired, rn=0.1 is a good value
! lon: longitude (Radian)
! lat: latitude  (Radian)
! Coded by S.-J. Lin for HIWPP benchmark, Oct2, 2014
!--------------------------------------------------------------------
  integer, intent(in):: nq          ! number of tracers
  integer, intent(in):: km          ! vertical dimension
  integer, intent(in):: i0, i1      ! compute domain dimension in E-W
  integer, intent(in):: j0, j1      ! compute domain dimension in N-S
  integer, intent(in):: ifirst, ilast, jfirst, jlast ! tracer array dimensions
  real, intent(in):: nx    ! east-west wave number
  real, intent(in):: ny    ! North-south wave number
  real, intent(in), optional:: rn    ! (optional) magnitude of random perturbation
  real(kind=R_GRID), intent(in), dimension(i0:i1,j0:j1):: lon, lat
  real, intent(out):: q(ifirst:ilast,jfirst:jlast,km,nq)
! Local var:
  real:: qt(i0:i1,j0:j1)
  real:: qtmp, ftmp
  integer:: i,j,k,iq

!$OMP parallel do default(none) shared(i0,i1,j0,j1,nx,lon,ny,lat,qt) &
!$OMP                          private(qtmp)
  do j=j0,j1
     do i=i0,i1
        qtmp = sin(nx*lon(i,j))*sin(ny*lat(i,j))
        if ( qtmp < 0. ) then
             qt(i,j) = 0.
        else
             qt(i,j) = 1.
        endif
     enddo
  enddo

  if ( present(rn) ) then   ! Add random noises to the set pattern
  do iq=1,nq
     call random_seed()
!$OMP parallel do default(none) shared(i0,i1,j0,j1,km,q,qt,rn,iq) &
!$OMP                          private(ftmp)
     do k=1,km
        do j=j0,j1
           do i=i0,i1
              call random_number(ftmp)
              q(i,j,k,iq) = qt(i,j) + rn*ftmp
           enddo
        enddo
     enddo
  enddo
  else
  do iq=1,nq
!$OMP parallel do default(none) shared(i0,i1,j0,j1,km,q,qt,iq) &
!$OMP                          private(ftmp)
     do k=1,km
        do j=j0,j1
           do i=i0,i1
              q(i,j,k,iq) = qt(i,j)
           enddo
        enddo
     enddo
  enddo
  endif

  end subroutine checker_tracers

  subroutine terminator_tracers(i0, i1, j0, j1, ifirst, ilast, jfirst, jlast,  &
       km, q, delp, ncnst, lon, lat)
!--------------------------------------------------------------------
! This routine implements the terminator test.
! Coded by Lucas Harris for DCMIP 2016, May 2016
!--------------------------------------------------------------------
  integer, intent(in):: km          ! vertical dimension
  integer, intent(in):: i0, i1      ! compute domain dimension in E-W
  integer, intent(in):: j0, j1      ! compute domain dimension in N-S
  integer, intent(in):: ifirst, ilast, jfirst, jlast ! tracer array dimensions
  integer, intent(in):: ncnst
  real(kind=R_GRID), intent(in), dimension(ifirst:ilast,jfirst:jlast):: lon, lat
  real, intent(inout):: q(ifirst:ilast,jfirst:jlast,km,ncnst)
  real, intent(in):: delp(ifirst:ilast,jfirst:jlast,km)
! Local var:
  real:: D, k1, r, ll, sinthc, costhc, mm
  integer:: i,j,k
  integer:: Cl, Cl2

  !NOTE: If you change the reaction rates, then you will have to change it both
  ! here and in fv_phys
  real, parameter :: qcly = 4.e-6
  real, parameter :: lc   = 5.*pi/3.
  real, parameter :: thc  = pi/9.
  real, parameter :: k2 = 1.

  sinthc = sin(thc)
  costhc = cos(thc)

  Cl  = get_tracer_index (MODEL_ATMOS, 'Cl')
  Cl2 = get_tracer_index (MODEL_ATMOS, 'Cl2')

  do j=j0,j1
     do i=i0,i1
        k1 = max(0., sin(lat(i,j))*sinthc + cos(lat(i,j))*costhc*cos(lon(i,j) - lc))
        r = k1/k2 * 0.25
        D = sqrt(r*r + 2.*r*qcly)
        q(i,j,1,Cl) = D - r
        q(i,j,1,Cl2) = 0.5*(qcly - q(i,j,1,Cl))
     enddo
  enddo

  do k=2,km
  do j=j0,j1
     do i=i0,i1
        q(i,j,k,Cl)  = q(i,j,1,Cl)
        q(i,j,k,Cl2) = q(i,j,1,Cl2)
     enddo
  enddo
  enddo

  !Compute qcly0
  qcly0 = 0.
  if (is_master()) then
     i = is
     j = js
     mm = 0.
     do k=1,km
        qcly0 = qcly0 + (q(i,j,k,Cl) + 2.*q(i,j,k,Cl2))*delp(i,j,k)
        mm = mm + delp(i,j,k)
     enddo
     qcly0 = qcly0/mm
  endif
  call mpp_sum(qcly0)
  if (is_master()) print*, ' qcly0 = ', qcly0
  

end subroutine terminator_tracers

  subroutine rankine_vortex(ubar, r0, p1, u, v, grid )
!----------------------------
! Rankine vortex
!----------------------------
  real, intent(in):: ubar ! max wind (m/s)
  real, intent(in):: r0   ! Radius of max wind (m)
  real, intent(in):: p1(2)   ! center position (longitude, latitude) in radian
  real, intent(inout):: u(isd:ied,  jsd:jed+1)
  real, intent(inout):: v(isd:ied+1,jsd:jed)
  real(kind=R_GRID), intent(IN) :: grid(isd:ied+1,jsd:jed+1,2)
! local:
  real(kind=R_GRID):: p2(2), p3(2), p4(2)
  real(kind=R_GRID):: e1(3), e2(3), ex(3), ey(3)
  real:: vr, r, d2, cos_p, x1, y1
  real:: utmp, vtmp
  integer i, j

! Compute u-wind
  do j=js,je+1
     do i=is,ie
        call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
! shift:
        p2(1) = p2(1) - p1(1)
        cos_p = sin(p2(2))*sin(p1(2)) + cos(p2(2))*cos(p1(2))*cos(p2(1))  
        r = radius*acos(cos_p)   ! great circle distance
!       if( r<0.) call mpp_error(FATAL, 'radius negative!')
        if( r<r0 ) then
            vr = ubar*r/r0
        else
            vr = ubar*r0/r
        endif
        x1 = cos(p2(2))*sin(p2(1))
        y1 = sin(p2(2))*cos(p1(2)) - cos(p2(2))*sin(p1(2))*cos(p2(1))
        d2 = max(1.e-25, sqrt(x1**2 + y1**2))
        utmp = -vr*y1/d2
        vtmp =  vr*x1/d2
        p3(1) = grid(i,j,  1) - p1(1)
        p3(2) = grid(i,j,  2)
        p4(1) = grid(i+1,j,1) - p1(1)
        p4(2) = grid(i+1,j,2)
        call get_unit_vect2(p3, p4, e1)
        call get_latlon_vector(p2, ex, ey)  ! note: p2 shifted
        u(i,j) = u(i,j) + utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
      enddo
  enddo

! Compute v-wind
  do j=js,je
     do i=is,ie+1
        call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p2)
! shift:
        p2(1) = p2(1) - p1(1)
        cos_p = sin(p2(2))*sin(p1(2)) + cos(p2(2))*cos(p1(2))*cos(p2(1))  
        r = radius*acos(cos_p)   ! great circle distance
        if( r<r0 ) then
            vr = ubar*r/r0
        else
            vr = ubar*r0/r
        endif
        x1 = cos(p2(2))*sin(p2(1))
        y1 = sin(p2(2))*cos(p1(2)) - cos(p2(2))*sin(p1(2))*cos(p2(1))
        d2 = max(1.e-25, sqrt(x1**2 + y1**2))
        utmp = -vr*y1/d2
        vtmp =  vr*x1/d2
        p3(1) = grid(i,j,  1) - p1(1)
        p3(2) = grid(i,j,  2)
        p4(1) = grid(i,j+1,1) - p1(1)
        p4(2) = grid(i,j+1,2)
        call get_unit_vect2(p3, p4, e2)
        call get_latlon_vector(p2, ex, ey)  ! note: p2 shifted
        v(i,j) = v(i,j) + utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
      enddo
  enddo
  end subroutine rankine_vortex



     real function gh_jet(npy, lat_in)
     integer, intent(in):: npy
     real, intent(in):: lat_in
     real lat, lon, dp, uu
     real h0, ft
     integer j,jm

      jm = 4 * npy 
!     h0 = 10.E3
      h0 = 10.157946867E3
      dp = pi / real(jm-1)

     if ( .not. gh_initialized ) then
! SP:
        allocate(gh_table(jm))
        allocate(lats_table(jm))
        gh_table(1) = grav*h0 
        lats_table(1) = -pi/2.
! Using only the mid-point for integration
      do j=2,jm
         lat = -pi/2. + (real(j-1)-0.5)*dp
         uu = u_jet(lat)
         ft = 2.*omega*sin(lat)
         gh_table(j) = gh_table(j-1) - uu*(radius*ft + tan(lat)*uu) * dp
         lats_table(j) = -pi/2. + real(j-1)*dp
      enddo
      gh_initialized = .true.
     endif

     if ( lat_in <= lats_table(1) ) then
          gh_jet = gh_table(1)
          return
     endif
     if ( lat_in >= lats_table(jm) ) then
          gh_jet = gh_table(jm)
          return
     endif

! Search:
     do j=1,jm-1
        if ( lat_in >=lats_table(j) .and. lat_in<=lats_table(j+1) ) then
             gh_jet = gh_table(j) + (gh_table(j+1)-gh_table(j))/dp * (lat_in-lats_table(j))
             return
        endif
     enddo
     end function gh_jet

     real function u_jet(lat)
      real lat, lon, dp
      real umax, en, ph0, ph1

      umax = 80.
      ph0 = pi/7.
      ph1 = pi/2. - ph0
      en =  exp( -4./(ph1-ph0)**2 )

      if ( lat>ph0 .and. lat<ph1 ) then
           u_jet = (umax/en)*exp( 1./( (lat-ph0)*(lat-ph1) ) )
      else
           u_jet = 0.
      endif
     end function u_jet
     
      subroutine get_case9_B(B, agrid)
      real, intent(OUT) :: B(isd:ied,jsd:jed)
      real, intent(IN) :: agrid(isd:ied,jsd:jed,2)
      real :: myC,yy,myB
      integer :: i,j
! Generate B forcing function
!
      gh0 = 720.*grav
      do j=jsd,jed
         do i=isd,ied
            if (sin(agrid(i,j,2)) > 0.) then
               myC = sin(agrid(i,j,1))
                yy = (cos(agrid(i,j,2))/sin(agrid(i,j,2)))**2
               myB = gh0*yy*exp(1.-yy)
               B(i,j) = myB*myC
            else
               B(i,j) = 0.
            endif
         enddo
      enddo

   end subroutine get_case9_B
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!     
   subroutine case9_forcing1(phis,time_since_start)

   real , intent(INOUT) :: phis(isd:ied  ,jsd:jed  )
   real , intent(IN) :: time_since_start
   real :: tday, amean
   integer :: i,j
!
! Generate B forcing function
!
              tday = time_since_start/86400.0
              if (tday >= 20.) then
                 AofT(2) = 0.5*(1.-cos(0.25*PI*(tday-20)))
                 if (tday == 24) AofT(2) = 1.0
              elseif (tday <= 4.) then
                 AofT(2) = 0.5*(1.-cos(0.25*PI*tday))
              elseif (tday <= 16.) then
                 AofT(2) = 1.
              else
                 AofT(2) = 0.5*(1.+cos(0.25*PI*(tday-16.)))
              endif
              amean = 0.5*(AofT(1)+AofT(2))
              do j=jsd,jed
                 do i=isd,ied
                    phis(i,j) = amean*case9_B(i,j)
                enddo
             enddo

   end subroutine case9_forcing1
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!     
   subroutine case9_forcing2(phis)
     real ,      intent(INOUT) :: phis(isd:ied  ,jsd:jed  )
     integer :: i,j
!
! Generate B forcing function
!
          do j=jsd,jed
             do i=isd,ied
                phis(i,j) = AofT(2)*case9_B(i,j)
             enddo
          enddo
          AofT(1) = AofT(2)

   end subroutine case9_forcing2
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

   subroutine case51_forcing(delp, uc, vc, u, v, ua, va, pe, time, dt, gridstruct, npx, npy, npz, ptop, domain)

     real, intent(INOUT) :: delp(isd:ied,jsd:jed,npz)
     real, intent(INOUT) :: uc(isd:ied+1,jsd:jed,npz)
     real, intent(INOUT) :: vc(isd:ied,jsd:jed+1,npz)
     real, intent(INOUT) :: u(isd:ied,jsd:jed+1,npz)
     real, intent(INOUT) :: v(isd:ied+1,jsd:jed,npz)
     real, intent(INOUT) :: ua(isd:ied,jsd:jed,npz)
     real, intent(INOUT) :: va(isd:ied,jsd:jed,npz)
     real, intent(INOUT) :: pe(is-1:ie+1, npz+1,js-1:je+1)  ! edge pressure (pascal)
     real, intent(IN) :: time, dt
     real, intent(INOUT) :: ptop
     integer, intent(IN) :: npx, npy, npz
     type(fv_grid_type), intent(IN), target :: gridstruct
     type(domain2d), intent(INOUT) :: domain

     real :: period
     real :: omega0

     integer :: i,j,k

     real :: s, l, dt2, V0, phase
     real :: ull, vll, lonp
     real :: p0(2), elon(3), elat(3)

     real :: psi(isd:ied,jsd:jed)
     real :: psi_b(isd:ied+1,jsd:jed+1)
     real :: dist, psi1, psi2

     real :: k_cell = 5

     real :: utmp, vtmp
     real(kind=R_GRID) :: e1(3), e2(3), ex(3), ey(3), pt(2), p1(2), p2(2), p3(2), rperiod, timefac, t00

     integer :: wind_field = 1 !Should be the same as tracer_test

     real(kind=R_GRID), pointer, dimension(:,:,:) :: agrid, grid
     real, pointer, dimension(:,:)   :: dx, dxa, dy, dya, dxc, dyc

     agrid => gridstruct%agrid_64
     grid  => gridstruct%grid_64

     dx  => gridstruct%dx
     dxa => gridstruct%dxa
     dxc => gridstruct%dxc
     dy  => gridstruct%dy
     dya => gridstruct%dya
     dyc => gridstruct%dyc

     period = real( 12*24*3600 ) !12 days
     
     l = 2.*pi/period
     dt2 = dt*0.5

     phase = pi*time/period

     !call prt_maxmin('pe', pe,  is, ie, js, je, 0, npz, 1.E-3)

     !Winds: NONDIVERGENT---just use streamfunction!

      psi(:,:) = 1.e25
      psi_b(:,:) = 1.e25


      select case (wind_field)
      case (0)

         omega0 = 23000.*pi/period

         t00 = 300.
         ptop = 100000.*exp(-12000.*grav/t00/rdgas)

         do j=js,je
         do k=1,npz+1
         do i=is,ie
            s = min(1.,2.*sqrt(sin((pe(i,k,j)-ptop)/(pe(i,npz+1,j)-ptop)*pi)))
            pe(i,k,j) = pe(i,k,j) + dt*omega0*sin(agrid(i,j,1)-period*(time+dt2))*cos(agrid(i,j,2))* &
                 cos(period*(time+dt2))*sin(s*0.5*pi)
         enddo
         enddo
         enddo
         
         do k=1,npz
         do j=js,je
         do i=is,ie
            delp(i,j,k) = pe(i,k+1,j) - pe(i,k,j)
         enddo
         enddo
         enddo

         v0 = 10.*RADIUS/period !k in DCMIP document
         ubar = 40.

         do j=jsd,jed
         do i=isd,ied
            psi(i,j) = (-1.0 * Ubar * radius *( sin(agrid(i,j,2))                  *cos(alpha) - &
                                            cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) ) )
         enddo
         enddo
         call mpp_update_domains( psi, domain )
         do j=jsd,jed+1
         do i=isd,ied+1
            psi_b(i,j) = (-1.0 * Ubar * radius *( sin(grid(i,j,2))                 *cos(alpha) - &
                                              cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) ) )
         enddo
         enddo

         k = 1

         do j=js,je+1
         do i=is,ie
            dist = dx(i,j)
            vc(i,j,k) = (psi_b(i+1,j)-psi_b(i,j))/dist
            if (dist==0) vc(i,j,k) = 0.
         enddo
         enddo
         do j=js,je
         do i=is,ie+1
            dist = dy(i,j)
            uc(i,j,k) = -1.0*(psi_b(i,j+1)-psi_b(i,j))/dist
            if (dist==0) uc(i,j,k) = 0.
         enddo
         enddo

         do j=js,je
         do i=is,ie+1
            dist = dxc(i,j)
            v(i,j,k) = (psi(i,j)-psi(i-1,j))/dist
            if (dist==0) v(i,j,k) = 0.
         enddo
         enddo
         do j=js,je+1
         do i=is,ie
            dist = dyc(i,j)
            u(i,j,k) = -1.0*(psi(i,j)-psi(i,j-1))/dist
            if (dist==0) u(i,j,k) = 0.
         enddo
         enddo

         do j=js,je
         do i=is,ie
            psi1 = 0.5*(psi(i,j)+psi(i,j-1))
            psi2 = 0.5*(psi(i,j)+psi(i,j+1))
            dist = dya(i,j)
            ua(i,j,k) = -1.0 * (psi2 - psi1) / (dist)
            if (dist==0) ua(i,j,k) = 0.
            psi1 = 0.5*(psi(i,j)+psi(i-1,j))
            psi2 = 0.5*(psi(i,j)+psi(i+1,j))
            dist = dxa(i,j)
            va(i,j,k) = (psi2 - psi1) / (dist)
            if (dist==0) va(i,j,k) = 0.
         enddo
         enddo

      case (1)

         omega0 = 23000.*pi/period

         do j=js,je
         do k=1,npz+1
         do i=is,ie
            s = min(1.,2.*sqrt(sin((pe(i,k,j)-ptop)/(pe(i,npz+1,j)-ptop)*pi)))
            pe(i,k,j) = pe(i,k,j) + dt*omega0*sin(agrid(i,j,1)-period*(time+dt2))*cos(agrid(i,j,2))* &
                 cos(period*(time+dt2))*sin(s*0.5*pi)
         enddo
         enddo
         enddo
         
         do k=1,npz
         do j=js,je
         do i=is,ie
            delp(i,j,k) = pe(i,k+1,j) - pe(i,k,j)
         enddo
         enddo
         enddo

         ubar = 10.*RADIUS/period !k in DCMIP document


         do j=js,je
            do i=is,ie+1
               p1(:) = grid(i  ,j ,1:2)
               p2(:) = grid(i,j+1 ,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e2) !! e2 is WRONG in halo??
               call get_latlon_vector(p3, ex, ey)
               l = p3(1) - 2.*pi*time/period
               utmp = ubar * sin(l)**2 * sin(2.*p3(2)) * cos(pi*time/period) + 2.*pi*RADIUS/period*cos(p3(2))
               vtmp = ubar * sin(2.*l) * cos(p3(2)) * cos(pi*time/period)
               v(i,j,1) = utmp*inner_prod(e2,ex) + vtmp*inner_prod(e2,ey)
            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               p1(:) = grid(i,  j,1:2)
               p2(:) = grid(i+1,j,1:2)
               call mid_pt_sphere(p1, p2, p3)
               call get_unit_vect2(p1, p2, e1)
               call get_latlon_vector(p3, ex, ey)
               l = p3(1) - 2.*pi*time/period
               utmp = ubar * sin(l)**2 * sin(2.*p3(2)) * cos(pi*time/period) + 2.*pi*RADIUS/period*cos(p3(2))
               vtmp = ubar * sin(2.*l) * cos(p3(2)) * cos(pi*time/period)
               u(i,j,1) = utmp*inner_prod(e1,ex) + vtmp*inner_prod(e1,ey)
            enddo
         enddo

         call mp_update_dwinds(u(:,:,1), v(:,:,1), npx, npy, domain)

! copy vertically; no wind shear
        do k=2,npz
           do j=jsd,jed+1
              do i=isd,ied
                 u(i,j,k) = u(i,j,1)
              enddo
           enddo
           do j=jsd,jed
              do i=isd,ied+1
                 v(i,j,k) = v(i,j,1)
              enddo
           enddo
        enddo

        call mp_update_dwinds(u, v, npx, npy, npz, domain)

         call dtoa( u(:,:,1), v(:,:,1),ua(:,:,1),va(:,:,1),dx,dy,dxa,dya,dxc,dyc,npx,npy,ng)
         call mpp_update_domains( ua, va, domain, gridtype=AGRID_PARAM) !! ABSOLUTELY NECESSARY!!
         call atoc(ua(:,:,1),va(:,:,1),uc(:,:,1),vc(:,:,1),dx,dy,dxa,dya,npx,npy,ng, gridstruct%nested, domain)
        
        do k=2,npz
           do j=js,je
              do i=is,ie
                 ua(i,j,k) = ua(i,j,1)
              enddo
           enddo
           do j=js,je
              do i=is,ie
                 va(i,j,k) = va(i,j,1)
              enddo
           enddo
        enddo

        do k=2,npz
           do j=js,je+1
              do i=is,ie
                 vc(i,j,k) = vc(i,j,1)
              enddo
           enddo
           do j=js,je
              do i=is,ie+1
                 uc(i,j,k) = uc(i,j,1)
              enddo
           enddo
        enddo

         !cases 2 and 3 are not nondivergent so we cannot use a streamfunction.
      case (2)

         omega0 = 0.25

         do j=js,je
         do k=1,npz+1
         do i=is,ie
            pe(i,k,j) = pe(i,k,j) + dt*omega0*grav*pe(i,k,j)/rdgas/300./k_cell* &
                 (-2.*sin(k_cell*agrid(i,j,2))*sin(agrid(i,j,2)) + k_cell*cos(agrid(i,j,2))*cos(k_cell*agrid(i,j,2)))* &
                 sin(pi*zz0(k)/12000.)*cos(phase)
         enddo
         enddo
         enddo
         
         do k=1,npz
         do j=js,je
         do i=is,ie
            delp(i,j,k) = pe(i,k+1,j) - pe(i,k,j)
         enddo
         enddo
         enddo

         ubar = 40.

         !Set lat-lon A-grid winds 
         k = 1
         do j=js,je
         do i=is,ie
            utmp = ubar*cos(agrid(i,j,2))
            vtmp = - RADIUS * omega0 * pi / k_cell / 12000. * &
                 cos(agrid(i,j,2)) * sin(k_cell * agrid(i,j,2)) * &
                 sin(pi*zz0(k)/12000.)*cos(phase)
         enddo
         enddo

      end select

      do k=2,npz
         u(:,:,k) = u(:,:,1)
         v(:,:,k) = v(:,:,1)
         uc(:,:,k) = uc(:,:,1)
         vc(:,:,k) = vc(:,:,1)
         ua(:,:,k) = ua(:,:,1)
         va(:,:,k) = va(:,:,1)
      enddo

      call mpp_update_domains( uc, vc, domain, gridtype=CGRID_NE_PARAM)
      call fill_corners(uc, vc, npx, npy, npz, VECTOR=.true., CGRID=.true.)
      call mp_update_dwinds(u, v, npx, npy, npz, domain)

      nullify(agrid)
      nullify(grid)

      nullify(dx)
      nullify(dxa)
      nullify(dy)
      nullify(dya)

   end subroutine case51_forcing

!-------------------------------------------------------------------------------
!     
!      get_stats :: get L-1, L-2, and L-inf norms and other stats as defined
!                                                in Williamson, 1994 (p.16)
       subroutine get_stats(dt, dtout, nt, maxnt, ndays, u,v,pt,delp,q,phis, ps, &
                            uc,vc, ua,va, npx, npy, npz, ncnst, ndims, nregions,    &
                            gridstruct, stats_lun, consv_lun, monitorFreq, tile, &
                            domain, nested)
         integer,      intent(IN) :: nt, maxnt
         real  ,    intent(IN) :: dt, dtout, ndays
         real ,      intent(INOUT) ::    u(isd:ied  ,jsd:jed+1,npz)
         real ,      intent(INOUT) ::    v(isd:ied+1,jsd:jed  ,npz)
         real ,      intent(INOUT) ::   pt(isd:ied  ,jsd:jed  ,npz)
         real ,      intent(INOUT) :: delp(isd:ied  ,jsd:jed  ,npz)
         real ,      intent(INOUT) ::    q(isd:ied  ,jsd:jed  ,npz, ncnst)
         real ,      intent(INOUT) :: phis(isd:ied  ,jsd:jed  )
         real ,      intent(INOUT) ::   ps(isd:ied  ,jsd:jed  )
         real ,      intent(INOUT) ::   uc(isd:ied+1,jsd:jed  ,npz)
         real ,      intent(INOUT) ::   vc(isd:ied  ,jsd:jed+1,npz)
         real ,      intent(INOUT) ::   ua(isd:ied  ,jsd:jed  ,npz)
         real ,      intent(INOUT) ::   va(isd:ied  ,jsd:jed  ,npz)
         integer,      intent(IN) :: npx, npy, npz, ncnst, tile
         integer,      intent(IN) :: ndims
         integer,      intent(IN) :: nregions
         integer,      intent(IN) :: stats_lun
         integer,      intent(IN) :: consv_lun
         integer,      intent(IN) :: monitorFreq
         type(fv_grid_type), target :: gridstruct
         type(domain2d), intent(INOUT) :: domain
         logical, intent(IN) :: nested

         real   :: L1_norm
         real   :: L2_norm
         real   :: Linf_norm
         real   :: pmin, pmin1, uamin1, vamin1
         real   :: pmax, pmax1, uamax1, vamax1
         real(kind=4) :: arr_r4(5)
         real   :: tmass0, tvort0, tener0, tKE0
         real   :: tmass, tvort, tener, tKE
         real   :: temp(is:ie,js:je)
         integer :: i0, j0, k0, n0
         integer :: i, j, k, n, iq

         real :: psmo, Vtx, p, w_p, p0
         real :: x1,y1,z1,x2,y2,z2,ang

         real   :: p1(2), p2(2), p3(2), r, r0, dist, heading

         real :: uc0(isd:ied+1,jsd:jed  ,npz)
         real :: vc0(isd:ied  ,jsd:jed+1,npz)

         real :: myDay
         integer :: myRec

         real, save, allocatable, dimension(:,:,:) :: u0, v0
         real  ::    up(isd:ied  ,jsd:jed+1,npz)
         real  ::    vp(isd:ied+1,jsd:jed  ,npz)

         real, dimension(:,:,:), pointer :: grid, agrid
         real, dimension(:,:),   pointer :: area, f0, dx, dy, dxa, dya, dxc, dyc
         
         grid => gridstruct%grid
         agrid=> gridstruct%agrid

         area  => gridstruct%area
         f0    => gridstruct%f0

         dx      => gridstruct%dx
         dy      => gridstruct%dy
         dxa     => gridstruct%dxa
         dya     => gridstruct%dya
         dxc     => gridstruct%dxc
         dyc     => gridstruct%dyc

         !!! DEBUG CODE
         if (nt == 0 .and. is_master()) print*, 'INITIALIZING GET_STATS'
         !!! END DEBUG CODE

         myDay = ndays*((FLOAT(nt)/FLOAT(maxnt)))

#if defined(SW_DYNAMICS)
      if (test_case==0) then
         phi0 = 0.0
         do j=js,je
            do i=is,ie
               x1 = agrid(i,j,1)
               y1 = agrid(i,j,2)
               z1 = radius
               p = p0_c0 * cos(y1)
               Vtx = ((3.0*SQRT(2.0))/2.0) * (( 1.0/cosh(p) )**2.0) * tanh(p)
               w_p = 0.0
               if (p /= 0.0) w_p = Vtx/p
              ! delp(i,j,1) = 1.0 - tanh( (p/rgamma) * sin(x1 - w_p*(nt*dt/86400.0)) )
               phi0(i,j,1) = 1.0 - tanh( (p/rgamma) * sin(x1 - w_p*(nt*dt/86400.0)) )
            enddo
         enddo
      elseif (test_case==1) then
! Get Current Height Field "Truth"
         p1(1) = pi/2.  + pi_shift
         p1(2) = 0.
         p2(1) = 3.*pi/2.  + pi_shift
         p2(2) = 0.
         r0 = radius/3. !RADIUS 3.
         dist = 2.0*pi*radius* ((FLOAT(nt)/FLOAT(maxnt)))
         heading = 3.0*pi/2.0 - alpha !5.0*pi/2.0 - alpha
         call get_pt_on_great_circle( p1, p2, dist, heading, p3)
         phi0 = 0.0
         do j=js,je
            do i=is,ie
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = great_circle_dist( p3, p2, radius )
               if (r < r0) then
                  phi0(i,j,1) = phis(i,j) + gh0*0.5*(1.0+cos(PI*r/r0))
               else
                  phi0(i,j,1) = phis(i,j)
               endif
            enddo
         enddo
     endif

! Get Height Field Stats
         call pmxn(delp(:,:,1), npx, npy, nregions, tile, gridstruct, pmin1, pmax1, i0, j0, n0)
         pmin1=pmin1/Grav
         pmax1=pmax1/Grav
         if (test_case <= 2) then
            call get_scalar_stats( delp(:,:,1), phi0(:,:,1), npx, npy, ndims, nregions, &
                                   pmin, pmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
            pmin=pmin/Grav
            pmax=pmax/Grav
            arr_r4(1) = pmin1
            arr_r4(2) = pmax1
            arr_r4(3) = L1_norm
            arr_r4(4) = L2_norm
            arr_r4(5) = Linf_norm
            !if (is_master()) write(stats_lun,rec=(nt)*2 + 1) arr_r4
         else
            arr_r4(1) = pmin1
            arr_r4(2) = pmax1
            arr_r4(3:5) = 0.
            pmin      = 0.
            pmax      = 0.
            L1_norm   = 0.
            L2_norm   = 0.
            Linf_norm = 0.
         endif

 200  format(i6.6,A,i6.6,A,e21.14)
 201  format('          ',A,e21.14,' ',e21.14)
 202  format('          ',A,i4.4,'x',i4.4,'x',i4.4)

         if ( (is_master()) .and. MOD(nt,monitorFreq)==0 ) then
             write(*,200) nt, ' step of ', maxnt, ' DAY ', myDay
             write(*,201) 'Height MAX        : ', pmax1
             write(*,201) 'Height MIN        : ', pmin1
             write(*,202) 'HGT MAX location  : ', i0, j0, n0
             if (test_case <= 2) then
                write(*,201) 'Height L1_norm    : ', L1_norm
                write(*,201) 'Height L2_norm    : ', L2_norm
                write(*,201) 'Height Linf_norm  : ', Linf_norm
             endif
         endif

! Get UV Stats
         call dtoa(u , v , ua, va, dx,dy,dxa,dya,dxc,dyc,npx, npy, ng)
         call pmxn(ua(:,:,1), npx, npy, nregions, tile, gridstruct, pmin1, pmax1, i0, j0, n0)
         if (test_case <= 2) then
            call get_vector_stats( ua(:,:,1), ua0(:,:,1), va(:,:,1), va0(:,:,1), npx, npy, ndims, nregions, &
                                   pmin, pmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
         endif
         arr_r4(1) = pmin1
         arr_r4(2) = pmax1
         arr_r4(3) = L1_norm
         arr_r4(4) = L2_norm
         arr_r4(5) = Linf_norm
         !if (is_master()) write(stats_lun,rec=(nt)*2 + 2) arr_r4
         if ( (is_master()) .and. MOD(nt,monitorFreq)==0) then
             write(*,201) 'UV     MAX        : ', pmax1
             write(*,201) 'UV     MIN        : ', pmin1
             write(*,202) 'UV  MAX location  : ', i0, j0, n0
             if (test_case <= 2) then
                write(*,201) 'UV     L1_norm    : ', L1_norm
                write(*,201) 'UV     L2_norm    : ', L2_norm
                write(*,201) 'UV     Linf_norm  : ', Linf_norm
             endif
         endif
#else

 200  format(i6.6,A,i6.6,A,e10.4)
 201  format('          ',A,e10.4,' ',e10.4,' ',i4.4,'x',i4.4,'x',i4.4,'x',i4.4)
 202  format('          ',A,e10.4,' ',e10.4,' ',i4.4,'x',i4.4,'x',i4.4,'x',i4.4,' ',e10.4)
 203  format('          ',A,i3.3,A,e10.4,' ',e10.4,' ',i4.4,'x',i4.4,'x',i4.4,'x',i4.4)

      if(is_master()) write(*,200) nt, ' step of ', maxnt, ' DAY ', myDay

! Surface Pressure
     psmo = globalsum(ps(is:ie,js:je), npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
     if(is_master()) write(*,*) '         Total surface pressure =', 0.01*psmo
     call pmxn(ps, npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
     if (is_master()) then
        write(*,201) 'PS   MAX|MIN      : ', 0.01*pmax, 0.01*pmin, i0, j0, n0
     endif

! Get PT Stats
         pmax1 = -1.e25 
         pmin1 =  1.e25  
         i0=-999
         j0=-999
         k0=-999
         n0=-999
         do k=1,npz 
            call pmxn(pt(:,:,k), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            pmin1 = min(pmin, pmin1)
            pmax1 = max(pmax, pmax1)
            if (pmax1 == pmax) k0 = k
         enddo
         if (is_master()) then
             write(*,201) 'PT   MAX|MIN      : ', pmax1, pmin1, i0, j0, k0, n0
         endif

#if defined(DEBUG_TEST_CASES)
     if(is_master()) write(*,*) ' '
         do k=1,npz
            pmax1 = -1.e25
            pmin1 =  1.e25
            i0=-999
            j0=-999
            k0=-999
            n0=-999
            call pmxn(pt(:,:,k), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            pmin1 = min(pmin, pmin1)
            pmax1 = max(pmax, pmax1)
            if (is_master()) then
                write(*,202) 'PT   MAX|MIN      : ', pmax1, pmin1, i0, j0, k, n0, 0.5*( (ak(k)+ak(k+1))/1.e5 + bk(k)+bk(k+1) )
            endif
         enddo
     if(is_master()) write(*,*) ' '
#endif

! Get DELP Stats
         pmax1 = -1.e25 
         pmin1 =  1.e25 
         i0=-999
         j0=-999
         k0=-999
         n0=-999
         do k=1,npz
            call pmxn(delp(:,:,k), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            pmin1 = min(pmin, pmin1)
            pmax1 = max(pmax, pmax1)
            if (pmax1 == pmax) k0 = k
         enddo
         if (is_master()) then
             write(*,201) 'Delp MAX|MIN      : ', pmax1, pmin1, i0, j0, k0, n0
         endif

! Get UV Stats
         uamax1 = -1.e25
         uamin1 =  1.e25
         i0=-999
         j0=-999
         k0=-999
         n0=-999
         do k=1,npz
            call dtoa(u(isd,jsd,k), v(isd,jsd,k), ua(isd,jsd,k), va(isd,jsd,k), dx,dy,dxa,dya,dxc,dyc,npx, npy, ng)
            call pmxn(ua(:,:,k), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            uamin1 = min(pmin, uamin1)
            uamax1 = max(pmax, uamax1)
            if (uamax1 == pmax) k0 = k
         enddo
         if (is_master()) then
             write(*,201) 'U    MAX|MIN      : ', uamax1, uamin1, i0, j0, k0, n0
         endif

         vamax1 = -1.e25
         vamin1 =  1.e25
         i0=-999
         j0=-999
         k0=-999
         n0=-999
         do k=1,npz
            call pmxn(va(:,:,k), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            vamin1 = min(pmin, vamin1)
            vamax1 = max(pmax, vamax1)
            if (vamax1 == pmax) k0 = k
         enddo
         if (is_master()) then
             write(*,201) 'V    MAX|MIN      : ', vamax1, vamin1, i0, j0, k0, n0
         endif

! Get Q Stats
         pmax1 = -1.e25 
         pmin1 =  1.e25 
         i0=-999
         j0=-999
         k0=-999
         n0=-999
         do k=1,npz
            call pmxn(q(isd,jsd,k,1), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            pmin1 = min(pmin, pmin1)
            pmax1 = max(pmax, pmax1)
            if (pmax1 == pmax) k0 = k
         enddo
         if (is_master()) then
             write(*,201) 'Q    MAX|MIN      : ', pmax1, pmin1, i0, j0, k0, n0
         endif

! Get tracer Stats
       do iq=2,ncnst
         pmax1 = -1.e25
         pmin1 =  1.e25
         i0=-999
         j0=-999
         k0=-999
         n0=-999
         do k=1,npz
            call pmxn(q(isd,jsd,k,iq), npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
            pmin1 = min(pmin, pmin1)
            pmax1 = max(pmax, pmax1)
            if (pmax1 == pmax) k0 = k
         enddo
         if (is_master()) then
             write(*,203) 'TR',iq-1,' MAX|MIN      : ', pmax1, pmin1, i0, j0, k0, n0
         endif
       enddo

#endif

      if (test_case == 12) then
! Get UV Stats
          call get_vector_stats( ua(:,:,22), ua0(:,:,22), va(:,:,22), va0(:,:,22), npx, npy, ndims, nregions, &
                                 pmin, pmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
          if (is_master()) then
             write(*,201) 'UV(850) L1_norm    : ', L1_norm
             write(*,201) 'UV(850) L2_norm    : ', L2_norm
             write(*,201) 'UV(850) Linf_norm  : ', Linf_norm
          endif
      endif 

      tmass = 0.0
      tKE   = 0.0
      tener = 0.0
      tvort = 0.0
#if defined(SW_DYNAMICS)
      do k=1,1
#else
      do k=1,npz
#endif
! Get conservation Stats

! Conservation of Mass
         temp(:,:) = delp(is:ie,js:je,k)
         tmass0 = globalsum(temp, npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         tmass = tmass + tmass0

         !if (.not. allocated(u0, v0)) then
         if (nt == 0) then
            allocate(u0(isd:ied,jsd:jed+1,npz))
            allocate(v0(isd:ied+1,jsd:jed,npz))
            u0 = u
            v0 = v
         endif
         
         !! UA is the PERTURBATION now
         up = u - u0
         vp = v - v0

         call dtoa(up(isd,jsd,k), vp(isd,jsd,k), ua, va, dx,dy, dxa, dya, dxc, dyc, npx, npy, ng)
         call atoc(ua(isd,jsd,k),va(isd,jsd,k),uc0(isd,jsd,k),vc0(isd,jsd,k),dx,dy,dxa,dya,npx,npy,ng,nested, domain, noComm=.true.)
! Conservation of Kinetic Energy
         do j=js,je
            do i=is,ie
                  temp(i,j) = ( uc0(i,j,k)*uc0(i,j,k) + uc0(i+1,j,k)*uc0(i+1,j,k) + &
                                vc0(i,j,k)*vc0(i,j,k) + vc0(i,j+1,k)*vc0(i,j+1,k) )
            enddo
         enddo
         tKE0 = globalsum(temp, npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         tKE = tKE + tKE0

! Conservation of Energy
         do j=js,je
            do i=is,ie
                  temp(i,j) = 0.5 * (delp(i,j,k)/Grav) * temp(i,j)  ! Include Previously calcullated KE 
                  temp(i,j) = temp(i,j) + &
                          Grav*((delp(i,j,k)/Grav + phis(i,j))*(delp(i,j,k)/Grav + phis(i,j))) - &
                          phis(i,j)*phis(i,j)
            enddo
         enddo
         tener0 = globalsum(temp, npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         tener = tener + tener0

! Conservation of Potential Enstrophy
         if (test_case>1) then
            do j=js,je
               do i=is,ie
                  temp(i,j) =  f0(i,j) + (1./area(i,j)) * ( (v(i+1,j,k)*dy(i+1,j) - v(i,j,k)*dy(i,j)) - &
                                                            (u(i,j+1,k)*dx(i,j+1) - u(i,j,k)*dx(i,j)) )
                  temp(i,j) = ( Grav*(temp(i,j)*temp(i,j))/delp(i,j,k) )
               enddo
            enddo
            tvort0 = globalsum(temp, npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
            tvort = tvort + tvort0
         else
            tvort=1.
         endif
      enddo

         if (nt == 0) then
            tmass_orig = tmass
            tener_orig = tener
            tvort_orig = tvort
         endif 
         arr_r4(1) = (tmass-tmass_orig)/tmass_orig
         arr_r4(2) = (tener-tener_orig)/tener_orig
         arr_r4(3) = (tvort-tvort_orig)/tvort_orig
         arr_r4(4) = tKE
         if (test_case==12) arr_r4(4) = L2_norm 
#if defined(SW_DYNAMICS)
         myRec = nt+1
#else
         myRec = myDay*86400.0/dtout + 1 
#endif
         if (is_master()) write(consv_lun,rec=myRec) arr_r4(1:4)
#if defined(SW_DYNAMICS)
         if ( (is_master()) .and. MOD(nt,monitorFreq)==0) then
#else
         if ( (is_master()) ) then 
#endif
             write(*,201) 'MASS TOTAL        : ', tmass
             write(*,201) 'NORMALIZED MASS   : ', (tmass-tmass_orig)/tmass_orig
             if (test_case >= 2) then
                write(*,201) 'Kinetic Energy KE : ', tKE
                write(*,201) 'ENERGY TOTAL      : ', tener
                write(*,201) 'NORMALIZED ENERGY : ', (tener-tener_orig)/tener_orig
                write(*,201) 'ENSTR TOTAL       : ', tvort
                write(*,201) 'NORMALIZED ENSTR  : ', (tvort-tvort_orig)/tvort_orig
             endif
             write(*,*) ' '
         endif

         nullify(grid)
         nullify(agrid)
         nullify(area)
         nullify(f0)
         nullify(dx)
         nullify(dy)

      end subroutine get_stats



   subroutine get_pt_on_great_circle(p1, p2, dist, heading, p3) 
!     get_pt_on_great_circle :: Get the mid-point on a great circle given:
!                                 -2 points (Lon/Lat) to define a great circle
!                                 -Great Cirle distance between 2 defining points
!                                 -Heading
!                              compute:
!                                 Arrival Point (Lon/Lat)

         real , intent(IN)  :: p1(2), p2(2)
         real , intent(IN)  :: dist
         real , intent(IN)  :: heading
         real , intent(OUT) :: p3(2)

         real  pha, dp

         pha = dist/radius

         p3(2) = ASIN( (COS(heading)*COS(p1(2))*SIN(pha)) + (SIN(p1(2))*COS(pha)) )
         dp = ATAN2( SIN(heading)*SIN(pha)*COS(p1(2)) , COS(pha) - SIN(p1(2))*SIN(p3(2)) )
         p3(1) = MOD( (p1(1)-pi)-dp+pi , 2.*pi ) !- pi Leave at 0 to 360

      end subroutine get_pt_on_great_circle
 

!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!      get_scalar_stats: get L-1, L-2, and L-inf norms and min/max stats as defined
!                                                in Williamson, 1994 (p.16)
!                     for any var

       subroutine get_scalar_stats(var, varT, npx, npy, ndims, nregions, &
                            vmin, vmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
         integer,      intent(IN) :: npx, npy
         integer,      intent(IN) :: ndims
         integer,      intent(IN) :: nregions, tile
         real  ,    intent(IN) ::  var(isd:ied,jsd:jed)
         real  ,    intent(IN) :: varT(isd:ied,jsd:jed)
         real  ,   intent(OUT) :: vmin
         real  ,   intent(OUT) :: vmax
         real  ,   intent(OUT) :: L1_norm
         real  ,   intent(OUT) :: L2_norm
         real  ,   intent(OUT) :: Linf_norm

         type(fv_grid_type), target :: gridstruct

         real   :: vmean
         real   :: vvar
         real   :: vmin1
         real   :: vmax1
         real   :: pdiffmn
         real   :: pdiffmx

         real   :: varSUM, varSUM2, varMAX
         real   :: gsum
         real   :: vminT, vmaxT, vmeanT, vvarT
         integer :: i0, j0, n0

         real, dimension(:,:,:), pointer :: grid, agrid
         real, dimension(:,:),   pointer :: area
         
         grid => gridstruct%grid
         agrid=> gridstruct%agrid

         area  => gridstruct%area

         varSUM = 0.
         varSUM2 = 0.
         varMAX = 0.
         L1_norm = 0.
         L2_norm = 0.
         Linf_norm = 0.
         vmean  = 0.
         vvar   = 0.
         vmax   = 0.
         vmin   = 0.
         pdiffmn= 0.
         pdiffmx= 0.
         vmeanT = 0.
         vvarT  = 0.
         vmaxT  = 0.
         vminT  = 0.

         vmean   = globalsum(var(is:ie,js:je) , npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         vmeanT  = globalsum(varT(is:ie,js:je), npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         vmean  = vmean  / (4.0*pi)
         vmeanT = vmeanT / (4.0*pi)

         call pmxn(var, npx, npy, nregions, tile, gridstruct, vmin , vmax , i0, j0, n0)
         call pmxn(varT, npx, npy, nregions, tile, gridstruct, vminT, vmaxT, i0, j0, n0)
         call pmxn(var-varT, npx, npy, nregions, tile, gridstruct, pdiffmn, pdiffmx, i0, j0, n0)

         vmax = (vmax - vmaxT) / (vmaxT-vminT)
         vmin = (vmin - vminT) / (vmaxT-vminT)

         varSUM  = globalsum(varT(is:ie,js:je), npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         varSUM2 = globalsum(varT(is:ie,js:je)**2., npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L1_norm = globalsum(ABS(var(is:ie,js:je)-varT(is:ie,js:je)), npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L2_norm = globalsum((var(is:ie,js:je)-varT(is:ie,js:je))**2., npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L1_norm = L1_norm/varSUM
         L2_norm = SQRT(L2_norm)/SQRT(varSUM2)

         call pmxn(ABS(varT), npx, npy, nregions, tile, gridstruct, vmin, vmax, i0, j0, n0)
         varMAX = vmax
         call pmxn(ABS(var-varT), npx, npy, nregions, tile, gridstruct, vmin, vmax, i0, j0, n0)
         Linf_norm = vmax/varMAX

      end subroutine get_scalar_stats
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!      get_vector_stats: get L-1, L-2, and L-inf norms and min/max stats as defined
!                                                in Williamson, 1994 (p.16)
!                     for any var

       subroutine get_vector_stats(varU, varUT, varV, varVT, &
                            npx, npy, ndims, nregions, &
                            vmin, vmax, L1_norm, L2_norm, Linf_norm, gridstruct, tile)
         integer,      intent(IN) :: npx, npy
         integer,      intent(IN) :: ndims
         integer,      intent(IN) :: nregions, tile
         real  ,    intent(IN) ::  varU(isd:ied,jsd:jed)
         real  ,    intent(IN) :: varUT(isd:ied,jsd:jed)
         real  ,    intent(IN) ::  varV(isd:ied,jsd:jed)
         real  ,    intent(IN) :: varVT(isd:ied,jsd:jed)
         real  ,   intent(OUT) :: vmin
         real  ,   intent(OUT) :: vmax
         real  ,   intent(OUT) :: L1_norm
         real  ,   intent(OUT) :: L2_norm
         real  ,   intent(OUT) :: Linf_norm

         real   ::  var(isd:ied,jsd:jed)
         real   :: varT(isd:ied,jsd:jed)
         real   :: vmean
         real   :: vvar
         real   :: vmin1
         real   :: vmax1
         real   :: pdiffmn
         real   :: pdiffmx

         real   :: varSUM, varSUM2, varMAX
         real   :: gsum
         real   :: vminT, vmaxT, vmeanT, vvarT
         integer :: i,j,n
         integer :: i0, j0, n0

         type(fv_grid_type), target :: gridstruct

         real, dimension(:,:,:), pointer :: grid, agrid
         real, dimension(:,:),   pointer :: area
         
         grid => gridstruct%grid
         agrid=> gridstruct%agrid

         area  => gridstruct%area

         varSUM = 0.
         varSUM2 = 0.
         varMAX = 0.
         L1_norm = 0.
         L2_norm = 0.
         Linf_norm = 0.
         vmean  = 0.
         vvar   = 0.
         vmax   = 0.
         vmin   = 0.
         pdiffmn= 0.
         pdiffmx= 0.
         vmeanT = 0.
         vvarT  = 0.
         vmaxT  = 0.
         vminT  = 0.

         do j=js,je
            do i=is,ie
               var(i,j) = SQRT( (varU(i,j)-varUT(i,j))**2. + &
                                (varV(i,j)-varVT(i,j))**2. )
               varT(i,j) = SQRT( varUT(i,j)*varUT(i,j) + &
                                 varVT(i,j)*varVT(i,j) )
            enddo
         enddo
         varSUM  = globalsum(varT(is:ie,js:je), npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L1_norm = globalsum(var(is:ie,js:je) , npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L1_norm = L1_norm/varSUM

         call pmxn(varT, npx, npy, nregions, tile, gridstruct, vmin, vmax, i0, j0, n0)
         varMAX = vmax
         call pmxn(var, npx, npy, nregions, tile, gridstruct, vmin, vmax, i0, j0, n0)
         Linf_norm = vmax/varMAX

         do j=js,je
            do i=is,ie
               var(i,j) = ( (varU(i,j)-varUT(i,j))**2. + &
                            (varV(i,j)-varVT(i,j))**2. )
              varT(i,j) = ( varUT(i,j)*varUT(i,j) + &
                            varVT(i,j)*varVT(i,j) )
            enddo
         enddo
         varSUM  = globalsum(varT(is:ie,js:je), npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L2_norm = globalsum(var(is:ie,js:je) , npx, npy, is,ie, js,je, isd, ied, jsd, jed, gridstruct, tile)
         L2_norm = SQRT(L2_norm)/SQRT(varSUM)

      end subroutine get_vector_stats
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!     check_courant_numbers :: 
!
       subroutine check_courant_numbers(uc,vc, ndt, n_split, gridstruct, npx, npy, npz, tile, noPrint)

       real, intent(IN) :: ndt
       integer, intent(IN) :: n_split
       integer, intent(IN) :: npx, npy, npz, tile
       logical, OPTIONAL, intent(IN) :: noPrint
       real ,      intent(IN) ::   uc(isd:ied+1,jsd:jed  ,npz)
       real ,      intent(IN) ::   vc(isd:ied  ,jsd:jed+1,npz)
 
       real :: ideal_c=0.06
       real :: tolerance= 1.e-3
       real :: dt_inc, dt_orig 
       real   :: meanCy, minCy, maxCy, meanCx, minCx, maxCx

       real :: counter
       logical :: ideal 

       integer :: i,j,k
       real :: dt

       type(fv_grid_type), intent(IN), target :: gridstruct
       real, dimension(:,:), pointer :: dxc, dyc

       dxc => gridstruct%dxc
       dyc => gridstruct%dyc

       dt = ndt/real(n_split)

 300  format(i4.4,' ',i4.4,' ',i4.4,' ',i4.4,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14,' ',e21.14)

       dt_orig = dt
       dt_inc = 1
       ideal = .false.

       do while(.not. ideal)
       
         counter = 0
         minCy = missing
         maxCy = -1.*missing
         minCx = missing
         maxCx = -1.*missing
         meanCx = 0
         meanCy = 0
         do k=1,npz
         do j=js,je
            do i=is,ie+1
               minCx = MIN(minCx, ABS( (dt/dxc(i,j))*uc(i,j,k) ))
               maxCx = MAX(maxCx, ABS( (dt/dxc(i,j))*uc(i,j,k) ))
               meanCx = meanCx + ABS( (dt/dxc(i,j))*uc(i,j,k) )

        if (ABS( (dt/dxc(i,j))*uc(i,j,k) ) > 1.0) then
           counter = counter+1
           write(*,300) i,j,k,tile, ABS( (dt/dxc(i,j))*uc(i,j,k) ), dt, dxc(i,j), uc(i,j,k), counter 
           call exit(1)
        endif

            enddo
         enddo
         do j=js,je+1
            do i=is,ie
               minCy = MIN(minCy, ABS( (dt/dyc(i,j))*vc(i,j,k) ))
               maxCy = MAX(maxCy, ABS( (dt/dyc(i,j))*vc(i,j,k) ))
               meanCy = meanCy + ABS( (dt/dyc(i,j))*vc(i,j,k) )

        if (ABS( (dt/dyc(i,j))*vc(i,j,k) ) > 1.0) then
           counter = counter+1
           write(*,300) i,j,k,tile, ABS( (dt/dyc(i,j))*vc(i,j,k) ), dt, dyc(i,j), vc(i,j,k), counter
           call exit(1)
        endif

            enddo
         enddo
         enddo

         call mp_reduce_max(maxCx)
         call mp_reduce_max(maxCy)
         minCx = -minCx
         minCy = -minCy
         call mp_reduce_max(minCx)
         call mp_reduce_max(minCy)
         minCx = -minCx
         minCy = -minCy
         call mp_reduce_sum(meanCx)
         call mp_reduce_sum(meanCy)
         meanCx = meanCx/(6.0*DBLE(npx)*DBLE(npy-1))
         meanCy = meanCy/(6.0*DBLE(npx-1)*DBLE(npy))

         !if ( (ABS(maxCy-ideal_c) <= tolerance) .and. (ABS(maxCx-ideal_c) <= tolerance) ) then 
            ideal = .true. 
         !elseif (maxCy-ideal_c > 0) then
         !   dt = dt - dt_inc 
         !else
         !   dt = dt + dt_inc
         !endif

      enddo

         if ( (.not. present(noPrint)) .and. (is_master()) ) then
            print*, ''
            print*, '--------------------------------------------'
            print*, 'Y-dir Courant number MIN  : ', minCy
            print*, 'Y-dir Courant number MAX  : ', maxCy
            print*, ''
            print*, 'X-dir Courant number MIN  : ', minCx
            print*, 'X-dir Courant number MAX  : ', maxCx
            print*, ''
            print*, 'X-dir Courant number MEAN : ', meanCx
            print*, 'Y-dir Courant number MEAN : ', meanCy
            print*, ''
            print*, 'NDT: ', ndt
            print*, 'n_split: ', n_split
            print*, 'DT: ', dt
            print*, ''
            print*, '--------------------------------------------'
            print*, ''
         endif

      end subroutine check_courant_numbers
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!     pmxn :: find max and min of field p
!
      subroutine pmxn(p, npx, npy, nregions, tile, gridstruct, pmin, pmax, i0, j0, n0)
         integer,      intent(IN) :: npx
         integer,      intent(IN) :: npy
         integer,      intent(IN) :: nregions, tile
         real  , intent(IN)  :: p(isd:ied,jsd:jed)
         type(fv_grid_type), intent(IN), target  :: gridstruct
         real  , intent(OUT) :: pmin
         real  , intent(OUT) :: pmax
         integer,      intent(OUT) :: i0
         integer,      intent(OUT) :: j0
         integer,      intent(OUT) :: n0

         real   :: temp
         integer :: i,j,n


      real, pointer, dimension(:,:,:)   :: agrid, grid
      real, pointer, dimension(:,:)     :: area, rarea, fC, f0
      real(kind=R_GRID), pointer, dimension(:,:,:)   :: ee1, ee2, en1, en2
      real(kind=R_GRID), pointer, dimension(:,:,:,:) :: ew, es
      real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

      logical, pointer :: cubed_sphere, latlon

      logical, pointer :: have_south_pole, have_north_pole

      integer, pointer :: ntiles_g
      real,    pointer :: acapN, acapS, globalarea

      grid => gridstruct%grid
      agrid=> gridstruct%agrid

      area  => gridstruct%area
      rarea => gridstruct%rarea

      fC    => gridstruct%fC
      f0    => gridstruct%f0

      ee1   => gridstruct%ee1
      ee2   => gridstruct%ee2
      ew    => gridstruct%ew
      es    => gridstruct%es
      en1   => gridstruct%en1
      en2   => gridstruct%en2

      dx      => gridstruct%dx
      dy      => gridstruct%dy
      dxa     => gridstruct%dxa
      dya     => gridstruct%dya
      rdxa    => gridstruct%rdxa
      rdya    => gridstruct%rdya
      dxc     => gridstruct%dxc
      dyc     => gridstruct%dyc
      
      cubed_sphere => gridstruct%cubed_sphere
      latlon       => gridstruct%latlon

      have_south_pole               => gridstruct%have_south_pole
      have_north_pole               => gridstruct%have_north_pole

      ntiles_g                      => gridstruct%ntiles_g
      acapN                         => gridstruct%acapN
      acapS                         => gridstruct%acapS
      globalarea                    => gridstruct%globalarea

         pmax = -1.e25
         pmin =  1.e25 
         i0 = -999
         j0 = -999
         n0 = tile

            do j=js,je
               do i=is,ie
                  temp = p(i,j)
                  if (temp > pmax) then
                     pmax = temp
                     i0 = i
                     j0 = j
                  elseif (temp < pmin) then
                     pmin = temp
                  endif
            enddo
         enddo

         temp = pmax
         call mp_reduce_max(temp)
         if (temp /= pmax) then
            i0 = -999
            j0 = -999
            n0 = -999
         endif
         pmax = temp
         call mp_reduce_max(i0)
         call mp_reduce_max(j0)
         call mp_reduce_max(n0)

         pmin = -pmin                  
         call mp_reduce_max(pmin)
         pmin = -pmin

      end subroutine pmxn
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!! These routines are no longer used
#ifdef NCDF_OUTPUT

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!     output_ncdf :: write out NETCDF fields
!
      subroutine output_ncdf(dt, nt, maxnt, nout, u,v,pt,delp,q,phis,ps, uc,vc, ua,va, &
                        omga, npx, npy, npz, ng, ncnst, ndims, nregions, ncid, &
                        npx_p1_id, npy_p1_id, npx_id, npy_id, npz_id, ntiles_id, ncnst_id, nt_id, &
                        phis_id, delp_id, ps_id, pt_id, pv_id, om_id, u_id, v_id, q_id, tracers_ids,  &
                        lats_id, lons_id, gridstruct, flagstruct)
      real,         intent(IN) :: dt
      integer,      intent(IN) :: nt, maxnt
      integer,      intent(INOUT) :: nout

      real ,      intent(INOUT) ::    u(isd:ied  ,jsd:jed+1,npz)
      real ,      intent(INOUT) ::    v(isd:ied+1,jsd:jed  ,npz)
      real ,      intent(INOUT) ::   pt(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) :: delp(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) ::    q(isd:ied  ,jsd:jed  ,npz, ncnst)

      real ,      intent(INOUT) :: phis(isd:ied  ,jsd:jed  )
      real ,      intent(INOUT) ::   ps(isd:ied  ,jsd:jed  )

      real ,      intent(INOUT) ::   uc(isd:ied+1,jsd:jed  ,npz)
      real ,      intent(INOUT) ::   vc(isd:ied  ,jsd:jed+1,npz)
      real ,      intent(INOUT) ::   ua(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) ::   va(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) :: omga(isd:ied  ,jsd:jed  ,npz)

      integer,      intent(IN) :: npx, npy, npz
      integer,      intent(IN) :: ng, ncnst
      integer,      intent(IN) :: ndims
      integer,      intent(IN) :: nregions
      integer,      intent(IN) :: ncid
      integer,      intent(IN) :: npx_p1_id, npy_p1_id, npx_id, npy_id, npz_id, ncnst_id
      integer,      intent(IN) :: ntiles_id, nt_id
      integer,      intent(IN) :: phis_id, delp_id, ps_id, pt_id, pv_id, u_id, v_id, q_id
      integer,      intent(IN) :: om_id          ! omega (dp/dt)
      integer,      intent(IN) :: tracers_ids(ncnst-1)
      integer,      intent(IN) :: lats_id, lons_id

      type(fv_grid_type), target :: gridstruct
      type(fv_flags_type), intent(IN) :: flagstruct

      real, allocatable :: tmp(:,:,:)
      real, allocatable :: tmpA(:,:,:)
#if defined(SW_DYNAMICS) 
      real, allocatable :: ut(:,:,:)
      real, allocatable :: vt(:,:,:)
#else       
      real, allocatable :: ut(:,:,:,:)
      real, allocatable :: vt(:,:,:,:)
      real, allocatable :: tmpA_3d(:,:,:,:)
#endif
      real, allocatable :: vort(:,:)

      real   :: p1(2)      ! Temporary Point
      real   :: p2(2)      ! Temporary Point
      real   :: p3(2)      ! Temporary Point
      real   :: p4(2)      ! Temporary Point
      real   :: pa(2)      ! Temporary Point
      real   :: utmp, vtmp, r, r0, dist, heading
      integer   ::  i,j,k,n,iq,nreg

      real :: Vtx, p, w_p
      real :: x1,y1,z1,x2,y2,z2,ang

      real, pointer, dimension(:,:,:)   :: agrid, grid
      real, pointer, dimension(:,:)     :: area, rarea
      real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

      grid =>  gridstruct%grid
      agrid => gridstruct%agrid

      area  => gridstruct%area
      rarea => gridstruct%rarea

      dx      => gridstruct%dx
      dy      => gridstruct%dy
      dxa     => gridstruct%dxa
      dya     => gridstruct%dya
      rdxa    => gridstruct%rdxa
      rdya    => gridstruct%rdya
      dxc     => gridstruct%dxc
      dyc     => gridstruct%dyc

      allocate( tmp(npx  ,npy  ,nregions) )
      allocate( tmpA(npx-1,npy-1,nregions) )
#if defined(SW_DYNAMICS) 
      allocate( ut(npx-1,npy-1,nregions) )
      allocate( vt(npx-1,npy-1,nregions) )
#else
      allocate( ut(npx-1,npy-1,npz,nregions) )
      allocate( vt(npx-1,npy-1,npz,nregions) )
      allocate( tmpA_3d(npx-1,npy-1,npz,nregions) )
#endif
      allocate( vort(isd:ied,jsd:jed) ) 

      nout = nout + 1

      if (nt==0) then
         tmp(is:ie+1,js:je+1,tile) = grid(is:ie+1,js:je+1,2)
         call wrtvar_ncdf(ncid, lats_id, nout, is,ie+1, js,je+1, npx+1, npy+1, 1, nregions, tmp(1:npx,1:npy,1:nregions), 3)
         tmp(is:ie+1,js:je+1,tile) = grid(is:ie+1,js:je+1,1)
         call wrtvar_ncdf(ncid, lons_id, nout, is,ie+1, js,je+1, npx+1, npy+1, 1, nregions, tmp(1:npx,1:npy,1:nregions), 3)
      endif

#if defined(SW_DYNAMICS)
      if (test_case > 1) then
         tmpA(is:ie,js:je,tile) = delp(is:ie,js:je,1)/Grav

         if ((nt==0) .and. (test_case==2)) then
         Ubar = (2.0*pi*radius)/(12.0*86400.0)
         gh0  = 2.94e4
         phis = 0.0
         do j=js,je+1
            do i=is,ie+1
               tmp(i,j,tile) = (gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                           ( -1.*cos(grid(i  ,j  ,1))*cos(grid(i  ,j  ,2))*sin(alpha) + &
                                 sin(grid(i  ,j  ,2))*cos(alpha) ) ** 2.0) / Grav
            enddo
         enddo
         endif

      else

       if (test_case==1) then
! Get Current Height Field "Truth"
         p1(1) = pi/2. + pi_shift
         p1(2) = 0.
         p2(1) = 3.*pi/2. + pi_shift
         p2(2) = 0.
         r0 = radius/3. !RADIUS /3.
         dist = 2.0*pi*radius* ((FLOAT(nt)/FLOAT(maxnt)))
         heading = 5.0*pi/2.0 - alpha
         call get_pt_on_great_circle( p1, p2, dist, heading, p3)
            do j=jsd,jed
               do i=isd,ied
                  p2(1) = agrid(i,j,1)
                  p2(2) = agrid(i,j,2)
                  r = great_circle_dist( p3, p2, radius )
                  if (r < r0) then
                     phi0(i,j,1) = phis(i,j) + gh0*0.5*(1.0+cos(PI*r/r0))
                  else
                     phi0(i,j,1) = phis(i,j)
                  endif
               enddo
            enddo
         elseif (test_case == 0) then
           phi0 = 0.0
           do j=jsd,jed
              do i=isd,ied
               x1 = agrid(i,j,1)
               y1 = agrid(i,j,2)
               z1 = radius
               p = p0_c0 * cos(y1)
               Vtx = ((3.0*SQRT(2.0))/2.0) * (( 1.0/cosh(p) )**2.0) * tanh(p)
               w_p = 0.0
               if (p /= 0.0) w_p = Vtx/p
               phi0(i,j,1) = 1.0 - tanh( (p/rgamma) * sin(x1 - w_p*(nt*dt/86400.0)) )
              enddo
           enddo
         endif

         tmpA(is:ie,js:je,tile) = phi0(is:ie,js:je,1)
         call wrtvar_ncdf(ncid, phis_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA, 3)
         tmpA(is:ie,js:je,tile) = delp(is:ie,js:je,1)
      endif
      call wrtvar_ncdf(ncid, ps_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA, 3)

      if (test_case == 9) then
! Calc Vorticity
         do j=jsd,jed
            do i=isd,ied
               vort(i,j) = f0(i,j) + (1./area(i,j)) * ( (v(i+1,j,1)*dy(i+1,j) - v(i,j,1)*dy(i,j)) - &
                                                        (u(i,j+1,1)*dx(i,j+1) - u(i,j,1)*dx(i,j)) )
               vort(i,j) = Grav*vort(i,j)/delp(i,j,1)
            enddo
         enddo
         tmpA(is:ie,js:je,tile) = vort(is:ie,js:je)
         call wrtvar_ncdf(ncid, pv_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA, 3)
      endif

      call cubed_to_latlon(u, v, ua, va, gridstruct, npx, npy, 1, 1, gridstruct%grid_type, gridstruct%nested, flagstruct%c2l_ord, bd)
      do j=js,je
         do i=is,ie
            ut(i,j,tile) = ua(i,j,1)
            vt(i,j,tile) = va(i,j,1)
         enddo
      enddo

      call wrtvar_ncdf(ncid, u_id, nout, is,ie, js,je, npx, npy, npz, nregions, ut(1:npx-1,1:npy-1,1:nregions), 3)
      call wrtvar_ncdf(ncid, v_id, nout, is,ie, js,je, npx, npy, npz, nregions, vt(1:npx-1,1:npy-1,1:nregions), 3)

      if ((test_case >= 2) .and. (nt==0) ) then
         tmpA(is:ie,js:je,tile) = phis(is:ie,js:je)/Grav
         call wrtvar_ncdf(ncid, phis_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA, 3)
      endif
#else

! Write Moisture Data
      tmpA_3d(is:ie,js:je,1:npz,tile) = q(is:ie,js:je,1:npz,1)
      call wrtvar_ncdf(ncid, q_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA_3d, 4)

! Write Tracer Data
      do iq=2,ncnst
         tmpA_3d(is:ie,js:je,1:npz,tile) = q(is:ie,js:je,1:npz,iq)
         call wrtvar_ncdf(ncid, tracers_ids(iq-1), nout, is,ie, js,je, npx, npy, npz, nregions, tmpA_3d, 4)
      enddo

! Write Surface height data
      tmpA(is:ie,js:je,tile) = phis(is:ie,js:je)/Grav
      call wrtvar_ncdf(ncid, phis_id, nout, is,ie, js,je, npx, npy, 1, nregions, tmpA, 3)

! Write Pressure Data
      tmpA(is:ie,js:je,tile) = ps(is:ie,js:je)
      call wrtvar_ncdf(ncid, ps_id, nout, is,ie, js,je, npx, npy, 1, nregions, tmpA, 3)
      do k=1,npz
         tmpA_3d(is:ie,js:je,k,tile) = delp(is:ie,js:je,k)/Grav
      enddo
      call wrtvar_ncdf(ncid, delp_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA_3d, 4)

! Write PT Data
      do k=1,npz
         tmpA_3d(is:ie,js:je,k,tile) = pt(is:ie,js:je,k)
      enddo
      call wrtvar_ncdf(ncid, pt_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA_3d, 4)

! Write U,V Data
      call cubed_to_latlon(u, v, ua, va, gridstruct, npx, npy, npz, gridstruct%grid_type, gridstruct%nested, flagstruct%c2l_ord)
      do k=1,npz
         do j=js,je
            do i=is,ie
               ut(i,j,k,tile) = ua(i,j,k)
               vt(i,j,k,tile) = va(i,j,k)
            enddo
         enddo
      enddo
      call wrtvar_ncdf(ncid, u_id, nout, is,ie, js,je, npx, npy, npz, nregions, ut(1:npx-1,1:npy-1,1:npz,1:nregions), 4)
      call wrtvar_ncdf(ncid, v_id, nout, is,ie, js,je, npx, npy, npz, nregions, vt(1:npx-1,1:npy-1,1:npz,1:nregions), 4)


! Calc Vorticity
      do k=1,npz
         do j=js,je
            do i=is,ie
               tmpA_3d(i,j,k,tile) = rarea(i,j) * ( (v(i+1,j,k)*dy(i+1,j) - v(i,j,k)*dy(i,j)) - &
                                                    (u(i,j+1,k)*dx(i,j+1) - u(i,j,k)*dx(i,j)) )
            enddo
         enddo
      enddo
      call wrtvar_ncdf(ncid, pv_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA_3d, 4)
!
! Output omega (dp/dt):
      do k=1,npz
         do j=js,je
            do i=is,ie
               tmpA_3d(i,j,k,tile) = omga(i,j,k)
            enddo
         enddo
      enddo
      call wrtvar_ncdf(ncid, om_id, nout, is,ie, js,je, npx, npy, npz, nregions, tmpA_3d, 4)

#endif

      deallocate( tmp )
      deallocate( tmpA )
#if defined(SW_DYNAMICS) 
      deallocate( ut )
      deallocate( vt )
#else
      deallocate( ut )
      deallocate( vt )
      deallocate( tmpA_3d )
#endif
      deallocate( vort )

      nullify(grid)
      nullify(agrid)

      nullify(area)

      nullify(dx)      
      nullify(dy)      
      nullify(dxa)     
      nullify(dya)     
      nullify(rdxa)    
      nullify(rdya)    
      nullify(dxc)     
      nullify(dyc)     

      end subroutine output_ncdf

!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
!     output :: write out fields
!
      subroutine output(dt, nt, maxnt, nout, u,v,pt,delp,q,phis,ps, uc,vc, ua,va, &
                        npx, npy, npz, ng, ncnst, ndims, nregions, phis_lun, phi_lun, &
                        pt_lun, pv_lun, uv_lun, gridstruct)

      real,         intent(IN) :: dt
      integer,      intent(IN) :: nt, maxnt
      integer,      intent(INOUT) :: nout

      real ,      intent(INOUT) ::    u(isd:ied  ,jsd:jed+1,npz)
      real ,      intent(INOUT) ::    v(isd:ied+1,jsd:jed  ,npz)
      real ,      intent(INOUT) ::   pt(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) :: delp(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) ::    q(isd:ied  ,jsd:jed  ,npz, ncnst)

      real ,      intent(INOUT) :: phis(isd:ied  ,jsd:jed  )
      real ,      intent(INOUT) ::   ps(isd:ied  ,jsd:jed  )

      real ,      intent(INOUT) ::   uc(isd:ied+1,jsd:jed  ,npz)
      real ,      intent(INOUT) ::   vc(isd:ied  ,jsd:jed+1,npz)
      real ,      intent(INOUT) ::   ua(isd:ied  ,jsd:jed  ,npz)
      real ,      intent(INOUT) ::   va(isd:ied  ,jsd:jed  ,npz)

      integer,      intent(IN) :: npx, npy, npz
      integer,      intent(IN) :: ng, ncnst
      integer,      intent(IN) :: ndims
      integer,      intent(IN) :: nregions
      integer,      intent(IN) :: phis_lun, phi_lun, pt_lun, pv_lun, uv_lun

      type(fv_grid_type), target :: gridstruct

      real   ::  tmp(1-ng:npx  +ng,1-ng:npy  +ng,1:nregions)
      real   :: tmpA(1-ng:npx-1+ng,1-ng:npy-1+ng,1:nregions)
      real   :: p1(2)      ! Temporary Point
      real   :: p2(2)      ! Temporary Point
      real   :: p3(2)      ! Temporary Point
      real   :: p4(2)      ! Temporary Point
      real   :: pa(2)      ! Temporary Point
      real   :: ut(1:npx,1:npy,1:nregions)
      real   :: vt(1:npx,1:npy,1:nregions)
      real   :: utmp, vtmp, r, r0, dist, heading
      integer   ::  i,j,k,n,nreg
      real   :: vort(isd:ied,jsd:jed)

      real :: Vtx, p, w_p
      real :: x1,y1,z1,x2,y2,z2,ang

      real, pointer, dimension(:,:,:)   :: agrid, grid
      real, pointer, dimension(:,:)     :: area, rarea
      real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

       grid => gridstruct%grid
      agrid => gridstruct%agrid

      area  => gridstruct%area

      dx      => gridstruct%dx
      dy      => gridstruct%dy
      dxa     => gridstruct%dxa
      dya     => gridstruct%dya
      rdxa    => gridstruct%rdxa
      rdya    => gridstruct%rdya
      dxc     => gridstruct%dxc
      dyc     => gridstruct%dyc

      cubed_sphere => gridstruct%cubed_sphere

      nout = nout + 1

#if defined(SW_DYNAMICS)
      if (test_case > 1) then
         call atob_s(delp(:,:,1)/Grav, tmp(isd:ied+1,jsd:jed+1,tile), npx,npy, dxa, dya, gridstruct%nested) !, altInterp=1)
         tmpA(is:ie,js:je,tile) = delp(is:ie,js:je,1)/Grav

         if ((nt==0) .and. (test_case==2)) then
         Ubar = (2.0*pi*radius)/(12.0*86400.0)
         gh0  = 2.94e4
         phis = 0.0
         do j=js,je+1
            do i=is,ie+1
               tmp(i,j,tile) = (gh0 - (radius*omega*Ubar + (Ubar*Ubar)/2.) * &
                           ( -1.*cos(grid(i  ,j  ,1))*cos(grid(i  ,j  ,2))*sin(alpha) + &
                                 sin(grid(i  ,j  ,2))*cos(alpha) ) ** 2.0) / Grav
            enddo
         enddo
         endif

      else

       if (test_case==1) then
! Get Current Height Field "Truth"
         p1(1) = pi/2. + pi_shift
         p1(2) = 0.
         p2(1) = 3.*pi/2. + pi_shift
         p2(2) = 0.
         r0 = radius/3. !RADIUS /3.
         dist = 2.0*pi*radius* ((FLOAT(nt)/FLOAT(maxnt)))
         heading = 5.0*pi/2.0 - alpha
         call get_pt_on_great_circle( p1, p2, dist, heading, p3)
            do j=jsd,jed
               do i=isd,ied
                  p2(1) = agrid(i,j,1)
                  p2(2) = agrid(i,j,2)
                  r = great_circle_dist( p3, p2, radius )
                  if (r < r0) then
                     phi0(i,j,1) = phis(i,j) + gh0*0.5*(1.0+cos(PI*r/r0))
                  else
                     phi0(i,j,1) = phis(i,j)
                  endif
               enddo
            enddo
         elseif (test_case == 0) then
           phi0 = 0.0
           do j=jsd,jed
              do i=isd,ied
               x1 = agrid(i,j,1) 
               y1 = agrid(i,j,2)
               z1 = radius
               p = p0_c0 * cos(y1)
               Vtx = ((3.0*SQRT(2.0))/2.0) * (( 1.0/cosh(p) )**2.0) * tanh(p)
               w_p = 0.0
               if (p /= 0.0) w_p = Vtx/p
               phi0(i,j,1) = 1.0 - tanh( (p/rgamma) * sin(x1 - w_p*(nt*dt/86400.0)) )
              enddo
           enddo
         endif

         call atob_s(phi0(:,:,1), tmp(isd:ied+1,jsd:jed+1,tile), npx,npy, dxa, dya, gridstruct%nested) !, altInterp=1)
         tmpA(is:ie,js:je,tile) = phi0(is:ie,js:je,1)
         call wrt2d(phis_lun, nout  , is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))
         call atob_s(delp(:,:,1), tmp(isd:ied+1,jsd:jed+1,tile), npx,npy, dxa, dya, gridstruct%nested) !, altInterp=1)
         tmpA(is:ie,js:je,tile) = delp(is:ie,js:je,1)
      endif
   !   call wrt2d(phi_lun, nout, is,ie+1, js,je+1, npx+1, npy+1, nregions, tmp(1:npx,1:npy,1:nregions))
      call wrt2d(phi_lun, nout, is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))

      if (test_case == 9) then
! Calc Vorticity
         do j=jsd,jed
            do i=isd,ied
               vort(i,j) = f0(i,j) + (1./area(i,j)) * ( (v(i+1,j,1)*dy(i+1,j) - v(i,j,1)*dy(i,j)) - &
                                                        (u(i,j+1,1)*dx(i,j+1) - u(i,j,1)*dx(i,j)) )
               vort(i,j) = Grav*vort(i,j)/delp(i,j,1)
            enddo
         enddo
         call atob_s(vort, tmp(isd:ied+1,jsd:jed+1,tile), npx,npy, dxa, dya, gridstruct%nested) !, altInterp=1)
         call wrt2d(pv_lun, nout, is,ie+1, js,je+1, npx+1, npy+1, nregions, tmp(1:npx,1:npy,1:nregions))
      endif

      call dtoa(u , v , ua, va, dx,dy,dxa,dya,dxc,dyc,npx, npy, ng)
! Rotate winds to standard Lat-Lon orientation
      if (cubed_sphere) then
         do j=js,je
            do i=is,ie
               call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
               call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
               call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p3)
               call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p4)
               utmp = ua(i,j,1)
               vtmp = va(i,j,1)
               if (cubed_sphere) call rotate_winds(utmp,vtmp, p1,p2,p3,p4, agrid(i,j,1:2), 2, 2)
               ut(i,j,tile) = utmp
               vt(i,j,tile) = vtmp
            enddo
         enddo
      endif

      call wrt2d(uv_lun, 2*(nout-1) + 1, is,ie, js,je, npx, npy, nregions,   ut(1:npx-1,1:npy-1,1:nregions))
      call wrt2d(uv_lun, 2*(nout-1) + 2, is,ie, js,je, npx, npy, nregions,   vt(1:npx-1,1:npy-1,1:nregions))

      if ((test_case >= 2) .and. (nt==0) ) then
         call atob_s(phis/Grav, tmp(isd:ied+1,jsd:jed+1,tile), npx,npy, dxa, dya, gridstruct%nested) !, altInterp=1)
       !  call wrt2d(phis_lun, nout  , is,ie+1, js,je+1, npx+1, npy+1, nregions, tmp(1:npx,1:npy,1:nregions))
         tmpA(is:ie,js:je,tile) = phis(is:ie,js:je)/Grav
         call wrt2d(phis_lun, nout  , is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))
      endif
#else

! Write Surface height data
      if (nt==0) then
         tmpA(is:ie,js:je,tile) = phis(is:ie,js:je)/Grav
         call wrt2d(phis_lun, nout  , is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))
      endif

! Write Pressure Data

      !if (tile==2) then
      !   do i=is,ie
      !      print*, i, ps(i,35) 
      !   enddo
      !endif
      tmpA(is:ie,js:je,tile) = ps(is:ie,js:je)
      call wrt2d(phi_lun, (nout-1)*(npz+1) + 1, is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))
      do k=1,npz
         tmpA(is:ie,js:je,tile) = delp(is:ie,js:je,k)/Grav
         call wrt2d(phi_lun, (nout-1)*(npz+1) + 1 + k, is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))
      enddo

! Write PT Data
      do k=1,npz
         tmpA(is:ie,js:je,tile) = pt(is:ie,js:je,k)
         call wrt2d(pt_lun, (nout-1)*npz + (k-1) + 1, is,ie, js,je, npx, npy, nregions, tmpA(1:npx-1,1:npy-1,1:nregions))
      enddo

! Write U,V Data
      do k=1,npz
         call dtoa(u(isd,jsd,k), v(isd,jsd,k), ua(isd,jsd,k), va(isd,jsd,k), dx,dy,dxa,dya,dxc,dyc,npx, npy, ng)
! Rotate winds to standard Lat-Lon orientation
         if (cubed_sphere) then
            do j=js,je
               do i=is,ie
                 call mid_pt_sphere(grid(i,j,1:2), grid(i,j+1,1:2), p1)
                 call mid_pt_sphere(grid(i,j,1:2), grid(i+1,j,1:2), p2)
                 call mid_pt_sphere(grid(i+1,j,1:2), grid(i+1,j+1,1:2), p3)
                 call mid_pt_sphere(grid(i,j+1,1:2), grid(i+1,j+1,1:2), p4)
                 utmp = ua(i,j,k)
                 vtmp = va(i,j,k)
                 if (cubed_sphere) call rotate_winds(utmp,vtmp, p1,p2,p3,p4, agrid(i,j,1:2), 2, 2)
                 ut(i,j,tile) = utmp
                 vt(i,j,tile) = vtmp
               enddo
            enddo
         endif
         call wrt2d(uv_lun, 2*((nout-1)*npz + (k-1)) + 1, is,ie, js,je, npx, npy, nregions,   ut(1:npx-1,1:npy-1,1:nregions))
         call wrt2d(uv_lun, 2*((nout-1)*npz + (k-1)) + 2, is,ie, js,je, npx, npy, nregions,   vt(1:npx-1,1:npy-1,1:nregions))
      enddo
#endif

      nullify(grid)
      nullify(agrid)

      nullify(area)

      nullify(dx)      
      nullify(dy)      
      nullify(dxa)     
      nullify(dya)     
      nullify(rdxa)    
      nullify(rdya)    
      nullify(dxc)     
      nullify(dyc)     

      nullify(cubed_sphere)

      end subroutine output
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!     wrt2d_ncdf :: write out a 2d field
!        
      subroutine wrtvar_ncdf(ncid, varid, nrec, i1,i2, j1,j2, npx, npy, npz, ntiles, p, ndims)
#include <netcdf.inc>
         integer,      intent(IN) :: ncid, varid
         integer,      intent(IN) :: nrec
         integer,      intent(IN) :: i1,i2,j1,j2
         integer,      intent(IN) :: npx
         integer,      intent(IN) :: npy
         integer,      intent(IN) :: npz
         integer,      intent(IN) :: ntiles
         real  , intent(IN)  :: p(npx-1,npy-1,npz,ntiles)
         integer,      intent(IN) :: ndims

         integer :: error
         real(kind=4), allocatable :: p_R4(:,:,:,:)
         integer :: i,j,k,n
         integer :: istart(ndims+1), icount(ndims+1)

         allocate( p_R4(npx-1,npy-1,npz,ntiles) )

         p_R4(:,:,:,:) = missing
         p_R4(i1:i2,j1:j2,1:npz,tile) = p(i1:i2,j1:j2,1:npz,tile)
         call mp_gather(p_R4, i1,i2, j1,j2, npx-1, npy-1, npz, ntiles)

         istart(:) = 1
         istart(ndims+1) = nrec
         icount(1) = npx-1
         icount(2) = npy-1
         icount(3) = npz
         if (ndims == 3) icount(3) = ntiles
         if (ndims == 4) icount(4) = ntiles
         icount(ndims+1) = 1

         if (is_master()) then  
            error = NF_PUT_VARA_REAL(ncid, varid, istart, icount, p_R4)
         endif ! masterproc

         deallocate( p_R4 )

      end subroutine wrtvar_ncdf
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!     wrt2d :: write out a 2d field
!
      subroutine wrt2d(iout, nrec, i1,i2, j1,j2, npx, npy, nregions, p)
         integer,      intent(IN) :: iout
         integer,      intent(IN) :: nrec
         integer,      intent(IN) :: i1,i2,j1,j2
         integer,      intent(IN) :: npx
         integer,      intent(IN) :: npy
         integer,      intent(IN) :: nregions
         real  , intent(IN)  :: p(npx-1,npy-1,nregions)

         real(kind=4) :: p_R4(npx-1,npy-1,nregions)
         integer :: i,j,n

         do n=tile,tile
            do j=j1,j2
               do i=i1,i2
                  p_R4(i,j,n) = p(i,j,n)
               enddo
            enddo
         enddo

         call mp_gather(p_R4, i1,i2, j1,j2, npx-1, npy-1, nregions) 

         if (is_master()) then
            write(iout,rec=nrec) p_R4(1:npx-1,1:npy-1,1:nregions)
         endif ! masterproc

      end subroutine wrt2d
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!     init_double_periodic
!
      subroutine init_double_periodic(u,v,w,pt,delp,q,phis, ps,pe,peln,pk,pkz,  uc,vc, ua,va, ak, bk,  &
                                      gridstruct, flagstruct, npx, npy, npz, ng, ncnst, nwat, ndims, nregions, dry_mass, &
                                      mountain, moist_phys, hydrostatic, hybrid_z, delz, ze0, ks, ptop, domain_in, tile_in, bd)

        
        type(fv_grid_bounds_type), intent(IN) :: bd
        real ,      intent(INOUT) ::    u(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
        real ,      intent(INOUT) ::    v(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
        real ,      intent(INOUT) ::    w(bd%isd:  ,bd%jsd:  ,1:)
        real ,      intent(INOUT) ::   pt(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
        real ,      intent(INOUT) :: delp(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
        real ,      intent(INOUT) ::    q(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz, ncnst)
        
        real ,      intent(INOUT) :: phis(bd%isd:bd%ied  ,bd%jsd:bd%jed  )

        real ,      intent(INOUT) ::   ps(bd%isd:bd%ied  ,bd%jsd:bd%jed  )
        real ,      intent(INOUT) ::   pe(bd%is-1:bd%ie+1,npz+1,bd%js-1:bd%je+1)
        real ,      intent(INOUT) ::   pk(bd%is:bd%ie    ,bd%js:bd%je    ,npz+1)
        real ,      intent(INOUT) :: peln(bd%is :bd%ie   ,npz+1    ,bd%js:bd%je)
        real ,      intent(INOUT) ::  pkz(bd%is:bd%ie    ,bd%js:bd%je    ,npz  )
        real ,      intent(INOUT) ::   uc(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
        real ,      intent(INOUT) ::   vc(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,npz)
        real ,      intent(INOUT) ::   ua(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
        real ,      intent(INOUT) ::   va(bd%isd:bd%ied  ,bd%jsd:bd%jed  ,npz)
        real ,      intent(inout) :: delz(bd%isd:,bd%jsd:,1:)
        real ,      intent(inout)   ::  ze0(bd%is:,bd%js:,1:)
        
        real ,      intent(inout)    ::   ak(npz+1)
        real ,      intent(inout)    ::   bk(npz+1)
        
        integer,      intent(IN) :: npx, npy, npz
        integer,      intent(IN) :: ng, ncnst, nwat
        integer,      intent(IN) :: ndims
        integer,      intent(IN) :: nregions
        
        real,         intent(IN) :: dry_mass
        logical,      intent(IN) :: mountain
        logical,      intent(IN) :: moist_phys
        logical,      intent(IN) :: hydrostatic, hybrid_z
        integer,      intent(INOUT) :: ks
        integer,      intent(INOUT), target :: tile_in
        real,         intent(INOUT) :: ptop

        type(domain2d), intent(IN), target :: domain_in

        type(fv_grid_type), target :: gridstruct
        type(fv_flags_type), target :: flagstruct

        real, dimension(bd%is:bd%ie):: pm, qs
        real, dimension(1:npz):: pk1, ts1, qs1
        real :: us0 = 30.
        real :: dist, r0, f0_const, prf, rgrav
        real :: ptmp, ze, zc, zm, utmp, vtmp
        real :: t00, p00, xmax, xc, xx, yy, pk0, pturb, ztop
        real :: ze1(npz+1)
         real:: dz1(npz)
        real:: zvir
        integer :: i, j, k, m, icenter, jcenter

        real, pointer, dimension(:,:,:)   :: agrid, grid
        real(kind=R_GRID), pointer, dimension(:,:)     :: area
        real, pointer, dimension(:,:)     :: rarea, fC, f0
        real, pointer, dimension(:,:,:)   :: ee1, ee2, en1, en2
        real, pointer, dimension(:,:,:,:) :: ew, es
        real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

        logical, pointer :: cubed_sphere, latlon

        type(domain2d), pointer :: domain
        integer, pointer :: tile

        logical, pointer :: have_south_pole, have_north_pole

        integer, pointer :: ntiles_g
        real,    pointer :: acapN, acapS, globalarea

        real(kind=R_GRID), pointer :: dx_const, dy_const
        
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

        agrid => gridstruct%agrid
        grid  => gridstruct%grid

        area => gridstruct%area_64

        dx      => gridstruct%dx
        dy      => gridstruct%dy
        dxa     => gridstruct%dxa
        dya     => gridstruct%dya
        rdxa    => gridstruct%rdxa
        rdya    => gridstruct%rdya
        dxc     => gridstruct%dxc
        dyc     => gridstruct%dyc

        fC    => gridstruct%fC
        f0    => gridstruct%f0

        !These are frequently used and so have pointers set up for them
        dx_const => flagstruct%dx_const
        dy_const => flagstruct%dy_const

        domain => domain_in
        tile => tile_in

        have_south_pole               => gridstruct%have_south_pole
        have_north_pole               => gridstruct%have_north_pole

        ntiles_g                      => gridstruct%ntiles_g
        acapN                         => gridstruct%acapN
        acapS                         => gridstruct%acapS
        globalarea                    => gridstruct%globalarea

        f0_const = 2.*omega*sin(flagstruct%deglat/180.*pi)
        f0(:,:) = f0_const
        fC(:,:) = f0_const

        q = 0.

        select case (test_case)
        case ( 1 )

           phis(:,:)=0.

           u (:,:,:)=10.
           v (:,:,:)=10.
           ua(:,:,:)=10.
           va(:,:,:)=10.
           uc(:,:,:)=10.
           vc(:,:,:)=10.
           pt(:,:,:)=1.
           delp(:,:,:)=0.
           
           do j=js,je
              if (j>0 .and. j<5) then
                 do i=is,ie
                    if (i>0 .and. i<5) then
                       delp(i,j,:)=1.
                    endif
                 enddo
              endif
           enddo
           call mpp_update_domains( delp, domain )

        case ( 2 )

           phis(:,:) = 0.

!          r0 = 5000.
           r0 = 5.*sqrt(dx_const**2 + dy_const**2)
           icenter = npx/2
           jcenter = npy/2
           do j=jsd,jed
              do i=isd,ied
                 dist=(i-icenter)*dx_const*(i-icenter)*dx_const   &
                       +(j-jcenter)*dy_const*(j-jcenter)*dy_const
                 dist=min(r0,sqrt(dist))
                 phis(i,j)=1500.*(1. - (dist/r0))
              enddo
           enddo

           u (:,:,:)=0.
           v (:,:,:)=0.
           ua(:,:,:)=0.
           va(:,:,:)=0.
           uc(:,:,:)=0.
           vc(:,:,:)=0.
           pt(:,:,:)=1.
           delp(:,:,:)=1500.

        case ( 14 )
!---------------------------
! Doubly periodic Aqua-plane
!---------------------------
           u(:,:,:) = 0.
           v(:,:,:) = 0.
           phis(:,:) = 0.

           call hydro_eq(npz, is, ie, js, je, ps, phis, dry_mass,      &
                         delp, ak, bk, pt, delz, area, ng, .false., hydrostatic, hybrid_z, domain)

	   ! *** Add Initial perturbation ***
	   if (bubble_do) then
	       r0 = 100.*sqrt(dx_const**2 + dy_const**2)
	       icenter = npx/2
	       jcenter = npy/2

	       do j=js,je
		  do i=is,ie
		     dist = (i-icenter)*dx_const*(i-icenter)*dx_const   &
			   +(j-jcenter)*dy_const*(j-jcenter)*dy_const
		     dist = min(r0, sqrt(dist))
		     do k=1,npz
			prf = ak(k) + ps(i,j)*bk(k)
			if ( prf > 100.E2 ) then
			     pt(i,j,k) = pt(i,j,k) + 0.01*(1. - (dist/r0)) * prf/ps(i,j) 
			endif
		     enddo
		  enddo
	       enddo
	   endif
          if ( hydrostatic ) then
          call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                     pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                     moist_phys, .true., nwat , domain)
          else
               w(:,:,:) = 0.
          call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                     pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                     moist_phys, hydrostatic, nwat, domain, .true. )
          endif

         q = 0.
         do k=1,npz
            do j=js,je
               do i=is,ie
                  pm(i) = delp(i,j,k)/(peln(i,k+1,j)-peln(i,k,j))
               enddo
               call qsmith(ie-is+1, 1, 1, pt(is:ie,j,k), pm, q(is:ie,j,k,1), qs)
               do i=is,ie
                  q(i,j,k,1) = max(2.E-6, 0.8*pm(i)/ps(i,j)*qs(i) )
               enddo
            enddo
         enddo

        case ( 15 )
!---------------------------
! Doubly periodic bubble
!---------------------------
           t00 = 250.

           u(:,:,:) = 0.
           v(:,:,:) = 0.
          pt(:,:,:) = t00
          q(:,:,:,:) = 1.E-6

          if ( .not. hydrostatic ) w(:,:,:) = 0.

           do j=jsd,jed
              do i=isd,ied
                 phis(i,j) = 0.
                   ps(i,j) = 1000.E2
              enddo
           enddo

           do k=1,npz
              do j=jsd,jed
                 do i=isd,ied
                    delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
                 enddo
              enddo
           enddo


           do k=1,npz
              do j=jsd,jed
                 do i=isd,ied
                         ptmp = delp(i,j,k)/(peln(i,k+1,j)-peln(i,k,j))
!                   pt(i,j,k) = t00
                 enddo
              enddo
           enddo

          call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                     pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                     moist_phys, .false., nwat, domain)

! *** Add Initial perturbation ***
           r0 = 5.*max(dx_const, dy_const)
           zc = 0.5e3         ! center of bubble  from surface
           icenter = npx/2
           jcenter = npy/2

           do j=js,je
              do i=is,ie
                 ze = 0.
                 do k=npz,1,-1
                    zm = ze - 0.5*delz(i,j,k)   ! layer center
                    ze = ze - delz(i,j,k)
                    dist = ((i-icenter)*dx_const)**2 + ((j-jcenter)*dy_const)**2 +  &
                           (zm-zc)**2
                    dist = sqrt(dist)
                    if ( dist <= r0 ) then
                         pt(i,j,k) = pt(i,j,k) + 5.*(1.-dist/r0)
                    endif
                 enddo
              enddo
           enddo

        case ( 16 )
!------------------------------------
! Non-hydrostatic 3D density current:
!------------------------------------
           phis = 0.
           u = 0.
           v = 0.
           w = 0.
          t00 = 300.
          p00 = 1.E5
          pk0 = p00**kappa
! Set up vertical coordinare with constant del-z spacing:
! Control: npz=64;  dx = 100 m; dt = 1; n_split=10
          ztop = 6.4E3
         ze1(    1) = ztop
         ze1(npz+1) = 0.
         do k=npz,2,-1
            ze1(k) = ze1(k+1) + ztop/real(npz)
         enddo

          do j=js,je
             do i=is,ie
                ps(i,j) = p00
                pe(i,npz+1,j) = p00
                pk(i,j,npz+1) = pk0
             enddo
          enddo

          do k=npz,1,-1
             do j=js,je
                do i=is,ie
                   delz(i,j,k) = ze1(k+1) - ze1(k)
                     pk(i,j,k) = pk(i,j,k+1) + grav*delz(i,j,k)/(cp_air*t00)*pk0
                     pe(i,k,j) = pk(i,j,k)**(1./kappa) 
                enddo
             enddo
          enddo

          ptop = pe(is,1,js)
          if ( is_master() ) write(*,*) 'Density curent testcase: model top (mb)=', ptop/100.

          do k=1,npz+1
             do j=js,je
                do i=is,ie
                   peln(i,k,j) = log(pe(i,k,j)) 
                    ze0(i,j,k) = ze1(k)
                enddo
             enddo
          enddo

          do k=1,npz
             do j=js,je
                do i=is,ie
                   pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
                  delp(i,j,k) =  pe(i,k+1,j)-pe(i,k,j)  
                    pt(i,j,k) = t00/pk0   ! potential temp
                enddo
             enddo
          enddo

          pturb = 15.
           xmax = 51.2E3 
             xc = xmax / 2.

         do k=1,npz
            zm = (0.5*(ze1(k)+ze1(k+1))-3.E3) / 2.E3
            do j=js,je
               do i=is,ie
! Impose perturbation in potential temperature: pturb
                  xx = (dx_const * (0.5+real(i-1)) - xc) / 4.E3 
                  yy = (dy_const * (0.5+real(j-1)) - xc) / 4.E3
                  dist = sqrt( xx**2 + yy**2 + zm**2 )
                  if ( dist<=1. ) then
                       pt(i,j,k) = pt(i,j,k) - pturb/pkz(i,j,k)*(cos(pi*dist)+1.)/2. 
                  endif
! Transform back to temperature:
                  pt(i,j,k) = pt(i,j,k) * pkz(i,j,k)
               enddo
            enddo
          enddo

      case ( 17 )
!---------------------------
! Doubly periodic SuperCell, straight wind (v==0)
!--------------------------
        zvir = rvgas/rdgas - 1.
        p00 = 1000.E2
          ps(:,:) = p00
        phis(:,:) = 0.
        do j=js,je
           do i=is,ie
                pk(i,j,1) = ptop**kappa
                pe(i,1,j) = ptop
              peln(i,1,j) = log(ptop)
           enddo
        enddo

        do k=1,npz
           do j=js,je
              do i=is,ie
                 delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
                 pe(i,k+1,j) = ak(k+1) + ps(i,j)*bk(k+1)
                 peln(i,k+1,j) = log(pe(i,k+1,j))
                   pk(i,j,k+1) = exp( kappa*peln(i,k+1,j) )
              enddo
           enddo
        enddo

        i = is
        j = js
        do k=1,npz
           pk1(k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
        enddo


        v(:,:,:) = 0.
        w(:,:,:) = 0.
        q(:,:,:,:) = 0.

        do k=1,npz
           do j=js,je
              do i=is,ie
                 pt(i,j,k)   = ts1(k)
                  q(i,j,k,1) = qs1(k)
                 delz(i,j,k) = rdgas/grav*ts1(k)*(1.+zvir*qs1(k))*(peln(i,k,j)-peln(i,k+1,j))
                enddo
             enddo
          enddo

        ze1(npz+1) = 0.
        do k=npz,1,-1
           ze1(k) = ze1(k+1) - delz(is,js,k)
        enddo

        do k=1,npz
             zm = 0.5*(ze1(k)+ze1(k+1))
           utmp = us0*tanh(zm/3.E3)
           do j=js,je+1
              do i=is,ie
                 u(i,j,k) = utmp
             enddo
           enddo
        enddo

        call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                   pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                   .true., hydrostatic, nwat, domain)

! *** Add Initial perturbation ***
        pturb = 2.
        r0 = 10.e3
        zc = 1.4e3         ! center of bubble  from surface
        icenter = (npx-1)/3 + 1
        jcenter = (npy-1)/2 + 1
        do k=1, npz
           zm = 0.5*(ze1(k)+ze1(k+1))
           ptmp = ( (zm-zc)/zc ) **2
           if ( ptmp < 1. ) then
              do j=js,je
                 do i=is,ie
                   dist = ptmp+((i-icenter)*dx_const/r0)**2+((j-jcenter)*dy_const/r0)**2
                   if ( dist < 1. ) then
                        pt(i,j,k) = pt(i,j,k) + pturb*(1.-sqrt(dist))
                   endif
                 enddo
              enddo
           endif
        enddo

      case ( 18 )
!---------------------------
! Doubly periodic SuperCell, quarter circle hodograph
! M. Toy, Apr 2013, MWR
        pturb = 2.5
        zvir = rvgas/rdgas - 1.
        p00 = 1000.E2
          ps(:,:) = p00
        phis(:,:) = 0.
        do j=js,je
           do i=is,ie
                pk(i,j,1) = ptop**kappa
                pe(i,1,j) = ptop
              peln(i,1,j) = log(ptop)
           enddo
        enddo

        do k=1,npz
           do j=js,je
              do i=is,ie
                 delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
                 pe(i,k+1,j) = ak(k+1) + ps(i,j)*bk(k+1)
                 peln(i,k+1,j) = log(pe(i,k+1,j))
                   pk(i,j,k+1) = exp( kappa*peln(i,k+1,j) )
              enddo
           enddo
        enddo

        i = is
        j = js
        do k=1,npz
           pk1(k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
        enddo


        w(:,:,:) = 0.
        q(:,:,:,:) = 0.

        do k=1,npz
           do j=js,je
              do i=is,ie
                 pt(i,j,k)   = ts1(k)
                  q(i,j,k,1) = qs1(k)
                 delz(i,j,k) = rdgas/grav*ts1(k)*(1.+zvir*qs1(k))*(peln(i,k,j)-peln(i,k+1,j))
                enddo
             enddo
          enddo

        ze1(npz+1) = 0.
        do k=npz,1,-1
           ze1(k) = ze1(k+1) - delz(is,js,k)
        enddo

! Quarter-circle hodograph (Harris approximation)
        us0 = 30.
        do k=1,npz
           zm = 0.5*(ze1(k)+ze1(k+1))
           if ( zm .le. 2.e3 ) then
               utmp = 8.*(1.-cos(pi*zm/4.e3)) 
               vtmp = 8.*sin(pi*zm/4.e3)
           elseif (zm .le. 6.e3 ) then
               utmp = 8. + (us0-8.)*(zm-2.e3)/4.e3
               vtmp = 8.
           else
               utmp = us0
               vtmp = 8.
           endif
! u-wind
           do j=js,je+1
              do i=is,ie
                 u(i,j,k) = utmp - 8.
             enddo
           enddo
! v-wind
           do j=js,je
              do i=is,ie+1
                 v(i,j,k) = vtmp - 4.
             enddo
           enddo
        enddo


        call p_var(npz, is, ie, js, je, ptop, ptop_min, delp, delz, pt, ps,   &
                   pe, peln, pk, pkz, kappa, q, ng, ncnst, area, dry_mass, .false., .false., &
                   .true., hydrostatic, nwat, domain)

! *** Add Initial perturbation ***
	if (bubble_do) then
	    r0 = 10.e3
	    zc = 1.4e3         ! center of bubble  from surface
	    icenter = (npx-1)/2 + 1
	    jcenter = (npy-1)/2 + 1
	    do k=1, npz
	       zm = 0.5*(ze1(k)+ze1(k+1))
	       ptmp = ( (zm-zc)/zc ) **2
	       if ( ptmp < 1. ) then
		  do j=js,je
		     do i=is,ie
		       dist = ptmp+((i-icenter)*dx_const/r0)**2+((j-jcenter)*dy_const/r0)**2
		       if ( dist < 1. ) then
			    pt(i,j,k) = pt(i,j,k) + pturb*(1.-sqrt(dist))
		       endif
		     enddo
		  enddo
	       endif
	    enddo
	 endif

        case ( 101 )

! IC for LES
         t00 = 250.      ! constant temp
         p00 = 1.E5
         pk0 = p00**kappa

         phis = 0.
         u = 0.
         v = 0.
         w = 0.
         pt(:,:,:) = t00
         q(:,:,:,1) = 0.

         if (.not.hybrid_z) call mpp_error(FATAL, 'hybrid_z must be .TRUE.')

          rgrav = 1./ grav

          if ( npz/=101)  then
              call mpp_error(FATAL, 'npz must be == 101 ')
          else
              call compute_dz_L101( npz, ztop, dz1 )
          endif

          call set_hybrid_z(is, ie, js, je, ng, npz, ztop, dz1, rgrav,  &
                            phis, ze0, delz)

          do j=js,je
             do i=is,ie
                ps(i,j) = p00
                pe(i,npz+1,j) = p00
                pk(i,j,npz+1) = pk0
                peln(i,npz+1,j) = log(p00)
             enddo
          enddo

          do k=npz,1,-1
             do j=js,je
                do i=is,ie
                   peln(i,k,j) = peln(i,k+1,j) + grav*delz(i,j,k)/(rdgas*t00)
                     pe(i,k,j) = exp(peln(i,k,j))
                     pk(i,j,k) = pe(i,k,j)**kappa
                enddo
             enddo
          enddo


! Set up fake "sigma" coordinate 
          call make_eta_level(npz, pe, area, ks, ak, bk, ptop, domain, bd)

          if ( is_master() ) write(*,*) 'LES testcase: computed model top (mb)=', ptop/100.

          do k=1,npz
             do j=js,je
                do i=is,ie
                   pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
                  delp(i,j,k) =  pe(i,k+1,j)-pe(i,k,j)  
                enddo
             enddo
          enddo

         do k=1,npz
            do j=js,je
               do i=is,ie
                  pm(i) = delp(i,j,k)/(peln(i,k+1,j)-peln(i,k,j))
               enddo
               call qsmith(ie-is+1, 1, 1, pt(is:ie,j,k), pm, q(is:ie,j,k,1), qs)
               do i=is,ie
                  if ( pm(i) > 100.E2 ) then
                       q(i,j,k,1) = 0.9*qs(i)
                  else
                       q(i,j,k,1) = 2.E-6
                  endif
               enddo
            enddo
         enddo

! *** Add perturbation ***
           r0 = 1.0e3         ! radius (m)
           zc = 1.0e3         ! center of bubble 
           icenter = npx/2
           jcenter = npy/2

           do k=1,npz
              do j=js,je
                 do i=is,ie
                    zm = 0.5*(ze0(i,j,k)+ze0(i,j,k+1))
                    dist = ((i-icenter)*dx_const)**2 + ((j-jcenter)*dy_const)**2 + (zm-zc)**2
                    dist = sqrt(dist)
                    if ( dist <= r0 ) then
                         pt(i,j,k) = pt(i,j,k) + 2.0*(1.-dist/r0)
                    endif
                 enddo
              enddo
           enddo

        end select

        nullify(grid)
        nullify(agrid)

         nullify(area)

    nullify(fC)
    nullify(f0)

    nullify(ee1)       
    nullify(ee2)   
    nullify(ew)    
    nullify(es)    
    nullify(en1)   
    nullify(en2)   

      nullify(dx)      
      nullify(dy)      
      nullify(dxa)     
      nullify(dya)     
      nullify(rdxa)    
      nullify(rdya)    
      nullify(dxc)     
      nullify(dyc)     

      nullify(dx_const)
      nullify(dy_const)

      nullify(domain)
      nullify(tile)

      nullify(have_south_pole) 
      nullify(have_north_pole) 

      nullify(ntiles_g)        
      nullify(acapN)           
      nullify(acapS)           
      nullify(globalarea)      

      end subroutine init_double_periodic

!>@brief The subroutine 'SuperK_Sounding' gets the sounding at "equator"; the
!! initial storm center.
!>@details This is the z-coordinate version 
!! (Morris Weisman & J. Klemp 2002 sounding)
 subroutine SuperK_Sounding(km, pe, p00, ze, pt, qz)
 integer, intent(in):: km
 real, intent(in):: p00
 real, intent(inout), dimension(km+1):: pe
 real, intent(in), dimension(km+1):: ze
! pt: potential temperature / pk0
! qz: specific humidity (mixing ratio)
 real, intent(out), dimension(km):: pt, qz
! Local:
 integer, parameter:: nx = 5
 real, parameter:: qst = 1.0e-6
 real, parameter:: qv0 = 1.4e-2
 real, parameter:: ztr = 12.E3
 real, parameter:: ttr = 213.
 real, parameter:: ptr = 343.    !< Tropopause potential temp.
 real, parameter:: pt0 = 300.    !< surface potential temperature
 real, dimension(km):: zs, rh, temp, dp, dp0
 real, dimension(km+1):: peln, pk
 real:: qs, zvir, fac_z, pk0, temp1, pm
 integer:: k, n, kk

 zvir = rvgas/rdgas - 1.
 pk0 = p00**kappa
 if ( (is_master()) ) then
     write(*,*) 'Computing sounding for HIWPP super-cell test using p00=', p00
 endif

 qz(:) = qst
 rh(:) = 0.25

 do k=1, km
    zs(k) = 0.5*(ze(k)+ze(k+1))
! Potential temperature
    if ( zs(k) .gt. ztr ) then
! Stratosphere:
         pt(k) = ptr*exp(grav*(zs(k)-ztr)/(cp_air*ttr))
    else
! Troposphere:
         fac_z = (zs(k)/ztr)**1.25
         pt(k) = pt0 + (ptr-pt0)* fac_z
         rh(k) =  1. -     0.75 * fac_z
! First guess on q:
         qz(k) = qv0 - (qv0-qst)*fac_z
    endif
    if ( is_master() ) write(*,*) zs(k), pt(k), qz(k)
! Convert to FV's definition of potential temperature
    pt(k) = pt(k) / pk0
 enddo

#ifdef USE_MOIST_P00
!--------------------------------------
! Iterate nx times with virtual effect:
!--------------------------------------
! pt & height remain unchanged
      pk(km+1) = pk0
      pe(km+1) = p00      ! Dry
    peln(km+1) = log(p00)

 do n=1, nx
! Derive pressure fields from hydrostatic balance:
    do k=km,1,-1
       pk(k) = pk(k+1) - grav*(ze(k)-ze(k+1))/(cp_air*pt(k)*(1.+zvir*qz(k)))
       peln(k) = log(pk(k)) / kappa 
         pe(k) = exp(peln(k)) 
    enddo
    do k=1, km
       pm = (pe(k+1)-pe(k))/(peln(k+1)-peln(k))
       temp(k) = pt(k)*pm**kappa
! NCAR form:
       qs = 380./pm*exp(17.27*(temp(k)-273.)/(temp(k)-36.))
       qz(k) = min( qv0, rh(k)*qs )
       if ( n==nx .and. is_master() ) write(*,*) 0.01*pm, temp(k), qz(k), qs
    enddo
 enddo
#else
! pt & height remain unchanged
      pk(km+1) = pk0
      pe(km+1) = p00      ! Dry
    peln(km+1) = log(p00)

! Derive "dry"  pressure fields from hydrostatic balance:
    do k=km,1,-1
       pk(k) = pk(k+1) - grav*(ze(k)-ze(k+1))/(cp_air*pt(k))
       peln(k) = log(pk(k)) / kappa 
         pe(k) = exp(peln(k)) 
    enddo
    do k=1, km
       dp0(k) = pe(k+1) - pe(k)
       pm = dp0(k)/(peln(k+1)-peln(k))
       temp(k) = pt(k)*pm**kappa
! NCAR form:
       qs = 380./pm*exp(17.27*(temp(k)-273.)/(temp(k)-36.))
       qz(k) = min( qv0, rh(k)*qs )
    enddo

 do n=1, nx

    do k=1, km
       dp(k) = dp0(k)*(1. + qz(k))    ! moist air
       pe(k+1) = pe(k) + dp(k) 
    enddo
! dry pressure, pt & height remain unchanged
    pk(km+1) = pe(km+1)**kappa
    peln(km+1) = log(pe(km+1))

! Derive pressure fields from hydrostatic balance:
    do k=km,1,-1
       pk(k) = pk(k+1) - grav*(ze(k)-ze(k+1))/(cp_air*pt(k)*(1.+zvir*qz(k)))
       peln(k) = log(pk(k)) / kappa 
         pe(k) = exp(peln(k)) 
    enddo
    do k=1, km
       pm = (pe(k+1)-pe(k))/(peln(k+1)-peln(k))
       temp(k) = pt(k)*pm**kappa
! NCAR form:
       qs = 380./pm*exp(17.27*(temp(k)-273.)/(temp(k)-36.))
       qz(k) = min( qv0, rh(k)*qs )
       if ( n==nx .and. is_master() ) write(*,*) 0.01*pm, temp(k), qz(k), qs
    enddo
 enddo
#endif
 
 if ( is_master() ) then
      write(*,*) 'Super_K: computed ptop (mb)=', 0.01*pe(1), ' PS=', 0.01*pe(km+1)
      call prt_m1('1D Sounding T0', temp, 1, km, 1, 1, 0, 1, 1.)
 endif

 end subroutine SuperK_Sounding

 subroutine balanced_K(km, is, ie, js, je, ng, ps0, ze1, ts1, qs1, uz1, dudz, pe, pk, pt,  &
                       delz, zvir, ptop, ak, bk, agrid)
 integer, intent(in):: is, ie, js, je, ng, km
 real, intent(in), dimension(km  ):: ts1, qs1, uz1, dudz
 real, intent(in), dimension(km+1):: ze1
 real, intent(in):: zvir, ps0
 real, intent(inout):: ptop
 real(kind=R_GRID), intent(in):: agrid(is-ng:ie+ng,js-ng:je+ng,2)
 real, intent(inout), dimension(km+1):: ak, bk
 real, intent(inout), dimension(is-ng:ie+ng,js-ng:je+ng,km):: pt, delz
 real, intent(out), dimension(is:ie,js:je,km+1):: pk
! pt is FV's cp*thelta_v
 real, intent(inout), dimension(is-1:ie+1,km+1,js-1:je+1):: pe
! Local
 integer, parameter:: nt=5
 integer, parameter:: nlat=1001
 real, dimension(nlat,km):: pt2, pky, dzc
 real, dimension(nlat,km+1):: pk2, pe2, peln2, pte
 real, dimension(km+1):: pe1
 real:: lat(nlat), latc(nlat-1)
 real:: fac_y, dlat, dz0, pk0, tmp1, tmp2, tmp3, pint
 integer::i,j,k,n, jj, k1
 real:: p00=1.e5

 pk0 = p00**kappa
 dz0 = ze1(km) - ze1(km+1)
!!! dzc(:,:) =dz0

 dlat = 0.5*pi/real(nlat-1)
 do j=1,nlat
    lat(j) = dlat*real(j-1)
    do k=1,km
       dzc(j,k) = ze1(k) - ze1(k+1)
    enddo
 enddo
 do j=1,nlat-1
    latc(j) = 0.5*(lat(j)+lat(j+1))
 enddo

! Initialize pt2
    do k=1,km
       do j=1,nlat
          pt2(j,k) = ts1(k)
       enddo
    enddo
    if ( is_master() ) then
        tmp1 = pk0/cp_air
        call prt_m1('Super_K PT0', pt2, 1, nlat, 1, km, 0, 1, tmp1)
    endif

! pt2 defined from Eq to NP
! Check NP
 do n=1, nt
! Compute edge values
    call ppme(pt2, pte, dzc, nlat, km)
    do k=1,km
       do j=2,nlat
          tmp1 = 0.5*(pte(j-1,k  ) + pte(j,k  ))
          tmp3 = 0.5*(pte(j-1,k+1) + pte(j,k+1))
          pt2(j,k) = pt2(j-1,k) + dlat/(2.*grav)*sin(2.*latc(j-1))*uz1(k)*  &
                   ( uz1(k)*(tmp1-tmp3)/dzc(j,k) - (pt2(j-1,k)+pt2(j,k))*dudz(k) )
       enddo
    enddo
    if ( is_master() ) then
        call prt_m1('Super_K PT', pt2, 1, nlat, 1, km, 0, 1, pk0/cp_air)
    endif
 enddo
!
! Compute surface pressure using gradient-wind balance:
!!!   pk2(1,km+1) = pk0
    pk2(1,km+1) = ps0**kappa      ! fixed at equator
 do j=2,nlat
    pk2(j,km+1) =  pk2(j-1,km+1) - dlat*uz1(km)*uz1(km)*sin(2.*latc(j-1))   &
                / (pt2(j-1,km) + pt2(j,km))
 enddo
! Compute pressure using hydrostatic balance:
 do j=1,nlat
    do k=km,1,-1
       pk2(j,k) = pk2(j,k+1) - grav*dzc(j,k)/pt2(j,k)
    enddo
 enddo

 do k=1,km+1
    do j=1,nlat
       peln2(j,k) = log(pk2(j,k)) / kappa
         pe2(j,k) = exp(peln2(j,k))
    enddo
 enddo
! Convert pt2 to temperature
 do k=1,km
    do j=1,nlat
       pky(j,k) = (pk2(j,k+1)-pk2(j,k))/(kappa*(peln2(j,k+1)-peln2(j,k)))
       pt2(j,k) = pt2(j,k)*pky(j,k)/(cp_air*(1.+zvir*qs1(k)))
    enddo
 enddo

 do k=1,km+1
    pe1(k) = pe2(1,k)
 enddo

 if ( is_master() ) then
    write(*,*) 'SuperK ptop at EQ=', 0.01*pe1(1), 'new ptop=', 0.01*ptop
    call prt_m1('Super_K pe',   pe2, 1, nlat, 1, km+1, 0, 1, 0.01)
    call prt_m1('Super_K Temp', pt2, 1, nlat, 1, km,   0, 1, 1.)
 endif

! Interpolate (pt2, pk2) from lat-dir to cubed-sphere
 do j=js, je
    do i=is, ie
       do jj=1,nlat-1
          if (abs(agrid(i,j,2))>=lat(jj) .and. abs(agrid(i,j,2))<=lat(jj+1) ) then
! found it !
              fac_y = (abs(agrid(i,j,2))-lat(jj)) / dlat
              do k=1,km
                 pt(i, j,k) = pt2(jj, k) + fac_y*(pt2(jj+1, k)-pt2(jj,k))
              enddo
              do k=1,km+1
                 pe(i,k,j) = pe2(jj,k) + fac_y*(pe2(jj+1,k)-pe2(jj,k))
              enddo
!             k = km+1
!             pk(i,j,k) = pk2(jj,k) + fac_y*(pk2(jj+1,k)-pk2(jj,k))
              goto 123
          endif
       enddo
123 continue
    enddo
 enddo

! Adjust pk
! ak & bk
! Adjusting model top to be a constant pressure surface, assuming isothermal atmosphere
! pe = ak + bk*ps
! One pressure layer
        pe1(1) = ptop
         ak(1) = ptop
          pint = pe1(2)
         bk(1) = 0.
         ak(2) = pint
         bk(2) = 0.
         do k=3,km+1
            bk(k) = (pe1(k) - pint) / (pe1(km+1)-pint)  ! bk == sigma
            ak(k) =  pe1(k) - bk(k) * pe1(km+1)
            if ( is_master() ) write(*,*) k, ak(k), bk(k)
         enddo
         ak(km+1) = 0.
         bk(km+1) = 1.
 do j=js, je
    do i=is, ie
       pe(i,1,j) = ptop
    enddo
 enddo


 end subroutine balanced_K

 subroutine SuperK_u(km, zz, um, dudz)
 integer, intent(in):: km
 real, intent(in):: zz(km)
 real, intent(out):: um(km), dudz(km)
! Local
 real, parameter:: zs = 5.e3
 real, parameter:: us = 30.
 real:: uc = 15.
 integer k

 do k=1, km
#ifndef TEST_TANHP
! MPAS specification:
    if ( zz(k) .gt. zs+1.e3 ) then
       um(k) = us
       dudz(k) = 0.
    elseif ( abs(zz(k)-zs) .le. 1.e3 ) then
       um(k) = us*(-4./5. + 3.*zz(k)/zs - 5./4.*(zz(k)/zs)**2)
       dudz(k) = us/zs*(3. - 5./2.*zz(k)/zs)
    else
       um(k) = us*zz(k)/zs
       dudz(k) = us/zs
    endif
! constant wind so as to make the storm relatively stationary
    um(k) = um(k) - uc
#else
    uc = 12.   ! this gives near stationary (in longitude) storms
    um(k) = us*tanh( zz(k)/zs ) - uc
    dudz(k) = (us/zs)/cosh(zz(k)/zs)**2
#endif
 enddo

 end subroutine superK_u 


 subroutine DCMIP16_BC(delp,pt,u,v,q,w,delz,&
      is,ie,js,je,isd,ied,jsd,jed,npz,nq,ak,bk,ptop, &
      pk,peln,pe,pkz,gz,phis,ps,grid,agrid, &
      hydrostatic, nwat, adiabatic, do_pert, domain)

   integer, intent(IN) :: is,ie,js,je,isd,ied,jsd,jed,npz,nq, nwat
   real, intent(IN) :: ptop
   real, intent(IN), dimension(npz+1) :: ak, bk
   real, intent(INOUT), dimension(isd:ied,jsd:jed,npz,nq) :: q
   real, intent(OUT), dimension(isd:ied,jsd:jed,npz) :: delp, pt, w, delz
   real, intent(OUT), dimension(isd:ied,jsd:jed+1,npz) :: u
   real, intent(OUT), dimension(isd:ied+1,jsd:jed,npz) :: v
   real, intent(OUT), dimension(is:ie,js:je,npz+1) :: pk
   real, intent(OUT), dimension(is:ie,npz+1,js:je) :: peln
   real, intent(OUT), dimension(is-1:ie+1,npz+1,js-1:je+1) :: pe
   real, intent(OUT), dimension(is:ie,js:je,npz) :: pkz
   real, intent(OUT), dimension(isd:ied,jsd:jed) :: phis,ps
   real(kind=R_GRID), intent(IN), dimension(isd:ied,jsd:jed,2) :: agrid
   real(kind=R_GRID), intent(IN), dimension(isd:ied+1,jsd:jed+1,2) :: grid
   real, intent(OUT), dimension(isd:ied,jsd:jed,npz+1) :: gz
   logical, intent(IN) :: hydrostatic,adiabatic,do_pert
   type(domain2d), intent(INOUT) :: domain

   real, parameter :: p0 = 1.e5
   real, parameter :: u0 = 35.
   real, parameter :: b = 2.
   real, parameter :: KK = 3.
   real, parameter :: Te = 310.
   real, parameter :: Tp = 240.
   real, parameter :: T0 = 0.5*(Te + Tp) !!WRONG in document
   real, parameter :: up = 1.
   real, parameter :: zp = 1.5e4
   real(kind=R_GRID), parameter :: lamp = pi/9.
   real(kind=R_GRID), parameter :: phip = 2.*lamp
   real(kind=R_GRID), parameter :: ppcenter(2) = (/ lamp, phip /)
   real, parameter :: Rp = radius/10.
   real, parameter :: lapse = 5.e-3
   real, parameter :: dT = 4.8e5
   real, parameter :: phiW = 2.*pi/9.
   real, parameter :: pW = 34000.
   real, parameter :: q0 = .018
   real, parameter :: qt = 1.e-12
   real, parameter :: ptrop = 1.e4

   real, parameter :: zconv = 1.e-6
   real, parameter :: rdgrav = rdgas/grav
   real, parameter :: zvir = rvgas/rdgas - 1.
   real, parameter :: rrdgrav = grav/rdgas

   integer :: i,j,k,iter, sphum, cl, cl2, n
   real :: p,z,z0,ziter,piter,titer,uu,vv,pl,pt_u,pt_v
   real(kind=R_GRID), dimension(2) :: pa
   real(kind=R_GRID), dimension(3) :: e1,e2,ex,ey
   real, dimension(is:ie,js:je+1) :: gz_u,p_u,peln_u,ps_u,u1,u2
   real(kind=R_GRID), dimension(is:ie,js:je+1) :: lat_u,lon_u
   real, dimension(is:ie+1,js:je) :: gz_v,p_v,peln_v,ps_v,v1,v2
   real(kind=R_GRID), dimension(is:ie+1,js:je) :: lat_v,lon_v

   !Compute ps, phis, delp, aux pressure variables, Temperature, winds
   ! (with or without perturbation), moisture, Terminator tracer, w, delz

   !Compute p, z, T on both the staggered and unstaggered grids. Then compute the zonal
   !  and meridional winds on both grids, and rotate as needed

   !PS
   do j=js,je
   do i=is,ie
      ps(i,j) = p0
   enddo
   enddo

   !delp 
   do k=1,npz
   do j=js,je
   do i=is,ie
      delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
   enddo
   enddo
   enddo

   !Pressure variables
   do j=js,je
   do i=is,ie
      pe(i,1,j)   = ptop
   enddo
   do i=is,ie
      peln(i,1,j) = log(ptop)
      pk(i,j,1) = ptop**kappa
   enddo
   do k=2,npz+1
   do i=is,ie
      pe(i,k,j)   = ak(k) + ps  (i,j)*bk(k)
   enddo
   do i=is,ie
      pk(i,j,k) = exp(kappa*log(pe(i,k,j)))
      peln(i,k,j) = log(pe(i,k,j))
   enddo
   enddo
   enddo

   do k=1,npz
   do j=js,je
   do i=is,ie
      pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
   enddo
   enddo
   enddo

   !Height: Use Newton's method
   !Cell centered
   do j=js,je
   do i=is,ie
      phis(i,j) = 0.
      gz(i,j,npz+1) = 0.
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je
   do i=is,ie
      p = pe(i,k,j)
      z = gz(i,j,k+1)
      do iter=1,30
         ziter = z
         piter = DCMIP16_BC_pressure(ziter,agrid(i,j,2))
         titer = DCMIP16_BC_temperature(ziter,agrid(i,j,2))
         z = ziter + (piter - p)*rdgrav*titer/piter
!!$         !!! DEBUG CODE
!!$         if (is_master() .and. i == is .and. j == js) then
!!$            write(*,'(A,I,2x,I, 4(2x,F10.3), 2x, F7.3)') ' NEWTON: ' , k, iter, piter, p, ziter, z, titer
!!$         endif
!!$         !!! END DEBUG CODE
         if (abs(z - ziter) < zconv) exit
      enddo      
      gz(i,j,k) = z
   enddo
   enddo
   enddo

   !Temperature: Compute from hydro balance
   do k=1,npz
   do j=js,je
   do i=is,ie
      pt(i,j,k) = rrdgrav * ( gz(i,j,k) - gz(i,j,k+1) ) / ( peln(i,k+1,j) - peln(i,k,j))
   enddo
   enddo
   enddo

   !Compute height and temperature for u and v points also, to be able to compute the local winds
   !Use temporary 2d arrays for this purpose
   do j=js,je+1
   do i=is,ie
      gz_u(i,j) = 0.
      p_u(i,j) = p0
      peln_u(i,j) = log(p0)
      ps_u(i,j) = p0
      call mid_pt_sphere(grid(i,j,:),grid(i+1,j,:),pa)
      lat_u(i,j) = pa(2)
      lon_u(i,j) = pa(1)
      call get_unit_vect2(grid(i,j,:),grid(i+1,j,:),e1)
      call get_latlon_vector(pa,ex,ey)
      u1(i,j) = inner_prod(e1,ex) !u components
      u2(i,j) = inner_prod(e1,ey)
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je+1
   do i=is,ie
      !Pressure (Top of interface)
      p = ak(k) + ps_u(i,j)*bk(k)
      pl = log(p)
      !Height (top of interface); use newton's method
      z = gz_u(i,j) !first guess, height of lower level
      z0 = z
      do iter=1,30
         ziter = z
         piter = DCMIP16_BC_pressure(ziter,lat_u(i,j))
         titer = DCMIP16_BC_temperature(ziter,lat_u(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
         if (abs(z - ziter) < zconv) exit
      enddo
      !Temperature, compute from hydro balance
      pt_u = rrdgrav * ( z - gz_u(i,j) ) / (peln_u(i,j) - pl)
      !Now compute winds. Note no meridional winds
      !!!NOTE: do we need to use LAYER-mean z?
      uu = DCMIP16_BC_uwind(0.5*(z+z0),pt_u,lat_u(i,j))
      if (do_pert) then
         uu = uu + DCMIP16_BC_uwind_pert(0.5*(z+z0),lat_u(i,j),lon_u(i,j))
      endif
      u(i,j,k) = u1(i,j)*uu 

      gz_u(i,j) = z
      p_u(i,j) = p
      peln_u(i,j) = pl
   enddo
   enddo
   enddo

   do j=js,je
   do i=is,ie+1
      gz_v(i,j) = 0.
      p_v(i,j) = p0
      peln_v(i,j) = log(p0)
      ps_v(i,j) = p0
      call mid_pt_sphere(grid(i,j,:),grid(i,j+1,:),pa)
      lat_v(i,j) = pa(2)
      lon_v(i,j) = pa(1)
      call get_unit_vect2(grid(i,j,:),grid(i,j+1,:),e2)
      call get_latlon_vector(pa,ex,ey)
      v1(i,j) = inner_prod(e2,ex) !v components
      v2(i,j) = inner_prod(e2,ey)
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je
   do i=is,ie+1
      !Pressure (Top of interface)
      p = ak(k) + ps_v(i,j)*bk(k)
      pl = log(p)
      !Height (top of interface); use newton's method
      z = gz_v(i,j) !first guess, height of lower level
      z0 = z
      do iter=1,30
         ziter = z
         piter = DCMIP16_BC_pressure(ziter,lat_v(i,j))
         titer = DCMIP16_BC_temperature(ziter,lat_v(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
         if (abs(z - ziter) < zconv) exit
      enddo
      !Temperature, compute from hydro balance
      pt_v = rrdgrav * ( z - gz_v(i,j) ) / (peln_v(i,j) - pl)
      !Now compute winds
      uu = DCMIP16_BC_uwind(0.5*(z+z0),pt_v,lat_v(i,j))
      if (do_pert) then
         uu = uu + DCMIP16_BC_uwind_pert(0.5*(z+z0),lat_v(i,j),lon_v(i,j))
      endif
      v(i,j,k) = v1(i,j)*uu
      gz_v(i,j) = z
      p_v(i,j) = p
      peln_v(i,j) = pl
   enddo
   enddo
   enddo

   !Compute moisture and other tracer fields, as desired
   do n=1,nq
   do k=1,npz
   do j=jsd,jed
   do i=isd,ied
      q(i,j,k,n) = 0.
   enddo
   enddo
   enddo
   enddo
   if (.not. adiabatic) then
      sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
      do k=1,npz
      do j=js,je
      do i=is,ie
         p = delp(i,j,k)/(peln(i,k+1,j) - peln(i,k,j))
         q(i,j,k,sphum) = DCMIP16_BC_sphum(p,ps(i,j),agrid(i,j,2),agrid(i,j,1))
         !Convert pt to non-virtual temperature
         pt(i,j,k) = pt(i,j,k) / ( 1. + zvir*q(i,j,k,sphum))
      enddo
      enddo
      enddo
   endif

   cl = get_tracer_index(MODEL_ATMOS, 'cl')
   cl2 = get_tracer_index(MODEL_ATMOS, 'cl2')
   if (cl > 0 .and. cl2 > 0) then
      call terminator_tracers(is,ie,js,je,isd,ied,jsd,jed,npz, &
           q, delp,nq,agrid(isd,jsd,1),agrid(isd,jsd,2))
      call mpp_update_domains(q,domain)
   endif

   !Compute nonhydrostatic variables, if needed
   if (.not. hydrostatic) then
      do k=1,npz
      do j=js,je
      do i=is,ie
         w(i,j,k) = 0.
         delz(i,j,k) = gz(i,j,k) - gz(i,j,k+1)
      enddo
      enddo
      enddo
   endif

 contains

   
   real function DCMIP16_BC_temperature(z, lat)

     real, intent(IN) :: z
     real(kind=R_GRID), intent(IN) :: lat
     real :: IT, T1, T2, Tr, zsc

     IT = exp(KK * log(cos(lat))) - KK/(KK+2.)*exp((KK+2.)*log(cos(lat)))
     zsc = z*grav/(b*Rdgas*T0)
     Tr = ( 1. - 2.*zsc**2.) * exp(-zsc**2. )

     T1 = (1./T0)*exp(lapse*z/T0) + (T0 - Tp)/(T0*Tp) * Tr
     T2 = 0.5* ( KK + 2.) * (Te - Tp)/(Te*Tp) * Tr

     DCMIP16_BC_temperature = 1./(T1 - T2*IT)

   end function DCMIP16_BC_temperature

   real function DCMIP16_BC_pressure(z,lat)

     real, intent(IN) :: z
     real(kind=R_GRID), intent(IN) :: lat
     real :: IT, Ti1, Ti2, Tir

     IT = exp(KK * log(cos(lat))) - KK/(KK+2.)*exp((KK+2.)*log(cos(lat)))
     Tir = z*exp(-(z*grav/(b*Rdgas*T0))*(z*grav/(b*Rdgas*T0)) )

     Ti1 = 1./lapse* (exp(lapse*z/T0) - 1.) + Tir*(T0-Tp)/(T0*Tp)
     Ti2 = 0.5*(KK+2.)*(Te-Tp)/(Te*Tp) * Tir

     DCMIP16_BC_pressure = p0*exp(-grav/Rdgas * ( Ti1 - Ti2*IT))

   end function DCMIP16_BC_pressure

   real function DCMIP16_BC_uwind(z,T,lat)

     real, intent(IN) :: z, T
     real(kind=R_GRID), intent(IN) :: lat
     real :: Tir, Ti2, UU, ur

     Tir = z*exp(-(z*grav/(b*Rdgas*T0))*(z*grav/(b*Rdgas*T0)) )
     Ti2 = 0.5*(KK+2.)*(Te-Tp)/(Te*Tp) * Tir

     UU = grav*KK/radius * Ti2 * ( cos(lat)**(int(KK)-1) - cos(lat)**(int(KK)+1) ) * T
     ur = - omega * radius * cos(lat) + sqrt( (omega*radius*cos(lat))**2 + radius*cos(lat)*UU)

     DCMIP16_BC_uwind = ur

   end function DCMIP16_BC_uwind

   real function DCMIP16_BC_uwind_pert(z,lat,lon)

     real, intent(IN) :: z
     real(kind=R_GRID), intent(IN) :: lat, lon
     real :: ZZ, zrat
     real(kind=R_GRID) :: dst, pphere(2)

     zrat = z/zp
     ZZ = max(1. - 3.*zrat*zrat + 2.*zrat*zrat*zrat, 0.)

     pphere = (/ lon, lat /)
     dst = great_circle_dist(pphere, ppcenter, radius)
     
     DCMIP16_BC_uwind_pert = max(0., up*ZZ*exp(-(dst/Rp)**2) )

   end function DCMIP16_BC_uwind_pert

   real function DCMIP16_BC_sphum(p,ps,lat, lon)

     real, intent(IN) :: p, ps
     real(kind=R_GRID), intent(IN) :: lat, lon
     real :: eta

     eta = p/ps

     DCMIP16_BC_sphum = qt
     if (p > ptrop) then
        DCMIP16_BC_sphum = q0 * exp(-(lat/phiW)**4) * exp(-( (eta-1.)*p0/pw)**2)
     endif

   end function DCMIP16_BC_sphum

 end subroutine DCMIP16_BC

 subroutine DCMIP16_TC(delp,pt,u,v,q,w,delz,&
      is,ie,js,je,isd,ied,jsd,jed,npz,nq,ak,bk,ptop, &
      pk,peln,pe,pkz,gz,phis,ps,grid,agrid, &
      hydrostatic, nwat, adiabatic)

   integer, intent(IN) :: is,ie,js,je,isd,ied,jsd,jed,npz,nq, nwat
   real, intent(IN) :: ptop
   real, intent(IN), dimension(npz+1) :: ak, bk
   real, intent(INOUT), dimension(isd:ied,jsd:jed,npz,nq) :: q
   real, intent(OUT), dimension(isd:ied,jsd:jed,npz) :: delp, pt, w, delz
   real, intent(OUT), dimension(isd:ied,jsd:jed+1,npz) :: u
   real, intent(OUT), dimension(isd:ied+1,jsd:jed,npz) :: v
   real, intent(OUT), dimension(is:ie,js:je,npz+1) :: pk
   real, intent(OUT), dimension(is:ie,npz+1,js:je) :: peln
   real, intent(OUT), dimension(is-1:ie+1,npz+1,js-1:je+1) :: pe
   real, intent(OUT), dimension(is:ie,js:je,npz) :: pkz
   real, intent(OUT), dimension(isd:ied,jsd:jed) :: phis,ps
   real(kind=R_GRID), intent(IN), dimension(isd:ied,jsd:jed,2) :: agrid
   real(kind=R_GRID), intent(IN), dimension(isd:ied+1,jsd:jed+1,2) :: grid
   real, intent(OUT), dimension(isd:ied,jsd:jed,npz+1) :: gz
   logical, intent(IN) :: hydrostatic,adiabatic

   real, parameter :: zt = 15000 !< m
   real, parameter :: q0 = 0.021 !< kg/kg
   real, parameter :: qt = 1.e-11 !< kg/kg
   real, parameter :: T0 = 302.15 !< K
   real, parameter :: Tv0 = 302.15*(1.+0.608*q0) !< K
   real, parameter :: Ts = 302.15 !< K
   real, parameter :: zq1 = 3000. !< m
   real, parameter :: zq2 = 8000. !< m
   real, parameter :: lapse = 7.e-3 !< K/m
   real, parameter :: Tvt = Tv0 - lapse*zt !< K
   real, parameter :: pb = 101500. !< Pa
   real, parameter :: ptt = pb*(TvT/Tv0)**(grav/Rdgas/lapse)
   real(kind=R_GRID), parameter :: lamp = pi
   real(kind=R_GRID), parameter :: phip = pi/18.
   real(kind=R_GRID), parameter :: ppcenter(2) = (/ lamp, phip /)
   real, parameter :: dp = 1115. !< Pa
   real, parameter :: rp = 282000. !< m
   real, parameter :: zp = 7000. !< m
   real, parameter :: fc = 2.*OMEGA*sin(phip)

   real, parameter :: zconv = 1.e-6
   real, parameter :: rdgrav = rdgas/grav
   real, parameter :: rrdgrav = grav/rdgas

   integer :: i,j,k,iter, sphum, cl, cl2, n
   real :: p,z,z0,ziter,piter,titer,uu,vv,pl, r
   real(kind=R_GRID), dimension(2) :: pa
   real(kind=R_GRID), dimension(3) :: e1,e2,ex,ey
   real, dimension(is:ie,js:je)   :: rc
   real, dimension(is:ie,js:je+1) :: gz_u,p_u,peln_u,ps_u,u1,u2, rc_u
   real(kind=R_GRID), dimension(is:ie,js:je+1) :: lat_u,lon_u
   real, dimension(is:ie+1,js:je) :: gz_v,p_v,peln_v,ps_v,v1,v2, rc_v
   real(kind=R_GRID), dimension(is:ie+1,js:je) :: lat_v,lon_v

   !Compute ps, phis, delp, aux pressure variables, Temperature, winds
   ! (with or without perturbation), moisture, w, delz

   !Compute p, z, T on both the staggered and unstaggered grids. Then compute the zonal
   !  and meridional winds on both grids, and rotate as needed

   !Save r for easy use
   do j=js,je
   do i=is,ie
      rc(i,j) = great_circle_dist(agrid(i,j,:), ppcenter, radius)
   enddo
   enddo

   !PS
   do j=js,je
   do i=is,ie
      ps(i,j) = pb - dp*exp( -sqrt((rc(i,j)/rp)**3) )
   enddo
   enddo

   !delp 
   do k=1,npz
   do j=js,je
   do i=is,ie
      delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
   enddo
   enddo
   enddo

   !Pressure variables
   do j=js,je
   do i=is,ie
      pe(i,1,j)   = ptop
   enddo
   do i=is,ie
      peln(i,1,j) = log(ptop)
      pk(i,j,1) = ptop**kappa
   enddo
   do k=2,npz+1
   do i=is,ie
      pe(i,k,j)   = ak(k) + ps  (i,j)*bk(k)
   enddo
   do i=is,ie
      pk(i,j,k) = exp(kappa*log(pe(i,k,j)))
      peln(i,k,j) = log(pe(i,k,j))
   enddo
   enddo
   enddo

   do k=1,npz
   do j=js,je
   do i=is,ie
      pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
   enddo
   enddo
   enddo

   !Height: Use Newton's method
   !Cell centered
   do j=js,je
   do i=is,ie
      phis(i,j) = 0.
      gz(i,j,npz+1) = 0.
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je
   do i=is,ie
      p = pe(i,k,j)
      z = gz(i,j,k+1)
      do iter=1,30
         ziter = z
         piter = DCMIP16_TC_pressure(ziter,rc(i,j))
         titer = DCMIP16_TC_temperature(ziter,rc(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
!!$         !!! DEBUG CODE
!!$         if (is_master() .and. i == is .and. j == js) then
!!$            write(*,'(A,I,2x,I, 4(2x,F10.3), 2x, F7.3)') ' NEWTON: ' , k, iter, piter, p, ziter, z, titer
!!$         endif
!!$         !!! END DEBUG CODE
         if (abs(z - ziter) < zconv) exit
      enddo      
      gz(i,j,k) = z
   enddo
   enddo
   enddo

   !Temperature: Compute from hydro balance
   do k=1,npz
   do j=js,je
   do i=is,ie
      pt(i,j,k) = rrdgrav * ( gz(i,j,k) - gz(i,j,k+1) ) / ( peln(i,k+1,j) - peln(i,k,j))
   enddo
   enddo
   enddo

   !Compute height and temperature for u and v points also, to be able to compute the local winds
   !Use temporary 2d arrays for this purpose
   do j=js,je+1
   do i=is,ie
      call mid_pt_sphere(grid(i,j,:),grid(i+1,j,:),pa)
      lat_u(i,j) = pa(2)
      lon_u(i,j) = pa(1)
      call get_unit_vect2(grid(i,j,:),grid(i+1,j,:),e1)
      call get_latlon_vector(pa,ex,ey)
      u1(i,j) = inner_prod(e1,ex) !u components
      u2(i,j) = inner_prod(e1,ey)
      rc_u(i,j) = great_circle_dist(pa, ppcenter, radius)
      gz_u(i,j) = 0.
      p_u(i,j) = pb - dp*exp( -sqrt((rc_u(i,j)/rp)**3) )
      peln_u(i,j) = log(p_u(i,j))
      ps_u(i,j) = p_u(i,j)
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je+1
   do i=is,ie
      !Pressure (Top of interface)
      p = ak(k) + ps_u(i,j)*bk(k)
      pl = log(p)
      !Height (top of interface); use newton's method
      z = gz_u(i,j) !first guess, height of lower level
      z0 = z
      do iter=1,30
         ziter = z
         piter = DCMIP16_TC_pressure(ziter,rc_u(i,j))
         titer = DCMIP16_TC_temperature(ziter,rc_u(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
         if (abs(z - ziter) < zconv) exit
      enddo
      !Now compute winds
      call DCMIP16_TC_uwind_pert(0.5*(z+z0),rc_u(i,j),lon_u(i,j),lat_u(i,j), uu, vv)
      u(i,j,k) = u1(i,j)*uu + u2(i,j)*vv

      gz_u(i,j) = z
      p_u(i,j) = p
      peln_u(i,j) = pl
   enddo
   enddo
   enddo

   do j=js,je
   do i=is,ie+1
      call mid_pt_sphere(grid(i,j,:),grid(i,j+1,:),pa)
      lat_v(i,j) = pa(2)
      lon_v(i,j) = pa(1)
      call get_unit_vect2(grid(i,j,:),grid(i,j+1,:),e2)
      call get_latlon_vector(pa,ex,ey)
      v1(i,j) = inner_prod(e2,ex) !v components
      v2(i,j) = inner_prod(e2,ey)
      rc_v(i,j) = great_circle_dist(pa, ppcenter, radius)
      gz_v(i,j) = 0.
      p_v(i,j) = pb - dp*exp( - sqrt((rc_v(i,j)/rp)**3) )
      peln_v(i,j) = log(p_v(i,j))
      ps_v(i,j) = p_v(i,j)
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je
   do i=is,ie+1
      !Pressure (Top of interface)
      p = ak(k) + ps_v(i,j)*bk(k)
      pl = log(p)
      !Height (top of interface); use newton's method
      z = gz_v(i,j) !first guess, height of lower level
      z0 = z
      do iter=1,30
         ziter = z
         piter = DCMIP16_TC_pressure(ziter,rc_v(i,j))
         titer = DCMIP16_TC_temperature(ziter,rc_v(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
         if (abs(z - ziter) < zconv) exit
      enddo
      !Now compute winds
      call DCMIP16_TC_uwind_pert(0.5*(z+z0),rc_v(i,j),lon_v(i,j),lat_v(i,j), uu, vv)
      v(i,j,k) = v1(i,j)*uu + v2(i,j)*vv
      gz_v(i,j) = z
      p_v(i,j) = p
      peln_v(i,j) = pl
   enddo
   enddo
   enddo

   !Compute moisture and other tracer fields, as desired
   do n=1,nq
   do k=1,npz
   do j=jsd,jed
   do i=isd,ied
      q(i,j,k,n) = 0.
   enddo
   enddo
   enddo
   enddo
   if (.not. adiabatic) then
      sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
      do k=1,npz
      do j=js,je
      do i=is,ie
         z = 0.5*(gz(i,j,k) + gz(i,j,k+1))
         q(i,j,k,sphum) = DCMIP16_TC_sphum(z)
      enddo
      enddo
      enddo
   endif

   !Compute nonhydrostatic variables, if needed
   if (.not. hydrostatic) then
      do k=1,npz
      do j=js,je
      do i=is,ie
         w(i,j,k) = 0.
         delz(i,j,k) = gz(i,j,k) - gz(i,j,k+1)
      enddo
      enddo
      enddo
   endif

 contains

   !Initialize with virtual temperature
   real function DCMIP16_TC_temperature(z, r)

     real, intent(IN) :: z, r
     real :: Tv, term1, term2

     if (z > zt) then
        DCMIP16_TC_temperature = Tvt
        return
     endif

     Tv = Tv0 - lapse*z
     term1 = grav*zp*zp* ( 1. - pb/dp * exp( sqrt(r/rp)**3 + (z/zp)**2 ) )
     term2 = 2*rdgas*Tv*z
     DCMIP16_TC_temperature = Tv + Tv*( 1./(1 + term2/term1) - 1.)

   end function DCMIP16_TC_temperature

   !Initialize with moist air mass
   real function DCMIP16_TC_pressure(z, r)

     real, intent(IN) :: z, r

     if (z <= zt) then
        DCMIP16_TC_pressure = pb*exp(grav/(Rdgas*lapse) * log( (Tv0-lapse*z)/Tv0) ) -dp* exp(-sqrt((r/rp)**3) - (z/zp)**2) * &
             exp( grav/(Rdgas*lapse) * log( (Tv0-lapse*z)/Tv0) ) 
     else
        DCMIP16_TC_pressure = ptt*exp(grav*(zt-z)/(Rdgas*Tvt))
     endif

   end function DCMIP16_TC_pressure

   subroutine DCMIP16_TC_uwind_pert(z,r,lon,lat,uu,vv)

     real, intent(IN) :: z, r
     real(kind=R_GRID), intent(IN) :: lon, lat
     real, intent(OUT) :: uu, vv
     real :: rfac, Tvrd, vt, fr5, d1, d2, d
     real(kind=R_GRID) :: dst, pphere(2)

     if (z > zt) then
        uu = 0.
        vv = 0.
        return
     endif

     rfac = sqrt(r/rp)**3

     fr5 = 0.5*fc*r
     Tvrd = (Tv0 - lapse*z)*Rdgas

     vt = -fr5 + sqrt( fr5**2 - (1.5 * rfac * Tvrd) / &
          ( 1. + 2*Tvrd*z/(grav*zp**2) - pb/dp*exp( rfac + (z/zp)**2) ) )
     
     d1 = sin(phip)*cos(lat) - cos(phip)*sin(lat)*cos(lon - lamp)
     d2 = cos(phip)*sin(lon - lamp)
     d = max(1.e-25,sqrt(d1*d1 + d2*d2))

     uu = vt * d1/d
     vv = vt * d2/d

   end subroutine DCMIP16_TC_uwind_pert

   real function DCMIP16_TC_sphum(z)

     real, intent(IN) :: z

     DCMIP16_TC_sphum = qt
     if (z < zt) then
        DCMIP16_TC_sphum = q0 * exp(-z/zq1) * exp(-(z/zq2 )**2)
     endif

   end function DCMIP16_TC_sphum

 end subroutine DCMIP16_TC

      subroutine init_latlon(u,v,pt,delp,q,phis, ps,pe,peln,pk,pkz,  uc,vc, ua,va, ak, bk,  &
                             gridstruct, npx, npy, npz, ng, ncnst, ndims, nregions, dry_mass,    &
                             mountain, moist_phys, hybrid_z, delz, ze0, domain_in, tile_in)

        real ,      intent(INOUT) ::    u(isd:ied  ,jsd:jed+1,npz)
        real ,      intent(INOUT) ::    v(isd:ied+1,jsd:jed  ,npz)
        real ,      intent(INOUT) ::   pt(isd:ied  ,jsd:jed  ,npz)
        real ,      intent(INOUT) :: delp(isd:ied  ,jsd:jed  ,npz)
        real ,      intent(INOUT) ::    q(isd:ied  ,jsd:jed  ,npz, ncnst)
        
        real ,      intent(INOUT) :: phis(isd:ied  ,jsd:jed  )

        real ,      intent(INOUT) ::   ps(isd:ied  ,jsd:jed  )
        real ,      intent(INOUT) ::   pe(is-1:ie+1,npz+1,js-1:je+1)
        real ,      intent(INOUT) ::   pk(is:ie    ,js:je    ,npz+1)
        real ,      intent(INOUT) :: peln(is :ie   ,npz+1    ,js:je)
        real ,      intent(INOUT) ::  pkz(is:ie    ,js:je    ,npz  )
        real ,      intent(INOUT) ::   uc(isd:ied+1,jsd:jed  ,npz)
        real ,      intent(INOUT) ::   vc(isd:ied  ,jsd:jed+1,npz)
        real ,      intent(INOUT) ::   ua(isd:ied  ,jsd:jed  ,npz)
        real ,      intent(INOUT) ::   va(isd:ied  ,jsd:jed  ,npz)
        real ,      intent(inout) :: delz(isd:,jsd:,1:)
        real ,      intent(inout)   ::  ze0(is:,js:,1:)
        
        real ,      intent(IN)    ::   ak(npz+1)
        real ,      intent(IN)    ::   bk(npz+1)
        
        integer,      intent(IN) :: npx, npy, npz
        integer,      intent(IN) :: ng, ncnst
        integer,      intent(IN) :: ndims
        integer,      intent(IN) :: nregions
        integer,target,intent(IN):: tile_in
        
        real,         intent(IN) :: dry_mass
        logical,      intent(IN) :: mountain
        logical,      intent(IN) :: moist_phys
        logical,      intent(IN) :: hybrid_z

        type(fv_grid_type), intent(IN), target :: gridstruct
        type(domain2d), intent(IN), target :: domain_in

        real, pointer, dimension(:,:,:)   :: agrid, grid
        real, pointer, dimension(:,:)     :: area, rarea, fC, f0
        real, pointer, dimension(:,:,:)   :: ee1, ee2, en1, en2
        real, pointer, dimension(:,:,:,:) :: ew, es
        real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

        logical, pointer :: cubed_sphere, latlon

        type(domain2d), pointer :: domain
        integer, pointer :: tile

        logical, pointer :: have_south_pole, have_north_pole

        integer, pointer :: ntiles_g
        real,    pointer :: acapN, acapS, globalarea

        real(kind=R_GRID) :: p1(2), p2(2)
        real :: r, r0
        integer :: i,j

        agrid => gridstruct%agrid
        grid  => gridstruct%grid

        area => gridstruct%area

        dx      => gridstruct%dx
        dy      => gridstruct%dy
        dxa     => gridstruct%dxa
        dya     => gridstruct%dya
        rdxa    => gridstruct%rdxa
        rdya    => gridstruct%rdya
        dxc     => gridstruct%dxc
        dyc     => gridstruct%dyc

        fC    => gridstruct%fC
        f0    => gridstruct%f0

        ntiles_g                      => gridstruct%ntiles_g
        acapN                         => gridstruct%acapN
        acapS                         => gridstruct%acapS
        globalarea                    => gridstruct%globalarea

        domain => domain_in
        tile => tile_in

        have_south_pole               => gridstruct%have_south_pole
        have_north_pole               => gridstruct%have_north_pole

        do j=jsd,jed+1
           do i=isd,ied+1
              fc(i,j) = 2.*omega*( -cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha)  &
                                   +sin(grid(i,j,2))*cos(alpha) )
           enddo
        enddo
        do j=jsd,jed
           do i=isd,ied
              f0(i,j) = 2.*omega*( -cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha)  &
                                   +sin(agrid(i,j,2))*cos(alpha) )
           enddo
        enddo

        select case (test_case)
        case ( 1 )

         Ubar = (2.0*pi*radius)/(12.0*86400.0)
         phis = 0.0
         r0 = radius/3. !RADIUS radius/3.
!!$         p1(1) = 0.
         p1(1) = pi/2. + pi_shift
         p1(2) = 0.
         do j=jsd,jed
            do i=isd,ied
               p2(1) = agrid(i,j,1)
               p2(2) = agrid(i,j,2)
               r = great_circle_dist( p1, p2, radius )
               if (r < r0) then
                  delp(i,j,1) = phis(i,j) + 0.5*(1.0+cos(PI*r/r0))
               else
                  delp(i,j,1) = phis(i,j)
               endif
            enddo
         enddo
         call init_latlon_winds(UBar, u, v, ua, va, uc, vc, 1, gridstruct)


!!$           phis(:,:)=0.
!!$
!!$           u (:,:,:)=10.
!!$           v (:,:,:)=10.
!!$           ua(:,:,:)=10.
!!$           va(:,:,:)=10.
!!$           uc(:,:,:)=10.
!!$           vc(:,:,:)=10.
!!$           pt(:,:,:)=1.
!!$           delp(:,:,:)=0.
!!$           
!!$           do j=js,je
!!$              if (j>10 .and. j<15) then
!!$                 do i=is,ie
!!$                    if (i>10 .and. i<15) then
!!$                       delp(i,j,:)=1.
!!$                    endif
!!$                 enddo
!!$              endif
!!$           enddo
!!$           call mpp_update_domains( delp, domain )

        end select

        nullify(grid)
        nullify(agrid)

        nullify(area)

        nullify(fC)
        nullify(f0)

      nullify(dx)      
      nullify(dy)      
      nullify(dxa)     
      nullify(dya)     
      nullify(rdxa)    
      nullify(rdya)    
      nullify(dxc)     
      nullify(dyc)     

      nullify(domain)
      nullify(tile)
      
      nullify(have_south_pole) 
      nullify(have_north_pole) 

      nullify(ntiles_g)        
      nullify(acapN)           
      nullify(acapS)           
      nullify(globalarea)      

      end subroutine init_latlon

      subroutine init_latlon_winds(UBar, u, v, ua, va, uc, vc, defOnGrid, gridstruct)

        ! defOnGrid = -1:null_op, 0:All-Grids, 1:C-Grid, 2:D-Grid, 3:A-Grid, 4:A-Grid then Rotate, 5:D-Grid with unit vectors then Rotate

        real,    intent(INOUT) :: UBar
        real,    intent(INOUT) ::  u(isd:ied  ,jsd:jed+1)
        real,    intent(INOUT) ::  v(isd:ied+1,jsd:jed  )
        real,    intent(INOUT) :: uc(isd:ied+1,jsd:jed  )
        real,    intent(INOUT) :: vc(isd:ied  ,jsd:jed+1)
        real,    intent(INOUT) :: ua(isd:ied  ,jsd:jed  )
        real,    intent(INOUT) :: va(isd:ied  ,jsd:jed  )
        integer, intent(IN)    :: defOnGrid
        type(fv_grid_type), intent(IN), target :: gridstruct

        real   :: p1(2),p2(2),p3(2),p4(2), pt(2)
        real :: e1(3), e2(3), ex(3), ey(3)

        real   :: dist, r, r0 
        integer :: i,j,k,n
        real :: utmp, vtmp

        real :: psi_b(isd:ied+1,jsd:jed+1), psi(isd:ied,jsd:jed), psi1, psi2 

        real, dimension(:,:,:), pointer :: grid, agrid
        real, dimension(:,:),   pointer :: area, dx, dy, dxc, dyc

        grid => gridstruct%grid
        agrid=> gridstruct%agrid

        area  => gridstruct%area
        dx    => gridstruct%dx
        dy    => gridstruct%dy
        dxc   => gridstruct%dxc
        dyc   => gridstruct%dyc

        psi(:,:) = 1.e25
        psi_b(:,:) = 1.e25
        do j=jsd,jed
           do i=isd,ied
              psi(i,j) = (-1.0 * Ubar * radius *( sin(agrid(i,j,2))                  *cos(alpha) - &
                                                  cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) ) )
           enddo
        enddo
        do j=jsd,jed+1
           do i=isd,ied+1
              psi_b(i,j) = (-1.0 * Ubar * radius *( sin(grid(i,j,2))                 *cos(alpha) - &
                                                    cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) ) )
           enddo
        enddo
        
        if ( defOnGrid == 1 ) then
           do j=jsd,jed+1
              do i=isd,ied
                 dist = dx(i,j)
                 vc(i,j) = (psi_b(i+1,j)-psi_b(i,j))/dist
                 if (dist==0) vc(i,j) = 0.
              enddo
           enddo
           do j=jsd,jed
              do i=isd,ied+1
                 dist = dy(i,j)
                 uc(i,j) = -1.0*(psi_b(i,j+1)-psi_b(i,j))/dist
                 if (dist==0) uc(i,j) = 0.
              enddo
           enddo

           
           do j=js,je
              do i=is,ie+1
                 dist = dxc(i,j)
                 v(i,j) = (psi(i,j)-psi(i-1,j))/dist
                 if (dist==0) v(i,j) = 0.            
              enddo
           enddo
           do j=js,je+1
              do i=is,ie
                 dist = dyc(i,j)
                 u(i,j) = -1.0*(psi(i,j)-psi(i,j-1))/dist
                 if (dist==0) u(i,j) = 0. 
              enddo
           enddo
        endif
     
      end subroutine init_latlon_winds

 subroutine d2a2c(im,jm,km, ifirst,ilast, jfirst,jlast, ng, nested, &
                  u,v, ua,va, uc,vc, gridstruct, domain)

! Input
  integer, intent(IN) :: im,jm,km
  integer, intent(IN) :: ifirst,ilast
  integer, intent(IN) :: jfirst,jlast
  integer, intent(IN) :: ng
  logical, intent(IN) :: nested
  type(fv_grid_type), intent(IN), target :: gridstruct
  type(domain2d), intent(INOUT) :: domain

  !real   , intent(in) :: sinlon(im,jm)
  !real   , intent(in) :: coslon(im,jm)
  !real   , intent(in) :: sinl5(im,jm)
  !real   , intent(in) :: cosl5(im,jm)

! Output
 ! real   , intent(inout) ::  u(ifirst-ng:ilast+ng,jfirst-ng:jlast+1+ng)
 ! real   , intent(inout) ::  v(ifirst-ng:ilast+1+ng,jfirst-ng:jlast+ng)
 ! real   , intent(inout) :: ua(ifirst-ng:ilast+ng,jfirst-ng:jlast+ng)
 ! real   , intent(inout) :: va(ifirst-ng:ilast+ng,jfirst-ng:jlast+ng)
 ! real   , intent(inout) :: uc(ifirst-ng:ilast+1+ng,jfirst-ng:jlast+ng)
 ! real   , intent(inout) :: vc(ifirst-ng:ilast+ng,jfirst-ng:jlast+1+ng)

  real   , intent(inout) ::  u(isd:ied,jsd:jed+1) !ifirst-ng:ilast+ng,jfirst-ng:jlast+1+ng)
  real   , intent(inout) ::  v(isd:ied+1,jsd:jed) !ifirst-ng:ilast+1+ng,jfirst-ng:jlast+ng)
  real   , intent(inout) :: ua(isd:ied,jsd:jed)   !ifirst-ng:ilast+ng,jfirst-ng:jlast+ng)
  real   , intent(inout) :: va(isd:ied,jsd:jed)   !(ifirst-ng:ilast+ng,jfirst-ng:jlast+ng)
  real   , intent(inout) :: uc(isd:ied+1,jsd:jed) !(ifirst-ng:ilast+1+ng,jfirst-ng:jlast+ng)
  real   , intent(inout) :: vc(isd:ied,jsd:jed+1) !(ifirst-ng:ilast+ng,jfirst-ng:jlast+1+ng)

!--------------------------------------------------------------
! Local 

  real   :: sinlon(im,jm)
  real   :: coslon(im,jm)
  real   :: sinl5(im,jm)
  real   :: cosl5(im,jm)

    real :: tmp1(jsd:jed+1)
    real :: tmp2(jsd:jed)
    real :: tmp3(jsd:jed)

    real  mag,mag1,mag2, ang,ang1,ang2 
    real  us, vs, un, vn
    integer i, j, k, im2
    integer js1g1
    integer js2g1
    integer js2g2
    integer js2gc
    integer js2gc1
    integer js2gcp1
    integer js2gd
    integer jn2gc
    integer jn1g1
    integer jn1g2
    integer jn2gd
    integer jn2gsp1

      real, pointer, dimension(:,:,:)   :: agrid, grid
      real, pointer, dimension(:,:)     :: area, rarea, fC, f0
      real(kind=R_GRID), pointer, dimension(:,:,:)   :: ee1, ee2, en1, en2
      real(kind=R_GRID), pointer, dimension(:,:,:,:) :: ew, es
      real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

      logical, pointer :: cubed_sphere, latlon

      logical, pointer :: have_south_pole, have_north_pole

      integer, pointer :: ntiles_g
      real,    pointer :: acapN, acapS, globalarea

      grid => gridstruct%grid
      agrid=> gridstruct%agrid

      area  => gridstruct%area
      rarea => gridstruct%rarea

      fC    => gridstruct%fC
      f0    => gridstruct%f0

      ee1   => gridstruct%ee1
      ee2   => gridstruct%ee2
      ew    => gridstruct%ew
      es    => gridstruct%es
      en1   => gridstruct%en1
      en2   => gridstruct%en2

      dx      => gridstruct%dx
      dy      => gridstruct%dy
      dxa     => gridstruct%dxa
      dya     => gridstruct%dya
      rdxa    => gridstruct%rdxa
      rdya    => gridstruct%rdya
      dxc     => gridstruct%dxc
      dyc     => gridstruct%dyc
      
      cubed_sphere => gridstruct%cubed_sphere
      latlon       => gridstruct%latlon

      have_south_pole               => gridstruct%have_south_pole
      have_north_pole               => gridstruct%have_north_pole

      ntiles_g                      => gridstruct%ntiles_g
      acapN                         => gridstruct%acapN
      acapS                         => gridstruct%acapS
      globalarea                    => gridstruct%globalarea

 if (cubed_sphere) then

    call dtoa( u, v,ua,va,dx,dy,dxa,dya,dxc,dyc,im,jm,ng)
    if (.not. nested) call fill_corners(ua, va, im, jm, VECTOR=.true., AGRID=.true.)
    call atoc(ua,va,uc,vc,dx,dy,dxa,dya,im,jm,ng, nested, domain, noComm=.true.)
    if (.not. nested) call fill_corners(uc, vc, im, jm, VECTOR=.true., CGRID=.true.)

 else  ! Lat-Lon

    im2 = im/2

! Set loop limits

    js1g1   = jfirst-1
    js2g1   = jfirst-1
    js2g2   = jfirst-2
    js2gc   = jfirst-ng
    js2gcp1 = jfirst-ng-1
    js2gd   = jfirst-ng
    jn1g1   = jlast+1
    jn1g2   = jlast+2
    jn2gc   = jlast+ng
    jn2gd   = jlast+ng-1
    jn2gsp1 = jlast+ng-1

    if (have_south_pole) then
       js1g1   = 1
       js2g1   = 2
       js2g2   = 2
       js2gc   = 2
       js2gcp1 = 2   ! NG-1 latitudes on S (starting at 2)
       js2gd   = 2
    endif
    if (have_north_pole) then
       jn1g1   = jm
       jn1g2   = jm
       jn2gc   = jm-1  ! NG latitudes on N (ending at jm-1)
       jn2gd   = jm-1
       jn2gsp1 = jm-1
    endif
!
! Treat the special case of ng = 1
!
    if ( ng == 1 .AND. ng > 1 ) THEN
        js2gc1 = js2gc
    else
        js2gc1 = jfirst-ng+1
        if (have_south_pole) js2gc1 = 2  ! NG-1 latitudes on S (starting at 2)
    endif

  do k=1,km

       if ((have_south_pole) .or. (have_north_pole)) then
! Get D-grid V-wind at the poles.
          call vpol5(u(1:im,:), v(1:im,:), im, jm,            &
                     coslon, sinlon, cosl5, sinl5, ng, ng, jfirst, jlast )
          call mp_ghost_ew(im,jm,1,1, ifirst,ilast, jfirst,jlast, 1,1, ng,ng, ng,ng, v(:,:))
       endif

       call dtoa(u, v, ua, va, dx,dy,dxa,dya,dxc,dyc,im, jm, ng)
       if (.not. nested) call fill_corners(ua, va, im, jm, VECTOR=.true., AGRID=.true.)

       if ( have_south_pole ) then
! Projection at SP
          us = 0.
          vs = 0.
          do i=1,im2
            us = us + (ua(i+im2,2)-ua(i,2))*sinlon(i,2)         &
                    + (va(i,2)-va(i+im2,2))*coslon(i,2)
            vs = vs + (ua(i+im2,2)-ua(i,2))*coslon(i,2)         &
                    + (va(i+im2,2)-va(i,2))*sinlon(i,2)
          enddo
          us = us/im
          vs = vs/im
! SP
          do i=1,im2
            ua(i,1)  = -us*sinlon(i,1) - vs*coslon(i,1)
            va(i,1)  =  us*coslon(i,1) - vs*sinlon(i,1)
            ua(i+im2,1)  = -ua(i,1)
            va(i+im2,1)  = -va(i,1)
          enddo
          ua(0   ,1) = ua(im,1)
          ua(im+1,1) = ua(1 ,1)
          va(im+1,1) = va(1 ,1)
        endif

        if ( have_north_pole ) then
! Projection at NP
          un = 0.
          vn = 0.
          j = jm-1
          do i=1,im2
            un = un + (ua(i+im2,j)-ua(i,j))*sinlon(i,j)        &
                    + (va(i+im2,j)-va(i,j))*coslon(i,j)
            vn = vn + (ua(i,j)-ua(i+im2,j))*coslon(i,j)        &
                    + (va(i+im2,j)-va(i,j))*sinlon(i,j)
          enddo
          un = un/im
          vn = vn/im
! NP
          do i=1,im2
            ua(i,jm) = -un*sinlon(i,jm) + vn*coslon(i,jm)
            va(i,jm) = -un*coslon(i,jm) - vn*sinlon(i,jm)
            ua(i+im2,jm) = -ua(i,jm)
            va(i+im2,jm) = -va(i,jm)
          enddo
          ua(0   ,jm) = ua(im,jm)
          ua(im+1,jm) = ua(1 ,jm)
          va(im+1,jm) = va(1 ,jm)
        endif

        if (latlon) call mp_ghost_ew(im,jm,1,1, ifirst,ilast, jfirst,jlast, 1,1, ng,ng, ng,ng, ua(:,:))
        if (latlon) call mp_ghost_ew(im,jm,1,1, ifirst,ilast, jfirst,jlast, 1,1, ng,ng, ng,ng, va(:,:))

! A -> C
        call atoc(ua, va, uc, vc, dx,dy,dxa,dya,im, jm, ng, nested, domain, noComm=.true.)

     enddo ! km loop

     if (.not. nested) call fill_corners(uc, vc, im, jm, VECTOR=.true., CGRID=.true.)
   endif


 end subroutine d2a2c

!>@brief The subroutine 'atob_s' interpolates a scalar from the A-Grid to the B-grid.
      subroutine atob_s(qin, qout, npx, npy, dxa, dya, nested, cubed_sphere, altInterp)
         integer,      intent(IN) :: npx, npy
         real  , intent(IN)    ::  qin(isd:ied  ,jsd:jed  )    !< A-grid field
         real  , intent(OUT)   :: qout(isd:ied+1,jsd:jed+1)    !< Output  B-grid field
         integer, OPTIONAL, intent(IN) :: altInterp 
         logical, intent(IN) :: nested, cubed_sphere
         real, intent(IN), dimension(isd:ied,jsd:jed)    :: dxa, dya

         integer :: i,j,n

         real :: tmp1j(jsd:jed+1)
         real :: tmp2j(jsd:jed+1)
         real :: tmp3j(jsd:jed+1)
         real :: tmp1i(isd:ied+1)
         real :: tmp2i(isd:ied+1)
         real :: tmp3i(isd:ied+1)
         real :: tmpq(isd:ied  ,jsd:jed  )
         real :: tmpq1(isd:ied+1,jsd:jed+1)
         real :: tmpq2(isd:ied+1,jsd:jed+1)

         if (present(altInterp)) then

         tmpq(:,:) = qin(:,:)

         if (.not. nested) call fill_corners(tmpq  , npx, npy, FILL=XDir, AGRID=.true.)
! ATOC
         do j=jsd,jed
            call interp_left_edge_1d(tmpq1(:,j), tmpq(:,j), dxa(:,j), isd, ied, altInterp) 
         enddo

         if (.not. nested) call fill_corners(tmpq  , npx, npy, FILL=YDir, AGRID=.true.)
! ATOD
         do i=isd,ied
            tmp1j(jsd:jed) = 0.0 
            tmp2j(jsd:jed) = tmpq(i,jsd:jed)
            tmp3j(jsd:jed) = dya(i,jsd:jed)
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed, altInterp)
            tmpq2(i,jsd:jed) = tmp1j(jsd:jed)
         enddo

! CTOB
         do i=isd,ied
            tmp1j(:) = tmpq1(i,:)
            tmp2j(:) = tmpq1(i,:)
            tmp3j(:) = 1.0  ! Uniform Weighting missing first value so will not reproduce
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed+1, altInterp) 
            tmpq1(i,:) = tmp1j(:)
         enddo

! DTOB
         do j=jsd,jed
            tmp1i(:) = tmpq2(:,j)
            tmp2i(:) = tmpq2(:,j)
            tmp3i(:) = 1.0  ! Uniform Weighting missing first value so will not reproduce
            call interp_left_edge_1d(tmp1i, tmp2i, tmp3i, isd, ied+1, altInterp)
            tmpq2(:,j) = tmp1i(:)
         enddo

! Average 
         do j=jsd,jed+1
            do i=isd,ied+1
               qout(i,j) = 0.5 * (tmpq1(i,j) + tmpq2(i,j))
            enddo
         enddo

! Fix Corners
         if (cubed_sphere  .and. .not. nested) then
            i=1
            j=1
            if ( (is==i) .and. (js==j) ) then
               qout(i,j) = (1./3.) * (qin(i,j) + qin(i-1,j) + qin(i,j-1))
            endif

            i=npx
            j=1
            if ( (ie+1==i) .and. (js==j) ) then
               qout(i,j) = (1./3.) * (qin(i-1,j) + qin(i-1,j-1) + qin(i,j))
            endif

            i=1
            j=npy
            if ( (is==i) .and. (je+1==j) ) then
               qout(i,j) = (1./3.) * (qin(i,j-1) + qin(i-1,j-1) + qin(i,j))
            endif

            i=npx
            j=npy
            if ( (ie+1==i) .and. (je+1==j) ) then
               qout(i,j) = (1./3.) * (qin(i-1,j-1) + qin(i,j-1) + qin(i-1,j))
            endif
        endif

        else ! altInterp

            do j=js,je+1
               do i=is,ie+1
                  qout(i,j) = 0.25 * (qin(i-1,j) + qin(i-1,j-1) + &
                                      qin(i  ,j) + qin(i  ,j-1))
               enddo
            enddo

            if (.not. nested) then
            i=1
            j=1
            if ( (is==i) .and. (js==j) ) then
               qout(i,j) = (1./3.) * (qin(i,j) + qin(i-1,j) + qin(i,j-1))
            endif

            i=npx
            j=1
            if ( (ie+1==i) .and. (js==j) ) then
               qout(i,j) = (1./3.) * (qin(i-1,j) + qin(i-1,j-1) + qin(i,j))
            endif

            i=1
            j=npy
            if ( (is==i) .and. (je+1==j) ) then
               qout(i,j) = (1./3.) * (qin(i,j-1) + qin(i-1,j-1) + qin(i,j))
            endif

            i=npx
            j=npy
            if ( (ie+1==i) .and. (je+1==j) ) then
               qout(i,j) = (1./3.) * (qin(i-1,j-1) + qin(i,j-1) + qin(i-1,j))
            endif
            endif !not nested

        endif ! altInterp

      end subroutine atob_s
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'atod' interpolates values from the A-Grid to the D-grid.
      subroutine atod(uin, vin, uout, vout, dxa, dya, dxc, dyc, npx, npy, ng, nested, domain)
         integer,      intent(IN) :: npx, npy, ng
         real  , intent(IN)    ::  uin(isd:ied  ,jsd:jed  ) !< A-grid u-wind field
         real  , intent(IN)    ::  vin(isd:ied  ,jsd:jed  ) !< A-grid v-wind field
         real  , intent(OUT)   :: uout(isd:ied  ,jsd:jed+1) !< D-grid u-wind field
         real  , intent(OUT)   :: vout(isd:ied+1,jsd:jed  ) !< D-grid v-wind field
         logical, intent(IN) :: nested
         real  , intent(IN), dimension(isd:ied,jsd:jed) :: dxa, dya
         real  , intent(IN), dimension(isd:ied+1,jsd:jed) :: dxc
         real  , intent(IN), dimension(isd:ied,jsd:jed+1) :: dyc
         type(domain2d), intent(INOUT) :: domain

         integer :: i,j
         real :: tmp1i(isd:ied+1)
         real :: tmp2i(isd:ied)
         real :: tmp3i(isd:ied)
         real :: tmp1j(jsd:jed+1)
         real :: tmp2j(jsd:jed)
         real :: tmp3j(jsd:jed)

         do j=jsd+1,jed
            tmp1i(:) = 0.0
            tmp2i(:) = vin(:,j)*dxa(:,j)
            tmp3i(:) = dxa(:,j)
            call interp_left_edge_1d(tmp1i, tmp2i, tmp3i, isd, ied, interpOrder)
            vout(:,j) = tmp1i(:)/dxc(:,j)
         enddo
         do i=isd+1,ied
            tmp1j(:) = 0.0
            tmp2j(:) = uin(i,:)*dya(i,:)
            tmp3j(:) = dya(i,:)
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed, interpOrder)
            uout(i,:) = tmp1j(:)/dyc(i,:)
         enddo
         call mp_update_dwinds(uout, vout, npx, npy, domain)
         if (.not. nested) call fill_corners(uout, vout, npx, npy, VECTOR=.true., DGRID=.true.)
      end subroutine atod
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'dtoa' interpolates values from the D-Grid to the A-grid.
      subroutine dtoa(uin, vin, uout, vout, dx, dy, dxa, dya, dxc, dyc, npx, npy, ng)
         integer,      intent(IN) :: npx, npy, ng
         real  , intent(IN)    ::  uin(isd:ied  ,jsd:jed+1)    !< D-grid u-wind field
         real  , intent(IN)    ::  vin(isd:ied+1,jsd:jed  )    !< D-grid v-wind field
         real  , intent(OUT)   :: uout(isd:ied  ,jsd:jed  )    !< A-grid u-wind field
         real  , intent(OUT)   :: vout(isd:ied  ,jsd:jed  )    !< A-grid v-wind field
         real  , intent(IN), dimension(isd:ied,jsd:jed+1) :: dx, dyc
         real  , intent(IN), dimension(isd:ied+1,jsd:jed) :: dy, dxc
         real  , intent(IN), dimension(isd:ied,jsd:jed) :: dxa, dya

         integer :: i,j,n

         real :: tmp1i(isd:ied+1)
         real :: tmp2i(isd:ied+1)
         real :: tmp3i(isd:ied+1)
         real :: tmp1j(jsd:jed+1)
         real :: tmp2j(jsd:jed+1)
         real :: tmp3j(jsd:jed+1)

!CLEANUP: replace dxa with rdxa, and dya with rdya; may change numbers.
#ifdef VORT_ON
! circulation (therefore, vort) conserving:
         do j=jsd,jed
            do i=isd,ied
                uout(i,j) = 0.5*(uin(i,j)*dx(i,j)+uin(i,j+1)*dx(i,j+1))/dxa(i,j)
                vout(i,j) = 0.5*(vin(i,j)*dy(i,j)+vin(i+1,j)*dy(i+1,j))/dya(i,j)
            enddo
         enddo
#else
         do i=isd,ied
            tmp1j(:) = 0.0
            tmp2j(:) = uin(i,:)*dyc(i,:)
            tmp3j(:) = dyc(i,:)
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed+1, interpOrder) 
            uout(i,jsd:jed) = tmp1j(jsd+1:jed+1)/dya(i,jsd:jed)
         enddo
         do j=jsd,jed
            tmp1i(:) = 0.0
            tmp2i(:) = vin(:,j)*dxc(:,j)
            tmp3i(:) = dxc(:,j)
            call interp_left_edge_1d(tmp1i, tmp2i, tmp3i, isd, ied+1, interpOrder) 
            vout(isd:ied,j) = tmp1i(isd+1:ied+1)/dxa(isd:ied,j)
         enddo
#endif

      end subroutine dtoa
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'atoc' interpolates values from the A-Grid to the C-grid.
      subroutine atoc(uin, vin, uout, vout, dx, dy, dxa, dya, npx, npy, ng, nested, domain, noComm)
         integer,      intent(IN) :: npx, npy, ng
         real  , intent(IN)    ::  uin(isd:ied  ,jsd:jed  ) !< A-grid u-wind field
         real  , intent(IN)    ::  vin(isd:ied  ,jsd:jed  ) !< A-grid v-wind field
         real  , intent(OUT)   :: uout(isd:ied+1,jsd:jed  ) !< C-grid u-wind field
         real  , intent(OUT)   :: vout(isd:ied  ,jsd:jed+1) !< C-grid v-wind field
         logical, intent(IN) :: nested
         logical, OPTIONAL, intent(IN)   :: noComm
         real  , intent(IN), dimension(isd:ied,jsd:jed+1) :: dx
         real  , intent(IN), dimension(isd:ied+1,jsd:jed) :: dy
         real  , intent(IN), dimension(isd:ied,jsd:jed) :: dxa, dya
         type(domain2d), intent(INOUT) :: domain

         real :: ang1
         integer :: i,j,n

         real :: tmp1i(isd:ied+1)
         real :: tmp2i(isd:ied)
         real :: tmp3i(isd:ied)
         real :: tmp1j(jsd:jed+1)
         real :: tmp2j(jsd:jed)
         real :: tmp3j(jsd:jed)

#if !defined(ALT_INTERP)
#ifdef VORT_ON
! Circulation conserving
         do j=jsd,jed
            do i=isd+1,ied
               uout(i,j) = ( uin(i,j)*dxa(i,j) + uin(i-1,j)*dxa(i-1,j) )    &
                           /        ( dxa(i,j) +            dxa(i-1,j) )
            enddo
         enddo
         do j=jsd+1,jed
            do i=isd,ied
               vout(i,j) = ( vin(i,j)*dya(i,j) + vin(i,j-1)*dya(i,j-1) )    &
                           /        ( dya(i,j) +            dya(i,j-1) )
            enddo
         enddo
#else
         do j=jsd,jed
            call interp_left_edge_1d(uout(:,j), uin(:,j), dxa(:,j), isd, ied, interpOrder)
         enddo
         do i=isd,ied
!!$            tmp1j(:) = vout(i,:)
            tmp2j(:) = vin(i,:)
            tmp3j(:) = dya(i,:)
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed, interpOrder)
            vout(i,:) = tmp1j(:)
         enddo 
#endif
#else

         do j=jsd,jed
!!$            tmp1i(:) = uout(:,j)
            tmp2i(:) = uin(:,j)*dya(:,j)
            tmp3i(:) = dxa(:,j)
            call interp_left_edge_1d(tmp1i, tmp2i, tmp3i, isd, ied, interpOrder)
            uout(:,j) = tmp1i(:)/dy(:,j)
         enddo
         do i=isd,ied
!!$            tmp1j(:) = vout(i,:)
            tmp2j(:) = vin(i,:)*dxa(i,:)
            tmp3j(:) = dya(i,:)
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed, interpOrder)
            vout(i,:) = tmp1j(:)/dx(i,:)
         enddo

       if (cubed_sphere .and. .not. nested) then
         csFac = COS(30.0*PI/180.0)
      ! apply Corner scale factor for interp on Cubed-Sphere
         if ( (is==1) .and. (js==1) ) then
            i=1
            j=1
            uout(i,j)=uout(i,j)*csFac
            uout(i,j-1)=uout(i,j-1)*csFac
            vout(i,j)=vout(i,j)*csFac
            vout(i-1,j)=vout(i-1,j)*csFac
         endif
         if ( (is==1) .and. (je==npy-1) ) then
            i=1
            j=npy-1
            uout(i,j)=uout(i,j)*csFac
            uout(i,j+1)=uout(i,j+1)*csFac
            vout(i,j+1)=vout(i,j+1)*csFac
            vout(i-1,j+1)=vout(i-1,j+1)*csFac
         endif
         if ( (ie==npx-1) .and. (je==npy-1) ) then
            i=npx-1
            j=npy-1
            uout(i+1,j)=uout(i+1,j)*csFac
            uout(i+1,j+1)=uout(i+1,j+1)*csFac
            vout(i,j+1)=vout(i,j+1)*csFac
            vout(i+1,j+1)=vout(i+1,j+1)*csFac
         endif
         if ( (ie==npx-1) .and. (js==1) ) then
            i=npx-1
            j=1
            uout(i+1,j)=uout(i+1,j)*csFac
            uout(i+1,j-1)=uout(i+1,j-1)*csFac
            vout(i,j)=vout(i,j)*csFac
            vout(i+1,j)=vout(i+1,j)*csFac
         endif
       endif

#endif

         if (present(noComm)) then
            if (.not. noComm) call mpp_update_domains( uout,vout, domain, gridtype=CGRID_NE_PARAM, complete=.true.)
         else
            call mpp_update_domains( uout,vout, domain, gridtype=CGRID_NE_PARAM, complete=.true.)
         endif
         if (.not. nested) call fill_corners(uout, vout, npx, npy, VECTOR=.true., CGRID=.true.)

      end subroutine atoc
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'ctoa' interpolates values from the C-Grid to the A-grid.
      subroutine ctoa(uin, vin, uout, vout, dx, dy, dxc, dyc, dxa, dya, npx, npy, ng)
         integer,      intent(IN) :: npx, npy, ng 
         real  , intent(IN)    ::  uin(isd:ied+1,jsd:jed  )    !< C-grid u-wind field
         real  , intent(IN)    ::  vin(isd:ied  ,jsd:jed+1)    !< C-grid v-wind field
         real  , intent(OUT)   :: uout(isd:ied  ,jsd:jed  )    !< A-grid u-wind field
         real  , intent(OUT)   :: vout(isd:ied  ,jsd:jed  )    !< A-grid v-wind field
         real  , intent(IN), dimension(isd:ied+1,jsd:jed) :: dxc, dy
         real  , intent(IN), dimension(isd:ied,jsd:jed+1) :: dyc, dx
         real  , intent(IN), dimension(isd:ied,jsd:jed) :: dxa, dya

         integer :: i,j

         real :: tmp1i(isd:ied+1)
         real :: tmp2i(isd:ied+1)
         real :: tmp3i(isd:ied+1)
         real :: tmp1j(jsd:jed+1)
         real :: tmp2j(jsd:jed+1)
         real :: tmp3j(jsd:jed+1)

        ! do j=jsd,jed
        !    do i=isd,ied
        !       uout(i,j) = 0.5 * (uin(i,j)*dy(i,j) + uin(i+1,j)*dy(i+1,j))/dya(i,j)
        !    enddo
        !  enddo
        ! do j=jsd,jed
        !    do i=isd,ied
        !       vout(i,j) = 0.5 * (vin(i,j)*dx(i,j) + vin(i,j+1)*dx(i,j+1))/dxa(i,j)
        !    enddo
        ! enddo
         do i=isd,ied
            tmp1j(:) = 0.0
            tmp2j(:) = vin(i,:)*dx(i,:)
            tmp3j(:) = dyc(i,:)
            call interp_left_edge_1d(tmp1j, tmp2j, tmp3j, jsd, jed+1, interpOrder)
            vout(i,jsd:jed) = tmp1j(jsd+1:jed+1)/dxa(i,jsd:jed)
         enddo
         do j=jsd,jed
            tmp1i(:) = 0.0
            tmp2i(:) = uin(:,j)*dy(:,j)
            tmp3i(:) = dxc(:,j)
            call interp_left_edge_1d(tmp1i, tmp2i, tmp3i, isd, ied+1, interpOrder)
            uout(isd:ied,j) = tmp1i(isd+1:ied+1)/dya(isd:ied,j)
         enddo

      end subroutine ctoa
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'rotate_winds' rotates winds from the sphere-to-cube to cube-to-sphere.
      subroutine rotate_winds(myU, myV, p1, p2, p3, p4, t1, ndims, dir)
         integer,      intent(IN) :: ndims
         real  , intent(INOUT) :: myU    !< u-wind field
         real  , intent(INOUT) :: myV    !< v-wind field
         real(kind=R_GRID)  , intent(IN)    :: p1(ndims)    !             p4     
         real(kind=R_GRID)  , intent(IN)    :: p2(ndims)    !                    
         real(kind=R_GRID)  , intent(IN)    :: p3(ndims)    !        p1   t1   p3
         real(kind=R_GRID)  , intent(IN)    :: p4(ndims)    !                    
         real(kind=R_GRID)  , intent(IN)    :: t1(ndims)    !             p2     
         integer,   intent(IN)    :: dir   !< Direction ; 1=>sphere-to-cube  2=> cube-to-sphere

         real(kind=R_GRID) :: ee1(3), ee2(3), ee3(3), elon(3), elat(3)

         real :: g11, g12, g21, g22

         real :: newu, newv

         call get_unit_vector(p3, t1, p1, ee1)
         call get_unit_vector(p4, t1, p2, ee2)
         elon(1) = -SIN(t1(1) - pi)
         elon(2) =  COS(t1(1) - pi)
         elon(3) = 0.0
         elat(1) = -SIN(t1(2))*COS(t1(1) - pi)
         elat(2) = -SIN(t1(2))*SIN(t1(1) - pi)
         elat(3) =  COS(t1(2))

         g11 = inner_prod(ee1,elon)
         g12 = inner_prod(ee1,elat)
         g21 = inner_prod(ee2,elon)
         g22 = inner_prod(ee2,elat)

         if (dir == 1) then    ! Sphere to Cube Rotation
            newu = myU*g11 + myV*g12
            newv = myU*g21 + myV*g22
         else
            newu = ( myU*g22 - myV*g12)/(g11*g22 - g21*g12) 
            newv = (-myU*g21 + myV*g11)/(g11*g22 - g21*g12)
         endif
         myU = newu
         myV = newv

      end subroutine rotate_winds

      subroutine mp_update_dwinds_2d(u, v, npx, npy, domain)
        use mpp_parameter_mod, only: DGRID_NE
         real  , intent(INOUT)   :: u(isd:ied  ,jsd:jed+1) !< D-grid u-wind field
         real  , intent(INOUT)   :: v(isd:ied+1,jsd:jed  ) !< D-grid v-wind field
         integer,      intent(IN) :: npx, npy
         type(domain2d), intent(INOUT) :: domain

         call mpp_update_domains( u, v, domain, gridtype=DGRID_NE, complete=.true.)
!        if (.not. nested) call fill_corners(u , v , npx, npy, VECTOR=.true., DGRID=.true.)

      end subroutine mp_update_dwinds_2d
!
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!
      subroutine mp_update_dwinds_3d(u, v, npx, npy, npz, domain)
        use mpp_parameter_mod, only: DGRID_NE
         real  , intent(INOUT)   :: u(isd:ied  ,jsd:jed+1,npz) !< D-grid u-wind field
         real  , intent(INOUT)   :: v(isd:ied+1,jsd:jed  ,npz) !< D-grid v-wind field
         integer,      intent(IN) :: npx, npy, npz
         type(domain2d), intent(INOUT) :: domain
         integer k

      call mpp_update_domains( u, v, domain, gridtype=DGRID_NE, complete=.true.)
!     do k=1,npz
!        if (.not. nested) call fill_corners(u(isd:,jsd:,k) , v(isd:,jsd:,k) , npx, npy, VECTOR=.true., DGRID=.true.)
!     enddo

      end subroutine mp_update_dwinds_3d

!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'gsum' computes the global sum.
      real  function globalsum(p, npx, npy, ifirst, ilast, jfirst, jlast, isd, ied, jsd, jed, gridstruct, tile) result (gsum)
         integer,   intent(IN)    :: npx, npy
         integer,   intent(IN)    :: ifirst, ilast
         integer,   intent(IN)    :: jfirst, jlast
         integer,   intent(IN)    :: isd, ied
         integer,   intent(IN)    :: jsd, jed, tile
         real  , intent(IN)    :: p(ifirst:ilast,jfirst:jlast)      !< field to be summed
         type(fv_grid_type), intent(IN), target :: gridstruct

         integer :: i,j,k,n
         integer :: j1, j2
         real  :: gsum0
         real, allocatable :: p_R8(:,:,:)

         real, pointer, dimension(:,:,:)   :: agrid, grid
         real, pointer, dimension(:,:)     :: area, rarea, fC, f0
         real, pointer, dimension(:,:)     :: dx,dy, dxa,dya, rdxa, rdya, dxc,dyc

         logical, pointer :: cubed_sphere, latlon

         logical, pointer :: have_south_pole, have_north_pole

         integer, pointer :: ntiles_g
         real,    pointer :: acapN, acapS, globalarea

         grid => gridstruct%grid
         agrid=> gridstruct%agrid

         area  => gridstruct%area
         rarea => gridstruct%rarea

         fC    => gridstruct%fC
         f0    => gridstruct%f0

         dx      => gridstruct%dx
         dy      => gridstruct%dy
         dxa     => gridstruct%dxa
         dya     => gridstruct%dya
         rdxa    => gridstruct%rdxa
         rdya    => gridstruct%rdya
         dxc     => gridstruct%dxc
         dyc     => gridstruct%dyc

         cubed_sphere => gridstruct%cubed_sphere
         latlon       => gridstruct%latlon

         have_south_pole               => gridstruct%have_south_pole
         have_north_pole               => gridstruct%have_north_pole

         ntiles_g                      => gridstruct%ntiles_g
         acapN                         => gridstruct%acapN
         acapS                         => gridstruct%acapS
         globalarea                    => gridstruct%globalarea

         allocate(p_r8(npx-1,npy-1,ntiles_g))
         gsum = 0.
            
         if (latlon) then          
            j1 = 2                          
            j2 = npy-2
            !!! WARNING: acapS and acapN have NOT been initialized.
            gsum = gsum + p(1,1)*acapS
            gsum = gsum + p(1,npy-1)*acapN
            do j=j1,j2
               do i=1,npx-1
                  gsum = gsum + p(i,j)*cos(agrid(i,j,2))
               enddo
            enddo
         else

            do n=tile,tile            
               do j=jfirst,jlast
                  do i=ifirst,ilast
                     p_R8(i,j,n) = p(i,j)*area(i,j)
                  enddo
               enddo
            enddo
            call mp_gather(p_R8, ifirst,ilast, jfirst,jlast, npx-1, npy-1, ntiles_g)
            if (is_master()) then
               do n=1,ntiles_g
                  do j=1,npy-1
                     do i=1,npx-1
                        gsum = gsum + p_R8(i,j,n)
                     enddo
                  enddo
               enddo
               gsum = gsum/globalarea
            endif
            call mpp_broadcast(gsum, mpp_root_pe())

         endif

         deallocate(p_r8)
         
      end function globalsum


 subroutine get_unit_vector( p1, p2, p3, uvect )
 real(kind=R_GRID), intent(in):: p1(2), p2(2), p3(2) ! input position unit vectors (spherical coordinates)
 real(kind=R_GRID), intent(out):: uvect(3)           ! output unit spherical cartesian
! local
 integer :: n 
 real(kind=R_GRID) :: xyz1(3), xyz2(3), xyz3(3)
 real :: dp(3) 

  call spherical_to_cartesian(p1(1), p1(2), one, xyz1(1), xyz1(2), xyz1(3))
  call spherical_to_cartesian(p2(1), p2(2), one, xyz2(1), xyz2(2), xyz2(3))
  call spherical_to_cartesian(p3(1), p3(2), one, xyz3(1), xyz3(2), xyz3(3))
  do n=1,3
     uvect(n) = xyz3(n)-xyz1(n)
  enddo
  call project_sphere_v(1, uvect,xyz2)
  call normalize_vect(1, uvect)

 end subroutine get_unit_vector


 subroutine normalize_vect(np, e)
!
! Make e an unit vector
!
 implicit none
 integer, intent(in):: np
 real(kind=R_GRID), intent(inout):: e(3,np)
! local:
 integer k, n
 real pdot

 do n=1,np
    pdot = sqrt(e(1,n)**2+e(2,n)**2+e(3,n)**2)
    do k=1,3
       e(k,n) = e(k,n) / pdot
    enddo
 enddo

 end subroutine normalize_vect
!------------------------------------------------------------------------------
!BOP
! !ROUTINE: mp_ghost_ew --- Ghost 4d east/west "lat/lon periodic
!
! !INTERFACE:
      subroutine mp_ghost_ew(im, jm, km, nq, ifirst, ilast, jfirst, jlast, &
                              kfirst, klast, ng_w, ng_e, ng_s, ng_n, q_ghst, q)
!
! !INPUT PARAMETERS:
      integer, intent(in):: im, jm, km, nq
      integer, intent(in):: ifirst, ilast
      integer, intent(in):: jfirst, jlast
      integer, intent(in):: kfirst, klast
      integer, intent(in):: ng_e      ! eastern  zones to ghost
      integer, intent(in):: ng_w      ! western  zones to ghost
      integer, intent(in):: ng_s      ! southern zones to ghost
      integer, intent(in):: ng_n      ! northern zones to ghost
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

!EOC
      end subroutine mp_ghost_ew






!-------------------------------------------------------------------------------
! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
!>@brief The subroutine 'interp_left_edge_1d' interpolates to left edge of a cell.
!>@details order = 1 -> Linear average
!>order = 2 -> Uniform PPM
!>order = 3 -> Non-Uniform PPM  
 subroutine interp_left_edge_1d(qout, qin, dx, ifirst, ilast, order)
 integer, intent(in):: ifirst,ilast
 real, intent(out)  :: qout(ifirst:)
 real, intent(in)   ::  qin(ifirst:)
 real, intent(in)   ::   dx(ifirst:)
 integer, intent(in):: order
 integer :: i

 real :: dm(ifirst:ilast),qmax,qmin
 real :: r3, da1, da2, a6da, a6, al, ar  
 real :: qLa, qLb1, qLb2
 real :: x

 r3 = 1./3.

 qout(:) = 0.0 
 if (order==1) then 
! 1st order Uniform linear averaging
    do i=ifirst+1,ilast
       qout(i) = 0.5 * (qin(i-1) + qin(i))
    enddo
 elseif (order==2) then
! Non-Uniform 1st order average 
    do i=ifirst+1,ilast
       qout(i) = (dx(i-1)*qin(i-1) + dx(i)*qin(i))/(dx(i-1)+dx(i))
    enddo
 elseif (order==3) then 

! PPM - Uniform 
    do i=ifirst+1,ilast-1
       dm(i) = 0.25*(qin(i+1) - qin(i-1))
    enddo
!
! Applies monotonic slope constraint
!
     do i=ifirst+1,ilast-1
        qmax = max(qin(i-1),qin(i),qin(i+1)) - qin(i)
        qmin = qin(i) - min(qin(i-1),qin(i),qin(i+1))
        dm(i) = sign(min(abs(dm(i)),qmin,qmax),dm(i))
     enddo

     do i=ifirst+1,ilast-1
         qout(i) = 0.5*(qin(i-1)+qin(i)) + r3*(dm(i-1) - dm(i))
       ! al = 0.5*(qin(i-1)+qin(i)) + r3*(dm(i-1) - dm(i))
       ! da1 = dm(i) + dm(i)
       ! qout(i) = qin(i) - sign(min(abs(da1),abs(al-qin(i))), da1)
     enddo

! First order average to fill in end points
     qout(ifirst+1) = 0.5 * (qin(ifirst) + qin(ifirst+1))
     qout(ilast) = 0.5 * (qin(ilast-1) + qin(ilast))

 elseif (order==4) then

  ! Non-Uniform PPM
     do i=ifirst+1,ilast-1
        dm(i) = ( (2.*dx(i-1) + dx(i) ) /                         &
                  (   dx(i+1) + dx(i) )  )  * ( qin(i+1) - qin(i) ) + &
                ( (dx(i)   + 2.*dx(i+1)) /                        &
                  (dx(i-1) +    dx(i)  )  ) * ( qin(i) - qin(i-1) )
        dm(i) = ( dx(i) / ( dx(i-1) + dx(i) + dx(i+1) ) ) * dm(i)
        if ( (qin(i+1)-qin(i))*(qin(i)-qin(i-1)) > 0.) then
           dm(i) = SIGN( MIN( ABS(dm(i)), 2.*ABS(qin(i)-qin(i-1)), 2.*ABS(qin(i+1)-qin(i)) ) , dm(i) )
        else
           dm(i) = 0.
        endif
     enddo

     do i=ifirst+2,ilast-1
        qLa = ( (dx(i-2) + dx(i-1)) / (2.*dx(i-1) +  dx(i)) ) - &
              ( (dx(i+1) + dx(i)) / (2.*dx(i) +  dx(i-1)) )
        qLa = ( (2.*dx(i) * dx(i-1))  / (dx(i-1) + dx(i)) ) * qLa * &
                (qin(i) - qin(i-1))
        qLb1 = dx(i-1) * ( (dx(i-2) + dx(i-1)) / (2.*dx(i-1) + dx(i)) ) * &
              dm(i)
        qLb2 = dx(i) * ( (dx(i) + dx(i+1)) / (dx(i-1) + 2.*dx(i)) ) * &
              dm(i-1)

        qout(i) = 1. / ( dx(i-2) + dx(i-1) + dx(i) + dx(i+1) )
        qout(i) = qout(i) * ( qLa - qLb1 + qLb2 )
        qout(i) = qin(i-1) + ( dx(i-1) / ( dx(i-1) + dx(i) ) ) * (qin(i) - qin(i-1)) + qout(i)
     enddo

 elseif (order==5) then
  
     ! Linear Spline
    do i=ifirst+1,ilast-1
       x = FLOAT(i-(ifirst+1))*FLOAT(ilast-ifirst+1-1)/FLOAT(ilast-ifirst-1) 
       qout(i) = qin(ifirst+NINT(x)) + (x - NINT(x)) * (qin(ifirst+NINT(x+1)) - qin(ifirst+NINT(x)))
      ! if (tile==1) print*, ifirst+NINT(x+1), ifirst+NINT(x), (x - NINT(x)) 
      ! if (tile==1) print*, 0.5*(qin(i-1)+qin(i)), qout(i)
    enddo

!!$   if (tile==1) print*,'x=fltarr(28)'
!!$    do i=ifirst,ilast
!!$       if (tile==1) print*, 'x(',i-ifirst,')=',qin(i)
!!$    enddo


	call mp_stop
	stop

 endif

 end subroutine interp_left_edge_1d
!------------------------------------------------------------------------------
!----------------------------------------------------------------------- 
!>@brief The subroutine 'vpol5' treats the V winds at the poles.  
!>@details This requires an average of the U- and V-winds, 
!!weighted by their angles of incidence at the pole points.     
 subroutine vpol5(u, v, im, jm, coslon, sinlon, cosl5, sinl5,    &
                  ng_d,  ng_s,  jfirst, jlast)
! !INPUT PARAMETERS:
      integer im                       !< Total longitudes
      integer jm                       !< Total latitudes
      integer jfirst                   !< First PE latitude (no ghosting)
      integer jlast                    !< Last  PE latitude (no ghosting)
      integer, intent(in):: ng_s, ng_d
      real, intent(in):: coslon(im,jm), sinlon(im,jm)
      real, intent(in):: cosl5(im,jm),sinl5(im,jm)
      real, intent(in):: u(im,jfirst-ng_d:jlast+ng_s)

! !INPUT/OUTPUT PARAMETERS:
      real, intent(inout):: v(im,jfirst-ng_d:jlast+ng_d)

! !LOCAL VARIABLES:

      integer i, imh
      real  uanp(im), uasp(im), vanp(im), vasp(im)
      real  un, vn, us, vs, r2im

! WS 99.05.25 :  Replaced conversions of IMR with IM
      r2im = 0.5d0/dble(im)
      imh  = im / 2

! WS 990726 :  Added condition to decide if poles are on this processor

   if ( jfirst-ng_d <= 1 ) then
         do i=1,im
            uasp(i) = u(i,  2) + u(i,3)
         enddo

         do i=1,im-1
            vasp(i)  = v(i,  2) + v(i+1,2)
         enddo
            vasp(im) = v(im,2) + v(1,2)

! Projection at SP
      us = 0.; vs = 0.

      do i=1,imh
         us = us + (uasp(i+imh)-uasp(i))*sinlon(i,1)    &
                 + (vasp(i)-vasp(i+imh))*coslon(i,1)
         vs = vs + (uasp(i+imh)-uasp(i))*coslon(i,1)    &
                 + (vasp(i+imh)-vasp(i))*sinlon(i,1)
      enddo
      us = us*r2im
      vs = vs*r2im

! get V-wind at SP

      do i=1,imh
         v(i,    1) =  us*cosl5(i,1) - vs*sinl5(i,1)
         v(i+imh,1) = -v(i,1)
      enddo

   endif

   if ( jlast+ng_d >= jm ) then

      do i=1,im
         uanp(i) = u(i,jm-1) + u(i,jm)
      enddo

      do i=1,im-1
         vanp(i) = v(i,jm-1) + v(i+1,jm-1)
      enddo
         vanp(im) = v(im,jm-1) + v(1,jm-1)

! Projection at NP

      un = 0.
      vn = 0.
      do i=1,imh
         un = un + (uanp(i+imh)-uanp(i))*sinlon(i,jm)   &
                 + (vanp(i+imh)-vanp(i))*coslon(i,jm)
         vn = vn + (uanp(i)-uanp(i+imh))*coslon(i,jm)   &
                 + (vanp(i+imh)-vanp(i))*sinlon(i,jm)
      enddo
      un = un*r2im
      vn = vn*r2im

! get V-wind at NP

      do i=1,imh
         v(i,    jm) = -un*cosl5(i,jm) - vn*sinl5(i,jm)
         v(i+imh,jm) = -v(i,jm)
      enddo

   endif

 end subroutine vpol5

 subroutine prt_m1(qname, q, is, ie, js, je, n_g, km, fac)
! Single PE version
      character(len=*), intent(in)::  qname
      integer, intent(in):: is, ie, js, je
      integer, intent(in):: n_g, km
      real, intent(in)::    q(is-n_g:ie+n_g, js-n_g:je+n_g, km)
      real, intent(in)::    fac

      real qmin, qmax
      integer i,j,k

      qmin = q(is,js,1)
      qmax = qmin

      do k=1,km
      do j=js,je
         do i=is,ie
            if( q(i,j,k) < qmin ) then
                qmin = q(i,j,k)
            elseif( q(i,j,k) > qmax ) then
                qmax = q(i,j,k)
            endif
          enddo
      enddo
      enddo

      write(*,*) qname, ' max = ', qmax*fac, ' min = ', qmin*fac

 end subroutine prt_m1

 subroutine var_dz(km, ztop, ze)
  integer, intent(in):: km
  real,    intent(in):: ztop
  real,    intent(out), dimension(km+1):: ze
! Local
  real, dimension(km):: dz, s_fac
  real dz0, sum1
  integer  k

      s_fac(km  ) = 0.25
      s_fac(km-1) = 0.30
      s_fac(km-2) = 0.50
      s_fac(km-3) = 0.70 
      s_fac(km-4) = 0.90
      s_fac(km-5) = 1.
      do k=km-6, 5, -1
         s_fac(k) = 1.05 * s_fac(k+1)
      enddo
      s_fac(4) = 1.1*s_fac(5)
      s_fac(3) = 1.2*s_fac(4)
      s_fac(2) = 1.3*s_fac(3)
      s_fac(1) = 1.5*s_fac(2)

      sum1 = 0.
      do k=1,km
         sum1 = sum1 + s_fac(k)
      enddo

      dz0 = ztop / sum1

      do k=1,km
         dz(k) = s_fac(k) * dz0
      enddo

      ze(km+1) = 0.
      do k=km,1,-1
         ze(k) = ze(k+1) + dz(k)
      enddo

! Re-scale dz with the stretched ztop
      do k=1,km
         dz(k) = dz(k) * (ztop/ze(1))
      enddo

      do k=km,1,-1
         ze(k) = ze(k+1) + dz(k)
      enddo
      ze(1) = ztop

      call sm1_edge(1, 1, 1, 1, km, 1, 1, ze, 1)

      if ( is_master() ) then
           write(*,*) 'var_dz: model top (km)=', ztop*0.001
           do k=km,1,-1
              dz(k) = ze(k) - ze(k+1)
              write(*,*) k, 0.5*(ze(k)+ze(k+1)), 'dz=', dz(k)
           enddo
       endif

 end subroutine var_dz

 subroutine sm1_edge(is, ie, js, je, km, i, j, ze, ntimes)
  integer, intent(in):: is, ie, js, je, km
  integer, intent(in):: ntimes, i, j
  real, intent(inout):: ze(is:ie,js:je,km+1)
! local:
  real, parameter:: df = 0.25
  real dz(km)
  real flux(km+1)
  integer k, n, k1, k2

      k2 = km-1
      do k=1,km
         dz(k) = ze(i,j,k+1) - ze(i,j,k)
      enddo

   do n=1,ntimes
      k1 = 2 + (ntimes-n)

      flux(k1  ) = 0.
      flux(k2+1) = 0.
      do k=k1+1,k2
         flux(k) = df*(dz(k) - dz(k-1))
      enddo

      do k=k1,k2
         dz(k) = dz(k) - flux(k) + flux(k+1)
      enddo
   enddo

   do k=km,1,-1
      ze(i,j,k) = ze(i,j,k+1) - dz(k)
   enddo

 end subroutine sm1_edge



end module test_cases_mod
