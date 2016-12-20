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
#ifdef OVERLOAD_R4
#define _GET_VAR1 get_var1_real 
#else
#define _GET_VAR1 get_var1_double
#endif

module external_ic_mod

   use external_sst_mod,   only: i_sst, j_sst, sst_ncep
   use fms_mod,            only: file_exist, read_data, field_exist, write_version_number
   use fms_mod,            only: open_namelist_file, check_nml_error, close_file
   use fms_mod,            only: get_mosaic_tile_file, read_data, error_mesg
   use fms_io_mod,         only: get_tile_string, field_size, free_restart_type
   use fms_io_mod,         only: restart_file_type, register_restart_field
   use fms_io_mod,         only: save_restart, restore_state, set_filename_appendix
   use mpp_mod,            only: mpp_error, FATAL, NOTE, mpp_pe, mpp_root_pe
   use mpp_mod,            only: stdlog, input_nml_file
   use mpp_parameter_mod,  only: AGRID_PARAM=>AGRID
   use mpp_domains_mod,    only: mpp_get_tile_id, domain2d, mpp_update_domains, NORTH, EAST
   use tracer_manager_mod, only: get_tracer_names, get_number_tracers, get_tracer_index
   use tracer_manager_mod, only: set_tracer_profile
   use field_manager_mod,  only: MODEL_ATMOS

   use constants_mod,     only: pi=>pi_8, omega, grav, kappa, rdgas, rvgas, cp_air
   use fv_arrays_mod,     only: fv_atmos_type, fv_grid_type, fv_grid_bounds_type, R_GRID
   use fv_diagnostics_mod,only: prt_maxmin
   use fv_grid_utils_mod, only: ptop_min, g_sum,mid_pt_sphere,get_unit_vect2,get_latlon_vector,inner_prod
   use fv_io_mod,         only: fv_io_read_tracers 
   use fv_mapz_mod,       only: mappm
   use fv_mp_mod,         only: ng, is_master, fill_corners, YDir, mp_reduce_min, mp_reduce_max
   use fv_surf_map_mod,   only: surfdrv
   use fv_surf_map_mod,   only: sgh_g, oro_g
   use fv_surf_map_mod,   only: del2_cubed_sphere, del4_cubed_sphere
   use fv_timing_mod,     only: timing_on, timing_off
   use init_hydro_mod,    only: p_var
   use fv_fill_mod,       only: fillz
   use fv_eta_mod,        only: set_eta
   use sim_nc_mod,        only: open_ncfile, close_ncfile, get_ncdim1, get_var1_double, get_var2_real,   &
                                get_var3_r4, get_var1_real
   use fv_nwp_nudge_mod,  only: T_is_Tv
   use test_cases_mod,    only: checker_tracers
! The "T" field in NCEP analysis is actually virtual temperature (Larry H. post processing)
! BEFORE 20051201

   use boundary_mod,      only: nested_grid_BC
   use mpp_domains_mod,       only: mpp_get_data_domain, mpp_get_global_domain, mpp_get_compute_domain

   implicit none
   private

   real, parameter:: zvir = rvgas/rdgas - 1.
   real(kind=R_GRID), parameter :: cnst_0p20=0.20d0
   real :: deg2rad

   public get_external_ic, get_cubed_sphere_terrain

!---- version number -----
   character(len=128) :: version = '$Id$'
   character(len=128) :: tagname = '$Name$'

contains

   subroutine get_external_ic( Atm, fv_domain, cold_start )

      type(fv_atmos_type), intent(inout), target :: Atm(:)
      type(domain2d),      intent(inout) :: fv_domain
      logical, intent(IN) :: cold_start
      real:: alpha = 0.
      real rdg
      integer i,j,k,nq

      real, pointer, dimension(:,:,:) :: grid, agrid
      real, pointer, dimension(:,:) :: fC, f0

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed
      integer :: sphum, liq_wat, ice_wat, o3mr

      is  = Atm(1)%bd%is
      ie  = Atm(1)%bd%ie
      js  = Atm(1)%bd%js
      je  = Atm(1)%bd%je
      isd = Atm(1)%bd%isd
      ied = Atm(1)%bd%ied
      jsd = Atm(1)%bd%jsd
      jed = Atm(1)%bd%jed

      grid  => Atm(1)%gridstruct%grid
      agrid => Atm(1)%gridstruct%agrid

      fC    => Atm(1)%gridstruct%fC
      f0    => Atm(1)%gridstruct%f0

! * Initialize coriolis param:
 
      do j=jsd,jed+1
         do i=isd,ied+1
            fc(i,j) = 2.*omega*( -1.*cos(grid(i,j,1))*cos(grid(i,j,2))*sin(alpha) + &
                                     sin(grid(i,j,2))*cos(alpha) )
         enddo
      enddo

      do j=jsd,jed
         do i=isd,ied
            f0(i,j) = 2.*omega*( -1.*cos(agrid(i,j,1))*cos(agrid(i,j,2))*sin(alpha) + &
                                     sin(agrid(i,j,2))*cos(alpha) )
         enddo
      enddo

      call mpp_update_domains( f0, fv_domain )
      if ( Atm(1)%gridstruct%cubed_sphere .and. .not. Atm(1)%neststruct%nested) call fill_corners(f0, Atm(1)%npx, Atm(1)%npy, YDir)
 
! Read in cubed_sphere terrain
      if ( Atm(1)%flagstruct%mountain ) then
           call get_cubed_sphere_terrain(Atm, fv_domain)
      else
         if (.not. Atm(1)%neststruct%nested) Atm(1)%phis = 0.
      endif
 
! Read in the specified external dataset and do all the needed transformation
      if ( Atm(1)%flagstruct%ncep_ic ) then
           nq = 1
                             call timing_on('NCEP_IC')
           call get_ncep_ic( Atm, fv_domain, nq )
                             call timing_off('NCEP_IC')
#ifndef NO_FV_TRACERS
           if (.not. cold_start) then
              call fv_io_read_tracers( fv_domain, Atm )
              if(is_master()) write(*,*) 'All tracers except sphum replaced by FV IC'
           endif
#endif
      elseif ( Atm(1)%flagstruct%nggps_ic ) then
                             call timing_on('NGGPS_IC')
           call get_nggps_ic( Atm, fv_domain )
                             call timing_off('NGGPS_IC')
      elseif ( Atm(1)%flagstruct%fv_diag_ic ) then
! Interpolate/remap diagnostic output from a FV model diagnostic output file on uniform lat-lon A grid:
               nq = size(Atm(1)%q,4)
! Needed variables: lon, lat, pfull(dim), zsurf, ps, ucomp, vcomp, w, temp, and all q
! delz not implemnetd yet; set make_nh = .true.
               call get_diag_ic( Atm, fv_domain, nq )
      else
! The following is to read in legacy lat-lon FV core restart file
!  is Atm%q defined in all cases?

           nq = size(Atm(1)%q,4)
           call get_fv_ic( Atm, fv_domain, nq )
      endif

      call prt_maxmin('PS', Atm(1)%ps, is, ie, js, je, ng, 1, 0.01)
      call prt_maxmin('TS', Atm(1)%ts, is, ie, js, je, 0, 1, 1.)
      call prt_maxmin('T', Atm(1)%pt, is, ie, js, je, ng, Atm(1)%npz, 1.)
      if (.not.Atm(1)%flagstruct%hydrostatic) call prt_maxmin('W', Atm(1)%w, is, ie, js, je, ng, Atm(1)%npz, 1.)
      call prt_maxmin('SPHUM', Atm(1)%q(:,:,:,1), is, ie, js, je, ng, Atm(1)%npz, 1.)
      if ( Atm(1)%flagstruct%nggps_ic ) then
        sphum   = get_tracer_index(MODEL_ATMOS, 'sphum')
        o3mr    = get_tracer_index(MODEL_ATMOS, 'o3mr')
        liq_wat   = get_tracer_index(MODEL_ATMOS, 'liq_wat')
        ice_wat   = get_tracer_index(MODEL_ATMOS, 'ice_wat')
        if ( liq_wat > 0 ) &
        call prt_maxmin('liq_wat', Atm(1)%q(:,:,:,liq_wat), is, ie, js, je, ng, Atm(1)%npz, 1.)
        if ( ice_wat > 0 ) &
        call prt_maxmin('ice_wat', Atm(1)%q(:,:,:,ice_wat), is, ie, js, je, ng, Atm(1)%npz, 1.)
        if ( o3mr > 0 ) &
        call prt_maxmin('O3MR', Atm(1)%q(:,:,:,o3mr), is, ie, js, je, ng, Atm(1)%npz, 1.)
      endif

      call p_var(Atm(1)%npz,  is, ie, js, je, Atm(1)%ak(1),  ptop_min,         &
                 Atm(1)%delp, Atm(1)%delz, Atm(1)%pt, Atm(1)%ps,               &
                 Atm(1)%pe,   Atm(1)%peln, Atm(1)%pk, Atm(1)%pkz,              &
                 kappa, Atm(1)%q, ng, Atm(1)%ncnst, Atm(1)%gridstruct%area_64, Atm(1)%flagstruct%dry_mass,           &
                 Atm(1)%flagstruct%adjust_dry_mass, Atm(1)%flagstruct%mountain, Atm(1)%flagstruct%moist_phys,   &
                 Atm(1)%flagstruct%hydrostatic, Atm(1)%flagstruct%nwat, Atm(1)%domain, Atm(1)%flagstruct%make_nh)

  end subroutine get_external_ic



  subroutine get_cubed_sphere_terrain( Atm, fv_domain )
    type(fv_atmos_type), intent(inout), target :: Atm(:)
    type(domain2d),      intent(inout) :: fv_domain
    integer              :: ntileMe
    integer, allocatable :: tile_id(:)
    character(len=64)    :: fname
    character(len=7)  :: gn
    integer              ::  n
    integer              ::  jbeg, jend
    real ftop
    real, allocatable :: g_dat2(:,:,:)
    real, allocatable :: pt_coarse(:,:,:)
    integer isc_p, iec_p, jsc_p, jec_p, isg, ieg, jsg,jeg

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = Atm(1)%bd%is
      ie  = Atm(1)%bd%ie
      js  = Atm(1)%bd%js
      je  = Atm(1)%bd%je
      isd = Atm(1)%bd%isd
      ied = Atm(1)%bd%ied
      jsd = Atm(1)%bd%jsd
      jed = Atm(1)%bd%jed

    if (Atm(1)%grid_number > 1) then
       !write(gn,'(A2, I1)') ".g", Atm(1)%grid_number
       write(gn,'(A5, I2.2)') ".nest", Atm(1)%grid_number
    else
       gn = ''
    end if

    ntileMe = size(Atm(:))  ! This will have to be modified for mult tiles per PE
                            ! ASSUMED always one at this point

    allocate( tile_id(ntileMe) )
    tile_id = mpp_get_tile_id( fv_domain )
    do n=1,ntileMe

       call get_tile_string(fname, 'INPUT/fv_core.res'//trim(gn)//'.tile', tile_id(n), '.nc' )
       if (mpp_pe() == mpp_root_pe()) print*, 'external_ic: looking for ', fname

       
       if( file_exist(fname) ) then
          call read_data(fname, 'phis', Atm(n)%phis(is:ie,js:je),      &
                         domain=fv_domain, tile_count=n)
       else
          call surfdrv(  Atm(n)%npx, Atm(n)%npy, Atm(n)%gridstruct%grid_64, Atm(n)%gridstruct%agrid_64,   &
                         Atm(n)%gridstruct%area_64, Atm(n)%gridstruct%dx, Atm(n)%gridstruct%dy, &
                         Atm(n)%gridstruct%dxc, Atm(n)%gridstruct%dyc, Atm(n)%gridstruct%sin_sg, &
                         Atm(n)%phis, Atm(n)%flagstruct%stretch_fac, &
                         Atm(n)%neststruct%nested, Atm(n)%neststruct%npx_global, Atm(N)%domain, &
                         Atm(n)%flagstruct%grid_number, Atm(n)%bd )
          call mpp_error(NOTE,'terrain datasets generated using USGS data')
       endif

    end do

	!Needed for reproducibility. DON'T REMOVE THIS!!
    call mpp_update_domains( Atm(1)%phis, Atm(1)%domain ) 
    ftop = g_sum(Atm(1)%domain, Atm(1)%phis(is:ie,js:je), is, ie, js, je, ng, Atm(1)%gridstruct%area_64, 1)
 
    call prt_maxmin('ZS', Atm(1)%phis,  is, ie, js, je, ng, 1, 1./grav)
    if(is_master()) write(*,*) 'mean terrain height (m)=', ftop/grav
 
    deallocate( tile_id )

  end subroutine get_cubed_sphere_terrain


  subroutine get_diag_ic( Atm, fv_domain, nq )
      type(fv_atmos_type), intent(inout) :: Atm(:)
      type(domain2d),      intent(inout) :: fv_domain
      integer, intent(in):: nq
! local:
      character(len=128) :: fname, tracer_name
      real(kind=4), allocatable:: wk1(:), wk2(:,:), wk3(:,:,:)
      real, allocatable:: tp(:,:,:), qp(:,:,:,:)
      real, allocatable:: ua(:,:,:), va(:,:,:), wa(:,:,:)
      real, allocatable:: lat(:), lon(:), ak0(:), bk0(:)
      real:: s2c(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je,4)
      integer, dimension(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je):: id1, id2, jdc
      real psc(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je)
      real gzc(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je)
      integer:: i, j, k, im, jm, km, npz, npt
      integer:: i1, i2, j1, ncid
      integer:: jbeg, jend
      integer tsize(3), tr_ind
      logical:: found

      integer  sphum, liq_wat, rainwat, ice_wat, snowwat, graupel

      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = Atm(1)%bd%is
      ie  = Atm(1)%bd%ie
      js  = Atm(1)%bd%js
      je  = Atm(1)%bd%je
      isd = Atm(1)%bd%isd
      ied = Atm(1)%bd%ied
      jsd = Atm(1)%bd%jsd
      jed = Atm(1)%bd%jed

      deg2rad = pi/180.

      npz = Atm(1)%npz

! Zero out all initial tracer fields:
      Atm(1)%q = 0.

      fname = Atm(1)%flagstruct%res_latlon_dynamics

      if( file_exist(fname) ) then
          call open_ncfile( fname, ncid )        ! open the file
          call get_ncdim1( ncid, 'lon',   tsize(1) )
          call get_ncdim1( ncid, 'lat',   tsize(2) )
          call get_ncdim1( ncid, 'pfull', tsize(3) )

          im = tsize(1); jm = tsize(2); km = tsize(3)

          if(is_master())  write(*,*) fname, ' FV_diag IC dimensions:', tsize

          allocate (  lon(im) )
          allocate (  lat(jm) )
 
          call _GET_VAR1 (ncid, 'lon', im, lon )
          call _GET_VAR1 (ncid, 'lat', jm, lat )

! Convert to radian
          do i=1,im
             lon(i) = lon(i) * deg2rad  ! lon(1) = 0.
          enddo
          do j=1,jm
             lat(j) = lat(j) * deg2rad
          enddo

          allocate ( ak0(km+1) )
          allocate ( bk0(km+1) )
! npz
          if ( npz /= km ) then
               call mpp_error(FATAL,'==>Error in get_diag_ic: vertical dim must be the same')
          else
               ak0(:) = Atm(1)%ak(:)
               bk0(:) = Atm(1)%bk(:)
          endif
      else
          call mpp_error(FATAL,'==> Error in get_diag_ic: Expected file '//trim(fname)//' for dynamics does not exist')
      endif

! Initialize lat-lon to Cubed bi-linear interpolation coeff:
      call remap_coef( im, jm, lon, lat, id1, id2, jdc, s2c , Atm(1)%gridstruct%agrid, Atm(1)%bd)

! Find bounding latitudes:
      jbeg = jm-1;         jend = 2
      do j=js,je
         do i=is,ie
              j1 = jdc(i,j)
            jbeg = min(jbeg, j1) 
            jend = max(jend, j1+1)
         enddo
      enddo

! remap surface pressure and height:
      allocate ( wk2(im,jbeg:jend) )
      call get_var3_r4( ncid, 'ps', 1,im, jbeg,jend, 1,1, wk2 )
      do j=js,je
         do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            psc(i,j) = s2c(i,j,1)*wk2(i1,j1  ) + s2c(i,j,2)*wk2(i2,j1  ) +  &
                       s2c(i,j,3)*wk2(i2,j1+1) + s2c(i,j,4)*wk2(i1,j1+1)
         enddo
      enddo

      call get_var3_r4( ncid, 'zsurf', 1,im, jbeg,jend, 1,1, wk2 )
      do j=js,je
         do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            gzc(i,j) = s2c(i,j,1)*wk2(i1,j1  ) + s2c(i,j,2)*wk2(i2,j1  ) +  &
                       s2c(i,j,3)*wk2(i2,j1+1) + s2c(i,j,4)*wk2(i1,j1+1)
         enddo
      enddo
      deallocate ( wk2 )

! Read in temperature:
      allocate ( wk3(1:im,jbeg:jend, 1:km) )
      call get_var3_r4( ncid, 'temp', 1,im, jbeg,jend, 1,km, wk3 )
      allocate (  tp(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
         do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            tp(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
         enddo
        enddo
      enddo

! Read in all tracers:
    allocate ( qp(is:ie,js:je,km,nq) )
    qp = 1.E25
    do tr_ind=1, nq
       call get_tracer_names(MODEL_ATMOS, tr_ind, tracer_name)
       if (field_exist(fname,tracer_name)) then
           call get_var3_r4( ncid, tracer_name, 1,im, jbeg,jend, 1,km, wk3 )
           do k=1,km
              do j=js,je
                 do i=is,ie
                    i1 = id1(i,j)
                    i2 = id2(i,j)
                    j1 = jdc(i,j)
                    qp(i,j,k,tr_ind) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                                       s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
                 enddo
              enddo
           enddo
       endif
    enddo
    call remap_scalar(im, jm, km, npz, nq, nq, ak0, bk0, psc, gzc, tp, qp, Atm(1))
    deallocate ( tp )
    deallocate ( qp )

! Winds:
      call get_var3_r4( ncid, 'ucomp', 1,im, jbeg,jend, 1,km, wk3 )
      allocate ( ua(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
          do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            ua(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
          enddo
        enddo
      enddo

      call get_var3_r4( ncid, 'vcomp', 1,im, jbeg,jend, 1,km, wk3 )
      allocate ( va(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
          do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            va(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
          enddo
        enddo
      enddo
      call remap_winds(im, jm, km, npz, ak0, bk0, psc, ua, va, Atm(1))

      deallocate ( ua )
      deallocate ( va )

      if ( .not. Atm(1)%flagstruct%hydrostatic ) then
        if (field_exist(fname,'w')) then
           allocate ( wa(is:ie,js:je,km) )
           call get_var3_r4( ncid, 'w', 1,im, jbeg,jend, 1,km, wk3 )
           do k=1,km
              do j=js,je
                 do i=is,ie
                    i1 = id1(i,j)
                    i2 = id2(i,j)
                    j1 = jdc(i,j)
                    wa(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                                s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
                 enddo
              enddo
           enddo
           call remap_wz(im, jm, km, npz, ng, ak0, bk0, psc, wa, Atm(1)%w, Atm(1))
           deallocate ( wa )
        else    ! use "w = - fac * omega" ?
           Atm(1)%w(:,:,:) = 0.
        endif
! delz:
        if (field_exist(fname,'delz')) then
           allocate ( wa(is:ie,js:je,km) )
           call get_var3_r4( ncid, 'delz', 1,im, jbeg,jend, 1,km, wk3 )
           do k=1,km
              do j=js,je
                 do i=is,ie
                    i1 = id1(i,j)
                    i2 = id2(i,j)
                    j1 = jdc(i,j)
                    wa(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                                s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
                 enddo
              enddo
           enddo
           call remap_wz(im, jm, km, npz, 0,  ak0, bk0, psc, wa, Atm(1)%delz, Atm(1))
           deallocate ( wa )
        else    ! Force make = T
           Atm(1)%flagstruct%make_nh = .true.
        endif

      endif   ! hydrostatic test

      call close_ncfile ( ncid )
      deallocate ( wk3 )
      deallocate ( ak0 )
      deallocate ( bk0 )
      deallocate ( lat )
      deallocate ( lon )

  end subroutine get_diag_ic



  subroutine get_nggps_ic (Atm, fv_domain)
!    read in data after it has been preprocessed with 
!    NCEP/EMC orography maker and global_chgres
!    and has been horiztontally interpolated to the 
!    current cubed-sphere grid
!
!--- variables read in from 'gfs_ctrl.nc'
!       VCOORD  -  level information
!                   maps to 'ak & bk'
!--- variables read in from 'sfc_data.nc'
!       land_frac  -  land-sea-ice mask (L:0 / S:1)
!                     maps to 'oro'
!       TSEA       -  surface skin temperature (k)
!                     maps to 'ts'
!--- variables read in from 'gfs_data.nc'
!       ZH  -  GFS grid height at edges (m)
!       PS  -  surface pressure (Pa)
!       U_W -  D-grid west  face tangential wind component (m/s)
!       V_W -  D-grid west  face normal wind component (m/s)
!       U_S -  D-grid south face tangential wind component (m/s)
!       V_S -  D-grid south face normal wind component (m/s)
!       W   -  vertical velocity 'omega' (Pa/s)
!       Q   -  prognostic tracer fields (Specific Humidity, 
!                                        O3 mixing ratio,
!                                        Cloud mixing ratio)
!--- Namelist variables 
!       filtered_terrain  -  use orography maker filtered terrain mapping
!       ncep_plevels      -  use NCEP pressure levels (implies no vertical remapping)


      type(fv_atmos_type), intent(inout) :: Atm(:)
      type(domain2d),      intent(inout) :: fv_domain
! local:
      real, dimension(:), allocatable:: ak, bk
      real, dimension(:,:), allocatable:: wk2, ps
      real, dimension(:,:,:), allocatable:: ud, vd, u_w, v_w, u_s, v_s, omga
      real, dimension(:,:,:), allocatable:: zh(:,:,:)  ! 3D height at 65 edges
      real, dimension(:,:,:,:), allocatable:: q
      real, dimension(:,:), allocatable :: phis_coarse ! lmh
      real rdg, wt
      integer:: n, npx, npy, npz, itoa, nt, ntprog, ntdiag, ntracers, ntrac
      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed
      integer :: ios, ierr, unit, id_res
      type (restart_file_type) :: ORO_restart, SFC_restart, GFS_restart
      character(len=6)  :: gn, stile_name
      character(len=64) :: tracer_name
      character(len=64) :: fn_gfs_ctl = 'gfs_ctrl.nc'
      character(len=64) :: fn_gfs_ics = 'gfs_data.nc'
      character(len=64) :: fn_sfc_ics = 'sfc_data.nc'
      character(len=64) :: fn_oro_ics = 'oro_data.nc'
      logical :: remap
      logical :: filtered_terrain = .true.
      logical :: ncep_plevels = .false.
      logical :: gfs_dwinds = .true.
      integer :: levp = 64
      logical :: checker_tr = .false.
      integer :: nt_checker = 0
      real(kind=R_GRID), dimension(2):: p1, p2, p3
      real(kind=R_GRID), dimension(3):: e1, e2, ex, ey
      integer:: i,j,k,nts, ks
      integer:: liq_wat
      namelist /external_ic_nml/ filtered_terrain, ncep_plevels, levp, gfs_dwinds, &
                                 checker_tr, nt_checker
#ifdef GFSL64
   real, dimension(65):: ak_sj, bk_sj
   data ak_sj/20.00000,      68.00000,     137.79000,   &
             221.95800,     318.26600,     428.43400,   &
             554.42400,     698.45700,     863.05803,   &
            1051.07995,    1265.75194,    1510.71101,   &
            1790.05098,    2108.36604,    2470.78817,   &
            2883.03811,    3351.46002,    3883.05187,   &
            4485.49315,    5167.14603,    5937.04991,   &
            6804.87379,    7780.84698,    8875.64338,   &
            9921.40745,   10760.99844,   11417.88354,   &
           11911.61193,   12258.61668,   12472.89642,   &
           12566.58298,   12550.43517,   12434.26075,   &
           12227.27484,   11938.39468,   11576.46910,   &
           11150.43640,   10669.41063,   10142.69482,   &
            9579.72458,    8989.94947,    8382.67090,   &
            7766.85063,    7150.91171,    6542.55077,   &
            5948.57894,    5374.81094,    4825.99383,   &
            4305.79754,    3816.84622,    3360.78848,   &
            2938.39801,    2549.69756,    2194.08449,   &
            1870.45732,    1577.34218,    1313.00028,   &
            1075.52114,     862.90778,     673.13815,   &
             504.22118,     354.22752,     221.32110,   &
             103.78014,       0./
   data bk_sj/0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00000,       0.00000,       0.00000,    &
              0.00179,       0.00705,       0.01564,    &
              0.02749,       0.04251,       0.06064,    &
              0.08182,       0.10595,       0.13294,    &
              0.16266,       0.19492,       0.22950,    &
              0.26615,       0.30455,       0.34435,    &
              0.38516,       0.42656,       0.46815,    &
              0.50949,       0.55020,       0.58989,    &
              0.62825,       0.66498,       0.69987,    &
              0.73275,       0.76351,       0.79208,    &
              0.81845,       0.84264,       0.86472,    &
              0.88478,       0.90290,       0.91923,    &
              0.93388,       0.94697,       0.95865,    &
              0.96904,       0.97826,       0.98642,    &
              0.99363,       1./
#else
! The following L63 setting is the same as NCEP GFS's L64 except the top layer
      real, dimension(64):: ak_sj, bk_sj
      data ak_sj/64.247,       137.790,       221.958,      &
                318.266,       428.434,       554.424,      &
                698.457,       863.05803,    1051.07995,    &  
               1265.75194,    1510.71101,    1790.05098,    &
               2108.36604,    2470.78817,    2883.03811,    &
               3351.46002,    3883.05187,    4485.49315,    &
               5167.14603,    5937.04991,    6804.87379,    &
               7780.84698,    8875.64338,   10100.20534,    &
              11264.35673,   12190.64366,   12905.42546,    &
              13430.87867,   13785.88765,   13986.77987,    &
              14047.96335,   13982.46770,   13802.40331,    &
              13519.33841,   13144.59486,   12689.45608,    &
              12165.28766,   11583.57006,   10955.84778,    &
              10293.60402,    9608.08306,    8910.07678,    &
               8209.70131,    7516.18560,    6837.69250,    &
               6181.19473,    5552.39653,    4955.72632,    &
               4394.37629,    3870.38682,    3384.76586,    &
               2937.63489,    2528.37666,    2155.78385,    &
               1818.20722,    1513.68173,    1240.03585,    &
                994.99144,     776.23591,     581.48797,    &
                408.53400,     255.26520,     119.70243, 0. /

      data bk_sj/0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00000,       0.00000,       0.00000,    &
                 0.00201,       0.00792,       0.01755,    &
                 0.03079,       0.04751,       0.06761,    &
                 0.09097,       0.11746,       0.14690,    &
                 0.17911,       0.21382,       0.25076,    &
                 0.28960,       0.32994,       0.37140,    &
                 0.41353,       0.45589,       0.49806,    &
                 0.53961,       0.58015,       0.61935,    &
                 0.65692,       0.69261,       0.72625,    &
                 0.75773,       0.78698,       0.81398,    &
                 0.83876,       0.86138,       0.88192,    &
                 0.90050,       0.91722,       0.93223,    &
                 0.94565,       0.95762,       0.96827,    &
                 0.97771,       0.98608,       0.99347,  1./
#endif

#ifdef TEMP_GFSPLV
      real, dimension(64):: ak_sj, bk_sj
      data ak_sj/64.247,      137.79,         221.958,      & 
                318.266,       428.434,       554.424,      &
                698.457,       863.058,      1051.08,       &
               1265.752,      1510.711,      1790.051,      &
               2108.366,      2470.788,      2883.038,      &
               3351.46,       3883.052,      4485.493,      &
               5167.146,      5937.05,       6804.874,      &
               7777.15,       8832.537,      9936.614,      &
              11054.85,      12152.94,      13197.07,       &
              14154.32,      14993.07,      15683.49,       &
              16197.97,      16511.74,      16611.6,        &
              16503.14,      16197.32,      15708.89,       &
              15056.34,      14261.43,      13348.67,       &
              12344.49,      11276.35,      10171.71,       &
               9057.051,      7956.908,      6893.117,      &
               5884.206,      4945.029,      4086.614,      &
               3316.217,      2637.553,      2051.15,       &
               1554.789,      1143.988,       812.489,      &
                552.72,        356.223,       214.015,      &
                116.899,        55.712,        21.516,      &
                  5.741,         0.575,         0.,      0. /

      data bk_sj/0.00000,       0.00000,       0.00000,     &
                 0.00000,       0.00000,       0.00000,     &
                 0.00000,       0.00000,       0.00000,     &
                 0.00000,       0.00000,       0.00000,     &
                 0.00000,       0.00000,       0.00000,     &
                 0.00000,       0.00000,       0.00000,     &
                 0.00000,       0.00000,       0.00000,     &
                 0.00003697,    0.00043106,    0.00163591,  &
                 0.00410671,    0.00829402,    0.01463712,  &
                 0.02355588,    0.03544162,    0.05064684,  &
                 0.06947458,    0.09216691,    0.1188122,   &
                 0.1492688,     0.1832962,     0.2205702,   &
                 0.2606854,     0.3031641,     0.3474685,   &
                 0.3930182,     0.4392108,     0.4854433,   &
                 0.5311348,     0.5757467,     0.6187996,   &
                 0.659887,      0.6986829,     0.7349452,   &
                 0.7685147,     0.7993097,     0.8273188,   &
                 0.8525907,     0.8752236,     0.895355,    &
                 0.913151,      0.9287973,     0.9424911,   &
                 0.9544341,     0.9648276,     0.9738676,   &
                 0.9817423,     0.9886266,     0.9946712, 1./
#endif

      call mpp_error(NOTE,'Using external_IC::get_nggps_ic which is valid only for data which has been &
                          &horizontally interpolated to the current cubed-sphere grid')
#ifdef INTERNAL_FILE_NML
      read (input_nml_file,external_ic_nml,iostat=ios)
      ierr = check_nml_error(ios,'external_ic_nml')
#else
      unit=open_namelist_file()
      read (unit,external_ic_nml,iostat=ios)
      ierr = check_nml_error(ios,'external_ic_nml')
      call close_file(unit)
#endif

      unit = stdlog()
      call write_version_number ( 'NGGPS_release', 'get_nggps_ics' )
      write(unit, nml=external_ic_nml)

      remap = .true.
      if (ncep_plevels) then
        if (filtered_terrain) then
          call mpp_error(NOTE,'External_IC::get_nggps_ic -  use externally-generated, filtered terrain &
                              &and NCEP pressure levels (vertical remapping)')
        else if (.not. filtered_terrain) then
          call mpp_error(NOTE,'External_IC::get_nggps_ic -  use externally-generated, raw terrain &
                              &and NCEP pressure levels (vertical remapping)')
        endif
      else  ! (.not.ncep_plevels)
        if (filtered_terrain) then
          call mpp_error(NOTE,'External_IC::get_nggps_ic -  use externally-generated, filtered terrain &
                              &and FV3 pressure levels (vertical remapping)')
        else if (.not. filtered_terrain) then
          call mpp_error(NOTE,'External_IC::get_nggps_ic -  use externally-generated, raw terrain &
                              &and FV3 pressure levels (vertical remapping)')
        endif
      endif

      is  = Atm(1)%bd%is
      ie  = Atm(1)%bd%ie
      js  = Atm(1)%bd%js
      je  = Atm(1)%bd%je
      isd = Atm(1)%bd%isd
      ied = Atm(1)%bd%ied
      jsd = Atm(1)%bd%jsd
      jed = Atm(1)%bd%jed
      npz = Atm(1)%npz
      call get_number_tracers(MODEL_ATMOS, num_tracers=ntracers, num_prog=ntprog)
      ntdiag = ntracers-ntprog

!--- set the 'nestXX' appendix for all files using fms_io
      if (Atm(1)%grid_number > 1) then
         write(gn,'(A4, I2.2)') "nest", Atm(1)%grid_number
      else
         gn = ''
      end if
      call set_filename_appendix('')

!--- test for existence of the GFS control file
      if (.not. file_exist('INPUT/'//trim(fn_gfs_ctl), no_domain=.TRUE.)) then
        call mpp_error(FATAL,'==> Error in External_ic::get_nggps_ic: file '//trim(fn_gfs_ctl)//' for NGGPS IC does not exist')
      endif
      call mpp_error(NOTE,'==> External_ic::get_nggps_ic: using control file '//trim(fn_gfs_ctl)//' for NGGPS IC')

!--- read in the number of tracers in the NCEP NGGPS ICs
      call read_data ('INPUT/'//trim(fn_gfs_ctl), 'ntrac', ntrac, no_domain=.TRUE.)
      if (ntrac > ntracers) call mpp_error(FATAL,'==> External_ic::get_nggps_ic: more NGGPS tracers &
                                 &than defined in field_table '//trim(fn_gfs_ctl)//' for NGGPS IC')

!--- read in ak and bk from the gfs control file using fms_io read_data ---
      allocate (wk2(levp+1,2))
      allocate (ak(levp+1))
      allocate (bk(levp+1))
      call read_data('INPUT/'//trim(fn_gfs_ctl),'vcoord',wk2, no_domain=.TRUE.)
      ak(1:levp+1) = wk2(1:levp+1,1)
      bk(1:levp+1) = wk2(1:levp+1,2)
      deallocate (wk2)

      if (.not. file_exist('INPUT/'//trim(fn_oro_ics), domain=Atm(1)%domain)) then
        call mpp_error(FATAL,'==> Error in External_ic::get_nggps_ic: tiled file '//trim(fn_oro_ics)//' for NGGPS IC does not exist')
      endif
      call mpp_error(NOTE,'==> External_ic::get_nggps_ic: using tiled data file '//trim(fn_oro_ics)//' for NGGPS IC')

      if (.not. file_exist('INPUT/'//trim(fn_sfc_ics), domain=Atm(1)%domain)) then
        call mpp_error(FATAL,'==> Error in External_ic::get_nggps_ic: tiled file '//trim(fn_sfc_ics)//' for NGGPS IC does not exist')
      endif
      call mpp_error(NOTE,'==> External_ic::get_nggps_ic: using tiled data file '//trim(fn_sfc_ics)//' for NGGPS IC')

      if (.not. file_exist('INPUT/'//trim(fn_gfs_ics), domain=Atm(1)%domain)) then
        call mpp_error(FATAL,'==> Error in External_ic::get_nggps_ic: tiled file '//trim(fn_gfs_ics)//' for NGGPS IC does not exist')
      endif
      call mpp_error(NOTE,'==> External_ic::get_nggps_ic: using tiled data file '//trim(fn_gfs_ics)//' for NGGPS IC')

      allocate (zh(is:ie,js:je,levp+1))   ! SJL
      allocate (ps(is:ie,js:je))
      allocate (omga(is:ie,js:je,levp))
      allocate (q (is:ie,js:je,levp,ntracers))
      allocate ( u_w(is:ie+1, js:je, 1:levp) )
      allocate ( v_w(is:ie+1, js:je, 1:levp) )
      allocate ( u_s(is:ie, js:je+1, 1:levp) )
      allocate ( v_s(is:ie, js:je+1, 1:levp) )

      do n = 1,size(Atm(:))

        !!! If a nested grid, save the filled coarse-grid topography for blending
        if (Atm(n)%neststruct%nested) then
          allocate(phis_coarse(isd:ied,jsd:jed))
          do j=jsd,jed
            do i=isd,ied
              phis_coarse(i,j) = Atm(n)%phis(i,j)
            enddo
          enddo
        endif

!--- read in surface temperature (k) and land-frac
        ! surface skin temperature
        id_res = register_restart_field (SFC_restart, fn_sfc_ics, 'tsea', Atm(n)%ts, domain=Atm(n)%domain)

        ! terrain surface height -- (needs to be transformed into phis = zs*grav)
        if (filtered_terrain) then
          id_res = register_restart_field (ORO_restart, fn_oro_ics, 'orog_filt', Atm(n)%phis, domain=Atm(n)%domain)
        elseif (.not. filtered_terrain) then
          id_res = register_restart_field (ORO_restart, fn_oro_ics, 'orog_raw', Atm(n)%phis, domain=Atm(n)%domain)
        endif

        if ( Atm(n)%flagstruct%fv_land ) then
          ! stddev
          id_res = register_restart_field (SFC_restart, fn_oro_ics, 'stddev', Atm(n)%sgh, domain=Atm(n)%domain)
          ! land-frac
          id_res = register_restart_field (SFC_restart, fn_oro_ics, 'land-frac', Atm(n)%oro, domain=Atm(n)%domain)
        endif
     
        ! surface pressure (Pa)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'ps', ps, domain=Atm(n)%domain)

        ! D-grid west  face tangential wind component (m/s)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'u_w', u_w, domain=Atm(n)%domain,position=EAST)
        ! D-grid west  face normal wind component (m/s)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'v_w', v_w, domain=Atm(n)%domain,position=EAST)
        ! D-grid south face tangential wind component (m/s)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'u_s', u_s, domain=Atm(n)%domain,position=NORTH)
        ! D-grid south face normal wind component (m/s)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'v_s', v_s, domain=Atm(n)%domain,position=NORTH)

        ! vertical velocity 'omega' (Pa/s)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'w', omga, domain=Atm(n)%domain)
        ! GFS grid height at edges (including surface height)
        id_res = register_restart_field (GFS_restart, fn_gfs_ics, 'ZH', zh, domain=Atm(n)%domain)

        ! prognostic tracers
        do nt = 1, ntracers
          call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
          id_res = register_restart_field (GFS_restart, fn_gfs_ics, trim(tracer_name), q(:,:,:,nt), &
                                           mandatory=.false.,domain=Atm(n)%domain)
        enddo

        ! initialize all tracers to default values prior to being input
        do nt = 1, ntprog
          call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
          ! set all tracers to an initial profile value
          call set_tracer_profile (MODEL_ATMOS, nt, Atm(n)%q(:,:,:,n)  )
        enddo
        do nt = ntprog+1, ntracers
          call get_tracer_names(MODEL_ATMOS, nt, tracer_name)
          ! set all tracers to an initial profile value
          call set_tracer_profile (MODEL_ATMOS, nt, Atm(n)%qdiag(:,:,:,nt)  )
        enddo

        ! read in the restart
        call restore_state (ORO_restart)
        call restore_state (SFC_restart)
        call restore_state (GFS_restart)
        ! free the restart type to be re-used by the nest
        call free_restart_type(ORO_restart)
        call free_restart_type(SFC_restart)
        call free_restart_type(GFS_restart)

        ! multiply NCEP ICs terrain 'phis' by gravity to be true geopotential
        Atm(n)%phis = Atm(n)%phis*grav
        
        ! set the pressure levels and ptop to be used
        if (ncep_plevels) then
          itoa = levp - npz + 1
          Atm(n)%ptop = ak(itoa)
          Atm(n)%ak(1:npz+1) = ak(itoa:levp+1)
          Atm(n)%bk(1:npz+1) = bk(itoa:levp+1)
        else
          if ( npz <= 64 ) then
             Atm(n)%ak(:) = ak_sj(:)
             Atm(n)%bk(:) = bk_sj(:)
             Atm(n)%ptop = Atm(n)%ak(1)
          else
             call set_eta(npz, ks, Atm(n)%ptop, Atm(n)%ak, Atm(n)%bk)
          endif
        endif
        ! call vertical remapping algorithms
        if(is_master())  write(*,*) 'GFS ak(1)=', ak(1), ' ak(2)=', ak(2)
        ak(1) = max(1.e-9, ak(1))

        call remap_scalar_nggps(Atm(n), levp, npz, ntracers, ak, bk, ps, q, omga, zh)

        allocate ( ud(is:ie,  js:je+1, 1:levp) )
        allocate ( vd(is:ie+1,js:je,   1:levp) )
        do k=1,levp
          do j=js,je+1
            do i=is,ie
              p1(:) = Atm(1)%gridstruct%grid(i,  j,1:2)
              p2(:) = Atm(1)%gridstruct%grid(i+1,j,1:2)
              call  mid_pt_sphere(p1, p2, p3)
              call get_unit_vect2(p1, p2, e1)
              call get_latlon_vector(p3, ex, ey)
              ud(i,j,k) = u_s(i,j,k)*inner_prod(e1,ex) + v_s(i,j,k)*inner_prod(e1,ey)
            enddo
          enddo
          do j=js,je
            do i=is,ie+1
              p1(:) = Atm(1)%gridstruct%grid(i,j  ,1:2)
              p2(:) = Atm(1)%gridstruct%grid(i,j+1,1:2)
              call  mid_pt_sphere(p1, p2, p3)
              call get_unit_vect2(p1, p2, e2)
              call get_latlon_vector(p3, ex, ey)
              vd(i,j,k) = u_w(i,j,k)*inner_prod(e2,ex) + v_w(i,j,k)*inner_prod(e2,ey)
            enddo
          enddo
        enddo
        deallocate ( u_w )
        deallocate ( v_w )
        deallocate ( u_s )
        deallocate ( v_s )
             
        call remap_dwinds(levp, npz, ak, bk, ps, ud, vd, Atm(n))
        deallocate ( ud )
        deallocate ( vd )
   
        !!! Perform terrain smoothing, if desired
        if ( Atm(n)%flagstruct%n_zs_filter > 0 ) then
          if (Atm(n)%neststruct%nested) then
            if (is_master()) write(*,*) 'Blending nested and coarse grid topography'
            npx = Atm(n)%npx
            npy = Atm(n)%npy
            do j=jsd,jed
              do i=isd,ied
                wt = max(0.,min(1.,real(5 - min(i,j,npx-i,npy-j,5))/5. ))
                Atm(n)%phis(i,j) = (1.-wt)*Atm(n)%phis(i,j) + wt*phis_coarse(i,j)
              enddo
            enddo
          endif

          if ( Atm(n)%flagstruct%nord_zs_filter == 2 ) then
            call del2_cubed_sphere(Atm(n)%npx, Atm(n)%npy, Atm(n)%phis, &
                   Atm(n)%gridstruct%area_64, Atm(n)%gridstruct%dx, Atm(n)%gridstruct%dy,   &
                   Atm(n)%gridstruct%dxc, Atm(n)%gridstruct%dyc, Atm(n)%gridstruct%sin_sg, &
                   Atm(n)%flagstruct%n_zs_filter, cnst_0p20*Atm(n)%gridstruct%da_min, &
                   .false., oro_g, Atm(n)%neststruct%nested, Atm(n)%domain, Atm(n)%bd)
            if ( is_master() ) write(*,*) 'Warning !!! del-2 terrain filter has been applied ', &
                   Atm(n)%flagstruct%n_zs_filter, ' times'
          else if( Atm(n)%flagstruct%nord_zs_filter == 4 ) then
            call del4_cubed_sphere(Atm(n)%npx, Atm(n)%npy, Atm(n)%phis, Atm(n)%gridstruct%area_64, &
                   Atm(n)%gridstruct%dx, Atm(n)%gridstruct%dy,   &
                   Atm(n)%gridstruct%dxc, Atm(n)%gridstruct%dyc, Atm(n)%gridstruct%sin_sg, &
                   Atm(n)%flagstruct%n_zs_filter, .false., oro_g, Atm(n)%neststruct%nested, &
                   Atm(n)%domain, Atm(n)%bd)
            if ( is_master() ) write(*,*) 'Warning !!! del-4 terrain filter has been applied ', &
                   Atm(n)%flagstruct%n_zs_filter, ' times'
          endif

        endif

        if (Atm(n)%neststruct%nested) then
          npx = Atm(n)%npx
          npy = Atm(n)%npy
          do j=jsd,jed
            do i=isd,ied
              wt = max(0.,min(1.,real(5 - min(i,j,npx-i,npy-j,5))/5. ))
              Atm(n)%phis(i,j) = (1.-wt)*Atm(n)%phis(i,j) + wt*phis_coarse(i,j)
            enddo
          enddo
          deallocate(phis_coarse)
        endif

        call mpp_update_domains( Atm(n)%phis, Atm(n)%domain, complete=.true. )
        liq_wat  = get_tracer_index(MODEL_ATMOS, 'liq_wat')
!--- Add cloud condensate from GFS to total MASS
        do k=1,npz
          do j=js,je
            do i=is,ie
              Atm(n)%delp(i,j,k) = Atm(n)%delp(i,j,k)*(1.+Atm(n)%q(i,j,k,liq_wat))
            enddo
          enddo
        enddo

!--- reset the tracers beyond condensate to a checkerboard pattern 
        if (checker_tr) then
          nts = ntracers - nt_checker+1
          call checker_tracers(is,ie, js,je, isd,ied, jsd,jed, nt_checker, &
                               npz, Atm(n)%q(:,:,:,nts:ntracers),          &
                               Atm(n)%gridstruct%agrid_64(is:ie,js:je,1),     &
                               Atm(n)%gridstruct%agrid_64(is:ie,js:je,2), 9., 9.)
        endif
      enddo ! n-loop

      Atm(1)%flagstruct%make_nh = .false.

      deallocate (ak)
      deallocate (bk)
      deallocate (ps)
      deallocate (q )

  end subroutine get_nggps_ic

  

  subroutine get_ncep_ic( Atm, fv_domain, nq )
      type(fv_atmos_type), intent(inout) :: Atm(:)
      type(domain2d),      intent(inout) :: fv_domain
      integer, intent(in):: nq
! local:
      character(len=128) :: fname
      real(kind=4), allocatable:: wk1(:), wk2(:,:), wk3(:,:,:)
      real, allocatable:: tp(:,:,:), qp(:,:,:)
      real, allocatable:: ua(:,:,:), va(:,:,:)
      real, allocatable:: lat(:), lon(:), ak0(:), bk0(:)
      real:: s2c(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je,4)
      integer, dimension(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je):: id1, id2, jdc
      real psc(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je)
      real gzc(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je)
      real tmean
      integer:: i, j, k, im, jm, km, npz, npt
      integer:: i1, i2, j1, ncid
      integer:: jbeg, jend
      integer tsize(3) 
      logical:: read_ts = .true.
      logical:: land_ts = .false.
      logical:: found
      integer :: is,  ie,  js,  je
      integer :: isd, ied, jsd, jed

      is  = Atm(1)%bd%is
      ie  = Atm(1)%bd%ie
      js  = Atm(1)%bd%js
      je  = Atm(1)%bd%je
      isd = Atm(1)%bd%isd
      ied = Atm(1)%bd%ied
      jsd = Atm(1)%bd%jsd
      jed = Atm(1)%bd%jed

      deg2rad = pi/180.

      npz = Atm(1)%npz

! Zero out all initial tracer fields:
! SJL: 20110716
!      Atm(1)%q = 0.

      fname = Atm(1)%flagstruct%res_latlon_dynamics

      if( file_exist(fname) ) then
          call open_ncfile( fname, ncid )        ! open the file
          call get_ncdim1( ncid, 'lon', tsize(1) )
          call get_ncdim1( ncid, 'lat', tsize(2) )
          call get_ncdim1( ncid, 'lev', tsize(3) )

          im = tsize(1); jm = tsize(2); km = tsize(3)

          if(is_master())  write(*,*) fname
          if(is_master())  write(*,*) ' NCEP IC dimensions:', tsize

          allocate (  lon(im) )
          allocate (  lat(jm) )
 
          call _GET_VAR1 (ncid, 'lon', im, lon )
          call _GET_VAR1 (ncid, 'lat', jm, lat )

! Convert to radian
          do i=1,im
             lon(i) = lon(i) * deg2rad  ! lon(1) = 0.
          enddo
          do j=1,jm
             lat(j) = lat(j) * deg2rad
          enddo

          allocate ( ak0(km+1) )
          allocate ( bk0(km+1) )
          call _GET_VAR1 (ncid, 'hyai', km+1, ak0, found )
          if ( .not. found )  ak0(:) = 0.

          call _GET_VAR1 (ncid, 'hybi', km+1, bk0 )

! Note: definition of NCEP hybrid is p(k) = a(k)*1.E5 + b(k)*ps
          ak0(:) = ak0(:) * 1.E5

! Limiter to prevent NAN at top during remapping
          if ( bk0(1) < 1.E-9 ) ak0(1) = max(1.e-9, ak0(1))
      else
          call mpp_error(FATAL,'==> Error in get_external_ic: Expected file '//trim(fname)//' for NCEP IC does not exist')
      endif

! Initialize lat-lon to Cubed bi-linear interpolation coeff:
      call remap_coef( im, jm, lon, lat, id1, id2, jdc, s2c , Atm(1)%gridstruct%agrid, Atm(1)%bd)

! Find bounding latitudes:
      jbeg = jm-1;         jend = 2
      do j=js,je
         do i=is,ie
              j1 = jdc(i,j)
            jbeg = min(jbeg, j1) 
            jend = max(jend, j1+1)
         enddo
      enddo

! remap surface pressure and height:

      allocate ( wk2(im,jbeg:jend) )
      call get_var3_r4( ncid, 'PS', 1,im, jbeg,jend, 1,1, wk2 )

      do j=js,je
         do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            psc(i,j) = s2c(i,j,1)*wk2(i1,j1  ) + s2c(i,j,2)*wk2(i2,j1  ) +  &
                       s2c(i,j,3)*wk2(i2,j1+1) + s2c(i,j,4)*wk2(i1,j1+1)
         enddo
      enddo

      call get_var3_r4( ncid, 'PHIS', 1,im, jbeg,jend, 1,1, wk2 )
      do j=js,je
         do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            gzc(i,j) = s2c(i,j,1)*wk2(i1,j1  ) + s2c(i,j,2)*wk2(i2,j1  ) +  &
                       s2c(i,j,3)*wk2(i2,j1+1) + s2c(i,j,4)*wk2(i1,j1+1)
         enddo
      enddo

      deallocate ( wk2 )
      allocate ( wk2(im,jm) )

      if ( read_ts ) then       ! read skin temperature; could be used for SST

        call get_var2_real( ncid, 'TS', im, jm, wk2 )

        if ( .not. land_ts ) then
           allocate ( wk1(im) )

           do j=1,jm
! Read NCEP ORO (1; land; 0: ocean; 2: sea_ice)
              call get_var3_r4( ncid, 'ORO', 1,im, j,j, 1,1, wk1 )
              tmean = 0.
              npt = 0
              do i=1,im
                 if( abs(wk1(i)-1.) > 0.99 ) then   ! ocean or sea ice
                     tmean = tmean + wk2(i,j)
                     npt = npt + 1
                 endif
              enddo
!------------------------------------------------------
! Replace TS over interior land with zonal mean SST/Ice
!------------------------------------------------------
              if ( npt /= 0 ) then
                   tmean= tmean / real(npt)
                   do i=1,im
                      if( abs(wk1(i)-1.) <= 0.99 ) then  ! Land points
                          if ( i==1 ) then
                               i1 = im;     i2 = 2
                          elseif ( i==im ) then
                               i1 = im-1;   i2 = 1
                          else
                               i1 = i-1;    i2 = i+1
                          endif
                          if ( abs(wk1(i2)-1.)>0.99 ) then     ! east side has priority
                               wk2(i,j) = wk2(i2,j)
                          elseif ( abs(wk1(i1)-1.)>0.99 ) then ! west side
                               wk2(i,j) = wk2(i1,j)
                          else
                               wk2(i,j) = tmean
                          endif
                      endif
                   enddo
              endif
           enddo   ! j-loop
           deallocate ( wk1 )
        endif   !(.not.land_ts)

        do j=js,je
          do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            Atm(1)%ts(i,j) = s2c(i,j,1)*wk2(i1,j1  ) + s2c(i,j,2)*wk2(i2,j1  ) +  &
                             s2c(i,j,3)*wk2(i2,j1+1) + s2c(i,j,4)*wk2(i1,j1+1)
          enddo
        enddo
        call prt_maxmin('SST_model', Atm(1)%ts, is, ie, js, je, 0, 1, 1.)

! Perform interp to FMS SST format/grid
#ifndef DYCORE_SOLO
        call ncep2fms(im, jm, lon, lat, wk2)
        if( is_master() ) then
          write(*,*) 'External_ic_mod: i_sst=', i_sst, ' j_sst=', j_sst
          call pmaxmin( 'SST_ncep_fms',  sst_ncep, i_sst, j_sst, 1.)
        endif
#endif
      endif  !(read_ts)

      deallocate ( wk2 )

! Read in temperature:
      allocate ( wk3(1:im,jbeg:jend, 1:km) )
      call get_var3_r4( ncid, 'T', 1,im, jbeg,jend, 1,km, wk3 )

      allocate (  tp(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
         do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            tp(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
         enddo
        enddo
      enddo

! Read in tracers: only sphum at this point
      call get_var3_r4( ncid, 'Q', 1,im, jbeg,jend, 1,km, wk3 )

      allocate ( qp(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
          do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            qp(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
          enddo
        enddo
      enddo

      call remap_scalar(im, jm, km, npz, nq, nq, ak0, bk0, psc, gzc, tp, qp, Atm(1))
      deallocate ( tp )
      deallocate ( qp )

! Winds:
      call get_var3_r4( ncid, 'U', 1,im, jbeg,jend, 1,km, wk3 )

      allocate ( ua(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
          do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            ua(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
          enddo
        enddo
      enddo

      call get_var3_r4( ncid, 'V', 1,im, jbeg,jend, 1,km, wk3 )
      call close_ncfile ( ncid )

      allocate ( va(is:ie,js:je,km) )
      do k=1,km
        do j=js,je
          do i=is,ie
            i1 = id1(i,j)
            i2 = id2(i,j)
            j1 = jdc(i,j)
            va(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k) +  &
                        s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
          enddo
        enddo
      enddo
      deallocate ( wk3 )

      call remap_winds(im, jm, km, npz, ak0, bk0, psc, ua, va, Atm(1))

      deallocate ( ua )
      deallocate ( va )

      deallocate ( ak0 )
      deallocate ( bk0 )
      deallocate ( lat )
      deallocate ( lon )

  
  end subroutine get_ncep_ic



  subroutine get_fv_ic( Atm, fv_domain, nq )
      type(fv_atmos_type), intent(inout) :: Atm(:)
      type(domain2d),      intent(inout) :: fv_domain
      integer, intent(in):: nq

      character(len=128) :: fname, tracer_name
      real, allocatable:: ps0(:,:), gz0(:,:), u0(:,:,:), v0(:,:,:), t0(:,:,:), dp0(:,:,:), q0(:,:,:,:)
      real, allocatable:: ua(:,:,:), va(:,:,:)
      real, allocatable:: lat(:), lon(:), ak0(:), bk0(:)
      integer :: i, j, k, im, jm, km, npz, tr_ind
      integer tsize(3)
!     integer sphum, liq_wat, ice_wat, cld_amt       ! GFDL AM2 physics
      logical found

      npz = Atm(1)%npz

! Zero out all initial tracer fields:
      Atm(1)%q = 0.

! Read in lat-lon FV core restart file
      fname = Atm(1)%flagstruct%res_latlon_dynamics

      if( file_exist(fname) ) then
          call field_size(fname, 'T', tsize, field_found=found)
          if(is_master()) write(*,*) 'Using lat-lon FV restart:', fname 

          if ( found ) then
               im = tsize(1); jm = tsize(2); km = tsize(3)
               if(is_master())  write(*,*) 'External IC dimensions:', tsize
          else
               call mpp_error(FATAL,'==> Error in get_external_ic: field not found')
          endif

! Define the lat-lon coordinate:
          allocate (  lon(im) )
          allocate (  lat(jm) )

          do i=1,im
             lon(i) = (0.5 + real(i-1)) * 2.*pi/real(im)
          enddo

          do j=1,jm
             lat(j) = -0.5*pi + real(j-1)*pi/real(jm-1)   ! SP to NP 
          enddo
 
          allocate ( ak0(1:km+1) )
          allocate ( bk0(1:km+1) )
          allocate ( ps0(1:im,1:jm) )
          allocate ( gz0(1:im,1:jm) )
          allocate (  u0(1:im,1:jm,1:km) )
          allocate (  v0(1:im,1:jm,1:km) )
          allocate (  t0(1:im,1:jm,1:km) )
          allocate ( dp0(1:im,1:jm,1:km) )

          call read_data (fname, 'ak', ak0)
          call read_data (fname, 'bk', bk0)
          call read_data (fname, 'Surface_geopotential', gz0)
          call read_data (fname, 'U',     u0)
          call read_data (fname, 'V',     v0)
          call read_data (fname, 'T',     t0)
          call read_data (fname, 'DELP', dp0)

! Share the load
          if(is_master()) call pmaxmin( 'ZS_data', gz0, im,    jm, 1./grav)
          if(mpp_pe()==1) call pmaxmin( 'U_data',   u0, im*jm, km, 1.)
          if(mpp_pe()==1) call pmaxmin( 'V_data',   v0, im*jm, km, 1.)
          if(mpp_pe()==2) call pmaxmin( 'T_data',   t0, im*jm, km, 1.)
          if(mpp_pe()==3) call pmaxmin( 'DEL-P',   dp0, im*jm, km, 0.01)


      else
          call mpp_error(FATAL,'==> Error in get_external_ic: Expected file '//trim(fname)//' for dynamics does not exist')
      endif

! Read in tracers: only AM2 "physics tracers" at this point
      fname = Atm(1)%flagstruct%res_latlon_tracers

      if( file_exist(fname) ) then
          if(is_master()) write(*,*) 'Using lat-lon tracer restart:', fname 

          allocate ( q0(im,jm,km,Atm(1)%ncnst) )
          q0 = 0.

          do tr_ind = 1, nq
            call get_tracer_names(MODEL_ATMOS, tr_ind, tracer_name)
            if (field_exist(fname,tracer_name)) then
               call read_data(fname, tracer_name, q0(1:im,1:jm,1:km,tr_ind))
               call mpp_error(NOTE,'==>  Have read tracer '//trim(tracer_name)//' from '//trim(fname))
               cycle
            endif
          enddo
      else
          call mpp_error(FATAL,'==> Error in get_external_ic: Expected file '//trim(fname)//' for tracers does not exist')
      endif

! D to A transform on lat-lon grid:
      allocate (  ua(im,jm,km) )
      allocate (  va(im,jm,km) )

      call d2a3d(u0, v0,  ua,  va, im, jm, km, lon)

      deallocate ( u0 ) 
      deallocate ( v0 ) 

      if(mpp_pe()==4) call pmaxmin( 'UA', ua, im*jm, km, 1.)
      if(mpp_pe()==4) call pmaxmin( 'VA', va, im*jm, km, 1.)

      do j=1,jm
         do i=1,im
            ps0(i,j) = ak0(1)
         enddo
      enddo

      do k=1,km
         do j=1,jm
            do i=1,im
               ps0(i,j) = ps0(i,j) + dp0(i,j,k)
            enddo
         enddo
      enddo

  if (is_master()) call pmaxmin( 'PS_data (mb)', ps0, im, jm, 0.01)

! Horizontal interpolation to the cubed sphere grid center
! remap vertically with terrain adjustment

      call remap_xyz( im, 1, jm, jm, km, npz, nq, Atm(1)%ncnst, lon, lat, ak0, bk0,   &
                      ps0,  gz0, ua, va, t0, q0, Atm(1) )

      deallocate ( ak0 ) 
      deallocate ( bk0 ) 
      deallocate ( ps0 ) 
      deallocate ( gz0 ) 
      deallocate ( t0 ) 
      deallocate ( q0 ) 
      deallocate ( dp0 ) 
      deallocate ( ua ) 
      deallocate ( va ) 
      deallocate ( lat ) 
      deallocate ( lon ) 

  end subroutine get_fv_ic


#ifndef DYCORE_SOLO
 subroutine ncep2fms(im, jm, lon, lat, wk)

  integer, intent(in):: im, jm
  real,    intent(in):: lon(im), lat(jm)
  real(kind=4),    intent(in):: wk(im,jm)
! local:
  real :: rdlon(im)
  real :: rdlat(jm)
  real:: a1, b1
  real:: delx, dely
  real:: xc, yc    ! "data" location
  real:: c1, c2, c3, c4
  integer i,j, i1, i2, jc, i0, j0, it, jt

  do i=1,im-1
     rdlon(i) = 1. / (lon(i+1) - lon(i))
  enddo
     rdlon(im) = 1. / (lon(1) + 2.*pi - lon(im))

  do j=1,jm-1
     rdlat(j) = 1. / (lat(j+1) - lat(j))
  enddo

! * Interpolate to "FMS" 1x1 SST data grid
! lon: 0.5, 1.5, ..., 359.5
! lat: -89.5, -88.5, ... , 88.5, 89.5

  delx = 360./real(i_sst)
  dely = 180./real(j_sst)

  jt = 1
  do 5000 j=1,j_sst

     yc = (-90. + dely * (0.5+real(j-1)))  * deg2rad
     if ( yc<lat(1) ) then
            jc = 1
            b1 = 0.
     elseif ( yc>lat(jm) ) then
            jc = jm-1
            b1 = 1.
     else
          do j0=jt,jm-1
          if ( yc>=lat(j0) .and. yc<=lat(j0+1) ) then
               jc = j0
               jt = j0
               b1 = (yc-lat(jc)) * rdlat(jc)
               go to 222
          endif
          enddo
     endif
222  continue
     it = 1

     do i=1,i_sst
        xc = delx * (0.5+real(i-1)) * deg2rad
       if ( xc>lon(im) ) then
            i1 = im;     i2 = 1
            a1 = (xc-lon(im)) * rdlon(im)
       elseif ( xc<lon(1) ) then
            i1 = im;     i2 = 1
            a1 = (xc+2.*pi-lon(im)) * rdlon(im)
       else
            do i0=it,im-1
            if ( xc>=lon(i0) .and. xc<=lon(i0+1) ) then
               i1 = i0;  i2 = i0+1
               it = i0
               a1 = (xc-lon(i1)) * rdlon(i0)
               go to 111
            endif
            enddo
       endif
111    continue

       if ( a1<0.0 .or. a1>1.0 .or.  b1<0.0 .or. b1>1.0 ) then
            write(*,*) 'gid=', mpp_pe(), i,j,a1, b1
       endif

       c1 = (1.-a1) * (1.-b1)
       c2 =     a1  * (1.-b1)
       c3 =     a1  *     b1
       c4 = (1.-a1) *     b1
! Interpolated surface pressure
       sst_ncep(i,j) = c1*wk(i1,jc  ) + c2*wk(i2,jc  ) +    &
                       c3*wk(i2,jc+1) + c4*wk(i1,jc+1)
     enddo   !i-loop
5000 continue   ! j-loop

 end subroutine ncep2fms
#endif



 subroutine remap_coef( im, jm, lon, lat, id1, id2, jdc, s2c, agrid, bd )

   type(fv_grid_bounds_type), intent(IN) :: bd
  integer, intent(in):: im, jm
  real,    intent(in):: lon(im), lat(jm)
  real,    intent(out):: s2c(bd%is:bd%ie,bd%js:bd%je,4)
  integer, intent(out), dimension(bd%is:bd%ie,bd%js:bd%je):: id1, id2, jdc
  real,    intent(in):: agrid(bd%isd:bd%ied,bd%jsd:bd%jed,2)
! local:
  real :: rdlon(im)
  real :: rdlat(jm)
  real:: a1, b1
  integer i,j, i1, i2, jc, i0, j0

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

  do i=1,im-1
     rdlon(i) = 1. / (lon(i+1) - lon(i))
  enddo
     rdlon(im) = 1. / (lon(1) + 2.*pi - lon(im))

  do j=1,jm-1
     rdlat(j) = 1. / (lat(j+1) - lat(j))
  enddo

! * Interpolate to cubed sphere cell center
  do 5000 j=js,je

     do i=is,ie

       if ( agrid(i,j,1)>lon(im) ) then
            i1 = im;     i2 = 1
            a1 = (agrid(i,j,1)-lon(im)) * rdlon(im)
       elseif ( agrid(i,j,1)<lon(1) ) then
            i1 = im;     i2 = 1
            a1 = (agrid(i,j,1)+2.*pi-lon(im)) * rdlon(im)
       else
            do i0=1,im-1
            if ( agrid(i,j,1)>=lon(i0) .and. agrid(i,j,1)<=lon(i0+1) ) then
               i1 = i0;  i2 = i0+1
               a1 = (agrid(i,j,1)-lon(i1)) * rdlon(i0)
               go to 111
            endif
            enddo
       endif
111    continue

       if ( agrid(i,j,2)<lat(1) ) then
            jc = 1
            b1 = 0.
       elseif ( agrid(i,j,2)>lat(jm) ) then
            jc = jm-1
            b1 = 1.
       else
          do j0=1,jm-1
          if ( agrid(i,j,2)>=lat(j0) .and. agrid(i,j,2)<=lat(j0+1) ) then
               jc = j0
               b1 = (agrid(i,j,2)-lat(jc)) * rdlat(jc)
               go to 222
          endif
          enddo
       endif
222    continue

       if ( a1<0.0 .or. a1>1.0 .or.  b1<0.0 .or. b1>1.0 ) then
            write(*,*) 'gid=', mpp_pe(), i,j,a1, b1
       endif

       s2c(i,j,1) = (1.-a1) * (1.-b1)
       s2c(i,j,2) =     a1  * (1.-b1)
       s2c(i,j,3) =     a1  *     b1
       s2c(i,j,4) = (1.-a1) *     b1
       id1(i,j) = i1
       id2(i,j) = i2
       jdc(i,j) = jc
     enddo   !i-loop
5000 continue   ! j-loop

 end subroutine remap_coef


 subroutine remap_scalar(im, jm, km, npz, nq, ncnst, ak0, bk0, psc, gzc, ta, qa, Atm)
  type(fv_atmos_type), intent(inout) :: Atm
  integer, intent(in):: im, jm, km, npz, nq, ncnst
  real,    intent(in):: ak0(km+1), bk0(km+1)
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je):: psc, gzc
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km):: ta
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km,ncnst):: qa
! local:
  real, dimension(Atm%bd%is:Atm%bd%ie,km):: tp
  real, dimension(Atm%bd%is:Atm%bd%ie,km+1):: pe0, pn0
  real, dimension(Atm%bd%is:Atm%bd%ie,npz):: qn1
  real, dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pe1, pn1
  real pt0(km), gz(km+1), pk0(km+1)
  real qp(Atm%bd%is:Atm%bd%ie,km,ncnst)
  real p1, p2, alpha, rdg
  real(kind=R_GRID):: pst
  integer i,j,k, iq
  integer  sphum, o3mr, clwmr
  integer :: is,  ie,  js,  je
  integer :: isd, ied, jsd, jed

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je
  isd = Atm%bd%isd
  ied = Atm%bd%ied
  jsd = Atm%bd%jsd
  jed = Atm%bd%jed


  sphum   = get_tracer_index(MODEL_ATMOS, 'sphum')
  clwmr   = get_tracer_index(MODEL_ATMOS, 'liq_wat')
  o3mr    = get_tracer_index(MODEL_ATMOS, 'o3mr')

  if (mpp_pe()==1) then
    print *, 'sphum = ', sphum
    print *, 'clwmr = ', clwmr
    print *, ' o3mr = ', o3mr
    print *, 'ncnst = ', ncnst
  endif

  if ( sphum/=1 ) then
       call mpp_error(FATAL,'SPHUM must be 1st tracer')
  endif

!rab  if ( o3mr/=ncnst ) then
!rab       call mpp_error(FATAL,'number of q species is not match')
!rab  endif

  do 5000 j=js,je

     do i=is,ie

       do iq=1,ncnst
          do k=1,km
             qp(i,k,iq) = qa(i,j,k,iq)
          enddo
       enddo

    if ( T_is_Tv ) then
! The "T" field in NCEP analysis is actually virtual temperature (Larry H. post processing)
! BEFORE 20051201
       do k=1,km
          tp(i,k) = ta(i,j,k)
       enddo
    else
       do k=1,km
          tp(i,k) = ta(i,j,k)*(1.+zvir*qp(i,k,sphum))
       enddo
    endif
! Tracers:

       do k=1,km+1
          pe0(i,k) = ak0(k) + bk0(k)*psc(i,j)
          pn0(i,k) = log(pe0(i,k))
            pk0(k) = pe0(i,k)**kappa
       enddo

#ifdef USE_DATA_ZS
       Atm%  ps(i,j) = psc(i,j)
       Atm%phis(i,j) = gzc(i,j)
#else

! * Adjust interpolated ps to model terrain
       gz(km+1) = gzc(i,j)
       do k=km,1,-1
           gz(k) = gz(k+1) + rdgas*tp(i,k)*(pn0(i,k+1)-pn0(i,k))
       enddo
! Only lowest layer potential temp is needed
          pt0(km) = tp(i,km)/(pk0(km+1)-pk0(km))*(kappa*(pn0(i,km+1)-pn0(i,km)))
       if( Atm%phis(i,j)>gzc(i,j) ) then
           do k=km,1,-1
              if( Atm%phis(i,j) <  gz(k)  .and.    &
                  Atm%phis(i,j) >= gz(k+1) ) then
                  pst = pk0(k) + (pk0(k+1)-pk0(k))*(gz(k)-Atm%phis(i,j))/(gz(k)-gz(k+1))
                  go to 123
              endif
           enddo
       else
! Extrapolation into the ground
           pst = pk0(km+1) + (gzc(i,j)-Atm%phis(i,j))/(cp_air*pt0(km))
       endif

123    Atm%ps(i,j) = pst**(1./kappa)
#endif
     enddo   !i-loop


     do i=is,ie
        pe1(i,1) = Atm%ak(1)
        pn1(i,1) = log(pe1(i,1))
     enddo
     do k=2,npz+1
       do i=is,ie
          pe1(i,k) = Atm%ak(k) + Atm%bk(k)*Atm%ps(i,j)
          pn1(i,k) = log(pe1(i,k))
       enddo
     enddo

! * Compute delp
     do k=1,npz
        do i=is,ie
           Atm%delp(i,j,k) = pe1(i,k+1) - pe1(i,k)
        enddo
     enddo

!---------------
! map shpum, o3mr, clwmr tracers
!----------------
      do iq=1,ncnst
         call mappm(km, pe0, qp(is,1,iq), npz, pe1,  qn1, is,ie, 0, 11, Atm%ptop)
         if (iq==sphum .and. Atm%flagstruct%ncep_ic ) then
             p1 = 200.E2
             p2 =  75.E2
! Blend model sphum with NCEP data
             do k=1,npz
                do i=is,ie
                   pst = 0.5*(pe1(i,k)+pe1(i,k+1))
                   if ( pst > p1 ) then
                        Atm%q(i,j,k,iq) = qn1(i,k)
                   elseif( pst > p2 ) then            ! p2 < pst < p1
                        alpha = (pst-p2)/(p1-p2)
                        Atm%q(i,j,k,1) = qn1(i,k)*alpha + Atm%q(i,j,k,1)*(1.-alpha)
                   endif
                enddo
             enddo
         else
             do k=1,npz
                do i=is,ie
                   Atm%q(i,j,k,iq) = qn1(i,k)
                enddo
             enddo
         endif
      enddo

!-------------------------------------------------------------
! map virtual temperature using geopotential conserving scheme.
!-------------------------------------------------------------
      call mappm(km, pn0, tp, npz, pn1, qn1, is,ie, 1, 9, Atm%ptop)
      do k=1,npz
         do i=is,ie
            Atm%pt(i,j,k) = qn1(i,k)/(1.+zvir*Atm%q(i,j,k,sphum))
         enddo
      enddo

      if ( .not. Atm%flagstruct%hydrostatic .and. Atm%flagstruct%ncep_ic ) then
! Replace delz with NCEP hydrostatic state
         rdg = -rdgas / grav
         do k=1,npz
            do i=is,ie
               atm%delz(i,j,k) = rdg*qn1(i,k)*(pn1(i,k+1)-pn1(i,k))
            enddo
         enddo
      endif

5000 continue

  call prt_maxmin('PS_model', Atm%ps, is, ie, js, je, ng, 1, 0.01)

  if (is_master()) write(*,*) 'done remap_scalar'

 end subroutine remap_scalar


 subroutine remap_scalar_nggps(Atm, km, npz, ncnst, ak0, bk0, psc, qa, omga, zh)
  type(fv_atmos_type), intent(inout) :: Atm
  integer, intent(in):: km, npz, ncnst
  real,    intent(in):: ak0(km+1), bk0(km+1)
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je):: psc
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km):: omga
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km,ncnst):: qa
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km+1):: zh
! local:
  real, dimension(Atm%bd%is:Atm%bd%ie,km+1):: pe0
  real, dimension(Atm%bd%is:Atm%bd%ie,npz):: qn1, dp2
  real, dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pe1
  real qp(Atm%bd%is:Atm%bd%ie,km)
  real wk(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)
!!! High-precision
  real(kind=R_GRID), dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pn1
  real(kind=R_GRID):: gz_fv(npz+1)
  real(kind=R_GRID), dimension(2*km+1):: gz, pn
  real(kind=R_GRID), dimension(Atm%bd%is:Atm%bd%ie,km+1):: pn0
  real(kind=R_GRID):: pst
!!! High-precision
  integer i,j,k, k2, l, iq
  integer  sphum, o3mr, clwmr
  integer :: is,  ie,  js,  je

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je

  sphum   = get_tracer_index(MODEL_ATMOS, 'sphum')
  clwmr   = get_tracer_index(MODEL_ATMOS, 'liq_wat')
  o3mr    = get_tracer_index(MODEL_ATMOS, 'o3mr')

  k2 = max(10, km/2)

  if (mpp_pe()==1) then
    print *, 'sphum = ', sphum
    print *, 'clwmr = ', clwmr
    print *, ' o3mr = ', o3mr
    print *, 'ncnst = ', ncnst
  endif

  if ( sphum/=1 ) then
       call mpp_error(FATAL,'SPHUM must be 1st tracer')
  endif

  do 5000 j=js,je
     do k=1,km+1
        do i=is,ie
           pe0(i,k) = ak0(k) + bk0(k)*psc(i,j)
           pn0(i,k) = log(pe0(i,k))
        enddo
     enddo

     do i=is,ie
        do k=1,km+1
           pn(k) = pn0(i,k)
           gz(k) = zh(i,j,k)*grav
        enddo
! Use log-p for interpolation/extrapolation
! mirror image method:
        do k=km+2, km+k2
              l = 2*(km+1) - k
           gz(k) = 2.*gz(km+1) - gz(l)
           pn(k) = 2.*pn(km+1) - pn(l)
        enddo

        do k=km+k2-1, 2, -1
          if( Atm%phis(i,j).le.gz(k) .and. Atm%phis(i,j).ge.gz(k+1) ) then
              pst = pn(k) + (pn(k+1)-pn(k))*(gz(k)-Atm%phis(i,j))/(gz(k)-gz(k+1))
              go to 123
          endif
        enddo
123     Atm%ps(i,j) = exp(pst)
     enddo   ! i-loop

     do i=is,ie
        pe1(i,1) = Atm%ak(1)
        pn1(i,1) = log(pe1(i,1))
     enddo
     do k=2,npz+1
       do i=is,ie
          pe1(i,k) = Atm%ak(k) + Atm%bk(k)*Atm%ps(i,j)
          pn1(i,k) = log(pe1(i,k))
       enddo
     enddo

! * Compute delp
     do k=1,npz
        do i=is,ie
           dp2(i,k) = pe1(i,k+1) - pe1(i,k)
           Atm%delp(i,j,k) = dp2(i,k)
        enddo
     enddo

! map shpum, o3mr, clwmr tracers
      do iq=1,ncnst
         do k=1,km
            do i=is,ie
               qp(i,k) = qa(i,j,k,iq)
            enddo
         enddo
         call mappm(km, pe0, qp, npz, pe1,  qn1, is,ie, 0, 8, Atm%ptop)
         if ( iq==1 ) then
            call fillq(ie-is+1, npz, 1, qn1, dp2)
         else
            call fillz(ie-is+1, npz, 1, qn1, dp2)
         endif
! The HiRam step of blending model sphum with NCEP data is obsolete because nggps is always cold starting...
         do k=1,npz
            do i=is,ie
               Atm%q(i,j,k,iq) = qn1(i,k)
            enddo
         enddo
      enddo

!---------------------------------------------------
! Retrive temperature using GFS geopotential height
!---------------------------------------------------
   do i=is,ie
! Make sure FV3 top is lower than GFS; can not do extrapolation above the top at this point
      if ( pn1(i,1) .lt. pn0(i,1) ) then
           call mpp_error(FATAL,'FV3 top higher than NCEP/GFS')
      endif

      do k=1,km+1
         gz(k) = zh(i,j,k)*grav
      enddo
      gz_fv(npz+1) = Atm%phis(i,j)

      do 555 k=1,npz
! Searching using FV3 log(pe): pn1
         do l=1,km
            if ( (pn1(i,k).le.pn0(i,l+1)) .and. (pn1(i,k).ge.pn0(i,l)) ) then
                gz_fv(k) = gz(l) + (gz(l+1)-gz(l))*(pn1(i,k)-pn0(i,l))/(pn0(i,l+1)-pn0(i,l))
                goto 555
            elseif ( pn1(i,k) .gt. pn0(i,km+1) ) then
! Isothermal under ground; linear in log-p extra-polation
                gz_fv(k) = gz(km+1) + (gz_fv(npz+1)-gz(km+1))*(pn1(i,k)-pn0(i,km+1))/(pn1(i,npz+1)-pn0(i,km+1))
                goto 555
            endif
         enddo
555   continue

! Compute true temperature using hydrostatic balance
      do k=1,npz
         Atm%pt(i,j,k) = (gz_fv(k)-gz_fv(k+1))/( rdgas*(pn1(i,k+1)-pn1(i,k))*(1.+zvir*Atm%q(i,j,k,sphum)) )
      enddo
      if ( .not. Atm%flagstruct%hydrostatic ) then
         do k=1,npz
            Atm%delz(i,j,k) = (gz_fv(k+1) - gz_fv(k)) / grav
         enddo
      endif

   enddo   ! i-loop

!-------------------------------------------------------------
! map omega
!------- ------------------------------------------------------
   if ( .not. Atm%flagstruct%hydrostatic ) then
      do k=1,km
         do i=is,ie
            qp(i,k) = omga(i,j,k)
         enddo
      enddo
      call mappm(km, pe0, qp, npz, pe1, qn1, is,ie, -1, 4, Atm%ptop)
      do k=1,npz
         do i=is,ie
            atm%w(i,j,k) = qn1(i,k)/atm%delp(i,j,k)*atm%delz(i,j,k)
         enddo
      enddo
   endif

5000 continue

! Add some diagnostics:
  call p_maxmin('PS_model (mb)', Atm%ps(is:ie,js:je), is, ie, js, je, 1, 0.01)
  call p_maxmin('PT_model', Atm%pt(is:ie,js:je,1:npz), is, ie, js, je, npz, 1.)
  do j=js,je
     do i=is,ie
        wk(i,j) = Atm%phis(i,j)/grav - zh(i,j,km+1)
     enddo
  enddo
  call pmaxmn('ZS_diff (m)', wk, is, ie, js, je, 1, 1., Atm%gridstruct%area_64, Atm%domain)

  do j=js,je
     do i=is,ie
        wk(i,j) = Atm%ps(i,j) - psc(i,j)
     enddo
  enddo
  call pmaxmn('PS_diff (mb)', wk, is, ie, js, je, 1, 0.01, Atm%gridstruct%area_64, Atm%domain)

  if (is_master()) write(*,*) 'done remap_scalar_nggps'

 end subroutine remap_scalar_nggps


 subroutine get_pt_wdz( Atm, npz, zh) 
  type(fv_atmos_type), intent(inout) :: Atm
  integer, intent(in):: npz
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,npz+2):: zh
! Local:
  real, dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pe0, pn0
  integer i,j,k, kshift
  integer :: is,  ie,  js,  je
  real:: grd

  grd = grav/rdgas

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je

  kshift = 1

! Needs openMP here
  do j = js, je

     do i=is,ie
        pe0(i,1) = atm%ak(1)
        pn0(i,1) = log(pe0(i,1))
     enddo

     do k=2,npz+1
        do i=is,ie
           pe0(i,k) = atm%ak(k) + atm%bk(k)*atm%ps(i,j)
           pn0(i,k) = log(pe0(i,k))
        enddo
     enddo

! Retrieve temp from height at edges:
! Approximate w from Omega
     do k= 1, npz
        if ( atm%flagstruct%hydrostatic )then
          do i = is, ie
             Atm%pt(i,j,k) = grd*(zh(i,j,k+kshift)-zh(i,j,k+kshift+1))/((pn0(i,k+1)-pn0(i,k))*(1.+zvir*atm%q(i,j,k,1)))
          enddo
        else
          do i = is, ie
           atm%delz(i,j,k) = zh(i,j,k+kshift+1) - zh(i,j,k+kshift)
             atm%pt(i,j,k) = grd*atm%delz(i,j,k)/((pn0(i,k)-pn0(i,k+1))*(1.+zvir*atm%q(i,j,k,1)))
              atm%w(i,j,k) = atm%omga(i,j,k)*atm%delz(i,j,k) / (pe0(i,k+1)-pe0(i,k))
          enddo
        endif
     enddo

  enddo

  call p_maxmin('T_ic', Atm%pt(is:ie,js:je,1:npz), is, ie, js, je, npz, 1.)

  if ( .not. atm%flagstruct%hydrostatic )   &
  call p_maxmin('W_ic', Atm%w(is:ie,js:je,1:npz),  is, ie, js, je, npz, 1.)

  end subroutine get_pt_wdz


 subroutine remap_dwinds(km, npz, ak0, bk0, psc, ud, vd, Atm)
  type(fv_atmos_type), intent(inout) :: Atm
  integer, intent(in):: km, npz
  real,    intent(in):: ak0(km+1), bk0(km+1)
  real,    intent(in):: psc(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)
  real,    intent(in)::  ud(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je+1,km)
  real,    intent(in)::  vd(Atm%bd%is:Atm%bd%ie+1,Atm%bd%js:Atm%bd%je,km)
! local:
  real, dimension(Atm%bd%isd:Atm%bd%ied,Atm%bd%jsd:Atm%bd%jed):: psd
  real, dimension(Atm%bd%is:Atm%bd%ie+1, km+1):: pe0
  real, dimension(Atm%bd%is:Atm%bd%ie+1,npz+1):: pe1
  real, dimension(Atm%bd%is:Atm%bd%ie+1,npz):: qn1
  integer i,j,k
  integer :: is,  ie,  js,  je
  integer :: isd, ied, jsd, jed

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je
  isd = Atm%bd%isd
  ied = Atm%bd%ied
  jsd = Atm%bd%jsd
  jed = Atm%bd%jed

!RAB - NEED TO FIX PSD & PS FOR IN HALO REGION FOR NESTED GRIDS
  if (Atm%neststruct%nested) then
     do j=jsd,jed
     do i=isd,ied
        psd(i,j) = Atm%ps(i,j)
     enddo
     enddo
  else
     do j=js,je
        do i=is,ie
           psd(i,j) = psc(i,j)
        enddo
     enddo
  endif
  call mpp_update_domains( psd,    Atm%domain, complete=.false. )
  call mpp_update_domains( Atm%ps, Atm%domain, complete=.true. )

  do 5000 j=js,je+1
!------
! map u
!------
     do k=1,km+1
        do i=is,ie
           pe0(i,k) = ak0(k) + bk0(k)*0.5*(psd(i,j-1)+psd(i,j))
        enddo
     enddo
     do k=1,npz+1
        do i=is,ie
           pe1(i,k) = Atm%ak(k) + Atm%bk(k)*0.5*(Atm%ps(i,j-1)+Atm%ps(i,j))
        enddo
     enddo
     call mappm(km, pe0(is:ie,1:km+1), ud(is:ie,j,1:km), npz, pe1(is:ie,1:npz+1),   &
                qn1(is:ie,1:npz), is,ie, -1, 8, Atm%ptop)
     do k=1,npz
        do i=is,ie
           Atm%u(i,j,k) = qn1(i,k)
        enddo
     enddo
!------
! map v
!------
     if ( j/=(je+1) ) then

     do k=1,km+1
        do i=is,ie+1
           pe0(i,k) = ak0(k) + bk0(k)*0.5*(psd(i-1,j)+psd(i,j))
        enddo
     enddo
     do k=1,npz+1
        do i=is,ie+1
           pe1(i,k) = Atm%ak(k) + Atm%bk(k)*0.5*(Atm%ps(i-1,j)+Atm%ps(i,j))
        enddo
     enddo
     call mappm(km, pe0(is:ie+1,1:km+1), vd(is:ie+1,j,1:km), npz, pe1(is:ie+1,1:npz+1),  &
                qn1(is:ie+1,1:npz), is,ie+1, -1, 8, Atm%ptop)
     do k=1,npz
        do i=is,ie+1
           Atm%v(i,j,k) = qn1(i,k)
        enddo
     enddo

     endif

5000 continue

  if (is_master()) write(*,*) 'done remap_dwinds'

 end subroutine remap_dwinds


 subroutine remap_winds(im, jm, km, npz, ak0, bk0, psc, ua, va, Atm)
  type(fv_atmos_type), intent(inout) :: Atm
  integer, intent(in):: im, jm, km, npz
  real,    intent(in):: ak0(km+1), bk0(km+1)
  real,    intent(in):: psc(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km):: ua, va
! local:
  real, dimension(Atm%bd%isd:Atm%bd%ied,Atm%bd%jsd:Atm%bd%jed,npz):: ut, vt   ! winds
  real, dimension(Atm%bd%is:Atm%bd%ie, km+1):: pe0
  real, dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pe1
  real, dimension(Atm%bd%is:Atm%bd%ie,npz):: qn1
  integer i,j,k

  integer :: is,  ie,  js,  je
  integer :: isd, ied, jsd, jed

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je
  isd = Atm%bd%isd
  ied = Atm%bd%ied
  jsd = Atm%bd%jsd
  jed = Atm%bd%jed

  do 5000 j=js,je

     do k=1,km+1
        do i=is,ie
           pe0(i,k) = ak0(k) + bk0(k)*psc(i,j)
        enddo
     enddo

     do k=1,npz+1
       do i=is,ie
          pe1(i,k) = Atm%ak(k) + Atm%bk(k)*Atm%ps(i,j)
       enddo
     enddo

!------
! map u
!------
      call mappm(km, pe0, ua(is:ie,j,1:km), npz, pe1, qn1, is,ie, -1, 4, Atm%ptop)
      do k=1,npz
         do i=is,ie
            ut(i,j,k) = qn1(i,k)
         enddo
      enddo
!------
! map v
!------
      call mappm(km, pe0, va(is:ie,j,1:km), npz, pe1, qn1, is,ie, -1, 4, Atm%ptop)
      do k=1,npz
         do i=is,ie
            vt(i,j,k) = qn1(i,k)
         enddo
      enddo

5000 continue

  call prt_maxmin('UT', ut, is, ie, js, je, ng, npz, 1.)
  call prt_maxmin('VT', vt, is, ie, js, je, ng, npz, 1.)
  call prt_maxmin('UA_top',ut(:,:,1), is, ie, js, je, ng, 1, 1.)

!----------------------------------------------
! winds: lat-lon ON A to Cubed-D transformation:
!----------------------------------------------
  call cubed_a2d(Atm%npx, Atm%npy, npz, ut, vt, Atm%u, Atm%v, Atm%gridstruct, Atm%domain, Atm%bd )

  if (is_master()) write(*,*) 'done remap_winds'

 end subroutine remap_winds


 subroutine remap_wz(im, jm, km, npz, mg, ak0, bk0, psc, wa, wz, Atm)
  type(fv_atmos_type), intent(inout) :: Atm
  integer, intent(in):: im, jm, km, npz
  integer, intent(in):: mg     ! mg = 0 for delz; mg=3 for w
  real,    intent(in):: ak0(km+1), bk0(km+1)
  real,    intent(in):: psc(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je)
  real,    intent(in), dimension(Atm%bd%is:Atm%bd%ie,Atm%bd%js:Atm%bd%je,km):: wa
  real,   intent(out):: wz(Atm%bd%is-mg:Atm%bd%ie+mg,Atm%bd%js-mg:Atm%bd%je+mg,npz)
! local:
  real, dimension(Atm%bd%is:Atm%bd%ie, km+1):: pe0
  real, dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pe1
  real, dimension(Atm%bd%is:Atm%bd%ie,npz):: qn1
  integer i,j,k
  integer :: is,  ie,  js,  je
  integer :: isd, ied, jsd, jed

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je
  isd = Atm%bd%isd
  ied = Atm%bd%ied
  jsd = Atm%bd%jsd
  jed = Atm%bd%jed

  do 5000 j=js,je

     do k=1,km+1
        do i=is,ie
           pe0(i,k) = ak0(k) + bk0(k)*psc(i,j)
        enddo
     enddo

     do k=1,npz+1
       do i=is,ie
          pe1(i,k) = Atm%ak(k) + Atm%bk(k)*Atm%ps(i,j)
       enddo
     enddo

!------
! map w
!------
      call mappm(km, pe0, wa(is:ie,j,1:km), npz, pe1, qn1, is,ie, -1, 4, Atm%ptop)
      do k=1,npz
         do i=is,ie
            wz(i,j,k) = qn1(i,k)
         enddo
      enddo

5000 continue

! call prt_maxmin('WZ', wz, is, ie, js, je, mg, npz, 1.)
! if (is_master()) write(*,*) 'done remap_wz'

 end subroutine remap_wz



  subroutine remap_xyz( im, jbeg, jend, jm, km, npz, nq, ncnst, lon, lat, ak0, bk0, ps0, gz0,   &
                        ua, va, ta, qa, Atm )

  type(fv_atmos_type), intent(inout), target :: Atm
  integer, intent(in):: im, jm, km, npz, nq, ncnst
  integer, intent(in):: jbeg, jend
  real,    intent(in):: lon(im), lat(jm), ak0(km+1), bk0(km+1)
  real,    intent(in):: gz0(im,jbeg:jend), ps0(im,jbeg:jend)
  real,    intent(in), dimension(im,jbeg:jend,km):: ua, va, ta
  real,    intent(in), dimension(im,jbeg:jend,km,ncnst):: qa

  real, pointer, dimension(:,:,:) :: agrid

! local:
  real, dimension(Atm%bd%isd:Atm%bd%ied,Atm%bd%jsd:Atm%bd%jed,npz):: ut, vt   ! winds 
  real, dimension(Atm%bd%is:Atm%bd%ie,km):: up, vp, tp
  real, dimension(Atm%bd%is:Atm%bd%ie,km+1):: pe0, pn0
  real pt0(km), gz(km+1), pk0(km+1)
  real qp(Atm%bd%is:Atm%bd%ie,km,ncnst)
  real, dimension(Atm%bd%is:Atm%bd%ie,npz):: qn1
  real, dimension(Atm%bd%is:Atm%bd%ie,npz+1):: pe1, pn1
  real :: rdlon(im)
  real :: rdlat(jm)
  real:: a1, b1, c1, c2, c3, c4
  real:: gzc, psc, pst
  integer i,j,k, i1, i2, jc, i0, j0, iq
! integer  sphum, liq_wat, ice_wat, cld_amt
  integer  sphum
  integer :: is,  ie,  js,  je
  integer :: isd, ied, jsd, jed

  is  = Atm%bd%is
  ie  = Atm%bd%ie
  js  = Atm%bd%js
  je  = Atm%bd%je
  isd = Atm%bd%isd
  ied = Atm%bd%ied
  jsd = Atm%bd%jsd
  jed = Atm%bd%jed

  !!NOTE: Only Atm is used in this routine.
  agrid => Atm%gridstruct%agrid

  sphum   = get_tracer_index(MODEL_ATMOS, 'sphum')
! liq_wat = get_tracer_index(MODEL_ATMOS, 'liq_wat')
! ice_wat = get_tracer_index(MODEL_ATMOS, 'ice_wat')
! cld_amt = get_tracer_index(MODEL_ATMOS, 'cld_amt')

   if ( sphum/=1 ) then
        call mpp_error(FATAL,'SPHUM must be 1st tracer')
   endif

  pk0(1) = ak0(1)**kappa 

  do i=1,im-1
     rdlon(i) = 1. / (lon(i+1) - lon(i))
  enddo
     rdlon(im) = 1. / (lon(1) + 2.*pi - lon(im))

  do j=1,jm-1
     rdlat(j) = 1. / (lat(j+1) - lat(j))
  enddo

! * Interpolate to cubed sphere cell center
  do 5000 j=js,je

     do i=is,ie
        pe0(i,1) = ak0(1)
        pn0(i,1) = log(ak0(1))
     enddo

     do i=is,ie

       if ( agrid(i,j,1)>lon(im) ) then
            i1 = im;     i2 = 1
            a1 = (agrid(i,j,1)-lon(im)) * rdlon(im)
       elseif ( agrid(i,j,1)<lon(1) ) then
            i1 = im;     i2 = 1
            a1 = (agrid(i,j,1)+2.*pi-lon(im)) * rdlon(im)
       else
            do i0=1,im-1
            if ( agrid(i,j,1)>=lon(i0) .and. agrid(i,j,1)<=lon(i0+1) ) then
               i1 = i0;  i2 = i0+1
               a1 = (agrid(i,j,1)-lon(i1)) * rdlon(i0)
               go to 111
            endif
            enddo
       endif

111    continue

       if ( agrid(i,j,2)<lat(1) ) then
            jc = 1
            b1 = 0.
       elseif ( agrid(i,j,2)>lat(jm) ) then
            jc = jm-1
            b1 = 1.
       else
          do j0=1,jm-1
          if ( agrid(i,j,2)>=lat(j0) .and. agrid(i,j,2)<=lat(j0+1) ) then
               jc = j0
               b1 = (agrid(i,j,2)-lat(jc)) * rdlat(jc)
               go to 222
          endif
          enddo
       endif
222    continue

#ifndef DEBUG_REMAP
       if ( a1<0.0 .or. a1>1.0 .or.  b1<0.0 .or. b1>1.0 ) then
            write(*,*) i,j,a1, b1
       endif
#endif
       c1 = (1.-a1) * (1.-b1)
       c2 =     a1  * (1.-b1)
       c3 =     a1  *     b1
       c4 = (1.-a1) *     b1

! Interpolated surface pressure
       psc = c1*ps0(i1,jc  ) + c2*ps0(i2,jc  ) +    &
             c3*ps0(i2,jc+1) + c4*ps0(i1,jc+1)

! Interpolated surface geopotential
       gzc = c1*gz0(i1,jc  ) + c2*gz0(i2,jc  ) +    &
             c3*gz0(i2,jc+1) + c4*gz0(i1,jc+1)

! 3D fields:
       do iq=1,ncnst
!          if ( iq==sphum .or. iq==liq_wat .or. iq==ice_wat .or. iq==cld_amt ) then
          do k=1,km
             qp(i,k,iq) = c1*qa(i1,jc,  k,iq) + c2*qa(i2,jc,  k,iq) +  &
                          c3*qa(i2,jc+1,k,iq) + c4*qa(i1,jc+1,k,iq)
          enddo
!          endif
       enddo

       do k=1,km
          up(i,k) = c1*ua(i1,jc,  k) + c2*ua(i2,jc,  k) +  &
                    c3*ua(i2,jc+1,k) + c4*ua(i1,jc+1,k)
          vp(i,k) = c1*va(i1,jc,  k) + c2*va(i2,jc,  k) +  &
                    c3*va(i2,jc+1,k) + c4*va(i1,jc+1,k)
          tp(i,k) = c1*ta(i1,jc,  k) + c2*ta(i2,jc,  k) +  &
                    c3*ta(i2,jc+1,k) + c4*ta(i1,jc+1,k)
! Virtual effect:
          tp(i,k) = tp(i,k)*(1.+zvir*qp(i,k,sphum))
       enddo
! Tracers:

       do k=2,km+1
          pe0(i,k) = ak0(k) + bk0(k)*psc
          pn0(i,k) = log(pe0(i,k))
          pk0(k) = pe0(i,k)**kappa
       enddo

#ifdef USE_DATA_ZS
       Atm%  ps(i,j) = psc
       Atm%phis(i,j) = gzc
#else

! * Adjust interpolated ps to model terrain
       gz(km+1) = gzc 
       do k=km,1,-1
           gz(k) = gz(k+1) + rdgas*tp(i,k)*(pn0(i,k+1)-pn0(i,k)) 
       enddo
! Only lowest layer potential temp is needed
          pt0(km) = tp(i,km)/(pk0(km+1)-pk0(km))*(kappa*(pn0(i,km+1)-pn0(i,km)))
       if( Atm%phis(i,j)>gzc ) then
           do k=km,1,-1
              if( Atm%phis(i,j) <  gz(k)  .and.    &
                  Atm%phis(i,j) >= gz(k+1) ) then
                  pst = pk0(k) + (pk0(k+1)-pk0(k))*(gz(k)-Atm%phis(i,j))/(gz(k)-gz(k+1))
                  go to 123
              endif
           enddo
       else
! Extrapolation into the ground
           pst = pk0(km+1) + (gzc-Atm%phis(i,j))/(cp_air*pt0(km))
       endif

123    Atm%ps(i,j) = pst**(1./kappa)
#endif
     enddo   !i-loop
 

! * Compute delp from ps
     do i=is,ie
        pe1(i,1) = Atm%ak(1)
        pn1(i,1) = log(pe1(i,1))
     enddo
     do k=2,npz+1
       do i=is,ie
          pe1(i,k) = Atm%ak(k) + Atm%bk(k)*Atm%ps(i,j)
          pn1(i,k) = log(pe1(i,k))
       enddo
     enddo

     do k=1,npz
        do i=is,ie
           Atm%delp(i,j,k) = pe1(i,k+1) - pe1(i,k)
        enddo
     enddo
 
! Use kord=9 for winds; kord=11 for tracers
!------
! map u
!------
      call mappm(km, pe0, up, npz, pe1, qn1, is,ie, -1, 9, Atm%ptop)
      do k=1,npz
         do i=is,ie
            ut(i,j,k) = qn1(i,k)
         enddo
      enddo
!------
! map v
!------
      call mappm(km, pe0, vp, npz, pe1, qn1, is,ie, -1, 9, Atm%ptop)
      do k=1,npz
         do i=is,ie
            vt(i,j,k) = qn1(i,k)
         enddo
      enddo

!---------------
! map tracers
!----------------
      do iq=1,ncnst
! Note: AM2 physics tracers only
!         if ( iq==sphum .or. iq==liq_wat .or. iq==ice_wat .or. iq==cld_amt ) then
         call mappm(km, pe0, qp(is,1,iq), npz, pe1,  qn1, is,ie, 0, 11, Atm%ptop)
         do k=1,npz
            do i=is,ie
               Atm%q(i,j,k,iq) = qn1(i,k)
            enddo
         enddo
!         endif
      enddo

!-------------------------------------------------------------
! map virtual temperature using geopotential conserving scheme.
!-------------------------------------------------------------
      call mappm(km, pn0, tp, npz, pn1, qn1, is,ie, 1, 9, Atm%ptop)
      do k=1,npz
         do i=is,ie
            Atm%pt(i,j,k) = qn1(i,k)/(1.+zvir*Atm%q(i,j,k,sphum))
         enddo
      enddo

5000 continue

  call prt_maxmin('PS_model', Atm%ps, is, ie, js, je, ng, 1, 0.01)
  call prt_maxmin('UT', ut, is, ie, js, je, ng, npz, 1.)
  call prt_maxmin('VT', vt, is, ie, js, je, ng, npz, 1.)

!----------------------------------------------
! winds: lat-lon ON A to Cubed-D transformation:
!----------------------------------------------
  call cubed_a2d(Atm%npx, Atm%npy, npz, ut, vt, Atm%u, Atm%v, Atm%gridstruct, Atm%domain, Atm%bd )

  if (is_master()) write(*,*) 'done remap_xyz'

 end subroutine remap_xyz


 subroutine cubed_a2d( npx, npy, npz, ua, va, u, v, gridstruct, fv_domain, bd )

! Purpose; Transform wind on A grid to D grid

  use mpp_domains_mod,    only: mpp_update_domains

  type(fv_grid_bounds_type), intent(IN) :: bd
  integer, intent(in):: npx, npy, npz
  real, intent(inout), dimension(bd%isd:bd%ied,bd%jsd:bd%jed,npz):: ua, va
  real, intent(out):: u(bd%isd:bd%ied,  bd%jsd:bd%jed+1,npz)
  real, intent(out):: v(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,npz)
  type(fv_grid_type), intent(IN), target :: gridstruct
  type(domain2d), intent(INOUT) :: fv_domain
! local:
  real v3(3,bd%is-1:bd%ie+1,bd%js-1:bd%je+1)
  real ue(3,bd%is-1:bd%ie+1,bd%js:bd%je+1)    ! 3D winds at edges
  real ve(3,bd%is:bd%ie+1,bd%js-1:bd%je+1)    ! 3D winds at edges
  real, dimension(bd%is:bd%ie):: ut1, ut2, ut3
  real, dimension(bd%js:bd%je):: vt1, vt2, vt3
  integer i, j, k, im2, jm2

  real(kind=R_GRID), pointer, dimension(:,:,:)   :: vlon, vlat
  real(kind=R_GRID), pointer, dimension(:)       :: edge_vect_w, edge_vect_e, edge_vect_s, edge_vect_n
  real(kind=R_GRID), pointer, dimension(:,:,:,:) :: ew, es

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

  vlon => gridstruct%vlon
  vlat => gridstruct%vlat

  edge_vect_w => gridstruct%edge_vect_w
  edge_vect_e => gridstruct%edge_vect_e
  edge_vect_s => gridstruct%edge_vect_s
  edge_vect_n => gridstruct%edge_vect_n
  
  ew => gridstruct%ew
  es => gridstruct%es

  call mpp_update_domains(ua, fv_domain, complete=.false.)
  call mpp_update_domains(va, fv_domain, complete=.true.)

    im2 = (npx-1)/2
    jm2 = (npy-1)/2

    do k=1, npz
! Compute 3D wind on A grid
       do j=js-1,je+1
          do i=is-1,ie+1
             v3(1,i,j) = ua(i,j,k)*vlon(i,j,1) + va(i,j,k)*vlat(i,j,1)
             v3(2,i,j) = ua(i,j,k)*vlon(i,j,2) + va(i,j,k)*vlat(i,j,2)
             v3(3,i,j) = ua(i,j,k)*vlon(i,j,3) + va(i,j,k)*vlat(i,j,3)
          enddo
       enddo

! A --> D
! Interpolate to cell edges
       do j=js,je+1
          do i=is-1,ie+1
             ue(1,i,j) = 0.5*(v3(1,i,j-1) + v3(1,i,j))
             ue(2,i,j) = 0.5*(v3(2,i,j-1) + v3(2,i,j))
             ue(3,i,j) = 0.5*(v3(3,i,j-1) + v3(3,i,j))
          enddo
       enddo

       do j=js-1,je+1
          do i=is,ie+1
             ve(1,i,j) = 0.5*(v3(1,i-1,j) + v3(1,i,j))
             ve(2,i,j) = 0.5*(v3(2,i-1,j) + v3(2,i,j))
             ve(3,i,j) = 0.5*(v3(3,i-1,j) + v3(3,i,j))
          enddo
       enddo

! --- E_W edges (for v-wind):
     if (.not. gridstruct%nested) then
     if ( is==1) then
       i = 1
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_w(j)*ve(1,i,j-1)+(1.-edge_vect_w(j))*ve(1,i,j)
             vt2(j) = edge_vect_w(j)*ve(2,i,j-1)+(1.-edge_vect_w(j))*ve(2,i,j)
             vt3(j) = edge_vect_w(j)*ve(3,i,j-1)+(1.-edge_vect_w(j))*ve(3,i,j)
        else
             vt1(j) = edge_vect_w(j)*ve(1,i,j+1)+(1.-edge_vect_w(j))*ve(1,i,j)
             vt2(j) = edge_vect_w(j)*ve(2,i,j+1)+(1.-edge_vect_w(j))*ve(2,i,j)
             vt3(j) = edge_vect_w(j)*ve(3,i,j+1)+(1.-edge_vect_w(j))*ve(3,i,j)
        endif
       enddo
       do j=js,je
          ve(1,i,j) = vt1(j)
          ve(2,i,j) = vt2(j)
          ve(3,i,j) = vt3(j)
       enddo
     endif

     if ( (ie+1)==npx ) then
       i = npx
       do j=js,je
        if ( j>jm2 ) then
             vt1(j) = edge_vect_e(j)*ve(1,i,j-1)+(1.-edge_vect_e(j))*ve(1,i,j)
             vt2(j) = edge_vect_e(j)*ve(2,i,j-1)+(1.-edge_vect_e(j))*ve(2,i,j)
             vt3(j) = edge_vect_e(j)*ve(3,i,j-1)+(1.-edge_vect_e(j))*ve(3,i,j)
        else
             vt1(j) = edge_vect_e(j)*ve(1,i,j+1)+(1.-edge_vect_e(j))*ve(1,i,j)
             vt2(j) = edge_vect_e(j)*ve(2,i,j+1)+(1.-edge_vect_e(j))*ve(2,i,j)
             vt3(j) = edge_vect_e(j)*ve(3,i,j+1)+(1.-edge_vect_e(j))*ve(3,i,j)
        endif
       enddo
       do j=js,je
          ve(1,i,j) = vt1(j)
          ve(2,i,j) = vt2(j)
          ve(3,i,j) = vt3(j)
       enddo
     endif

! N-S edges (for u-wind):
     if ( js==1 ) then
       j = 1
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_s(i)*ue(1,i-1,j)+(1.-edge_vect_s(i))*ue(1,i,j)
             ut2(i) = edge_vect_s(i)*ue(2,i-1,j)+(1.-edge_vect_s(i))*ue(2,i,j)
             ut3(i) = edge_vect_s(i)*ue(3,i-1,j)+(1.-edge_vect_s(i))*ue(3,i,j)
        else
             ut1(i) = edge_vect_s(i)*ue(1,i+1,j)+(1.-edge_vect_s(i))*ue(1,i,j)
             ut2(i) = edge_vect_s(i)*ue(2,i+1,j)+(1.-edge_vect_s(i))*ue(2,i,j)
             ut3(i) = edge_vect_s(i)*ue(3,i+1,j)+(1.-edge_vect_s(i))*ue(3,i,j)
        endif
       enddo
       do i=is,ie
          ue(1,i,j) = ut1(i)
          ue(2,i,j) = ut2(i)
          ue(3,i,j) = ut3(i)
       enddo
     endif

     if ( (je+1)==npy ) then
       j = npy
       do i=is,ie
        if ( i>im2 ) then
             ut1(i) = edge_vect_n(i)*ue(1,i-1,j)+(1.-edge_vect_n(i))*ue(1,i,j)
             ut2(i) = edge_vect_n(i)*ue(2,i-1,j)+(1.-edge_vect_n(i))*ue(2,i,j)
             ut3(i) = edge_vect_n(i)*ue(3,i-1,j)+(1.-edge_vect_n(i))*ue(3,i,j)
        else
             ut1(i) = edge_vect_n(i)*ue(1,i+1,j)+(1.-edge_vect_n(i))*ue(1,i,j)
             ut2(i) = edge_vect_n(i)*ue(2,i+1,j)+(1.-edge_vect_n(i))*ue(2,i,j)
             ut3(i) = edge_vect_n(i)*ue(3,i+1,j)+(1.-edge_vect_n(i))*ue(3,i,j)
        endif
       enddo
       do i=is,ie
          ue(1,i,j) = ut1(i)
          ue(2,i,j) = ut2(i)
          ue(3,i,j) = ut3(i)
       enddo
     endif

     endif ! .not. nested

     do j=js,je+1
        do i=is,ie
           u(i,j,k) =  ue(1,i,j)*es(1,i,j,1) +  &
                       ue(2,i,j)*es(2,i,j,1) +  &
                       ue(3,i,j)*es(3,i,j,1)
        enddo
     enddo
     do j=js,je
        do i=is,ie+1
           v(i,j,k) = ve(1,i,j)*ew(1,i,j,2) +  &
                      ve(2,i,j)*ew(2,i,j,2) +  &
                      ve(3,i,j)*ew(3,i,j,2)
        enddo
     enddo
 
   enddo         ! k-loop

 end subroutine cubed_a2d



 subroutine d2a3d(u, v,  ua,   va,  im,  jm, km, lon)
      integer, intent(in):: im, jm, km           ! Dimensions
      real, intent(in ) :: lon(im)
      real, intent(in ), dimension(im,jm,km):: u, v
      real, intent(out), dimension(im,jm,km):: ua, va
! local
      real :: coslon(im),sinlon(im)    ! Sine and cosine in longitude
      integer i, j, k
      integer imh
      real un, vn, us, vs

      integer :: ks, ke

      imh = im/2

      do i=1,im
         sinlon(i) = sin(lon(i))
         coslon(i) = cos(lon(i))
      enddo

      do k=1,km
         do j=2,jm-1
            do i=1,im
               ua(i,j,k) = 0.5*(u(i,j,k) + u(i,j+1,k))
            enddo
         enddo

         do j=2,jm-1
            do i=1,im-1
               va(i,j,k) = 0.5*(v(i,j,k) + v(i+1,j,k))
            enddo
            va(im,j,k) = 0.5*(v(im,j,k) + v(1,j,k))
         enddo

! Projection at SP
             us = 0.
             vs = 0.
             do i=1,imh
                us = us + (ua(i+imh,2,k)-ua(i,2,k))*sinlon(i)      &
                     + (va(i,2,k)-va(i+imh,2,k))*coslon(i)
                vs = vs + (ua(i+imh,2,k)-ua(i,2,k))*coslon(i)      &
                     + (va(i+imh,2,k)-va(i,2,k))*sinlon(i)
             enddo
             us = us/im
             vs = vs/im
             do i=1,imh
                ua(i,1,k)   = -us*sinlon(i) - vs*coslon(i)
                va(i,1,k)   =  us*coslon(i) - vs*sinlon(i)
                ua(i+imh,1,k)   = -ua(i,1,k)
                va(i+imh,1,k)   = -va(i,1,k)
             enddo

! Projection at NP
             un = 0.
             vn = 0.
             do i=1,imh
                un = un + (ua(i+imh,jm-1,k)-ua(i,jm-1,k))*sinlon(i)    &
                     + (va(i+imh,jm-1,k)-va(i,jm-1,k))*coslon(i)
                vn = vn + (ua(i,jm-1,k)-ua(i+imh,jm-1,k))*coslon(i)    &
                     + (va(i+imh,jm-1,k)-va(i,jm-1,k))*sinlon(i)
             enddo

             un = un/im
             vn = vn/im
             do i=1,imh
                ua(i,jm,k) = -un*sinlon(i) + vn*coslon(i)
                va(i,jm,k) = -un*coslon(i) - vn*sinlon(i)
                ua(i+imh,jm,k) = -ua(i,jm,k)
                va(i+imh,jm,k) = -va(i,jm,k)
             enddo
      enddo

  end subroutine d2a3d



  subroutine pmaxmin( qname, a, im, jm, fac )

      integer, intent(in):: im, jm
      character(len=*) :: qname
      integer i, j
      real a(im,jm)

      real qmin(jm), qmax(jm)
      real pmax, pmin
      real fac                     ! multiplication factor

      do j=1,jm
         pmax = a(1,j)
         pmin = a(1,j)
         do i=2,im
            pmax = max(pmax, a(i,j))
            pmin = min(pmin, a(i,j))
         enddo
         qmax(j) = pmax
         qmin(j) = pmin
      enddo
!
! Now find max/min of amax/amin
!
            pmax = qmax(1)
            pmin = qmin(1)
         do j=2,jm
            pmax = max(pmax, qmax(j))
            pmin = min(pmin, qmin(j))
         enddo

      write(*,*) qname, ' max = ', pmax*fac, ' min = ', pmin*fac

 end subroutine pmaxmin

subroutine pmaxmn(qname, q, is, ie, js, je, km, fac, area, domain)
      character(len=*), intent(in)::  qname
      integer, intent(in):: is, ie, js, je
      integer, intent(in):: km
      real, intent(in)::    q(is:ie, js:je, km)
      real, intent(in)::    fac
      real(kind=R_GRID), intent(IN)::  area(is-3:ie+3, js-3:je+3)
      type(domain2d), intent(INOUT) :: domain
!---local variables
      real qmin, qmax, gmean
      integer i,j,k

      qmin = q(is,js,1)
      qmax = qmin
      gmean = 0.

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

      call mp_reduce_min(qmin)
      call mp_reduce_max(qmax)

      gmean = g_sum(domain, q(is,js,km), is, ie, js, je, 3, area, 1, reproduce=.true.)
      if(is_master()) write(6,*) qname, qmax*fac, qmin*fac, gmean*fac

 end subroutine pmaxmn

 subroutine p_maxmin(qname, q, is, ie, js, je, km, fac)
      character(len=*), intent(in)::  qname
      integer, intent(in):: is, ie, js, je, km
      real, intent(in)::    q(is:ie, js:je, km)
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
      call mp_reduce_min(qmin)
      call mp_reduce_max(qmax)
      if(is_master()) write(6,*) qname, qmax*fac, qmin*fac

 end subroutine p_maxmin

 subroutine fillq(im, km, nq, q, dp)
   integer,  intent(in):: im                ! No. of longitudes
   integer,  intent(in):: km                ! No. of levels
   integer,  intent(in):: nq                ! Total number of tracers
   real , intent(in)::  dp(im,km)       ! pressure thickness
   real , intent(inout) :: q(im,km,nq)   ! tracer mixing ratio
! !LOCAL VARIABLES:
   integer i, k, ic, k1

   do ic=1,nq
! Bottom up:
      do k=km,2,-1
         k1 = k-1
         do i=1,im
           if( q(i,k,ic) < 0. ) then
               q(i,k1,ic) = q(i,k1,ic) + q(i,k,ic)*dp(i,k)/dp(i,k1)
               q(i,k ,ic) = 0.
           endif
         enddo
      enddo
! Top down:
      do k=1,km-1
         k1 = k+1
         do i=1,im
            if( q(i,k,ic) < 0. ) then
                q(i,k1,ic) = q(i,k1,ic) + q(i,k,ic)*dp(i,k)/dp(i,k1)
                q(i,k ,ic) = 0.
            endif
         enddo
      enddo

   enddo

 end subroutine fillq

 end module external_ic_mod

