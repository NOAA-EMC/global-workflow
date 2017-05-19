!    Copyright (C) 2015 Henry R. Winterbottom

!    Email: Henry.Winterbottom@noaa.gov

!    Snail-mail:

!    Henry R. Winterbottom
!    NOAA/OAR/PSD R/PSD1
!    325 Broadway
!    Boulder, CO 80303-3328

!    This file is part of global-model-py.

!    global-model-py is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.

!    global-model-py is distributed in the hope that it will be
!    useful, but WITHOUT ANY WARRANTY; without even the implied
!    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!    See the GNU General Public License for more details.

!    You should have received a copy of the GNU General Public License
!    along with global-model-py.  If not, see
!    <http://www.gnu.org/licenses/>.

module fv3_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use gfs_nems_interface
  use namelist_def
  use netcdf
  use variable_interface

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type analysis_grid
     character(len=500)                                                :: filename
     real(r_kind),                   dimension(:,:,:),     allocatable :: dpres
     real(r_kind),                   dimension(:,:,:),     allocatable :: ugrd
     real(r_kind),                   dimension(:,:,:),     allocatable :: vgrd
     real(r_kind),                   dimension(:,:,:),     allocatable :: spfh
     real(r_kind),                   dimension(:,:,:),     allocatable :: tmp
     real(r_kind),                   dimension(:,:,:),     allocatable :: clwmr
     real(r_kind),                   dimension(:,:,:),     allocatable :: o3mr
     real(r_kind),                   dimension(:,:),       allocatable :: psfc
     real(r_kind),                   dimension(:),         allocatable :: ak
     real(r_kind),                   dimension(:),         allocatable :: bk
     real(r_kind),                   dimension(:),         allocatable :: ck
  end type analysis_grid ! type analysis_grid

  type increment_grid
     real(r_kind),                   dimension(:,:,:),     allocatable :: delp_inc
     real(r_kind),                   dimension(:,:,:),     allocatable :: u_inc
     real(r_kind),                   dimension(:,:,:),     allocatable :: v_inc
     real(r_kind),                   dimension(:,:,:),     allocatable :: sphum_inc
     real(r_kind),                   dimension(:,:,:),     allocatable :: temp_inc
     real(r_kind),                   dimension(:,:,:),     allocatable :: clwmr_inc
     real(r_kind),                   dimension(:,:,:),     allocatable :: o3mr_inc
     real(r_kind),                   dimension(:),         allocatable :: lon
     real(r_kind),                   dimension(:),         allocatable :: lat
     real(r_kind),                   dimension(:),         allocatable :: lev
     real(r_kind),                   dimension(:),         allocatable :: ilev
     real(r_kind),                   dimension(:),         allocatable :: pfull
     real(r_kind),                   dimension(:),         allocatable :: hyai
     real(r_kind),                   dimension(:),         allocatable :: hybi
     integer                                                           :: nx
     integer                                                           :: ny
     integer                                                           :: nz
     integer                                                           :: nzp1
  end type increment_grid ! type increment_grid

  ! Define global variables

  type(nemsio_meta)                                                    :: meta_nemsio
  type(analysis_grid)                                                  :: an_grid
  type(analysis_grid)                                                  :: fg_grid

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: fv3_calc_increment

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! fv3_calc_increment.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_calc_increment()

    ! Define variables computed within routine

    type(increment_grid)                                                 :: grid

    !=====================================================================

    ! Compute local variables

    call fv3_increment_compute(grid)

    ! Define local variables

    call fv3_increment_write(grid)

    ! Deallocate memory for local variables

    call fv3_increment_cleanup(grid)

    !=====================================================================

  end subroutine fv3_calc_increment

  !=======================================================================

  ! fv3_increment_write.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_write(grid)

    ! Define variables passed to routine

    type(increment_grid)                                                 :: grid

    ! Define variables computed within routine

    integer,                    dimension(3)                             :: dimid_3d
    integer,                    dimension(1)                             :: dimid_1d
    integer                                                              :: varid_lon
    integer                                                              :: varid_lat
    integer                                                              :: varid_lev
    integer                                                              :: varid_pfull
    integer                                                              :: varid_ilev
    integer                                                              :: varid_hyai
    integer                                                              :: varid_hybi
    integer                                                              :: varid_u_inc
    integer                                                              :: varid_v_inc
    integer                                                              :: varid_delp_inc
    integer                                                              :: varid_t_inc
    integer                                                              :: varid_sphum_inc
    integer                                                              :: varid_liq_wat_inc
    integer                                                              :: varid_o3mr_inc
    integer                                                              :: dimid_lon
    integer                                                              :: dimid_lat
    integer                                                              :: dimid_lev
    integer                                                              :: dimid_ilev
    integer                                                              :: ncfileid
    integer                                                              :: ncvarid
    integer                                                              :: ncdimid
    integer                                                              :: ncstatus

    !=====================================================================

    ! Define local variables

    if (debug) print *,'writing to ',trim(increment_filename)
    ncstatus    = nf90_create(trim(increment_filename),                 &
         cmode=ior(NF90_CLOBBER,NF90_64BIT_OFFSET),ncid=ncfileid)
    if (ncstatus /= nf90_noerr) then
       print *, 'error opening file ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_dim(ncfileid,'lon',grid%nx,dimid_lon)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lon dim ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_dim(ncfileid,'lat',grid%ny,dimid_lat)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lat dim ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_dim(ncfileid,'lev',grid%nz,dimid_lev)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lev dim ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_dim(ncfileid,'ilev',grid%nzp1,dimid_ilev)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating ilev dim ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    dimid_1d(1) = dimid_lon
    ncstatus    = nf90_def_var(ncfileid,'lon',nf90_float,dimid_1d,         &
         & varid_lon)
    print *,'dims',grid%nx,grid%ny,grid%nz,grid%nzp1
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lon ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_att(ncfileid,varid_lon,'units','degrees_east')
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lon units ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    dimid_1d(1) = dimid_lat
    ncstatus    = nf90_def_var(ncfileid,'lat',nf90_float,dimid_1d,         &
         & varid_lat)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lat ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_att(ncfileid,varid_lat,'units','degrees_north')
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lat units ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    dimid_1d(1) = dimid_lev
    ncstatus    = nf90_def_var(ncfileid,'lev',nf90_float,dimid_1d,         &
         & varid_lev)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating lev ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'pfull',nf90_float,dimid_1d,       &
         & varid_pfull)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating pfull ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    dimid_1d(1) = dimid_ilev
    ncstatus    = nf90_def_var(ncfileid,'ilev',nf90_float,dimid_1d,        &
         & varid_ilev)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating ilev ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'hyai',nf90_float,dimid_1d,        &
         & varid_hyai)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating hyai ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'hybi',nf90_float,dimid_1d,        &
         & varid_hybi)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating hybi ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    dimid_3d(1) = dimid_lon
    dimid_3d(2) = dimid_lat
    dimid_3d(3) = dimid_lev
    ncstatus    = nf90_def_var(ncfileid,'u_inc',nf90_float,dimid_3d,       &
         & varid_u_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating u_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'v_inc',nf90_float,dimid_3d,       &
         & varid_v_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating v_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'delp_inc',nf90_float,dimid_3d,    &
         & varid_delp_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating delp_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'T_inc',nf90_float,dimid_3d,       &
         & varid_t_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating T_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'sphum_inc',nf90_float,dimid_3d,   &
         & varid_sphum_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating sphum_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'liq_wat_inc',nf90_float,dimid_3d, &
         & varid_liq_wat_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating liq_wat_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_def_var(ncfileid,'o3mr_inc',nf90_float,dimid_3d,    &
         & varid_o3mr_inc)
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating o3mr_inc ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_att(ncfileid,nf90_global,'source','GSI')
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating global attribute source',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_att(ncfileid,nf90_global,'comment','global analysis increment from calc_increment.x')
    if (ncstatus /= nf90_noerr) then
       print *, 'error creating global attribute comment',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_enddef(ncfileid)
    if (ncstatus /= nf90_noerr) then
       print *,'enddef error ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_lon,grid%lon)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing lon ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_lat,grid%lat)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing lat ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_lev,grid%lev)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing lev ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_pfull,grid%pfull)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing pfull ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_ilev,grid%ilev)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing ilev ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_hyai,grid%hyai)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing hyai ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_put_var(ncfileid,varid_hybi,grid%hybi)
    if (ncstatus /= nf90_noerr) then
       print *, 'error writing hybi ',trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing u_inc, min/max =',&
     minval(grid%u_inc),maxval(grid%u_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_u_inc,grid%u_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing v_inc, min/max =',&
    minval(grid%v_inc),maxval(grid%v_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_v_inc,grid%v_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing delp_inc, min/max =',&
    minval(grid%delp_inc),maxval(grid%delp_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_delp_inc,grid%delp_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing temp_inc, min/max =',&
    minval(grid%temp_inc),maxval(grid%temp_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_t_inc,grid%temp_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing sphum_inc, min/max =',&
    minval(grid%sphum_inc),maxval(grid%sphum_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_sphum_inc,grid%sphum_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing clwmr_inc, min/max =',&
    minval(grid%clwmr_inc),maxval(grid%clwmr_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_liq_wat_inc,grid%clwmr_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    print *,'writing o3mr_inc, min/max =',&
    minval(grid%o3mr_inc),maxval(grid%o3mr_inc)
    ncstatus    = nf90_put_var(ncfileid,varid_o3mr_inc,grid%o3mr_inc)
    if (ncstatus /= nf90_noerr) then
       print *, trim(nf90_strerror(ncstatus))
       stop 1
    endif
    ncstatus    = nf90_close(ncfileid)
    if (ncstatus /= nf90_noerr) then
       print *, 'error closing file:',trim(nf90_strerror(ncstatus))
       stop 1
    endif

    !=====================================================================

  end subroutine fv3_increment_write

  !=======================================================================

  ! fv3_increment_compute.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_compute(incr_grid)

    ! Define variables passed to routine

    type(increment_grid)                                                 :: incr_grid

    ! Define variables computed within routine

    type(gfs_grid)                                                       :: grid

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Define local variables

    call init_constants_derived()
    call fv3_increment_initialize(incr_grid)
    an_grid%filename = analysis_filename
    fg_grid%filename = firstguess_filename
    call fv3_increment_define_analysis(an_grid)
    call fv3_increment_define_analysis(fg_grid)

    ! Compute local variables

    incr_grid%u_inc     = an_grid%ugrd  - fg_grid%ugrd
    incr_grid%v_inc     = an_grid%vgrd  - fg_grid%vgrd
    incr_grid%delp_inc  = an_grid%dpres - fg_grid%dpres
    incr_grid%temp_inc  = an_grid%tmp   - fg_grid%tmp
    incr_grid%sphum_inc = an_grid%spfh  - fg_grid%spfh
    incr_grid%clwmr_inc = an_grid%clwmr - fg_grid%clwmr
    incr_grid%o3mr_inc  = an_grid%o3mr  - fg_grid%o3mr

    ! Define local variables

    grid%nlons   = meta_nemsio%dimx
    grid%nlats   = meta_nemsio%dimy
    call gfs_grid_initialize(grid, meta_nemsio)
    !incr_grid%lon = grid%rlon(:,1)*rad2deg
    !incr_grid%lat = grid%rlat(1,:)*rad2deg
    incr_grid%lon = grid%rlon(:,1)
    ! reverse latitudes (so they are in increasing order, S to N)
    if (grid%rlat(1,1) > grid%rlat(1,grid%nlats)) then
       do j=1,grid%nlats
          incr_grid%lat(j) = grid%rlat(1,grid%nlats-j+1)
       enddo
    else
       incr_grid%lat = grid%rlat(1,:)
    endif

    ! Loop through local variable

    do k = 1, incr_grid%nz

       ! Define local variables

       incr_grid%lev(k)   = real(k)
       incr_grid%pfull(k) = real(k)

    end do ! do k = 1, incr_grid%nz

    ! Loop through local variable

    do k = 1, incr_grid%nzp1

       ! Define local variables

       incr_grid%ilev(k) = real(k)
       incr_grid%hyai(k) = real(k)
       incr_grid%hybi(k) = real(k)

    end do ! do k = 1, incr_grid%nzp1

    ! Deallocate memory for local variables

    call gfs_grid_cleanup(grid)

    !=====================================================================

  end subroutine fv3_increment_compute

  !=======================================================================

  ! fv3_increment_define_analysis.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_define_analysis(grid)

    ! Define variables passed to routine

    type(analysis_grid)                                                  :: grid

    ! Define variables computed within routine

    type(varinfo)                                                        :: var_info
    real(r_kind),               dimension(:,:,:),            allocatable :: pressi
    real(r_kind),               dimension(:,:,:),            allocatable :: vcoord
    real(r_kind),               dimension(:),                allocatable :: workgrid
    logical flip_lats

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Define local variables

    call gfs_nems_initialize(meta_nemsio,filename=grid%filename)
    ! Allocate memory for local variables

    if(.not. allocated(pressi))                                            &
         & allocate(pressi(meta_nemsio%dimx,meta_nemsio%dimy,              &
         & meta_nemsio%dimz + 1))
    if(.not. allocated(vcoord))                                            &
         & allocate(vcoord(meta_nemsio%dimz + 1,3,2))
    if(.not. allocated(workgrid))                                          &
         & allocate(workgrid(meta_nemsio%dimx*meta_nemsio%dimy))

    ! Define local variables

    print *,'lats',meta_nemsio%lat(1), meta_nemsio%lat(meta_nemsio%dimx*meta_nemsio%dimy)
    if (meta_nemsio%lat(1) > meta_nemsio%lat(meta_nemsio%dimx*meta_nemsio%dimy)) then
      flip_lats = .true.
    else
      flip_lats = .false.
    endif
    print *,'flip_lats',flip_lats
    call gfs_nems_vcoord(meta_nemsio,grid%filename,vcoord)
    grid%ak           = vcoord(:,1,1)
    grid%bk           = vcoord(:,2,1)
    var_info%var_name = 'psfc'
    call variable_lookup(var_info)
    call gfs_nems_read(workgrid,var_info%nems_name,var_info%nems_levtyp,   &
         & 1)
    grid%psfc(:,:)    = reshape(workgrid,(/meta_nemsio%dimx,               &
         & meta_nemsio%dimy/))

    ! Loop through local variable

    do k = 1, meta_nemsio%dimz + 1

       ! Compute local variables

       pressi(:,:,k) = grid%ak(k) + grid%bk(k)*grid%psfc(:,:)

    end do ! do k = 1, meta_nemsio%dimz + 1

    ! Loop through local variable

    do k = 1, meta_nemsio%dimz

       ! Compute local variables

       ! defined as higher pressure minus lower pressure
       grid%dpres(:,:,meta_nemsio%dimz - k + 1) = pressi(:,:,k) -          &
            & pressi(:,:,k+1)
       !print *,'dpres',k,minval(grid%dpres(:,:,meta_nemsio%dimz - k + 1)),&
       !maxval(grid%dpres(:,:,meta_nemsio%dimz - k + 1))

       ! Define local variables

       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%dpres(:,:,meta_nemsio%dimz - k + 1))

    end do ! do k = 1, meta_nemsio%dimz

    ! Loop through local variable

    do k = 1, meta_nemsio%dimz

       ! Define local variables

       var_info%var_name                        = 'ugrd'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%ugrd(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%ugrd(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'vgrd'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%vgrd(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%vgrd(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'spfh'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%spfh(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%spfh(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'tmp'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%tmp(:,:,meta_nemsio%dimz - k + 1)   =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%tmp(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'clwmr'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%clwmr(:,:,meta_nemsio%dimz - k + 1) =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%clwmr(:,:,meta_nemsio%dimz - k + 1))
       var_info%var_name                        = 'o3mr'
       call variable_lookup(var_info)
       call gfs_nems_read(workgrid,var_info%nems_name,                     &
            & var_info%nems_levtyp,k)
       grid%o3mr(:,:,meta_nemsio%dimz - k + 1)  =                          &
            & reshape(workgrid,(/meta_nemsio%dimx,meta_nemsio%dimy/))
       if (flip_lats) call gfs_nems_flip_xlat_axis(meta_nemsio,            &
            & grid%o3mr(:,:,meta_nemsio%dimz - k + 1))

    end do ! do k = 1, meta_nemsio%dimz

    ! Deallocate memory for local variables

    if(allocated(pressi))   deallocate(pressi)
    if(allocated(vcoord))   deallocate(vcoord)
    if(allocated(workgrid)) deallocate(workgrid)

    ! Define local variables

    call gfs_nems_finalize()

    !=====================================================================

  end subroutine fv3_increment_define_analysis

  !=======================================================================

  ! fv3_increment_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_initialize(grid)

    ! Define variables passed to routine

    type(increment_grid)                                                 :: grid

    !=====================================================================

    ! Define local variables

    call gfs_nems_initialize(meta_nemsio,filename=analysis_filename)
    grid%nx   = meta_nemsio%dimx
    grid%ny   = meta_nemsio%dimy
    grid%nz   = meta_nemsio%dimz
    grid%nzp1 = grid%nz + 1
    call gfs_nems_finalize()

    ! Allocate memory for local variables

    if(.not. allocated(grid%delp_inc))                                     &
         & allocate(grid%delp_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%u_inc))                                        &
         & allocate(grid%u_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%v_inc))                                        &
         & allocate(grid%v_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%sphum_inc))                                    &
         & allocate(grid%sphum_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%temp_inc))                                     &
         & allocate(grid%temp_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%clwmr_inc))                                    &
         & allocate(grid%clwmr_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%o3mr_inc))                                     &
         & allocate(grid%o3mr_inc(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nx))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ny))
    if(.not. allocated(grid%lev))                                          &
         & allocate(grid%lev(grid%nz))
    if(.not. allocated(grid%ilev))                                         &
         & allocate(grid%ilev(grid%nzp1))
    if(.not. allocated(grid%pfull))                                        &
         & allocate(grid%pfull(grid%nz))
    if(.not. allocated(grid%hyai))                                         &
         & allocate(grid%hyai(grid%nzp1))
    if(.not. allocated(grid%hybi))                                         &
         & allocate(grid%hybi(grid%nzp1))
    if(.not. allocated(an_grid%dpres))                                     &
         & allocate(an_grid%dpres(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%ugrd))                                      &
         & allocate(an_grid%ugrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%vgrd))                                      &
         & allocate(an_grid%vgrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%spfh))                                      &
         & allocate(an_grid%spfh(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%tmp))                                       &
         & allocate(an_grid%tmp(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%clwmr))                                     &
         & allocate(an_grid%clwmr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%o3mr))                                      &
         & allocate(an_grid%o3mr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(an_grid%psfc))                                      &
         & allocate(an_grid%psfc(grid%nx,grid%ny))
    if(.not. allocated(an_grid%ak))                                        &
         & allocate(an_grid%ak(grid%nz+1))
    if(.not. allocated(an_grid%bk))                                        &
         & allocate(an_grid%bk(grid%nz+1))
    if(.not. allocated(an_grid%ck))                                        &
         & allocate(an_grid%ck(grid%nz+1))
    if(.not. allocated(fg_grid%dpres))                                     &
         & allocate(fg_grid%dpres(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%ugrd))                                      &
         & allocate(fg_grid%ugrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%vgrd))                                      &
         & allocate(fg_grid%vgrd(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%spfh))                                      &
         & allocate(fg_grid%spfh(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%tmp))                                       &
         & allocate(fg_grid%tmp(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%clwmr))                                     &
         & allocate(fg_grid%clwmr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%o3mr))                                      &
         & allocate(fg_grid%o3mr(grid%nx,grid%ny,grid%nz))
    if(.not. allocated(fg_grid%psfc))                                      &
         & allocate(fg_grid%psfc(grid%nx,grid%ny))
    if(.not. allocated(fg_grid%ak))                                        &
         & allocate(fg_grid%ak(grid%nz+1))
    if(.not. allocated(fg_grid%bk))                                        &
         & allocate(fg_grid%bk(grid%nz+1))
    if(.not. allocated(fg_grid%ck))                                        &
         & allocate(fg_grid%ck(grid%nz+1))

    !=====================================================================

  end subroutine fv3_increment_initialize

  !=======================================================================

  ! fv3_increment_cleanup.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_increment_cleanup(grid)

    ! Define variables passed to routine

    type(increment_grid)                                                 :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%delp_inc))  deallocate(grid%delp_inc)
    if(allocated(grid%u_inc))     deallocate(grid%u_inc)
    if(allocated(grid%v_inc))     deallocate(grid%v_inc)
    if(allocated(grid%sphum_inc)) deallocate(grid%sphum_inc)
    if(allocated(grid%temp_inc))  deallocate(grid%temp_inc)
    if(allocated(grid%clwmr_inc)) deallocate(grid%clwmr_inc)
    if(allocated(grid%o3mr_inc))  deallocate(grid%o3mr_inc)
    if(allocated(grid%lon))       deallocate(grid%lon)
    if(allocated(grid%lat))       deallocate(grid%lat)
    if(allocated(grid%lev))       deallocate(grid%lev)
    if(allocated(grid%ilev))      deallocate(grid%ilev)
    if(allocated(grid%pfull))     deallocate(grid%pfull)
    if(allocated(grid%hyai))      deallocate(grid%hyai)
    if(allocated(grid%hybi))      deallocate(grid%hybi)
    if(allocated(an_grid%dpres))  deallocate(an_grid%dpres)
    if(allocated(an_grid%ugrd))   deallocate(an_grid%ugrd)
    if(allocated(an_grid%vgrd))   deallocate(an_grid%vgrd)
    if(allocated(an_grid%spfh))   deallocate(an_grid%spfh)
    if(allocated(an_grid%tmp))    deallocate(an_grid%tmp)
    if(allocated(an_grid%clwmr))  deallocate(an_grid%clwmr)
    if(allocated(an_grid%o3mr))   deallocate(an_grid%o3mr)
    if(allocated(an_grid%psfc))   deallocate(an_grid%psfc)
    if(allocated(an_grid%ak))     deallocate(an_grid%ak)
    if(allocated(an_grid%bk))     deallocate(an_grid%bk)
    if(allocated(fg_grid%ck))     deallocate(an_grid%ck)
    if(allocated(fg_grid%dpres))  deallocate(fg_grid%dpres)
    if(allocated(fg_grid%ugrd))   deallocate(fg_grid%ugrd)
    if(allocated(fg_grid%vgrd))   deallocate(fg_grid%vgrd)
    if(allocated(fg_grid%spfh))   deallocate(fg_grid%spfh)
    if(allocated(fg_grid%tmp))    deallocate(fg_grid%tmp)
    if(allocated(fg_grid%clwmr))  deallocate(fg_grid%clwmr)
    if(allocated(fg_grid%o3mr))   deallocate(fg_grid%o3mr)
    if(allocated(fg_grid%psfc))   deallocate(fg_grid%psfc)
    if(allocated(fg_grid%ak))     deallocate(fg_grid%ak)
    if(allocated(fg_grid%bk))     deallocate(fg_grid%bk)
    if(allocated(fg_grid%ck))     deallocate(fg_grid%ck)

    !=====================================================================

  end subroutine fv3_increment_cleanup

  !=======================================================================

end module fv3_interface
