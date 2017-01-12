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

module gfs_nems_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use namelist_def
  use nemsio_module

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type gfs_grid
     real(r_kind),                 dimension(:,:),     allocatable :: rlon
     real(r_kind),                 dimension(:,:),     allocatable :: rlat
     real(r_kind)                                                  :: rlon_min
     real(r_kind)                                                  :: rlon_max
     real(r_kind)                                                  :: rlat_min
     real(r_kind)                                                  :: rlat_max
     real(r_kind)                                                  :: dx
     real(r_kind)                                                  :: dy
     integer                                                       :: ntrunc
     integer                                                       :: ncoords
     integer                                                       :: nlons
     integer                                                       :: nlats
     integer                                                       :: nz
  end type gfs_grid ! type gfs_grid

  type nemsio_meta
     character(nemsio_charkind),   dimension(:),       allocatable :: recname
     character(nemsio_charkind),   dimension(:),       allocatable :: reclevtyp
     character(16),                dimension(:),       allocatable :: variname
     character(16),                dimension(:),       allocatable :: varr8name
     character(16),                dimension(:),       allocatable :: aryiname
     character(16),                dimension(:),       allocatable :: aryr8name
     character(nemsio_charkind8)                                   :: gdatatype
     character(nemsio_charkind8)                                   :: modelname
     real(nemsio_realkind),        dimension(:,:,:),   allocatable :: vcoord
     real(nemsio_realkind),        dimension(:),       allocatable :: lon
     real(nemsio_realkind),        dimension(:),       allocatable :: lat
     real(nemsio_realkind)                                         :: rlon_min
     real(nemsio_realkind)                                         :: rlon_max
     real(nemsio_realkind)                                         :: rlat_min
     real(nemsio_realkind)                                         :: rlat_max
     integer(nemsio_intkind),      dimension(:,:),     allocatable :: aryival
     integer(nemsio_intkind),      dimension(:),       allocatable :: reclev
     integer(nemsio_intkind),      dimension(:),       allocatable :: varival
     integer(nemsio_intkind),      dimension(:),       allocatable :: aryilen
     integer(nemsio_intkind),      dimension(:),       allocatable :: aryr8len
     integer(nemsio_intkind)                                       :: idate(7)
     integer(nemsio_intkind)                                       :: version
     integer(nemsio_intkind)                                       :: nreo_vc
     integer(nemsio_intkind)                                       :: nrec
     integer(nemsio_intkind)                                       :: nmeta
     integer(nemsio_intkind)                                       :: nmetavari
     integer(nemsio_intkind)                                       :: nmetaaryi
     integer(nemsio_intkind)                                       :: nfhour
     integer(nemsio_intkind)                                       :: nfminute
     integer(nemsio_intkind)                                       :: nfsecondn
     integer(nemsio_intkind)                                       :: nfsecondd
     integer(nemsio_intkind)                                       :: dimx
     integer(nemsio_intkind)                                       :: dimy
     integer(nemsio_intkind)                                       :: dimz
     integer(nemsio_intkind)                                       :: nframe
     integer(nemsio_intkind)                                       :: nsoil
     integer(nemsio_intkind)                                       :: ntrac
     integer(nemsio_intkind)                                       :: jcap
     integer(nemsio_intkind)                                       :: ncldt
     integer(nemsio_intkind)                                       :: idvc
     integer(nemsio_intkind)                                       :: idsl
     integer(nemsio_intkind)                                       :: idvm
     integer(nemsio_intkind)                                       :: idrt
     integer(nemsio_intkind)                                       :: fhour
  end type nemsio_meta ! type nemsio_meta

  !-----------------------------------------------------------------------

  ! Define global variables

  type(nemsio_gfile)                                               :: gfile
  integer                                                          :: nemsio_iret

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: gfs_grid_initialize
  public :: gfs_grid_cleanup
  public :: gfs_grid
  public :: gfs_nems_initialize
  public :: gfs_nems_finalize
  public :: gfs_nems_read
  public :: gfs_nems_write
  public :: gfs_nems_vcoord
  public :: gfs_nems_flip_xlat_axis
  public :: nemsio_meta

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! gfs_nems_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_initialize(meta_nemsio,filename)

    ! Define variables passed to routine
    
    type(nemsio_meta)                                                    :: meta_nemsio
    character(len=500),                        optional,   intent(inout) :: filename

    !=====================================================================

    ! Define local variables

    call nemsio_init(iret=nemsio_iret)

    ! Check local variable and proceed accordingly


    ! Define local variables

    call nemsio_open(gfile,trim(adjustl(filename)),'read',              &
         & iret=nemsio_iret)
    call nemsio_getfilehead(gfile,iret=nemsio_iret,                     &
         & dimx=meta_nemsio%dimx,                                       &
         & nrec=meta_nemsio%nrec,                                       & 
         & dimy=meta_nemsio%dimy)                                       
    if (.not. allocated(meta_nemsio%lon)) &
    allocate(meta_nemsio%lon(meta_nemsio%dimx*meta_nemsio%dimy))
    if (.not. allocated(meta_nemsio%lat)) &
    allocate(meta_nemsio%lat(meta_nemsio%dimx*meta_nemsio%dimy))
    call nemsio_getfilehead(gfile,iret=nemsio_iret,                     &
         & dimz=meta_nemsio%dimz,                                       &
         & lat=meta_nemsio%lat,                                         &
         & lon=meta_nemsio%lon,                                         &
         & idate=meta_nemsio%idate,                                     &
         & nframe=meta_nemsio%nframe,                                   &
         & idrt=meta_nemsio%idrt,                                       &
         & ncldt=meta_nemsio%ncldt,                                     &
         & idvc=meta_nemsio%idvc,                                       &
         & nfhour=meta_nemsio%fhour,                                    &
         & nfminute=meta_nemsio%nfminute,                               &
         & nfsecondn=meta_nemsio%nfsecondn,                             &
         & nfsecondd=meta_nemsio%nfsecondd)

    ! Define format statements

500 format(a,'nemsio_fhr',i3.3)

    !=====================================================================

  end subroutine gfs_nems_initialize

  !=======================================================================

  ! gfs_nems_finalize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_finalize()

    !=====================================================================

    ! Define local variables

    call nemsio_close(gfile,iret=nemsio_iret)

    !=====================================================================

  end subroutine gfs_nems_finalize

  !=======================================================================

  ! gfs_nems_vcoord.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_vcoord(meta_nemsio,filename,vcoord)

    ! Define variables passed to routine
    
    type(nemsio_gfile)                                                   :: lgfile
    type(nemsio_meta)                                                    :: meta_nemsio
    character(len=500)                                                   :: filename
    real(r_kind),             dimension(meta_nemsio%dimz+1,3,2)          :: vcoord

    !=====================================================================

    ! Define local variables

    call nemsio_open(lgfile,trim(adjustl(filename)),'read',                &
         & iret=nemsio_iret)
    call nemsio_getfilehead(lgfile,iret=nemsio_iret,vcoord=vcoord)
    call nemsio_close(lgfile,iret=nemsio_iret)

    !=====================================================================

  end subroutine gfs_nems_vcoord

  !=======================================================================

  ! gfs_nems_flip_xlat_axis.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_flip_xlat_axis(meta_nemsio,grid)
    ! flip latitudes from N to S to S to N

    ! Define variables passed to routine

    type(nemsio_meta)                                                    :: meta_nemsio
    real(nemsio_realkind),  dimension(meta_nemsio%dimx,meta_nemsio%dimy) :: grid

    ! Define variables computed within routine

    real(nemsio_realkind),  dimension(meta_nemsio%dimx,meta_nemsio%dimy) :: workgrid

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Define local variables

    workgrid = grid

    ! Loop through local variable

    do j = 1, meta_nemsio%dimy

       ! Loop through local variable

       do i = 1, meta_nemsio%dimx

          ! Define local variables

          grid(i,meta_nemsio%dimy - j + 1) = workgrid(i,j)

       end do ! do i = 1, meta_nemsio%dimx

    end do ! do j = 1, meta_nemsio%dimy

    !=====================================================================

  end subroutine gfs_nems_flip_xlat_axis

  !=======================================================================

  ! gfs_nems_read.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_read(nems_data,nems_varname,nems_levtyp,nems_lev)

    ! Define variables passed to routine

    character(nemsio_charkind)                                           :: nems_varname
    character(nemsio_charkind)                                           :: nems_levtyp
    real(nemsio_realkind)                                                :: nems_data(:)
    integer(nemsio_intkind)                                              :: nems_lev

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Define local variables

    call nemsio_readrecv(gfile,trim(adjustl(nems_varname)),levtyp=         &
         & trim(adjustl(nems_levtyp)),lev=nems_lev,data=nems_data,         &
         & iret=nemsio_iret)

    ! Check local variable and proceed accordingly

    if(debug) write(6,500) trim(adjustl(nems_varname)), nemsio_iret,       &
         & nems_lev, minval(nems_data), maxval(nems_data)

    !=====================================================================

    ! Define format statements

500 format('GFS_NEMS_READ: NEMS I/O name = ', a, '; readrecv return ',     &
         & 'code = ', i5,'; level = ', i3, '; (min,max) = (', f13.5,f13.5, &
         & ').')

    !=====================================================================

  end subroutine gfs_nems_read

  !=======================================================================

  ! gfs_nems_write.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_write(nems_data,nems_varname,nems_levtyp,nems_lev)

    ! Define variables passed to routine

    character(nemsio_charkind)                                           :: nems_varname
    character(nemsio_charkind)                                           :: nems_levtyp
    real(nemsio_realkind)                                                :: nems_data(:)
    integer(nemsio_intkind)                                              :: nems_lev

    !=====================================================================

    ! Define local variables

    call nemsio_writerecv(gfile,trim(adjustl(nems_varname)),levtyp=        &
         & trim(adjustl(nems_levtyp)),lev=nems_lev,data=nems_data,         &
         & iret=nemsio_iret)

    ! Check local variable and proceed accordingly

    if(debug) write(6,500) trim(adjustl(nems_varname)), nemsio_iret,       &
         & nems_lev, minval(nems_data), maxval(nems_data)

    !=====================================================================

    ! Define format statements

500 format('GFS_NEMS_WRITE: NEMS I/O name = ', a, '; writerecv return ',   &
         & 'code = ', i5,'; level = ', i3, '; (min,max) = (', f13.5,f13.5, &
         & ').')

    !=====================================================================

  end subroutine gfs_nems_write

  !=======================================================================

  ! gfs_grid_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_grid_initialize(grid,meta_nemsio)

    ! Define variables passed to routine
    
    type(gfs_grid)                                                       :: grid
    type(nemsio_meta)                                                    :: meta_nemsio

    ! Define variables computed within routine

    real(r_kind),               dimension(:),                allocatable :: slat
    real(r_kind),               dimension(:),                allocatable :: wlat
    real(r_kind),               dimension(:),                allocatable :: workgrid

    ! Define counting variables

    integer                                                              :: i, j, k, n

    !=====================================================================

    ! Define local variables

    call init_constants_derived()

    ! Allocate memory for local variables
       
    if(.not. allocated(grid%rlon))                                         &
         & allocate(grid%rlon(grid%nlons,grid%nlats))
    if(.not. allocated(grid%rlat))                                         &
         & allocate(grid%rlat(grid%nlons,grid%nlats))
    if(.not. allocated(workgrid))                                          &
         & allocate(workgrid(grid%nlats))

    ! Compute local variables
    
    grid%ncoords = grid%nlons*grid%nlats
    
    n = 0
    do j=1,grid%nlats 
    do i=1,grid%nlons
       n = n + 1
       grid%rlon(i,j) = meta_nemsio%lon(n)
       grid%rlat(i,j) = meta_nemsio%lat(n)
    enddo
    enddo

    ! Deallocate memory for local variables

    if(allocated(slat))     deallocate(slat)
    if(allocated(wlat))     deallocate(wlat)
    if(allocated(workgrid)) deallocate(workgrid)

    !=====================================================================

  end subroutine gfs_grid_initialize

  !=======================================================================

  ! gfs_grid_cleanup.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_grid_cleanup(grid)

    ! Define variables passed to routine
    
    type(gfs_grid)                                                       :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%rlon)) deallocate(grid%rlon)
    if(allocated(grid%rlat)) deallocate(grid%rlat)

    !=====================================================================

  end subroutine gfs_grid_cleanup

  !=======================================================================

end module gfs_nems_interface
