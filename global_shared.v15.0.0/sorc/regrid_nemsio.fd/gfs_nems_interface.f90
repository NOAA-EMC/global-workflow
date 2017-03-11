module gfs_nems_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use interpolation_interface
  use mpi_interface
  use namelist_def
  use nemsio_module
  use netcdfio_interface
  use variable_interface

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type gfs_grid
     real(r_kind),                 dimension(:,:),     allocatable :: rlon
     real(r_kind),                 dimension(:,:),     allocatable :: rlat
     integer                                                       :: ncoords
     integer                                                       :: nlons
     integer                                                       :: nlats
     integer                                                       :: nz
  end type gfs_grid ! type gfs_grid

  type nemsio_meta
     character(nemsio_charkind),   dimension(:),       allocatable :: recname
     character(nemsio_charkind),   dimension(:),       allocatable :: reclevtyp
     character(nemsio_charkind),   dimension(:),       allocatable :: variname
     character(nemsio_charkind),   dimension(:),       allocatable :: varr8name
     character(nemsio_charkind),   dimension(:),       allocatable :: aryiname
     character(nemsio_charkind),   dimension(:),       allocatable :: aryr8name
     character(nemsio_charkind8)                                   :: gdatatype
     character(nemsio_charkind8)                                   :: modelname
     real(nemsio_realkind),        dimension(:,:,:),   allocatable :: vcoord
     real(nemsio_realkind),        dimension(:),       allocatable :: lon
     real(nemsio_realkind),        dimension(:),       allocatable :: lat
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
     integer(nemsio_intkind)                                       :: jcap
     integer(nemsio_intkind)                                       :: dimx
     integer(nemsio_intkind)                                       :: dimy
     integer(nemsio_intkind)                                       :: dimz
     integer(nemsio_intkind)                                       :: nframe
     integer(nemsio_intkind)                                       :: nsoil
     integer(nemsio_intkind)                                       :: ntrac
     integer(nemsio_intkind)                                       :: ncldt
     integer(nemsio_intkind)                                       :: idvc
     integer(nemsio_intkind)                                       :: idsl
     integer(nemsio_intkind)                                       :: idvm
     integer(nemsio_intkind)                                       :: idrt
     integer(nemsio_intkind)                                       :: fhour
  end type nemsio_meta ! type nemsio_meta

  !-----------------------------------------------------------------------

  ! Define global variables

  type(nemsio_gfile)                                       :: gfile2d,gfile3d
  integer                                                          :: nemsio_iret

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: gfs_grid_initialize
  public :: gfs_grid_cleanup
  public :: gfs_grid
  public :: gfs_nems_meta_initialization
  public :: gfs_nems_meta_cleanup
  public :: gfs_nems_initialize
  public :: gfs_nems_finalize
  public :: gfs_nems_write
  public :: nemsio_meta

contains

  !=======================================================================

  ! gfs_nems_write.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_write(c2dor3d,nems_data,nems_varname,nems_levtyp,nems_lev)

    ! Define variables passed to routine

    character(nemsio_charkind)                                           :: nems_varname
    character(nemsio_charkind)                                           :: nems_levtyp
    real(nemsio_realkind)                                                :: nems_data(:)
    integer(nemsio_intkind)                                              :: nems_lev
    character(len=2) :: c2dor3d

    !=====================================================================

    ! Define local variables

    if (c2dor3d == '2d') then
       call nemsio_writerecv(gfile2d,trim(adjustl(nems_varname)),levtyp=        &
            & trim(adjustl(nems_levtyp)),lev=nems_lev,data=nems_data,         &
            & iret=nemsio_iret)
    else if (c2dor3d == '3d') then
       call nemsio_writerecv(gfile3d,trim(adjustl(nems_varname)),levtyp=        &
            & trim(adjustl(nems_levtyp)),lev=nems_lev,data=nems_data,         &
            & iret=nemsio_iret)
    else
       nemsio_iret=-99
    endif

    ! Check local variable and proceed accordingly

    if(debug) write(6,500) c2dor3d,trim(adjustl(nems_varname)), nemsio_iret,       &
         & nems_lev, minval(nems_data), maxval(nems_data)

    !=====================================================================

    ! Define format statements

500 format('GFS_NEMS_WRITE',a2,': NEMS I/O name = ', a, '; writerecv return ',   &
         & 'code = ', i5,'; level = ', i3, '; (min,max) = (', f13.5,f13.5, &
         & ').')
    if (nemsio_iret /= 0) then
       print *,'nemsio_writerecv failed, stopping...'
       call mpi_interface_terminate()
       stop
    endif

    !=====================================================================

  end subroutine gfs_nems_write

  !=======================================================================

  ! gfs_nems_meta_initialization.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_meta_initialization(meta_nemsio,var_info,grid)

    ! Define variables passed to routine
    
    type(nemsio_meta)                                                    :: meta_nemsio
    type(varinfo)                                                        :: var_info(:)
    type(gfs_grid)                                                       :: grid

    ! Define variables computed within routine

    integer                                                              :: offset
    integer                                                              :: n2dvar
    integer                                                              :: n3dvar

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(meta_nemsio%recname))                               &
         & allocate(meta_nemsio%recname(meta_nemsio%nrec))
    if(.not. allocated(meta_nemsio%reclevtyp))                             &
         & allocate(meta_nemsio%reclevtyp(meta_nemsio%nrec))
    if(.not. allocated(meta_nemsio%reclev))                                &
         & allocate(meta_nemsio%reclev(meta_nemsio%nrec))
    if(.not. allocated(meta_nemsio%variname))                              &
         & allocate(meta_nemsio%variname(meta_nemsio%nmetavari))
    if(.not. allocated(meta_nemsio%varival))                               &
         & allocate(meta_nemsio%varival(meta_nemsio%nmetavari))
    if(.not. allocated(meta_nemsio%aryiname))                              &
         & allocate(meta_nemsio%aryiname(meta_nemsio%nmetaaryi))
    if(.not. allocated(meta_nemsio%aryilen))                               &
         & allocate(meta_nemsio%aryilen(meta_nemsio%nmetaaryi))
    if(.not. allocated(meta_nemsio%vcoord))                                &
         & allocate(meta_nemsio%vcoord(meta_nemsio%dimz+1,3,2))
    if(.not. allocated(meta_nemsio%aryival))                               &
         & allocate(meta_nemsio%aryival(grid%nlats/2,                      &
         & meta_nemsio%nmetaaryi))
    if(.not. allocated(meta_nemsio%lon))                                   &
         & allocate(meta_nemsio%lon(grid%ncoords))
    if(.not. allocated(meta_nemsio%lat))                                   &
         & allocate(meta_nemsio%lat(grid%ncoords))
    meta_nemsio%vcoord(:,:,:)=0.0
    ! Define local variables

    meta_nemsio%lon                       =                                &
         & reshape(grid%rlon,(/grid%ncoords/))
    meta_nemsio%lat                       =                                &
         & reshape(grid%rlat,(/grid%ncoords/))
    meta_nemsio%aryilen(1)                = grid%nlats/2
    meta_nemsio%aryiname(1)               = 'lpl'
    meta_nemsio%aryival(1:grid%nlats/2,1) = grid%nlons
    k                                     = 0

    ! Loop through local variable
    offset = 0
    n3dvar = 0
    n2dvar = 0


    do i = 1, size(var_info)

       ! Check local variable and proceed accordingly

       if(var_info(i)%ndims .eq. 2) then

          ! Define local variables
          
          k                        = k + 1
          meta_nemsio%reclev(k)    = var_info(i)%nems_lev
          meta_nemsio%recname(k)   = trim(adjustl(var_info(i)%nems_name))
          meta_nemsio%reclevtyp(k) = trim(adjustl(var_info(i)%nems_levtyp))
          n2dvar                   = k

       else if(var_info(i)%ndims .eq. 3) then

          ! Loop through local variable

          meta_nemsio%variname(1) = 'LEVS'
          meta_nemsio%varival(1) = meta_nemsio%dimz
          meta_nemsio%variname(2) = 'NVCOORD'
          meta_nemsio%varival(2) = 2
          meta_nemsio%variname(3) = 'IVS'
          meta_nemsio%varival(3) = 200509
          do k = 1, meta_nemsio%dimz

             ! Define local variables

             meta_nemsio%reclev(k+n2dvar+offset)    = k
             meta_nemsio%recname(k+n2dvar+offset)   =                      &
                  & trim(adjustl(var_info(i)%nems_name))
             meta_nemsio%reclevtyp(k+n2dvar+offset) =                      &
                  & trim(adjustl(var_info(i)%nems_levtyp))

          end do ! do k = 1, nczdim

          ! Define local variables

          n3dvar = n3dvar + 1
          offset = nczdim*n3dvar

       end if ! if(var_info(i)%ndims .eq. 3)
          
    end do ! do i = 1, size(var_info)

    !=====================================================================

  end subroutine gfs_nems_meta_initialization

  !=======================================================================

  ! gfs_nems_meta_cleanup.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_meta_cleanup(meta_nemsio2d,meta_nemsio3d)

    ! Define variables passed to routine
    
    type(nemsio_meta)                                                    :: meta_nemsio2d,meta_nemsio3d

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(meta_nemsio2d%recname))                                     &
         & deallocate(meta_nemsio2d%recname)
    if(allocated(meta_nemsio2d%reclevtyp))                                   &
         & deallocate(meta_nemsio2d%reclevtyp)
    if(allocated(meta_nemsio2d%reclev))                                      &
         & deallocate(meta_nemsio2d%reclev)
    if(allocated(meta_nemsio2d%variname))                                    &
         & deallocate(meta_nemsio2d%variname)
    if(allocated(meta_nemsio2d%aryiname))                                    &
         & deallocate(meta_nemsio2d%aryiname)
    if(allocated(meta_nemsio2d%aryival))                                     &
         & deallocate(meta_nemsio2d%aryival)
    if(allocated(meta_nemsio2d%aryilen))                                     &
         & deallocate(meta_nemsio2d%aryilen)
    if(allocated(meta_nemsio2d%vcoord))                                      &
         & deallocate(meta_nemsio2d%vcoord)
    if(allocated(meta_nemsio2d%lon))                                         &
         & deallocate(meta_nemsio2d%lon)
    if(allocated(meta_nemsio2d%lat))                                         &
         & deallocate(meta_nemsio2d%lat)
    if(allocated(meta_nemsio3d%recname))                                     &
         & deallocate(meta_nemsio3d%recname)
    if(allocated(meta_nemsio3d%reclevtyp))                                   &
         & deallocate(meta_nemsio3d%reclevtyp)
    if(allocated(meta_nemsio3d%reclev))                                      &
         & deallocate(meta_nemsio3d%reclev)
    if(allocated(meta_nemsio3d%variname))                                    &
         & deallocate(meta_nemsio3d%variname)
    if(allocated(meta_nemsio3d%aryiname))                                    &
         & deallocate(meta_nemsio3d%aryiname)
    if(allocated(meta_nemsio3d%aryival))                                     &
         & deallocate(meta_nemsio3d%aryival)
    if(allocated(meta_nemsio3d%aryilen))                                     &
         & deallocate(meta_nemsio3d%aryilen)
    if(allocated(meta_nemsio3d%vcoord))                                      &
         & deallocate(meta_nemsio3d%vcoord)
    if(allocated(meta_nemsio3d%lon))                                         &
         & deallocate(meta_nemsio3d%lon)
    if(allocated(meta_nemsio3d%lat))                                         &
         & deallocate(meta_nemsio3d%lat)

    !=====================================================================

  end subroutine gfs_nems_meta_cleanup

  !=======================================================================

  ! gfs_nems_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_initialize(meta_nemsio2d, meta_nemsio3d)

    ! Define variables passed to routine
    
    type(nemsio_meta)                                                    :: meta_nemsio2d,meta_nemsio3d
    character(len=500)                                                   :: filename
    character(len=7)                                                     :: suffix

    !=====================================================================

    ! Define local variables

    call nemsio_init(iret=nemsio_iret)
    write(suffix,500) meta_nemsio2d%nfhour
    filename = trim(adjustl(datapathout2d))//suffix
    meta_nemsio2d%gdatatype = trim(adjustl(nemsio_opt2d))
    meta_nemsio3d%gdatatype = trim(adjustl(nemsio_opt3d))
    call nemsio_open(gfile2d,trim(adjustl(filename)),'write',                &
         & iret=nemsio_iret,                                               &
         & modelname=trim(adjustl(meta_nemsio2d%modelname)),                 &
         & version=meta_nemsio2d%version,                                    &
         & gdatatype=meta_nemsio2d%gdatatype,                                &
         & jcap=meta_nemsio2d%jcap,                                          &
         & dimx=meta_nemsio2d%dimx,                                          &
         & dimy=meta_nemsio2d%dimy,                                          &
         & dimz=meta_nemsio2d%dimz,                                          &
         & idate=meta_nemsio2d%idate,                                        &
         & nrec=meta_nemsio2d%nrec,                                          & 
         & nframe=meta_nemsio2d%nframe,                                      &
         & idrt=meta_nemsio2d%idrt,                                          &
         & ncldt=meta_nemsio2d%ncldt,                                        &
         & idvc=meta_nemsio2d%idvc,                                          &
         & idvm=meta_nemsio2d%idvm,                                          &
         & idsl=meta_nemsio2d%idsl,                                          &
         & nfhour=meta_nemsio2d%fhour,                                       &
         & nfminute=meta_nemsio2d%nfminute,                                  &
         & nfsecondn=meta_nemsio2d%nfsecondn,                                &
         & nfsecondd=meta_nemsio2d%nfsecondd,                                &
         & extrameta=.true.,                                                 &
         & nmetaaryi=meta_nemsio2d%nmetaaryi,                                &
         & recname=meta_nemsio2d%recname,                                    &
         & reclevtyp=meta_nemsio2d%reclevtyp,                                &
         & reclev=meta_nemsio2d%reclev,                                      &
         & aryiname=meta_nemsio2d%aryiname,                                  &
         & aryilen=meta_nemsio2d%aryilen,                                    &
         & aryival=meta_nemsio2d%aryival,                                    &
         & vcoord=meta_nemsio2d%vcoord)
    write(suffix,500) meta_nemsio3d%nfhour
    filename = trim(adjustl(datapathout3d))//suffix
    call nemsio_open(gfile3d,trim(adjustl(filename)),'write',                &
         & iret=nemsio_iret,                                                 &
         & modelname=trim(adjustl(meta_nemsio3d%modelname)),                 &
         & version=meta_nemsio3d%version,                                    &
         & gdatatype=meta_nemsio3d%gdatatype,                                &
         & jcap=meta_nemsio3d%jcap,                                          &
         & dimx=meta_nemsio3d%dimx,                                          &
         & dimy=meta_nemsio3d%dimy,                                          &
         & dimz=meta_nemsio3d%dimz,                                          &
         & idate=meta_nemsio3d%idate,                                        &
         & nrec=meta_nemsio3d%nrec,                                          & 
         & nframe=meta_nemsio3d%nframe,                                      &
         & idrt=meta_nemsio3d%idrt,                                          &
         & ncldt=meta_nemsio3d%ncldt,                                        &
         & idvc=meta_nemsio3d%idvc,                                          &
         & idvm=meta_nemsio3d%idvm,                                          &
         & idsl=meta_nemsio3d%idsl,                                          &
         & nfhour=meta_nemsio3d%fhour,                                       &
         & nfminute=meta_nemsio3d%nfminute,                                  &
         & nfsecondn=meta_nemsio3d%nfsecondn,                                &
         & nfsecondd=meta_nemsio3d%nfsecondd,                                &
         & extrameta=.true.,                                                 &
         & nmetaaryi=meta_nemsio3d%nmetaaryi,                                &
         & recname=meta_nemsio3d%recname,                                    &
         & reclevtyp=meta_nemsio3d%reclevtyp,                                &
         & reclev=meta_nemsio3d%reclev,                                      &
         & aryiname=meta_nemsio3d%aryiname,                                  &
         & aryilen=meta_nemsio3d%aryilen,                                    &
         & aryival=meta_nemsio3d%aryival,                                    &
         & variname=meta_nemsio3d%variname,                                  &
         & varival=meta_nemsio3d%varival,                                    &
         & nmetavari=meta_nemsio3d%nmetavari,                                &
         & vcoord=meta_nemsio3d%vcoord)

    !=====================================================================

    ! Define format statements

500 format('.fhr',i3.3)

    !=====================================================================

  end subroutine gfs_nems_initialize

  !=======================================================================

  ! gfs_nems_finalize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_nems_finalize()

    !=====================================================================

    ! Define local variables

    call nemsio_close(gfile2d,iret=nemsio_iret)
    call nemsio_close(gfile3d,iret=nemsio_iret)

    !=====================================================================

  end subroutine gfs_nems_finalize

  !=======================================================================

  ! gfs_grid_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine gfs_grid_initialize(grid)

    ! Define variables passed to routine
    
    type(gfs_grid)                                                       :: grid

    ! Define variables computed within routine

    real(r_kind),               dimension(:),                allocatable :: slat
    real(r_kind),               dimension(:),                allocatable :: wlat
    real(r_kind),               dimension(:),                allocatable :: workgrid

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call init_constants_derived()

       ! Check local variable and proceed accordingly

       ! Define local variables

       grid%nlons    = nlons
       grid%nlats    = nlats

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_barrier(mpi_comm_world,mpi_ierror)

    ! Broadcast all necessary variables to compute nodes

    call mpi_bcast(grid%nlons,1,mpi_integer,mpi_masternode,mpi_comm_world, &
         & mpi_ierror)
    call mpi_bcast(grid%nlats,1,mpi_integer,mpi_masternode,mpi_comm_world, &
         & mpi_ierror)

    ! Allocate memory for local variables
       
    if(.not. allocated(grid%rlon))                                         &
         & allocate(grid%rlon(grid%nlons,grid%nlats))
    if(.not. allocated(grid%rlat))                                         &
         & allocate(grid%rlat(grid%nlons,grid%nlats))

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Allocate memory for local variables
       
       if(.not. allocated(slat))     allocate(slat(grid%nlats))
       if(.not. allocated(wlat))     allocate(wlat(grid%nlats))
       if(.not. allocated(workgrid)) allocate(workgrid(grid%nlats))

       ! Compute local variables

       grid%ncoords = grid%nlons*grid%nlats
       call splat(grid%nlats,slat,wlat)
       workgrid = acos(slat) - pi/2.0

       ! Loop through local variable

       do j = 1, grid%nlats

          ! Loop through local variable

          do i = 1, grid%nlons

             ! Compute local variables

             grid%rlon(i,j) = (i-1)*(360./grid%nlons)*deg2rad
             grid%rlat(i,j) = workgrid(grid%nlats - j + 1)

          end do ! do i = 1, grid%nlons

       end do ! do j = 1, grid%nlats

       ! Deallocate memory for local variables

       if(allocated(slat))     deallocate(slat)
       if(allocated(wlat))     deallocate(wlat)
       if(allocated(workgrid)) deallocate(workgrid)

    endif ! if(mpi_procid .eq. mpi_masternode)

    ! Broadcast all necessary variables to compute nodes

    call mpi_bcast(grid%ncoords,1,mpi_integer,mpi_masternode,              &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(grid%rlon,grid%ncoords,mpi_real,mpi_masternode,         &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(grid%rlat,grid%ncoords,mpi_real,mpi_masternode,         &
         & mpi_comm_world,mpi_ierror)

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
