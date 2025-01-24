module gridinfo

  !========================================================================

  !$$$ Module documentation block
  ! 
  ! This module contains various routines to ingest, define, and compute 
  ! variables from Weather Research and Forecasting (WRF) model Advanced
  ! Research WRF (ARW) and Non-hydrostatic Mesoscale Model (NMM) dynamical
  ! cores which are required by the Ensemble Kalman Filter (ENKF) currently
  ! designed for operations within the National Centers for Environmental
  ! Prediction (NCEP) Global Forecasting System (GFS)
  !
  ! prgmmr: Winterbottom        org: ESRL/PSD1       date: 2011-11-30
  !
  ! program history log:
  !   
  !   2011-11-30  Initial version.
  !   2013-01-02  Updated subroutine getgridinfo_nmm.f90 such that the 
  !               pressure profile (i.e., presslmn) is computed as it is in  
  !               within the GSI subroutine get_wrf_nmm_ensperts.F90.
  !               Henry R. Winterbottom
  !   2017-05-12 Y. Wang and X. Wang - add more state variables in
  !                       cvars3d_supported for radar DA, POC: xuguang.wang@ou.edu
  !
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================

  ! Define associated modules

  use constants, only: rearth_equator, omega, pi, deg2rad, zero, one, rad2deg, &
                       rearth,max_varname_length
  use kinds,     only: i_kind, r_kind, r_single, i_long, r_double
  use params,    only: datapath, nlevs, nlons, nlats,           &
                       arw, nmm
  use mpisetup, only: nproc, mpi_integer, mpi_real4,mpi_status
  use mpimod, only: mpi_comm_world
  use netcdf_io

  implicit none

  !--------------------------------------------------------------------------

  ! Define data structures

  type griddimensions
     integer                                                   :: xdim
     integer                                                   :: ydim
     integer                                                   :: zdim
  end type griddimensions

  ! Define structure types available to all routines

  type(griddimensions),                             public     :: dimensions

  ! Define variables defining the grid attributes (typically from the
  ! ensemble mean)

  real(r_single),      dimension(:,:), allocatable, public     :: logp
  real(r_single),      dimension(:,:), allocatable, public     :: gridloc
  real(r_single),      dimension(:),   allocatable, public     :: lonsgrd
  real(r_single),      dimension(:),   allocatable, public     :: latsgrd
  real(r_single),      dimension(:),   allocatable, public     :: taper_vert
  real(r_single),                                   public     :: ptop
  integer(i_long),                                  public     :: npts
  integer(i_kind),                                  public     :: nlevs_pres

  ! Define all public subroutines within this module

  private
  public :: getgridinfo
  public :: gridinfo_cleanup
  public :: cross2dot
  public :: dot2cross
  ! supported variable names in anavinfo
  character(len=max_varname_length),public, dimension(19) :: vars3d_supported = (/'u   ', 'v   ', 'tv  ', 'q   ', 'w   ', 'cw  ', 'ph  ', 'ql  ', 'qr  ', 'qs  ', 'qg  ', 'qi  ', 'qni ', 'qnr ', 'qnc ', 'dbz ', 'oz  ', 'tsen', 'prse' /)
  character(len=max_varname_length),public, dimension(2)  :: vars2d_supported = (/ 'ps ', 'sst' /)
  character(len=max_varname_length),public, dimension(8)  :: vars2d_landonly = (/'', '', '', '', '', '', '', '' /)


contains

  subroutine getgridinfo(fileprefix, reducedgrid)
  character(len=120), intent(in) :: fileprefix
  logical, intent(in)            :: reducedgrid

    if (arw) then
       call getgridinfo_arw(fileprefix)
    else
       call getgridinfo_nmm(fileprefix)
    end if
  end subroutine getgridinfo

  !=========================================================================

  ! getgridinfo_arw.f90: This subroutine will define, compute, and
  ! return all attributes required from WRF ARW model grid, provided
  ! the file name string; typically this subroutine will operate on
  ! the 'ensemble mean file' since it returns the longitude, latitude,
  ! and log(pressure) values which is currently defaulted to the
  ! ensemble mean in the EnKF implication of J. Whitaker

  ! NOTE: the EnKF assumes all variables are defined along mass (i.e.,
  ! unstaggered grid points) and thus we statically assign array
  ! dimensions during allocation process

  !-------------------------------------------------------------------------

  subroutine getgridinfo_arw(fileprefix)
    character(len=120), intent(in) :: fileprefix

    ! Define variables ingested from external file
    real,      dimension(:,:,:),  allocatable :: wrfarw_mu
    real,      dimension(:,:,:),  allocatable :: wrfarw_mub
    real,      dimension(:,:,:),  allocatable :: wrfarw_znu

   ! Define variables returned by subroutine
    real(r_kind),      dimension(:,:),    allocatable :: presslmn
    real(r_kind),      dimension(:),      allocatable :: spressmn
    
    ! Define variables computed within subroutine
    character(len=500)                                :: filename
    real,      dimension(:,:,:),  allocatable :: workgrid
    integer                                           :: nlevsin
    integer                                           :: nlonsin
    integer                                           :: nlatsin
    integer                                           :: nn
    integer                                           :: ierr

    ! Define variables required for netcdf I/O
    character(len=12)                                 :: varstringname
    character(len=20), dimension(3)                   :: dimstring
    integer,           dimension(3)                   :: dims

    ! Define counting variables 
    integer                                           :: i, j, k
    integer                                           :: count

    !======================================================================
    if(.not. arw .and. .not. nmm) then
       ! Print message to user
       write(6,*) '!!! USER !!! You have not defined the logical variables appropriately which '
       write(6,*) '             state that you are using the WRF ARW or NMM dynamical cores    '
       write(6,*) '             within the namelist. Aborting routine.'

       ! Exit routine
       call stop2(22)
 
    end if ! if(.not. arw .and. .not. nmm)
    ! Define local values and prepare for array dimension definitions
    dimstring(1) = "west_east"
    dimstring(2) = "south_north"
    dimstring(3) = "bottom_top"

    ! Build the ensemble mean filename expected by routine
    filename = trim(adjustl(datapath))//trim(adjustl(fileprefix))//"ensmean"

    ! Obtain unstaggered grid dimensions from ingested variable file
    call netcdfdimension(filename,3,dimstring,dims)

    ! Define array dimension structure type
    dimensions = griddimensions(dims(1),dims(2),dims(3))

    ! Compute all variables required by subsequent routines
    npts = dimensions%xdim*dimensions%ydim
    nlevsin = dimensions%zdim
    nlonsin = dimensions%xdim
    nlatsin = dimensions%ydim

    !----------------------------------------------------------------------
    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly
    if (nlons .ne. nlonsin) then
       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlons ingested from file = ', nlonsin
       write(6,*) '      nlons specified in namelist = ', nlons
       write(6,*) 'Failed in subroutine getgridinfo_arw; Aborting!'

       ! Exit routine
       call stop2(22)
    end if ! if (nlons .ne. nlonsin)

    if (nlats .ne. nlatsin) then
       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlats ingested from file = ', nlatsin
       write(6,*) '      nlats specified in namelist = ', nlats
       write(6,*) 'Failed in subroutine getgridinfo_arw; Aborting!'

       ! Exit routine
       call stop2(22)
    end if ! if (nlats .ne. nlatsin)

    if (nlevs .ne. nlevsin) then
       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlevs ingested from file = ', nlevsin
       write(6,*) '      nlevs specified in namelist = ', nlevs
       write(6,*) 'Failed in subroutine getgridinfo_arw; Aborting!'

       ! Exit routine
       call stop2(22)
    end if ! if (nlevs .ne. nlevsin)

    !----------------------------------------------------------------------
    ! Compute local variable; number of model levels plus surface
    ! (levels at which ens. mean log pressure defined for localization
    ! via array logp)
    nlevs_pres=dimensions%zdim+1

    ! Allocate memory for global arrays
    if(.not. allocated(lonsgrd)) allocate(lonsgrd(npts))
    if(.not. allocated(latsgrd)) allocate(latsgrd(npts))
    if(.not. allocated(taper_vert)) allocate(taper_vert(nlevs))
    if(.not. allocated(logp))    allocate(logp(npts,nlevs_pres))
    taper_vert = one

    !======================================================================
    ! Begin: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes
    !----------------------------------------------------------------------
    if (nproc  .eq. 0) then ! only read data on root.
       ! Allocate memory for all global arrays
       if(.not. allocated(presslmn)) allocate(presslmn(npts,nlevs))
       if(.not. allocated(spressmn)) allocate(spressmn(npts))
       ! Allocate memory for all local arrays
       if(.not. allocated(wrfarw_mu))                                       &
            & allocate(wrfarw_mu(dimensions%xdim,dimensions%ydim,1))
       if(.not. allocated(wrfarw_mub))                                      &
            & allocate(wrfarw_mub(dimensions%xdim,dimensions%ydim,1))
       if(.not. allocated(wrfarw_znu))                                      &
            & allocate(wrfarw_znu(1,1,dimensions%zdim))
       ! Allocate memory for local variable grid
       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,  &
            & dimensions%ydim,1))

       ! Ingest variable from external file
       varstringname = 'XLONG'
       call readnetcdfdata(filename,workgrid,varstringname,              &
            & dimensions%xdim,dimensions%ydim,1)

       ! Initialize counting variable
       count = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, dimensions%ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, dimensions%xdim
             ! Convert from degrees to radians and update the
             ! global longitude array
             lonsgrd(count) = workgrid(i,j,1)*deg2rad
          
             count = count + 1
          end do ! do i = 1, dimensions%xdim
       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid
       if(allocated(workgrid)) deallocate(workgrid)

       ! Allocate memory for local variable grid
       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,  &
            & dimensions%ydim,1))
       
       ! Ingest variable from external file
       varstringname = 'XLAT'
       call readnetcdfdata(filename,workgrid,varstringname,              &
            & dimensions%xdim,dimensions%ydim,1)

       ! Initialize counting variable
       count = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, dimensions%ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, dimensions%xdim
             ! Convert from degrees to radians and update the
             ! global latitude array
             latsgrd(count) = workgrid(i,j,1)*deg2rad

             count = count + 1
          end do ! do i = 1, dimensions%xdim
       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid
       if(allocated(workgrid)) deallocate(workgrid)

    !----------------------------------------------------------------------
       
       ! Ingest the model vertical (eta) levels from the external file
       varstringname = 'ZNU'
       call readnetcdfdata(filename,wrfarw_znu,varstringname,1,1,           &
            & dimensions%zdim)

       ! Ingest the model perturbation dry air mass from the external
       ! file
       varstringname = 'MU'
       call readnetcdfdata(filename,wrfarw_mu,varstringname,                &
            & dimensions%xdim,dimensions%ydim,1)

       ! Ingest the model base state dry air mass from the external
       ! file
       varstringname = 'MUB'
       call readnetcdfdata(filename,wrfarw_mub,varstringname,               &
            & dimensions%xdim,dimensions%ydim,1)

       ! Allocate memory for local variable grid
       if(.not. allocated(workgrid)) allocate(workgrid(1,1,1))

       ! Ingest variable from external file
       varstringname = 'P_TOP'
       call readnetcdfdata(filename,workgrid,varstringname,1,1,1)
    
       ! Define local variable
       ptop = workgrid(1,1,1)

       ! Rescale pressure from Pa to hPa
       ptop = ptop/100.0

       ! Deallocate memory for local variable grid
       if(allocated(workgrid)) deallocate(workgrid)

    !----------------------------------------------------------------------

       ! Initialize counting variable
       count = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, dimensions%ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, dimensions%xdim
             ! Convert from Pa to hPa and update the global surface
             ! pressure array
             spressmn(count) = (wrfarw_mu(i,j,1) + &
                                wrfarw_mub(i,j,1) + ptop*100.0)/100.0

             count = count + 1
          end do ! do i = 1, dimensions%xdim
       end do ! do j = 1, dimensions%ydim

    !----------------------------------------------------------------------

       ! Loop through vertical coordinate
       do k = 1, dimensions%zdim
          ! Initialize counting variable
          count = 1
          ! Loop through meridional horizontal coordinate
          do j = 1, dimensions%ydim
             ! Loop through zonal horizontal coordinate
             do i = 1, dimensions%xdim
                ! Compute the pressure within the respective layer
                ! (dry hydrostatic pressure)
                presslmn(count,k) = wrfarw_znu(1,1,k)*(wrfarw_mu(i,j,1) +   &
                     & wrfarw_mub(i,j,1)) + ptop*100.0
             
                ! Rescale pressure from Pa to hPa
                presslmn(count,k) = presslmn(count,k)/100.0
             
                ! Compute the log of the pressure within the
                ! respective layer
                logp(count,k) = -log(presslmn(count,k))

                count = count + 1
             end do ! do i = 1, dimensions%xdim
          end do ! do j = 1, dimensions%ydim
       end do ! do k = 1, dimensions%zdim

       ! Compute local variable
       logp(:,nlevs_pres) = -log(spressmn(:))

       write(6,*) 'Surface pressure (spressmn) min/max range:',             &
            & minval(spressmn),maxval(spressmn)

    !----------------------------------------------------------------------

       ! Deallocate memory for all local arrays
       if(allocated(wrfarw_mu))  deallocate(wrfarw_mu)
       if(allocated(wrfarw_mub)) deallocate(wrfarw_mub)
       if(allocated(wrfarw_znu)) deallocate(wrfarw_znu)
       if(allocated(presslmn))   deallocate(presslmn)
       if(allocated(spressmn))   deallocate(spressmn)

    !----------------------------------------------------------------------

    end if ! nproc == 0

    !----------------------------------------------------------------------

    ! End: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !======================================================================

    ! Broadcast all common variables these out to all nodes
    call MPI_Bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)

    !----------------------------------------------------------------------

    ! Allocate memory for local variable
    if(.not. allocated(gridloc)) allocate(gridloc(3,npts))

    ! Loop through each grid coordinate and perform the coordinate
    ! transform for regular simulation domains 

    do nn = 1, npts
       ! Compute local variables
       gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
       gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
       gridloc(3,nn) = sin(latsgrd(nn))
    end do ! do nn = 1, npts

  end subroutine getgridinfo_arw

  !=========================================================================

  ! getgridinfo_nmm.f90: This subroutine will define, compute, and
  ! return all attributes required from WRF-NMM model grid, provided
  ! the file name string; typically this subroutine will operate on
  ! the 'ensemble mean file' since it returns the longitude, latitude,
  ! and log(pressure) values which is currently defaulted to the
  ! ensemble mean in the EnKF implication of J. Whitaker

  !-------------------------------------------------------------------------

  subroutine getgridinfo_nmm(fileprefix)
    character(len=120), intent(in) :: fileprefix

    ! Define variables ingested from external file
    real,      dimension(:,:,:),  allocatable :: wrfnmm_eta
    real,      dimension(:,:,:),  allocatable :: wrfnmm_pd
    real,      dimension(:,:,:),  allocatable :: wrfnmm_pdtop
    real,      dimension(:,:,:),  allocatable :: wrfnmm_pt
    real,      dimension(:,:,:),  allocatable :: wrfnmm_aeta1
    real,      dimension(:,:,:),  allocatable :: wrfnmm_aeta2

    ! Define variables returned by subroutine
    real(r_kind),      dimension(:,:),    allocatable :: presslmn
    real(r_kind),      dimension(:),      allocatable :: spressmn
    
    ! Define variables computed within subroutine
    character(len=500)                                :: filename
    real,      dimension(:,:,:),  allocatable         :: workgrid
    integer                                           :: nlevsin
    integer                                           :: nlonsin
    integer                                           :: nlatsin
    integer                                           :: nn
    integer                                           :: ierr

    ! Define variables required for netcdf I/O
    character(len=12)                                 :: varstringname
    character(len=20), dimension(3)                   :: dimstring
    integer,           dimension(3)                   :: dims

    ! Define counting variables 
    integer                                           :: i, j, k
    integer                                           :: count

    !======================================================================

    if(.not. arw .and. .not. nmm) then
       write(6,*) '!!! USER !!! You have not defined the logical variables appropriately which '
       write(6,*) '             state that you are using the WRF ARW or NMM dynamical cores    '
       write(6,*) '             within the namelist. Aborting routine.'
       ! Exit routine
       call stop2(22)
    end if ! if(.not. arw .and. .not. nmm)

    ! Define local values and prepare for array dimension definitions
    dimstring(1) = "west_east"
    dimstring(2) = "south_north"
    dimstring(3) = "bottom_top"

    ! Build the ensemble mean filename expected by routine
    filename = trim(adjustl(datapath))//trim(adjustl(fileprefix))//"ensmean"

    ! Obtain unstaggered grid dimensions from ingested variable file
    call netcdfdimension(filename,3,dimstring,dims)

    ! Define array dimension structure type
    dimensions = griddimensions(dims(1),dims(2),dims(3))

    ! Compute all variables required by subsequent routines
    npts = dimensions%xdim*dimensions%ydim
    nlevsin = dimensions%zdim
    nlonsin = dimensions%xdim
    nlatsin = dimensions%ydim

    !----------------------------------------------------------------------

    ! Perform sanity and error checks for variable dimensions; proceed
    ! accordingly
    if (nlons .ne. nlonsin) then
       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlons ingested from file = ', nlonsin
       write(6,*) '      nlons specified in namelist = ', nlons
       write(6,*) 'Failed in subroutine getgridinfo_nmm; Aborting!'

       ! Exit routine
       call stop2(22)
    end if ! if (nlons .ne. nlonsin)

    if (nlats .ne. nlatsin) then
       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlats ingested from file = ', nlatsin
       write(6,*) '      nlats specified in namelist = ', nlats
       write(6,*) 'Failed in subroutine getgridinfo_nmm; Aborting!'

       ! Exit routine
       call stop2(22)
    end if ! if (nlats .ne. nlatsin)

    if (nlevs .ne. nlevsin) then
       write(6,*) 'Error reading input file in gridinfo...' 
       write(6,*) '      nlevs ingested from file = ', nlevsin
       write(6,*) '      nlevs specified in namelist = ', nlevs
       write(6,*) 'Failed in subroutine getgridinfo_nmm; Aborting!'

       ! Exit routine
       call stop2(22)
    end if ! if (nlevs .ne. nlevsin)

    !----------------------------------------------------------------------

    ! Compute local variable; number of model levels plus surface
    ! (levels at which ens. mean log pressure defined for localization
    ! via array logp)
    nlevs_pres=dimensions%zdim+1

    ! Allocate memory for global arrays
    if(.not. allocated(lonsgrd)) allocate(lonsgrd(npts))
    if(.not. allocated(latsgrd)) allocate(latsgrd(npts))
    if(.not. allocated(logp))    allocate(logp(npts,nlevs_pres))

    !======================================================================

    ! Begin: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !----------------------------------------------------------------------

    if (nproc  .eq. 0) then ! only read data on root.
       ! Allocate memory for all global arrays
       if(.not. allocated(presslmn)) allocate(presslmn(npts,nlevs))
       if(.not. allocated(spressmn)) allocate(spressmn(npts))

       ! Allocate memory for all local arrays
       if(.not. allocated(wrfnmm_eta))                                      &
            & allocate(wrfnmm_eta(1,1,dimensions%zdim))
       if(.not. allocated(wrfnmm_pd))                                       &
            & allocate(wrfnmm_pd(dimensions%xdim,dimensions%ydim,1))
       if(.not. allocated(wrfnmm_pdtop))                                    &
            & allocate(wrfnmm_pdtop(1,1,1))
       if(.not. allocated(wrfnmm_pt))                                       &
            & allocate(wrfnmm_pt(1,1,1))
       if(.not. allocated(wrfnmm_aeta1))                                    &
            & allocate(wrfnmm_aeta1(1,1,dimensions%zdim))
       if(.not. allocated(wrfnmm_aeta2))                                    &
            & allocate(wrfnmm_aeta2(1,1,dimensions%zdim))

    !----------------------------------------------------------------------

       ! Allocate memory for local variable grid
       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,     &
            & dimensions%ydim,1))

       ! Ingest variable from external file
       varstringname = 'GLON'
       call readnetcdfdata(filename,workgrid,varstringname,dimensions%xdim, &
            & dimensions%ydim,1)

       ! Initialize counting variable
       count = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, dimensions%ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, dimensions%xdim
             ! Convert from degrees to radians and update the global
             ! longitude array
             lonsgrd(count) = workgrid(i,j,1)
             
             count = count + 1
          end do ! do i = 1, dimensions%xdim
       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid
       if(allocated(workgrid)) deallocate(workgrid)

       ! Allocate memory for local variable grid
       if(.not. allocated(workgrid)) allocate(workgrid(dimensions%xdim,     &
            & dimensions%ydim,1))
       
       ! Ingest variable from external file
       varstringname = 'GLAT'
       call readnetcdfdata(filename,workgrid,varstringname,                 &
            & dimensions%xdim,dimensions%ydim,1)

       ! Initialize counting variable
       count = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, dimensions%ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, dimensions%xdim
             ! Convert from degrees to radians and update the global
             ! latitude array
             latsgrd(count) = workgrid(i,j,1)

             count = count + 1
          end do ! do i = 1, dimensions%xdim
       end do ! do j = 1, dimensions%ydim

       ! Deallocate memory for local variable grid
       if(allocated(workgrid)) deallocate(workgrid)

    !----------------------------------------------------------------------

       ! Ingest variables from external file
       varstringname = 'PD'
       call readnetcdfdata(filename,wrfnmm_pd,varstringname,                &
            & dimensions%xdim,dimensions%ydim,1)

       varstringname = 'PDTOP'
       call readnetcdfdata(filename,wrfnmm_pdtop,varstringname,1,1,1)

       varstringname = 'PT'
       call readnetcdfdata(filename,wrfnmm_pt,varstringname,1,1,1)

       varstringname = 'AETA1'
       call readnetcdfdata(filename,wrfnmm_aeta1,varstringname,1,1,dimensions%zdim)

       varstringname = 'AETA2'
       call readnetcdfdata(filename,wrfnmm_aeta2,varstringname,1,1,dimensions%zdim)

       ! Initialize counting variable
       count = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, dimensions%ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, dimensions%xdim
             ! Convert from Pa to hPa and update the global surface
             ! pressure array
             spressmn(count) = (wrfnmm_pd(i,j,1) + wrfnmm_pdtop(1,1,1) +    &
                  & wrfnmm_pt(1,1,1))/100.0

             count = count + 1
          end do ! do i = 1, dimensions%xdim
       end do ! do j = 1, dimensions%ydim

       ! Define local variable and rescale pressure from Pa to hPa
       ptop = wrfnmm_pt(1,1,1)/100.0

    !----------------------------------------------------------------------

       ! Loop through vertical coordinate
       do k = 1, dimensions%zdim
          ! Initialize counting variable
          count = 1
          ! Loop through meridional horizontal coordinate
          do j = 1, dimensions%ydim
             ! Loop through zonal horizontal coordinate
             do i = 1, dimensions%xdim
                ! Compute the pressure within the respective layer
                ! (dry hydrostatic pressure)
                presslmn(count,k) = (wrfnmm_aeta1(1,1,k)*                   &
                     & wrfnmm_pdtop(1,1,1)) + wrfnmm_aeta2(1,1,k)*(         &
                     & spressmn(count)*100.0 - wrfnmm_pdtop(1,1,1) -        &
                     & wrfnmm_pt(1,1,1)) + wrfnmm_pt(1,1,1)
             
                ! Rescale pressure from Pa to hPa
                presslmn(count,k) = presslmn(count,k)/100.0
             
                ! Compute the log of the pressure within the
                ! respective layer
                logp(count,k) = -log(presslmn(count,k))

                count = count + 1
             end do ! do i = 1, dimensions%xdim
          end do ! do j = 1, dimensions%ydim
       end do ! do k = 1, dimensions%zdim

       ! Compute local variable
       logp(:,nlevs_pres) = -log(spressmn(:))

       write(6,*) 'Surface pressure (spressmn) min/max range:',             &
            & minval(spressmn),maxval(spressmn)
       write(6,*) 'Longitude range (min/max): ', minval(lonsgrd*rad2deg),   &
            & maxval(lonsgrd*rad2deg)
       write(6,*) 'Latitude range (min/max): ', minval(latsgrd*rad2deg),    & 
            & maxval(latsgrd*rad2deg)

    !----------------------------------------------------------------------

       ! Deallocate memory for all local arrays
       if(allocated(wrfnmm_pd))    deallocate(wrfnmm_pd)
       if(allocated(wrfnmm_pdtop)) deallocate(wrfnmm_pdtop)
       if(allocated(wrfnmm_pt))    deallocate(wrfnmm_pt)
       if(allocated(wrfnmm_aeta1)) deallocate(wrfnmm_aeta1)
       if(allocated(wrfnmm_aeta2)) deallocate(wrfnmm_aeta2)
       if(allocated(presslmn))     deallocate(presslmn)
       if(allocated(spressmn))     deallocate(spressmn)

    !----------------------------------------------------------------------

    end if ! nproc == 0

    !----------------------------------------------------------------------

    ! End: Ingest all grid variables required for EnKF routines and
    ! perform necessary conversions; the data is only ingested on the
    ! master node and then subsequently passed to the slave nodes

    !======================================================================

    ! Broadcast all common variables these out to all nodes
    call MPI_Bcast(logp,npts*nlevs_pres,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(lonsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(latsgrd,npts,mpi_real4,0,MPI_COMM_WORLD,ierr)
    call MPI_Bcast(ptop,1,mpi_real4,0,MPI_COMM_WORLD,ierr)

    !----------------------------------------------------------------------

    ! Allocate memory for local variable
    if(.not. allocated(gridloc)) allocate(gridloc(3,npts))

    ! Loop through each grid coordinate and perform the coordinate
    ! transform for regular simulation domains

    do nn = 1, npts
       ! Compute local variables
       gridloc(1,nn) = cos(latsgrd(nn))*cos(lonsgrd(nn))
       gridloc(2,nn) = cos(latsgrd(nn))*sin(lonsgrd(nn))
       gridloc(3,nn) = sin(latsgrd(nn))
    end do ! do nn = 1, npts

  end subroutine getgridinfo_nmm

  !========================================================================

  ! cross2dot.f90: This subroutine will ingest an array of values and
  ! the respective input dimensions and will return an array of
  ! staggered points on a grid of unstaggered points

  !-------------------------------------------------------------------------

  subroutine cross2dot(varin,sxdim,sydim,szdim,xdim,ydim,zdim,varout)

    ! Define array dimension variables
    integer,                                        intent(in)  :: sxdim, sydim, szdim
    integer,                                        intent(in)  :: xdim, ydim, zdim
    
    ! Define variables passed to subroutine
    real,     dimension(sxdim,sydim,szdim), intent(in)  :: varin

    ! Define variables returned by subroutine
    real,     dimension(xdim,ydim,zdim),    intent(out) :: varout    

    !======================================================================

    ! Check in which dimension variable grid is staggered and
    ! interpolate from staggered grid to unstaggered grid

    if (sxdim .gt. xdim) then
       varout = 0.5*(varin(1:xdim,:,:)+varin(2:xdim+1,:,:))
    else if (sydim .gt. ydim) then
       varout = 0.5*(varin(:,1:ydim,:)+varin(:,2:ydim+1,:))
    else if (szdim .gt. zdim) then
       varout = 0.5*(varin(:,:,1:zdim)+varin(:,:,2:zdim+1))
    else
       varout = varin
    end if

    !======================================================================

    ! Return calculated value

    return

    !======================================================================

  end subroutine cross2dot

  !========================================================================

  ! dot2cross.f90: This subroutine will ingest an array of values and
  ! the respective input dimensions and will return an array of
  ! staggered points on a grid of unstaggered points via linear interpolation.

  !-------------------------------------------------------------------------

  subroutine dot2cross(xdim,ydim,zdim,sxdim,sydim,szdim,varin,   &
       &     varout)

    ! Define array dimension variables

    integer,                                        intent(in)  :: sxdim, sydim, szdim
    integer,                                        intent(in)  :: xdim, ydim, zdim
    
    ! Define variables passed to subroutine
    
    real,     dimension(xdim,ydim,zdim), intent(in)  :: varin

    ! Define variables returned by subroutine

    real,     dimension(sxdim,sydim,szdim),    intent(out) :: varout    

    !======================================================================

    if(sxdim .gt. xdim) then
       ! inverse of:
       ! varout = 0.5*(varin(1:xdim,:,:)+varin(2:xdim+1,:,:))
       varout(2:sxdim-1,:,:) = 0.5*(varin(1:xdim-1,:,:) + varin(2:xdim,:,:))
       varout(1,:,:) = 1.5*varin(1,:,:) - 0.5*varin(2,:,:)
       ! linear extrapolation to outer points (outside of mass grid)
       varout(sxdim,:,:)  = 1.5*varin(xdim,:,:) - 0.5*varin(xdim-1,:,:)
    else if(sydim .gt. ydim) then
       ! inverse of:
       ! varout = 0.5*(varin(:,1:ydim,:)+varin(:,2:ydim+1,:))
       varout(:,2:sydim-1,:) = 0.5*(varin(:,1:ydim-1,:) + varin(:,2:ydim,:))
       varout(:,1,:) = 1.5*varin(:,1,:) - 0.5*varin(:,2,:)
       varout(:,sydim,:)  = 1.5*varin(:,ydim,:) - 0.5*varin(:,ydim-1,:)
    else if(szdim .gt. zdim) then
       ! inverse of:
       ! varout = 0.5*(varin(:,:,1:zdim)+varin(:,:,2:zdim+1))
       varout(:,:,2:szdim-1) = 0.5*(varin(:,:,1:zdim-1) + varin(:,:,2:zdim))
       varout(:,:,1) = 1.5*varin(:,:,1) - 0.5*varin(:,:,2)
       varout(:,:,szdim)  = 1.5*varin(:,:,zdim) - 0.5*varin(:,:,zdim-1)
    else
       varout = varin
    end if ! if(sxdim .gt. xdim)

    !=======================================================================

    ! Return calculated value

    return

    !=======================================================================

  end subroutine dot2cross

  !=========================================================================

  subroutine gridinfo_cleanup()
    if (allocated(lonsgrd))       deallocate(lonsgrd)
    if (allocated(latsgrd))       deallocate(latsgrd)
    if (allocated(taper_vert))    deallocate(taper_vert)
    if (allocated(logp))          deallocate(logp)
    if (allocated(gridloc))       deallocate(gridloc)
  end subroutine gridinfo_cleanup

  !=========================================================================

end module gridinfo
