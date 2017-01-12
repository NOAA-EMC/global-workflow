module interpolation_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants
  use kinds

  !-----------------------------------------------------------------------

  use namelist_def
  use netcdf
  use netcdfio_interface

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: interpolation_initialize_gridvar
  public :: interpolation_cleanup_gridvar
  public :: interpolation_define_gridvar
  public :: interpolation_define_gridvar_out
  public :: interpolation_compute
  public :: gridvar

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type esmfgrid
     character(len=500)                                                :: filename
     real(r_double),                    dimension(:),      allocatable :: s
     integer,                           dimension(:),      allocatable :: col
     integer,                           dimension(:),      allocatable :: row
     integer                                                           :: n_s
  end type esmfgrid ! type esmfgrid

  type gridvar
     logical,                           dimension(:),      allocatable :: check
     real(r_double),                    dimension(:),      allocatable :: var
     integer                                                           :: ncoords
     integer                                                           :: nx
     integer                                                           :: ny
  end type gridvar ! type gridvar

  ! Define global variables

  integer                                                              :: ncfileid
  integer                                                              :: ncvarid
  integer                                                              :: ncdimid
  integer                                                              :: ncstatus

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! interpolation_compute.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_compute(invar,outvar,is_bilinear,is_nrstnghbr)

    ! Define variables passed to routine

    type(gridvar)                                                        :: invar
    type(gridvar)                                                        :: outvar
    logical                                                              :: is_bilinear
    logical                                                              :: is_nrstnghbr 

    !=====================================================================

    ! Check local variable and proceed accordingly

    call interpolation_esmf(invar,outvar,is_bilinear, &
         & is_nrstnghbr)

    !=====================================================================

  end subroutine interpolation_compute

  !=======================================================================

  ! interpolation_define_gridvar.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_define_gridvar(grid,xdim,ydim,ngrid,input)

    ! Define variables passed to routine

    use nemsio_module, only: nemsio_realkind
    integer                                                              :: ngrid
    integer                                                              :: xdim
    integer                                                              :: ydim
    type(gridvar)                                                        :: grid
    real(nemsio_realkind)                                                   :: input(ngrid,xdim,ydim)

    ! Define variables computed within routine

    integer                                                              :: ncount

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Define local variables

    ncount = 1

    ! Loop through local variable

    do k = 1, ngrid

       ! Loop through local variable

       do j = 1, ydim

          ! Loop through local variable
       
          do i = 1, xdim
          
             ! Define local variables
          
             grid%var(ncount) = input(k,i,j)
             ncount           = ncount + 1

          end do ! do i = 1, xdim

       end do ! do j = 1, ydim

    end do ! do k = 1, ngrid

    !=====================================================================

  end subroutine interpolation_define_gridvar

  !=======================================================================

  ! interpolation_define_gridvar_out.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_define_gridvar_out(grid,xdim,ydim,output)

    ! Define variables passed to routine

    integer                                                              :: xdim
    integer                                                              :: ydim
    type(gridvar)                                                        :: grid
    real(r_double)                                                       :: output(xdim,ydim)

    ! Define variables computed within routine

    integer                                                              :: ncount

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Define local variables

    ncount = 1 
    
    ! Loop through local variable
    
    do j = 1, ydim
       
       ! Loop through local variable
       
       do i = 1, xdim

          ! Define local variables

          output(j,i) = grid%var(ncount)
          ncount      = ncount + 1
       
       end do ! do j = 1, ydim

    end do ! do i = 1, xdim

    !=====================================================================

  end subroutine interpolation_define_gridvar_out

  !=======================================================================

  ! interpolation_initialize_gridvar.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_initialize_gridvar(grid)

    ! Define variables passed to routine

    type(gridvar)                                                        :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%check)) allocate(grid%check(grid%ncoords))
    if(.not. allocated(grid%var))   allocate(grid%var(grid%ncoords))

    !=====================================================================

  end subroutine interpolation_initialize_gridvar

  !=======================================================================

  ! interpolation_cleanup_gridvar.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_cleanup_gridvar(grid)

    ! Define variables passed to routine

    type(gridvar)                                                        :: grid

    !=====================================================================
    
    ! Deallocate memory for local variables

    if(allocated(grid%check)) deallocate(grid%check)
    if(allocated(grid%var))   deallocate(grid%var)

    !=====================================================================

  end subroutine interpolation_cleanup_gridvar

  !=======================================================================

  ! interpolation_initialize_esmf.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_initialize_esmf(grid)

    ! Define variables passed to routine

    type(esmfgrid)                                                       :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    call interpolation_cleanup_esmf(grid)

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(grid%filename)),mode=           &
         & nf90_nowrite,ncid=ncfileid)
    ncstatus = nf90_inq_dimid(ncfileid,'n_s',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_s)
    ncstatus = nf90_close(ncfileid)

    ! Allocate memory for local variables

    if(.not. allocated(grid%s))                                            &
         & allocate(grid%s(grid%n_s))
    if(.not. allocated(grid%row))                                          &
         & allocate(grid%row(grid%n_s))
    if(.not. allocated(grid%col))                                          &
         & allocate(grid%col(grid%n_s))

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(grid%filename)),mode=           &
         & nf90_nowrite,ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,'col',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%col)
    ncstatus = nf90_inq_varid(ncfileid,'row',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%row)
    ncstatus = nf90_inq_varid(ncfileid,'S',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%s)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine interpolation_initialize_esmf

  !=======================================================================


  ! interpolation_cleanup_esmf.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_cleanup_esmf(grid)

    ! Define variables passed to routine

    type(esmfgrid)                                                       :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%s))   deallocate(grid%s)
    if(allocated(grid%row)) deallocate(grid%row)
    if(allocated(grid%col)) deallocate(grid%col)

    !=====================================================================

  end subroutine interpolation_cleanup_esmf

  !=======================================================================

  ! interpolation_esmf.f90:

  !-----------------------------------------------------------------------

  subroutine interpolation_esmf(invar,outvar,is_bilinear,is_nrstnghbr)

    ! Define variables passed to routine

    type(gridvar)                                                        :: invar
    type(gridvar)                                                        :: outvar
    logical                                                              :: is_bilinear
    logical                                                              :: is_nrstnghbr 

    ! Define variables computed within routine

    type(esmfgrid)                                                       :: grid

    ! Define counting variables

    integer                                                              :: i, j, k, l

    !=====================================================================

    ! Define local variables

    outvar%check = .false.
    outvar%var   = dble(0.0)

    ! Check local variable and proceed accordingly
    
    if(is_bilinear) then
       
       ! Define local variables
       
       grid%filename = esmf_bilinear_filename
       
    else if(is_nrstnghbr) then
       
       ! Define local variables
       
       grid%filename = esmf_neareststod_filename
       
    end if ! if(is_bilinear)
    
    ! Check local variable and proceed accordingly
    
    if(trim(adjustl(grid%filename)) .eq. 'NOT USED') then
       
       ! Define local variables
       
       write(6,500) grid%filename
       stop
       
    end if ! if(trim(adjustl(grid%filename)) .eq. 'NOT USED')

    ! Define local variables
    
    call interpolation_initialize_esmf(grid)
       
    ! Loop through local variable 
    
    do i = 1, grid%n_s
       
       ! Compute local variables
       
       outvar%var(grid%row(i)) = outvar%var(grid%row(i)) +                  &
            & grid%s(i)*invar%var(grid%col(i))
       
       ! Define local variables

       outvar%check(grid%row(i)) = .true.
       
    end do ! do i = 1, grid%n_s

    ! Deallocate memory for local variables
    
    call interpolation_cleanup_esmf(grid)

    ! Define local variables
       
    grid%filename = esmf_neareststod_filename

    ! Check local variable and proceed accordingly

    if(trim(adjustl(grid%filename)) .ne. 'NOT USED') then

       ! Define local variables

       call interpolation_initialize_esmf(grid)

       ! Loop through local variable 
    
       do i = 1, grid%n_s

          ! Check local variable and proceed accordingly

          if(.not. outvar%check(grid%row(i))) then
       
             ! Compute local variables
       
             outvar%var(grid%row(i)) = outvar%var(grid%row(i)) +           &
                  & grid%s(i)*invar%var(grid%col(i))

          end if ! if(.not. outvar%check(grid%row(i)))
       
       end do ! do i = 1, grid%n_s

    end if ! if(trim(adjustl(grid%filename)) .ne. 'NOT USED')

    !=====================================================================

    ! Define format statements

500 format('INTERPOLATION_ESMF: Filename ', a, ' is invalid. Aborting!!!')

    !=====================================================================

  end subroutine interpolation_esmf

  !=======================================================================

end module interpolation_interface
