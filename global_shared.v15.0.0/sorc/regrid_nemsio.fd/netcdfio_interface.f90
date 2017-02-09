module netcdfio_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use kinds

  !-----------------------------------------------------------------------

  use namelist_def
  use netcdf
  use mpi_interface

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define global variables

  logical                                                              :: ncstatic
  integer                                                              :: ncrec
  integer                                                              :: ncxdim
  integer                                                              :: ncydim
  integer                                                              :: nczdim
  integer                                                              :: nctdim
  integer                                                              :: ncfileid
  integer                                                              :: ncvarid
  integer                                                              :: ncdimid
  integer                                                              :: ncstatus

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  interface netcdfio_values_1d
     module procedure netcdfio_values_1d_dblepr
     module procedure netcdfio_values_1d_realpr
     module procedure netcdfio_values_1d_intepr
  end interface ! interface netcdfio_values_2d
  interface netcdfio_values_2d
     module procedure netcdfio_values_2d_dblepr
     module procedure netcdfio_values_2d_realpr
     module procedure netcdfio_values_2d_intepr
  end interface ! interface netcdfio_values_2d
  interface netcdfio_values_3d
     module procedure netcdfio_values_3d_dblepr
     module procedure netcdfio_values_3d_realpr
     module procedure netcdfio_values_3d_intepr
  end interface ! interface netcdfio_values_3d
  interface netcdfio_global_attr
     module procedure netcdfio_global_attr_char
  end interface ! interface netcdfio_global_attr
  interface netcdfio_variable_attr
     module procedure netcdfio_variable_attr_char
  end interface ! interface netcdfio_variable_attr
  public :: netcdfio_values_1d
  public :: netcdfio_values_2d
  public :: netcdfio_values_3d
  public :: netcdfio_dimension
  public :: netcdfio_global_attr
  public :: netcdfio_variable_attr
  public :: ncrec
  public :: ncxdim
  public :: ncydim
  public :: nczdim
  public :: nctdim
  public :: ncstatic

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! netcdfio_global_attr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_global_attr_char(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    character(len=*)                                                     :: varvalue

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_get_att(ncfileid,nf90_global,trim(adjustl(varname)),  &
         & varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdfio_global_attr_char

  subroutine netcdfio_variable_attr_char(filename,varname,attribute,varvalue)

      implicit none

      !=======================================================================

      ! Define variables passed to subroutine

      character(len=500),intent(in) ::       filename
      character(len=*),intent(in) ::         attribute
      character(len=*),intent(in) ::         varname

      ! Define variables returned by subroutine

      character(len=80),intent(out) ::    varvalue

      ! Define variables for decoding netCDF data

      integer               ncid, varid, ncstatus

      ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,ncid=ncid)
      ncstatus = nf90_inq_varid(ncid,trim(adjustl(varname)),varid)
      ncstatus = nf90_get_att(ncid,varid,trim(adjustl(attribute)),varvalue)
      ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdfio_variable_attr_char

  !=======================================================================

  ! netcdfio_values_1d_dblepr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_1d_dblepr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    real(r_double)                                                       :: varvalue(:)

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus /= 0) then
       varvalue = -1.e30
    else
       ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue)
       if (ncstatus .ne. 0) then
          print *,'fv3 read failed for ',trim(adjustl(varname))
          call mpi_interface_terminate()
          stop
       endif
    endif
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdfio_values_1d_dblepr

  !=======================================================================

  ! netcdfio_values_2d_dblepr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_2d_dblepr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    real(r_double),             dimension(ncxdim,ncydim)                 :: varvalue

    ! Define variables computed within routine
    
    integer,                    dimension(3)                             :: start
    integer,                    dimension(3)                             :: count

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(ncstatic)       start = (/1,1,1/)
    if(.not. ncstatic) start = (/1,1,ncrec/)
    count    = (/ncxdim,ncydim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(debug) write(6,500) trim(adjustl(varname)), minval(varvalue),      &
         & maxval(varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

    ! Define format statements

500 format('NETCDFIO_VALUES_2D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

    !=====================================================================

  end subroutine netcdfio_values_2d_dblepr

  !=======================================================================

  ! netcdfio_values_3d_dblepr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_3d_dblepr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    real(r_double),             dimension(ncxdim,ncydim,nczdim)          :: varvalue

    ! Define variables computed within routine
    
    integer,                    dimension(4)                             :: start
    integer,                    dimension(4)                             :: count

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(ncstatic)       start = (/1,1,1,1/)
    if(.not. ncstatic) start = (/1,1,1,ncrec/)
    count    = (/ncxdim,ncydim,nczdim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(debug) write(6,500) trim(adjustl(varname)), minval(varvalue),      &
         & maxval(varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

    ! Define format statements

500 format('NETCDFIO_VALUES_3D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

    !=====================================================================

  end subroutine netcdfio_values_3d_dblepr

  !=======================================================================

  ! netcdfio_values_1d_realpr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_1d_realpr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    real(r_kind)                                                         :: varvalue(:)

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if (ncstatus /= 0) then
       varvalue = -1.e30
    else
       ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue)
       if (ncstatus .ne. 0) then
           print *,'fv3 read failed for ',trim(adjustl(varname))
           call mpi_interface_terminate()
           stop
       endif
    endif
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdfio_values_1d_realpr

  !=======================================================================

  ! netcdfio_values_2d_realpr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_2d_realpr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    real(r_kind),               dimension(ncxdim,ncydim)                 :: varvalue

    ! Define variables computed within routine

    integer,                    dimension(3)                             :: start
    integer,                    dimension(3)                             :: count

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(ncstatic)       start = (/1,1,1/)
    if(.not. ncstatic) start = (/1,1,ncrec/)
    count    = (/ncxdim,ncydim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(debug) write(6,500) trim(adjustl(varname)), minval(varvalue),      &
         & maxval(varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

    ! Define format statements

500 format('NETCDFIO_VALUES_2D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

    !=====================================================================

  end subroutine netcdfio_values_2d_realpr

  !=======================================================================

  ! netcdfio_values_3d_realpr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_3d_realpr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    real(r_kind),               dimension(ncxdim,ncydim,nczdim)          :: varvalue

    ! Define variables computed within routine
    
    integer,                    dimension(4)                             :: start
    integer,                    dimension(4)                             :: count

    !=====================================================================

    ! Define local variables
    
    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(ncstatic)       start = (/1,1,1,1/)
    if(.not. ncstatic) start = (/1,1,1,ncrec/)
    count    = (/ncxdim,ncydim,nczdim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(debug) write(6,500) trim(adjustl(varname)), minval(varvalue),      &
         & maxval(varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

    ! Define format statements

500 format('NETCDFIO_VALUES_3D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

    !=====================================================================

  end subroutine netcdfio_values_3d_realpr

  !=======================================================================

  ! netcdfio_values_1d_intepr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_1d_intepr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    integer                                                              :: varvalue(:)

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus /= 0) then
       varvalue = -9999
    else
       ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue)
    endif
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdfio_values_1d_intepr

  !=======================================================================

  ! netcdfio_values_2d_intepr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_2d_intepr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    integer,                    dimension(ncxdim,ncydim)                 :: varvalue

    ! Define variables computed within routine
    
    integer,                    dimension(3)                             :: start
    integer,                    dimension(3)                             :: count

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if(ncstatic)       start = (/1,1,1/)
    if(.not. ncstatic) start = (/1,1,ncrec/)
    count    = (/ncxdim,ncydim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    if(debug) write(6,500) trim(adjustl(varname)), minval(varvalue),      &
         & maxval(varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

    ! Define format statements

500 format('NETCDFIO_VALUES_2D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

    !=====================================================================

  end subroutine netcdfio_values_2d_intepr

  !=======================================================================

  ! netcdfio_values_3d_intepr.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_values_3d_intepr(filename,varname,varvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: varname
    integer,                    dimension(ncxdim,ncydim,nczdim)          :: varvalue

    ! Define variables computed within routine
    
    integer,                    dimension(4)                             :: start
    integer,                    dimension(4)                             :: count

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    if (ncstatus .ne. 0) then
        print *,'fv3 read failed for ',trim(adjustl(varname))
        call mpi_interface_terminate()
        stop
    endif
    if(ncstatic)       start = (/1,1,1,1/)
    if(.not. ncstatic) start = (/1,1,1,ncrec/)
    count    = (/ncxdim,ncydim,nczdim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    if(debug) write(6,500) trim(adjustl(varname)), minval(varvalue),      &
         & maxval(varvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

    ! Define format statements

500 format('NETCDFIO_VALUES_3D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

    !=====================================================================

  end subroutine netcdfio_values_3d_intepr

  !=======================================================================

  ! netcdfio_dimension.f90:

  !-----------------------------------------------------------------------

  subroutine netcdfio_dimension(filename,dimname,dimvalue)

    ! Define variables passed to routine

    character(len=500)                                                   :: filename
    character(len=*)                                                     :: dimname
    integer                                                              :: dimvalue

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_open(path=trim(adjustl(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_dimid(ncfileid,trim(adjustl(dimname)),ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=dimvalue)
    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdfio_dimension

  !=======================================================================

end module netcdfio_interface
