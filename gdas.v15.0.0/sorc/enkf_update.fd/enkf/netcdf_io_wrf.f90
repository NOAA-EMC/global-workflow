module netcdf_io

    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_nowrite,nf90_global
    use netcdf, only: nf90_inquire_dimension,nf90_inquire_variable
    use netcdf, only: nf90_inq_dimid,nf90_inq_varid
    use netcdf, only: nf90_get_var,nf90_put_var
    use netcdf, only: nf90_get_att,nf90_put_att
    use netcdf_mod, only: nc_check

    implicit none

    private
    public :: readnetcdfdata, writenetcdfdata, variableattribute_char, &
              netcdfdimension, globalattribute_real
contains

      ! Subroutine will ingest a netcdf file and character strings, definining 
      ! the user specified dimensions, and return an array of variable integer 
      ! dimensions contained within the respective file

      subroutine netcdfdimension(netcdf_input,ndims,dimstring,dims)

      implicit none

      !=======================================================================

      ! Define array dimension variables

      integer,intent(in) :: ndims

      ! Define variables passed to subroutine

      character(len=500),intent(in) ::        netcdf_input
      character*20,intent(in) ::        dimstring(ndims)
      integer,intent(out) ::              dims(ndims)

      ! Define variables for decoding netCDF data

      character*20         dimname
      integer              ncid, dimid

      ! Define counting variables

      integer              i

      !=======================================================================

      ! Loop through dimensions of all arrays passed to subroutine

      do i = 1, ndims

         ! Open analysis netCDF file passed to routine

         call nc_check( nf90_open(netcdf_input,nf90_nowrite,ncid),&
             '','open '//trim(netcdf_input) )
    
         ! Get record id of variable field
    
         !dimid = ncdid(ncid,dimstring(i),rcode)
         call nc_check( nf90_inq_dimid(ncid,dimstring(i),dimid),&
             '','inq_dimid '//trim(dimstring(i))//' '//trim(netcdf_input) )
   
         ! Get dimension name and size
    
         !call ncdinq(ncid,dimid,dimname,dims(i),rcode)
         call nc_check( nf90_inquire_dimension(ncid,dimid,dimname,dims(i)),&
             '','inquire_dimension '//trim(dimstring(i))//' '//trim(netcdf_input) )

         ! Close analysis netCDF file passed to routine
       
         call nc_check( nf90_close(ncid),&
             '','close '//trim(netcdf_input) )

      end do

      !=======================================================================

      ! Return calculated values

      end subroutine netcdfdimension

      ! Subroutine will ingest a datafile name and the corresponding variable 
      ! identification in the netcdf and return an array of values for the 
      ! respective netcdf variable

      subroutine readnetcdfdata(netcdf_input,varname,varnamestring,&
           xdim,ydim,zdim)

      implicit none

      !=======================================================================

      ! Define array dimension variables

      integer,intent(in) ::              xdim, ydim, zdim

      ! Define variables passed to subroutine

      character(len=500),intent(in) ::        netcdf_input
      character*12,intent(in) ::         varnamestring

      ! Define variable returned by subroutine

      real,intent(out) ::                 varname(xdim,ydim,zdim)

      ! Define variables calculated with subroutine

      integer              start(10), count(10), vdims(10)

      ! Define variables for decoding netCDF data
    
      integer              ncid, varid, nvatts
      integer              nctype, nvdim, ndsize
      character*1024       strbuf

      ! Define counting variables

      integer              i

      !=======================================================================

      start = 1; count = 1

      ! Open netcdf file passed to subroutine

      call nc_check( nf90_open(netcdf_input,nf90_nowrite,ncid),&
          '','open '//trim(netcdf_input) )

      ! Determine position withing netcdf file of variable

      call nc_check( nf90_inq_varid(ncid,varnamestring,varid),&
          '','inq_varid '//trim(varnamestring)//' '//trim(netcdf_input) )

      ! Retrieve information for requested variable from netcdf file

      !call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
      call nc_check( nf90_inquire_variable(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts),&
          '','inquire_variable '//trim(strbuf)//' '//trim(netcdf_input) )

      !=======================================================================

      ! Begin: Assign array dimensions for variable

      do i = 1, nvdim

         ! Obtain variable dimensions

         !call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
         call nc_check( nf90_inquire_dimension(ncid,vdims(i),strbuf,ndsize),&
             '','inquire_dimension '//trim(strbuf)//' '//trim(netcdf_input) )

         ! Assign hyperslab starting index

         start(i) = 1

         ! Assign hyperslab ending index

         count(i) = ndsize

      end do

      ! End: Assign array dimensions for variable

      !=======================================================================

      ! Get data for requested variable
      
      !call ncvgt(ncid,varid,start,count,varname,rcode)
      call nc_check( nf90_get_var(ncid,varid,varname,start(1:3),count(1:3)),&
          '','get_var '//trim(varnamestring)//' '//trim(netcdf_input) )

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      call nc_check( nf90_close(ncid),&
          '','close '//trim(netcdf_input) )

      ! Return calculated values

      end subroutine readnetcdfdata


      !***********************************************************************

      ! Subroutine will ingest a variable array and write the variable 
      ! field to an external file

      subroutine writenetcdfdata(netcdf_output,varname,varnamestring,&
           xdim,ydim,zdim)

      implicit none

      !=======================================================================

      ! Define array dimension variables

      integer,intent(in) ::              xdim, ydim, zdim

      ! Define variables passed to subroutine

      character(len=500), intent(in) ::        netcdf_output
      character*12, intent(in) ::         varnamestring
      real,intent(in) ::              varname(xdim,ydim,zdim)

      ! Define variables calculated with subroutine

      integer              start(10), count(10), vdims(10)

      ! Define variables for decoding netCDF data
    
      integer              ncid, varid, nvatts
      integer              nctype, nvdim, ndsize
      character*1024       strbuf

      ! Define counting variables

      integer              i

      !=======================================================================

      ! Open netcdf file passed to subroutine

      call nc_check( nf90_open(netcdf_output,nf90_write,ncid),&
          '','open '//trim(netcdf_output) )

      ! Determine position withing netcdf file of variable

      call nc_check( nf90_inq_varid(ncid,varnamestring,varid),&
          '','inq_varid '//trim(varnamestring)//' '//trim(netcdf_output) )

      ! Retrieve information for requested variable from netcdf file

      !call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
      call nc_check( nf90_inquire_variable(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts),&
          '','inquire_variable '//trim(strbuf)//' '//trim(netcdf_output) )

      !=======================================================================

      ! Begin: Assign array dimensions for variable

      do i = 1, nvdim

         ! Obtain variable dimensions

         !call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
         call nc_check( nf90_inquire_dimension(ncid,vdims(i),strbuf,ndsize),&
             '','inquire_dimension '//trim(strbuf)//' '//trim(netcdf_output) )

         ! Assign hyperslab starting index

         start(i) = 1

         ! Assign hyperslab ending index

         count(i) = ndsize

      end do

      ! End: Assign array dimensions for variable

      !=======================================================================

      ! Get data for requested variable
      
      !call ncvpt(ncid,varid,start,count,varname,rcode)
      call nc_check( nf90_put_var(ncid,varid,varname,start(1:3),count(1:3)),&
          '','put_var '//trim(varnamestring)//' '//trim(netcdf_output) )

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      call nc_check( nf90_close(ncid),&
          '','close '//trim(netcdf_output) )

      ! Return calculated values

      end subroutine writenetcdfdata

      ! This subroutine will ingest a netcdf formatted file and return the value 
      ! pertaining to a character-string attribute variable specified  by the 
      ! user

      subroutine variableattribute_char(netcdf_input,varname,attribute,&
           value)

      implicit none

!      include 'netcdf.inc'

      !=======================================================================

      ! Define variables passed to subroutine

      character(len=500),intent(in) ::        netcdf_input
      character*50,intent(in) ::         attribute
      character*12,intent(in) ::         varname

      ! Define variables returned by subroutine

      character*12,intent(out) ::         value

      ! Define variables for decoding netCDF data

      integer               ncid, varid

      !=======================================================================

      ! Open netcdf file passed to subroutine
    
      call nc_check( nf90_open(netcdf_input,nf90_nowrite,ncid),&
          '','open '//trim(netcdf_input) )

      ! Define variable position within netcdf file

      !varid = ncvid(ncid,varname,status)
      call nc_check( nf90_inq_varid(ncid,varname,varid),&
          '','inq_varid '//trim(varname)//' '//trim(netcdf_input) )

      ! Obtain global attribute value

      call nc_check( nf90_get_att(ncid,varid,attribute,value),&
          '','get_att '//trim(attribute)//' '//trim(netcdf_input) )

      !=======================================================================

      ! Close netcdf data file passed to routine

      call nc_check( nf90_close(ncid),&
          '','close '//trim(netcdf_input) )

      ! Return calculated values

      end subroutine variableattribute_char

      !***********************************************************************

      ! This module will ingest a netcdf formatted file and return the value 
      ! pertaining to a real-value global attribute variable specified by the 
      ! user

      subroutine globalattribute_real(netcdf_input,attribute,value)

      implicit none

!      include 'netcdf.inc'

      !=======================================================================

      ! Define variables passed to subroutine

      character(len=500),intent(in) ::        netcdf_input
      character*50,intent(in) ::         attribute

      ! Define variables returned by subroutine

      real,intent(out) ::                 value

      ! Define variables for decoding netCDF data
    
      integer              ncid

      !=======================================================================

      ! Open netcdf file passed to subroutine
    
      !ncid = ncopn(netcdf_input,ncnowrit,rcode)
      call nc_check( nf90_open(netcdf_input,nf90_nowrite,ncid),&
          '','open '//trim(netcdf_input) )

      ! Obtain global attribute value

      !call ncagt(ncid,ncglobal,attribute,value,status)
      call nc_check( nf90_get_att(ncid,nf90_global,attribute,value),&
          '','get_att '//trim(attribute)//' '//trim(netcdf_input) )

      !=======================================================================

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      call nc_check( nf90_close(ncid),&
          '','close '//trim(netcdf_input) )

      ! Return calculated values

      end subroutine globalattribute_real

end module netcdf_io
