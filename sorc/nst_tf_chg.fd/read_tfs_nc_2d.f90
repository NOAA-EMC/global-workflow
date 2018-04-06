subroutine read_tfs_nc(filename,tf,mask,nlon,nlat)
!
! abstract : read Tf at GFS Gaussin grids in netCDF 
!
  use netcdf
  implicit none
  
  ! This is the name of the data file we will read.
  character (len=6),            intent(in)  :: filename
  integer,                      intent(in)  :: nlat,nlon
  real, dimension(nlon,nlat),   intent(out) :: tf
  integer, dimension(nlon,nlat),   intent(in) :: mask
! Local variables
  real, dimension(nlon,nlat) :: tf_tmp

  integer :: ncid

  integer, parameter :: ndims = 2
  real, parameter :: xoffset = 273.15, scale_factor = 0.01
  character (len = *), parameter :: lat_name = "latitude"
  character (len = *), parameter :: lon_name = "longitude"
  character (len = *), parameter :: tf_name="tf"
  character (len = *), parameter :: mask_name="slmsk"
  integer :: no_fill,fill_value
  integer :: lon_varid, lat_varid, tf_varid, mask_varid

  ! The start and count arrays will tell the netCDF library where to read our data.
  integer, dimension(ndims) :: start, count
  real, dimension(nlat) :: xlats
  real, dimension(nlon) :: xlons

  character (len = *), parameter :: units = "units"
  character (len = *), parameter :: tf_units = "kelvin", mask_units = "none"
  character (len = *), parameter :: lat_units = "degrees_north"
  character (len = *), parameter :: lon_units = "degrees_east"

  integer :: missv
! Loop indices
  integer :: rec, i,j

! To check the units attributes.
  character*80 tf_units_in, mask_units_in
  character*80 lat_units_in, lon_units_in

  write(*,*) 'filename, nlon, nlat : ',filename, nlon, nlat

! Open the file. 
! call nc_check( nf90_open(filename, nf90_netcdf4, ncid) )
  call nc_check( nf90_open(filename, nf90_nowrite, ncid) )

! Get the varids of the latitude and longitude coordinate variables.
! call nc_check( nf90_inq_varid(ncid, lat_name, lat_varid) )
! call nc_check( nf90_inq_varid(ncid, lon_name, lon_varid) )

! Read the time, latitude and longitude data.
! call nc_check( nf90_get_var(ncid, lat_varid, xlats) )
! call nc_check( nf90_get_var(ncid, lon_varid, xlons) )

! Get the varids of the tf & mask netCDF variables.
  call nc_check( nf90_inq_varid(ncid, tf_name, tf_varid) )
! call nc_check( nf90_inq_varid(ncid, mask_name, mask_varid) )

! Get the missing value of analysed_sst
! call nc_check( nf90_get_att(ncid, tf_varid, '_FillValue', missv) )
! write(*,*) 'ostia Tf fill_value, missing_value : ',missv,missing_value

! Read 1 record of nlat*nlon values, starting at the beginning 
! of the record (the (1, 1, rec) element in the netCDF file).
  start = (/ 1, 1 /)
  count = (/ nlon, nlat /)

! Read the tf & mask data from the file, one record at a time.
  call nc_check( nf90_get_var(ncid, tf_varid,   tf_tmp, start, count) )
! call nc_check( nf90_get_var(ncid, mask_varid, mask,   start, count) )

  do j = 1, nlat
     do i = 1, nlon
        if ( mask(i,j) == 0 ) then
           tf(i,j) = real(tf_tmp(i,nlat+1-j))*scale_factor + xoffset
        endif
     enddo
  enddo

! Close the file. This frees up any internal netCDF resources
! associated with the file.
  call nc_check( nf90_close(ncid) )

! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS reading file ", filename, "!"

end subroutine read_tfs_nc

