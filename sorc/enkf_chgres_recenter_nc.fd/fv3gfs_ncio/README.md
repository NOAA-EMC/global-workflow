# fv3gfs_ncio
module for reading/writing FV3 netcdf lat/lon data.


* open a Dataset.
```fortran
use module_fv3gfs_ncio
type(Dataset) :: ds
ds = open_dataset('gfs.t00z.atmf240.nc')
```
* read an attribute.
```fortran
real(4), allocatable, dimension(:) :: ak,bk
character(len=32) charatt
! ak,bk are allocated and filled.
call read_attribute(ds, 'ak', ak)
call read_attribute(ds, 'bk', bk)
! read character variable attribute
call read_attribute(ds, 'long_name', charatt, 'vgrd')
```
* read a variable.
```fortran
real(4), allocatable, dimension(:) :: lats,lons
real(4), allocatable, dimension(:,:,:) :: psfc
! arrays must be of correct rank (but not necessarily
! the same type). They are allocated and filled.
! The entire variable is read at once.
call read_vardata(ds,'latitudes',lats)
call read_vardata(ds,'latitudes',lons)
call read_vardata(ds,'pressfc',psfc)
```
* create a new dataset from a template dataset.

A template dataset can be created from a cdl file by running
`ncgen -7 -o <output netcdf filename> <input cdl file>`.
A [cdl](https://www.unidata.ucar.edu/software/netcdf/netcdf/CDL-Syntax.html)
file can be created from an existing dataset by running
`ncdump -cs <existing dataset> > <output cdl file>`. The cdl
text file can be edited with a text editor.
```fortran
type(Dataset) :: dso
! copy_vardata is optional, default .false. means just copy
! variables, dimensions and attributes and coordinate variable 
! data (don't copy all variable data).
dso = create_dataset('gfs.t00z.atmf240_copy.nc', ds, copy_vardata=.true.)
```
* write a variable.
```fortran
real(8), allocatable, dimension(:) :: times
call read_vardata(dso, 'time', times)
! times is now allocated and filled with values from template dataset.
! now overwrite with new values and write back.
times = times + 6 ! add six hours.
call write_vardata(dso, 'time', times)
```
* quantize variable data before writing for better compression.
```fortran
! Lossy compression controlled by parameter nbits (1-31).
! The floating point data is quantized to improve compression
! See doi:10.5194/gmd-10-413-2017.  The method employed
! here is identical to the 'scaled linear packing' method in
! that paper, except that the data are scaling into an arbitrary
! range (2**nbits-1 not just 2**16-1) and are stored as re-scaled floats
! instead of short integers. The zlib algorithm does almost as
! well packing the rescaled floats as it does the scaled
! integers, and this avoids the need for the client to apply the
! rescaling (plus it allows the ability to adjust the packing range).
data_save = data
nbits = 14
call quantize_data(data_save, data, nbits, compress_err)
! compress_err is the max abs compression error (can be saved
! as a variable attribute).
```
* write an attribute.
```fortran
charatt = 'hours since 2016-01-04 06:00:00'
call write_attribute(dso, 'units', charatt, 'time')
```
* access Variable and Dimension derived data types.
```fortran
type(Variable) :: var
type(Dimension) :: dim
! see module_fv3gfs_ncio.f90 for type definitions.
! type members can be used to the call netcdf-fortran90 interface
! directly.
var = get_var(ds, 'ugrd')
dim = get_dim(ds, 'time')
```
* close a dataset.
```fortran
call close_dataset(ds)
call close_dataset(dso)
