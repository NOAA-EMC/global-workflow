 program scrip

!----------------------------------------------------------------------
! Create "scrip" files that describes a gaussian grid.
! Two files are created: the normal gaussian grid and one with
! two extra rows for the N/S poles.
!----------------------------------------------------------------------

 implicit none

 character(len=128)             :: outfile
 character(len=20)              :: title
 character(len=5)               :: idim_ch, jdim_ch, jdimp_ch
 character(len=6)               :: cres

 integer                        :: header_buffer_val = 16384
 integer                        :: fsize=65536, inital = 0
 integer                        :: error, ncid
 integer                        :: i, j, idim, jdim, ijdim
 integer                        :: jdimp
 integer                        :: dim_size, dim_corners, dim_rank
 integer                        :: id_dims, id_center_lat, id_center_lon
 integer                        :: id_imask, id_corner_lat, id_corner_lon
 integer                        :: num_corners = 4
 integer                        :: rank = 2
 integer(kind=4), allocatable   :: mask(:)

 real(kind=8)                   :: corner_lon_src
 real(kind=8)                   :: dx_src, lat_edge
 real(kind=8), allocatable      :: lats(:,:), lons(:,:), dum1d(:)
 real(kind=8), allocatable      :: dum2d(:,:), latsp(:,:), lonsp(:,:)
 real(kind=8), allocatable      :: lats_corner(:,:,:), lons_corner(:,:,:)
 real(kind=8), allocatable      :: latsp_corner(:,:,:), lonsp_corner(:,:,:)
 real(kind=8), allocatable      :: slat(:), wlat(:)

 include "netcdf.inc"

 call getarg(1, cres)

 select case (trim(cres))
   case ("c48","C48")
    idim              = 192  ! cres * 4
    jdim              = 94   ! cres * 2
    jdimp             = 96   ! include two rows for the poles
    idim_ch           = "192"
    jdim_ch           = "94"
    jdimp_ch          = "96"
   case ("c96","C96")
    idim              = 384  ! cres * 4
    jdim              = 192   ! cres * 2
    jdimp             = 194   ! include two rows for the poles
    idim_ch           = "384"
    jdim_ch           = "192"
    jdimp_ch          = "194"
   case ("c128","C128")
    idim              = 512  ! cres * 4
    jdim              = 256  ! cres * 2
    jdimp             = 258  ! include two rows for the poles
    idim_ch           = "512"
    jdim_ch           = "256"
    jdimp_ch          = "258"
   case ("c192","C192")
    idim              = 768  ! cres * 4
    jdim              = 384  ! cres * 2
    jdimp             = 386  ! include two rows for the poles
    idim_ch           = "768"
    jdim_ch           = "384"
    jdimp_ch          = "386"
   case ("c384","C384")
    idim              = 1536 ! cres * 4
    jdim              = 768  ! cres * 2
    jdimp             = 770  ! include two rows for the poles
    idim_ch           = "1536"
    jdim_ch           = "768"
    jdimp_ch          = "770"
   case ("c768","C768")
    idim              = 3072 ! cres * 4
    jdim              = 1536 ! cres * 2
    jdimp             = 1538 ! include two rows for the poles
    idim_ch           = "3072"
    jdim_ch           = "1536"
    jdimp_ch          = "1538"
   case ("c1152","C1152")
    idim              = 4608  ! cres * 4
    jdim              = 2304  ! cres * 2
    jdimp             = 2306  ! include two rows for the poles
    idim_ch           = "4608"
    jdim_ch           = "2304"
    jdimp_ch          = "2306"
   case ("c3072","C3072")
    idim              = 12288  ! cres * 4
    jdim              = 6144   ! cres * 2
    jdimp             = 6146   ! include two rows for the poles
    idim_ch           = "12288"
    jdim_ch           = "6144"
    jdimp_ch          = "6146"
   case default
    print*,'- Resolution not supported ', trim(cres)
    stop 3
 end select

 corner_lon_src    = 0.0
 dx_src            = 360.0 / float(idim)
 ijdim             = idim*jdim

 allocate(slat(jdim))
 allocate(wlat(jdim))

 call splat(4, jdim, slat, wlat)

 allocate(lats(idim,jdim))
 allocate(lats_corner(num_corners,idim,jdim))
 allocate(lons(idim,jdim))
 allocate(lons_corner(num_corners,idim,jdim))

 do j = 1, jdim
   lats(:,j) = 90.0 - (acos(slat(j))* 180.0 / (4.*atan(1.)))
 enddo

 deallocate(slat, wlat)

!----------------------------------------------------------------
! First, output file without poles.
!----------------------------------------------------------------

!----------------------------------------------------------------
! Set corners in counter-clockwise order
!
!  2     1
!
!     C
!
!  3     4
!----------------------------------------------------------------

 lats_corner(1,:,1) = 90.0
 lats_corner(2,:,1) = 90.0

 lats_corner(3,:,jdim) = -90.0
 lats_corner(4,:,jdim) = -90.0

 do j = 1, jdim - 1
   lat_edge = (lats(1,j) + lats(1,j+1)) / 2.0
   lats_corner(3,:,j) = lat_edge
   lats_corner(4,:,j) = lat_edge
   lats_corner(1,:,j+1) = lat_edge
   lats_corner(2,:,j+1) = lat_edge
 enddo

 do i = 1, idim
   lons(i,:) = corner_lon_src + float(i-1)*dx_src
   lons_corner(1,i,:) = lons(i,:) + (dx_src*0.5)
   lons_corner(2,i,:) = lons(i,:) - (dx_src*0.5)
   lons_corner(3,i,:) = lons(i,:) - (dx_src*0.5)
   lons_corner(4,i,:) = lons(i,:) + (dx_src*0.5)
 enddo

 i = 1
 j = 1
 print*,'center ',lats(i,j),lons(i,j)
 print*,'corner 1 ',lats_corner(1,i,j),lons_corner(1,i,j)
 print*,'corner 2 ',lats_corner(2,i,j),lons_corner(2,i,j)
 print*,'corner 3 ',lats_corner(3,i,j),lons_corner(3,i,j)
 print*,'corner 4 ',lats_corner(4,i,j),lons_corner(4,i,j)

 i = 1
 j = 2
 print*,'center ',lats(i,j),lons(i,j)
 print*,'corner 1 ',lats_corner(1,i,j),lons_corner(1,i,j)
 print*,'corner 2 ',lats_corner(2,i,j),lons_corner(2,i,j)
 print*,'corner 3 ',lats_corner(3,i,j),lons_corner(3,i,j)
 print*,'corner 4 ',lats_corner(4,i,j),lons_corner(4,i,j)

 i = 1
 j = jdim
 print*,'center ',lats(i,j),lons(i,j)
 print*,'corner 1 ',lats_corner(1,i,j),lons_corner(1,i,j)
 print*,'corner 2 ',lats_corner(2,i,j),lons_corner(2,i,j)
 print*,'corner 3 ',lats_corner(3,i,j),lons_corner(3,i,j)
 print*,'corner 4 ',lats_corner(4,i,j),lons_corner(4,i,j)

 i = 1
 j = jdim-1
 print*,'center ',lats(i,j),lons(i,j)
 print*,'corner 1 ',lats_corner(1,i,j),lons_corner(1,i,j)
 print*,'corner 2 ',lats_corner(2,i,j),lons_corner(2,i,j)
 print*,'corner 3 ',lats_corner(3,i,j),lons_corner(3,i,j)
 print*,'corner 4 ',lats_corner(4,i,j),lons_corner(4,i,j)

 allocate(mask(ijdim))
 mask = 1

! output file without pole.

 outfile = " "
 outfile = "./gaussian." // trim(idim_ch) // "." // trim(jdim_ch) // ".nc"
 title   = " "
 title   = "gaussian." // trim(idim_ch) // "." // trim(jdim_ch)

!--- open the file
 error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
 print*, 'error after open ', error

!--- define dimension
 error = nf_def_dim(ncid, 'grid_size', ijdim, dim_size)
 error = nf_def_dim(ncid, 'grid_corners', num_corners, dim_corners)
 error = nf_def_dim(ncid, 'grid_rank', rank, dim_rank)

!--- define field
 error = nf_def_var(ncid, 'grid_dims', NF_INT, 1, (/dim_rank/), id_dims)
 error = nf_def_var(ncid, 'grid_center_lat', NF_DOUBLE, 1, (/dim_size/), id_center_lat)
 error = nf_put_att_text(ncid, id_center_lat, "units", 7, "degrees")
 error = nf_def_var(ncid, 'grid_center_lon', NF_DOUBLE, 1, (/dim_size/), id_center_lon)
 error = nf_put_att_text(ncid, id_center_lon, "units", 7, "degrees")
 error = nf_def_var(ncid, 'grid_imask', NF_INT, 1, (/dim_size/), id_imask)
 error = nf_put_att_text(ncid, id_imask, "units", 8, "unitless")
 error = nf_def_var(ncid, 'grid_corner_lat', NF_DOUBLE, 2, (/dim_corners,dim_size/), id_corner_lat)
 error = nf_put_att_text(ncid, id_corner_lat, "units", 7, "degrees")
 error = nf_def_var(ncid, 'grid_corner_lon', NF_DOUBLE, 2, (/dim_corners,dim_size/), id_corner_lon)
 error = nf_put_att_text(ncid, id_corner_lon, "units", 7, "degrees")
 error = nf_put_att_text(ncid, NF_GLOBAL, "title", 20, trim(title))
 error = nf__enddef(ncid, header_buffer_val,4,0,4)

!--- set fields
 error = nf_put_var_int( ncid, id_dims, (/idim,jdim/))

 allocate(dum1d(ijdim))
 dum1d = reshape(lats, (/ijdim/))
 error = nf_put_var_double( ncid, id_center_lat, dum1d)
 dum1d = reshape(lons, (/ijdim/))
 error = nf_put_var_double( ncid, id_center_lon, dum1d)
 deallocate(dum1d)

 error = nf_put_var_int( ncid, id_imask, mask)
 deallocate(mask)

 allocate(dum2d(num_corners,ijdim))
 dum2d = reshape (lats_corner, (/num_corners,ijdim/))
 error = nf_put_var_double( ncid, id_corner_lat, dum2d)

 dum2d = reshape (lons_corner, (/num_corners,ijdim/))
 error = nf_put_var_double( ncid, id_corner_lon, dum2d)
 deallocate(dum2d)

 error = nf_close(ncid)

!----------------------------------------------------------------
! output file with poles.
!----------------------------------------------------------------

 outfile = " "
 outfile = "./gaussian." // trim(idim_ch) // "." // trim(jdimp_ch) // ".nc"
 title   = " "
 title   = "gaussian." // trim(idim_ch) // "." // trim(jdimp_ch)

 ijdim = idim*jdimp

 allocate(latsp(idim,jdimp))
 allocate(lonsp(idim,jdimp))

 do j = 2, jdim+1
   latsp(:,j) = lats(:,j-1)
   lonsp(:,j) = lons(:,j-1)
 enddo

 latsp(:,1) = 90.0_8
 lonsp(:,1) = 0.0_8

 latsp(:,jdimp) = -90.0_8
 lonsp(:,jdimp) = 0.0_8

 deallocate(lats, lons)

 allocate(latsp_corner(num_corners,idim,jdimp))
 allocate(lonsp_corner(num_corners,idim,jdimp))

 latsp_corner(:,:,1) = 89.5_8
 latsp_corner(:,:,jdimp) = -89.5_8

 lonsp_corner(1,:,1) = 0.0_8
 lonsp_corner(2,:,1) = 90.0_8
 lonsp_corner(3,:,1) = 180.0_8
 lonsp_corner(4,:,1) = 270.0_8

 lonsp_corner(1,:,jdimp) = 0.0_8
 lonsp_corner(2,:,jdimp) = 90.0_8
 lonsp_corner(3,:,jdimp) = 180.0_8
 lonsp_corner(4,:,jdimp) = 270.0_8

 do j = 2, jdim+1
   latsp_corner(:,:,j) = lats_corner(:,:,j-1)
   lonsp_corner(:,:,j) = lons_corner(:,:,j-1)
 enddo

 deallocate(lats_corner, lons_corner)

!--- open the file
 error = NF__CREATE(outfile, IOR(NF_NETCDF4,NF_CLASSIC_MODEL), inital, fsize, ncid)
 print*, 'error after open ', error

!--- define dimension
 error = nf_def_dim(ncid, 'grid_size', ijdim, dim_size)
 error = nf_def_dim(ncid, 'grid_corners', num_corners, dim_corners)
 error = nf_def_dim(ncid, 'grid_rank', rank, dim_rank)

!--- define field
 error = nf_def_var(ncid, 'grid_dims', NF_INT, 1, (/dim_rank/), id_dims)
 error = nf_def_var(ncid, 'grid_center_lat', NF_DOUBLE, 1, (/dim_size/), id_center_lat)
 error = nf_put_att_text(ncid, id_center_lat, "units", 7, "degrees")
 error = nf_def_var(ncid, 'grid_center_lon', NF_DOUBLE, 1, (/dim_size/), id_center_lon)
 error = nf_put_att_text(ncid, id_center_lon, "units", 7, "degrees")
 error = nf_def_var(ncid, 'grid_imask', NF_INT, 1, (/dim_size/), id_imask)
 error = nf_put_att_text(ncid, id_imask, "units", 8, "unitless")
 error = nf_def_var(ncid, 'grid_corner_lat', NF_DOUBLE, 2, (/dim_corners,dim_size/), id_corner_lat)
 error = nf_put_att_text(ncid, id_corner_lat, "units", 7, "degrees")
 error = nf_def_var(ncid, 'grid_corner_lon', NF_DOUBLE, 2, (/dim_corners,dim_size/), id_corner_lon)
 error = nf_put_att_text(ncid, id_corner_lon, "units", 7, "degrees")
 error = nf_put_att_text(ncid, NF_GLOBAL, "title", 20, trim(title))
 error = nf__enddef(ncid, header_buffer_val,4,0,4)
 
!--- set fields
 error = nf_put_var_int( ncid, id_dims, (/idim,jdimp/))

 allocate(dum1d(ijdim))
 dum1d = reshape(latsp, (/ijdim/))
 error = nf_put_var_double( ncid, id_center_lat, dum1d)
 dum1d = reshape(lonsp, (/ijdim/))
 error = nf_put_var_double( ncid, id_center_lon, dum1d)
 deallocate(dum1d)

 allocate(mask(ijdim))
 mask = 1
 error = nf_put_var_int( ncid, id_imask, mask)
 deallocate(mask)

 allocate(dum2d(num_corners,ijdim))
 dum2d = reshape (latsp_corner, (/num_corners,ijdim/))
 print*,'lat corner check ',maxval(dum2d),minval(dum2d)
 error = nf_put_var_double( ncid, id_corner_lat, dum2d)
 deallocate(latsp_corner)

 dum2d = reshape (lonsp_corner, (/num_corners,ijdim/))
 error = nf_put_var_double( ncid, id_corner_lon, dum2d)
 deallocate(dum2d, lonsp_corner)

 error = nf_close(ncid)

 print*,'- DONE.'

 end program scrip
