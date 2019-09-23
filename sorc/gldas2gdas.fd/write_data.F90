 module write_data

 use esmf
 use model_grid
 use interp

 implicit none

 contains

 subroutine update_gdas_file(localpet)

 use netcdf

 integer, intent(in) :: localpet

 character(len=100)  :: gdas_files(6)

 integer             :: error, tile, ncid, id_var

 real (esmf_kind_r8), allocatable :: data_one_tile(:,:)
 real (kind=4)      , allocatable :: dum2d(:,:)

 if (localpet == 0) then
   allocate (data_one_tile(i_gdas, j_gdas))
   allocate (dum2d(i_gdas, j_gdas))
 else
   allocate (data_one_tile(0,0))
   allocate (dum2d(0,0))
 endif

 gdas_files(1) = "./sfc_data.tile1.nc"
 gdas_files(2) = "./sfc_data.tile2.nc"
 gdas_files(3) = "./sfc_data.tile3.nc"
 gdas_files(4) = "./sfc_data.tile4.nc"
 gdas_files(5) = "./sfc_data.tile5.nc"
 gdas_files(6) = "./sfc_data.tile6.nc"

 do tile = 1, 6

   print*,"- CALL FieldGather FOR TILE: ", tile
   call ESMF_FieldGather(soil_type_target_grid, data_one_tile,  &
                         rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     print*,'update file for tile ',tile, maxval(data_one_tile), &
                                          minval(data_one_tile)
     
     print*,'open file ',trim(gdas_files(tile))

     error = nf90_open(trim(gdas_files(tile)), nf90_write, ncid)
     call netcdf_err(error, 'opening gdas surface file')

     error = nf90_inq_varid(ncid, 'stype', id_var)
     call netcdf_err(error, 'finding soil type')

     dum2d = data_one_tile
     error = nf90_put_var(ncid, id_var, dum2d)
     call netcdf_err(error, 'writing soil type')
     
     error = nf90_close(ncid)


   endif

 enddo  ! tile number


 deallocate (data_one_tile)

 end subroutine update_gdas_file

 end module write_data
