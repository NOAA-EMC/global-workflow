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

 integer             :: error, tile, ncid, id_var, i, j

 integer(esmf_kind_i4), allocatable :: mask_one_tile(:,:)

 real (esmf_kind_r8), allocatable :: data_one_tile(:,:), data_one_tile3d(:,:,:)
 real (kind=4)      , allocatable :: dum2d(:,:), dum3d(:,:,:)

 if (localpet == 0) then
   allocate (data_one_tile(i_gdas, j_gdas))
   allocate (mask_one_tile(i_gdas, j_gdas))
   allocate (data_one_tile3d(i_gdas, j_gdas,lsoil))
   allocate (dum2d(i_gdas, j_gdas))
   allocate (dum3d(i_gdas, j_gdas, lsoil))
 else
   allocate (data_one_tile(0,0))
   allocate (mask_one_tile(0,0))
   allocate (data_one_tile3d(0,0,0))
   allocate (dum2d(0,0))
   allocate (dum3d(0,0,0))
 endif

 gdas_files(1) = "./sfc_data.tile1.nc"
 gdas_files(2) = "./sfc_data.tile2.nc"
 gdas_files(3) = "./sfc_data.tile3.nc"
 gdas_files(4) = "./sfc_data.tile4.nc"
 gdas_files(5) = "./sfc_data.tile5.nc"
 gdas_files(6) = "./sfc_data.tile6.nc"

 do tile = 1, 6

   if (localpet == 0) then
     print*,'open file ',trim(gdas_files(tile))
     error = nf90_open(trim(gdas_files(tile)), nf90_write, ncid)
     call netcdf_err(error, 'opening gdas surface file')
   endif

   print*,"- CALL FieldGather FOR gdas interp mask TILE: ", tile
   call ESMF_FieldGather(lsmask_target_grid, mask_one_tile,  &
                         rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldGather", error)

!  print*,"- CALL FieldGather FOR TILE: ", tile
!  call ESMF_FieldGather(soil_type_from_input_grid, data_one_tile,  &
!                        rootPet=0, tile=tile, rc=error)
!  if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
!    call error_handler("IN FieldGather", error)

!  if (localpet == 0) then
!    print*,'update soil type for tile ',tile, maxval(data_one_tile), &
!                                         minval(data_one_tile)
!    error = nf90_inq_varid(ncid, 'stype', id_var)
!    call netcdf_err(error, 'finding soil type')
!    dum2d = data_one_tile
!    error = nf90_put_var(ncid, id_var, dum2d)
!    call netcdf_err(error, 'writing soil type')
!  endif

   print*,"- CALL FieldGather FOR TILE: ", tile
   call ESMF_FieldGather(soilm_tot_target_grid, data_one_tile3d,  &
                         rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     print*,'update soilm tot for tile ',tile, maxval(data_one_tile3d), &
                                          minval(data_one_tile3d)

     error = nf90_inq_varid(ncid, 'smc', id_var)
     call netcdf_err(error, 'finding smc')

     error = nf90_get_var(ncid, id_var, dum3d)
     call netcdf_err(error, 'reading smc')

     do j = 1, j_gdas
     do i = 1, i_gdas
       if (mask_one_tile(i,j) == 1) then
         dum3d(i,j,:) = data_one_tile3d(i,j,:)
       endif
     enddo
     enddo

     error = nf90_put_var(ncid, id_var, dum3d)
     call netcdf_err(error, 'writing smc')
   endif
     
   print*,"- CALL FieldGather FOR TILE: ", tile
   call ESMF_FieldGather(soilm_liq_target_grid, data_one_tile3d,  &
                         rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     print*,'update soilm liq for tile ',tile, maxval(data_one_tile3d), &
                                          minval(data_one_tile3d)

     error = nf90_inq_varid(ncid, 'slc', id_var)
     call netcdf_err(error, 'finding slc')

     error = nf90_get_var(ncid, id_var, dum3d)
     call netcdf_err(error, 'reading slc')

     do j = 1, j_gdas
     do i = 1, i_gdas
       if (mask_one_tile(i,j) == 1) then
         dum3d(i,j,:) = data_one_tile3d(i,j,:)
       endif
     enddo
     enddo

     error = nf90_put_var(ncid, id_var, dum3d)
     call netcdf_err(error, 'writing slc')

   endif

   print*,"- CALL FieldGather FOR TILE: ", tile
   call ESMF_FieldGather(soil_temp_target_grid, data_one_tile3d,  &
                         rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     print*,'update soil temp for tile ',tile, maxval(data_one_tile3d), &
                                          minval(data_one_tile3d)

     error = nf90_inq_varid(ncid, 'stc', id_var)
     call netcdf_err(error, 'finding stc')

     error = nf90_get_var(ncid, id_var, dum3d)
     call netcdf_err(error, 'reading stc')

     do j = 1, j_gdas
     do i = 1, i_gdas
       if (mask_one_tile(i,j) == 1) then
         dum3d(i,j,:) = data_one_tile3d(i,j,:)
       endif
     enddo
     enddo

     error = nf90_put_var(ncid, id_var, dum3d)
     call netcdf_err(error, 'writing stc')

   endif

   if (localpet == 0) error = nf90_close(ncid)

 enddo  ! tile number

 deallocate (data_one_tile, mask_one_tile)

 end subroutine update_gdas_file

 end module write_data
