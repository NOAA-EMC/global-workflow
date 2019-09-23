 module write_data

 use esmf
 use model_grid
 use interp

 implicit none

 contains

 subroutine update_gdas_file(localpet)

 integer, intent(in) :: localpet

 integer             :: error, tile

 real (esmf_kind_r8), allocatable :: data_one_tile(:,:)

 if (localpet == 0) then
   allocate (data_one_tile(i_gdas, j_gdas))
 else
   allocate (data_one_tile(0,0))
 endif

 do tile = 1, 6

   print*,"- CALL FieldGather FOR TILE: ", tile
   call ESMF_FieldGather(soil_type_target_grid, data_one_tile,  &
                         rootPet=0, tile=tile, rc=error)
   if(ESMF_logFoundError(rcToCheck=error,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldGather", error)

   if (localpet == 0) then
     print*,'update file for tile ',tile, maxval(data_one_tile), &
                                          minval(data_one_tile)
   endif

 enddo  ! tile number


 deallocate (data_one_tile)

 end subroutine update_gdas_file

 end module write_data
