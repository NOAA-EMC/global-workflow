 module interp

 use esmf
 use input_data
 use model_grid

 type(esmf_field), public           :: soil_type_from_input_grid
 type(esmf_field), public           :: soil_type_target_grid
 type(esmf_field), public           :: lsmask_target_grid
 type(esmf_field), public           :: soilm_tot_target_grid
 type(esmf_field), public           :: soilm_liq_target_grid
 type(esmf_field), public           :: soil_temp_target_grid

 contains

 subroutine interp_sfc(localpet)

 implicit none

 integer, intent(in) :: localpet

 integer             :: rc, isrctermprocessing

 integer(esmf_kind_i4), pointer     :: lsmask_target_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: lsmask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_target_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer      :: unmapped_ptr(:)

 type(esmf_regridmethod_flag)        :: method
 type(esmf_routehandle)              :: regrid_land

 call set_gdas_mask(localpet)

 print*,"- CALL FieldCreate FOR target gdas soil type."
 soil_type_from_input_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target gdas smc."
 soilm_tot_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, &
                                     ungriddedLBound=(/1/), &
                                     ungriddedUBound=(/lsoil/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target gdas slc."
 soilm_liq_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, &
                                     ungriddedLBound=(/1/), &
                                     ungriddedUBound=(/lsoil/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target gdas stc."
 soil_temp_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, &
                                     ungriddedLBound=(/1/), &
                                     ungriddedUBound=(/lsoil/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

!----------------------------------------------------------------------------
! Interpolate all land to all land
!----------------------------------------------------------------------------

 print*,"- CALL GridAddItem FOR TARGET GRID."
 call ESMF_GridAddItem(gdas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR TARGET GRID."
 nullify(mask_target_ptr)
 call ESMF_GridGetItem(gdas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 call ESMF_FieldGet(lsmask_target_grid, farrayPtr=lsmask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

!mask_target_ptr = 1
 mask_target_ptr = lsmask_target_ptr

 print*,"- CALL GridAddItem FOR INPUT GRID."
 call ESMF_GridAddItem(gldas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR INPUT GRID."
 nullify(mask_input_ptr)
 call ESMF_GridGetItem(gldas_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 call ESMF_FieldGet(lsmask_input_grid, farrayPtr=lsmask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

!mask_input_ptr = 1
 mask_input_ptr = lsmask_input_ptr

 method=ESMF_REGRIDMETHOD_NEAREST_STOD
 isrctermprocessing = 1

 print*,"- CALL FieldRegridStore for land to land."
 call ESMF_FieldRegridStore(soil_type_input_grid, &
                            soil_type_from_input_grid, &
                            srcmaskvalues=(/0/), &
                            dstmaskvalues=(/0/), &
                            polemethod=ESMF_POLEMETHOD_NONE, &
                            srctermprocessing=isrctermprocessing, &
                            unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
                            routehandle=regrid_land, &
                            regridmethod=method, &
                            unmappedDstList=unmapped_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegridStore", rc)

 print*,"- CALL Field_Regrid for soil type."
 call ESMF_FieldRegrid(soil_type_input_grid, &
                       soil_type_from_input_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for soil temp."
 call ESMF_FieldRegrid(soil_temp_input_grid, &
                       soil_temp_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for slc."
 call ESMF_FieldRegrid(soilm_liq_input_grid, &
                       soilm_liq_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for smc."
 call ESMF_FieldRegrid(soilm_tot_input_grid, &
                       soilm_tot_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 end subroutine interp_sfc

 subroutine set_gdas_mask(localpet)

 use netcdf 

 implicit none

 integer, intent(in) :: localpet

 character(len=100)  :: gdas_files(6)

 integer :: rc, error, ncid, tile, id_var, i, j

 integer (esmf_kind_i4), allocatable :: final_mask_one_tile(:,:)

 real (kind=4), allocatable :: mask_one_tile(:,:)
 real (esmf_kind_r8), allocatable :: soilt_one_tile(:,:)
 real (kind=4), allocatable :: snow_one_tile(:,:)
 real (kind=4), allocatable :: soilm_one_tile(:,:,:)

 print*,"- CALL FieldCreate FOR target landmask."
 lsmask_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_I4, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR target soil type."
 soil_type_target_grid = ESMF_FieldCreate(gdas_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 if (localpet == 0) then
   allocate (final_mask_one_tile(i_gdas, j_gdas))
   allocate (mask_one_tile(i_gdas, j_gdas))
   allocate (soilm_one_tile(i_gdas, j_gdas, lsoil))
   allocate (snow_one_tile(i_gdas, j_gdas))
   allocate (soilt_one_tile(i_gdas, j_gdas))
 else
   allocate (final_mask_one_tile(0,0))
   allocate (mask_one_tile(0,0))
   allocate (soilm_one_tile(0,0,0))
   allocate (snow_one_tile(0,0))
   allocate (soilt_one_tile(0,0))
 endif

 gdas_files(1) = "./sfc_data.tile1.nc"
 gdas_files(2) = "./sfc_data.tile2.nc"
 gdas_files(3) = "./sfc_data.tile3.nc"
 gdas_files(4) = "./sfc_data.tile4.nc"
 gdas_files(5) = "./sfc_data.tile5.nc"
 gdas_files(6) = "./sfc_data.tile6.nc"
 
 do tile = 1, 6

   if(localpet == 0) then

     print*,'open ',trim(gdas_files(tile))
     error = nf90_open(trim(gdas_files(tile)), nf90_nowrite, ncid)
     call netcdf_err(error, 'opening gdas surface file')

     error = nf90_inq_varid(ncid, 'slmsk', id_var)
     call netcdf_err(error, 'getting mask id')
     error = nf90_get_var(ncid, id_var, mask_one_tile)
     call netcdf_err(error, 'reading mask')
     print*,'gdas mask ',maxval(mask_one_tile),minval(mask_one_tile)

     error = nf90_inq_varid(ncid, 'stype', id_var)
     call netcdf_err(error, 'getting stype id')
     error = nf90_get_var(ncid, id_var, soilt_one_tile)
     call netcdf_err(error, 'reading stype')
     print*,'gdas stype ',maxval(soilt_one_tile),minval(soilt_one_tile)

     error = nf90_inq_varid(ncid, 'sheleg', id_var)
     call netcdf_err(error, 'getting sheleg id')
     error = nf90_get_var(ncid, id_var, snow_one_tile)
     call netcdf_err(error, 'reading snow')
     print*,'gdas snow ',maxval(snow_one_tile),minval(snow_one_tile)

     error = nf90_inq_varid(ncid, 'smc', id_var)
     call netcdf_err(error, 'getting smc id')
     error = nf90_get_var(ncid, id_var, soilm_one_tile)
     call netcdf_err(error, 'reading soilm')
     print*,'gdas soilm ',maxval(soilm_one_tile),minval(soilm_one_tile)

     final_mask_one_tile = 1

     do j = 1, j_gdas
     do i = 1, i_gdas
       if (mask_one_tile(i,j) < 0.5 .or. mask_one_tile(i,j) > 1.5) then
         final_mask_one_tile(i,j) = 0
       endif
       if (soilm_one_tile(i,j,1) > 0.9) then
         final_mask_one_tile(i,j) = 0
       endif
       if (snow_one_tile(i,j) > 0.0) then
         final_mask_one_tile(i,j) = 0
       endif
     enddo
     enddo

     error = nf90_close (ncid)

   endif  ! is this localpet 0?

   print*,"- CALL FieldScatter FOR gdas mask."
   call ESMF_FieldScatter(lsmask_target_grid, final_mask_one_tile, rootpet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)
   
   print*,"- CALL FieldScatter FOR gdas soil type."
   call ESMF_FieldScatter(soil_type_target_grid, soilt_one_tile, rootpet=0, tile=tile, rc=rc)
   if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)

 enddo  ! tile 

 deallocate (soilt_one_tile, mask_one_tile, snow_one_tile, soilm_one_tile, final_mask_one_tile)

 end subroutine set_gdas_mask

 end module interp
