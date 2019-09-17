 module interp

 use esmf

 implicit none

 private

 type(esmf_field), public           :: soil_type_from_input_grid
                                       ! soil type interpolated from
                                       ! input grid

 public :: interp_sfc

 contains

 subroutine interp_sfc

 use input_data
 use model_grid
 use gldas_data

 implicit none

 integer :: rc, isrctermprocessing

 integer(esmf_kind_i4), pointer     :: mask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_target_ptr(:,:)
 integer(esmf_kind_i4), pointer      :: unmapped_ptr(:)

 real(esmf_kind_r8), pointer         :: gldas_target_ptr(:,:)
 real(esmf_kind_r8), pointer         :: gdas_input_ptr(:,:)

 type(esmf_regridmethod_flag)        :: method
 type(esmf_routehandle)              :: regrid_land

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID SOIL TYPE."
 soil_type_from_input_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

!----------------------------------------------------------------------------
! Interpolate all land to all land
!----------------------------------------------------------------------------

 print*,"- CALL GridAddItem FOR TARGET GRID."
 call ESMF_GridAddItem(target_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR TARGET GRID."
 nullify(mask_target_ptr)
 call ESMF_GridGetItem(target_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 print*,"- CALL FieldGet FOR TARGET GRID MASK."
 nullify(gldas_target_ptr)
 call ESMF_FieldGet(landsea_mask_target_grid, farrayPtr=gldas_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 mask_target_ptr = 0
 where(nint(gldas_target_ptr)==1) mask_target_ptr = 1

 print*,"- CALL GridAddItem FOR INPUT GRID."
 call ESMF_GridAddItem(input_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridAddItem", rc)

 print*,"- CALL GridGetItem FOR INPUT GRID."
 nullify(mask_input_ptr)
 call ESMF_GridGetItem(input_grid, &
                       itemflag=ESMF_GRIDITEM_MASK, &
                       farrayPtr=mask_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN GridGetItem", rc)

 print*,"- CALL FieldGet FOR INPUT GRID MASK."
 nullify(gdas_input_ptr)
 call ESMF_FieldGet(landsea_mask_input_grid, farrayPtr=gdas_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 mask_input_ptr = 0
 where(nint(gdas_input_ptr)==1) mask_input_ptr = 1

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

 print*,"- CALL Field_Regrid for soil type over land."
 call ESMF_FieldRegrid(soil_type_input_grid, &
                       soil_type_from_input_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)



 end subroutine interp_sfc

 end module interp
