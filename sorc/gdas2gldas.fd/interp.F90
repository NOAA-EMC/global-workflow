 module interp

 use esmf
 use input_data
 use model_grid
 use gldas_data


 implicit none

 private

 type(esmf_field), public           :: soil_type_from_input_grid
                                       ! soil type interpolated from
                                       ! input grid
 type(esmf_field), public           :: soil_temp_target_grid
 type(esmf_field), public           :: soilm_tot_target_grid
 type(esmf_field), public           :: soilm_liq_target_grid
 type(esmf_field), public           :: veg_greenness_target_grid
 type(esmf_field), public           :: vtype_target_grid
 type(esmf_field), public           :: snoalb_target_grid
 type(esmf_field), public           :: slope_target_grid
 type(esmf_field), public           :: tg3_target_grid
 type(esmf_field), public           :: zorl_target_grid
 type(esmf_field), public           :: orog_target_grid
 type(esmf_field), public           :: snowxy_target_grid
 type(esmf_field), public           :: tvxy_target_grid
 type(esmf_field), public           :: tgxy_target_grid
 type(esmf_field), public           :: canicexy_target_grid
 type(esmf_field), public           :: canliqxy_target_grid
 type(esmf_field), public           :: eahxy_target_grid
 type(esmf_field), public           :: tahxy_target_grid
 type(esmf_field), public           :: cmxy_target_grid
 type(esmf_field), public           :: chxy_target_grid
 type(esmf_field), public           :: fwetxy_target_grid
 type(esmf_field), public           :: sneqvoxy_target_grid
 type(esmf_field), public           :: alboldxy_target_grid
 type(esmf_field), public           :: qsnowxy_target_grid
 type(esmf_field), public           :: wslakexy_target_grid
 type(esmf_field), public           :: zwtxy_target_grid
 type(esmf_field), public           :: waxy_target_grid
 type(esmf_field), public           :: wtxy_target_grid
 type(esmf_field), public           :: lfmassxy_target_grid
 type(esmf_field), public           :: rtmassxy_target_grid
 type(esmf_field), public           :: stmassxy_target_grid
 type(esmf_field), public           :: woodxy_target_grid
 type(esmf_field), public           :: stblcpxy_target_grid
 type(esmf_field), public           :: fastcpxy_target_grid
 type(esmf_field), public           :: xsaixy_target_grid
 type(esmf_field), public           :: xlaixy_target_grid
 type(esmf_field), public           :: taussxy_target_grid
 type(esmf_field), public           :: smcwtdxy_target_grid
 type(esmf_field), public           :: deeprechxy_target_grid
 type(esmf_field), public           :: rechxy_target_grid

 integer, public  :: lsoil_target

 public :: interp_sfc

 contains

 subroutine interp_sfc(localpet)

 implicit none

 integer, intent(in) :: localpet

 integer :: rc, isrctermprocessing

 integer(esmf_kind_i4), pointer     :: mask_input_ptr(:,:)
 integer(esmf_kind_i4), pointer     :: mask_target_ptr(:,:)
 integer(esmf_kind_i4), pointer      :: unmapped_ptr(:)

 real(esmf_kind_r8), pointer         :: gldas_target_ptr(:,:)
 real(esmf_kind_r8), pointer         :: gdas_input_ptr(:,:)

 type(esmf_regridmethod_flag)        :: method
 type(esmf_routehandle)              :: regrid_land

 lsoil_target = lsoil_input

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID orog."
 orog_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID zorl."
 zorl_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID tg3."
 tg3_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID slope."
 slope_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID snoalb."
 snoalb_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID vtype."
 vtype_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID stblcpxy."
 stblcpxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID woodxy."
 woodxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID stmassxy."
 stmassxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID rtmassxy."
 rtmassxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID lfmassxy."
 lfmassxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID wtxy."
 wtxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID waxy."
 waxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID zwtxy."
 zwtxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID wslakexy."
 wslakexy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID qsnowxy."
 qsnowxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID alboldxy."
 alboldxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID sneqvoxy."
 sneqvoxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID fwetxy."
 fwetxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID chxy."
 chxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID cmxy."
 cmxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID tahxy."
 tahxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID eahxy."
 eahxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID canliqxy."
 canliqxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID canicexy."
 canicexy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID tgxy."
 tgxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID tvxy."
 tvxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID snowxy."
 snowxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID fastcpxy."
 fastcpxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID XLAI."
 xlaixy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID XSAI."
 xsaixy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID taussxy."
 taussxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID smcwtdxy."
 smcwtdxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID deeprechxy."
 deeprechxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID rechxy."
 rechxy_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID GREENNESS."
 veg_greenness_target_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INTERPOLATED TARGET GRID SOIL TYPE."
 soil_type_from_input_grid = ESMF_FieldCreate(target_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET SOIL TEMPERATURE."
 soil_temp_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_target/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET TOTAL SOIL MOISTURE."
 soilm_tot_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_target/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR TARGET LIQUID SOIL MOISTURE."
 soilm_liq_target_grid = ESMF_FieldCreate(target_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_target/), rc=rc)
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

!print*,"- CALL FieldGet FOR TARGET GRID MASK."
!nullify(gldas_target_ptr)
!call ESMF_FieldGet(landsea_mask_target_grid, farrayPtr=gldas_target_ptr, rc=rc)
!if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
!   call error_handler("IN FieldGet", rc)

 mask_target_ptr = 1

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

!print*,"- CALL FieldGet FOR INPUT GRID MASK."
!nullify(gdas_input_ptr)
!call ESMF_FieldGet(landsea_mask_input_grid, farrayPtr=gdas_input_ptr, rc=rc)
!if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
!   call error_handler("IN FieldGet", rc)

 mask_input_ptr = 1

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

 print*,"- CALL Field_Regrid for xlai over land."
 call ESMF_FieldRegrid(xlaixy_input_grid, &
                       xlaixy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for landmask."
 call ESMF_FieldRegrid(landsea_mask_input_grid, &
                       landsea_mask_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for canliqxy."
 call ESMF_FieldRegrid(canliqxy_input_grid, &
                       canliqxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for canicexy."
 call ESMF_FieldRegrid(canicexy_input_grid, &
                       canicexy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for tgxy."
 call ESMF_FieldRegrid(tgxy_input_grid, &
                       tgxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for tvxy."
 call ESMF_FieldRegrid(tvxy_input_grid, &
                       tvxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for snowxy."
 call ESMF_FieldRegrid(snowxy_input_grid, &
                       snowxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for eahxy."
 call ESMF_FieldRegrid(eahxy_input_grid, &
                       eahxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for tahxy."
 call ESMF_FieldRegrid(tahxy_input_grid, &
                       tahxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for cmxy."
 call ESMF_FieldRegrid(cmxy_input_grid, &
                       cmxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for chxy."
 call ESMF_FieldRegrid(chxy_input_grid, &
                       chxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for fwetxy."
 call ESMF_FieldRegrid(fwetxy_input_grid, &
                       fwetxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for sneqvoxy."
 call ESMF_FieldRegrid(sneqvoxy_input_grid, &
                       sneqvoxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for alboldxy."
 call ESMF_FieldRegrid(alboldxy_input_grid, &
                       alboldxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for qsnowxy."
 call ESMF_FieldRegrid(qsnowxy_input_grid, &
                       qsnowxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for wslakexy."
 call ESMF_FieldRegrid(wslakexy_input_grid, &
                       wslakexy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for zwtxy."
 call ESMF_FieldRegrid(zwtxy_input_grid, &
                       zwtxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for waxy."
 call ESMF_FieldRegrid(waxy_input_grid, &
                       waxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for wtxy."
 call ESMF_FieldRegrid(wtxy_input_grid, &
                       wtxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for lfmassxy."
 call ESMF_FieldRegrid(lfmassxy_input_grid, &
                       lfmassxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for rtmassxy."
 call ESMF_FieldRegrid(rtmassxy_input_grid, &
                       rtmassxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for stmassxy."
 call ESMF_FieldRegrid(stmassxy_input_grid, &
                       stmassxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for woodxy."
 call ESMF_FieldRegrid(woodxy_input_grid, &
                       woodxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for stblcpxy."
 call ESMF_FieldRegrid(stblcpxy_input_grid, &
                       stblcpxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for fastcpxy."
 call ESMF_FieldRegrid(fastcpxy_input_grid, &
                       fastcpxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for xsaixy."
 call ESMF_FieldRegrid(xsaixy_input_grid, &
                       xsaixy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for taussxy."
 call ESMF_FieldRegrid(taussxy_input_grid, &
                       taussxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for smcwtdxy."
 call ESMF_FieldRegrid(smcwtdxy_input_grid, &
                       smcwtdxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for deeprechxy."
 call ESMF_FieldRegrid(deeprechxy_input_grid, &
                       deeprechxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for rechxy."
 call ESMF_FieldRegrid(rechxy_input_grid, &
                       rechxy_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for orog"
 call ESMF_FieldRegrid(orog_input_grid, &
                       orog_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for zorl"
 call ESMF_FieldRegrid(zorl_input_grid, &
                       zorl_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for tg3"
 call ESMF_FieldRegrid(tg3_input_grid, &
                       tg3_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for slope"
 call ESMF_FieldRegrid(slope_input_grid, &
                       slope_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for snoalb"
 call ESMF_FieldRegrid(snoalb_input_grid, &
                       snoalb_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for vtype"
 call ESMF_FieldRegrid(vtype_input_grid, &
                       vtype_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for greenness over land."
 call ESMF_FieldRegrid(veg_greenness_input_grid, &
                       veg_greenness_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for soil type over land."
 call ESMF_FieldRegrid(soil_type_input_grid, &
                       soil_type_from_input_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for soil temp over land."
 call ESMF_FieldRegrid(soil_temp_input_grid, &
                       soil_temp_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for liquid soil moist over land."
 call ESMF_FieldRegrid(soilm_liq_input_grid, &
                       soilm_liq_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)

 print*,"- CALL Field_Regrid for total soil moist over land."
 call ESMF_FieldRegrid(soilm_tot_input_grid, &
                       soilm_tot_target_grid, &
                       routehandle=regrid_land, &
                       termorderflag=ESMF_TERMORDER_SRCSEQ, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldRegrid", rc)


!call rescale_soil_moisture(localpet)

 end subroutine interp_sfc

!---------------------------------------------------------------------------------------------
! Adjust soil moisture for changes in soil type between the input and target grids.
!---------------------------------------------------------------------------------------------

 subroutine rescale_soil_moisture(localpet)

 use esmf

 implicit none

 integer, intent(in) :: localpet
 integer                            :: clb(3), cub(3), i, j, k, rc
 integer                            :: soilt_input, soilt_target
 integer, parameter                 :: num_soil_cats = 16
 integer(esmf_kind_i4), pointer        :: soil_type_target_ptr(:,:)

 real(esmf_kind_r8), pointer        :: landmask_ptr(:,:)
 real(esmf_kind_r8), pointer        :: soilm_tot_ptr(:,:,:)
 real(esmf_kind_r8), pointer        :: soil_type_input_ptr(:,:)
 real(esmf_kind_r8), pointer        :: veg_greenness_ptr(:,:)
 real                               :: f1, fn, smcdir, smctra
 real                               :: bb(num_soil_cats)
 real                               :: maxsmc(num_soil_cats)
 real                               :: satdk(num_soil_cats)
 real                               :: satpsi(num_soil_cats)
 real                               :: satdw(num_soil_cats)
 real                               :: f11(num_soil_cats)
 real                               :: refsmc(num_soil_cats)
 real                               :: refsmc1, smhigh, smlow
 real                               :: wltsmc(num_soil_cats)
 real                               :: wltsmc1
 real                               :: drysmc(num_soil_cats)

 data smlow /0.5/

 data smhigh /6.0/

 data bb /4.05, 4.26, 4.74, 5.33, 5.33, 5.25, &
          6.77, 8.72, 8.17, 10.73, 10.39, 11.55, &
          5.25, -9.99, 4.05, 4.26/

 data maxsmc /0.395, 0.421, 0.434, 0.476, 0.476, 0.439, &
              0.404, 0.464, 0.465, 0.406, 0.468, 0.457, &
              0.464, -9.99, 0.200, 0.421/

 data satdk /1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6, &
             3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6, &
             1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5, &
             1.4078e-5/

 data satpsi /0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548, &
              0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677, &
              0.3548, -9.99,  0.0350, 0.0363/

 do i = 1, num_soil_cats

   if (maxsmc(i) > 0.0) then

   SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
   F11(I) = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
   REFSMC1 = MAXSMC(I)*(5.79E-9/SATDK(I)) **(1.0/(2.0*BB(I)+3.0))
   REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / SMHIGH
   WLTSMC1 = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
   WLTSMC(I) = WLTSMC1 - SMLOW * WLTSMC1

!----------------------------------------------------------------------
!  CURRENT VERSION DRYSMC VALUES THAT EQUATE TO WLTSMC.
!  FUTURE VERSION COULD LET DRYSMC BE INDEPENDENTLY SET VIA NAMELIST.
!----------------------------------------------------------------------

   DRYSMC(I) = WLTSMC(I)

   end if

 END DO

 if (localpet == 1) then
 print*,'maxsmc ',maxsmc
 print*,'refsmc ',refsmc
 print*,'wltsmc ',wltsmc
 print*,'drysmc ',drysmc
 endif

 print*,"- RESCALE SOIL MOISTURE FOR CHANGES IN SOIL TYPE."

 print*,"- CALL FieldGet FOR TOTAL SOIL MOISTURE."
 call ESMF_FieldGet(soilm_tot_target_grid, &
                    computationalLBound=clb, &
                    computationalUBound=cub, &
                    farrayPtr=soilm_tot_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

! this is the mask of points processed by this code - i.e., not including snow
! or glacial points.  this is not the entire gldas mask.

 print*,"- CALL FieldGet FOR LAND MASK."
 call ESMF_FieldGet(landsea_mask_target_grid, &
                    farrayPtr=landmask_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR VEGETATION GREENNESS."
 call ESMF_FieldGet(veg_greenness_target_grid, &
                    farrayPtr=veg_greenness_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR TARGET GRID SOIL TYPE."
 call ESMF_FieldGet(soil_type_target_grid, &
                    farrayPtr=soil_type_target_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 print*,"- CALL FieldGet FOR SOIL TYPE FROM INPUT GRID."
 call ESMF_FieldGet(soil_type_from_input_grid, &
                    farrayPtr=soil_type_input_ptr, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldGet", rc)

 do j = clb(2), cub(2)
   do i = clb(1), cub(1)

!---------------------------------------------------------------------------------------------
! Check land points.
!---------------------------------------------------------------------------------------------

     if (nint(landmask_ptr(i,j)) == 1) then

        soilt_target = soil_type_target_ptr(i,j)
        soilt_input  = nint(soil_type_input_ptr(i,j))

!---------------------------------------------------------------------------------------------
! Rescale soil moisture at points where the soil type between the input and output
! grids is different.  Caution, this logic assumes the input and target grids use the same
! soil type dataset.
!---------------------------------------------------------------------------------------------

        if (soilt_target /= soilt_input) then

!---------------------------------------------------------------------------------------------
! Rescale top layer.  First, determine direct evaporation part:
!---------------------------------------------------------------------------------------------

          f1=(soilm_tot_ptr(i,j,1)-drysmc(soilt_input)) /    &
             (maxsmc(soilt_input)-drysmc(soilt_input))

          smcdir=drysmc(soilt_target) + f1 *        &
                (maxsmc(soilt_target) - drysmc(soilt_target))

!---------------------------------------------------------------------------------------------
! Continue top layer rescale.  Now determine transpiration part:
!---------------------------------------------------------------------------------------------

          if (soilm_tot_ptr(i,j,1) < refsmc(soilt_input)) then
            f1=(soilm_tot_ptr(i,j,1) - wltsmc(soilt_input)) /       &
               (refsmc(soilt_input) - wltsmc(soilt_input))
            smctra=wltsmc(soilt_target) + f1  *     &
                  (refsmc(soilt_target) - wltsmc(soilt_target))
          else
            f1=(soilm_tot_ptr(i,j,1) - refsmc(soilt_input)) /        &
               (maxsmc(soilt_input) - refsmc(soilt_input))
            smctra=refsmc(soilt_target) + f1 *      &
                  (maxsmc(soilt_target) - refsmc(soilt_target))
          endif

!---------------------------------------------------------------------------------------------
! Top layer is weighted by green vegetation fraction:
!---------------------------------------------------------------------------------------------

          soilm_tot_ptr(i,j,1) = ((1.0 - veg_greenness_ptr(i,j)) * smcdir)  + &
                                  (veg_greenness_ptr(i,j) * smctra)

!---------------------------------------------------------------------------------------------
! Rescale bottom layers as follows:
!
! - Rescale between wilting point and reference value when wilting < soil m < reference, or
! - Rescale between reference point and maximum value when reference < soil m < max.
!---------------------------------------------------------------------------------------------

          do k = 2, cub(3)
            if (soilm_tot_ptr(i,j,k) < refsmc(soilt_input)) then
              fn = (soilm_tot_ptr(i,j,k) - wltsmc(soilt_input)) /        &
                (refsmc(soilt_input) - wltsmc(soilt_input))
              soilm_tot_ptr(i,j,k) = wltsmc(soilt_target) + fn *         &
                (refsmc(soilt_target) - wltsmc(soilt_target))
            else
              fn = (soilm_tot_ptr(i,j,k) - refsmc(soilt_input)) /         &
                (maxsmc(soilt_input) - refsmc(soilt_input))
              soilm_tot_ptr(i,j,k) = refsmc(soilt_target) + fn *         &
                (maxsmc(soilt_target) - refsmc(soilt_target))
            endif
          enddo

        endif ! is soil type different?

!---------------------------------------------------------------------------------------------
! Range check all layers.
!---------------------------------------------------------------------------------------------

        soilm_tot_ptr(i,j,1)=min(soilm_tot_ptr(i,j,1),maxsmc(soilt_target))
        soilm_tot_ptr(i,j,1)=max(drysmc(soilt_target),soilm_tot_ptr(i,j,1))

        do k = 2, cub(3)
          soilm_tot_ptr(i,j,k)=min(soilm_tot_ptr(i,j,k),maxsmc(soilt_target))
          soilm_tot_ptr(i,j,k)=max(wltsmc(soilt_target),soilm_tot_ptr(i,j,k))
        enddo

     endif ! is this a land point?

   enddo
 enddo

 return

 end subroutine rescale_soil_moisture

 end module interp
