 module input_data

! Read input gdas data on tiles.

 use esmf

 implicit none

 private

 character(len=200) :: data_dir_input_grid = "NULL"
 character(len=200) :: sfc_files_input_grid(6) = "NULL"
 character(len=200) :: orog_dir_input_grid = "NULL"
 character(len=200) :: orog_files_input_grid(6) = "NULL"

 integer, public :: i_input, j_input
 integer, parameter, public :: lsoil_input = 4
 integer, parameter, public :: lsnow_input = 3
 integer, parameter, public :: levels_input = lsoil_input + lsnow_input

 type(esmf_field), public        :: landsea_mask_input_grid
 type(esmf_field), public        :: soil_type_input_grid    ! soil type
 type(esmf_field), public        :: snow_liq_equiv_input_grid
 type(esmf_field), public        :: snow_depth_input_grid
 type(esmf_field), public        :: soilm_liq_input_grid
 type(esmf_field), public        :: soilm_tot_input_grid
 type(esmf_field), public        :: soil_temp_input_grid
 type(esmf_field), public        :: veg_greenness_input_grid
 type(esmf_field), public        :: snowxy_input_grid
 type(esmf_field), public        :: tgxy_input_grid
 type(esmf_field), public        :: tvxy_input_grid
 type(esmf_field), public        :: canicexy_input_grid
 type(esmf_field), public        :: canliqxy_input_grid
 type(esmf_field), public        :: eahxy_input_grid
 type(esmf_field), public        :: tahxy_input_grid
 type(esmf_field), public        :: cmxy_input_grid
 type(esmf_field), public        :: chxy_input_grid
 type(esmf_field), public        :: fwetxy_input_grid
 type(esmf_field), public        :: sneqvoxy_input_grid
 type(esmf_field), public        :: alboldxy_input_grid
 type(esmf_field), public        :: qsnowxy_input_grid
 type(esmf_field), public        :: wslakexy_input_grid
 type(esmf_field), public        :: zwtxy_input_grid
 type(esmf_field), public        :: waxy_input_grid
 type(esmf_field), public        :: wtxy_input_grid
 type(esmf_field), public        :: lfmassxy_input_grid
 type(esmf_field), public        :: rtmassxy_input_grid
 type(esmf_field), public        :: stmassxy_input_grid
 type(esmf_field), public        :: woodxy_input_grid
 type(esmf_field), public        :: stblcpxy_input_grid
 type(esmf_field), public        :: fastcpxy_input_grid
 type(esmf_field), public        :: xlaixy_input_grid
 type(esmf_field), public        :: xsaixy_input_grid
 type(esmf_field), public        :: taussxy_input_grid
 type(esmf_field), public        :: smcwtdxy_input_grid
 type(esmf_field), public        :: deeprechxy_input_grid
 type(esmf_field), public        :: rechxy_input_grid
 type(esmf_field), public        :: snicexy_input_grid
 type(esmf_field), public        :: snliqxy_input_grid
 type(esmf_field), public        :: tsnoxy_input_grid
 type(esmf_field), public        :: smoiseq_input_grid
 type(esmf_field), public        :: zsnsoxy_input_grid
 type(esmf_field), public        :: vtype_input_grid
 type(esmf_field), public        :: snoalb_input_grid
 type(esmf_field), public        :: slope_input_grid
 type(esmf_field), public        :: tg3_input_grid
 type(esmf_field), public        :: zorl_input_grid
 type(esmf_field), public        :: orog_input_grid


 public :: read_input_data

 contains

 subroutine read_input_data(localpet)

 use model_grid

 implicit none

 integer, intent(in) :: localpet

 integer :: rc

 print*,"- CALL FieldCreate FOR INPUT GRID LANDSEA MASK."
 landsea_mask_input_grid = ESMF_FieldCreate(input_grid, &
                                     typekind=ESMF_TYPEKIND_R8, &
                                     staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT SOIL TYPE."
 soil_type_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT SNOW LIQ EQUIV."
 snow_liq_equiv_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT PHYSICAL SNOW DEPTH."
 snow_depth_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT wslakexy."
 wslakexy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT zwtxy."
 zwtxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT wtxy."
 wtxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT waxy."
 waxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT stmassxy."
 stmassxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT stblcpxy."
 stblcpxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT woodxy."
 woodxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT rtmassxy."
 rtmassxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT lfmassxy."
 lfmassxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT qsnowxy."
 qsnowxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT alboldxy."
 alboldxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT canliqxy."
 canliqxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT canicexy."
 canicexy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT eahxy."
 eahxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT tahxy."
 tahxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT chxy."
 chxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT cmxy."
 cmxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT tgxy."
 tgxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT tvxy."
 tvxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT snowxy."
 snowxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT fastcpxy."
 fastcpxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT tsnoxy."
 tsnoxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsnow_input/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT snliqxy."
 snliqxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsnow_input/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT snicexy."
 snicexy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsnow_input/),rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT XSAI."
 xsaixy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT smcwtdxy."
 smcwtdxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT taussxy."
 taussxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT rechxy."
 rechxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT deeprechxy."
 deeprechxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT XLAI."
 xlaixy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT GREENNESS."
 veg_greenness_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT fwetxy."
 fwetxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT sneqvoxy."
 sneqvoxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT SOIL TEMPERATURE."
 soil_temp_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT smoiseq"
 smoiseq_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT TOTAL SOIL MOISTURE."
 soilm_tot_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT LIQ SOIL MOISTURE."
 soilm_liq_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/lsoil_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT zsnsoxy."
 zsnsoxy_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, &
                                   ungriddedLBound=(/1/), &
                                   ungriddedUBound=(/levels_input/), rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT vtype."
 vtype_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT snoalb."
 snoalb_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT slope."
 slope_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT tg3."
 tg3_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 print*,"- CALL FieldCreate FOR INPUT zorl."
 zorl_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)


 print*,"- CALL FieldCreate FOR INPUT orog."
 orog_input_grid = ESMF_FieldCreate(input_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 call read_input_sfc_restart_file(localpet)

 end subroutine read_input_data

 subroutine read_input_sfc_restart_file(localpet)

 use netcdf

 implicit none

 integer, intent(in) :: localpet

 character(len=400) :: tilefile

 integer :: ncid, id_dim, error, tile, rc, i, j

 real(esmf_kind_r8), allocatable :: data_one_tile(:,:), soilm(:,:,:)
 real(esmf_kind_r8), allocatable :: mask(:,:), snow_liq(:,:), snow_d(:,:)
 real(esmf_kind_r8), allocatable :: data_one_tile_3d(:,:,:)
 real(esmf_kind_r8), allocatable :: data_one_tile_3dsnow(:,:,:)
 real(esmf_kind_r8), allocatable :: data_one_tile_3dlevels(:,:,:)

 namelist /config/ data_dir_input_grid, sfc_files_input_grid, &
                   orog_dir_input_grid, orog_files_input_grid

 open(41, file="./fort.41", iostat=rc)
 if (rc /= 0) call error_handler("OPENING SETUP NAMELIST.", rc)
 read(41, nml=config, iostat=rc)
 if (rc /= 0) call error_handler("READING SETUP NAMELIST.", rc)
 close (41)

 data_dir_input_grid=trim(data_dir_input_grid) // '/'

 tilefile = trim(data_dir_input_grid) // "/" // trim(sfc_files_input_grid(1))
 print*,"- READ GRID DIMENSIONS FROM: ", trim(tilefile)
 error=nf90_open(trim(tilefile),nf90_nowrite,ncid)
 call netcdf_err(error, 'opening: '//trim(tilefile) )

 error=nf90_inq_dimid(ncid, 'xaxis_1', id_dim)
 call netcdf_err(error, 'reading xaxis_1 id' )
 error=nf90_inquire_dimension(ncid,id_dim,len=i_input)
 call netcdf_err(error, 'reading xaxis_1 value' )

 error=nf90_inq_dimid(ncid, 'yaxis_1', id_dim)
 call netcdf_err(error, 'reading yaxis_1 id' )
 error=nf90_inquire_dimension(ncid,id_dim,len=j_input)
 call netcdf_err(error, 'reading yaxis_1 value' )

 print*,'- INPUT GRID DIMENSIONS ARE: ', i_input, j_input

 if (localpet == 0) then
   allocate(data_one_tile(i_input,j_input))
   allocate(mask(i_input,j_input))
   allocate(snow_liq(i_input,j_input))
   allocate(snow_d(i_input,j_input))
   allocate(data_one_tile_3d(i_input,j_input,lsoil_input))
   allocate(data_one_tile_3dsnow(i_input,j_input,lsnow_input))
   allocate(data_one_tile_3dlevels(i_input,j_input,levels_input))
   allocate(soilm(i_input,j_input,lsoil_input))
 else
   allocate(data_one_tile(0,0))
   allocate(mask(0,0))
   allocate(snow_liq(0,0))
   allocate(snow_d(0,0))
   allocate(data_one_tile_3d(0,0,0))
   allocate(data_one_tile_3dsnow(0,0,0))
   allocate(data_one_tile_3dlevels(0,0,0))
   allocate(soilm(0,0,0))
 endif


 TILE_LOOP : do tile = 1, 6

! zorl

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('zorl', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input zorl for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT zorl."
  call ESMF_FieldScatter(zorl_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tg3

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('tg3', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input tg3 for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT tg3."
  call ESMF_FieldScatter(tg3_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! slope

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('slope', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input slope for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT slope."
  call ESMF_FieldScatter(slope_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snoalb

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('snoalb', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input snoalb for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT snoalb."
  call ESMF_FieldScatter(snoalb_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! vtype

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('vtype', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input vtype for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT vtype."
  call ESMF_FieldScatter(vtype_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! stmassxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('stmassxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input stmassxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT stmassxy."
  call ESMF_FieldScatter(stmassxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! stblcpxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('stblcpxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input stblcpxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT stblcpxy."
  call ESMF_FieldScatter(stblcpxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! woodxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('woodxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input woodxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)

  endif

  print*,"- CALL FieldScatter FOR INPUT woodxy."
  call ESMF_FieldScatter(woodxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! rtmassxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('rtmassxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input rtmassxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT rtmassxy."
  call ESMF_FieldScatter(rtmassxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! lfmassxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('lfmassxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input lfmassxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT lfmassxy."
  call ESMF_FieldScatter(lfmassxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! waxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('waxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input waxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT waxy."
  call ESMF_FieldScatter(waxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! wtxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('wtxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input wtxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT wtxy."
  call ESMF_FieldScatter(wtxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! zwtxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('zwtxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input zwtxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT zwtxy."
  call ESMF_FieldScatter(zwtxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! wslakexy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('wslakexy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input wslakexy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT wslakexy."
  call ESMF_FieldScatter(wslakexy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! alboldxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('alboldxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input albdoldxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT alboldxy."
  call ESMF_FieldScatter(alboldxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! qsnowxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('qsnowxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input qsnowxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT qsnowxy."
  call ESMF_FieldScatter(qsnowxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! sneqvoxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('sneqvoxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input sneqvoxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT sneqvoxy."
  call ESMF_FieldScatter(sneqvoxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! fwetxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('fwetxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input fwetxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT fwetxy."
  call ESMF_FieldScatter(fwetxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! chxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('chxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input chxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT chxy."
  call ESMF_FieldScatter(chxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! cmxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('cmxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input cmxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT cmxy."
  call ESMF_FieldScatter(cmxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! eahxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('eahxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input eahxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT eahxy."
  call ESMF_FieldScatter(eahxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tahxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('tahxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input tahxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT tahxy."
  call ESMF_FieldScatter(tahxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! canliqxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('canliqxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input canliqxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT canliqxy."
  call ESMF_FieldScatter(canliqxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! canicexy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('canicexy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input canicexy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT canicexy."
  call ESMF_FieldScatter(canicexy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tvxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('tvxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input tvxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT tvxy."
  call ESMF_FieldScatter(tvxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tgxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('tgxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input tgxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT tgxy."
  call ESMF_FieldScatter(tgxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snowxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('snowxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input snowxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT snowxy."
  call ESMF_FieldScatter(snowxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! fastcpxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('fastcpxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input fastcpxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT fastcpxy."
  call ESMF_FieldScatter(fastcpxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! xsai

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('xsaixy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input xsai for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT XSAI."
  call ESMF_FieldScatter(xsaixy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! smcwtdxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('smcwtdxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input smcwtdxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT smcwtdxy."
  call ESMF_FieldScatter(smcwtdxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! taussxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('taussxy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input taussxy for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT taussxy."
  call ESMF_FieldScatter(taussxy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tsnoxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('tsnoxy', tile, i_input, j_input, &
                                   lsnow_input, sfcdata_3d=data_one_tile_3dsnow)
    print*,'input tsnoxy for tile ',tile, maxval(data_one_tile_3dsnow), minval(data_one_tile_3dsnow)
  endif

  print*,"- CALL FieldScatter FOR INPUT tsnoxy."
  call ESMF_FieldScatter(tsnoxy_input_grid, data_one_tile_3dsnow, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snliqxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('snliqxy', tile, i_input, j_input, &
                                   lsnow_input, sfcdata_3d=data_one_tile_3dsnow)
    print*,'input snliqxy for tile ',tile, maxval(data_one_tile_3dsnow), minval(data_one_tile_3dsnow)
  endif

  print*,"- CALL FieldScatter FOR INPUT snliqxy."
  call ESMF_FieldScatter(snliqxy_input_grid, data_one_tile_3dsnow, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snicexy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('snicexy', tile, i_input, j_input, &
                                   lsnow_input, sfcdata_3d=data_one_tile_3dsnow)
    print*,'input snicexy1 for tile ',tile, maxval(data_one_tile_3dsnow(:,:,1)), minval(data_one_tile_3dsnow(:,:,1))
    print*,'input snicexy2 for tile ',tile, maxval(data_one_tile_3dsnow(:,:,2)), minval(data_one_tile_3dsnow(:,:,2))
    print*,'input snicexy3 for tile ',tile, maxval(data_one_tile_3dsnow(:,:,3)), minval(data_one_tile_3dsnow(:,:,3))
  endif

  print*,"- CALL FieldScatter FOR INPUT snicexy."
  call ESMF_FieldScatter(snicexy_input_grid, data_one_tile_3dsnow, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! xlai

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('xlaixy', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input xlai for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT XLAI."
  call ESMF_FieldScatter(xlaixy_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! Veg greenness

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('vfrac', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input greenness for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID GREENNESS."
  call ESMF_FieldScatter(veg_greenness_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! Soil type

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('stype', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input soil type for tile ',tile, maxval(data_one_tile), minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOIL TYPE."
  call ESMF_FieldScatter(soil_type_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snow liq equiv

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('sheleg', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=snow_liq)
    print*,'input snow liq equiv for tile ',tile,maxval(snow_liq),  &
                                                 minval(snow_liq)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SNOW LIQ EQUIV."
  call ESMF_FieldScatter(snow_liq_equiv_input_grid, snow_liq, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! snow depth

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('snwdph', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=snow_d)
    print*,'input snow depth for tile ',tile, maxval(snow_d), minval(snow_d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SNOW DEPTH."
  call ESMF_FieldScatter(snow_depth_input_grid, snow_d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! smoiseq

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('smoiseq', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=data_one_tile_3d)
    print*,'input smoiseq for tile ',tile, maxval(data_one_tile_3d), minval(data_one_tile_3d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID smoiseq."
  call ESMF_FieldScatter(smoiseq_input_grid, data_one_tile_3d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! soil temp

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('stc', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=data_one_tile_3d)
    print*,'input soil temp for tile ',tile, maxval(data_one_tile_3d), minval(data_one_tile_3d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOIL TEMP."
  call ESMF_FieldScatter(soil_temp_input_grid, data_one_tile_3d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! zsnsoxy

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('zsnsoxy', tile, i_input, j_input, &
                                   levels_input, &
                                   sfcdata_3d=data_one_tile_3dlevels)
    print*,'input zsnsoxy for tile ',tile, maxval(data_one_tile_3dlevels), minval(data_one_tile_3dlevels)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID zsnsoxy."
  call ESMF_FieldScatter(zsnsoxy_input_grid, data_one_tile_3dlevels, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! liq soil m

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('slc', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=data_one_tile_3d)
    print*,'input soilm liq for tile ',tile, maxval(data_one_tile_3d), minval(data_one_tile_3d)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOILM LIQ."
  call ESMF_FieldScatter(soilm_liq_input_grid, data_one_tile_3d, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! tot soil m

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('smc', tile, i_input, j_input, &
                                   lsoil_input, sfcdata_3d=soilm)
    print*,'input soilm tot for tile ',tile, maxval(soilm), minval(soilm)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID SOILM TOT."
  call ESMF_FieldScatter(soilm_tot_input_grid, soilm, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! orog

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('orog_raw', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input orog for tile ',tile, maxval(data_one_tile),  &
                                   minval(data_one_tile)
  endif

  print*,"- CALL FieldScatter FOR INPUT GRID orog."
  call ESMF_FieldScatter(orog_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

! land mask

  if (localpet == 0) then
    call read_fv3_grid_data_netcdf('slmsk', tile, i_input, j_input, &
                                   lsoil_input, sfcdata=data_one_tile)
    print*,'input slmsk for tile ',tile, maxval(data_one_tile),  &
                                   minval(data_one_tile)
  endif

! if (localpet == 0) then
!   data_one_tile = 1.0  ! gdas point to process
!   do j = 1, j_input
!   do i = 1, i_input
!     if (nint(mask(i,j)) == 0) then
!       data_one_tile(i,j) = 0.0  ! don't process water
!     endif
!     if (nint(mask(i,j)) == 2) then
!       data_one_tile(i,j) = 0.0  ! don't process sea ice
!     endif
!     if (snow_d(i,j) > 0.0) then
!       data_one_tile(i,j) = 0.0  ! don't process snow
!     endif
!     if (snow_liq(i,j) > 0.0) then
!       data_one_tile(i,j) = 0.0  ! don't process snow
!     endif
!     if (soilm(i,j,1) > 0.9) then
!       data_one_tile(i,j) = 0.0  ! don't process land ice
!     endif
!   enddo
!   enddo
! endif

  print*,"- CALL FieldScatter FOR INPUT LANDSEA MASK."
  call ESMF_FieldScatter(landsea_mask_input_grid, data_one_tile, rootpet=0, tile=tile, rc=rc)
  if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
     call error_handler("IN FieldScatter", rc)

 enddo TILE_LOOP

 deallocate (data_one_tile, data_one_tile_3d, mask, snow_liq, snow_d, soilm)

 end subroutine read_input_sfc_restart_file

 SUBROUTINE READ_FV3_GRID_DATA_NETCDF(FIELD,TILE_NUM,IMO,JMO,LMO, &
                                      SFCDATA, SFCDATA_3D)

 use netcdf 

 IMPLICIT NONE

 CHARACTER(LEN=*),INTENT(IN)      :: FIELD

 INTEGER, INTENT(IN)   :: IMO, JMO, LMO, TILE_NUM

 REAL(ESMF_KIND_R8), INTENT(OUT), OPTIONAL     :: SFCDATA(IMO,JMO)
 REAL(ESMF_KIND_R8), INTENT(OUT), OPTIONAL     :: SFCDATA_3D(IMO,JMO,LMO)

 CHARACTER(LEN=256)    :: TILEFILE

 INTEGER               :: ERROR, NCID, ID_VAR

 if (index(field, "orog") > 0) then
   TILEFILE = TRIM(OROG_DIR_INPUT_GRID) // "/" // TRIM(OROG_FILES_INPUT_GRID(TILE_NUM))
 else
   TILEFILE = TRIM(DATA_DIR_INPUT_GRID) // "/" // TRIM(SFC_FILES_INPUT_GRID(TILE_NUM))
 endif
 PRINT*,'WILL READ ',TRIM(FIELD), ' FROM: ', TRIM(TILEFILE)

 ERROR=NF90_OPEN(TRIM(TILEFILE),NF90_NOWRITE,NCID)
 CALL NETCDF_ERR(ERROR, 'OPENING: '//TRIM(TILEFILE) )

 ERROR=NF90_INQ_VARID(NCID, FIELD, ID_VAR)
 CALL NETCDF_ERR(ERROR, 'READING FIELD ID' )

 IF (PRESENT(SFCDATA_3D)) THEN
   ERROR=NF90_GET_VAR(NCID, ID_VAR, SFCDATA_3D)
   CALL NETCDF_ERR(ERROR, 'READING FIELD' )
 ELSE
   ERROR=NF90_GET_VAR(NCID, ID_VAR, SFCDATA)
   CALL NETCDF_ERR(ERROR, 'READING FIELD' )
 ENDIF

 ERROR = NF90_CLOSE(NCID)

 END SUBROUTINE READ_FV3_GRID_DATA_NETCDF

 end module input_data
