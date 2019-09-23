 module input_data

! Read data on gldas grid

 use esmf
 use model_grid

 implicit none

 type(esmf_field), public        :: soil_type_input_grid 

 contains

 subroutine read_input_data(localpet)

 use model_grid

 implicit none

 integer, intent(in) :: localpet

 integer :: rc

 print*,"- CALL FieldCreate FOR INPUT gldas SOIL TYPE."
 soil_type_input_grid = ESMF_FieldCreate(gldas_grid, &
                                   typekind=ESMF_TYPEKIND_R8, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER,rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldCreate", rc)

 call read_gldas_data(localpet)

 end subroutine read_input_data

 subroutine read_gldas_data(localpet)

 implicit none

 integer, intent(in) :: localpet

 integer             :: rc

 real(esmf_kind_r8), allocatable :: dummy(:,:)

 if (localpet == 0) then
   allocate (dummy(i_gldas,j_gldas))
 else
   allocate (dummy(0,0))
 endif

 if (localpet == 0) then
   dummy=777.
 endif

 print*,"- CALL FieldScatter FOR input soil type."
 call ESMF_FieldScatter(soil_type_input_grid, dummy, rootpet=0, rc=rc)
 if(ESMF_logFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__line__,file=__file__)) &
    call error_handler("IN FieldScatter", rc)


 deallocate (dummy)

 end subroutine read_gldas_data

 end module input_data
